library(tidyverse)
library(stringi)
library(scales)
library(lubridate)
library(openxlsx)
library(readxl)
library(sf)


sf_use_s2(FALSE) 

CAMINHO_DADOS <- "C:/Users/Detran/OneDrive - PRODESP/Documentos/alocacao_superin/painelalocacao/data" #alteraraqui
CAMINHO_COMPLETO_EXPORTACAO <- "C:/Users/Detran/OneDrive - PRODESP/Documentos/alocacao_superin/atlasfevereiro/teste_svg" #alteraraqui

if (!dir.exists(CAMINHO_COMPLETO_EXPORTACAO)) dir.create(CAMINHO_COMPLETO_EXPORTACAO, recursive = TRUE)

START_DATE <- as.Date("2025-01-01") #janelatemporal ajustar
END_DATE   <- as.Date("2025-12-31") #janelatemporal ajustar

ORDEM_DIAS <- c("segunda-feira", "terca-feira", "quarta-feira", "quinta-feira", "sexta-feira", "sabado", "domingo")
ORDEM_TURNOS <- c("MADRUGADA", "MANHA", "TARDE", "NOITE")
CODIGO_LENGTH <- 7

mapa_meses <- c("01"="Jan","02"="Fev","03"="Mar","04"="Abr","05"="Mai","06"="Jun",
                "07"="Jul","08"="Ago","09"="Set","10"="Out","11"="Nov","12"="Dez")
seq_meses <- seq(floor_date(START_DATE, "month"), floor_date(END_DATE, "month"), by = "month")
ORDEM_MESES <- paste0(mapa_meses[format(seq_meses, "%m")], " ", format(seq_meses, "%Y"))

cm_to_in <- function(cm) cm * 0.393701
full_path <- function(file_name) { paste0(CAMINHO_DADOS, "/", file_name) }
safe_name <- function(x) { x %>% normalize_text() %>% str_replace_all("[^A-Z0-9_]+", "_") %>% str_replace_all("^_|_$", "") }
`%||%` <- function(a, b) if (!is.null(a)) a else b


remove_accents <- function(x) { stringi::stri_trans_general(str = x, id = "Latin-ASCII") }
normalize_text <- function(x) { x %>% str_trim() %>% remove_accents() %>% toupper() }

limpa_texto <- function(x) {
  x %>% as.character() %>% 
    str_replace_all("\u00A0", " ") %>%
    str_replace_all("[\r\n\t]", " ") %>% 
    str_squish() %>% str_trim() %>% toupper()
}


fix_coords <- function(x) {
  
  if (is.na(x)) return(NA)
  
  while (abs(x) > 180) {
    x <- x / 10
  }
  return(x)
}

message("Lendo arquivos...")

bd_municipios <- read_excel(full_path("bd_municipios.xlsx"), col_names = TRUE, .name_repair = "unique") %>%
  rename(CD_MUN_KEY = "CD_MUN_7", SUPERINTENDENCIA = "SUPERINTENDENCIA", municipio_origem = "NM_MUN") %>%
  select(municipio_origem, SUPERINTENDENCIA, CD_MUN_KEY) %>%
  mutate(CD_MUN_KEY = as.character(CD_MUN_KEY))

sinistros <- read_delim(full_path("sinistros_2022-2025.csv"), delim = ";", show_col_types = FALSE, locale = locale(encoding = "CP1252")) %>%
  rename(id_sinistro = 1) %>% mutate(cod_ibge = as.character(cod_ibge))

veiculos <- read_delim(full_path("veiculos_2022-2025.csv"), delim = ";", show_col_types = FALSE, locale = locale(encoding = "CP1252")) %>%
  rename(id_sinistro = 1) %>% select(id_sinistro, tipo_veiculo = 7)

pessoas <- read_delim(full_path("pessoas_2022-2025.csv"), delim = ";", show_col_types = FALSE, locale = locale(encoding = "CP1252")) %>%
  rename(id_sinistro = 1) %>% select(id_sinistro, tipo_vitima = 11)

path_shp <- full_path("distrito_municipal_v2.shp") 
path_xls <- full_path("regioes_municipais.xls")
mapa_sp_distritos <- NULL

if (file.exists(path_shp) && file.exists(path_xls)) {
  message("Carregando mapa SP...")
  shp_raw <- read_sf(path_shp) %>% st_make_valid() %>% st_transform(crs = 4326) %>% mutate(chave_join = limpa_texto(nm_distrit))
  xls_regioes <- read_excel(path_xls, sheet = "tab1") %>% mutate(chave_join = limpa_texto(distrito), NOME_REGIAO = str_to_title(regiao)) %>% select(chave_join, NOME_REGIAO)
  mapa_sp_distritos <- left_join(shp_raw, xls_regioes, by = "chave_join")
} else { warning("Arquivos de mapa não encontrados.") }


sinistros_clean <- sinistros %>%
  mutate(tipo_registro = limpa_texto(tipo_registro)) %>%
  filter(tipo_registro %in% c("SINISTRO NAO FATAL", "SINISTRO FATAL")) %>%
  mutate(cod_ibge_KEY = str_pad(str_replace_all(str_trim(as.character(cod_ibge)), "[^0-9]", ""), width = CODIGO_LENGTH, side = "left", pad = "0")) %>%
  filter(nchar(cod_ibge_KEY) == CODIGO_LENGTH) %>%
  mutate(data_formatada = dmy(data_sinistro, quiet = TRUE)) %>%
  filter(!is.na(data_formatada), data_formatada >= START_DATE, data_formatada <= END_DATE) %>%
  mutate(
    fatal = ifelse(tipo_registro == "SINISTRO FATAL", 1, 0),
    mes_numero = format(data_formatada, "%m"),
    ano_sinistro = format(data_formatada, "%Y"),
    mes_ano = paste0(mapa_meses[mes_numero], " ", ano_sinistro),
    turno = remove_accents(limpa_texto(turno)) 
  ) %>%
  filter(!is.na(turno), turno != "NAO DISPONIVEL", turno != "") %>%
  mutate(
    dia_da_semana = remove_accents(str_to_lower(dia_da_semana)),
    dia_da_semana = str_replace_all(dia_da_semana, "[^a-z]", " "),
    dia_da_semana = str_squish(dia_da_semana),
    dia_da_semana = str_replace(dia_da_semana, " ", "-"),
    dia_da_semana = factor(dia_da_semana, levels = ORDEM_DIAS),
    turno = factor(turno, levels = ORDEM_TURNOS)
  ) %>%
  filter(!is.na(dia_da_semana), !is.na(turno)) %>%
  select(-data_formatada, -mes_numero, -ano_sinistro)

dados_completos <- sinistros_clean %>%
  left_join(bd_municipios_clean, by = c("cod_ibge_KEY" = "CD_MUN_KEY")) %>%
  filter(!is.na(SUPERINTENDENCIA)) %>%
  select(-any_of("municipio")) %>% rename(municipio = municipio_origem) %>%
  select(-cod_ibge, -cod_ibge_KEY)

if (nrow(dados_completos) == 0) stop("ERRO FATAL: Base vazia após filtros.")

veiculos_clean <- veiculos %>% filter(tipo_veiculo != "OUTROS", !is.na(tipo_veiculo)) %>% mutate(modo_envolvido = limpa_texto(tipo_veiculo)) %>% select(id_sinistro, modo_envolvido) %>% distinct()
pedestres_clean <- pessoas %>% mutate(tipo_vitima = limpa_texto(tipo_vitima)) %>% filter(tipo_vitima == "PEDESTRE") %>% mutate(modo_envolvido = "PEDESTRE") %>% select(id_sinistro, modo_envolvido) %>% distinct()
modos_envolvidos <- bind_rows(veiculos_clean, pedestres_clean)
dados_g5 <- dados_completos %>% select(id_sinistro, SUPERINTENDENCIA, tipo_registro, fatal) %>% inner_join(modos_envolvidos, by = "id_sinistro") %>% distinct()
superintendencias <- unique(dados_completos$SUPERINTENDENCIA) %>% na.omit()

# Funcao
generate_all_calculations_and_export_svgs <- function(super_nome) {
  
  message(paste("Processando:", super_nome))
  
  dados_super    <- dados_completos %>% filter(SUPERINTENDENCIA == super_nome)
  dados_g5_super <- dados_g5 %>% filter(SUPERINTENDENCIA == super_nome)
  
  pasta_super <- file.path(CAMINHO_COMPLETO_EXPORTACAO, safe_name(super_nome))
  if (!dir.exists(pasta_super)) dir.create(pasta_super, recursive = TRUE)
  caminho_base <- file.path(pasta_super, paste0(safe_name(super_nome), "_"))
  
  df_meses_base <- tibble(mes_ano = factor(ORDEM_MESES, levels = ORDEM_MESES))
  
  tema_original_barras <- theme_minimal() + 
    theme(
      plot.title = element_blank(),
      axis.text.x = element_text(size = 6, family = "Open Sans", angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_line(colour = "gray90", linewidth = 0.2),
      panel.grid.major.x = element_blank(), 
      panel.grid.minor = element_blank()
    )

  tema_original_ranking <- theme_minimal() +
    theme(
       plot.title = element_blank(),
       axis.text = element_text(size = 7, family = "Open Sans"),
       panel.grid.major.x = element_line(colour = "gray90", linewidth = 0.2),
       panel.grid.major.y = element_blank(), 
       panel.grid.minor = element_blank()
    )

  # GF1
  gf1 <- dados_super %>% mutate(mes_ano = factor(mes_ano, levels = ORDEM_MESES)) %>% group_by(mes_ano) %>% summarise(Qtd = n_distinct(id_sinistro), .groups = "drop") %>% right_join(df_meses_base, by="mes_ano") %>% mutate(Qtd = replace_na(Qtd, 0))
  p1 <- ggplot(gf1, aes(x=mes_ano, y=Qtd)) + geom_col(fill="#005CA8", width=0.7) + 
    geom_text(aes(label=scales::number(Qtd, big.mark=".", decimal.mark=","), y=ifelse(Qtd>0, Qtd/2, NA)), angle=90, vjust=0.5, color="white", size=2.6, fontface="bold", family="Open Sans") + 
    labs(x=NULL, y=NULL) + 
    tema_original_barras + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  ggsave(paste0(caminho_base, "GF1_Total_por_mes.svg"), p1, width=cm_to_in(9), height=cm_to_in(4))
  
  # GF2
  gf2 <- dados_super %>% filter(fatal==1) %>% mutate(mes_ano = factor(mes_ano, levels = ORDEM_MESES)) %>% group_by(mes_ano) %>% summarise(Qtd = n_distinct(id_sinistro), .groups = "drop") %>% right_join(df_meses_base, by="mes_ano") %>% mutate(Qtd = replace_na(Qtd, 0))
  p2 <- ggplot(gf2, aes(x=mes_ano, y=Qtd)) + geom_col(fill="#005CA8", width=0.7) + 
    geom_text(aes(label=scales::number(Qtd, big.mark=".", decimal.mark=","), y=ifelse(Qtd>0, Qtd/2, NA)), angle=90, vjust=0.5, color="white", size=2.6, fontface="bold", family="Open Sans") + 
    labs(x=NULL, y=NULL) + 
    tema_original_barras + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  ggsave(paste0(caminho_base, "GF2_Fatais_por_mes.svg"), p2, width=cm_to_in(9), height=cm_to_in(4))

  # HEATMAP 
  df_hm <- dados_super %>% group_by(id_sinistro, dia_da_semana, turno) %>% summarise(T=1, F=max(fatal), .groups="drop") %>% group_by(dia_da_semana, turno) %>% summarise(Total=sum(T), Fatal=sum(F), .groups="drop") %>% complete(dia_da_semana, turno, fill=list(Total=0, Fatal=0))
  
  map_d <- setNames(names(mapa_dias_abrev), mapa_dias_abrev); map_t <- setNames(names(mapa_turnos_display), mapa_turnos_display)
  
  df_hm_plot <- df_hm %>%
    filter(!is.na(dia_da_semana), !is.na(turno)) %>%
    mutate(dia_disp = factor(mapa_dias_abrev[as.character(dia_da_semana)], levels = unname(mapa_dias_abrev[ORDEM_DIAS])),
           turno_disp = factor(mapa_turnos_display[as.character(turno)], levels = c("Madrugada", "Noite", "Tarde", "Manhã")))
  
  p5 <- ggplot(df_hm_plot, aes(x=dia_disp, y=turno_disp, fill=Total)) + geom_tile(color="white", linewidth=0.6) + scale_fill_gradient(low="#005ca8", high="#004077") + 
    geom_text(aes(label=Total), color="white", size=2.6, fontface="bold", family="Open Sans") + 
    theme_minimal() + 
    theme(plot.title = element_blank(), axis.text = element_text(size = 7, family = "Open Sans"), panel.grid = element_blank(), legend.position = "none") + 
    labs(x=NULL, y=NULL) + scale_x_discrete(position = "top")
  ggsave(paste0(caminho_base, "DF5_Heatmap_Total.svg"), p5, width=cm_to_in(9), height=cm_to_in(2.7))
  
  p6 <- ggplot(df_hm_plot, aes(x=dia_disp, y=turno_disp, fill=Fatal)) + geom_tile(color="white", linewidth=0.6) + scale_fill_gradient(low="#005ca8", high="#004077") + 
    geom_text(aes(label=Fatal), color="white", size=2.6, fontface="bold", family="Open Sans") + 
    theme_minimal() + 
    theme(plot.title = element_blank(), axis.text = element_text(size = 7, family = "Open Sans"), panel.grid = element_blank(), legend.position = "none") + 
    labs(x=NULL, y=NULL) + scale_x_discrete(position = "top")
  ggsave(paste0(caminho_base, "DF6_Heatmap_Fatal.svg"), p6, width=cm_to_in(9), height=cm_to_in(2.7))

  #  DF11 (modos)
  df_modos <- dados_g5_super %>% filter(fatal==1) %>% group_by(modo_envolvido) %>% summarise(Qtd=n_distinct(id_sinistro), .groups="drop") %>% 
    mutate(Modo = case_when(modo_envolvido=="PEDESTRE"~"Pedestre", str_detect(modo_envolvido,"MOTO")~"Motocicleta", str_detect(modo_envolvido,"BICIC")~"Bicicleta", str_detect(modo_envolvido,"AUTOM")~"Automóvel", str_detect(modo_envolvido,"ONIB")~"Ônibus", str_detect(modo_envolvido,"CAMIN")~"Caminhão", TRUE~NA_character_)) %>%
    filter(!is.na(Modo)) %>% group_by(Modo) %>% summarise(Qtd=sum(Qtd)) %>% mutate(Pct=Qtd/sum(Qtd))
  
  if(nrow(df_modos)>0) {
    pal <- c("Caminhão"="#D3A1FA", "Ônibus"="#B456E0", "Automóvel"="#390077", "Motocicleta"="#A4CFED", "Bicicleta"="#005CA8", "Pedestre"="#004077")
    df_modos$Modo <- factor(df_modos$Modo, levels=names(pal))
    p11 <- ggplot(df_modos, aes(x="", y=Pct, fill=Modo)) + geom_col(position=position_stack(reverse=TRUE), width=0.6) + scale_fill_manual(values=pal) + 
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
      coord_flip() + theme_minimal() + 
      theme(plot.title = element_blank(), axis.text.y = element_text(size=7, family="Open Sans"), axis.text.x = element_blank(), legend.position = "top", legend.direction = "horizontal", legend.key.size = grid::unit(0.28, "cm"), panel.grid = element_blank()) +
      labs(x=NULL, y=NULL, fill=NULL)
    ggsave(paste0(caminho_base, "DF11_Modos_Fatais_100pct.svg"), p11, width=cm_to_in(18), height=cm_to_in(2.5))
  }

  #  LOGICA ESPACIAL
  is_sp <- normalize_text(super_nome) == "SAO PAULO"
  gerou_zonas <- FALSE
  
  if (is_sp && !is.null(mapa_sp_distritos)) {
    cols <- names(dados_super)
    clat <- cols[str_detect(tolower(cols), "lat")][1]
    clon <- cols[str_detect(tolower(cols), "lon")][1]
    
    if(!is.na(clat) && !is.na(clon)) {
      
      d_coord <- dados_super %>% 
        mutate(
          lat_raw = as.numeric(str_replace(as.character(.data[[clat]]), ",", ".")),
          lon_raw = as.numeric(str_replace(as.character(.data[[clon]]), ",", "."))
        ) %>%
        rowwise() %>%
        mutate(
          lat = fix_coords(lat_raw),
          lon = fix_coords(lon_raw)
        ) %>%
        ungroup() %>%
        filter(!is.na(lat), !is.na(lon))
      
      if(nrow(d_coord) > 0) {
        
        if(mean(d_coord$lon, na.rm=TRUE) > 0) { d_coord$lon <- -abs(d_coord$lon); d_coord$lat <- -abs(d_coord$lat) }
        
        if(abs(mean(d_coord$lon, na.rm=TRUE)) < 35) { 
           message("   [SP INFO] Invertendo Lat/Lon detectado...")
           tmp <- d_coord$lat; d_coord$lat <- d_coord$lon; d_coord$lon <- tmp
        }
        
        message(paste("   [SP DEBUG CORRIGIDO] Ex Lat:", head(d_coord$lat,1), "Lon:", head(d_coord$lon,1)))

        sf_pts <- st_as_sf(d_coord, coords=c("lon","lat"), crs=4326)
        sf_join <- st_join(sf_pts, mapa_sp_distritos)
        
        df_z <- sf_join %>% st_drop_geometry() %>% filter(!is.na(NOME_REGIAO)) %>%
          group_by(NOME_REGIAO) %>% summarise(Total=n_distinct(id_sinistro), Fat=n_distinct(id_sinistro[fatal==1])) %>% arrange(desc(Total))
        
        if(nrow(df_z) > 0) {
          wb <- createWorkbook()
          addWorksheet(wb, "Zonas_Total"); writeData(wb, "Zonas_Total", df_z)
          addWorksheet(wb, "Zonas_Fatal"); writeData(wb, "Zonas_Fatal", df_z %>% arrange(desc(Fat)))
          saveWorkbook(wb, paste0(caminho_base, "Dados_Zonas_SP.xlsx"), overwrite=TRUE)
          
          pz <- ggplot(df_z, aes(x=reorder(NOME_REGIAO, Total), y=Total)) + geom_col(fill="#005CA8", width=0.7) + coord_flip() + 
            geom_text(aes(label=scales::number(Total, big.mark=".", decimal.mark=",")), hjust=-0.1, size=2.8, fontface="bold", color="#004077", family="Open Sans") + 
            labs(x=NULL, y=NULL) + tema_original_ranking +
            scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
          ggsave(paste0(caminho_base, "DF3_Ranking_Zonas_Total.svg"), pz, width=cm_to_in(9), height=cm_to_in(9.5))
          
          pzf <- df_z %>% arrange(desc(Fat))
          pz2 <- ggplot(pzf, aes(x=reorder(NOME_REGIAO, Fat), y=Fat)) + geom_col(fill="#005CA8", width=0.7) + coord_flip() + 
            geom_text(aes(label=scales::number(Fat, big.mark=".", decimal.mark=",")), hjust=-0.1, size=2.8, fontface="bold", color="#004077", family="Open Sans") + 
            labs(x=NULL, y=NULL) + tema_original_ranking +
            scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
          ggsave(paste0(caminho_base, "DF4_Ranking_Zonas_Fatal.svg"), pz2, width=cm_to_in(9), height=cm_to_in(9.5))
          
          gerou_zonas <- TRUE
        }
      }
    }
  }
  
  if (!gerou_zonas) {
    n_mun <- n_distinct(dados_super$municipio)
    if (n_mun > 1) {
      
      df3 <- dados_super %>% group_by(municipio) %>% summarise(T=n_distinct(id_sinistro)) %>% arrange(desc(T)) %>% slice_head(n=20)
      p3 <- ggplot(df3, aes(x=reorder(str_to_title(municipio), T), y=T)) + geom_col(fill="#005CA8") + coord_flip() + 
        geom_text(aes(label=scales::number(T, big.mark=".", decimal.mark=",")), hjust=-0.1, size=2.8, fontface="bold", color="#004077", family="Open Sans") + 
        labs(x=NULL, y=NULL) + tema_original_ranking + scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
      ggsave(paste0(caminho_base, "DF3_Ranking_total_municipios_Top20.svg"), p3, width=cm_to_in(9), height=cm_to_in(9.5))
      
      df4 <- dados_super %>% filter(fatal==1) %>% group_by(municipio) %>% summarise(F=n_distinct(id_sinistro)) %>% arrange(desc(F)) %>% slice_head(n=20)
      p4 <- ggplot(df4, aes(x=reorder(str_to_title(municipio), F), y=F)) + geom_col(fill="#005CA8") + coord_flip() + 
        geom_text(aes(label=scales::number(F, big.mark=".", decimal.mark=",")), hjust=-0.1, size=2.8, fontface="bold", color="#004077", family="Open Sans") + 
        labs(x=NULL, y=NULL) + tema_original_ranking + scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
      ggsave(paste0(caminho_base, "DF4_Ranking_fatal_municipios_Top20.svg"), p4, width=cm_to_in(9), height=cm_to_in(9.5))
      
    } else {
      tot <- n_distinct(dados_super$id_sinistro); fat <- n_distinct(dados_super$id_sinistro[dados_super$fatal==1])
      dfr <- tibble(T=factor(c("Sinistros totais","Sinistros fatais"), levels=c("Sinistros totais","Sinistros fatais")), Q=c(tot, fat))
      
      tema_resumo <- theme_minimal() + 
        theme(plot.title=element_blank(), axis.text.x=element_text(size=7, family="Open Sans", face="bold"), axis.text.y=element_blank(), panel.grid.major.y=element_line(colour="gray90", linewidth=0.2), panel.grid.major.x=element_blank(), panel.grid.minor=element_blank())
      
      pr <- ggplot(dfr, aes(x=T, y=Q)) + geom_col(fill="#005CA8", width=0.6) + 
        geom_text(aes(label=scales::number(Q, big.mark=".", decimal.mark=",")), vjust=-0.3, size=2.8, fontface="bold", color="#004077", family="Open Sans") + 
        labs(x=NULL, y=NULL) + tema_resumo + scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
      ggsave(paste0(caminho_base, "DF3_DF4_Resumo_Total_vs_Fatal.svg"), pr, width=cm_to_in(9), height=cm_to_in(9.5))
    }
  }
  return(list(success=TRUE, super=super_nome))
}

# EXECUÇÃO 
results <- list()
for (s in superintendencias) {
  results[[s]] <- tryCatch(generate_all_calculations_and_export_svgs(s), error=function(e) list(success=FALSE, error=e$message))
}
message("Concluído.")