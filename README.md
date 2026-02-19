# Painel da sinistralidade por Superintendências do DentranSP`no Estado de São Paulo

Este repositório contém um script em linguagem R focado no processamento, análise e visualização de dados de sinistros de trânsito no Estado de São Paulo. O código automatiza a geração de gráficos e rankings (exportados em formato SVG) segmentados por Superintendências, divisão organizacional do departamento pelo estado.

## Sobre os Dados (Open Data)
Para garantir a reprodutibilidade e evitar o armazenamento de arquivos pesados, este projeto **não** hospeda as bases de dados brutas de sinistros, veículos e pessoas. 

Em vez disso, o script utiliza o pacote `ost.utils` para baixar automaticamente os microdados mais recentes diretamente da base do **Infosiga SP** para um diretório temporário (`tempdir()`) durante a execução. Ao finalizar o R, esses arquivos pesados são descartados automaticamente da sua máquina.

## 🗺️ Dados Espaciais e Auxiliares (Inclusos no Repositório)
Para garantir a reprodução exata dos mapas e segmentações, este repositório já inclui na pasta `data/` os arquivos estruturais do projeto:

* **Dados do GeoSampa (`distrito_municipal_v2.shp` e extensões):** Camada espacial oficial contendo a geometria dos distritos do município de São Paulo.
* **Divisão Administrativa (`regioes_municipais.xls`):** Tabela de relacionamento que mapeia cada distrito paulistano para a sua respectiva macrorregião (Leste, Norte, Sul, Oeste, Centro) e para a sua Prefeitura Regional.
* **Superintendências (`bd_municipios.xlsx`):** Base de "de-para" que relaciona todos os municípios do Estado de São Paulo às suas respectivas Superintendências de trânsito.

*Nota: Os microdados pesados de sinistros e vítimas não ficam armazenados aqui. O script faz o download automático via API do Infosiga SP.*

## Tecnologias e Pacotes Utilizados
* **Linguagem:** R
* **Manipulação de Dados:** `tidyverse`, `lubridate`, `stringi`
* **Leitura de Arquivos:** `readxl`, `openxlsx`
* **Dados Espaciais:** `sf`
* **Integração Infosiga:** `ost.utils`
