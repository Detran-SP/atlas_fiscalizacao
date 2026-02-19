# atlas_fiscalizacao / Painel das superintendências
# Análise da sinistralidade por Superintendências do DentranSP`no Estado de São Paulo

Este repositório contém um script em linguagem R focado no processamento, análise e visualização de dados de sinistros de trânsito no Estado de São Paulo. O código automatiza a geração de gráficos e rankings (exportados em formato SVG) segmentados por Superintendências, divisão organizacional do departamento pelo estado.

## Sobre os Dados (Open Data)
Para garantir a reprodutibilidade e evitar o armazenamento de arquivos pesados, este projeto **não** hospeda as bases de dados brutas de sinistros, veículos e pessoas. 

Em vez disso, o script utiliza o pacote `ost.utils` para baixar automaticamente os microdados mais recentes diretamente da base do **Infosiga SP** para um diretório temporário (`tempdir()`) durante a execução. Ao finalizar o R, esses arquivos pesados são descartados automaticamente da sua máquina.

### Arquivos Auxiliares Locais
Os únicos arquivos estáticos necessários para rodar este projeto são:
* `bd_municipios.xlsx`: Base de de-para relacionando municípios às suas respectivas Superintendências.
* `distrito_municipal_v2.shp` e `regioes_municipais.xls` (Opcionais): Arquivos de shapefile e regiões para a geração de análises espaciais detalhadas na capital.

## 🛠️ Tecnologias e Pacotes Utilizados
* **Linguagem:** R
* **Manipulação de Dados:** `tidyverse`, `lubridate`, `stringi`
* **Leitura de Arquivos:** `readxl`, `openxlsx`
* **Dados Espaciais:** `sf`
* **Integração Infosiga:** `ost.utils`
