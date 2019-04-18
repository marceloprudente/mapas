# Criar highchart map a partir de shapefiles

# baixar bibliotecas
library(rgdal)
library(spdplyr)
library(geojsonio)
library(rmapshaper)
library(highcharter)
library(tidyverse)
library(magrittr)
# baixar shapefile
county <- readOGR(dsn = path.expand("~/mapas/dados/mun"), layer = "municipios_2010", 
                  verbose = FALSE)

# manipulando bases com spdplyr
county_id =  county %>% mutate(id2 = as.character(id))

# transformar em json
# no hichchart, só funciona se for uma lista
mun_list <- geojsonio::geojson_list(county_id)

# Mapa de Sergipe  
# filtrar base com spdplyr
se_map =  county %>% mutate(id2 = as.character(id),
                            uf = as.character(uf)) %>% 
  filter(uf == "SE")

# transformar em json
# no hichchart, só funciona se for uma lista
se_list <- geojsonio::geojson_list(se_map)

# criar mapa com info do shapefile
se <- as.data.frame(se_map) %>% 
  mutate_all(as.character) %>% 
  mutate_at(vars(populacao, pib), as.numeric) %>% 
  mutate(pop_faixas = cut(populacao,c(0, 15000, 50000, 100000, 1000000, 1500000),
                          labels = c("Até 15k", "Entre 15k e 50k",
                                     "Entre 50k e 100k", "Entre 100k e 1m",
                                     "Acima de 1m")))
# baixar chave dos municipios
chave <- readxl::read_excel("~/dados/chave_municipios.xlsx") %>% 
  mutate(CO_MNICDV = as.character(CO_MNICDV))

# mesclar tabelas
se <- inner_join(se, chave, by = c("codigo_ibg" = "CO_MNICDV"))


## MAPA 1 - SERGIPE - com rotulos municipios
highchart() %>% 
  hc_add_series_map(se_list,
                    se,
                    joinBy =c("id2",  "id"),
                    value = "populacao",
                    name = "População",
                    borderColor = "#FAFAFA",
                    borderWidth = 0.1,
                    # para identificar os rotulos, temos que ver as propriedades
                    # da lista: se_list$features[[1]]$properties
                    # no caso dos dados do mapa, variavel chama-se DS_NOM
                    # Por isso, especificamos point.DS_NOM
                    dataLabels = list(enabled = TRUE, format = '{point.DS_NOM}'),
                    # a mesma coisa para os labels interativos, alteramos o
                    # point.format
                    tooltip = list(valueDecimals = 0, 
                                   pointFormat = ' {point.DS_NOM} - {point.populacao}  ')) %>% 
  hc_title(text = "Sergipe - população municipal") %>%
  hc_colorAxis(dataClasses = color_classes(c(0, 15000, 50000, 100000, 500000, 1000000))) %>% 
  hc_legend(layout = "vertical", align = "right", valueDecimals = 0) 

# MAPA 2 - SERGIPE sem títulos

# cores do mapa
cols <- ggsci::pal_startrek()(6) 

highchart() %>% 
  hc_add_series_map(se_list,
                    se,
                    joinBy =c("id2",  "id"),
                    value = "populacao",
                    name = "População",
                    borderColor = "#FAFAFA",
                    borderWidth = 0.1,
                    # a mesma coisa para os labels interativos, alteramos o
                    # point.format
                    tooltip = list(valueDecimals = 0, 
                                   pointFormat = ' {point.DS_NOM} - {point.populacao}  ')) %>% 
  hc_title(text = "Sergipe - população municipal") %>%
  hc_colorAxis(  minColor = "#BC3C29", maxColor = "#434348",
                 dataClasses = color_classes(breaks = c(0, 15000, 50000, 100000, 500000, 1000000), 
                                             colors = cols)) %>% 
  hc_legend(layout = "vertical", align = "right", valueDecimals = 0) 

