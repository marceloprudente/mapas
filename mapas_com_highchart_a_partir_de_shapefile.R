# Criar highchart map a partir de shapefiles

# baixar bibliotecas
library(rgdal)
library(spdplyr)
library(geojsonio)
library(rmapshaper)
# baixar shapefile
county <- readOGR(dsn = "~/mapas", layer = "municipios_2010", verbose = FALSE)

# manipulando bases com spdplyr
county_id =  county %>% mutate(id2 = as.character(id))

# transformar em json
# no hichchart, só funciona se for uma lista
mun_list <- geojsonio::geojson_list(county_id)

# depois, vamos recuperar os dados do shapefile e fazer algumas transformações
pop <- as.data.frame(county) %>% 
  mutate_all(as.character) %>% 
  mutate_at(vars(populacao, pib), as.numeric) %>% 
  mutate(pop_faixas = cut(populacao,c(0, 15000, 50000, 100000, 1000000, Inf),
                          labels = c("Até 15k", "Entre 15k e 50k",
                                    "Entre 50k e 100k", "Entre 100k e 1m",
                                    "Acima de 1m")))

# dar uma olhada nos dados
glimpse(pop)


# Mapa  
# Variável contínua
library(highcharter)
highchart() %>% 
  hc_add_series_map(mun_list,
                pop,
                joinBy =c("id2",  "id"),
                value = "populacao",
                name = "População",
                borderColor = "red",
                borderWidth = 0.2) %>% 
  hc_title(text = "Brasil - população municipal") %>%
# Por categoria
highchart() %>% 
  hc_add_series_map(mun_list,
                    pop,
                    joinBy =c("id2",  "id"),
                    value = "populacao",
                    name = "População",
                    borderColor = "#FAFAFA",
                    borderWidth = 0.1) %>% 
  hc_title(text = "Brasil - população municipal") %>%
  hc_colorAxis(dataClasses = color_classes(c(0, 15000, 50000, 100000, 1000000, 15000000))) %>% 
  hc_legend(layout = "vertical", align = "right", valueDecimals = 0) 

  

