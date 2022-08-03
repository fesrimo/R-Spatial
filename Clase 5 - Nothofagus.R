install.packages("leaflet")
library(leaflet)
install.packages("leaflet.extras")
library(leaflet.extras)
library(rworldxtra)
library(raster)
library(sf)
library(tidyverse)

Nothofagus <- read_csv("C:\\Users\\Usuario\\OneDrive\\Escritorio\\San Marcos\\Estadistica Espacial\\Clase5-23.05.22\\Nothofagus.csv")
## Generando un primer mapa

leaflet() %>% addTiles()

##Chile con Argentina
#Pongamos puntos en un mapa

leaflet() %>% addTiles() %>%
  addCircleMarkers(data = Nothofagus, lat = ~latitude, lng = ~longitude)

leaflet() %>% addTiles() %>%
  addCircles(data = Nothofagus, lat = ~latitude, lng = ~longitude)

## Para agregar colores
##Cambie el color según el número de especies
# Numero de especies
Number_spp <- Nothofagus$scrubbed_species_binomial %>% unique() %>% length()
Number_spp
## Nombre de las especies

Spp_Names <- Nothofagus$scrubbed_species_binomial %>% unique()
Spp_Names

## Colores de www.colorbrewer2.org
## Colores de las especies seran

Colores <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", 
             "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6")
## Generamos la paleta

pal <- colorFactor(Colores, domain = Spp_Names)

colnames(Nothofagus)
## Mapa con colores
m <- leaflet() %>% addTiles() %>% addCircles(data = Nothofagus, lat = ~latitude, 
                                             lng = ~longitude, color = ~pal(scrubbed_species_binomial), 
                                             fillOpacity = 0.8, popup = ~date_collected, label = ~scrubbed_species_binomial, 
                                             group = "Especies")
m <- leaflet() %>% addTiles() %>% addCircles(data = Nothofagus, lat = ~latitude, 
                                             lng = ~longitude, color = ~pal(scrubbed_species_binomial), 
                                             fillOpacity = 0.8, popup = ~date_collected, label = ~datasource, 
                                             group = "Especies")
m
## Generar una leyenda
## Para que aparezca la especie del punto

m <- m %>% addLegend(data = Nothofagus, "bottomright", pal = pal, 
                     values = ~scrubbed_species_binomial, title = "Especies de Nothofagus", 
                     opacity = 0.8, group = "Leyenda")

## Seleccionar capas

m <- m %>% addLayersControl(overlayGroups = c("Especies", "Leyenda"), 
                            options = layersControlOptions(collapsed = F))


### Una capa por cada especieta
##Primero generamos una base de datos para cada especie
N_antarctica <- Nothofagus %>% dplyr::filter(scrubbed_species_binomial == Spp_Names[2])

N_antarctica

leaflet() %>% addTiles() %>% addCircles(data = N_antarctica, lat = ~latitude, 
                                        lng = ~longitude, color = ~pal(scrubbed_species_binomial), 
                                        fillOpacity = 0.8, popup = ~scrubbed_species_binomial, label = ~datasource, 
                                        group = "antarctica")

## Loop para todas las especies
Spp_Names
Spp_Pres <- list()
for (i in 1:length(Spp_Names)) {
  Spp_Pres[[i]] <- Nothofagus %>% dplyr::filter(scrubbed_species_binomial == Spp_Names[i])
}
names(Spp_Pres) <- Spp_Names

### Para un mapa con un loop

Spp_Map <- leaflet() %>% addTiles()
for (i in 1:length(Spp_Pres)) {
  Spp_Map <- Spp_Map %>% addCircles(data = Spp_Pres[[i]], lat = ~latitude, 
                                    lng = ~longitude, color = ~pal(scrubbed_species_binomial), 
                                    fillOpacity = 0.5, label = ~scrubbed_species_binomial, 
                                    popup = ~datasource, group = Spp_Names[i])
}


Spp_Map <- Spp_Map %>% addLegend(data = Nothofagus, "bottomright", 
                                 pal = pal, values = ~scrubbed_species_binomial, title = "Especies", 
                                 opacity = 1, group = "Leyenda")


Grupos <- c("Leyenda", Spp_Names)

Spp_Map <- Spp_Map %>% addLayersControl(overlayGroups = Grupos,
                                        options = layersControlOptions(collapsed = TRUE))

##Densidad por espacie
## Generar un heatmap

Heat_Map <- leaflet() %>% addTiles()
for (i in 1:length(Spp_Pres)) {
  Heat_Map <- Heat_Map %>% addHeatmap(data = Spp_Pres[[i]], 
                                      lat = ~latitude, lng = ~longitude, group = Spp_Names[i], blur = 50, radius = 20)
}
Heat_Map <- Heat_Map %>% addLayersControl(baseGroups = Spp_Names, 
                                          options = layersControlOptions(collapsed = FALSE))

Heat_Map
### Trabajando con poligonos
###Ejemplo regiones de Chile
getwd()

unzip("C:/Users/Home/Downloads/UNMSM 2022-1/Introducción a la estadística espacial/Clases 2022-1/Chile.zip")

Chile <- st_read("Chile.shp") 

Chile_Spat <- Chile %>% as_Spatial()



leaflet(Chile) %>% addTiles() %>% addPolygons()


Regiones_Chile <- leaflet() %>% addTiles()  %>% 
  addPolygons(data = Chile_Spat, 
              fillColor = topo.colors(16, alpha = NULL), 
              weight = 0.5, 
              label = ~NAME_1, 
              group = "Regiones")%>% 
  addLayersControl(overlayGroups = "Regiones", options = layersControlOptions(collapsed = TRUE))


Regiones_Chile <- Regiones_Chile %>% 
  addMeasurePathToolbar(options = measurePathOptions(imperial = F,
                                                     minPixelDistance = 100, 
                                                     showDistances = T))



#### Permite dibujar areas

Regiones_Chile <- leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = Chile_Spat, 
              fillColor = topo.colors(16, alpha = NULL), weight = 0.5, 
              label = ~NAME_1, group = "Regiones") %>% 
  addDrawToolbar(targetGroup = "Regiones Marinas", 
                 editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>% 
  addLayersControl(overlayGroups = c("Regiones", "Regiones Marinas"), 
                   options = layersControlOptions(collapsed = TRUE)) %>% 
  addMeasurePathToolbar(options = measurePathOptions(imperial = F, 
                                                     minPixelDistance = 100, showDistances = T)) %>%
  addStyleEditor()




#### Mapedit

library(mapview)
library(mapedit)

Chile_SF <- Chile_Spat %>% st_as_sf()


Nuevas_Regiones <- mapview(Chile_SF) %>%
  editMap("Chile_SF")