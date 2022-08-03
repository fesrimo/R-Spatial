install.packages("ggforce")
install.packages("ggrepel")
install.packages("curl") 
library(ggplot2)
library(ggforce)
library(ggrepel)
library(scales)
library(sp)
library(raster)
library(rworldxtra)
library(sf)
library(tidyverse)
library(curl)

Casos_Activos_valpo <- readRDS('C:/Users/Usuario/OneDrive/Escritorio/San Marcos/Estadistica Espacial/Clase 06.06/Casos_Activos_Valpo.rds')
dim(Casos_Activos_valpo)

#Grafico Simple
ggplot()+ geom_sf(data= Casos_Activos_valpo, aes(fill = Activos_por_100.000)) +
  facet_wrap(~Fecha)

#Cambiamos escalas y colores.
## Puede ser: Heat.colors, rainbow, terrain.colors, topo.colors y cm.colors
ggplot()+ geom_sf(data= Casos_Activos_valpo, aes(fill = Activos_por_100.000)) +
  facet_wrap(~Fecha) + scale_fill_gradientn(name = "Activos por 100.000 hab",
                      colours = heat.colors(30))


ggplot()+ geom_sf(data= Casos_Activos_valpo, aes(fill = Activos_por_100.000)) +
  facet_wrap(~Fecha) + scale_fill_gradientn(name = "Activos por 100.000 hab",
                       colours = rainbow(10))

##Rev cambia la direccion de escala
## Tambien scale_fill_virids
ggplot()+ geom_sf(data= Casos_Activos_valpo, aes(fill = Activos_por_100.000)) +
  facet_wrap(~Fecha) + scale_fill_gradientn(name = "Activos por 100.000 hab",
                                            colours = rev(heat.colors(30)))

##breaks numero de escalas
ggplot()+ geom_sf(data= Casos_Activos_valpo, aes(fill = Activos_por_100.000)) +
  facet_wrap(~Fecha) + scale_fill_gradientn(name = "Activos por 100.000 hab", breaks=seq(0,200,by=25),
                                            colours = rev(heat.colors(30)))

## high - color de valor alto
## low  - color de valor bajo
## midpoint - que valor se condiera medio

ggplot()+ geom_sf(data= Casos_Activos_valpo,size=0.05, aes(fill = Activos_por_100.000)) +
  facet_wrap(~Fecha) +
   scale_fill_gradient2(name = "Activos por 100.000 hab",
                        mid="white",
                        high = ("red"),
                        low = ("blue"),
                        midpoint = median (Casos_Activos_valpo$Activos_por_100.000))

##Colocando mas detalles a los graficos

ggplot()+ geom_sf(data= Casos_Activos_valpo,size=0.05, aes(fill = Activos_por_100.000)) +
  facet_wrap(~Fecha) +
  scale_fill_gradient2(name = "Activos por 100.000 hab",
                       mid="white",
                       high = ("red"),
                       low = ("blue"),
                       midpoint = 40)+
  labs(title = "Prvalencia de COVID19 en valparaiso",
       subtitle = paste("Actualizado en", max(Casos_Activos_valpo$Fecha)),
       caption= "Datos: https://github.com/MinCiencia/Datos-COVID19",
       y=NULL,
       x=NULL)

#######creando el mapa
Mapa<-ggplot()+ geom_sf(data= Casos_Activos_valpo,size=0.05, aes(fill = Activos_por_100.000)) +
  facet_wrap(~Fecha) +
  scale_fill_gradient2(name = "Activos por 100.000 hab",
                       mid="white",
                       high = ("red"),
                       low = ("blue"),
                       midpoint = 40)+
  labs(title = "Prvalencia de COVID19 en valparaiso",
       subtitle = paste("Actualizado en", max(Casos_Activos_valpo$Fecha)),
       caption= "Datos: https://github.com/MinCiencia/Datos-COVID19",
       y=NULL,
       x=NULL)
### TRABAJANDO CON MAPAS
download.file("https://www.bcn.cl/obtienearchivo?id=repositorio/10221/10400/2/Toponimos.zip", destfile= "Topo.zip")
unzip("Topo.zip")
Topo<-read_sf("Toponimos.shp")
head(Topo)

Topo<-Read_sf("Toponimos.shp") %>%
  dplyr::filter(Region == "DE VALPARAISO")
dim(Topo["Nombre"])

#Saco las islas
Topo <- read_sf("Toponimos.shp") %>%
  dplyr::filter(Region == "DE VALPARAISO") %>%
  st_crop(Casos_Activos_valpo)

plot(Topo["Nombre"])

Mapa + geom_sf(data = Topo)

### Reduciendo las cantidad de puntos
Topo2 <- Topo %>% dplyr::filter(Clase_Topo == "Centro Poblado")

Topo2 %>% dim()          

Mapa + geom_sf(data = Topo2)

##### Filtro a solo capitales pronviciales
Topo2 <- Topo %>% dplyr::filter(Nombre %in% c("Los Andes", "Quilpué", 
                                              "La Ligua", "Quillota", "San Antonio", "San Felipe", "Valparaíso"), 
                                Clase_Topo %in% c("Centro Poblado"))

Mapa + geom_sf(data = Topo2)

################################
# Opcion 1 label
# Agregamos nombres
Mapa + geom_sf(data = Topo2) +
  geom_label_repel(data=Topo2,
                   aes(label = Nombre,
                       geometry = geometry),
                   stat = "sf_coordinates",
                   force = 1)
################################
# Opcion 1 label
# Agregamos nombres
Mapa + geom_sf(data = Topo2) +
  geom_sf_label(data=Topo2,
                   aes(label = Nombre,
                       geometry = geometry),
                   stat = "sf_coordinates",
                   force = 1)
##############################utiliando solo el texto
Mapa + geom_sf(data = Topo2) +
  geom_text(data=Topo2,
                aes(label = Nombre,
                    geometry = geometry),
                stat = "sf_coordinates",
                force = 1)
##############################UTILIZANDO REPEL (MAS ORDEN)
Mapa + geom_sf(data = Topo2) +
  geom_text_repel(data=Topo2,
            aes(label = Nombre,
                geometry = geometry),
            stat = "sf_coordinates",
            force = 1)

#################SI QUEREMOS EXPORTAR (PARA UN PAPER)
tiff("Mapa.tiff", units = "in", width = 10, height = 5, res = 600,
     compression = "lzw")

Mapa + geom_sf(data = Topo2) +
  geom_text_repel(data=Topo2,
                  aes(label = Nombre,
                      geometry = geometry),
                  stat = "sf_coordinates",
                  force = 1)
dev.off()
getwd()
