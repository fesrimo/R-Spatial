install.packages("ggplot2")
library(ggplot2)
install.packages("ggforce")
library(ggforce)
install.packages("ggrepel")
library(ggrepel)
install.packages("scales")
library(scales)
install.packages("sp")
library(sp)
install.packages("raster")
library(raster)
install.packages("rworldxtra")
library(rworldxtra)
install.packages("sf")
library(sf)
install.packages("tidyverse")
library(tidyverse)

Casos_Activos_Valpo <- readRDS("C:/Users/Home/Downloads/UNMSM 2022-1/Introducción a la estadística espacial/Clases 2022-1/Casos_Activos_Valpo.rds")
dim(Casos_Activos_Valpo)
View(Casos_Activos_Valpo)
# Gráfico simple
ggplot() + geom_sf(data = Casos_Activos_Valpo, aes(fill = Activos_por_100.000)) + 
  facet_wrap(~Fecha)

### Cambiamos escalas y colores.
##Puede ser: heat.colors, rainbow, terrain.colors, topo.colors y cm.colors
ggplot() + geom_sf(data = Casos_Activos_Valpo, aes(fill = Activos_por_100.000)) + 
  facet_wrap(~Fecha) + scale_fill_gradientn(name = "Activos por 100.000 hab", 
                                            colours = heat.colors(30))

###rev.- cambia la dirección de escala
## Tambien scale_fill_viridis
ggplot() + geom_sf(data = Casos_Activos_Valpo, aes(fill = Activos_por_100.000)) + 
  facet_wrap(~Fecha) + scale_fill_gradientn(name = "Activos por 100.000 hab", 
                                            colours = rev(heat.colors(30)))

####breaks numero de escalas
ggplot() + geom_sf(data = Casos_Activos_Valpo, aes(fill = Activos_por_100.000)) + 
  facet_wrap(~Fecha) + scale_fill_gradientn(name = "Activos por 100.000 hab", breaks=seq(0,200,by=25), 
                                            colours = rev(heat.colors(30)))

## Gradient2
## combina 2 gradiente, uno hacia arriba y otro hacia abajo
## mid.- dice cual es el color del punto medio
## high .- color de valor alto
## low .- color de valor bajo
## mipoint .- que valor se considera medio

ggplot() + geom_sf(data = Casos_Activos_Valpo, size = 0.05, aes(fill = Activos_por_100.000)) + 
  facet_wrap(~Fecha) + 
  scale_fill_gradient2(name = "Activos por cada 100.000 hab.",
                       mid = "white", 
                       high = ("red"),
                       low = ("blue"),
                       midpoint = median(Casos_Activos_Valpo$Activos_por_100.000))

#Le ponemos un nombre
Mapa <- ggplot() + geom_sf(data = Casos_Activos_Valpo, size = 0.05, aes(fill = Activos_por_100.000)) + 
  facet_wrap(~Fecha) + 
  scale_fill_gradient2(name = "Activos por cada 100.000 hab.",
                       mid = "white", 
                       high = ("red"),
                       low = ("blue"),
                       midpoint = 40) +
  labs(title = "Prevalencia de COVID19 en Valparaíso",
       subtitle = paste("Actualizado en", max(Casos_Activos_Valpo$Fecha)),
       caption = "Datos: https://github.com/MinCiencia/Datos-COVID19",
       y = NULL, 
       x = NULL)+
theme(legend.position = "bottom")

Mapa

## Trabajamos con Puntos
download.file("https://www.bcn.cl/obtienearchivo?id=repositorio/10221/10400/2/Toponimos.zip", 
              destfile = "Topo.zip")
unzip("Topo.zip")
Topo<-read_sf("Toponimos.shp")
head(Topo)
Topo$Region %>% unique
Topo$Clase_Topo %>% unique
dim(Topo)

Topo <- read_sf("Toponimos.shp") %>% 
  dplyr::filter(Region =="DE VALPARAISO")
dim(Topo)

plot(Topo["Nombre"])

#Saco las islas
Topo <- read_sf("Toponimos.shp") %>% 
  dplyr::filter(Region =="DE VALPARAISO") %>% 
  st_crop(Casos_Activos_Valpo)

plot(Topo["Nombre"])

Mapa + geom_sf(data = Topo)
#Nos quedamos con los centros poblados

Topo2 <- Topo %>% dplyr::filter(Clase_Topo == "Centro Poblado")
#Sus dimensiones
Topo2 %>% dim()
Mapa + geom_sf(data = Topo2)
### Filtro a solo capitales provinciales

Topo2 <- Topo %>% dplyr::filter(Nombre %in% c("Los Andes", "Quilpué", 
                                              "La Ligua", "Quillota", "San Antonio", "San Felipe", "Valparaíso"), 
                                Clase_Topo %in% c("Centro Poblado"))

Mapa + geom_sf(data = Topo2)

Mapa + geom_sf(data = Topo2) + 
  geom_sf_label(data = Topo2, 
                   aes(label = Nombre, 
                       geometry = geometry), 
                   stat = "sf_coordinates", 
                   force = 1)

## Opción 1 label
##Agregamos nombres

Mapa + geom_sf(data = Topo2) + 
  geom_label_repel(data = Topo2, 
                   aes(label = Nombre, 
                       geometry = geometry), 
                   stat = "sf_coordinates", 
                   force = 1)

## Opción 2 text

Mapa + geom_sf(data = Topo2) + 
  geom_text_repel(data = Topo2, 
                  aes(label = Nombre, 
                      geometry = geometry), 
                  stat = "sf_coordinates", 
                  force = 1)

### Para exportar
### En lugar de tiff se puede usar bmp, jpeg, png, svg, cairo_pdf

tiff("Mapa.tiff", units = "in", width = 10, height = 5, res = 600, 
     compression = "lzw")

Mapa + geom_sf(data = Topo2) + 
  geom_text_repel(data = Topo2, 
                  aes(label = Nombre, 
                      geometry = geometry), 
                  stat = "sf_coordinates", 
                  force = 1)

dev.off()

##Para conseguir el directorio donde estamos trabajando
getwd()

### Rasters

download.file("https://ndownloader.figshare.com/files/21843771", 
              destfile = "Priority.tiff")
Priority <- raster("Priority.tiff")
Priority <- readAll(Priority) 

saveRDS(Priority, "Priority.rds")

library(rgdal)

Priority <- readGDAL("Priority.tiff")
Priority <- raster("/home/derek/Downloads/NT_Plants_Verts_SSI_rcp85_mean.tif")

## Agregamos un shapefile

data("countriesHigh")
SA <- countriesHigh %>% st_as_sf() %>% st_make_valid() %>% st_crop(extent(Priority))
ggplot() + geom_sf(data = SA)


## Transformar mi raster 

## Agregar 0 en vez de NA

values(Priority) <- ifelse(is.na(values(Priority)), 0, values(Priority))

## Crear una mascara

MASK <- rasterize(SA, Priority, field = 1)

Priority <- MASK*Priority

Priority <- trim(Priority)

Priority2 <- Priority %>% as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  rename(Priority = layer) %>% 
  mutate(Priority = case_when(Priority >= 0.97 ~ "Prioridad muy alta", 
                              Priority < 0.97 & Priority > 0.87 ~ "Prioridad alta", 
                              Priority == 0 ~ "Altamente desarrollado")) %>% 
  dplyr::filter(!is.na(Priority)) %>% 
  mutate(Priority = fct_relevel(Priority, "Prioridad muy alta", "Prioridad alta"))


## Graficar primero

P <- ggplot() + geom_sf(data = SA, size = 0.1) + 
  geom_raster(data = Priority2, aes(x = x, y = y, fill = Priority))

P <- P + scale_fill_manual(name = "Priority", values = c("#006d2c", 
                                                         "#31a354", "#d7c29e"))


P <- P + ylab("") + xlab("") + theme_bw()


P + facet_zoom(xlim = c(-87.117, -82.56), 
               ylim = c(5.51, 11.21), 
               horizontal = T, 
               zoom.size = 0.8, 
               shrink = F)

tiff("Priority.tiff", units = "in", width = 10, height = 8, res = 600, 
     compression = "lzw")
P
dev.off()