.libPaths("c:/Rlib")  ## Cambia de directorio cuando sale acceso denegado
# Load libraries ----------------------------------------------------------
install.packages("pacman")
require(pacman)
pacman::p_load(raster, showtext, rgdal, rgeos, stringr, sf, rJava, readxl, tidyverse, fs, gtools, rgeoda)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Add font ----------------------------------------------------------------
font_add_google(family = 'Fira', name = 'Fira code')
font_add_google(family = 'Roboto', name = 'Roboto condensed' )
showtext_auto()

getwd()
setwd("C:/Users/Home/Downloads/UNMSM 2022-1/Introducción a la estadística espacial/Clases 2022-1")
# Load data ---------------------------------------------------------------
shpf <- st_read('shp/homicidios_barrios_2019.gpkg')
cmns <- st_read('shp/comunas.gpkg')
cmns <- st_transform(cmns, crs = st_crs(4326))
mpio <- st_read('shp/mpios_valle.gpkg')

plot(st_geometry(shpf))
plot(st_geometry(cmns))
plot(st_geometry(mpio))
shpf
# Moran analysis ----------------------------------------------------------
##Por barrios
shpf <- mutate(shpf, gid = 1:nrow(shpf)) ##Agrego identificador único
qnwg <- queen_weights(shpf, order = 1)  ###Peso tipo reyna, con cuantos vecinos limita cada poligono a orden 1
morn <- local_moran(qnwg, st_drop_geometry(shpf['y2019'])) ##Calculamos las etiquetas

mran_lbls <- lisa_labels(morn)
mran_lbls
###Not significant .- p-valor > 0.05, No existe suficiente evidencia estadística
###para decir que si existe autocorrelación espacial con los vecinos
###"High-High"  - Polígono con altos valores en la variable a analizar (homicidios) en su poligono 
###y a su alrededor también se da altos homicidios
###"Low-Low"         
##"Low-High"       
 
mran_clrs <- setNames(lisa_colors(morn), mran_lbls)   ###Colores a utilizar
mran_clrs         #Código html

shpf_clst <- shpf %>%                 ##Le agregamos el tipo de cluster
  st_drop_geometry() %>%
  select(gid) %>%
  mutate(cluster_num = lisa_clusters(morn) + 1, # add 1 bc clusters are zero-indexed
         cluster = factor(mran_lbls[cluster_num], levels = mran_lbls)) %>%
  right_join(shpf, by = "gid") %>%
  st_as_sf() %>% 
  st_transform(x = ., crs = st_crs(4326))  ##Lo transformamos a un sistema de coordenas gegráfico wGS1984

# A simple map
ggplot(shpf_clst, aes(fill = cluster)) +         
  geom_sf(color = "white", size = 0) +
  scale_fill_manual(values = mran_clrs, na.value = "green") +
  theme_dark()

mpios <- st_read('shp/MGN_MPIO_POLITICO.shp')
mpios <- filter(mpios, DPTO_CNMBR == 'VALLE DEL CAUCA')

# A nice map
ggm <- ggplot() +                 ###Municipios de Cali
  geom_sf(data = shpf_clst, aes(fill = cluster), color = 'white', size = 0) + 
  scale_fill_manual(values = mran_clrs, na.value = 'green') +
  geom_sf(data = cmns, col = 'grey10', fill = NA) +
  geom_sf_text(data = cmns, aes(label = COMUNA), size = 10, col = '#76766F', family = 'Roboto') +
  geom_sf(data = mpio, fill = NA, col = 'grey10') +
  theme_bw() + 
  coord_sf(xlim = extent(cmns)[1:2], ylim = extent(cmns)[3:4]) +
  ggtitle(label = 'Análisis LISA - Cantidad de homicidios en Cali', 
          subtitle = 'Año 2019') +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Cluster', caption = 'Centro de Investigación y Documentación Socioeconómica (CIDSE)') +
  theme(axis.text.x = element_text(size = 19, family = 'Roboto'), 
        axis.text.y = element_text(size = 19, family = 'Roboto', angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 26, family = 'Roboto'), 
        axis.title.y = element_text(size = 26, family = 'Roboto'),
        legend.text = element_text(size = 19, family = 'Roboto'),
        plot.title = element_text(size = 30, face = 'bold', hjust = 0.5, family = 'Roboto'),
        plot.subtitle = element_text(size = 30, face = 'bold', hjust = 0.5, family = 'Roboto'),
        legend.title = element_text(size = 26, family = 'Roboto', face = 'bold'),
        legend.position = c(0.815, 0.2),
        panel.background = element_rect(fill = "white"),
        plot.caption = element_text(size = 10, face = 'bold', family = 'Roboto'),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.1))  

ggsave(plot = ggm, 
       filename = 'map_s11_moran.png', 
       units = 'in', 
       width = 5, height = 7, dpi = 300)  
