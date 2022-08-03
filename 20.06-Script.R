###¿Que son los SDM (Species Distribution Models)?

#Objectives

#Entender que modelan las SDM
#
#
#
#


#Es una distribucion potencial, no real, 

install.packages("dplyr") ### YA INSTALADA
library(dplyr)

install.packages("sp")  ### YA INSTALADA
library(sp)

install.packages("raster")  ### YA INSTALADA
library(raster)

install.packages("BIEN")  ### YA INSTALADA
library(BIEN)

install.packages("dismo") ### YA INSTALADA
library(dismo)

install.packages("ENMeval") ### YA INSTALADA
library(ENMeval)

install.packages("maxnet") ### YA INSTALADA
library(maxnet)

library(rworldxtra)
library(sf)
library(tidyverse)

###Descargando Presencias
N_pimilio <- BIEN_occurrence_species(species = "Nothofagus pumilio")
BIEN_occurrence_state()

Hull <- N_pumilio_SF %>% st_union() %>% st_convex_hull()


data("countriesHigh")
SA <- countriesHigh %>% st_as_sf() %>% st_make_valid() %>% st_crop(Buffer)

ggplot()+
  geom_sf(data = SA) +
  geom_sf(data = N_pimilio_SF) +
  theme_bw()

## Bajamos capas climaticas
Bioclimatic <- getData(name = "worldclim", var="bio", res=0.5,
                        lon= -70, lat= -50)
plot(Bioclimatic[[2]])

#Cortamos las capas de acuerdo al buffer
Bioclimatic <- Bioclimatic %>% crop(Buffer) %>% trim()
names(Bioclimatic)

## Cortamos el Tile
names(Bioclimatic) <- str_remove_all(names(Bioclimatic),"_43")
names(Bioclimatic)

#Tenemos las 4 primeras capas bioclimaticas
plot(Bioclimatic[[c(1:4)]],colNA="black")

### Generar un background
### puntos aleatorios
set.seed(2020)
Pres <- N_pumilio %>% dplyr::select(longitude, latitude) %>% 
  mutate(Pres = 1)

### puntos generando 5000 puntos de background
set.seed(2020)
Aus <- dismo::randomPoints(mask = Bioclimatic[[1]], n = 5000) %>% 
  as.data.frame() %>% rename(longitude = x, latitude = y) %>% 
  mutate(Pres = 0)  

Pres_Aus <- bind_rows(Pres,Aus)

Pres_Aus_Sf <- Pres_Aus %>% st_as_sf(coords = c(1, 2), 
                                     crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

### Obtenemos las condiciones para cada una de las capas bioclimaticas
###Son capas bioclimaticas por eso usamos raster
Condiciones <- raster::extract(Bioclimatic, Pres_Aus_Sf) %>% 
  as.data.frame() %>% bind_cols(Pres_Aus)

### Cogemos 2 capas
ggplot(Condiciones, aes(x = bio1, y = bio12)) +
  geom_point(aes(color = factor(Pres)), alpha = 0.05) +
  geom_density2d(aes(color = factor(Pres)))


Mod1 <- maxnet(p = Condiciones$Pres, data = Condiciones[,1:19], 
               regmult = 1, maxnet.formula(p = Condiciones$Pres, data = Condiciones[,1:19], classes = "lq"), clamp = F)


plot(Mod1,c("bio12"))
plot(Mod1,c("bio2"))

plot(Mod1,type = "clogclog", c("bio1","bio2","bio3","bio12"))