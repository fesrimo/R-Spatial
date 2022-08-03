.libPaths("c:/Rlib")  ## Cambia de directorio cuando sale acceso denegado
install.packages("dplyr")
library(dplyr)
install.packages("sp")
install.packages("raster")
install.packages("BIEN")
library(BIEN)
library(sp)
install.packages("dismo")
library(dismo)
install.packages("ENMeval")
library(ENMeval)
install.packages("maxnet")
library(maxnet)
library(raster)
install.packages("rworldxtra")
library(rworldxtra)
install.packages("sf")
library(sf)
install.packages("tidyverse")
library(tidyverse)

## Descargar presencias

N_pumilio <- BIEN_occurrence_species(species = "Nothofagus pumilio")
View(N_pumilio)
## Tranformamos en SF

N_pumilio_SF <- N_pumilio %>% st_as_sf(coords = c(3, 2), 
                                       crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

N_pumilio_SF
##Generar un buffer
##Creamos un poligono convexo
Hull <- N_pumilio_SF %>% st_union() %>% st_convex_hull()

Buffer <- Hull %>% st_buffer(dist = 1) %>% st_as_sf()

## Poligono map
##Cortamos en base buffer

data("countriesHigh")
SA <- countriesHigh %>% st_as_sf() %>% st_make_valid() %>% st_crop(Buffer)

ggplot() + 
  geom_sf(data = SA) +
  geom_sf(data = N_pumilio_SF) + 
  theme_bw()

## Bajamos capas climaticas
## res=0.5 es la resolución de un kilometro

Bioclimatic <- getData(name = "worldclim", var = "bio", res = 0.5, 
                       lon = -70, lat = -50)
##plot(Bioclimatic[[1]])
## capas climaticas
## Cortamos las capas de acuerdo al Buffer
Bioclimatic <- Bioclimatic %>% crop(Buffer) %>% trim()

##names(Bioclimatic)
## Cortamos el Tile
names(Bioclimatic) <- str_remove_all(names(Bioclimatic), "_43")
##Tenemos las 4 primeras capas bioclimaticas
##plot(Bioclimatic[[c(1:4)]])
##plot(Bioclimatic[[c(1:4)]],colNA="black")

#### Generar un background
###background son los puntos de fondo
##Puntos aleatorios

set.seed(2020)

Pres <- N_pumilio %>% dplyr::select(longitude, latitude) %>% 
  mutate(Pres = 1)
###summary(Pres)
###Generamos 5000 puntos de background 
set.seed(2020)

Aus <- dismo::randomPoints(mask = Bioclimatic[[1]], n = 5000) %>% 
  as.data.frame() %>% rename(longitude = x, latitude = y) %>% 
  mutate(Pres = 0)  




Pres_Aus <- bind_rows(Pres, Aus)
Pres_Aus_Sf <- Pres_Aus %>% st_as_sf(coords = c(1, 2), 
                                     crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

###Obtenemos las condiciones para cada una de las capas bioclimaticas
Condiciones <- raster::extract(Bioclimatic, Pres_Aus_Sf) %>% 
  as.data.frame() %>% bind_cols(Pres_Aus)

##Cogemos 2 capas
ggplot(Condiciones, aes(x = bio1, y = bio12)) +
  geom_point(aes(color = factor(Pres)), alpha = 0.05) +
  geom_density2d(aes(color = factor(Pres)))

##Empezamos a modelar
## Primer modelo 
##regmult = 1 --> Multiplicador de regularización(Número)
##classes = "lqpth" --> respuestas lineales, cuadraticas, producto, trensfor y bisagra

Mod1 <- maxnet(p = Condiciones$Pres, data = Condiciones[,1:19], 
               regmult = 1, maxnet.formula(p = Condiciones$Pres, data = Condiciones[,1:19], classes = "lq"), clamp = F)


### Veamos respuestas de variables
##plot(Mod1, c("bio12"))
##plot(Mod1, c("bio1"))

plot(Mod1, type = "cloglog", c("bio1","bio2","bio3","bio12"))
plot(Mod1, c("bio2"), type = "cloglog")
plot(Mod1, c("bio2"), type = "logistic")


Mod2 <- maxnet(p = Condiciones$Pres, data = Condiciones[, 1:19], 
               regmult = 1, maxnet.formula(p = Condiciones$Pres, data = Condiciones[, 
                                                                                    1:19], classes = "l"))
Mod3 <- maxnet(p = Condiciones$Pres, data = Condiciones[, 1:19], 
               regmult = 1, maxnet.formula(p = Condiciones$Pres, data = Condiciones[, 
                                                                                    1:19], classes = "lh"))

plot(Mod2, c("bio2"))
plot(Mod3, c("bio2"))
plot(Mod3, c("bio1","bio2","bio3","bio12"))

Mod4 <- maxnet(p = Condiciones$Pres, data = Condiciones[, 1:19], 
               regmult = 1)

plot(Mod4, c("bio1","bio2","bio3","bio12"))

Prediction <- predict(Bioclimatic, Mod4, type = "cloglog")
plot(Prediction,colNA="black")
plot(Prediction)
### Transformar en Presencia Ausencia
###Verdaderos positivos y verdaderos negativos
##Queremos las coordenadas geograficas de nuestras presencias
Eval <- dismo::evaluate(p = Pres[, 1:2], a = Aus[, 1:2], model = Mod4, 
                        x = Bioclimatic, type = "cloglog")

View(Eval@confusion)

##5000 es el total de TP_TN
EvalDF <- Eval@confusion %>% as.data.frame %>% mutate(Threshold = Eval@t) %>% 
  mutate(TP_TN = (tp/nrow(N_pumilio)) + (tn/5000))


head(EvalDF)
View(EvalDF)
Prediction

EvalThres <- EvalDF %>% dplyr::filter(TP_TN == max(TP_TN))


Prediction <- Prediction %>% as("SpatialPixelsDataFrame") %>% as.data.frame() %>% mutate(Binary = ifelse(layer >= 
                                                                                                           EvalThres$Threshold, "Presencia", "Ausencia"))

View(Prediction)
plot(Prediction)
### USando ENMeval
###Seleccionamos el mejor modelo
###Tenemos que hacer un crossvalidation
###kfolds = 5 --> 5 pedacitos
###n.bg --> número de background
Results <- ENMevaluate(occ = N_pumilio[, c(3, 2)], env = Bioclimatic, 
                       RMvalues = c(0.75, 1, 1.25), n.bg = 5000, method = "randomkfold", 
                       overlap = F, kfolds = 5, bin.output = T, fc = c("L", "LQ", 
                                                                       "LQP"), rasterPreds = T)

View(Results@results)

#Evaluamos el modelo con el AUC
##avgTest_AUC
Models <- Results@results
Models$ID <- 1:nrow(Models)
Models <- Models %>% arrange(AICc)


BestModels <- Results@models[[Models$ID[1]]]
## Prediccion del mejor modelo
Prediction <- predict(Bioclimatic, BestModels, type = "cloglog") 


plot(Prediction, colNA = "black")