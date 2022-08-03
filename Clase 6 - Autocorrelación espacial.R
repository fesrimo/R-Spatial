install.packages("gstat")
library(stars)
library(gstat)
library(raster)
library(rgdal)
library(rworldxtra)
library(sf)
library(sp)
library(tidyverse)

## Cargar los datos de meuse
data(meuse)
## Transformamos la base de datos en un sf para poder mapearlo lo más sencillo.

Meuse <- st_as_sf(meuse, coords = c(1, 2), crs = "+init=epsg:28992")
View(Meuse)
class(Meuse)
class(meuse)
View(meuse)

###Estudiemos la concentración de zinc, particulas por millon
## Transformamos a SpatialPoints

coordinates(meuse) = ~x + y
proj4string(meuse) <- CRS("+init=epsg:28992")

# graficamos concentración de zinc

ggplot() + geom_sf(data = Meuse, aes(color = zinc)) + 
  scale_color_viridis_c() + theme_bw()

### Partimos con un semi-variograma
# Modelo nulo
#Log es para que no genere valores negativos
Z_vgm_null <- variogram(log(zinc) ~ 1, meuse) %>% mutate(Modelo = "Nulo")
Z_vgm_null
plot(Z_vgm_null)

# Modelo espacial
Z_vgm_Spat <- variogram(log(zinc) ~ x + y, meuse) %>%
  mutate(Modelo = "Espacial")
plot(Z_vgm_Spat)

# Modelo distancia

Z_vgm_Dist <- variogram(log(zinc) ~ dist, meuse) %>% 
  mutate(Modelo = "distancia")
plot(Z_vgm_Dist)

## Modelo sqrt Dist
Z_vgm_Dist_sq <- variogram(log(zinc) ~ sqrt(dist), meuse) %>% 
  mutate(Modelo = "sqrt(dist)")
plot(Z_vgm_Dist_sq)

View(Z_vgm_Dist_sq)

# Unir todos esos modelos
Z_vgm <- list(Z_vgm_Dist, Z_vgm_null, Z_vgm_Spat, Z_vgm_Dist_sq) %>% 
  reduce(bind_rows)
## Plotear con ggplot

ggplot(Z_vgm_Dist, aes(x = dist, y = gamma)) + 
  geom_point() + theme_bw()

# Para todos juntos
ggplot(Z_vgm, aes(x = dist, y = gamma)) + 
  geom_point(aes(color = Modelo)) + theme_bw()

### Ajuste para ver el variograma

Abn_fit_null <- fit.variogram(Z_vgm_null, model = vgm(1, "Sph",700, 1))
Abn_fit_null
Abn_fit_Spat <- fit.variogram(Z_vgm_Spat, model = vgm(1, "Sph",700, 1))
Abn_fit_Spat
Abn_fit_Dist <- fit.variogram(Z_vgm_Dist, model = vgm(1, "Sph",700, 1))
Abn_fit_Dist
Abn_fit_Dist_sq <- fit.variogram(Z_vgm_Dist_sq, model = vgm(1,"Sph", 700, 1))
Abn_fit_Dist_sq

## Tenemos nuestro grid
#Cogemos la base de datos meuse.grid
data(meuse.grid)
Meuse_Grid <- st_as_sf(meuse.grid, coords = c(1,2), crs = "+init=epsg:28992")
ggplot()+geom_sf(data=Meuse_Grid,aes(color=dist))+scale_color_viridis_c()
## Modelo Kriging
#Predicción

#La función krige es para hacer un modelo
Null_pred <- krige(log(zinc) ~ 1, Meuse, Meuse_Grid, model = Abn_fit_null) %>% 
  mutate(Modelo = "Nulo")
## [using ordinary kriging]
Spat_pred <- krige(log(zinc) ~ 1, Meuse, Meuse_Grid, model = Abn_fit_Spat) %>% 
  mutate(Modelo = "Espacial")
## [using ordinary kriging]
Dist_pred <- krige(log(zinc) ~ 1, Meuse, Meuse_Grid, model = Abn_fit_Dist) %>% 
  mutate(Modelo = "distancia")
## [using ordinary kriging]
Dist_sq_pred <- krige(log(zinc) ~ 1, Meuse, Meuse_Grid, model = Abn_fit_Dist_sq) %>% 
  mutate(Modelo = "sqrt(dist)")

View(Null_pred)
View(Dist_sq_pred)

## Juntando predicciones
Pred <- list(Null_pred, Spat_pred, Dist_pred, Dist_sq_pred) %>% reduce(bind_rows)
View(Pred)

ggplot() + geom_sf(data = Pred, aes(color = exp(var1.pred))) + scale_color_viridis_c(name="[Zinc]") + 
  facet_wrap(~Modelo) + theme_bw()

## Observando predicciones

ggplot() + geom_sf(data =Spat_pred, aes(color = exp(var1.pred))) + scale_color_viridis_c(name = "Concentración Zinc")

## Observando varianza
ggplot() + geom_sf(data =Spat_pred, aes(color = var1.var)) + scale_color_viridis_c()

## Observando todos los modelos
ggplot() + geom_sf(data = Pred, aes(color = exp(var1.pred))) + scale_color_viridis_c() + 
  facet_wrap(~Modelo) + theme_bw()

######################################################
###################Seleccion de modelos por cv#######
#####################################################
#####################################################
Null_CV <- krige.cv(log(zinc) ~ 1, Meuse, model = Abn_fit_null, nfold = 5) %>% 
  st_as_sf() %>% mutate(Modelo = "Nulo")

Spat_CV <- krige.cv(log(zinc) ~ x+y, meuse, model = Abn_fit_Spat, 
                    nfold = 5) %>% st_as_sf() %>% mutate(Modelo = "Espacial")
Dist_CV <- krige.cv(log(zinc) ~ dist, Meuse, model = Abn_fit_Dist, 
                    nfold = 5) %>% st_as_sf() %>% mutate(Modelo = "distancia")
Dist_sq_CV <- krige.cv(log(zinc) ~ sqrt(dist), Meuse, model = Abn_fit_Dist_sq, 
                       nfold = 5) %>% st_as_sf() %>% mutate(Modelo = "sqrt(dist)")

Pred_CV <- list(Null_CV, Spat_CV, Dist_CV, Dist_sq_CV) %>% reduce(bind_rows)
View(Pred_CV)

Resumen <- Pred_CV %>% as.data.frame() %>% group_by(Modelo) %>% 
  summarise(RMSE = sqrt(sum(residual^2)/length(residual))) %>% arrange(RMSE)
Resumen
### Diagnosticos

ggplot(Dist_CV, aes(x = observed, y = var1.pred)) + geom_smooth(method = "lm")+ geom_point()
Var <- variogram(residual ~ 1, Dist_CV)

ggplot(Var, aes(x = dist, y = gamma)) + geom_point() + theme_bw() + 
  xlab("Distancia metros") + ylim(c(0, max(Var$gamma)))
ggplot() + geom_sf(data = Dist_pred, aes(color = exp(var1.pred))) +
  scale_color_viridis_c() 
