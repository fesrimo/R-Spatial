.libPaths("c:/Rlib")  ## Cambia de directorio cuando sale acceso denegado
##########################################
## I:   Procesos Puntuales ############### 
##########################################

## Cargar Librerias utilizadas
install.packages("spatstat")
library(raster)  ##Manejo de raster
library(rgdal)   ##Lectura de .shp
library(maptools)##Para leer el shape
library(spatstat)##Modelos de PP
library(sp)	     #Manejo de datos espaciales
library(classInt)#Intervalos de clase

##########################################
##Directorio de trabajo y lectura de datos
###################################################
getwd()
setwd("C:/Users/Home/Downloads/UNMSM 2022-1/Introducción a la estadística espacial/Clases 2022-1//DataCrimen/")

WindowObs <- readOGR('.',"VentanaObsWGS84")##Ventana de observacion
pointsdelitos <- readOGR('.',"DelitosClip")##Delitos
cuadDist <- readOGR('.',"VentanaObsdistlima")##Poligonos distritales

################################################################
#### 1.2.- Lectura de puntos y patrones puntuales ##############
################################################################
#Puntos de hechos delictivos--------------------------
#Transformando a UTM 18 Sur
points.deli=spTransform(pointsdelitos,CRS("+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 

#Ventana de observacion--------------------
#Transformando a UTM 18 Sur
w.lima=spTransform(WindowObs,CRS("+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
w.lima <- as(w.lima,"SpatialPolygons")
W=as(w.lima,"owin")##Ventana de observaci?n

#Creaci?n del patron puntual
x.deli=coordinates(points.deli)[,1]
y.deli=coordinates(points.deli)[,2]
ppp.deli=ppp(x=x.deli,y=y.deli,window=W)#Patr?n puntual
unitname(ppp.deli)=c("metro", "metros")#Asignamos las unidades
ppp.deli=rescale(ppp.deli, 1000)#Cambiamos a kilometros
unitname(ppp.deli)= c("km", "kms")
plot(ppp.deli,cols="Black",border="Gray80",pch=19,cex=0.5)

#Estimaci?n de la intensidad, caso Homogeneo
summary(ppp.deli)


################################################################
#### 1.3.- Estimacion de la intensidad kernel#########################
################################################################
#Aleatoriedad Espacial Completa (CSR)
ppp.deli=rescale(ppp.deli, 1/1000)#Cambiamos a metros
unitname(ppp.deli)= c("m", "ms")
summary(ppp.deli)

#Estimacion de la densidad kernel
h=400#ms:banda de suavizado de b?squeda
xrang=ppp.deli$window$xrange; yrang=ppp.deli$window$yrange#Rango para el grid(x,y)
nx=1210;ny=944#Total de puntos en y donde se estimar? la densidad
N=nx*ny#N?mero total de puntos en donde se estimar? la densidad

###Generamos el grid bidimensional de puntos
delta.x=(xrang[2]-xrang[1])/nx	; delta.y=(yrang[2]-yrang[1])/ny
x_t0=(xrang[1]-delta.x/2)	;y_t0=(yrang[1]-delta.y/2)#Origen (x0,y0) del grid
grid.x=x_t0+(1:nx)*delta.x	;grid.y=y_t0+(1:ny)*delta.y
m <- as.mask(W, xy=list(x=grid.x, y=grid.y))
ppp1=ppp(x=x.deli,y=y.deli,window=m)
densidad.kernel <- density.ppp(ppp1,sigma=h,kernel ="gaussian",edge=FALSE,leaveoneout=FALSE,scalekernel=TRUE,diggle=TRUE)*1000000
plot(densidad.kernel,col=heat.colors(16))
raster1=raster(densidad.kernel,crs="+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

############################################################
##1.5. - Estad?sticas de Resumen############################
############################################################
ppp.deli=rescale(ppp.deli, 1000)#Cambiamos a metros
unitname(ppp.deli)= c("km", "kms")

###Funci?n de correlaci?n par funci?n - g
	#Caso homogeneo
g_fcp=pcf(ppp.deli)
op=par(mfrow=c(1,2))
plot(g_fcp,main="Funci?n - g (homog?neo)",col="Gray60",ylim=c(0,4.5),xlim=c(0,1.5))
	#Caso no homogeneo
g_inhom=pcfinhom(ppp.deli,sigma=1.5,rmax=1.5,kernel = "epanechnikov",correction ="Ripley")
plot(g_inhom,col="Gray60",main="Funci?n - g (no homogeneo)",ylim=c(0,4.5),xlim=c(0,1.5))
par(op)

####Funcion - K 
	#Caso homogeneo
functionK=Kest(ppp.deli,correction="none",rmax=1.5)#;
op=par(mfrow=c(2,2),cex.lab=0.7,cex.axis=0.7,mgp=c(1.5,0.5,0),cex=0.7)
plot(functionK,col="Gray60",main="Funci?n k- Ripley (homogeneo)")
	#Caso No Homogeneo
densidad <- density(ppp.deli, sigma=0.5,edge=FALSE)
Knohom <- Kinhom(ppp.deli,correction="none",sigma=1.5,rmax=1.5)
plot(Knohom,main="Funci?n - K (no homogeneo)",col="Gray60",xlim=c(0,1.5))

####Funcion - L
	#Caso homogeneo
Lc <- Lest(ppp.deli,correction="none",rmax=1.5)
plot(Lc, main = "Función - L (homogéneo)",col="Gray60")
	#Caso no homogeneo
Linhom <- Linhom(ppp.deli,correction="none",rmax=1.5, sigma=1.5)
plot(Linhom, main = "Función - L (no homogéneo)",col="Gray60",xlim=c(0,1.5))
par(op)


##Basado en distancias entre pares de puntos###

##Funcion - F: De espacio vacio (u a x).
Fc=Fest(ppp.deli,correction=c("km"))
##Funcion -G (x a x)
Gc=Gest(ppp.deli,correction="km")
#Funcion - J
Jc=Jest(ppp.deli,correction=c("km"),r=Gc$r)

#grafico 3 en 1
par(mfrow=c(2,2));plot(Fc);plot(Gc);plot(Jc);par(op)


##Prueba de hipotesis de aleatoriedad espacial completa
M <- quadrat.test(ppp.deli, nx = 4,ny=4);M;
plot(M)
(M$observed-M$expected)/sqrt(M$expected);M$residuals #Residuales


###########################################################
##1.6. - Modelos de procesos puntuales#####################
###########################################################

W=as(w.lima,"owin")
W=rescale(W,1000)

##1.6.1- COVARIABLES ESPACIALES
#-------------------------------------------
#--Distancia a la comisaria mas cercana-----
#-------------------------------------------
dist.comi=raster("DistanciaComisariaSerenazgo2.tif")
m.dist=as.matrix(dist.comi)/1000;n.dist=dim(m.dist)[1];m.distOrd=m.dist[n.dist:1,]
im.dist <- as.im(X = m.distOrd, W = W,eps=c(20,20))


#------------------------------------
#--Distancia al l?mite distrital-----
#------------------------------------
dist.lim=raster("DistanciaLimite31_3.tif")
m.lim=as.matrix(dist.lim)/1000;n.lim=dim(m.lim)[1];m.limOrd=m.lim[n.lim:1,]
im.lim <- as.im(X = m.limOrd, W = W,eps=c(20,20))
#-------------------------------------------
#-Presupuesto Municipal---------------------
#-------------------------------------------
dist.pres=raster("PresupuestoLimite31_2.tif")
m.pres=as.matrix(dist.pres);n.pres=dim(m.pres)[1];m.presOrd=m.pres[n.pres:1,]
im.pres <- as.im(X = m.presOrd, W = W,eps=c(20,20))

#------------------------------------------
#---Densidad poblacional-------------------
#------------------------------------------
dens=raster("DensidadPobRad1000.tif")
m.dens=as.matrix(dens);n.dens=dim(m.dens)[1];m.densOrd=m.dens[n.dens:1,]
im.dens <- as.im(X = m.densOrd, W = W,eps=c(20,20))

#####Graficando las covariables###############
#library(RColorBrewer)
colorpounto=rgb(red=.30, green=.30, blue=.30, alpha=0.9, maxColorValue = 1)
#pal.col=heat.colors(30)
pal.col=cm.colors(30)
##distancia a la comisaria
par(mar=c(0.1,0.1,0.1,2))
plot(im.dist,col=pal.col[length(pal.col):1])
plot(W, add=TRUE,border="gray30")
plot(ppp.deli, add=TRUE,pch=19,col=colorpounto,cex=0.7)
##distancia a un l?mite distrital
plot(im.lim,col=pal.col,breaks=c(0:29/25,4))
plot(W, add=TRUE,border="gray30")
plot(ppp.deli, add=TRUE,pch=19,col=colorpounto,cex=0.7)
##presupuesto
pal.col.pre=heat.colors(10)
plot(im.pres,col=pal.col.pre,breaks=c(0,10,20,30,40,60,80,100,200,400,500))
plot(W, add=TRUE,border="gray30")
plot(ppp.deli, add=TRUE,pch=19,col=colorpounto,cex=0.7)
##densidad poblacional
plot(im.dens,col=pal.col[length(pal.col):1])
plot(W, add=TRUE,border="gray30")
plot(ppp.deli, add=TRUE,pch=19,col=colorpounto,cex=0.7)

#----------------------------------------------------------
#-MODELOS ESTAD?STICOS-------------------------------------
#----------------------------------------------------------
spatstat.options(npixel=c(1210,944))
options(digits=7)

##Proceso Poisson Estacionario(homog?neo)
mod0=ppm(ppp.deli~1);summary(mod0)

#----------------------------------------------------------
#---Modelos univariados - Proceso Poisson no estacionario
#----------------------------------------------------------
##Distancia a una comisaria o serenazgo
mod1 = ppm(ppp.deli~(im.dist<0.11))##110metros
summary(mod1)

##Limite distrital
mod2 = ppm(ppp.deli~im.lim)
summary(mod2)

##Prespuesto
mod3 = ppm(ppp.deli~(im.pres>60))##Limite distrital
summary(mod3)

#Densidad poblacional
mod4 = ppm(ppp.deli~im.dens)##Limite distrital
summary(mod4)

#----------------------------------------------------------------------
#Modelos con multiples covariables - Proceso Poisson no estacionario---
#----------------------------------------------------------------------
mod5 = ppm(ppp.deli~(im.dist<0.11)+im.lim+(im.pres>60)+im.dens)
mod6 = ppm(ppp.deli~im.lim+(im.pres>60)+im.dens,eps=c(20,20))
mod7 = ppm(ppp.deli~im.lim+(im.pres>60),eps=c(20,20))
mod8 = ppm(ppp.deli~im.lim+im.dens,eps=c(20,20))

#-----------------------------------------------------------
#--Seleccion del modelo-------------------------------------
#-----------------------------------------------------------
anova(mod6, mod5, test="LR")
anova(mod7, mod6, test="LR")
anova(mod8, mod6, test="LR")
anova(mod0, mod8, test="LR")
anova(mod1, mod8, test="LR")
anova(mod2, mod8, test="LR")
anova(mod3, mod8, test="LR")
anova(mod4, mod8, test="LR")

##Valores predichos en el mejor modelo
plot(mod8, how="image", se=FALSE,ngrid = c(1210,944))

#--------------------------------------------------------
##Diagnostico de residuales------------------------------
#--------------------------------------------------------
residuals.ppm(mod8)
par(mar=c(0,0,0,0))
diagnose.ppm(mod8,type="raw",oldstyle=TRUE,sigma=NULL)

##ANEXO 1:M?todo kernel para la densidad(gaussiano estandar)
##Implementado en spatstat
spatstat.options(npixel=c(1210,944))#Cantidad de pixeles
dens.deli=density.ppp(ppp.deli,sigma=0.4#Ancho de b?squeda(b=400 metros)
,edge=FALSE,at="pixel",weights = NULL,diggle=FALSE
,dimyx=c(1210,944)#Cantidad de filas y columnas
)

################
##ANEXOS########
################

#---------------------------------------------------------
##Modelo Cluster(Neyman -Scott)with Cauchy kernel---------
#---------------------------------------------------------
mod9 <- kppm(ppp.deli~im.lim+im.dens,clusters="Cauchy")
summary(mod9)

#Graficos
par(mar=c(0,0,0,1))
plot.kppm(mod9,ngrid = c(1210,944),what=c("intensity"),col=pal.col[length(pal.col):1]
 ,cex=0.7)
par(mar=c(0,0,0,1))
plot.kppm(mod9,what=c("cluster"),locations=ppp.deli,col=pal.col[length(pal.col):1])
plot.kppm(mod9,what=c("statistic"))

