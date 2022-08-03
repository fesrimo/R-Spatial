install.packages('foreign')
install.packages('MODIStsp')
install.packages('MODISTools')
install.packages('rasterVis')
install.packages('lattice')

library(raster)
library(sp)
library(foreign)
library(MODIStsp)
library(MODISTools)
library(lattice)
library(rasterVis)

TDF_NDVI <- mt_subset(product = "MOD13Q1",
                      lat = -54,
                      lon =  -70,
                      band = "250m_16_days_NDVI" ,
                      start = "2020-01-01",
                      end = "2020-06-01",
                      km_lr = 50,
                      km_ab = 50,
                      site_name = "Tierra del fuego",
                      internal = TRUE,
                      progress = FALSE)

#########################################
##Revisamos las bandas que tengo
bands <- mt_bands(product = "MOD13Q1")
View(bands)

#########################################
#Nos interesa los NVDI
dates <- mt_dates(product = "MOD13Q1", lat = -54, lon = -68) %>% 
  mutate(calendar_date = lubridate::ymd(calendar_date)) %>% 
  arrange(desc(calendar_date))

TDF_Raster <- mt_to_raster(df = TDF_NDVI, reproject = TRUE)

rasterVis::levelplot(TDF_Raster)

values(TDF_Raster) <- ifelse(values(TDF_Raster) < -0.2, NA, values(TDF_Raster))

library(rworldxtra)
data("countriesHigh")

TDF_SF <- countriesHigh %>% st_as_sf() %>%  st_make_valid() %>% st_crop(TDF_Raster)


TDF_Raster_DF <- TDF_Raster %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  pivot_longer(starts_with("X20"), names_to = "Fecha", values_to = "NDVI") %>% 
  mutate(Fecha = str_remove_all(Fecha, "X"), Fecha = lubridate::ymd(Fecha))

ggplot() + geom_raster(data = TDF_Raster_DF, aes(x = x, y = y, fill = NDVI)) +
  geom_sf(data = TDF_SF, alpha = 0) + theme_bw()+
  facet_wrap(~Fecha) + scale_fill_viridis_c()

TDF_Raster_DF_punto <- TDF_Raster_DF %>% dplyr::filter(!is.na(NDVI)) %>% mutate(XY = paste(x, y))

TDF_Raster_DF_punto_1 <- TDF_Raster_DF_punto %>% dplyr::filter(!is.na(NDVI)) %>% mutate(XY = paste(x, y))
View(TDF_Raster_DF_punto_1)


ggplot(TDF_Raster_DF_punto, aes(x = Fecha, y = NDVI, group = XY)) + geom_path(alpha = 0.01)


#Veo bandas

bandas <- mt_bands("MCD12Q1")


bandas <- mt_bands("MCD12Q1")
dates <- mt_dates(product = "MCD12Q1", lat = -54, lon = -68) %>% 
  mutate(calendar_date = lubridate::ymd(calendar_date)) %>% 
  arrange(desc(calendar_date))


TDF_Raster_Type <- mt_to_raster(df = TDF_Type, reproject = T)