#Got forest age raster from here: https://opendata.nfis.org/mapserver/nfis-change_eng.html
#It was generated form this study: https://www.sciencedirect.com/science/article/pii/S0034425723000809?via%3Dihub

#SETUP ----

library(terra)
library(here)
library(sf)
library(mapview)
library(raster)
library(ggplot2)
library(tidyverse)

#Load ROI
roi <- st_read(here("data/roi.gpkg"))

# AGE ----

#Read the age raster reduced to roi buffer
age_ras <- rast("C:/Users/hseely/Downloads/CA_forest_age_2019/CA_forest_age_2019.tif") 

#Buffer the roi by 5k and convert to ras CRS
roi_buff <- st_buffer(roi, 10) %>% 
  vect() %>% 
  terra::project(terra::crs(age_ras))

#Get the extent of ROI buf
roi_buf_ext <- ext(roi_buff)

#Crop the age raster to the extent of the ROI buffer
age_ras_crop <- crop(age_ras, roi_buf_ext) %>%
  mask(roi_buff) %>%
  project(terra::crs(vect(roi)))

#Add five years to forest age (since data is from 2019)
age_ras_crop <- age_ras_crop + 5

#View
plot(age_ras_crop)
plot(roi, add=T, border = "red")

mapview(raster(age_ras_crop), col.regions = rev(terrain.colors(10)))

#EXport age raster
writeRaster(age_ras_crop, here("data/age_ras.tif"), overwrite = T)


