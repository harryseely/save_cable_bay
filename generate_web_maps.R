# Get packages

library(sf)
library(mapview)
library(osmdata)
library(tidyverse)
library(here)
library(raster)
library(terra)
library(webshot)

# Generate map of key boundaries ----

#Load parcels to protect
parcels <- st_read("data/parcels_to_protect.gpkg")

#Set main CRS for project
main_crs <- st_crs(parcels)

#Load Join Point Park
join_point_park <- st_read("data/Joan Point Park/Joan Point Park.shp") %>%
  st_transform(crs = main_crs)

#Read open street map lines
lines <- st_read("data/osm_lines/osm_lines.shp")%>%
  st_transform(crs = main_crs)

#Extract the cable bay trail lines
cb_trail <- lines %>% 
  filter(name == "Cable Bay Trail")

#Add 100m buffer to trail
trail_buffer <- st_buffer(cb_trail, 100) %>%
  group_by() %>%
  summarize()

#Set unique IDs for each feature
parcels$id <- paste0("parcel_", 1:nrow(parcels))
join_point_park$id <- paste0("joan_point_park_", 1:nrow(join_point_park))
trail_buffer$id <- "100m Trail Buffer"

#Convert parcels to single polygon
parcels_single_poly <- parcels %>% 
  group_by() %>% 
  summarize() %>%
  st_cast("POLYGON") %>%
  rename(geometry = geom)

#Combine adn dissolve
roi <- dplyr::bind_rows(list(parcels_single_poly, join_point_park, trail_buffer)) %>%
  group_by() %>%
  summarize() %>%
  st_buffer(dist = 0.1) %>%
  group_by() %>%
  summarize()

#View ROI with transparent fill and red outline
m <- mapview(roi, 
             col.regions = NA, 
             alpha.regions = 0, 
             col = "red", 
             map.types="Esri.WorldImagery", 
             lwd = 1.7,
             layer.name = "ROI"
             )

#Clip additional OSM lines to ROI
other_trails <- st_intersection(lines, roi)

#Line feature is not clipped to polygon
other_trails <- st_cast(other_trails, "LINESTRING")

#Remove Cable Bay Trail from lines
other_trails <- other_trails %>% 
  filter(name != "Cable Bay Trail" | is.na(name))

#Load the age ras
age_ras <- raster(here("data/age_ras.tif"))

#Parcel map
parcel_map <- mapview(parcels, 
                      alpha.regions=0.1, 
                      lwd = 2,
                      color = "grey",
                      layer.name = "Parcels"
)

buffer_map <-   mapview(trail_buffer, 
                        layer.name = "100m Buffer",
                        alpha.regions=0.3, 
                        col.regions="green") 

#View buffer
final_map <- m + 
  
  parcel_map + 
  
  mapview(join_point_park, 
          alpha.regions=0.3, 
          col.regions = "#006400",
          layer.name = "Joan Point Park") +
  
  mapview(cb_trail, 
          color  = "orange",
          layer.name = "Cable Bay Trail") + 
  
  buffer_map + 
  
  mapview(other_trails, 
          color = "brown",
          layer.name = "Other Trails",
          lwd = 1.5
          ) 

final_map

# Forest age map ----

#Add forest age map
forest_age_map <- m + 
  mapview(age_ras, 
          col.regions = rev(terrain.colors(10)[-10]),
          layer.name = "Forest Age (years)",
          legend=TRUE,
          at = seq(0, 150, 25),
          na.color = "transparent"
  )

forest_age_map

# Canopy height map ----

#Add canopy height map
chm <- rast(here("data/chm.tif")) %>%
  #Downsample
  terra::aggregate(8) 

zmax <- terra::global(chm, max, na.rm = T)

#Separate CHM into discrete/continuous
chm_discrete <- terra::classify(chm, c(0, 20, 55)) %>%
  raster()

chm_continuous <- raster(chm)

#Set NA values to 0
chm_map <- m + 
  mapview(chm_continuous, 
          col.regions = terrain.colors(10, rev=T),
          layer.name = "Forest Canopy Height (m)",
          legend=TRUE,
          at = c(0, 5, 10, 20, 30, 55),
          na.color = "transparent"
  )

chm_map

# Hydrology map -----

#Read the nanaimo boundary for coastline
nanaimo <- st_read(here("data/nanaimo_boundary/nearby_nanaimo_boundary.shp")) %>%
  st_transform(crs = main_crs)

streams <- st_read(here("data/hydrology/streams.shp")) %>%
  st_transform(crs = main_crs) %>%
  st_intersection(roi) %>%
  mutate(`Stream Size` = case_when(
    grid_code == 1 ~ "Small",
    grid_code == 2 ~ "Medium",
    grid_code == 3 ~ "Large"
  )) %>%
  mutate(buf_dist = case_when(
    `Stream Size` == "Small" ~ 1.5,
    `Stream Size` == "Medium" ~ 3,
    `Stream Size` == "Large" ~ 6
  ))

#Apply a conditional buffer to the streams
streams <- st_buffer(streams, dist = streams$buf_dist, nQuadSegs=100) %>%
  group_by(`Stream Size`) %>%
  summarize()
  

#Load the drainage baisins
baisins <- rast(here("data/hydrology/basin.tif")) %>%
  terra::aggregate(4) %>%
  mask(roi) %>%
  crop(roi)

baisins <- round((baisins/100) - 28, 0) %>%
  #Convert to categorical data type
  as.factor() %>%
  raster()

plot(baisins, col = rainbow(100))

baisins_mask <- raster::reclassify(baisins, c(-Inf, Inf, 1))

plot(baisins, col = rainbow(100))

hydrology_map <- m +
  
  mapview(baisins_mask, 
          col.regions = "black",
          layer.name = "ROI",
          na.color = "transparent",
          alpha.regions = 1,
          legend = F
          
  ) +
  mapview(baisins,
          col.regions = rainbow(100),
          layer.name = "Drainage Basins",
          na.color = "transparent",
          alpha.regions = 0.4
          
  ) +

  mapview(streams, 
          zcol = "Stream Size",
          col.region = rev(c("#89CFF0", "#0096FF", "#00008a")),
          color = rev(c("#89CFF0", "#0096FF", "#00008a")),
          layer.name = "Streams",
          alpha.regions = 1,
          lwd = 1.5,
          legend = T) ; hydrology_map

# Export all maps to html ----

#Export to web map
mapshot(
  final_map,
  url = here("web_map_html/aoi_web_map.html"),
  selfcontained=F
  )

mapshot(
  forest_age_map,
  url = here("web_map_html/forest_age_map.html"),
  selfcontained=F
)

mapshot(
  chm_map,
  url = here("web_map_html/chm_map.html"),
  selfcontained=F
)

mapshot(
  hydrology_map,
  url = here("web_map_html/hydrology_map.html"),
  selfcontained=F
)




#Export layers to boundaries folder
gdrive_boundaries_dir <- "G:/.shortcut-targets-by-id/12umh7NSzh5vEeAKF78qodLZci2pbIp4h/Cable Bay/Data/Boundaries"

st_write(roi, file.path(gdrive_boundaries_dir, "roi.gpkg"), append = F)
st_write(parcels, file.path(gdrive_boundaries_dir, "target_parcels.gpkg"), append = F)
st_write(trail_buffer, file.path(gdrive_boundaries_dir, "cb_trail_buffer_100m.gpkg"), append = F)
st_write(other_trails, file.path(gdrive_boundaries_dir, "other_trails.gpkg"), append = F)
 
join_point_park %>%
  select(name, geometry) %>%
  st_write(file.path(gdrive_boundaries_dir, "joan_point_park.gpkg"), append = F)

#Produce some histograms

hist(age_ras, 
     main = "Forest Age Histogram", 
     xlab = "Age (years)", 
     ylab = "Frequency")

hist(chm_continuous, 
     main = "Canopy Height Histogram", 
     xlab = "Height (m)", 
     ylab = "Frequency",
     breaks=25
     )



