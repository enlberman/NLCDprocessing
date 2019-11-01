# Code to get tree canopy at the census tract level using NLCD Tree Canopy Cover raster (2016 release). 

# You'll need to have already downloaded that raster as well as a census tract shapefiles. Tree Canopy can be found at mrlc.gov ("NLCD 2016 USFS Tree Canopy Cover (CONUS)"). Census tract shapefiles can be found at https://www.census.gov/cgi-bin/geo/shapefiles/index.php by selecting Census Tracts as the layer type. Each state will have it's own shapefile in a different folder when unzipped

# KES, 31OCT2019

setwd("D:/New_Greenspace")

#Needed libraries
library(sf)
library(rgdal)
library(raster)
library(fasterize)
library(sp)
library(mosaic)
library(dplyr)

#Path to img file of tree canopy data
treecanopypath <- "D:/New_Greenspace/NLCD_2016_TreeCanopy_raster/NLCD_2016_Tree_Canopy_L48_20190831.img"
#Directory of state census tract files
states <- list.dirs('D:/New_Greenspace/CensusTractShapeFiles2010/')

#Census tracts you want to process. At a minimum, needs columns for State FIPS code and GEOID of census tracts. Could be edited to process all census tracts.
SubTracts <- read.csv("censustracttest.csv")
SubTracts <- SubTracts %>% arrange(statefp10)
#data storage
d <- data.frame(Tract = SubTracts$geoid10, TreeCanopy = as.vector(as.integer(rep("NA",nrow(SubTracts)))))

#Raster of data
treecanopy <- raster(treecanopypath)
treecanopy

for (i in 2:length(states)){
  current.state <- states[i]
  s <- as.integer(substr(current.state,nchar(current.state)-9,nchar(current.state)-8))
  if (s %in% SubTracts$statefp10){
    setwd(current.state)
    print(paste("BEGINNING PROCESSING - STATE:",i))
    current.state.shp <- readOGR(".")
    current.state.shp <- spTransform(current.state.shp,treecanopy@crs)
    current.state.shp <- current.state.shp[order(current.state.shp$COUNTYFP10,current.state.shp$TRACTCE10),]
    current.state.subs.shp <- current.state.shp[which(current.state.shp$GEOID10 %in% SubTracts$geoid10),]
    n_ct <- length(current.state.subs.shp$TRACTCE10)
    for (j in 1:n_ct){
      print(paste("PROCESSING ZIP CODE",j,"of",n_ct,":",as.character(current.state.subs.shp$GEOID10[j])))
      #selecting census tract
      this_ct <- as.character(current.state.subs.shp$GEOID10[j])
      this.ct.shp <- current.state.subs.shp[as.character(current.state.subs.shp$GEOID10) %in% this_ct,]
      # crop
      print("cropping...")
      treecanopy.crop <- crop(treecanopy, extent(this.ct.shp))
      #transforming to sf object in order to use fasterize package
      this.ct.sf <- st_as_sf(this.ct.shp)
      # rasterize, using fasterize
      print("rasterizing...")
      treecanopy.ct.r <- fasterize::fasterize(this.ct.sf, treecanopy.crop)
      # mask
      print("masking...")
      treecanopy.ct.r.mask <- mask(treecanopy.crop, treecanopy.ct.r)
      #plotting. can be commented out to save time
      print("plotting...")
      plot(treecanopy.ct.r.mask)
      # saving mean of tree canopy values in the raster masked to match the census tract
      d[which(d$Tract == as.numeric(this_ct)),2] <- cellStats(treecanopy.ct.r.mask, mean)
      }
  }
  else next
}



setwd("D:/New_Greenspace")
write.csv(d, "CensusTract_TreeCanopy.csv")
