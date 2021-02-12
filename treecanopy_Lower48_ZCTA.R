# Code to get tree canopy at the zip code level using NLCD Tree Canopy Cover raster (2016 release). 

# You'll need to have already downloaded that raster as well as a zipcode shapefile. Tree Canopy can be found at mrlc.gov ("NLCD 2016 USFS Tree Canopy Cover (CONUS)"). Zip code shapefile (technically zip code tabulation areas) can be found at https://www.census.gov/cgi-bin/geo/shapefiles/index.php by selecting ZIP Code Tabulation Areas as the layer type.
# You will also need the "zipsbystate.csv" file (this page)
# KES, 09FEB2021

setwd("D:/Box Sync/Documents/MyOngoing/New_Greenspace")

#Needed libraries
library(tidyverse)
library(sf)
library(rgdal)
library(raster)
library(fasterize)
library(sp)
library(mosaic)
library(data.table)

#Path to img file of tree canopy data
treecanopypath <- "D:/Backed Up Files from C Drive/NLCD_2016_TreeCanopy_raster/NLCD_2016_Tree_Canopy_L48_20190831.img"

#Raster of data
treecanopy <- raster(treecanopypath)
treecanopy

#Getting zips for the lower 48
zipsbystate <- read.csv("Generated_CSVs/zipsbystate.csv", colClasses = c(STATEFIPS="factor",STATE="factor",zipcode="factor",zip5="factor"))
zipsbystate <- zipsbystate %>% filter(zip5 != "0", zip5 != "99999") %>% unique()
lower48zips <- zipsbystate %>% filter(STATE != "AK", STATE != "HI") %>% arrange(zip5)

#Reading in the shapefile of ZIP codes in the US
zips <- 'D:/Box Sync/Documents/MyOngoing/New_Greenspace/ZIPCodeShapeFiles2017/'
setwd(zips)
zips.shp <- readOGR(".","cb_2017_us_zcta510_500k")
#Matching CRS between canopy and ZCTA layers
zips.shp <- spTransform(zips.shp,treecanopy@crs)
zips.shp <- zips.shp[order(zips.shp$ZCTA5CE10),]
#just taking lower 48 as that's what we have tree canopy for
zips.lower48.shp <- zips.shp[which(zips.shp$ZCTA5CE10 %in% lower48zips$zip5),]
#Going back to main directory
setwd("D:/Box Sync/Documents/MyOngoing/New_Greenspace")
#data storage setup
n_zips <- length(zips.lower48.shp$ZCTA5CE10)
d <- data.frame(ZIPCODE = zips.lower48.shp$ZCTA5CE10, TreeCanopy = as.vector(as.integer(rep("NA",n_zips))))

for (j in 1:n_zips){
  print(paste("PROCESSING ZIP CODE",j,"of",n_zips,":",as.character(zips.lower48.shp$ZCTA5CE10[j]), "Start Time:", Sys.time()))
  #selecting zipcode
  this_zip <- as.character(zips.lower48.shp$ZCTA5CE10[j])
  this.zip.shp <- zips.lower48.shp[as.character(zips.lower48.shp$ZCTA5CE10) %in% this_zip,]
  # crop
  print("cropping...")
  treecanopy.crop <- crop(treecanopy, extent(this.zip.shp))
  #transforming to sf object in order to use fasterize package
  this.zip.sf <- st_as_sf(this.zip.shp)
  # rasterize, using fasterize
  print("rasterizing...")
  treecanopy.zip.r <- fasterize::fasterize(this.zip.sf, treecanopy.crop)
  # mask
  print("masking...")
  treecanopy.zip.r.mask <- mask(treecanopy.crop, treecanopy.zip.r)
  #plotting. commented out to save time
  #print("plotting...")
  #plot(treecanopy.zip.r.mask)
  # saving mean of tree canopy values in the raster masked to match the zipcode
  d[j,2] <- cellStats(treecanopy.zip.r.mask, mean)
}

write_csv(d, "Lower49_ZIPCODE_TreeCanopy.csv")
