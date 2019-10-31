# Code to get tree canopy at the zip code level using NLCD Tree Canopy Cover raster (2016 release). 

# You'll need to have already downloaded that raster as well as a zipcode shapefile. Tree Canopy can be found at mrlc.gov ("NLCD 2016 USFS Tree Canopy Cover (CONUS)"). Zip code shapefile (technically zip code tabulation areas) can be found at https://www.census.gov/cgi-bin/geo/shapefiles/index.php by selecting ZIP Code Tabulation Areas as the layer type.
# You will also need the "zipsbystate.csv" file (this page)
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

#Raster of data
treecanopy <- raster(treecanopypath)
treecanopy

#Getting zips for the lower 48
zipsbystate <- read.csv("zipsbystate.csv", colClasses = c(STATEFIPS="factor",STATE="factor",zipcode="factor",zip5="factor"))
zipsbystate <- zipsbystate %>% filter(zip5 != "0", zip5 != "99999") %>% unique()
lower48zips <- zipsbystate %>% filter(STATE != "AK", STATE != "HI") %>% arrange(zip5)

#Here is where you should read in your list of zips that you are interested in. need them to be characters to keep the leading zeros. This is an example list
SubZips <- as.character(c(10023,11787,15701,15906,19087,19382,22801,28025,30087,30307))
#sorting is important as the landcover will be done in numerical order
SubZips <- sort(SubZips)

#Reading in the shapefile of ZIP codes in the US
zips <- 'D:/New_Greenspace/ZIPCodeShapeFiles2017/'
setwd(zips)
zips.shp <- readOGR(".","cb_2017_us_zcta510_500k")
zips.shp <- spTransform(zips.shp,treecanopy@crs)
zips.shp <- zips.shp[order(zips.shp$ZCTA5CE10),]
#just taking lower 48 as that's what we have tree canopy for
zips.lower48.shp <- zips.shp[which(zips.shp$ZCTA5CE10 %in% lower48zips$zip5),]
#just getting the sub zips that we have tree canopy for
subzipsInLand <- SubZips[which(SubZips %in% lower48zips$zip5)]
#just getting the trees that we have subjects living in.
zips.land.sub <- zips.lower48.shp[which(zips.lower48.shp$ZCTA5CE10 %in% subzipsInLand),]

#data storage setup
n_zips <- length(zips.land.sub$ZCTA5CE10)
d <- data.frame(ZIPCODE = subzipsInLand, TreeCanopy = as.vector(as.integer(rep("NA",n_zips))))

for (j in 1:n_zips){
  print(paste("PROCESSING ZIP CODE",j,"of",n_zips,":",as.character(zips.land.sub$ZCTA5CE10[j])))
  #selecting zipcode
  this_zip <- as.character(zips.land.sub$ZCTA5CE10[j])
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
  #plotting. can be commented out to save time
  print("plotting...")
  plot(treecanopy.zip.r.mask)
  # saving mean of tree canopy values in the raster masked to match the zipcode
  d[j,2] <- cellStats(treecanopy.zip.r.mask, mean)
}

write.csv(d, "ZIPCODE_TreeCanopy.csv")
