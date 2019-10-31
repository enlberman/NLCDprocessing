#Getting land cover by ZIP code, all in one shapefile folder. Need the Zip code shapefile, the land cover file, the zip code by state csv and a list of zip codes you want to process, entered as characters.
#Zip code shapefile (technically zip code tabulation areas) can be found at https://www.census.gov/cgi-bin/geo/shapefiles/index.php by selecting ZIP Code Tabulation Areas as the layer type.
#Land cover can be found at mrlc.gov ("NLCD 2016 Land Cover (CONUS)")
#KES, 31OCT2019

#Load these packages, and properly set your working directory
setwd("D:/New_Greenspace/")
library(rgdal)
library(raster)
library(mosaic)
library(dplyr)
library(fasterize)
library(sf)

#Read in the land cover raster (change to your file location) and land cover types/ Must have previously downloaded from mrlc.gov
land11 <- 'D:/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img'
coverraster.r <- raster(land11)
header <- as.vector(c("0","11","12","21","22","23","24","31","41","42","43","52","71","81","82","90","95"))
types <- as.vector(c("water","snowice","devopen","devlow","devmed","devhigh","barren","deciduous","evergreen","mixedforest","shrub","grasslands","haypasture","cultivated","woodywetlands","herbwetlands"))

#Getting zips for the lower 48
zipsbystate <- read.csv("zipsbystate.csv", colClasses = c(STATEFIPS="factor",STATE="factor",zipcode="factor",zip5="factor"))
zipsbystate <- zipsbystate %>% filter(zip5 != "0", zip5 != "99999") %>% unique()
lower48zips <- zipsbystate %>% filter(STATE != "AK", STATE != "HI") %>% arrange(zip5)

#Here is where you should read in your list of zips that you are interested in. need them to be characters to keep the leading zeros
SubZips <- as.character(c(10023,11787,15701,15906,19087,19382,22801,28025,30087,30307))
#sorting is important as the landcover will be done in numerical order
SubZips <- sort(SubZips)

#Reading in the shapefile of ZIP codes in the US
zips <- 'D:/New_Greenspace/ZIPCodeShapeFiles2017/'
setwd(zips)
zips.shp <- readOGR(".","cb_2017_us_zcta510_500k")
zips.shp <- spTransform(zips.shp,coverraster.r@crs)
zips.shp <- zips.shp[order(zips.shp$ZCTA5CE10),]
#just taking lower 48 as that's what we have land cover for
zips.lower48.shp <- zips.shp[which(zips.shp$ZCTA5CE10 %in% lower48zips$zip5),]
#just getting the sub zips that we have land cover for
subzipsInLand <- SubZips[which(SubZips %in% lower48zips$zip5)]
#just getting the land that we have subjects living in.
zips.land.sub <- zips.lower48.shp[which(zips.lower48.shp$ZCTA5CE10 %in% subzipsInLand),]

#data storage setup
n_zips <- length(zips.land.sub$ZCTA5CE10)
d <- data.frame(zipname=numeric(length(header)))
vectorOfTables.perc <- vector(mode = 'list', length = n_zips)

#loop over zips
for (j in 1:n_zips){
  # pick jth zip code
  this_zip <- as.character(zips.land.sub$ZCTA5CE10[j])
  # print status of loop
  print(paste("PROCESSING ZIP CODE",j,"of",n_zips,":",as.character(zips.lower48.shp$ZCTA5CE10[j])))
  # get shape of jth census tract (this can be plotted with plot(this.zip.shp))
  this.zip.shp <- zips.lower48.shp[as.character(zips.lower48.shp$ZCTA5CE10) %in% this_zip,]
  #transforming to sf object in order to use fasterize package
  this.zip.sf <- st_as_sf(this.zip.shp)
  # crop
  print("cropping...")
  coverraster.crop <- crop(coverraster.r, extent(this.zip.shp))
  # rasterize, using fasterize
  print("rasterizing...")
  coverraster.zip.r <- fasterize::fasterize(this.zip.sf, coverraster.crop)
  # mask
  print("masking...")
  coverraster.zip.r.mask <- mask(coverraster.crop, coverraster.zip.r)
  # Legends get changed during processing, and so we change them to match the raster map
  coverraster.zip.r.mask@legend <- coverraster.r@legend
  coverraster.zip.r.mask[is.na(coverraster.zip.r.mask),] <- 0 
   # Plot the cropped, rasterized, and masked county map. Not necessary if want more speed
  plot(coverraster.zip.r.mask)
  # Begin process of tabulating percentage land cover
  print("creating cover tables...")
  raster.mat <- as.matrix(coverraster.zip.r.mask)  # First convert masked raster data to matrix format
  # table tabulates all data within matrix data, in pixels
  vectorOfTables.perc[[j]] <- table(raster.mat)     
} #END OF LOOP OVER ZIP CODES

#Transforming from list into dataframe; making a row per zipcode and labeling the columns as land cover types  
for(k in 1:length(subzipsInLand)){
  a <- data.frame(vectorOfTables.perc[k])
  b <- a[match(header,a$raster.mat),]
  c <- b[2]
  d <- cbind(d,c)
}
d[is.na(d)] <- 0
d <- d[-1,]
colnames(d)[2:dim(d)[2]] <- subzipsInLand
d.df <- as.data.frame(t(as.matrix(d[,-1])))
colnames(d.df)[1:length(names(d.df))] <- types

#Going from pixels to percentages
d.df1 <- d.df %>% mutate(totalpix = water+snowice+devhigh+devmed+devlow+devopen+barren+ deciduous+evergreen+mixedforest+shrub+grasslands+haypasture+cultivated+woodywetlands+ herbwetlands)
rownames(d.df1) <- subzipsInLand
for (s in 1:16){
  d.df1[17+s] <- NA
  colnames(d.df1)[17+s] <- paste0("Perc",colnames(d.df1[s]))
  d.df1[17+s] <- (d.df1[s])/d.df1$totalpix
}

#Just saving percentages
d.df2 <- d.df1[-c(1:17)]

#writing the csv
filename <- "D:/New_Greenspace/ZIPCodeGreenSpace_test.csv"
write.csv(d.df2,filename,row.names = T)
