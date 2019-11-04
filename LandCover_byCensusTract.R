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

#Read in the land cover raster (change to your file location) and land cover types/ Must have previously downloaded from mrlc.gov
land11 <- 'D:/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img'
coverraster.r <- raster(land11)
header <- as.vector(c("0","11","12","21","22","23","24","31","41","42","43","52","71","81","82","90","95"))
types <- as.vector(c("water","snowice","devopen","devlow","devmed","devhigh","barren","deciduous","evergreen","mixedforest","shrub","grasslands","haypasture","cultivated","woodywetlands","herbwetlands"))

#Directory of state census tract files
states <- list.dirs('D:/New_Greenspace/CensusTractShapeFiles2010/')

#Census tracts you want to process. At a minimum, needs columns for State FIPS code and GEOID of census tracts. Could be edited to process all census tracts.
SubTracts <- read.csv("censustracttest.csv")
SubTracts <- SubTracts %>% arrange(statefp10)

dat <- data.frame()


for (i in 2:length(states)){
  current.state <- states[i]
  s <- as.integer(substr(current.state,nchar(current.state)-9,nchar(current.state)-8))
  if (s %in% SubTracts$statefp10){
    setwd(current.state)
    print(paste("BEGINNING PROCESSING - STATE:",i))
    current.state.shp <- readOGR(".")
    current.state.shp <- spTransform(current.state.shp,coverraster.r@crs)
    current.state.shp <- current.state.shp[order(current.state.shp$COUNTYFP10,current.state.shp$TRACTCE10),]
    current.state.subs.shp <- current.state.shp[which(current.state.shp$GEOID10 %in% SubTracts$geoid10),]
    n_ct <- length(current.state.subs.shp$TRACTCE10)
    vectorOfTables.perc <- vector(mode = 'list', length = n_ct)
    d <- data.frame(ctname=numeric(length(header)))
    for (j in 1:n_ct){
      print(paste("PROCESSING ZIP CODE",j,"of",n_ct,":",as.character(current.state.subs.shp$GEOID10[j])))
      #selecting census tract
      this_ct <- as.character(current.state.subs.shp$GEOID10[j])
      this.ct.shp <- current.state.subs.shp[as.character(current.state.subs.shp$GEOID10) %in% this_ct,]
      # crop
      print("cropping...")
      coverraster.r.crop <- crop(coverraster.r, extent(this.ct.shp))
      #transforming to sf object in order to use fasterize package
      this.ct.sf <- st_as_sf(this.ct.shp)
      # rasterize, using fasterize
      print("rasterizing...")
      coverraster.r.ct.r <- fasterize::fasterize(this.ct.sf, coverraster.r.crop)
      # mask
      print("masking...")
      coverraster.r.ct.r.mask <- mask(coverraster.r.crop, coverraster.r.ct.r)
      #plotting. can be commented out to save time
      print("plotting...")
      plot(coverraster.r.ct.r.mask)
      # Begin process of tabulating percentage land cover
      print("creating cover tables...")
      raster.mat <- as.matrix(coverraster.r.ct.r.mask)  # First convert masked raster data to matrix format
      # table tabulates all data within matrix data, in pixels
      vectorOfTables.perc[[j]] <- table(raster.mat) 
    }
    #Transforming from list into dataframe; making a row per zipcode and labeling the columns as land cover types 
    for(k in 1:length(vectorOfTables.perc)){
      a <- data.frame(vectorOfTables.perc[k])
      b <- a[match(header,a$raster.mat),]
      c <- b[2]
      d <- cbind(d,c)
      }
    d[is.na(d)] <- 0
    d <- d[-1,]
    colnames(d)[2:dim(d)[2]] <- current.state.subs.shp$GEOID10
    d.df <- as.data.frame(t(as.matrix(d[,-1])))
    colnames(d.df)[1:length(names(d.df))] <- types

    #Going from pixels to percentages
    d.df1 <- d.df %>% mutate(totalpix = water+snowice+devhigh+devmed+devlow+devopen+barren+ deciduous+evergreen+mixedforest+shrub+grasslands+haypasture+cultivated+woodywetlands+ herbwetlands)
rownames(d.df1) <- current.state.subs.shp$GEOID10
    for (s in 1:16){
      d.df1[17+s] <- NA
      colnames(d.df1)[17+s] <- paste0("Perc",colnames(d.df1[s]))
      d.df1[17+s] <- (d.df1[s])/d.df1$totalpix
    }

    #Just saving percentages
    d.df2 <- d.df1[-c(1:17)]
    # saving to persist past this loop
    dat <- rbind(dat, d.df2)
  }
  else next
}

 
dat <- cbind(as.numeric(rownames(dat)), data.frame(dat, row.names = NULL))
dat <- dat %>% rename(Tract = `as.numeric(rownames(dat))`)

#writing the csv
filename <- "D:/New_Greenspace/CensusTractLandCover_test.csv"
write.csv(dat,filename,row.names = T)

