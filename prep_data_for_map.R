setwd('C:/Users/Sandy/Documents/Maps/nycrent')

#Load required packages
library(sp)
library(spdep)
library(rgdal)
library(foreign)
library(maptools)

#Clear list space
rm(list=ls())

###############################################################################################################
#Step 1: Parse the pdf files of the building locations. To read in the files, first save the pdfs as plain text.
###############################################################################################################

#Define parse function
parse <- function(file){
  mbldgs <- readLines(file)
  mdata <- data.frame(Block=character(),
                      Lot=character(),
                      Zip=character(),
                      stringsAsFactors=FALSE)
  
  for(i in 1:length(mbldgs)){
    str <- unlist(strsplit(mbldgs[i], " "))
    len <- length(str)
    if(len==1){
      mdata[i,1] <- ""
      mdata[i,2] <- ""
      mdata[i,3] <- ""
    }
    else if(len >1){
      block <- str[len-1]
      lot <- str[len]
      zip <- str[1]
      mdata[i,1] <- block
      mdata[i,2] <- lot
      mdata[i,3] <- zip
    }
  }
  
  mdata$Block <- as.integer(mdata$Block)
  mdata$Lot <- as.integer(mdata$Lot)
  mdata$Zip <- as.integer(mdata$Zip)
  mdata <-na.omit(mdata)
  
  return(mdata)
}

#Run parse function on all borough txt files
mhtn <- parse('rent_controlled_buildings/2012ManhattanBldgs.txt')
bklyn <- parse('rent_controlled_buildings/2012BrooklynBldgs.txt')
queens <- parse('rent_controlled_buildings/2012QueensBldgs.txt')
staten <- parse('rent_controlled_buildings/2012StatenIslBldgs.txt')
bronx <- parse('rent_controlled_buildings/2012BronxBldgs.txt')

######################################################################################################
#2.Collapse building files into block units and zip units to merge to the shapefiles by block and zip.
######################################################################################################

#Define collapse function to get summarized count of buildings per block
collapse <- function(var, boro){
  table <- as.data.frame(table(var))
  table$BORO <- boro
  names <- c('BLOCK', 'COUNT', 'BORO')
  names(table) <- names
  return(table)
}

#Run collapse function on all five boroughs for block
m <- collapse(mhtn$Block, 1)
br <- collapse(bronx$Block, 2)
bk <- collapse(bklyn$Block, 3)
q <- collapse(queens$Block, 4)
s <- collapse(staten$Block, 5)
#Create combined block file for all five boroughs
all <- rbind(m,br,bk,q,s)

#Run collapse function on all five boroughs for zip code
m <- collapse(mhtn$Zip, 1)
br <- collapse(bronx$Zip, 2)
bk <- collapse(bklyn$Zip, 3)
q <- collapse(queens$Zip, 4)
s <- collapse(staten$Zip, 5)
#Create combined zip file for all five boroughs
allzip <- rbind(m,br,bk,q,s)

#########################################################################################################
#3. Read in census data and clean up for merge to buildings data and shapefiles.
#########################################################################################################

#Read in census data and merge to building data by zipcode
census <- read.csv('ACS_11_5YR_B25071/ACS_11_5YR_B25071.csv')
census_small <- census[c(2,4:5)]
census_small <- merge(census_small, allzip, by.x="GEO.id2", by.y="BLOCK", all.x=TRUE)

#Convert numeric fields to numeric and blanks to NAs
for(i in 1:ncol(census_small)){
  census_small[,i] <- as.numeric(as.character(census_small[,i]))  
}

#Read in census shapefile by zipcode and merge with census data
crs <- CRS("+proj=longlat")
shp <- readShapeSpatial("tl_2010_36_zcta510/tl_2010_36_zcta510.shp", proj4string=crs)
m <- merge(shp, census_small, by.x="ZCTA5CE10", by.y="GEO.id2")

#Read in lookup table of NYC neighborhood zip codes (source: http://www.nyc.gov/html/doh/downloads/pdf/data/appb.pdf)
nyczip <- read.csv('zipcodes_nyc_boroughs.csv', header=FALSE)
nyczip <- na.omit(nyczip)

#Merge to census shapefile and only keep the zip codes in NYC boroughs
nyc <- merge(m, nyczip, by.x="ZCTA5CE10", by.y="V1", all.x=FALSE)

#Convert to KML file for upload to Google Fusion Table (see http://www.nceas.ucsb.edu/scicomp/usecases/shapeFileToKML for more help)
GpProjString <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
nycGP <- nyc
proj4string(nycGP) <- GpProjString
nycGP <- spTransform(nycGP,GpProjString)
writeOGR(nycGP,".","nycGP",driver="ESRI Shapefile")  
nycGP <- readOGR("nycGP.shp","nycGP")
writeOGR(nycGP, "nycGP.kml", "nycGP", driver="KML")

##############################################################################################
#4.Read in NYC tax block shapefile and merge to buldings data by block
###############################################################################################

#Read in NYC tax block shapefile - this is in lambert conformal conic projection, see metadata for details
crs <- CRS("+proj=lcc +lat_0=40.166667+lon_0=-74 +lat_1=40.666667 +lat_2=41.033333 +datum=NAD83")
block <- readShapeSpatial("Digital_Tax_Map_shapefile_12-12/DTM_1212_Tax_Block_Polygon.shp", proj4string=crs)
#Merge building file by tax block to shapefile
rent <- merge(block, all, by=c("BORO", "BLOCK"), all.x=FALSE)
#Write out shapefile to change the projection in QGIS and write to kml file
#QGIS is an open source GIS program: http://www.qgis.org/en/site/)
writeSpatialShape(rent, "rent")
