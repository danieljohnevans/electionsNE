library(ggmap)
library(dplyr)
library(tidyr)
library(rgdal)
library(ggplot2)
library(USAboundaries)
library(stringr)

## Import and combine data sheets

wd <- setwd("your/path/here")
load_data <- function(path) { 
  files <- list.files( path = wd)
  tables <- lapply(files, read.csv, sep = "\t")
  do.call(rbind, tables)
}

mapdata <- load_data(wd)

towns <- str_c(mapdata$Town, mapdata$State, sep= ", ")
allT <- (str_trim(unique(sort(towns), stringsAsFactors=FALSE)))
##remove first 9 towns bc they are false positives
allTowns <- allT[9:2372]

##bring all towns into a data frame and geocode. Note: due to limits of Google's geocode API this must
## must be limited to 2500 searches per day

firstTowns <- data.frame (towns = c (allTowns),stringsAsFactors = FALSE)
geocodedTowns <- geocode(firstTowns$towns)
firstTowns <- cbind(firstTowns, geocodedTowns)
allLocations <- firstTowns

##write.csv(firstTowns, file= "neAllLocations.csv")

## Import allLocations results from a previous save. This step is extraneous if geocoded.

allLocations <- read.csv ("your/csv/here.csv",
                          stringsAsFactors = FALSE)

##merge allLocations to mapdata

str(mapdata)
mapdata <- mutate(mapdata, 
                  location = str_c(mapdata$Town, mapdata$City))
mapdata <-  mutate(mapdata, 
                   location = str_c(mapdata$location, mapdata$State, sep= ", "))

str(allLocations)
names(allLocations) <- c("x", "location", "lon", "lat")
str(allLocations)

mapdata_merged <- left_join(mapdata, allLocations, by = c("location" = "location"))
str(mapdata_merged)

## Writes master file:
##write.csv(mapdata_merged, file= "neMapdataMerged.csv")

## Map Code
USA <- c("Connecticut","Maine", "Massachusetts", "New Hampshire", 
         "New York", "Rhode Island", "Vermont")  
map <- us_boundaries(as.Date("1825-03-15"), type = "county", state = USA)
usMap <- ggplot() +  geom_polygon(data=map, aes(x=long, y=lat, group=group))
usMap +
  ggtitle("County Boundaries on March 15, 1825") +
  geom_text(data = allLocations, aes(x = lon, y = lat, label = location), 
            color="gray",
            vjust = -1,
            size = 4) +
  geom_point(data = allLocations, aes(x = lon, y = lat), color= "red") +
  theme(legend.position = "bottom" ) +
  theme_minimal()
