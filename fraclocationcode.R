#exploring location data of fractionation study

#setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin

#load your libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(openmeteo)

##call in the analytical data
Loc <- read.csv(file="Location.csv", header=TRUE, sep=",")

weather_history(
  c(44.98630, -73.30327), 
  "2000-01-01", "2022-01-01",
  daily= c("precipitation_sum","precipitation_hours"),
  response_units= list(precipitation_unit = "inch")
)
  apply(array, margin, ...)
  
# work with Nclimgrid data  
library(raster)
  
r <- brick("nclimgrid_tavg.nc")
  
xy <- SpatialPoints(cbind(-81.8125, 24.5625))
  
  result <- extract(r, xy, sp=T)
  
  
  
  result$X2023.07.01   
  
 






