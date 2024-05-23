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
  
  
  # Install and load required libraries
  install.packages("ggplot2")
  install.packages("sf")
  install.packages("maps")
  install.packages("mapdata")
  
  library(ggplot2)
  library(sf)
  library(maps)
  library(mapdata)
  
  # Get the map data for Vermont
  vermont_map <- map_data("state")
  vermont_map <- subset(vermont_map, region == "vermont")
  
   # Plot the map and data points
  ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = Loc, aes(x = lon, y = lat), color = "red", size = 3) +
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "Latitude and Longitude Points on the Map of Vermont",
         x = "Longitude",
         y = "Latitude") +
    theme_minimal()
  
  data <- read.csv(file="fracData2.csv", header=TRUE, sep=",")
  
  # join tables
  data <- data %>%
    left_join(Loc, by = "Field_Code")
  
   # fracData<-fracData %>%
  #     left_join(.,Master,by=c("Field_Code", "soil_texture_class")) 
  # fracData<-fracData %>%
  #   left_join(.,PRISM2annual,by="Field_Code")
  # names(fracData)
  
 
  ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = data, aes(x = lon, y = lat, color = mgCpergSoilM), size = 3) +
    scale_color_gradient(low = "blue", high = "red") +  # Adjust color gradient as needed
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "Latitude and Longitude Points on the Map of Vermont",
         x = "Longitude",
         y = "Latitude",
         color = "MgMAOM") +  # Add color legend title
    theme_minimal()

#improve colors
  summary(data$mgCpergSoilM)
  ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = data, aes(x = lon, y = lat, color = mgCpergSoilM), size = 3) +
    scale_color_gradientn(colors = c("blue", "green", "yellow", "red"),  # Adjust colors as needed
                          breaks = c(11, 15, 21, 25),
                          labels = c("Low", "Moderate", "High", "Very High"),
                          limits = c(3, 48)) +  # Adjust limits based on your data
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "Latitude and Longitude Points on the Map of Vermont",
         x = "Longitude",
         y = "Latitude",
         color = "MgMAOM") +  # Add color legend title
    theme_minimal()

#for POM
  summary(data$mgCpergSoilP)
  ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = data, aes(x = lon, y = lat, color = mgCpergSoilP), size = 3) +
    scale_color_gradientn(colors = c("blue", "green", "yellow", "red"),  # Adjust colors as needed
                          breaks = c(4, 6, 8, 12),
                          labels = c("Low", "Moderate", "High", "Very High"),
                          limits = c(2, 32)) +  # Adjust limits based on your data
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "Latitude and Longitude Points on the Map of Vermont",
         x = "Longitude",
         y = "Latitude",
         color = "MgPOM") +  # Add color legend title
    theme_minimal()

  #logitpropMAOM
  library(gtools)
  data <- data %>%
    dplyr::mutate(logitpropM = logit(propM))
  
  summary(data$logitpropM)
  ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = data, aes(x = lon, y = lat, color = logitpropM), size = 3) +
    scale_color_gradientn(colors = c("blue", "green", "yellow", "red"),  # Adjust colors as needed
                          breaks = c(.4, .8, 1.1, 2),
                          labels = c("Low", "Moderate", "High", "Very High"),
                          limits = c(-1, 2)) +  # Adjust limits based on your data
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "Latitude and Longitude Points on the Map of Vermont",
         x = "Longitude",
         y = "Latitude",
         color = "logit Proportion MAOM") +  # Add color legend title
    theme_minimal()
