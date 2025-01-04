#exploring location data of fractionation study
#Supplement Figure3abc Lines 329-387

#setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin


# Install and load required libraries
#install.packages("ggplot2")
#install.packages("sf")
#install.packages("maps")
#install.packages("mapdata")

#load your libraries
library(sf)
library(maps)
library(mapdata)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggpubr)

##call in the analytical data
Loc <- read.csv(file="Location.csv", header=TRUE, sep=",")
data <- read.csv(file="older data/fracData2.csv", header=TRUE, sep=",")
data <- read.csv("data.csv")

view(Loc)

# Get the map data for Vermont
vermont_map <- map_data("state")
vermont_map <- subset(vermont_map, region == "vermont")

#############Code to check data #######################
# Find Field_Code with NA in location
na_field_codes <- data %>%
  filter(is.na("lat")) %>%
  group_by(Field_Code) %>%
  summarise(count_na = n(), .groups = 'drop')
# Find rows where either lon or lat is NA
na_field_codes <- Loc %>%
  filter(is.na(lon) | is.na(lat))

# Print result
print(na_rows)
# Print result
print(na_field_codes)

# Find Field_Code where either lon or lat is NA
na_field_codes <- Loc[is.na(Loc$lon) | is.na(Loc$lat), "Field_Code"]

# Print result
print(na_field_codes)

# Check if there are any NA values in Type.x
any_na <- Loc %>%
  summarise(any_na = any(is.na(Type.x))) %>%
  pull(any_na)

# Print result
if (any_na) {
  print("There are NA values in Type.x")
} else {
  print("There are no NA values in Type.x")
}
View(Loc)

# Check if there are any NA values in Lon
any_na <- Loc %>%
  summarise(any_na = any(is.na(lon))) %>%
  pull(any_na)

if (any_na) {
  print("There are NA values in Long")
} else {
  print("There are no NA values in Long")
}

#####################End data Check##################


############### Not clear if any of this is needed. maybe trash######
weather_history(
  c(44.98630, -73.30327), 
  "2000-01-01", "2022-01-01",
  daily= c("precipitation_sum","precipitation_hours"),
  response_units= list(precipitation_unit = "inch")
)
  apply(array, margin, ...)
  
  m1 = gls(mgCpergSoilM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
             aggregate_stability * soil_texture_clay + 
             active_carbon + 
             + + ph * soil_texture_clay, 
           data = data, 
           na.action = na.exclude, 
           method = "ML")
  summary(m1)
  
  # work with Nclimgrid data  
library(raster)
  
r <- brick("nclimgrid_tavg.nc")
  
xy <- SpatialPoints(cbind(-81.8125, 24.5625))
  
  result <- extract(r, xy, sp=T)
    result$X2023.07.01   
  

 ######################Prepping Figure 1 map Not sure what is needed############## 

   # Plot the map and data points Figure 1
  ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = Loc, aes(x = lon, y = lat), color = "red", size = 3) +
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "Latitude and Longitude Points on the Map of Vermont",
         x = "Longitude",
         y = "Latitude") +
    theme_minimal()
  
  str(data) 
  data <- data %>%
    mutate(Type.x = as.character(Type.x)) %>%
    mutate(Type.x = gsub("Field crops", "Wheat", Type.x)) %>%
    mutate(Type.x = factor(Type.x))  # Convert back to factor if needed
  
  # Assuming 'Field_Code' is the shared column between vermont_map and data
  data$Type.x <- as.character(data$Type.x)  # Convert factor to character
  data$Type.x[data$Type.x == "Field Crops"] <- "Wheat"
  data$Type.x <- factor(data$Type.x)  # Convert back to factor
  data$Type.x <- droplevels(data$Type.x)
  
  
  merged_data <- merge(Loc, data, by = "Field_Code", all.x = TRUE)
  
view(merged_data)
  # Replace "Field Crops" with "Wheat", convert to factor, and drop unused levels
  data <- data %>%
    mutate(Type.x = as.character(Type.x)) %>%       # Convert factor to character
    mutate(Type.x = recode(Type.x, "Field crops" = "Wheat")) %>%  # Replace "Field Crops" with "Wheat"
    mutate(Type.x = factor(Type.x)) %>%              # Convert back to factor
    mutate(Type.x = droplevels(Type.x))              # Drop unused levels
  
  # Merge data with Loc by "Field_Code"
  merged_data <- merge(Loc, data, by = "Field_Code", all.x = TRUE)
  
  # Print the merged data
  print(merged_data)
  
  filtered_data <- merged_data %>%
    filter(Type.x != "Wheat", "Corn", "Hay", "Pasture", "Veg")
  
  
  # Convert Type.x to a factor for correct color mapping
  merged_data$Type.x <- factor(merged_data$Type.x)
  
  merged_data <- merged_data %>%
    filter(!is.na(lon) & !is.na(lat))
  
  levels(merged_data$Type.x)
  
  #make a map by field type
  ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = merged_data, aes(x = lon, y = lat, color = Type.x), size = 3, alpha=.5) +  # Mapping Type.x to color
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "",
         x = "Longitude",
         y = "Latitude",
         color = "Crop Type") +  # Legend title
    scale_color_discrete(name = "Crop Type", labels = levels(merged_data$Type.x)) +  # Custom legend labels
    theme_minimal()
  

  #improve map

  ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = merged_data, aes(x = lon, y = lat, color = Type.x), size = 3, alpha=.5) +  # Mapping Type.x to color
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "Sample locations in Vermont",
         x = "Longitude",
         y = "Latitude",
         color = "Crop Type") +  # Legend title
    scale_color_brewer(palette = "Set3", name = "Crop Type", labels = levels(merged_data$Type.x)) +  # Apply BrBG palette
    theme_minimal()
  

  ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = merged_data, aes(x = lon, y = lat, color = Type.x), size = 3, alpha = 0.5) +  # Mapping Type.x to color
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "Sample locations in Vermont",
         x = "Longitude",
         y = "Latitude",
         color = "Crop Type") +  # Legend title
    scale_color_brewer(palette = "Dark2", name = "Crop Type", labels = levels(merged_data$Type.x)) +  # Use Dark2 for better visibility
    theme_minimal()
  

  


  ###########FINAL FINAL FINAL Figure 1###############################
  # Plotting. Figure 1. Updated code with improved color contrast
  # Remove rows with NA in 'Type.x'
  merged_data_clean <- merged_data %>%
    filter(!is.na(Type.x)) 
  view(merged_data_clean)
  # Replace "Field Crops" with "Wheat", convert to factor, and drop unused levels
  merged_data_clean <- merged_data_clean %>%
    mutate(Type.x = as.character(Type.x)) %>%       # Convert factor to character
    mutate(Type.x = recode(Type.x, "Field crops" = "Wheat")) %>%  # Replace "Field Crops" with "Wheat"
    mutate(Type.x = factor(Type.x)) %>%              # Convert back to factor
    mutate(Type.x = droplevels(Type.x))              # Drop unused levels
  
  unique(merged_data_clean$Type.x)
  merged_data_clean <- merged_data_clean %>%
    mutate(Type.x = str_replace(Type.x, fixed("Field crops", ignore_case = TRUE), "Wheat")) %>% # Handle case insensitivity
    mutate(Type.x = factor(Type.x)) %>%              # Convert back to factor
    mutate(Type.x = droplevels(Type.x))              # Drop unused levels
  
  # Define a custom muted color palette with more visible options
  custom_colors <- c("#8DA0CB", "#FC8D62", "#66C2A5", "#E78AC3", "#E5C494", "#A6D740")  # Replaced yellow with a soft tan
  
  view(merged_data_clean)
  
  #Figure 1 Final optionb
  ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = merged_data_clean, aes(x = lon, y = lat, color = Type.x), size = 3, alpha = 0.5) +  # Mapping Type.x to color
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "",
         x = "Longitude",
         y = "Latitude",
         color = "Crop Type") +  # Legend title
    scale_color_brewer(palette = "Dark2", name = "Crop Type", labels = levels(merged_data$Type.x)) +  # Use Dark2 for better visibility
    theme_minimal()
  
  # Create the plot and assign it to a variable
  Figure1Final <- ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = merged_data_clean, aes(x = lon, y = lat, color = Type.x), size = 3, alpha = 0.5) +  # Mapping Type.x to color
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "",
         x = "Longitude",
         y = "Latitude",
         color = "Crop Type") +  # Legend title
    scale_color_brewer(palette = "Dark2", name = "Crop Type", labels = levels(merged_data_clean$Type.x)) +  # Use Dark2 for better visibility
    theme_minimal()
  
  
  Figure1Final <- ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = merged_data_clean, aes(x = lon, y = lat, color = Type.x), size = 3, alpha = 0.5) +  # Mapping Type.x to color
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "",
         x = "Longitude",
         y = "Latitude",
         color = "Crop Type") +  # Legend title
    scale_color_brewer(palette = "Dark2", name = "Crop Type", labels = levels(merged_data_clean$Type.x)) +  # Use Dark2 for better visibility
    theme_minimal() +
    theme(
      legend.title = element_text(size = 12),  # Increase legend title font size
      legend.text = element_text(size = 12),   # Increase legend items font size
      axis.title.x = element_text(size = 12),  # Increase x-axis title font size
      axis.title.y = element_text(size = 12),  # Increase y-axis title font size
      axis.text.x = element_text(size = 12),   # Increase x-axis label font size
      axis.text.y = element_text(size = 12)    # Increase y-axis label font size
    )
  Figure1Final
  # Save the plot using ggsave
  ggsave("Figure1Final.jpeg", plot = Figure1Final, width = 10, height = 8, dpi = 300)
   #######################################Figure 1 Final Complete#########
  
### If someone complains about the color palette above, try this one:
    # Define a custom muted color palette for better visibility and contrast
    custom_colors <- c("#8DA0CB", "#FC8D62", "#66C2A5", "#E78AC3", "#FFD92F", "#A6D854")
  
  ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = merged_data_clean, aes(x = lon, y = lat, color = Type.x), size = 3, alpha = 0.5) +  # Mapping Type.x to color
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "",
         x = "Longitude",
         y = "Latitude",
         color = "Crop Type") +  # Legend title
    scale_color_manual(values = custom_colors, name = "Crop Type", 
                       labels = levels(merged_data_clean$Type.x)) +  # Apply custom colors
    theme_minimal()
  
  #Or this one
    # Define a custom color palette for better visibility
  custom_colors <- c("blue", "red", "green", "purple", "orange", "brown")
  
  ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = merged_data_clean, aes(x = lon, y = lat, color = Type.x), size = 3, alpha = 0.5) +  # Mapping Type.x to color
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "",
         x = "Longitude",
         y = "Latitude",
         color = "Crop Type") +  # Legend title
    scale_color_manual(values = custom_colors, name = "Crop Type", 
                       labels = levels(merged_data_clean$Type.x)) +  # Apply custom colors
    theme_minimal()
  
  #for pastels, try this:
  ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = merged_data_clean, aes(x = lon, y = lat, color = Type.x), size = 3, alpha = 0.5) +  # Mapping Type.x to color
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "",
         x = "Longitude",
         y = "Latitude",
         color = "Crop Type") +  # Legend title
    scale_color_brewer(palette = "Set3", name = "Crop Type", 
                       labels = levels(merged_data_clean$Type.x)) +  # Apply Set3 palette
    theme_minimal()
  
  
  #########################################################################
 
  ####New Figures##################################
  
  
  
  
  
  
  
  
  
   ggsave("mgPOM_Figure4.jpeg", width = 10, height = 8)
  
  ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = merged_data, aes(x = lon, y = lat, color = Type.x), size = 3, alpha = 0.5) +  # Mapping Type.x to color
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "Sample locations in Vermont",
         x = "Longitude",
         y = "Latitude",
         color = "Crop Type") +  # Legend title
    scale_color_brewer(palette = "Dark2", name = "Crop Type", labels = levels(merged_data$Type.x)) +  # Use Dark2 for better visibility
    theme_minimal()
  
  
  #add soils data to map of plots colored by field type
  # Assuming 'soil_texture_class' is a column in the 'data' dataframe
  # Load soil data for Vermont (FIND THIS)
  soil_data <- read.csv("soil_data.csv")  # Replace with your actual data loading method
  
  # Assuming 'Field_Code' is the shared column between Loc and merged_data
  merged_data <- merge(Loc, data, by = "Field_Code", all.x = TRUE)
  
  # Convert Type.x to a factor for correct color mapping
  merged_data$Type.x <- factor(merged_data$Type.x)
  
  ggplot() +
    geom_polygon(data = Loc, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_polygon(data = soil_data, aes(x = long, y = lat, group = group, fill = soil_texture_class), color = "gray") +
    geom_point(data = merged_data, aes(x = lon, y = lat, color = Type.x), size = 3) +  # Mapping Type.x to color
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "Latitude and Longitude Points on the Map of Vermont",
         x = "Longitude",
         y = "Latitude",
         color = "Farm Type") +  # Legend title
    scale_color_discrete(name = "Farm Type", labels = levels(merged_data$Type.x)) +  # Custom legend labels
    scale_fill_manual(name = "Soil Type", values = c("brown", "yellow", "green", "blue")) +  # Custom soil legend colors
    theme_minimal()
  
  # join tables
  data <- data %>%
    left_join(Loc, by = "Field_Code")
  
   # fracData<-fracData %>%
  #     left_join(.,Master,by=c("Field_Code", "soil_texture_class")) 
  # fracData<-fracData %>%
  #   left_join(.,PRISM2annual,by="Field_Code")
  # names(fracData)
  view(merged_data)
  
  
  
####################################################################
#Supplement Figure1abc. SF3. Supplement Figure 1 FINAL final
  #for POM supplement Figure 3 a POC
  # Rename merged_data to data
  data <- merged_data
  summary(data$mgCpergSoilP)
  plot_a <- ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = data, aes(x = lon, y = lat, color = mgCpergSoilP), size = 3) +
    scale_color_gradientn(colors = c("blue", "green", "yellow", "red"),  # Adjust colors as needed
                          breaks = c(4, 6, 8, 12),
                          labels = c("Low", "Moderate", "High", "Very High"),
                          limits = c(2, 32)) +  # Adjust limits based on your data
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(
         x = "Longitude",
         y = "Latitude",
         color = "") +  # Add color legend title
    theme_minimal()
  
   #supplement Figure 3 b mg MAOC
  summary(data$mgCpergSoilM)
  plot_b <-ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = data, aes(x = lon, y = lat, color = mgCpergSoilM), size = 3) +
    scale_color_gradientn(colors = c("blue", "green", "yellow", "red"),  # Adjust colors as needed
                          breaks = c(11, 15, 21, 25),
                          labels = c("Low", "Moderate", "High", "Very High"),
                          limits = c(3, 48)) +  # Adjust limits based on your data
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(
         x = "Longitude",
         y = "Latitude",
         color = "") +  # Add color legend title
    theme_minimal()
  
  #Proportion SFc
  #logitpropMAOM supplement Figure 3 c Prop MAOC
  # Add logitpropM to merged_data
data <- data %>%
    mutate(logitpropM = log(propM / (1 - propM)))
  plot_c <- ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = data, aes(x = lon, y = lat, color = logitpropM), size = 3) +
    scale_color_gradientn(colors = c("blue", "green", "yellow", "red"),  # Adjust colors as needed
                          breaks = c(.4, .8, 1.1, 2),
                          labels = c("Low", "Moderate", "High", "Very High"),
                          limits = c(-1, 2)) +  # Adjust limits based on your data
    coord_fixed(1.3) +  # Fix aspect ratio
       labs(
      x = "Longitude",
      y = "Latitude",
      color = "") +  # Add color legend title
    theme_minimal()
  
  # Arrange plots side by side with labels "a", "b", and "c", ensuring equal sizing
  SuppFig1 <-ggarrange(plot_a, plot_b, plot_c, 
            labels = c("a", "b", "c"),        # Label each plot
            ncol = 3, nrow = 1,               # Arrange in a 1-row, 3-column layout
            align = "hv",                     # Align horizontally and vertically
            common.legend = TRUE,             # Use a common legend if appropriate
            legend = "right",                 # Position legend on the right side
            label.x = 0.3,                   # Center labels horizontally
            label.y = 0.9                    # Adjust vertical position closer to plots (try values < 1.0)
  )

  SuppFig1
  ggsave("SuppFig1abc.jpeg",plot = SuppFig1, width = 10, height = 6, dpi=300)
  
  
  
  
  library(gtools)
  data <- data %>%
    dplyr::mutate(logitpropM = logit(propM))
  
  summary(data$logitpropM)
  
  

  
  library(dplyr)
  
  # join tables
  data2 <- data %>%
    left_join(Loc, by = "Field_Code")
  
  view(data2)
  
  #correlation plots
  library(corrplot)
  cordata <- cor(data[,c("mgCpergSoilM","ph","ppt.cm","Latitude", "Longitude", "tmeanC","aggregate_stability","soil_texture_clay","active_carbon")], use="pairwise.complete.obs", method="pearson")
  corrplot(cordata)
  view(cordata)  

  
  #look at the unexplained varibility in the models to see if lat/long can explain
  library(nlme)
  m3 = gls(mgCpergSoilM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
             active_carbon +aggregate_stability, 
           data = data, 
           na.action = na.exclude, 
           method = "ML")
  
  
  F_Final <- fitted(m3)
  R_Final <- residuals(m3, type = "pearson", scaled = TRUE)
  N = !is.na(data$mgCpergSoilM)
  Rfull <- NA
  Rfull[N] <- R_Final
  op <- par(mfrow = c(2,2), mar = c(5,4,1,1))  
  plot(F_Final, R_Final)
  hist(Rfull)
  plot(Rfull ~ data2$aggregate_stability)
  plot(Rfull ~ data2$soil_texture_clay)
  plot(Rfull ~ data2$active_carbon)
  plot(Rfull ~ data2$tmeanC)
  plot(Rfull ~ data2$ppt.cm)
  plot(Rfull ~ data2$Latitude)
  plot(Rfull ~ data2$ph)
  plot(Rfull ~ data2$Longitude)
  par(op)
  
  
  m4M=gls(logitpropM~ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
            active_carbon, 
          data=data, na.action=na.exclude, method="ML")
  
  F_Final <- fitted(m4M)
  R_Final <- residuals(m4M, type = "pearson", scaled = TRUE)
  N = !is.na(data2$logitpropM)
  Rfull <- NA
  Rfull[N] <- R_Final
  op <- par(mfrow = c(2,2), mar = c(5,4,1,1))  
  plot(F_Final, R_Final)
  hist(Rfull)
  plot(Rfull ~ data2$aggregate_stability)
  plot(Rfull ~ data2$soil_texture_clay)
  plot(Rfull ~ data2$active_carbon)
  plot(Rfull ~ data2$tmeanC)
  plot(Rfull ~ data2$ppt.cm)
  plot(Rfull ~ data2$ph)
  plot(Rfull ~ data2$Latitude)
  plot(Rfull ~ data2$Longitude)
  par(op)
  
  m3P=gls(mgCpergSoilP~ppt.cm*tmeanC
          +aggregate_stability+active_carbon,
          data=data, na.action=na.exclude, method="REML") 
  
  
  F_Final <- fitted(m3P)
  R_Final <- residuals(m3P, type = "pearson", scaled = TRUE)
  N = !is.na(data2$mgCpergSoilP)
  Rfull <- NA
  Rfull[N] <- R_Final
  op <- par(mfrow = c(2,2), mar = c(5,4,1,1))  
  plot(F_Final, R_Final)
  hist(Rfull)

  plot(Rfull ~ data2$Latitude)
  plot(Rfull ~ data2$Longitude)
  par(op)
  
