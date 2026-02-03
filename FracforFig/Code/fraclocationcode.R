#exploring location data of fractionation study
#Supplement Figure3abc Lines 329-387

#setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin


# Install and load required libraries
#install.packages("ggplot2")
#install.packages("sf")
#install.packages("maps")
#install.packages("mapdata")
#install.packages("tigris")
library(tigris)
#load your libraries
library(sf)
library(maps)
library(mapdata)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggpubr)
library(dplyr)
##call in the analytical data
Loc <- read.csv(file="Location.csv", header=TRUE, sep=",")
data <- read.csv(file="older data/fracData2.csv", header=TRUE, sep=",")
View(data)

Loc <- Loc %>%
  left_join(data %>% select(Field_Code, Type.x), by = "Field_Code")

Loc <- Loc %>%
  left_join(data %>% dplyr::select(Field_Code, Type.x), by = "Field_Code")

View(Loc)
# Get the map data for Vermont
vermont_map <- map_data("state")
vermont_map <- subset(vermont_map, region == "vermont")

vermont_counties <- map_data("county") %>%
  subset(region == "vermont")

# Get counties for Vermont as an sf object
vt_counties_sf <- counties(state = "VT", cb = TRUE, class = "sf")
 ######################Prepping Figure 1 test############## 

   # Plot the map and data points Figure 1
  ggplot() +
        geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_polygon(data = vermont_counties, aes(x = long, y = lat, group = group), 
                   fill = NA, color = "grey30", size = 0.5) +    # County outlines
      coord_fixed(1.3) +
       geom_point(data = Loc, aes(x = lon, y = lat), color = "red", size = 3) +
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "Latitude and Longitude Points on the Map of Vermont",
         x = "Longitude",
         y = "Latitude") +
    theme_minimal()
  
###########clean the data

Loc <- Loc %>%
  filter(!is.na(Type.x), !is.na(lat), !is.na(lon)) %>%    # Step 1: Remove NAs
  mutate(Type.x = recode(as.character(Type.x),            # Step 2: Recode after making sure it's character
                         "Field crops" = "Wheat",
                         "Veg" = "Vegetable")) %>%
  mutate(Type.x = factor(Type.x))                         # Step 3: Force back to factor


Loc <- Loc %>%
  filter(!is.na(Type.x), !is.na(lat), !is.na(lon)) %>%
  mutate(
    Type.x = case_when(
      Type.x == "Field crops" ~ "Wheat",
      Type.x == "Veg" ~ "Vegetable",
      TRUE ~ as.character(Type.x)  # keep other values unchanged
    ),
    Type.x = factor(Type.x)
  )



 
  ###########FINAL FINAL FINAL Figure 1###############################
  
  # Define a custom muted color palette with more visible options
  custom_colors <- c("#8DA0CB", "#FC8D62", "#66C2A5", "#E78AC3", "#E5C494", "#A6D740")  # Replaced yellow with a soft tan
  

  #Figure 1 Final optionb
  # Now plot  #################4.28.2025
  Figure1Final <- ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_polygon(data = vermont_counties, aes(x = long, y = lat, group = group), 
                 fill = NA, color = "grey30", size = 0.5) +
    geom_point(data = Loc, aes(x = lon, y = lat, color = Type.x), size = 3, alpha = 0.5) +
    coord_fixed(1.3) +
    labs(title = "",
         x = "Longitude",
         y = "Latitude",
         color = "Crop Type") +  # This controls legend title
    scale_color_brewer(palette = "Dark2", name = "Crop Type") +  # <-- DO NOT manually set labels!
    theme_minimal() +
    theme(
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
  
  Figure1Final
  
  # Save the plot using ggsave
  ggsave("FinalFigures/Figure1Finalv2.jpeg", plot = Figure1Final, width = 10, height = 8, dpi = 600)
  ggsave("FinalFigures/Figure1Finalv3.pdf", plot = Figure1Final, width = 10, height = 8, dpi = 600)
  ggsave("FinalFigures/Figure1Finalv3.png", plot = Figure1Final, width = 10, height = 8, dpi = 600)
  ggsave("FinalFigures/Figure1Finalv3.eps", device = cairo_ps, plot = Figure1Final, width = 10, height = 8, dpi = 600)
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
  
 #Figure1
   ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_polygon(data = vermont_counties, aes(x = long, y = lat, group = group), 
                 fill = NA, color = "grey30", size = 0.5) +    # County outlines
    geom_point(data = merged_data_clean, aes(x = lon, y = lat, color = Type.x.x), size = 3, alpha = 0.5) +  # Mapping Type.x to color
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(title = "Sample locations in Vermont",
         x = "Longitude",
         y = "Latitude",
         color = "Crop Type") +  # Legend title
    scale_color_brewer(palette = "Dark2", name = "Crop Type", labels = levels(merged_data_clean$Type.x.x)) +  # Use Dark2 for better visibility
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
  view(data)
  
  
  
####################################################################
#Supplement Figure1abc. SF3. Supplement Figure 1 FINAL final
  #for POM supplement Figure 3 a POC

  summary(data$mgCpergSoilP)
  
  vermont_counties <- map_data("county") %>%
    filter(region == "vermont") %>%
    arrange
  
  plot_a <- ggplot() +
       geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_polygon(data = vermont_counties, aes(x = long, y = lat, group = group), 
                 fill = NA, color = "grey40", linewidth = 0.5) +  # County lines
   geom_point(data = data, aes(x = Longitude, y = Latitude, color = mgCpergSoilP), size = 3, alpha = .7) +
    scale_color_gradientn(colors = c("blue", "green", "yellow", "red"),  # Adjust colors as needed
                         # breaks = c(4, 6, 8, 12),
                          #labels = c("Low", "Moderate", "High", "Very High"),
                          limits = c(2, 32)) +  # Adjust limits based on your data
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(
         x = "Longitude",
         y = "Latitude",
         color = "") +  # Add color legend title
    theme_minimal()
 plot_a 
   #supplement Figure 3 b mg MAOC
  summary(data$mgCpergSoilM)
  plot_b <-ggplot() +
    geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_polygon(data = vermont_counties, aes(x = long, y = lat, group = group), 
                 fill = NA, color = "grey40", linewidth = 0.5) +  # County lines
    geom_point(data = data, aes(x = Longitude, y = Latitude, color = mgCpergSoilM), size = 3, alpha= .7) +
    scale_color_gradientn(colors = c("blue", "green", "yellow", "red"),  # Adjust colors as needed
                          #breaks = c(11, 15, 21, 25),
                          #labels = c("Low", "Moderate", "High", "Very High"),
                          limits = c(3, 48)) +  # Adjust limits based on your data
    coord_fixed(1.3) +  # Fix aspect ratio
    labs(
         x = "Longitude",
         y = "Latitude",
         color = "") +  # Add color legend title
    theme_minimal()
  plot_b
  #Proportion SFc
  #logitpropMAOM supplement Figure 3 c Prop MAOC
  # Add logitpropM to merged_data
data <- data %>%
    mutate(logitpropM = log(propM / (1 - propM)))
  # plot_c <- ggplot() +
  #   geom_polygon(data = vermont_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  #   geom_polygon(data = vermont_counties, aes(x = long, y = lat, group = group), 
  #                fill = NA, color = "grey40", linewidth = 0.5) +  # County lines
  #   geom_point(data = data, aes(x = Longitude, y = Latitude, color = logitpropM), size = 3, alpha = .7) +
  #   scale_color_gradientn(colors = c("blue", "green", "yellow", "red"),  # Adjust colors as needed
  #                        # breaks = c(.4, .8, 1.1, 2),
  #                         #labels = c("Low", "Moderate", "High", "Very High"),
  #                         limits = c(-1, 2)) +  # Adjust limits based on your data
  #   coord_fixed(1.3) +  # Fix aspect ratio
  #      labs(
  #     x = "Longitude",
  #     y = "Latitude",
  #     color = "") +  # Add color legend title
  #   theme_minimal()
  # plot_c
  # Arrange plots side by side with labels "a", "b", and "c", ensuring equal sizing
  SuppFig1 <-ggarrange(plot_a, plot_b, 
            labels = c("a", "b"),        # Label each plot
            ncol = 2, nrow = 1,               # Arrange in a 1-row, 3-column layout
            align = "hv",                     # Align horizontally and vertically
            common.legend = TRUE,             # Use a common legend if appropriate
            legend = "right",                 # Position legend on the right side
            label.x = 0.1,                   # Center labels horizontally
            label.y = .99999                    # Adjust vertical position closer to plots (try values < 1.0)
  )

  SuppFig1
  ggsave("FinalFigures/SuppFig1abc_submit.jpeg",plot = SuppFig1, width = 10, height = 6, dpi=600)
  
  
  
  
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
  
