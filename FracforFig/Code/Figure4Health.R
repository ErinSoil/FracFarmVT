#soil health regression; Figure 4 submission January 2026
setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin

#load your libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(nlme)
library(ggeffects)
library(emmeans)
library(ggpubr)
library(cowplot)

data <- read.csv("data_pH.csv")
#own_theme below sets ggplot parameters for how plots should look. 
own_theme <- theme_bw(base_size = 11) +
  theme(rect = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(12,5.5,5.5,5.5), "points"))
# Perform linear regression POC
regression_model_POC <- lm(mgCpergSoilP ~ overall.score, data = data)
summary(regression_model_POC)

# Replace 'Type.x' column values with 'Pasture' where 'Field_Code' is 'D7'
data$Type.x[data$Field_Code == "D7"] <- "Pasture"

data <- data %>%
  mutate(AP = case_when(
    Type.x %in% c("Hay", "Pasture") ~ "Perennial",
    Type.x %in% c("Veg", "Corn", "Field Crop") ~ "Annual",
    TRUE ~ NA_character_  # Set to NA if none of the conditions match
  ))

# Remove NA values from the AP column
data <- data %>%
  filter(!is.na(AP))

# Create a plot with the regression line
POC_health <- ggplot(data, aes(x = overall.score, y = mgCpergSoilP, color = AP)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE, color = "black", fill = "gray", alpha = 0.2) +  # Single black line for the overall relationship
  labs(x = "Soil Health Score",
       y = expression("mg POC g"^-1~"soil")) +
  scale_color_manual(values = c("#cd853f", "darkgreen"),  # Lighter brown and dark green
                     name = NULL) +  # Remove legend title
  own_theme +
  theme(legend.position = c(.2,.8))

# Display the plot
POC_health

# Create a plot with the regression line
MAOC_health <- ggplot(data, aes(x = overall.score, y = mgCpergSoilM, color = AP)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE, color = "black", fill = "gray", alpha = 0.2) +  # Single black line for the overall relationship
  labs(x = "Soil Health Score",
       y = expression("mg MAOC g"^-1~"soil")) +
  scale_color_manual(values = c("#cd853f", "darkgreen"),  # Lighter brown and dark green
                     name = NULL) +  # Remove legend title
own_theme +
  theme(legend.position = "none")
# Display the plot
MAOC_health

# Create a plot with the regression line for Proportion of Carbon as MAOC
PropMAOC_health <- ggplot(data, aes(x = overall.score, y = propM, color = AP)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE, color = "black", fill = "gray", alpha = 0.2) +  # Add shaded confidence interval
  labs(x = "Soil Health Score",
       y = expression("Prop C as MAOC")) +
  scale_color_manual(values = c("#cd853f", "darkgreen"),  # Lighter brown and dark green
                     name = NULL) +  # Remove legend title
  own_theme +
  theme(legend.position = "none")

# Display the plot
PropMAOC_health

# Create the plots without x axis labels and titles
POC_health_no_x <- POC_health + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

MAOC_health_no_x <- MAOC_health + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Arrange the plots in 1 column, 3 rows, with shared x-axis
Figure_7dhealth <- plot_grid(
  POC_health_no_x,  # No legend for this plot
  MAOC_health_no_x,  # No legend for this plot
  PropMAOC_health,  # No legend for this plot
  ncol = 1,  # 1 column of plots
  nrow = 3,  # 3 rows
  align = 'v',  # Align vertically
  labels = c("a", "b", "c"),  # Label the plots as "a", "b", and "c"
  label_x = 0.03,   # Adjust x position for labels
  label_y = 1.005    # Adjust y position for labels
)

Figure_7dhealth

ggsave("FinalFigures/Figure4v3.jpeg", plot = Figure_7dhealth, width = 4, height = 6, units = "in", dpi = 600)




