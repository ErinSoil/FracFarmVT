#Analyzing data and building linear models with mgPOM as response variable

#setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin

##call in the analytical data
data <- read.csv("data_pH.csv")

View(data)
#load your libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(emmeans)
library(nlme)
library(ggeffects)
library (gmodels)
library(reshape2)
library(car) # for Levene's Test
library(ggpubr) # for easy plotting
library(multcomp) # for Tukey's HSD test
library(cowplot)
library(gridExtra)
library(grid)   # needed for textGrob and gpar

##call in the analytical data
data <- read.csv("data_pH.csv")
View(data)

hist(data$ph)

data <- data %>%
  mutate(AP = case_when(
    Type.x %in% c("Hay", "Pasture") ~ "perennial",
    Type.x %in% c("Veg", "Corn", "Field Crop") ~ "annual",
    TRUE ~ NA_character_  # Set to NA if none of the conditions match
  ))

# Replace 'Type.x' column values with 'Pasture' where 'Field_Code' is 'D7'
data$Type.x[data$Field_Code == "D7"] <- "Pasture"


# Step 2: Define color palette
crop_colors <- c("perennial" = "darkgreen", "annual" = "#cd853f")  # Update if necessary

# Step 3: Create a helper function for plotting
create_regression_plot <- function(data, xvar, yvar, ylabel) {
  ggplot(data, aes(x = !!sym(xvar), y = !!sym(yvar), color = AP)) +
    geom_point(alpha = 0.5) +
    stat_smooth(method = "lm", se = TRUE, color = "black", fill = "gray", alpha = 0.2) +
    labs(x = "Soil Health Index",
         y = ylabel) +
    scale_color_manual(values = crop_colors, name = NULL) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 12),
      axis.line = element_line(color = "black", size = 0.5)  # <<---- Add solid axis lines
    )
}

# Step 4: Create the three plots
POC_health <- create_regression_plot(data_clean, "overall.score", "mgCpergSoilP", expression("mg POC g"^-1~"soil"))
MAOC_health <- create_regression_plot(data_clean, "overall.score", "mgCpergSoilM", expression("mg MAOC g"^-1~"soil"))
PropMAOC_health <- create_regression_plot(data_clean, "overall.score", "propM", expression("Proportion of Carbon as MAOC")) +
  ylim(0.3, 0.9)
# Step 5: Format plots (remove individual legends and axis for top two)
format_no_legend_no_xaxis <- function(plot) {
  plot +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

POC_health_no_legend <- format_no_legend_no_xaxis(POC_health)
MAOC_health_no_legend <- format_no_legend_no_xaxis(MAOC_health)
# Bottom plot: keep x-axis labels
PropMAOC_health_no_legend <- PropMAOC_health + 
  theme(
    legend.position = "none",
    axis.title.x = element_blank()
  )

# Step 6: Arrange the three plots vertically
Figure_7dhealth <- plot_grid(
  POC_health_no_legend,
  MAOC_health_no_legend,
  PropMAOC_health_no_legend,
  ncol = 1, nrow = 3,
  align = 'v',
  labels = c("a", "b", "c"),
  label_x = 0.05,
  label_y = 1.08
)

# Step 7: Extract shared legend
shared_legend <- get_legend(
  POC_health +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(size = 10))
)

# Step 8: Stack legend + plots
Figure_7dhealth_with_legend <- plot_grid(
  shared_legend,
  Figure_7dhealth,
  ncol = 1,
  rel_heights = c(0.08, 1)
)

# Step 9: Add shared x-axis label
final_plot <- plot_grid(
  Figure_7dhealth_with_legend,
  grid::textGrob("Soil Health Index", gp = gpar(fontsize = 14)),
  ncol = 1,
  rel_heights = c(1, 0.05)
)


# Display
print(final_plot)



# Step 10: Save the final figure
ggsave("Figure4_healthv.jpeg", plot = final_plot, width = 15, height = 22, units = "cm", dpi = 600)




