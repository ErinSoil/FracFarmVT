#soil health regression
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
    Type.x %in% c("Hay", "Pasture") ~ "perennial",
    Type.x %in% c("Veg", "Corn", "Field Crop") ~ "annual",
    TRUE ~ NA_character_  # Set to NA if none of the conditions match
  ))

# Remove NA values from the AP column
data <- data %>%
  filter(!is.na(AP))


# Create a plot with the regression line
POC_health <- ggplot(data_clean, aes(x = overall.score, y = mgCpergSoilP, color = AP)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = TRUE, color = "black", fill = "gray", alpha = 0.2) +  # Single black line for the overall relationship
  labs(x = "Soil Health Index",
       y = expression("mg POC g"^-1~"soil")) +
  scale_color_manual(values = c("#cd853f", "darkgreen"),  # Lighter brown and dark green
                     name = NULL) +  # Remove legend title
  own_theme +
  theme(legend.position = c(.2,.8))

# Display the plot
POC_health

# Create a plot with the regression line
MAOC_health <- ggplot(data_clean, aes(x = overall.score, y = mgCpergSoilM, color = AP)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = TRUE, color = "black", fill = "gray", alpha = 0.2) +  # Single black line for the overall relationship
  labs(x = "Soil Health Index",
       y = expression("mg MAOC g"^-1~"soil")) +
  scale_color_manual(values = c("#cd853f", "darkgreen"),  # Lighter brown and dark green
                     name = NULL) +  # Remove legend title
own_theme +
  theme(legend.position = "none")
# Display the plot
MAOC_health

#View(data_clean)


# Create a plot with the regression line for Proportion of Carbon as MAOC
PropMAOC_health <- ggplot(data, aes(x = overall.score, y = propM, color = AP)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = TRUE, color = "black", fill = "gray", alpha = 0.2) +  # Add shaded confidence interval
  labs(x = "Soil Health Index",
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

# PropMAOC_health_no_x <- MAOC_health + 
#   theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

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

# # Add a common x-axis label at the bottom (for all plots)
# Figure_7dhealth_final <- Figure_7dhealth +
#   # theme(axis.title.x = element_text(size = 14),
#   #       axis.text.x = element_text(size = 12),
#   #       axis.ticks.x = element_line(color = "black")) +
#   annotation_custom(grob = grid::textGrob("Soil Health Index", gp = grid::gpar(fontsize = 11)),
#                     ymin = -Inf, ymax = -Inf, xmin = -Inf, xmax = Inf)
# 
# # Display the final figure
# Figure_7dhealth_final

ggsave("FinalFigures/Figure4.jpeg", plot = Figure_7dhealth, width = 4, height = 6, units = "in", dpi = 600)
#ggsave("FinalFigures/Figure4.pdf", plot = Figure_7dhealth_final, width = 20, height = 13, units = "cm", dpi = 600)





