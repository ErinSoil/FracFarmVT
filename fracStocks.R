#Calcuating stocks for Frac study

#setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin

setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT")
getwd()  # Check if the directory changed successfully


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
library(ggpubr)
library(cowplot)
library(gridExtra)


# Replace 'Type.x' column values with 'Pasture' where 'Field_Code' is 'D7'
data$Type.x[data$Field_Code == "D7"] <- "Pasture"

# Assuming 'data' is your dataframe
subset_Type <- data$Type[data$Type.x == "Veg"]

# Print the subset of Field_Code values where Type.x is "Field Crop"
print(subset_Type)

hist(data$BD30)


#calculate carbon and nitrogen stocks in kg C (or N) per m2
fracStock <-data %>%
  mutate(cStock=(total_c/100*BD30*(30)*100^2)/1000,
         nStock=(total_n/100*BD30*(30)*100^2)/1000)

#calculate carbon and nitrogen stocks in kg C (or N) per m2 #same thing!!!
fracStock2 <-data %>%
  mutate(cStock=((total_c/100)*BD30*(30)*10),
         nStock=((total_n/100)*BD30*(30)*10))

view(fracStock)


summary_overall <- fracStock %>%
  summarise(
    mean_cStock = mean(cStock, na.rm = TRUE),
    sd_cStock = sd(cStock, na.rm = TRUE),
    min_cStock = min(cStock, na.rm = TRUE),
    max_cStock = max(cStock, na.rm = TRUE),
    n = sum(!is.na(cStock))
  )
print(summary_overall)

summary_by_type <- fracStock %>%
  group_by(Type.x) %>%
  summarise(
    mean_cStock = mean(cStock, na.rm = TRUE),
    sd_cStock = sd(cStock, na.rm = TRUE),
    min_cStock = min(cStock, na.rm = TRUE),
    max_cStock = max(cStock, na.rm = TRUE),
    n = sum(!is.na(cStock))
  )
print(summary_by_type)


fracStock <- fracStock %>%
  mutate(AP = ifelse(Type.x %in% c("Hay", "Pasture"), "P", "A"))

####by field type
ggplot(fracStock, aes(x = Type.x, y = cStock, fill = Type.x)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "cStock Distribution by Field Type",
    x = "Field Type",
    y = "cStock"
  ) +
  theme(legend.position = "none")  # if you want to remove legend since it's repetitive
################

# Fit the linear model
model <- lm(cStock ~ overall.score, data = fracStock)

# Get the summary statistics
summary(model)

#own_theme below sets ggplot parameters for how plots should look. 
own_theme <- theme_bw(base_size = 11) +
  theme(rect = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(12,5.5,5.5,5.5), "points"))

####this is the latest 4.28.2025## Fig7v

Fig7<-ggplot(fracStock, aes(x = overall.score, y = cStock, color = AP)) +
  geom_point(alpha = 0.7) +  # points for each field
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # regression line
 own_theme +
  # theme(
  #   panel.grid.major = element_line(color = "gray80"),  # show major gridlines
  #   panel.grid.minor = element_line(color = "gray90"),  # show minor gridlines
  #   axis.line = element_line(color = "black", linewidth = 0.5),  # solid axis lines
  #   axis.ticks = element_line(color = "black")
  # ) +
  # theme_minimal() +  # Minimal theme for aesthetics
  theme(#axis.title.x = element_text(size = 12),  # Increase x-axis label size
    #axis.title.y = element_text(size = 12),  # Increase y-axis label size
    #axis.line = element_line(color = "black"),  # Add axis lines
    legend.title = element_blank(),  # Remove legend title
    legend.position = c(.2,.85)) +  # Position legend on the right
 # scale_color_manual(values = c("Annual" = "#cd853f", "Perennial" = "darkgreen"))  # Custom colors for annual and perennial
  labs(
    title = " ",
    x = "Soil Health Index",
    y = expression("Carbon stock (kg C " * m^{-2} * ") to 30cm"),
    color = "Crop Type"
  ) +
  scale_color_manual(
    values = c("A" = "#cd853f", "P" = "darkgreen"),
    labels = c("A" = "Annual", "P" = "Perennial")
  )
Fig7


ggsave("FinalFigures/Fig7.jpeg", plot = Fig7, width = 4, height = 3, dpi = 600)


# --- Step 1: Clean and set up fracStock ---
# Create a new variable that combines Tillage_1to4 = 2, 3, and 4 into a single group
fracStock<- fracStock %>%
  mutate(Tillage_Grouped = ifelse(Tillage_1to4 == 1, "1", "2, 3, 4"))

# Ensure the new variable is a factor
fracStock$Tillage_Grouped <- as.factor(fracStock$Tillage_Grouped)
# RECREATE the AP column based on Type.x
fracStock_clean <- fracStock %>%
  mutate(AP = case_when(
    Type.x %in% c("Hay", "Pasture") ~ "Perennial",
    Type.x %in% c("Veg", "Corn", "Field Crop") ~ "Annual",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Tillage_Grouped), !is.na(AP))  # Remove NAs in Tillage_Grouped and AP

# Make factors
fracStock_clean <- fracStock_clean %>%
  mutate(
    Tillage_Grouped = factor(Tillage_Grouped, levels = c(1, 2, 3, 4),
                             labels = c("No-Till", "Till", "Till", "Till")),
    AP = factor(AP, levels = c("Perennial", "Annual"))
  )

# --- Step 2: Set color schemes ---

fill_colors_Till <- c("No-Till" = "pink", "Till" = "darkgrey")
fill_colors_AP <- c("Perennial" = "pink", "Annual" = "darkgrey")

# --- Step 3: Set y-axis limits for cStock ---

y_limits <- range(fracStock_clean$cStock, na.rm = TRUE)

# --- Step 4: Create the plots ---

# Plot 1: Tillage_Grouped
AgTill <- ggplot(fracStock_clean, aes(x = Tillage_Grouped, y = cStock, fill = Tillage_Grouped)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter(color = "black", width = 0.2, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  scale_fill_manual(values = fill_colors_Till) +
  scale_x_discrete(labels = c("No-Till", "Till")) +
  labs(x = "", y = expression("Carbon Stock to 30 cm (kg C " * m^{-2} * ")"))+ # Update Y-axis label to match variable
  guides(fill = "none") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16, color = "black")
  ) +
  ylim(y_limits)

# Plot 2: AP (Perennial vs Annual)
plot_AgAP <- ggplot(fracStock_clean, aes(x = AP, y = cStock, fill = AP)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter(color = "black", width = 0.2, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  scale_fill_manual(values = fill_colors_AP) +
  scale_x_discrete(labels = c("Perennial", "Annual")) +
  labs(x = "", y = NULL, fill = NULL) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16, color = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ylim(y_limits)

# --- Step 5: Arrange the two plots ---

SF_fracStock <- grid.arrange(
  AgTill, plot_AgAP,
  ncol = 2,
  widths = c(1, 1),
  heights = c(1),
  top = ""  # Optional title
)
# Overall n for cStock
sum(!is.na(fracStock$cStock))

SF-fracStock

View(fracStock)
# Save the plot if you want
ggsave("SF_fracStock.jpeg", plot = SF_fracStock, width = 10, height = 6, dpi = 300)

# Run a one-way ANOVA
anova_result <- aov(cStock ~ Type.x, data = fracStock)

anova_result2<- aov(cStock ~ AP, data = fracStock)
anova_result3<- aov(cStock ~ Tillage_Grouped, data = fracStock)

anova_result4<- aov(soil_texture_clay ~ AP, data = fracStock)
anova_result5<- aov(soil_texture_clay ~ Tillage_Grouped, data = fracStock)
# See the summary of the ANOVA
summary(anova_result2)
summary(anova_result3)
summary(anova_result4)
summary(anova_result5)

TukeyHSD(anova_result)

library(dplyr)

# Calculate mean and standard error by Type.x
cStock_summary <- fracStock %>%
  group_by(Type.x) %>%
  summarise(
    mean_cStock = mean(cStock, na.rm = TRUE),
    se_cStock = sd(cStock, na.rm = TRUE) / sqrt(sum(!is.na(cStock))),
    n = sum(!is.na(cStock))
  )

# View the summary
cStock_summary

