#Analyzing data and building linear models with mgPOM as response variable

#setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin

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

##call in the analytical data
data <- read.csv("data.csv")
View(data)

summary(data$OM30)
summary(data$soil_texture_clay)
summary(data$tmeanC)
summary(data$ppt.cm)
summary(data)
view(data)
summary(data$Owned)
summary(data$Acres)
summary(data$ph)
hist(data$ph)
sum(data$ph > 7)
view(data)

# Replace 'Type.x' column values with 'Pasture' where 'Field_Code' is 'D7'
data$Type.x[data$Field_Code == "D7"] <- "Pasture"

# Assuming 'data' is your dataframe
subset_Type <- data$Type[data$Type.x == "Veg"]

# Print the subset of Field_Code values where Type.x is "Field Crop"
print(subset_Type)

# Count the number of fields with pH greater than 7
data$ph <- as.numeric(data$ph)

count <- sum(data$ph > 7.00, na.rm = TRUE)

# Print the result
print(count)

anova_result2 <- aov(ph ~ Type.x, data=data)

# test for differences in OM for field types

# Perform ANOVA
anova_result <- aov(OM30 ~ Type.x, data = data)

# Summary of ANOVA
summary(anova_result2)

# Conduct post-hoc tests using Tukey's HSD test for pairwise comparisons
posthoc <- emmeans(anova_result2, ~ Type.x)

# Print pairwise comparisons
print(posthoc, type = "compact")

# Use table() function to count occurrences of each field type
type_counts <- table(data$Type.x)

# Print the counts
print(type_counts)

# Use table() function to count occurrences of each field type
type_counts <- table(data$soil_texture_class)

# Print the counts
print(type_counts)

# Example ANOVA
anova_model <- aov(soil_texture_clay ~ Type.x, data = data)

# Summary of ANOVA
summary(anova_model)

# Example linear regression
lm_model <- lm(soil_texture_clay ~ Type.x, data = data)

# Summary of linear regression
summary(lm_model)

# Example Tukey's HSD test for post-hoc analysis
tukey_test <- TukeyHSD(anova_model)

# View Tukey's HSD test results
print(tukey_test)

# test for differences in soil texture class for field types
#all CIs overlap indicatin no sigficant difference
# Perform ANOVA
anova_result <- aov(soil_texture_clay ~ Type.x, data = data)
# Summary of ANOVA
summary(anova_result)
# Conduct post-hoc tests using Tukey's HSD test for pairwise comparisons
posthoc <- emmeans(anova_result, ~ Type.x)
# Print pairwise comparisons
print(posthoc, type = "compact")


# Example scatter plot with jitter
ggplot(data, aes(x = Type.x, y = soil_texture_clay)) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.7) +
  labs(x = "Field Type", y = "Soil Texture Clay") +
  ggtitle("Relationship between Soil Texture Clay and Field Type") +
  theme_minimal()

# Example contingency table
cont_table <- table(data$Type.x, data$soil_texture_class)

# View the contingency table
cont_table
# Load necessary library
library(vcd)  # For mosaic plot

# Example mosaic plot
mosaicplot(cont_table, main = "Mosaic Plot of Field Type vs. Soil Texture Class")

# Summarize the data by soil_texture_class and Type.x
summary_data <- data %>%
  group_by(soil_texture_class, Type.x) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)  # Calculate percentage within each soil_texture_class

# Plot stacked bar plot
library(ggplot2)
ggplot(summary_data, aes(x = soil_texture_class, y = percent, fill = Type.x)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Soil Texture Class", y = "Percentage", fill = "Field Type") +
  ggtitle("Distribution of Field Type across Soil Texture Classes") +
  theme_minimal()

# Plot grouped bar plot
ggplot(summary_data, aes(x = soil_texture_class, y = percent, fill = Type.x)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Soil Texture Class", y = "Percentage", fill = "Field Type") +
  ggtitle("Distribution of Field Type across Soil Texture Classes") +
  theme_minimal()

# Example of performing chi-square test
cont_table <- table(data$soil_texture_class, data$Type.x)
chi_square_test <- chisq.test(cont_table)

# View the contingency table
print(cont_table)

# View the chi-square test results
print(chi_square_test)

# Perform chi-square test
cont_table <- table(data$soil_texture_class, data$Type.x)
chi_square_test <- chisq.test(cont_table)

# Residual analysis
residuals <- residuals(chi_square_test)

# View residuals
print(residuals)

# Compute adjusted residuals
adjusted_res <- chisq.test(cont_table)$residuals

# View adjusted residuals
print(adjusted_res)

# Example of mosaic plot
library(vcd)
mosaicplot(cont_table, main = "Mosaic Plot of Field Type vs. Soil Texture Class")

# Use table() function to count occurrences of each field type
type_counts <- table(data$Type.x)

# Print the counts
print(type_counts)


# Create a contingency table
contingency_table <- table(data$Type.x, data$soil_texture_class)

# Print the contingency table
print(contingency_table)

# Perform chi-square test of independence
chi2_test <- chisq.test(contingency_table)

# Print the chi-square test results
print(chi2_test)

# chi test shows that this is a significant association between field typee and soil texture class

# Create a contingency table
contingency_table <- table(data$Type.x, data$soil_texture_class)

# Convert contingency table to data frame for plotting
contingency_df <- as.data.frame.matrix(contingency_table)

# Reshape data for plotting (optional, depending on how you want to visualize)

contingency_melted <- melt(contingency_df)

# Plot clustered bar plot
ggplot(contingency_melted, aes(x = soil_texture_class, y = value, fill = Type.x)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Soil Texture Class", y = "Count") +
  ggtitle("Distribution of Field Types by Soil Texture") +
  theme_minimal()

library(gplots)
# Plot heatmap of contingency table
heatmap.2(as.matrix(contingency_table),
          trace = "none",
          col = heat.colors(length(unique(data$Type.x))),
          dendrogram = "row",
          main = "Association between Field Type and Soil Texture",
          xlab = "Soil Texture Class",
          ylab = "Field Type")

  # Load ggplot2 package if not already loaded
  library(ggplot2)


#soil health regression
# Perform linear regression POC
regression_model_POC <- lm(mgCpergSoilP ~ overall.score, data = data)
summary(regression_model_POC)

  # Create a plot with the regression line
POC_health <- ggplot(data, aes(x = overall.score, y = mgCpergSoilP, color=AP)) +
    geom_point(alpha = 0.5) +
    stat_smooth(method = "lm", se = FALSE, color= "AP") +
    labs(x = "Soil Health Index",
         y = expression("mg POC g"^-1~"soil")) +
    theme_minimal()
POC_health


# Remove rows with NA values in the relevant columns
data_clean <- na.omit(data[, c("overall.score", "mgCpergSoilP", "AP")])

POC_health <- ggplot(data_clean, aes(x = overall.score, y = mgCpergSoilP, color = AP)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE) +  # 'color' aesthetic in ggplot will be used
  labs(
    x = "Soil Health Index",
    y = expression("mg POC " * g^{-1} * " soil")
  ) +
  theme_minimal()

POC_health
# Remove rows with NA values in the relevant columns
data_clean <- na.omit(data[, c("overall.score", "mgCpergSoilM", "AP")])

MAOC_health <- ggplot(data_clean, aes(x = overall.score, y = mgCpergSoilM, color = AP)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE) +  # 'color' aesthetic in ggplot will be used
  labs(
    x = "Soil Health Index",
    y = expression("mg MAOC " * g^{-1} * " soil")
  ) +
  theme_minimal()

MAOC_health

ggsave("POC_health.jpeg", width = 15, height = 8)

   # Perform linear regression MAOC
  regression_model_MAOC <- lm(mgCpergSoilM ~ overall.score, data = data)
  summary
  

  # Create a plot with the regression line
  MAOC_health <- ggplot(data, aes(x = overall.score, y = mgCpergSoilM, color=AP)) +
    geom_point(alpha = 0.5) +
    stat_smooth(method = "lm", se = FALSE, color = "black") +
    labs(x = "Soil Health Index",
         y = expression("mg MAOC g"^-1~"soil")) +
    theme_minimal()
  ggsave("MAOC_health.jpeg", width = 15, height = 8)
  
   # Perform linear regression Prop MAOC
  regression_model_propMAOC <- lm(logitpropM ~ overall.score, data = data)
  summary(regression_model_propMAOC)
  
  # Create a plot with the regression line
  PropM_health <- ggplot(data, aes(x = overall.score, y = logitpropM, color=AP)) +
    geom_point(alpha = 0.5) +
    stat_smooth(method = "lm", se = FALSE, color = "black") +
    labs(x = "Soil Health Index",
         y = expression("Proportion of carbon as MAOC")) +
    theme_minimal()
  ggsave("PropM_health.jpeg", width = 15, height = 8)
  soilhealth3panel<- ggarrange(POC_health, MAOC_health, PropM_health,nrow=1,ncol=3, common.legend=T, legend="left", labels=c("a","b", "c"))
ggsave("soilhealth3panel.jpeg", width = 15, height = 8)
  
  # Define the coefficient for soil health regression (slopes)
  coef_POC <- .257  # in mg find in model summary
  coef_MAOC <- .377
  coef_PropM <- -.008
  # Define the mean values for each response variable
    mean_POC <- 8.17  
    mean_MAOC <- 16.77
    mean_PropM <- .76
  percentage_increase <- 10  #set a percentage increase of soil health based on data
    # Calculate the absolute increase in POC for the given percentage increase in aggregate stability
  absolute_increase_poc <- coef_POC * percentage_increase
  absolute_increase_maoc <- coef_MAOC * percentage_increase
  absolute_increase_propmaoc <- coef_PropM * percentage_increase
  # Calculate the percent increase in POC for a 1% increase in aggregate stability
  Onepercent_increase_poc <- (coef_POC / mean_POC) * 100
  Onepercent_increase_maoc <- (coef_MAOC / mean_MAOC) * 100
  Onepercent_increase_propmaoc <- (coef_PropM / mean_PropM) * 100
  # Calculate the percent increase in POC based on the mean value
  percent_increase_poc <- (absolute_increase_poc / mean_POC) * 100
  percent_increase_maoc <- (absolute_increase_maoc / mean_MAOC) * 100
  percent_increase_propmaoc <- (absolute_increase_propmaoc / mean_PropM) * 100
    # Print the result POC
  print(paste("Absolute increase in POC for a 10 unit in soil health index is", round(absolute_increase_poc, 2), "mg"))
  print(paste("Percent increase in POC for a 1 unit increase in soil health index is", round(Onepercent_increase_poc, 2), "%"))
  print(paste("Percent increase in POC for a", percentage_increase, "% increase in soil health is", round(percent_increase_poc, 2), "%"))
  # Print the result MAOC
  print(paste("Absolute increase in MAOC for a 10 unit in soil health index is", round(absolute_increase_maoc, 2), "mg"))
  print(paste("Percent increase in MAOC for a 1 unit increase in soil health index is", round(Onepercent_increase_maoc, 2), "%"))
  print(paste("Percent increase in MAOC for a", percentage_increase, "% increase in soil health is", round(percent_increase_maoc, 2), "%"))
  # Print the result Prop MAOC
  print(paste("Absolute increase in Prop MAOC for a 10 unit in soil health index is", round(absolute_increase_propmaoc, 2), "%"))
  print(paste("Percent increase in Prop MAOC for a 1 unit increase in soil health index is", round(Onepercent_increase_propmaoc, 2), "%"))
  print(paste("Percent increase in Prop MAOC for a", percentage_increase, "% increase in soil health is", round(percent_increase_propmaoc, 2), "%"))
  


#Correlation plot
cordata <- cor(data[,c("mgCpergSoilP","overall.score","mgCpergSoilM","logitpropM")], use="pairwise.complete.obs", method="pearson")
corrplot(cordata)
cordata

#view na for ph: Z1, Z2 are truly NA
missing_ph <- subset(data, is.na(ph) | ph == "")
missing_field_codes <- missing_ph$Field_Code
print(missing_field_codes)

missing_mgCpergSoilP <- subset(data, is.na(mgCpergSoilP) | mgCpergSoilP == "")
missing_field_codes <- missing_mgCpergSoilP$Field_Code
print(missing_field_codes)

#First, graphically explore relationships among independent variables to look for interactions


data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))

ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilP, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))

ggplot(data=data, aes(x=ppt.cm, y=mgCpergSoilP, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))

ggplot(data=data, aes(x=ph, y=mgCpergSoilP, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))

ggplot(data=data, aes(x=aggregate_stability, y=mgCpergSoilP, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))

ggplot(data=data, aes(x=active_carbon, y=mgCpergSoilP, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

#more interactions with ph
summary(data$ph)

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilP, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=ppt.cm, y=mgCpergSoilP, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=aggregate_stability, y=mgCpergSoilP, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=soil_texture_clay, y=mgCpergSoilP, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=active_carbon, y=mgCpergSoilP, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

#interactions with ppt

data <- data %>% 
  mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 110.0, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilP, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")


data <- data %>% 
  mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 110.0, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=aggregate_stability, y=mgCpergSoilP, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")

summary(data$ppt.cm)
data <- data %>% 
  mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 110.0, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilP, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")


data <- data %>% 
  mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 110.0, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=active_carbon, y=mgCpergSoilP, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")

#interactions with aggregate stability
summary(data$aggregate_stability)

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=ph, y=mgCpergSoilP, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=ppt.cm, y=mgCpergSoilP, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilP, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=soil_texture_clay, y=mgCpergSoilP, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=active_carbon, y=mgCpergSoilP, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

#tmeanC and active carbon
summary(data$tmeanC)

data <- data %>% 
  mutate(tmeanCcategory=cut(tmeanC, breaks=c(-Inf,6.8, 7.6, Inf), labels=c("low","med","high")))

ggplot(data=data, aes(x=active_carbon, y=mgCpergSoilP, col=tmeanC))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ tmeanCcategory, ncol=1, scales="free_x")

#Models
#Correlation plot
cordata <- cor(data[,c("mgCpergSoilP","ph","ppt.cm","tmeanC","aggregate_stability","soil_texture_clay","active_carbon")], use="pairwise.complete.obs", method="pearson")
corrplot(cordata)

##Linear Mixed Model for dependent variable (mgCpergSoilP)
#test without random effect, because only one value per field

m1P=gls(mgCpergSoilP~ppt.cm*tmeanC+soil_texture_clay+
          aggregate_stability+active_carbon+ 
          ph, data=data, na.action=na.exclude, method="ML")
summary(m1P)

m2P=gls(mgCpergSoilP~ppt.cm*tmeanC+soil_texture_clay+
          +aggregate_stability+active_carbon, data=data, na.action=na.exclude, method="ML")
summary(m2P)
anova(m1P, m2P)

m3P=gls(mgCpergSoilP~ppt.cm*tmeanC
           +aggregate_stability+active_carbon+ph, 
         data=data, na.action=na.exclude, method="ML")
summary(m3P)
anova(m2P,m3P)
anova(m3P)

n <- nobs(m3P)
print(n)


# Perform ANOVA on the model
anova_result <- anova(m3P)

# Print the ANOVA table
print(anova_result)

# Extract the degrees of freedom for each variable
num_df <- anova_result$"numDF"  # Numerator degrees of freedom
den_df <- anova_result$"denDF"  # Denominator degrees of freedom

# Print the degrees of freedom
print(num_df)
print(den_df)

#pseudo R squared calculation (fit between model predicted data and actual data)
data$mgCpergSoilP.pred=as.vector(fitted(m3P))
R4=lm(mgCpergSoilP~mgCpergSoilP.pred, data=data, na.action=na.omit)
summary(R4) #r2=.4825

#check assumptions, distrubution of residuals
#final model should use REML
m3P=gls(mgCpergSoilP~ppt.cm*tmeanC
        +aggregate_stability+active_carbon+ph,
        data=data, na.action=na.exclude, method="REML") 
summary(m3P)
####predict(x= aggregate_stability) percent increase over the range of value obererved. 

F_Final <- fitted(m3P)
R_Final <- residuals(m3P, type = "pearson", scaled = TRUE)
N = !is.na(data$mgCpergSoilP)
Rfull <- NA
Rfull[N] <- R_Final
op <- par(mfrow = c(2,2), mar = c(5,4,1,1))  
plot(F_Final, R_Final)
hist(Rfull)
plot(Rfull ~ data$aggregate_stability)
plot(Rfull ~ data$soil_texture_clay)
plot(Rfull ~ data$active_carbon)
plot(Rfull ~ data$tmeanC)
plot(Rfull ~ data$ppt.cm)
plot(Rfull ~ data$ph)
par(op)

#partial residuals to test that the relationship is linear
#do these for each of the significant predictors (do green and red lines match?)

op <- par(mfrow = c(2,2), mar = c(5,4,1,1)) #this makes it so all the graphs are plotted in the same window (a 2 x 2 grid)

ppt.cm.c <- summary(m3P)$coefficients[2] #predictor coefficient
ppt.cm.pr <- Rfull + ppt.cm.c*data$ppt.cm  #Residuals + pred coef * predictor value
{scatter.smooth(data$ppt.cm, ppt.cm.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(ppt.cm.c*data$ppt.cm ~ data$ppt.cm), col = "red")} 

soil_texture_clay.c <- summary(m3P)$coefficients[3] #predictor coefficient
soil_texture_clay.pr <- Rfull + soil_texture_clay.c*data$soil_texture_clay  #Residuals + pred coef * predictor value
{scatter.smooth(data$soil_texture_clay, soil_texture_clay.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(soil_texture_clay.c*data$soil_texture_clay ~ data$soil_texture_clay), col = "red")} 

aggregate_stability.c <- summary(m3P)$coefficients[5] #predictor coefficient
aggregate_stability.pr <- Rfull + aggregate_stability.c*data$aggregate_stability  #Residuals + pred coef * predictor value
{scatter.smooth(data$aggregate_stability, aggregate_stability.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(aggregate_stability.c*data$aggregate_stability ~ data$aggregate_stability), col = "red")} 

active_carbon.c <- summary(m3P)$coefficients[6] #predictor coefficient
active_carbon.pr <- Rfull + active_carbon.c*data$active_carbon  #Residuals + pred coef * predictor value
{scatter.smooth(data$active_carbon, active_carbon.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(active_carbon.c*data$active_carbon ~ data$active_carbon), col = "red")} 

par(op)

#Visualize significant relationships

#own_theme below sets ggplot parameters for how plots should look. 
own_theme <- theme_bw(base_size = 11) +
  theme(rect = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank())

#ppt and tmeanC, option 1
summary(data$ppt.cm)
summary(data$tmeanC)

pred_pptC <- ggpredict(m3P, terms = c("ppt.cm", "tmeanC[6.8,7.5]"))
pred_pptC$tmean_group <- pred_pptC$group
levels(pred_pptC$tmean_group) <- c("low (4.5-7.2)",  
                                   "high (7.2-8.6)")
data <- data %>%
  drop_na(tmeanC) %>% 
  dplyr::mutate(tmean_group = cut(tmeanC, breaks = c(4.5,7.2,8.6)))

levels(data$tmean_group) <- c("low (4.5-7.2)",  
                              "high (7.2-8.6)")

mgPOM_pptC <-data %>% 
  ggplot() +
  geom_point(aes(x = ppt.cm, y = mgCpergSoilP, col = tmean_group), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_pptC, mapping = aes(x=x, y=predicted, col = tmean_group), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  #theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in POM per g soil")) +
  scale_x_continuous(expression("Mean Annual Precipitation (cm)"),
                     label = scales::comma) +
  scale_color_manual(values = c("blue", "red")) # adjust colors if needed

mgPOM_pptC

ggsave("mgPOM_ppt_tmeanC.jpeg", width = 4, height = 3)


#ppt and tmeanC, option 2, Figure 4 Final
summary(data$ppt.cm)
pred_tmeanppt <- ggpredict(m3P, terms = c("tmeanC","ppt.cm[101,110]"))
pred_tmeanppt$ppt_group <- pred_tmeanppt$group
levels(pred_tmeanppt$ppt_group) <- c("low (92-104)",  
                                   "high (104-142)")
data <- data %>%
  drop_na(ppt.cm) %>% 
   dplyr::mutate(ppt_group = cut(ppt.cm, breaks = c(92,104,142)))
 levels(data$ppt_group) <- c("low (92-104)",  
                            "high (104-142)")
  mgPOM_tmeanppt <-data %>% 
   ggplot() +
   geom_point(aes(x = tmeanC, y = mgCpergSoilP, col = ppt_group), #plot your data
              size = 1.5, alpha = 0.5) +
   geom_line(pred_tmeanppt, mapping = aes(x=x, y=predicted, col = ppt_group), #plot the model's prediction (based on linear )
             lwd = 1) +
   own_theme+
   #theme(legend.position = "none") +
   scale_y_continuous(expression("mg POC g"^-1 * "soil")) +
   scale_x_continuous(expression("Mean Annual Temperature(°C)"),
                      label = scales::comma) +
   scale_color_manual(name = "Mean Annual Precipitation (cm)", values = c("red", "blue"))+
    theme_minimal()
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )
 mgPOM_tmeanppt
 ggsave("mgPOM_Figure4.jpeg", width = 10, height = 8)
 
 
 #fix font
 library(ggplot2)
 library(dplyr)
 library(ggeffects)  # Ensure ggeffects is loaded for ggpredict function
 
 # Generate predictions
 pred_tmeanppt <- ggpredict(m3P, terms = c("tmeanC", "ppt.cm[101,110]"))
 
 # Assign and update the ppt_group levels in pred_tmeanppt
 pred_tmeanppt$ppt_group <- pred_tmeanppt$group
 pred_tmeanppt$ppt_group <- factor(pred_tmeanppt$ppt_group, 
                                   levels = c("low", "high"),
                                   labels = c("low (92-104)", "high (104-142)"))
 
 # Prepare the data
 data <- data %>%
   drop_na(ppt.cm) %>%
   mutate(ppt_group = cut(ppt.cm, breaks = c(92, 104, 142),
                          labels = c("low (92-104)", "high (104-142)")))
 
 # Plot the data
 mgPOM_tmeanppt <- ggplot(data) +
   geom_point(aes(x = tmeanC, y = mgCpergSoilP, color = ppt_group), 
              size = 1.5, alpha = 0.5) +
   geom_line(data = pred_tmeanppt, aes(x = x, y = predicted, color = ppt_group), 
             size = 1) +
   labs(
     title = "Relationship between Mean Annual Temperature and mg POC per g Soil",
     x = "Mean Annual Temperature (°C)",
     y = expression("mg POC g"^-1 * "soil"),
     color = "Mean Annual Precipitation (cm)"
   ) +
   scale_color_manual(values = c("low (92-104)" = "red", "high (104-142)" = "blue")) +
   theme_minimal() +
   theme(
     axis.title = element_text(size = 14),
     axis.text = element_text(size = 12),
     legend.title = element_text(size = 12),
     legend.text = element_text(size = 10)
   )
 
 # Display the plot
 print(mgPOM_tmeanppt)
 
 # Save the plot
 ggsave("mgPOM_Figure4.jpeg", plot = mgPOM_tmeanppt, width = 10, height = 8)
 ``
 
 
 
 
 #tying to get line to end
 #ppt and tmeanC, option 2
 # Ensure predictions are within the range of observed tmeanC values
 # Identify the range of observed data

  # Determine the range of observed data for both factors
 range_tmeanC <- range(data$tmeanC, na.rm = TRUE)
 range_ppt_cm <- range(data$ppt.cm, na.rm = TRUE)
 
 # Generate predictions across a grid within the range of observed data
 tmeanC_seq <- seq(from = range_tmeanC[1], to = range_tmeanC[2], length.out = 100)
 ppt_cm_seq <- seq(from = range_ppt_cm[1], to = range_ppt_cm[2], length.out = 100)
 
 # Create a data frame for prediction
 prediction_data <- expand.grid(tmeanC = tmeanC_seq, ppt.cm = ppt_cm_seq)
 
 # Generate predictions using the model
 predictions <- predict(m3P, newdata = prediction_data, interval = "confidence")

 
summary(m3P)
 # Combine predictions with the prediction data
 pred_tmeanppt <- cbind(prediction_data, predicted = predictions)
 

 pred_tmeanppt <- ggpredict(m3P, terms = c("tmeanC","ppt.cm[101,110]"))
 pred_tmeanppt$ppt_group <- pred_tmeanppt$group
 levels(pred_tmeanppt$ppt_group) <- c("low (92-104)",  
                                      "high (104-142)")
 data <- data %>%
   drop_na(ppt.cm) %>% 
   dplyr::mutate(ppt_group = cut(ppt.cm, breaks = c(92,104,142)))
 
 levels(data$ppt_group) <- c("low (92-104)",  
                             "high (104-142)")
 
 
 # Plot the data and the fitted line
 mgPOM_tmeanppt <- data %>%
   ggplot() +
   geom_point(aes(x = tmeanC, y = mgCpergSoilP, col = ppt_group), size = 1.5, alpha = 0.5) +
   geom_line(data = pred_tmeanppt, aes(x = x, y = predicted, col = ppt_group), lwd = 1) +
   own_theme +
   scale_y_continuous(expression("mg POC g"^-1~"soil")) +
   scale_x_continuous(expression("Mean Annual Temperature (°C)"),
                      label = scales::comma) +
   scale_color_manual(values = c("red", "blue")) +
   labs(color = "Precipitation Group")
 
 print(mgPOM_tmeanppt)
 
 ggsave(" mgPOM_tmeanppt.jpeg", width = 10, height = 8)
view(data)

#for agg stability color by AP
pred_aggregate_stability <- ggpredict(m3P, terms = c("aggregate_stability","AP"))
mgPOM_aggregate_stability <-data %>% 
  ggplot() +
  geom_point(aes(x = aggregate_stability, y = mgCpergSoilP,color=AP),
                     size = 1.5, alpha = 0.5) +
  geom_line(pred_aggregate_stability, mapping = aes(x=x, y=predicted,color=group), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  #theme(legend.position = "none") +
  scale_y_continuous(
    name = expression("mg POC " * g^{-1} * " soil")
  )
scale_x_continuous(expression("Aggregate Stability (%)"),
                     label = scales::comma) 

mgPOM_aggregate_stability
ggsave("mgPOM_aggregate_stability.jpeg", width = 15, height = 8, units="cm")

library(ggplot2)
library(ggeffects)
library(dplyr)

# Define the coefficient for active carbon
coef_activecarbon <-  0.01317  # in ppm
# Define the mean value of POC (baseline POC)
mean_POC <- 8.17  # Replace with the actual mean value from your data
# Define the ppm increase in active carbon
ppm_increase <- 100  # For a 100 ppm increase in active carbon

# Calculate the absolute increase in POC for the given ppm increase in active carbon
absolute_increase_poc <- coef_activecarbon * ppm_increase

# Calculate the percent increase in POC based on the mean value
percent_increase_poc <- (absolute_increase_poc / mean_POC) * 100

# Print the result
print(paste("Percent increase in POC for a", ppm_increase, "ppm in active carbon is", round(percent_increase_poc, 2), "%"))


# Assuming 'data' is your data frame and 'mgCpergSoilP' is the column name for POC
mean_poc <- mean(data$mgCpergSoilP, na.rm = TRUE)

# Print the mean POC value
print(paste("The mean POC is", round(mean_poc, 2), "mg"))



# Generate predictions from the model
pred_aggregate_stability <- ggpredict(m3P, terms = c("aggregate_stability"))
# Convert the ggpredict object to a data frame
pred_df <- as.data.frame(pred_aggregate_stability)

# Create the plot

mgPOM_aggregate_stability <- data %>%
  ggplot(aes(x = aggregate_stability, y = mgCpergSoilP)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_line(data = pred_df, aes(x = x, y = predicted, color = "black"), lwd = 1) +  # Plot the model's prediction
  own_theme +  # Ensure 'own_theme' is defined or remove it
  theme(legend.position = "none") +
   scale_y_continuous(
    name = expression("mg POC " * g^{-1} * " soil")) +
  scale_x_continuous(name = expression("Aggregate Stability (%)"),
    labels = scales::comma) 
  mgPOM_aggregate_stability
  
  mgPOM_aggregate_stability <- data %>%
    ggplot(aes(x = aggregate_stability, y = mgCpergSoilP)) +
    geom_point(size = 1.5, alpha = 0.5) +
    geom_line(data = pred_df, aes(x = x, y = predicted), color = "black", lwd = 1) +  # Plot the model's prediction
    own_theme +  # Ensure 'own_theme' is defined or remove it
    theme(legend.position = "none") +
    scale_y_continuous(name = expression("mg POC " * g^{-1} * " soil")) +
    scale_x_continuous(name = expression("Aggregate Stability (%)"),
                       labels = scales::comma)
  
  mgPOM_aggregate_stability
  
ggsave("mgPOM_aggregate_stability.jpeg", width = 4, height = 3)

# Define the coefficient for aggregate stability
coef_aggregate_stability <- 0.06799  # in mg find in model summary
# Define the mean value of POC 
mean_POC <- 8.17  # Replace with the actual mean value from data
percentage_increase <- 10  #set a percentage increase of agg stability based on data

# Calculate the absolute increase in POC for the given percentage increase in aggregate stability
absolute_increase_poc <- coef_aggregate_stability * percentage_increase
# Calculate the percent increase in POC for a 1% increase in aggregate stability
Onepercent_increase_poc <- (coef_aggregate_stability / mean_POC) * 100
# Calculate the percent increase in POC based on the mean value
percent_increase_poc <- (absolute_increase_poc / mean_POC) * 100

# Print the result
print(paste("Absolute increase in POC for a 10% increase in aggregate stability is", round(absolute_increase_poc, 2), "mg"))
# Print the result
print(paste("Percent increase in POC for a 1% increase in aggregate stability is", round(Onepercent_increase_poc, 2), "%"))
# Print the result
print(paste("Percent increase in POC for a", percentage_increase, "% increase in aggregate stability is", round(percent_increase_poc, 2), "%"))


# Define the coefficient for active carbon
coef_activecarbon <-  0.01317  # in ppm
# Define the mean value of POC (baseline POC)
mean_POC <- 8.17  # Replace with the actual mean value from your data
# Define the ppm increase in active carbon
ppm_increase <- 100  # For a 100 ppm increase in active carbon

# Calculate the absolute increase in POC for the given ppm increase in active carbon
absolute_increase_poc <- coef_activecarbon * ppm_increase

# Calculate the percent increase in POC based on the mean value
percent_increase_poc <- (absolute_increase_poc / mean_POC) * 100

# Print the result
print(paste("Percent increase in POC for a", ppm_increase, "ppm in active carbon is", round(percent_increase_poc, 2), "%"))


# Assuming 'data' is your data frame and 'mgCpergSoilP' is the column name for POC
mean_poc <- mean(data$mgCpergSoilP, na.rm = TRUE)

# Print the mean POC value
print(paste("The mean POC is", round(mean_poc, 2), "mg"))

#Figure 3b

# Generate predictions from the model
pred_active_carbon <- ggpredict(m3P, terms = c("active_carbon"))
# Convert the ggpredict object to a data frame
pred_df <- as.data.frame(pred_active_carbon)

# Create the plot
mgPOM_active_carbon <- data %>%
  ggplot(aes(x = active_carbon, y = mgCpergSoilP)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_line(data = pred_df, aes(x = x, y = predicted), color = "black", lwd = 1) +  # Change color of the line
  own_theme +  # Ensure 'own_theme' is defined or remove it
  theme(legend.position = "none") +
  scale_y_continuous(
    name = expression("mg POC " * g^{-1} * " soil")
  ) +
  scale_x_continuous(
    name = expression("Active Carbon (ppm)"),
    labels = scales::comma
  )

mgPOM_active_carbon
ggarrange(mgPOM_aggregate_stability,mgPOM_active_carbon,nrow=1, common.legend=T, legend="left", labels=c("a","b"))

#ppt on own
pred_ppt <- ggpredict(m3P, terms = c("ppt.cm"))
mgPOM_ppt <-data %>% 
  ggplot() +
  geom_point(aes(x = ppt.cm, y = mgCpergSoilP), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_ppt, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in POM per g soil"))+
  scale_x_continuous(expression("mean annual precipitation (cm)"),
                     label = scales::comma) 

mgPOM_ppt

#tmeanC on own
pred_temp <- ggpredict(m3P, terms = c("tmeanC"))
mgPOM_temp <-data %>% 
  ggplot() +
  geom_point(aes(x = tmeanC, y = mgCpergSoilP), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_temp, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in POM per g soil"))+
  scale_x_continuous(expression("mean annual temperature (C)"),
                     label = scales::comma) 

mgPOM_temp


# Create a violin plot with individual data points
ggplot(data, aes(x = Type.x, y = mgCpergSoilP)) +
  geom_violin(trim = FALSE, fill = "lightblue") +  # Create the violin plot
  geom_jitter(width = 0.2, size = 1, color = "darkblue") +  # Add jittered points
  labs(title = "Distribution of mg POM by Field Type",
       x = "Field Type",
       y = "mgC per g Soil POM") +
  theme_minimal()  # Apply a minimal theme for a clean look   

# Create a violin plot with individual data points and mean line
ggplot(data, aes(x = Type.x, y = mgCpergSoilP, color = Type.x, fill = Type.x)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Create the violin plot with semi-transparent fill
  geom_jitter(width = 0.2, size = 1) +  # Add jittered points
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, color = "black", fill = "yellow") +  # Add mean points
  labs(title = "Distribution of mgCpergSoilP by Field Type",
       x = "Field Type",
       y = "mgC per g Soil POM") +
  theme_minimal()  # Apply a minimal theme for a clean look

# Create a violin plot with individual data points and mean line for soil texture class
ggplot(data, aes(x = soil_texture_class, y = mgCpergSoilP, color = soil_texture_class, fill = soil_texture_class)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Create the violin plot with semi-transparent fill
  geom_jitter(width = 0.2, size = 1) +  # Add jittered points
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, color = "black", fill = "yellow") +  # Add mean points
  labs(title = "Distribution of mgCpergSoilP by Field Type",
       x = "Soil Texture Class",
       y = "mgC per g Soil POM") +
  theme_minimal()  # Apply a minimal theme for a clean look

# Create a violin plot with individual data points and mean line for soil texture class
ggplot(data, aes(x = soil_texture_class, y = mgCpergSoilP, color = soil_texture_class, fill = soil_texture_class)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Create the violin plot with semi-transparent fill
  geom_jitter(width = 0.2, size = 1) +  # Add jittered points
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, color = "black", fill = "yellow") +  # Add mean points
  labs(title = "Distribution of mgCpergSoilP by Soil Texture Class",
       x = "Soil Texture Class",
       y = "mgC per g Soil POM") +
  theme_minimal() +  # Apply a minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Adjust text angle, justification, and size
        plot.margin = margin(5, 5, 10, 5))  # Increase the bottom margin to give more space to labels

# Load necessary library
library(ggplot2)  # For plotting


# Get unique options in soil_texture_class
unique_soil_texture <- unique(data$soil_texture_class)

# Print the unique options
print(unique_soil_texture)

view(data$soil_texture_clay)

# Soil texture and field type
set.seed(123)
data <- data.frame(
  soil_texture_class = sample(c("Sandy", "Loam", "Clayey"), 100, replace = TRUE),
  Type.x = sample(c("Corn", "field crops", "hay", "pasture", "Veg", "corn"), 100, replace = TRUE),
  mgCpergSoilP = rnorm(100, mean = 10, sd = 2)
)

# Create a boxplot or violin plot
ggplot(data, aes(x = cut(soil_texture_clay, breaks = 5), y = Type.x)) +
  geom_boxplot() +  # or geom_violin() for a violin plot
  labs(x = "Soil Texture Clay Levels", y = "Field Type") +
  ggtitle("Comparison of Field Types across Soil Texture Clay Levels")


# Create a violin plot with individual data points and mean line for soil texture and field type
ggplot(data, aes(x = soil_texture_class, y = mgCpergSoilP, fill = Type.x)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Create the violin plot with semi-transparent fill
  geom_jitter(width = 0.2, size = 1, position = position_jitterdodge()) +  # Add jittered points, dodge by Type.x
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, color = "black", fill = "yellow", position = position_dodge(width = 0.2)) +  # Add mean points, dodge by Type.x
  labs(title = "Distribution of mgCpergSoilP by Soil Texture and Field Type",
       x = "Soil Texture Class",
       y = "mgC per g Soil POM",
       fill = "Field Type") +
  theme_minimal() +  # Apply a minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Adjust text angle, justification, and size
        plot.margin = margin(5, 5, 10, 5))  # Increase the bottom margin to give more space to labels


#anova by field type to see differences 
field_anova<- aov(mgCpergSoilP~Type.x, data=data)
summary(field_anova)  

TukeyHSD(field_anova)

# Calculate means by field type
means_by_field_type <- data %>%
  group_by(Type.x) %>%
  summarise(mean_mgCpergSoilP = mean(mgCpergSoilP, na.rm = TRUE))

print(means_by_field_type)

#anova by field type to see differences 
field_anova<- aov(mgCpergSoilM~Type.x, data=data)
summary(field_anova)  

TukeyHSD(field_anova)

# Calculate means by field type
means_by_field_type <- data %>%
  group_by(Type.x) %>%
  summarise(mean_mgCpergSoilM = mean(mgCpergSoilM, na.rm = TRUE))

print(means_by_field_type)


#anova by field type to see differences 
field_anova<- aov(propM~Type.x, data=data)
summary(field_anova)  
# Calculate means by field type
TukeyHSD(field_anova)
means_by_field_type <- data %>%
  group_by(Type.x) %>%
  summarise(mean_propM = mean(propM, na.rm = TRUE))

print(means_by_field_type)


# Create the new column 'AP'
data <- data %>%
  mutate(AP = case_when(
    Type.x %in% c("Hay", "Pasture") ~ "pere",
    Type.x %in% c("Corn", "Veg") ~ "annual",
    TRUE ~ NA_character_ # This handles any values not specified in the conditions
  ))

# View the updated dataframe
print(data)
view(data)

#anova by AP to see differences 
AP_anova<- aov(mgCpergSoilP~AP, data=data)
summary(AP_anova)  

TukeyHSD(AP_anova)

# Calculate the means and standard errors
library(dplyr)

# Calculate means
means <- data %>%
  group_by(AP) %>%
  summarise(mean_mgCpergSoilP = mean(mgCpergSoilP, na.rm = TRUE))

# Calculate standard errors
errors <- data %>%
  group_by(AP) %>%
  summarise(
    mean_mgCpergSoilP = mean(mgCpergSoilP, na.rm = TRUE),
    se_mgCpergSoilP = sd(mgCpergSoilP, na.rm = TRUE) / sqrt(n())
  )

# Print the results
print(means)
print(errors)


#AP data for MAOC
#anova by AP to see differences 
APM_anova<- aov(mgCpergSoilM~AP, data=data)
summary(APM_anova)  

TukeyHSD(APM_anova)
summary(APM_anova)

# Calculate the means and standard errors
library(dplyr)

# Calculate means
means <- data %>%
  group_by(AP) %>%
  summarise(mean_mgCpergSoilM = mean(mgCpergSoilM, na.rm = TRUE))

# Calculate standard errors
errors <- data %>%
  group_by(AP) %>%
  summarise(
    mean_mgCpergSoilM = mean(mgCpergSoilM, na.rm = TRUE),
    se_mgCpergSoilM = sd(mgCpergSoilM, na.rm = TRUE) / sqrt(n())
  )

# Print the results
print(means)
print(errors)


#AP data for logitprop MAOC
#anova by AP to see differences 
APPM_anova<- aov(logitpropM~AP, data=data)
summary(APPM_anova)  

TukeyHSD(APM_anova)
summary(APM_anova)

# Calculate the means and standard errors
library(dplyr)

# Calculate means
means <- data %>%
  group_by(AP) %>%
  summarise(mean_logitpropM = mean(logitpropM, na.rm = TRUE))

# Calculate standard errors
errors <- data %>%
  group_by(AP) %>%
  summarise(
    mean_logitpropM = mean(logitpropM, na.rm = TRUE),
    se_logitproplM = sd(logitpropM, na.rm = TRUE) / sqrt(n())
  )

# Print the results
print(means)
print(errors)

#AP data for soil health score
#anova by AP to see differences 
APSH_anova<- aov(overall.score~AP, data=data)
summary(APSH_anova)  

TukeyHSD(APSH_anova)
summary(APSH_anova)

# Calculate means
means <- data %>%
  group_by(AP) %>%
  summarise(mean_overall.score = mean(overall.score, na.rm = TRUE))

# Calculate standard errors
errors <- data %>%
  group_by(AP) %>%
  summarise(
    mean_overall.score = mean(overall.score, na.rm = TRUE),
    se_overall.score = sd(overall.score, na.rm = TRUE) / sqrt(n())
  )

# Print the results
print(means)
print(errors)


#anova by field type to see differences MAOC
field_anova<- aov(logitpropM~Type.x, data=data)
summary(field_anova)  

TukeyHSD(field_anova)

# Calculate means
means <- data %>%
  group_by(Type.x) %>%
  summarise(mean_mgCpergSoilM = mean(mgCpergSoilM, na.rm = TRUE))

# Calculate standard errors
errors <- data %>%
  group_by(Type.x) %>%
  summarise(
    mean_mgCpergSoilM = mean(mgCpergSoilM, na.rm = TRUE),
    se_mgCpergSoilM = sd(mgCpergSoilM, na.rm = TRUE) / sqrt(n())
  )

# Print the results
print(means)
print(errors)

# Calculate means
means <- data %>%
  group_by(Type.x) %>%
  summarise(mean_logitpropM = mean(logitpropM, na.rm = TRUE))

# Calculate standard errors
errors <- data %>%
  group_by(Type.x) %>%
  summarise(
    mean_logitPropM = mean(logitpropM, na.rm = TRUE),
    se_PropM = sd(propM, na.rm = TRUE) / sqrt(n())
  )

# Print the results
print(means)
print(errors)


#anova by soil texture class to see differences 
texture_anova<- aov(mgCpergSoilP~soil_texture_class, data=data)
summary(texture_anova)  

TukeyHSD(texture_anova)
# Create a new column Tillage_Category with descriptive labels
data <- data %>%
  mutate(Tillage_Category = factor(Tillage_1to4,
                                   levels = c(1, 2, 3, 4),
                                   labels = c("No Till", 
                                              "1-7 inch Till", 
                                              "7-9 inch Till", 
                                              ">9 inch Till")))



# Check the new column
summary(data$Tillage_Category)


# Update Type.x: Change "Field crops" to "Wheat"
data <- data %>%
  mutate(Type.x = recode(Type.x, "Field crops" = "Wheat"))

# Check the result to ensure the changes were applied correctly
table(data$Type.x)
library(dplyr)

# Convert Type.x to character if it is a factor
data <- data %>%
  mutate(Type.x = as.character(Type.x))

# Update Type.x: Change "Field crops" to "Wheat"
data <- data %>%
  mutate(Type.x = recode(Type.x, "Field crops" = "Wheat"))

library(dplyr)

# Inspect the data type and unique values in Type.x
str(data$Type.x)  # Check the structure of the column
unique(data$Type.x)  # Check unique values

# Convert Type.x to character if it is a factor
data <- data %>%
  mutate(Type.x = as.character(Type.x))

# Update Type.x: Change "Field crops" to "Wheat"
data <- data %>%
  mutate(Type.x = ifelse(Type.x == "Field crops", "Wheat", Type.x))

# Optionally convert back to factor
data$Type.x <- factor(data$Type.x)

# Check the result to ensure the changes were applied correctly
table(data$Type.x)




# Check the result to ensure the changes were applied correctly
table(data$Type.x)

# Create the stacked bar plot

ggplot(data, aes(x = Type.x, fill = factor(Tillage_Category))) +
  geom_bar(position = "stack") +
  labs(title = "",
       x = "Crop Type",
       y = "Number of Fields",
       fill = "Tillage Category") +
  theme_minimal()

library(dplyr)

# Inspect the data type and unique values in Type.x
str(data$Type.x)  # Check the structure of the column
unique(data$Type.x)  # Check unique values

# Convert Type.x to character if it is a factor
data <- data %>%
  mutate(Type.x = as.character(Type.x))

# Update Type.x: Change "Field crops" to "Wheat"
data <- data %>%
  mutate(Type.x = ifelse(Type.x == "Field crops", "Wheat", Type.x))

# Reorder and convert Type.x to factor with the desired levels
data$Type.x <- factor(data$Type.x,
                      levels = c("Hay", "Pasture", "Corn", "Veg", "Wheat"))

# Check the result to ensure the changes were applied correctly
table(data$Type.x)


# Create the stacked bar plot
ggplot(data, aes(x = Type.x, fill = factor(till.passes))) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Tillage Passes by Field Type",
       x = "Field Type",
       y = "Count",
       fill = "Tillage Category") +
  theme_minimal()


# Perform linear regression
regression_modelPM <- lm(mgCpergSoilM ~ mgCpergSoilP, data = data)

# Summarize the regression model
summary(regression_modelPM)


Figure8<-ggplot(data, aes(x = mgCpergSoilP, y = mgCpergSoilM)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    x = expression(paste("mg POC ", g^{-1}, " soil")),
    y = expression(paste("mg MAOC ", g^{-1}, " soil"))
  ) +
  theme_minimal()
Figure8
ggsave("Figure8.jpeg", width = 15, height = 8)
  
  # Subset the data frame to get only the rows where Type.x is 'Field_crops'
  field_crops_samples <- subset(data, Type.x == "Field crops")
  
  # Extract the Field_code for those samples
  field_crops_codes <- field_crops_samples$Field_Code
  
  # Print the result
  print(field_crops_codes)
  
  library(ggplot2)
  library(dplyr)
  library(car) # for Levene's Test
  library(ggpubr) # for easy plotting
  
  # Check the distribution of pH for each Type.x using boxplots
  ggboxplot(data, x = "Type.x", y = "ph", 
            color = "Type.x", palette = "jco",
            ylab = "pH", xlab = "Field Type")
  
  # Check the normality assumption
  shapiro_test <- data %>%
    group_by(Type.x) %>%
    summarise(shapiro_p = shapiro.test(ph)$p.value)
  
  print(shapiro_test)
  
  # Check the homogeneity of variances assumption
  levene_test <- leveneTest(ph ~ Type.x, data = data)
  
  print(levene_test)
  
  # If p-value of Shapiro-Wilk test and Levene's test are greater than 0.05, proceed with ANOVA
  if (all(shapiro_test$shapiro_p > 0.05) && levene_test$p.value > 0.05) {
    # Perform ANOVA
    anova_result <- aov(ph ~ Type.x, data = data)
    summary(anova_result)
    
    # Post-hoc test if ANOVA is significant
    if (summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
      tukey_result <- TukeyHSD(anova_result)
      print(tukey_result)
    }
  } else {
    # If assumptions are not met, perform the Kruskal-Wallis test
    kruskal_result <- kruskal.test(ph ~ Type.x, data = data)
    print(kruskal_result)
  }
  
  
  # Post-hoc test if ANOVA is significant
  if (summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
    tukey_result <- TukeyHSD(anova_result)
    print(tukey_result)
    
    # Plot the Tukey HSD results
    plot(tukey_result)
  }
  } else {
    # If assumptions are not met, perform the Kruskal-Wallis test
    kruskal_result <- kruskal.test(ph ~ Type.x, data = data)
    print(kruskal_result)
    
    # If Kruskal-Wallis test is significant, perform post-hoc Dunn test
    if (kruskal_result$p.value < 0.05) {
      library(FSA) # For Dunn test
      dunn_result <- dunnTest(pH ~ Type.x, data = data, method = "bh")
      print(dunn_result)
    }
  }




# test for differences in pH for field types
#all CIs overlap indicatin no sigficant difference
# Perform ANOVA
anova_result <- aov(ph ~ Type.x, data = data)
# Summary of ANOVA
summary(anova_result)
# Conduct post-hoc tests using Tukey's HSD test for pairwise comparisons
posthoc <- emmeans(anova_result, ~ Type.x)
# Print pairwise comparisons
print(posthoc, type = "compact")

# Example scatter plot with jitter
ggplot(data, aes(x = Type.x, y = ph)) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.7) +
  labs(x = "Field Type", y = "pH") +
  ggtitle("Relationship between pH and Field Type") +
  theme_minimal()

# look at the normality of each variable within each group
shapiro_test_results <- data %>%
  group_by(Type.x) %>%
  summarise(
    K = shapiro.test(k)$p.value,
    Mn = shapiro.test(mn)$p.value,
    Fe = shapiro.test(fe)$p.value,
    Mg = shapiro.test(mg)$p.value,
    Zn = shapiro.test(zn)$p.value
  )
print(shapiro_test_results)

# Perform MANOVA
manova_result <- manova(cbind(k, mn, fe, mg, zn) ~ Type.x, data = data)
summary(manova_result)

# If MANOVA is significant, perform post-hoc tests
summary.aov(manova_result)

# Perform separate ANOVAs with Bonferroni correction
anova_results <- list(
  K = aov(k ~ Type.x, data = data),
  Mn = aov(mn ~ Type.x, data = data),
  Fe = aov(fe ~ Type.x, data = data),
  Mg = aov(mg ~ Type.x, data = data),
  Zn = aov(zn ~ Type.x, data = data)
)

# Print ANOVA summaries
lapply(anova_results, summary)

# Apply Bonferroni correction (or other corrections) to p-values
p_values <- sapply(anova_results, function(x) summary(x)[[1]]$`Pr(>F)`[1])
p_values_adjusted <- p.adjust(p_values, method = "bonferroni")

print(data.frame(Variable = names(p_values), P_value = p_values, Adjusted_P_value = p_values_adjusted))
}

# Perform ANOVAs for each cation
anova_results <- list(
  K = aov(k ~ Type.x, data = data),
  Mn = aov(mn ~ Type.x, data = data),
  Fe = aov(fe ~ Type.x, data = data),
  Mg = aov(mg ~ Type.x, data = data),
  Zn = aov(zn ~ Type.x, data = data)
)

# Print ANOVA summaries
anova_summaries <- lapply(anova_results, summary)
print(anova_summaries)

# Perform Tukey's HSD test if ANOVA is significant
tukey_results <- lapply(anova_results, function(x) {
  if (summary(x)[[1]]$`Pr(>F)`[1] < 0.05) {
    TukeyHSD(x)
  } else {
    NULL
  }
})

# Print Tukey's HSD results
print(tukey_results)

# Optionally plot Tukey's HSD results
par(mfrow = c(3, 2))
for (i in 1:length(tukey_results)) {
  if (!is.null(tukey_results[[i]])) {
    plot(tukey_results[[i]], main = names(tukey_results)[i])
  }
}

#regression of MAOM and tillage

regression_model <- lm(mgCpergSoilP ~ Tillage_1to4, data = data)

#Summarize the regression model
summary(regression_model)

#graph the regression

# Create a new data frame with predictions
data_with_predictions <- data %>%
  mutate(predicted = predict(regression_model, newdata = data))

library(ggplot2)
library(dplyr)

# Assuming regression_model is already created and data_with_predictions is calculated

# Create a new data frame with predictions for plotting
data_with_predictions <- data %>%
  mutate(predicted = predict(regression_model, newdata = data)) %>%
  group_by(Tillage_1to4) %>%
  summarize(mean_predicted = mean(predicted), 
            mean_mgCpergSoilP = mean(mgCpergSoilP), 
            .groups = 'drop')

# Plot the data
ggplot(data, aes(x = Tillage_1to4, y = mgCpergSoilP)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.5) +  # Scatter plot with jitter to avoid overplotting
  geom_point(data = data_with_predictions, aes(x = Tillage_1to4, y = mean_predicted), color = "red", size = 3, shape = 1) +  # Predicted values
  geom_line(data = data_with_predictions, aes(x = Tillage_1to4, y = mean_predicted, group = 1), color = "blue", size = 1) +  # Regression line
  labs(title = "Regression of mgCpergSoilP on Tillage_1to4",
       x = "Tillage Category",
       y = "POC mg C per g Soil",
       color = "Legend") +
  scale_x_discrete(labels = c("1" = "No Till", "2" = "1-7inch Till", "3" = "7-9inch Till", "4" = ">9inch Till")) +  # Customize x-axis labels
  theme_minimal()


# Example scatter plot with jitter
library(ggplot2)

ggplot(data, aes(x = as.factor(Tillage_1to4), y = mgCpergSoilP)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.7) +
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 3, color = "red") +
  labs(x = "Tillage", y = "POC") +
  ggtitle("Relationship between Tillage and POC") +
  theme_minimal()

# Load necessary library
library(dplyr)

#Ensure Tillage_1to4 is a factor
data <- data %>%
  mutate(Tillage_1to4 = as.factor(Tillage_1to4))

#Perform ANOVA
anova_result <- aov(mgCpergSoilP ~ Tillage_1to4, data = data)
summary(anova_result)

#Perform Tukey's HSD test if ANOVA is significant
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Optionally, plot the Tukey HSD results
plot(tukey_result)


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(multcomp)

# Create a new variable that combines Tillage_1to4 = 2, 3, and 4 into a single group
data <- data %>%
  mutate(Tillage_Grouped = ifelse(Tillage_1to4 == 1, "1", "2, 3, 4"))

# Ensure the new variable is a factor
data$Tillage_Grouped <- as.factor(data$Tillage_Grouped)

# Perform ANOVA
anova_result <- aov(mgCpergSoilM ~ Tillage_Grouped, data = data)
summary(anova_result)

# If ANOVA is significant, perform Tukey's HSD test
if (summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  plot(tukey_result)
} else {
  print("No significant difference between tillage groups.")
}

#compare the means
# Calculate means of mgCpergSoilP for each Tillage Group
means <- data %>%
  group_by(Tillage_Grouped) %>%
  summarise(Mean_mgCpergSoilP = mean(mgCpergSoilP, na.rm = TRUE))

print(means)

# Perform ANOVA
anova_result <- aov(mgCpergSoilP ~ Tillage_Grouped, data = data)
summary(anova_result)

# If ANOVA is significant, perform Tukey's HSD test
if (summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  plot(tukey_result)
} else {
  print("No significant difference between tillage groups.")
}

#compare the means
# Calculate means of mgCpergSoilM for each Tillage Group
means <- data %>%
  group_by(Tillage_Grouped) %>%
  summarise(Mean_mgCpergSoilM = mean(mgCpergSoilM, na.rm = TRUE))

print(means)

# Create box plot with means
ggplot(data, aes(x = Tillage_Grouped, y = mgCpergSoilM)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.7) +
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 3, color = "red") +
  labs(x = "Tillage Grouped", y = "MAOM") +
  ggtitle("Relationship between Tillage and MAOM") +
  theme_minimal()

#Aggregrate stabiity by field type

#Perform ANOVA
anova_result <- aov(aggregate_stability ~ Type.x, data = data)
summary(anova_result)

#Perform Tukey's HSD test if ANOVA is significant
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Optionally, plot the Tukey HSD results
plot(tukey_result)

# Create box plot with means
ggplot(data, aes(x = Type.x, y = aggregate_stability)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.7) +
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 3, color = "red") +
  labs(x = "Field Type", y = "Aggregate Stability") +
  ggtitle("Relationship between Field Type and Aggregate Stability") +
  theme_minimal()

#Active Carbon by field type

#Perform ANOVA
anova_result <- aov(active_carbon ~ Type.x, data = data)
summary(anova_result)

#Perform Tukey's HSD test if ANOVA is significant
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Optionally, plot the Tukey HSD results
plot(tukey_result)

# Create box plot with means
ggplot(data, aes(x = Type.x, y = active_carbon)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.7) +
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 3, color = "red") +
  labs(x = "Field Type", y = "Active Carbon") +
  ggtitle("Relationship between Field Type and Active Carbon") +
  theme_minimal()


#POM and MAOM
# Perform linear regression
regression_model <- lm(mgCpergSoilM ~ mgCpergSoilP, data = data)

# Summarize the regression model
summary(regression_model)

# Create a plot with the regression line
ggplot(data, aes(x = mgCpergSoilP, y = mgCpergSoilM)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of MAOM on POM",
       x = "POM",
       y = "MAOM") +
  theme_minimal()

#compost
# Count occurrences of "compost" in the column
count_compost <- sum(grepl("compost", data$Soil.amendments, ignore.case = TRUE))

# Output the count
count_compost

#tillage and active carbon


regression_model <- lm(active_carbon ~ Tillage_1to4, data = data)

#Summarize the regression model
summary(regression_model)

library(ggplot2)
library(dplyr)

# Assuming regression_model is already created

# Create a data frame with predictions for plotting
data_with_predictions <- data %>%
  mutate(predicted = predict(regression_model, newdata = data)) %>%
  group_by(Tillage_1to4) %>%
  summarize(mean_predicted = mean(predicted), 
            mean_active_carbon = mean(active_carbon), 
            .groups = 'drop')

# Plot the data
ggplot(data, aes(x = Tillage_1to4, y = active_carbon)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.5) +  # Scatter plot with jitter to avoid overplotting
  geom_point(data = data_with_predictions, aes(x = Tillage_1to4, y = mean_predicted), color = "red", size = 3, shape = 1) +  # Predicted values
  geom_line(data = data_with_predictions, aes(x = Tillage_1to4, y = mean_predicted, group = 1), color = "blue", size = 1) +  # Regression line
  labs(title = "Regression of Active Carbon on Tillage Category",
       x = "Tillage Category",
       y = "Active Carbon",
       color = "Legend") +
  scale_x_discrete(labels = c("1" = "No Till", "2" = "1-7inch Till", "3" = "7-9inch Till", "4" = ">9inch Till")) +  # Customize x-axis labels
  theme_minimal()

#tillage and POC


regression_model <- lm(mgCpergSoilP ~ Tillage_1to4, data = data)

#Summarize the regression model
summary(regression_model)


library(ggplot2)

# Plot with regression line
ggplot(data, aes(x = as.factor(Tillage_1to4), y = mgCpergSoilP)) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.7) +  # Scatter plot with jitter
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1) +  # Linear regression line
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 3, color = "red") +  # Mean points
  labs(x = "Tillage", y = "mgCpergSoilP") +
  ggtitle("Relationship between Tillage and mgCpergSoilP") +
  theme_minimal()

# Plot with regression line
ggplot(data, aes(x = as.factor(Tillage_Grouped), y = mgCpergSoilP)) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.7) +  # Scatter plot with jitter
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1) +  # Linear regression line
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 3, color = "red") +  # Mean points
  labs(x = "Tillage", y = "mgCpergSoilP") +
  ggtitle("Relationship between Tillage and mgCpergSoilP") +
  theme_minimal()


# Example scatter plot with jitter
library(ggplot2)

ggplot(data, aes(x = as.factor(Tillage_1to4), y = active_carbon)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.7) +
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 3, color = "red") +
  labs(x = "Tillage", y = "PoxC") +
  ggtitle("Relationship between Tillage and PoxC") +
  theme_minimal()

summary(Tillage_1to4)

#tillage and agg stability
# Assuming 'data' is your dataframe
count_by_tillage <- data %>%
  group_by(Tillage_1to4) %>%
  count(Type.x)

# Print the result
print(count_by_tillage)

regression_model <- lm(agg_stability ~ Tillage_1to4, data = data)

#Summarize the regression model
summary(regression_model)

# Example scatter plot with jitter
library(ggplot2)

ggplot(data, aes(x = as.factor(Tillage_1to4), y = active_carbon)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.7) +
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 3, color = "red") +
  labs(x = "Tillage", y = "PoxC") +
  ggtitle("Relationship between Tillage and PoxC") +
  theme_minimal()

# Calculate means and standard errors of mgCpergSoilP for each Tillage Category
means_and_se <- data %>%
  group_by(Tillage_1to4) %>%
  summarise(
    Mean_mgCpergSoilP = mean(mgCpergSoilP, na.rm = TRUE),
    Std_Error = sd(mgCpergSoilP, na.rm = TRUE) / sqrt(n())  # Standard Error
  )

print(means_and_se)
# Calculate means and standard errors of mgCpergSoilM for each Tillage Category
means_and_se <- data %>%
  group_by(Tillage_1to4) %>%
  summarise(
    Mean_mgCpergSoilM = mean(mgCpergSoilM, na.rm = TRUE),
    Std_Error = sd(mgCpergSoilM, na.rm = TRUE) / sqrt(n())  # Standard Error
  )

print(means_and_se)


# Calculate means, standard errors, and sample sizes of mgCpergSoilP for each Tillage Category
means_and_stats <- data %>%
  group_by(Tillage_1to4) %>%
  summarise(
    Mean_mgCpergSoilP = mean(mgCpergSoilP, na.rm = TRUE),
    Std_Error = sd(mgCpergSoilP, na.rm = TRUE) / sqrt(n()),  # Standard Error
    n = n()  # Sample size
  )

print(means_and_stats)

# Perform ANOVA
anova_result <- aov(mgCpergSoilP ~ Tillage_1to4, data = data)

# Summarize the ANOVA result
summary(anova_result)

# Convert Tillage_1to4 to a factor
data$Tillage_1to4 <- as.factor(data$Tillage_1to4)

# Perform ANOVA
anova_result <- aov(mgCpergSoilP ~ Tillage_1to4, data = data)

# Summarize the ANOVA result
summary(anova_result)

# Apply Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)

# Print the Tukey HSD result
print(tukey_result)

# Optionally, convert Tukey result to a data frame for better readability
tukey_df <- as.data.frame(tukey_result$`Tillage_1to4`)
print(tukey_df)


# Convert Tillage_1to4 to a factor
data$Tillage_1to4 <- as.factor(data$Tillage_1to4)

# Perform ANOVA
anova_result <- aov(mgCpergSoilM ~ Tillage_1to4, data = data)

# Summarize the ANOVA result
summary(anova_result)

# Apply Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)

# Print the Tukey HSD result
print(tukey_result)

# Optionally, convert Tukey result to a data frame for better readability
tukey_df <- as.data.frame(tukey_result$`Tillage_1to4`)
print(tukey_df)

view (data)
# Using base R with grepl
search_term <- "manure"
result <- data[grepl(search_term, data$column_name, ignore.case = TRUE), ]

# Using dplyr
library(dplyr)
result <- data %>%
  filter(grepl(search_term, column_name, ignore.case = TRUE))

# Print the result
print(result)
