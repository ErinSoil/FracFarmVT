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

##call in the analytical data
data <- read.csv("data.csv")
View(data)

#soil health regression

# Perform linear regression
regression_model <- lm(mgCpergSoilP ~ overall.score, data = data)

# Summarize the regression model
summary(regression_model)

# Create a plot with the regression line
ggplot(data, aes(x = overall.score, y = mgCpergSoilP)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of mgCpergSoilP on overall.score",
       x = "Overall Score",
       y = "mgC per g Soil P") +
  theme_minimal()

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
           +aggregate_stability+active_carbon 
        , data=data, na.action=na.exclude, method="ML")
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
        +aggregate_stability+active_carbon,
        data=data, na.action=na.exclude, method="REML") 
summary(m3P)

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


#ppt and tmeanC, option 2
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
   scale_y_continuous(expression("mg C in POM per g soil")) +
   scale_x_continuous(expression("Mean Annual Temperature(C)"),
                      label = scales::comma) +
   scale_color_manual(values = c("blue", "red")) # adjust colors if needed
 
 mgPOM_tmeanppt

#for agg stability 
pred_aggregate_stability <- ggpredict(m3P, terms = c("aggregate_stability"))
mgPOM_aggregate_stability <-data %>% 
  ggplot() +
  geom_point(aes(x = aggregate_stability, y = mgCpergSoilP), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_aggregate_stability, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in POM per g soil"))+
  scale_x_continuous(expression("aggregate stability"),
                     label = scales::comma) 

mgPOM_aggregate_stability
ggsave("mgPOM_aggregate_stability.jpeg", width = 4, height = 3)

#Active Carbon
pred_active_carbon <- ggpredict(m3P, terms = c("active_carbon"))
mgPOM_active_carbon <-data %>% 
  ggplot() +
  geom_point(aes(x = active_carbon, y = mgCpergSoilP), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_active_carbon, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in POM per g soil"))+
  scale_x_continuous(expression("active carbon"),
                     label = scales::comma) 

mgPOM_active_carbon

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


#anova by field type to see differences 
field_anova<- aov(mgCpergSoilP~Type.x, data=data)
summary(field_anova)  

TukeyHSD(field_anova)

#anova by soil texture class to see differences 
texture_anova<- aov(mgCpergSoilP~soil_texture_class, data=data)
summary(texture_anova)  

TukeyHSD(texture_anova)
