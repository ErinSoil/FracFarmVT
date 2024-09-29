#Analyzing data and Buildin models for response variable MgMAOM

#setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
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

##call in the analytical data
data <- read.csv("data.csv")
view(data)
summary(data$ph)

#soil health regression

# Perform linear regression
regression_model <- lm(mgCpergSoilM ~ overall.score, data = data)

# Summarize the regression model
summary(regression_model)

# Create a plot with the regression line
ggplot(data, aes(x = overall.score, y = mgCpergSoilM)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of mgCpergSoilM on overall.score",
       x = "Overall Score",
       y = "mgC per g Soil M") +
  theme_minimal()



#pH regression

# Perform linear regression
regression_model6 <- lm(logitpropM ~ ph, data = data)

# Summarize the regression model
summary(regression_model6)

# Create a plot with the regression line
ggplot(data, aes(x = ph, y = logitpropM)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of logitpropMAOM on pH",
       x = "pH",
       y = "logitpropM") +
  theme_minimal()


# Basic histogram
hist(data$ph, main="Histogram of pH", xlab="pH", ylab="Frequency", col="blue", border="black")



# Load the dplyr package
library(dplyr)

# Filter for pasture and count organic matter additions
result <- data %>%
  filter(Type.x == "Corn") %>%
  count(Organic.matter.additions)

# View the result
print(result)



# Perform linear regression try it the other way, to see!
regression_model <- lm(overall.score ~ mgCpergSoilM, data = data)

# Summarize the regression model
summary(regression_model)

# Create a plot with the regression line
ggplot(data, aes(x = mgCpergSoilM, y = overall.score)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of mgCpergSoilM on overall.score",
       x = "mgC per g Soil M",
       y = "Soil Health Score") +
  theme_minimal()







#First, graphically explore relationships 
#to look for interactions between independent variables that research shows could have interactions

summary(data$soil_texture_clay)
data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))
ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilM, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))
ggplot(data=data, aes(x=ppt.cm, y=mgCpergSoilM, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))
ggplot(data=data, aes(x=ph, y=mgCpergSoilM, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))
ggplot(data=data, aes(x=aggregate_stability, y=mgCpergSoilM, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))
ggplot(data=data, aes(x=active_carbon, y=mgCpergSoilM, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

#more interactions with ph
summary(data$ph)

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilM, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=ppt.cm, y=mgCpergSoilM, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=aggregate_stability, y=mgCpergSoilM, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=soil_texture_clay, y=mgCpergSoilM, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=active_carbon, y=mgCpergSoilM, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

#interactions with ppt
summary(data$ppt.cm)
data <- data %>% 
  mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 110.0, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilM, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")


data <- data %>% 
  mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 110.0, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=aggregate_stability, y=mgCpergSoilM, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")

summary(data$ppt.cm)
data <- data %>% 
  mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 110.0, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilM, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")


data <- data %>% 
  mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 110.0, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=active_carbon, y=mgCpergSoilM, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")

#interactions with aggregate stability
summary(data$aggregate_stability)
data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=ph, y=mgCpergSoilM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=ppt.cm, y=mgCpergSoilM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=soil_texture_clay, y=mgCpergSoilM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=active_carbon, y=mgCpergSoilM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

#tmeanC and active carbon
summary(data$tmeanC)

data <- data %>% 
  mutate(tmeanCcategory=cut(tmeanC, breaks=c(-Inf,6.8, 7.6, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=active_carbon, y=mgCpergSoilM, col=tmeanC))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ tmeanCcategory, ncol=1, scales="free_x")

#Models
#Correlation plot
library(corrplot)
cordata <- cor(data[,c("mgCpergSoilM","ph","ppt.cm","tmeanC","aggregate_stability","soil_texture_clay","active_carbon")], use="pairwise.complete.obs", method="pearson")
corrplot(cordata)
view(cordata)

##Linear Mixed Model for dependent variable (mgCpergSoilM)
#test without random effect, because only one value per field

m1 = gls(mgCpergSoilM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
           aggregate_stability * soil_texture_clay + 
           active_carbon + ph * soil_texture_clay, 
         data = data, 
         na.action = na.exclude, 
         method = "ML")
summary(m1)
anova(m1)

#check if variance structure improves the model (to test, both methods were set to REML)

m2 = gls(mgCpergSoilM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
           active_carbon + ph * soil_texture_clay + aggregate_stability, 
         data = data, 
         na.action = na.exclude, 
         method = "ML")
summary(m2)
anova(m2)


anova (m1,m2)

m3 = gls(mgCpergSoilM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
           active_carbon + aggregate_stability,
         data = data, 
         na.action = na.exclude, 
         method = "ML")
summary(m3)
anova (m2,m3)
anova(m3)

n <- nobs(m3)
print(n)

#pseudo R squared calculation (fit between model predicted data and actual data)
data$mgCpergSoilM.pred=as.vector(fitted(m3))
R2=lm(mgCpergSoilM~mgCpergSoilM.pred, data=data, na.action=na.omit)
summary(R2) #r2=0.36

#check assumptions, distribution of residuals

F_Final <- fitted(m3)
R_Final <- residuals(m3, type = "pearson", scaled = TRUE)
N = !is.na(data$mgCpergSoilM)
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

ppt.cm.c <- summary(m3)$coefficients[2] #predictor coefficient
ppt.cm.pr <- Rfull + ppt.cm.c*data$ppt.cm  #Residuals + pred coef * predictor value
{scatter.smooth(data$ppt.cm, ppt.cm.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(ppt.cm.c*data$ppt.cm ~ data$ppt.cm), col = "red")} 

soil_texture_clay.c <- summary(m3)$coefficients[3] #predictor coefficient
soil_texture_clay.pr <- Rfull + soil_texture_clay.c*data$soil_texture_clay  #Residuals + pred coef * predictor value
{scatter.smooth(data$soil_texture_clay, soil_texture_clay.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(soil_texture_clay.c*data$soil_texture_clay ~ data$soil_texture_clay), col = "red")} 

aggregate_stability.c <- summary(m3)$coefficients[5] #predictor coefficient
aggregate_stability.pr <- Rfull + aggregate_stability.c*data$aggregate_stability  #Residuals + pred coef * predictor value
{scatter.smooth(data$aggregate_stability, aggregate_stability.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(aggregate_stability.c*data$aggregate_stability ~ data$aggregate_stability), col = "red")} 

active_carbon.c <- summary(m3)$coefficients[6] #predictor coefficient
active_carbon.pr <- Rfull + active_carbon.c*data$active_carbon  #Residuals + pred coef * predictor value
{scatter.smooth(data$active_carbon, active_carbon.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(active_carbon.c*data$active_carbon ~ data$active_carbon), col = "red")} 

tmeanC.c <- summary(m3)$coefficients[6] #predictor coefficient
tmeanC.pr <- Rfull + tmeanC.c*data$tmeanC  #Residuals + pred coef * predictor value
{scatter.smooth(data$tmeanC, tmeanC.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(tmeanC.c*data$tmeanC ~ data$tmeanC), col = "red")} 

par(op)

#Visualize significant relationships

#own_theme below sets ggplot parameters for how plots should look. 
own_theme <- theme_bw(base_size = 11) +
  theme(rect = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank())

#for aggregate stability
pred_aggregate_stability <- ggpredict(m3, terms = c("aggregate_stability"))

mgMAOM_aggregate_stability <-data %>% 
  ggplot() +
  geom_point(aes(x = aggregate_stability, y = mgCpergSoilM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_aggregate_stability, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg MAOC g"^-1*"soil"))+
  scale_x_continuous(expression("Aggregate Stability (%)"),
              label = scales::comma) 
mgMAOM_aggregate_stability
ggsave("mgMAOM_aggregate_stability.jpeg", width = 15, height = 8, units="cm")
  
# Define the coefficient for aggregate stability
coef_aggregate_stability <-  0.10780  # in mg find in model summary
# Define the mean value of MAOC 
# Print the mean MAOC value
mean_maoc <- mean(data$mgCpergSoilM, na.rm = TRUE)
print(paste("The mean MAOC is", round(mean_maoc, 2), "mg"))

mean_MAOC <- 16.77  # Replace with the actual mean value from data
percentage_increase <- 10  #set a percentage increase of agg stability based on data

# Calculate the absolute increase in MAOC for the given percentage increase in aggregate stability
absolute_increase_maoc <- coef_aggregate_stability * percentage_increase
# Calculate the percent increase in POC for a 1% increase in aggregate stability
Onepercent_increase_maoc <- (coef_aggregate_stability / mean_MAOC) * 100
# Calculate the percent increase in POC based on the mean value
percent_increase_maoc <- (absolute_increase_maoc / mean_MAOC) * 100

# Print the result
print(paste("Absolute increase in MAOC for a 10% increase in aggregate stability is", round(absolute_increase_maoc, 2), "mg"))
# Print the result
print(paste("Percent increase in MAOC for a 1% increase in aggregate stability is", round(Onepercent_increase_maoc, 2), "%"))
# Print the result
print(paste("Percent increase in MAOC for a", percentage_increase, "% increase in aggregate stability is", round(percent_increase_maoc, 2), "%"))


#for active_carbon
pred_active_carbon <- ggpredict(m3, terms = c("active_carbon"))

mgMAOM_active_carbon <-data %>% 
          ggplot() +
          geom_point(aes(x = active_carbon, y = mgCpergSoilM), #plot your data
                                size = 1.5, alpha = 0.5) +
          geom_line(pred_active_carbon, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
                               lwd = 1) +
          own_theme+
        theme(legend.position = "none") +
                     scale_y_continuous(expression("mg MAOC g"^-1*"soil"))+
                     scale_x_continuous(expression("Active Carbon (ppm)"),
                                        label = scales::comma) 
                   mgMAOM_active_carbon
ggsave("mgMAOM_active_carbon.jpeg", width = 15, height = 8, units="cm")
 
#get them side by side for presentation
#Figure5
ggarrange(mgMAOM_aggregate_stability,mgMAOM_active_carbon,nrow=1, common.legend=T, legend="left", labels=c("a","b"))
ggsave("Figure5.jpeg", width = 15, height = 8, units="cm")

# Assuming 'data' is your data frame and 'mgCpergSoilM' is the column name for POC
mean_maoc <- mean(data$mgCpergSoilM, na.rm = TRUE)

print(paste("The mean POC is", round(mean_maoc, 2), "mg"))

# Define the coefficient for active carbon
coef_activecarbon <-   0.01955  # in ppm
# Define the mean value of MAOC 
mean_MAOC <- 16.77  # Replace with the actual mean value from your data
# Define the ppm increase in active carbon
ppm_increase <- 100  # For a 100 ppm increase in active carbon
# Calculate the absolute increase in MAOC for the given ppm increase in active carbon
absolute_increase_maoc <- coef_activecarbon * ppm_increase
# Calculate the percent increase in MAOC based on the mean value
percent_increase_maoc <- (absolute_increase_maoc / mean_MAOC) * 100

# Print the result
print(paste("Percent increase in MAOC for a", ppm_increase, "ppm in active carbon is", round(percent_increase_maoc, 2), "%"))
# Print the result
print(paste("Absolute increase in MAOC for a 100ppm increase in active carbon is", round(absolute_increase_maoc, 2), "mg"))
# Print the result
print(paste("Percent increase in MAOC for a 1% increase in actice carbon is", round(Onepercent_increase_maoc, 2), "%"))



                  
#for 3 way interaction of tmean, ppt, and texture Figure 6 Final

summary(data$soil_texture_clay)
summary(data$ppt.cm)
pred_3way <- ggpredict(m3, terms = c("tmeanC","ppt.cm[101,110]", 
  "soil_texture_clay[10, 32]"))

pred_3way$ppt_group <- pred_3way$group
levels(pred_3way$ppt_group) <- c("low (92-104)",  
                                   "high (104-142)")
pred_3way$clay_facet <- pred_3way$facet
levels(pred_3way$clay_facet) <- c("low clay (6-19%)",  
                                   "high clay (19-54%)")

data <- data %>%
  drop_na(ppt.cm) %>% 
  drop_na(soil_texture_clay) %>% 
  dplyr::mutate(ppt_group = cut(ppt.cm, breaks = c(92,104,142)),
                clay_facet = cut(soil_texture_clay, breaks = c(6,19,54)))

levels(data$ppt_group) <- c("low (92-104)",  
                            "high (104-142)")
 
levels(data$clay_facet) <- c("low clay (6-19%)",  
                                   "high clay (19-54%)")
mgMAOM_3way <-data %>% 
   ggplot() +
   geom_point(aes(x = tmeanC, y = mgCpergSoilM, col = ppt_group), #plot your data
              size = 1.5, alpha = 0.5) +
   geom_line(pred_3way, mapping = aes(x=x, y=predicted, col = ppt_group), #plot the model's prediction (based on linear )
             lwd = 1) +
   facet_wrap(~clay_facet) +
   own_theme+
   #theme(legend.position = "none") +
   scale_y_continuous(expression(paste("mg MAOC g"^-1,"soil"))) +
   scale_x_continuous(expression("Mean Annual Temperature (°C)"),
                      label = scales::comma) +
   guides(col=guide_legend(title="Mean Annual Precipitation (cm)")) +
   scale_color_manual(values = c("red", "blue")) 
 
mgMAOM_3way
ggsave("mgMAOM_3way.jpeg", width = 8, height = 4)
 #share legend and a, b, ggarrange
#get slope for the lines in the 3 way 
mylist<- list(ppt.cm=c(101,110),soil_texture_clay=c(10,32))
  
emtrends(m3,pairwise~ppt.cm*soil_texture_clay,var="tmeanC", at=mylist)                
                   
   #analyze data by field type. group by and color by field type this code is in progress
    #exploration
                   mgMAOM_active_carbonbyField <-data
                     ggplot() +
                     geom_point(aes(x = active_carbon, y = mgCpergSoilM, color=Type.x), 
                                size = 1.5, alpha = 0.5) +
                     #geom_smooth() +
                     own_theme+
                     #theme(legend.position = "none") +
                     scale_y_continuous(expression("mg C in MAOM per g soil"))+
                     scale_x_continuous(expression("active_carbon"),
                                        label = scales::comma) 
                   mgMAOM_active_carbonbyField
                   ggsave("mgMAOM_active_carbonbyField.jpeg", width = 4, height = 3)
                   
                   mgMAOM_agg_stability_byField <-data %>% 
                     ggplot() +
                     geom_point(aes(x = aggregate_stability, y = mgCpergSoilM, color=Type.x), 
                                size = 1.5, alpha = 0.5) +
                     #geom_smooth() +
                     own_theme+
                     #theme(legend.position = "none") +
                     scale_y_continuous(expression("mg C in MAOM per g soil"))+
                     scale_x_continuous(expression("aggregrate stability"),
                                        label = scales::comma) 
                   mgMAOM_agg_stability_byField
                   ggsave("mgMAOM_aggregrate_stability_byField.jpeg", width = 4, height = 3)
                   
                   
                   
#anova by field type to see differences 
      field_anova<- aov(mgCpergSoilM~Type.x, data=data)
       summary(field_anova)  
       
  TukeyHSD(field_anova)
            
# Create a violin plot with individual data points
       ggplot(data, aes(x = Type.x, y = mgCpergSoilM)) +
       geom_violin(trim = FALSE, fill = "lightblue") +  # Create the violin plot
       geom_jitter(width = 0.2, size = 1, color = "darkblue") +  # Add jittered points
       labs(title = "Distribution of mgCpergSoilM by Field Type",
                          x = "Field Type",
                          y = "mgC per g Soil M") +
       theme_minimal()  # Apply a minimal theme for a clean look              
      
       
       # Create a violin plot with individual data points and mean line
       ggplot(data, aes(x = Type.x, y = mgCpergSoilM, color = Type.x, fill = Type.x)) +
         geom_violin(trim = FALSE, alpha = 0.5) +  # Create the violin plot with semi-transparent fill
         geom_jitter(width = 0.2, size = 1) +  # Add jittered points
         stat_summary(fun = mean, geom = "point", shape = 23, size = 2, color = "black", fill = "yellow") +  # Add mean points
         labs(title = "Distribution of mgCpergSoilM by Field Type",
              x = "Field Type",
              y = "mgC per g Soil M") +
         theme_minimal()  # Apply a minimal theme for a clean look
       
       
       # Create a violin plot with individual data points and mean line for soil texture class
       ggplot(data, aes(x = soil_texture_class, y = mgCpergSoilM, color = soil_texture_class, fill = soil_texture_class)) +
         geom_violin(trim = FALSE, alpha = 0.5) +  # Create the violin plot with semi-transparent fill
         geom_jitter(width = 0.2, size = 1) +  # Add jittered points
         stat_summary(fun = mean, geom = "point", shape = 23, size = 2, color = "black", fill = "yellow") +  # Add mean points
         labs(title = "Distribution of mgCpergSoilM by Soil Texture Class",
              x = "Soil Texture Class",
              y = "mgC per g Soil MAOM") +
         theme_minimal() +  # Apply a minimal theme for a clean look
         theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Adjust text angle, justification, and size
               plot.margin = margin(5, 5, 10, 5))  # Increase the bottom margin to give more space to labels
       
       
       # Load the necessary library
       library(dplyr)
       
       # Assuming 'data' is a data frame already loaded into your environment
       # Replace this with your actual data loading method, e.g., read.csv or read_excel
       # data <- read.csv("your_data_file.csv")
       
       # Grouping the data by 'Type.x' and calculating the required statistics
       summary <- data %>%
         group_by(Type.x) %>%
         summarise(
           n = n(),
           MgMAOM_max = max(mgCpergSoilM, na.rm = TRUE),
           MgMAOM_min = min(mgCpergSoilM, na.rm = TRUE),
           MgMAOM_mean = mean(mgCpergSoilM, na.rm = TRUE),
           MgMAOM_sd = sd(mgCpergSoilM, na.rm = TRUE),
           MgPOM_max = max(mgCpergSoilP, na.rm = TRUE),
           MgPOM_min = min(mgCpergSoilP, na.rm = TRUE),
           MgPOM_mean = mean(mgCpergSoilP, na.rm = TRUE),
           MgPOM_sd = sd(mgCpergSoilP, na.rm = TRUE),
           pMAOM_max = max(propM, na.rm = TRUE),
           pMAOM_min = min(propM, na.rm = TRUE),
           pMAOM_mean = mean(propM, na.rm = TRUE),
           pMAOM_sd = sd(propM, na.rm = TRUE)
         )
       
       # Print the summary table
       print(summary)
       
       library(dplyr)
       
       # Grouping the data by 'Type.x' and calculating the required statistics
       summary <- data %>%
         group_by(Type.x) %>%
         summarise(
           n = n(),
           MgMAOM_summary = paste0(
             round(mean(mgCpergSoilM, na.rm = TRUE), 2), " ± ", 
             round(sd(mgCpergSoilM, na.rm = TRUE), 2), " (", 
             round(min(mgCpergSoilM, na.rm = TRUE), 2), " - ", 
             round(max(mgCpergSoilM, na.rm = TRUE), 2), ")"
           ),
           MgPOM_summary = paste0(
             round(mean(mgCpergSoilP, na.rm = TRUE), 2), " ± ", 
             round(sd(mgCpergSoilP, na.rm = TRUE), 2), " (", 
             round(min(mgCpergSoilP, na.rm = TRUE), 2), " - ", 
             round(max(mgCpergSoilP, na.rm = TRUE), 2), ")"
           ),
           pMAOM_summary = paste0(
             round(mean(propM, na.rm = TRUE), 2), " ± ", 
             round(sd(propM, na.rm = TRUE), 2), " (", 
             round(min(propM, na.rm = TRUE), 2), " - ", 
             round(max(propM, na.rm = TRUE), 2), ")"
           )
         )
       
       # Print the summary table
       print(summary)
       