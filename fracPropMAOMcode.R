#Creating Linear Models and Graphing Relationships with Proporation of MAOM

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
#Frac <- read.csv(file="Frac.csv", header=TRUE, sep=",")
#Loc <- read.csv(file="Location.csv", header=TRUE, sep=",")
#Mang <- read.csv(file="Mang.csv", header=TRUE, sep=",") 
#Master <- read.csv(file="Master.csv", header=TRUE, sep=",", fileEncoding="latin1")
#PRISM2annual <- read.csv(file="PRISM2annual.csv", header=TRUE, sep=",")

data <- read.csv(file="fracData.csv", header=TRUE, sep=",")

# join tables
#fracData<-Frac %>%
#  left_join(.,Mang,by="Field_Code")
# fracData<-fracData %>%
#     left_join(.,Master,by=c("Field_Code", "soil_texture_class")) 
# fracData<-fracData %>%
#   left_join(.,PRISM2annual,by="Field_Code")
# names(fracData)

#linear model
#First, graphically explore relationships among variables
#look for interactions between independent variables

#interactions with clay
View (data)

ggplot(data=data, aes(x=tmeanC, y=propM, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

ggplot(data=data, aes(x=ppt.cm, y=propM, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

#interactions with ph
data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6.17, 6.52, 6.93, Inf), labels=c("low","med","high", "veryhigh")))

ggplot(data=data, aes(x=tmeanC, y=propM, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")


ggplot(data=data, aes(x=ppt.cm, y=propM, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

#interactions with ppt
data <- data %>% 
  mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 106.4, 110.0, Inf), labels=c("low","med","high", "veryhigh")))

ggplot(data=data, aes(x=tmeanC, y=propM, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")


ggplot(data=data, aes(x=aggregate_stability, y=propM, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")


ggplot(data=data, aes(x=ph, y=propM, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")


ggplot(data=data, aes(x=active_carbon, y=propM, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")

#interactions with aggregate stability
summary(data$aggregate_stability)

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.8, 46.7, 63.7, Inf), labels=c("low","med","high", "veryhigh")))

ggplot(data=data, aes(x=ph, y=propM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")


ggplot(data=data, aes(x=ppt.cm, y=propM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

ggplot(data=data, aes(x=tmeanC, y=propM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")


ggplot(data=data, aes(x=soil_texture_clay, y=propM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

ggplot(data=data, aes(x=active_carbon, y=propM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")


#Correlation plot
cordata <- cor(data[,c("propM","ph","ppt.cm","tmeanC","aggregate_stability","soil_texture_clay","active_carbon")], use="pairwise.complete.obs", method="pearson")
corrplot(cordata)
view(cordata)

##Linear Mixed Model for dependent variable (propM)
#test without random effect, because only one value per field

m1M=gls(propM~ppt.cm+soil_texture_clay+
         tmeanC+aggregate_stability+active_carbon+ 
         ph, data=data, na.action=na.exclude, method="ML")
summary(m1M)
anova(m1M)

#new mb test, minus ph
mbM=gls(propM~ppt.cm+soil_texture_clay+
          tmeanC+aggregate_stability+active_carbon, data=data, na.action=na.exclude, method="ML")
summary(mbM)
anova(mbM)
#minus clay
mcM=gls(propM~ppt.cm+ph+
          tmeanC+aggregate_stability+active_carbon, data=data, na.action=na.exclude, method="ML")
summary(mcM)
anova(mcM)
#minus ppt
mdM=gls(propM~ph+soil_texture_clay+
          tmeanC+aggregate_stability+active_carbon, data=data, na.action=na.exclude, method="ML")
summary(mdM)
anova(mdM)

#minus agg stability
meM=gls(propM~ppt.cm+soil_texture_clay+
          tmeanC+ph+active_carbon, data=data, na.action=na.exclude, method="ML")
summary(meM)
anova(meM)

#minus active carbon
meM=gls(propM~ppt.cm+soil_texture_clay+
          tmeanC+ph+aggregate_stability, data=data, na.action=na.exclude, method="ML")
summary(meM)
anova(meM)

#minus temp
mfM=gls(propM~ppt.cm+soil_texture_clay+
          aggregate_stability+active_carbon+ 
          ph, data=data, na.action=na.exclude, method="ML")
summary(mfM)
anova(mfM)

#anova for different field types 

mType=aov(aggregate_stability~Type.x, data=data, na.action=na.exclude) 
summary(mType)
TukeyHSD(mType)


mType=aov(propM~Type.x, data=data, na.action=na.exclude) 
summary(mType)
TukeyHSD(mType)


#check if variance structure improves the model, (to test, both methods were set to REML)
m1aM=gls(propM~ppt.cm+soil_texture_clay+
          tmeanC+aggregate_stability+active_carbon+ 
          ph, data=data, na.action=na.exclude, weights = varFixed(~propM), method="ML")
anova(m1M, m1aM) #adding variance structure does not improve the model

m2M=gls(propM~ppt.cm+soil_texture_clay+
         tmeanC+aggregate_stability+active_carbon+ 
         ph, data=data, na.action=na.exclude, method="ML")

anova(m1M,m2M)
anova(m2M)

m3M=gls(propM~ppt.cm+soil_texture_clay+
         tmeanC+aggregate_stability+active_carbon,
       data=data, na.action=na.exclude, method="ML")

anova(m2M,m3M)
anova(m3M)

m4M=gls(propM~ppt.cm*soil_texture_clay+
         tmeanC+aggregate_stability*
         active_carbon,
       data=data, na.action=na.exclude, method="ML")
anova(m3M,m4M)
anova(m4M)

m5M=gls(propM~ppt.cm+soil_texture_clay+
         aggregate_stability+
         active_carbon,
       data=data, na.action=na.exclude, method="ML")

anova(m4M,m5M)
anova(m5M)

#above is if you want to drop non significant predictors until all are significant as model selection, but there is really no need. 
#I would stick with model 1

#check assumptions, distrubution of residuals


F_Final <- fitted(m1M)
R_Final <- residuals(m1M, type = "pearson", scaled = TRUE)
N = !is.na(data$propM)
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

ppt.cm.c <- summary(m1M)$coefficients[2] #predictor coefficient
ppt.cm.pr <- Rfull + ppt.cm.c*data$ppt.cm  #Residuals + pred coef * predictor value
{scatter.smooth(data$ppt.cm, ppt.cm.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(ppt.cm.c*data$ppt.cm ~ data$ppt.cm), col = "red")} 

soil_texture_clay.c <- summary(m1M)$coefficients[3] #predictor coefficient
soil_texture_clay.pr <- Rfull + soil_texture_clay.c*data$soil_texture_clay  #Residuals + pred coef * predictor value
{scatter.smooth(data$soil_texture_clay, soil_texture_clay.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(soil_texture_clay.c*data$soil_texture_clay ~ data$soil_texture_clay), col = "red")} 

aggregate_stability.c <- summary(m1M)$coefficients[5] #predictor coefficient
aggregate_stability.pr <- Rfull + aggregate_stability.c*data$aggregate_stability  #Residuals + pred coef * predictor value
{scatter.smooth(data$aggregate_stability, aggregate_stability.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(aggregate_stability.c*data$aggregate_stability ~ data$aggregate_stability), col = "red")} 

active_carbon.c <- summary(m1M)$coefficients[6] #predictor coefficient
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

#for aggregate stability
pred_aggregate_stability <- ggpredict(m1M, terms = c("aggregate_stability"))
propMAOM_aggregate_stability <-data %>% 
  ggplot() +
  geom_point(aes(x = aggregate_stability, y = propM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_aggregate_stability, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("prop C in MAOM"))+
  scale_x_continuous(expression("aggregate_stability"),
                     label = scales::comma) 
propMAOM_aggregate_stability
ggsave("propMAOM_aggregate_stability.jpeg", width = 4, height = 3)


#ppt
pred_ppt <- ggpredict(m1M, terms = c("ppt.cm"))
propMAOM_ppt <-data %>% 
  ggplot() +
  geom_point(aes(x = ppt.cm, y = propM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_ppt, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("prop C in MAOM"))+
  scale_x_continuous(expression("Mean Annual Precipitation (cm)"),
                     label = scales::comma) 
propMAOM_ppt
ggsave("propMAOM_ppt.jpeg", width = 4, height = 3)


#for clay # not sig different from zero, a little negative
pred_soil_texture_clay <- ggpredict(m1M, terms = c("soil_texture_clay"))
propMAOM_soil_texture_clay <-data %>% 
  ggplot() +
  geom_point(aes(x = soil_texture_clay, y = propM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_soil_texture_clay, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("prop C in MAOM"))+
  scale_x_continuous(expression("clay"),
                     label = scales::comma) 
propMAOM_soil_texture_clay
ggsave("propMAOM_soil_texture_clay.jpeg", width = 4, height = 3)


#for active_carbon
pred_active_carbon <- ggpredict(m1M, terms = c("active_carbon"))
propMAOM_active_carbon <-data %>% 
  ggplot() +
  geom_point(aes(x = active_carbon, y = propM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_active_carbon, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("prop C in MAOM"))+
  scale_x_continuous(expression("active_carbon"),
                     label = scales::comma) 
propMAOM_active_carbon
ggsave("propMAOM_active_carbon.jpeg", width = 4, height = 3)


#for tmeanC
pred_tmeanC <- ggpredict(m1M, terms = c("tmeanC"))
propMAOM_tmeanC <-data %>% 
  ggplot() +
  geom_point(aes(x = tmeanC, y = propM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_tmeanC, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("prop C in MAOM"))+
  scale_x_continuous(expression("active_carbon"),
                     label = scales::comma) 
propMAOM_tmeanC
ggsave("propMAOM_active_carbon.jpeg", width = 4, height = 3)


#analyze data by field type. group by and color by field type this code is in progress
#exploration
propMAOM_active_carbonbyField <-data %>% 
  ggplot() +
  geom_point(aes(x = active_carbon, y = propM, color=Type.x), 
             size = 1.5, alpha = 0.5) +
  #geom_smooth() +
  own_theme+
  #theme(legend.position = "none") +
  scale_y_continuous(expression("prop C in MAOM"))+
  scale_x_continuous(expression("active_carbon"),
                     label = scales::comma) 
propMAOM_active_carbonbyField
ggsave("propMAOM_active_carbonbyField.jpeg", width = 4, height = 3)

#box plot

propMAOMbyFieldType <- ggplot(data, aes(x=Type.x, y=propM)) + 
  geom_boxplot()
propMAOMbyFieldType

