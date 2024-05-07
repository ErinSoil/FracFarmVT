##run linear models using POM as the Response Variable

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

data <- read.csv(file="fracData.csv", header=TRUE, sep=",")

#First, graphically explore relationships among variables
#look for interactions between independent variables

View (data)
data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))

ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilP, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

ggplot(data=data, aes(x=ppt.cm, y=mgCpergSoilP, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

#more interactions with ph
data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6.17, 6.52, 6.93, Inf), labels=c("low","med","high", "veryhigh")))

ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilP, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

ggplot(data=data, aes(x=ppt.cm, y=mgCpergSoilP, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

#interactions with ppt
data <- data %>% 
  mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 106.4, 110.0, Inf), labels=c("low","med","high", "veryhigh")))

ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilP, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")

ggplot(data=data, aes(x=aggregate_stability, y=mgCpergSoilP, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")

ggplot(data=data, aes(x=ph, y=mgCpergSoilP, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")

ggplot(data=data, aes(x=active_carbon, y=mgCpergSoilP, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")

#interactions with aggregate stability
summary(data$aggregate_stability)

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.8, 46.7, 63.7, Inf), labels=c("low","med","high", "veryhigh")))

ggplot(data=data, aes(x=ph, y=mgCpergSoilP, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

ggplot(data=data, aes(x=ppt.cm, y=mgCpergSoilP, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilP, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

ggplot(data=data, aes(x=soil_texture_clay, y=mgCpergSoilP, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")


ggplot(data=data, aes(x=active_carbon, y=mgCpergSoilP, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

#Models
#Correlation plot
cordata <- cor(data[,c("mgCpergSoilP","ph","ppt.cm","tmeanC","aggregate_stability","soil_texture_clay","active_carbon")], use="pairwise.complete.obs", method="pearson")
corrplot(cordata)

##Linear Mixed Model for dependent variable (mgCpergSoilP)

#test without random effect, because only one value per field

m1P=gls(mgCpergSoilP~soil_texture_clay*ppt.cm+soil_texture_clay*tmeanC+ppt.cm*tmeanC+aggregate_stability*active_carbon+ 
         ph, data=data, na.action=na.exclude, method="ML")
summary(m1P)
anova(m1P)

#Erin needs better understanding of this. Do we change REML to ML?  #check if variance structure improves the model, it does not (to test, both methods were set to REML) ##this doesn't work now
m1a=gls(mgCpergSoilP~ppt.cm*soil_texture_clay+
          soil_texture_clay*tmeanC+ppt.cm*tmeanC+aggregate_stability+active_carbon+ 
          ph, data=data, na.action=na.exclude, weights = varFixed(~mgCpergSoilP), method="REML")
anova(m1P, m1a) #adding variance structure does not improve the model

m2=gls(mgCpergSoilP~ppt.cm*soil_texture_clay+
         soil_texture_clay*tmeanC+ppt.cm*tmeanC+aggregate_stability+active_carbon,
       data=data, na.action=na.exclude, method="ML")

anova(m1P,m2)
anova(m2)

m3=gls(mgCpergSoilP~ppt.cm+
         soil_texture_clay*tmeanC+ppt.cm*tmeanC+aggregate_stability+
         active_carbon,
       data=data, na.action=na.exclude, method="ML")

anova(m2,m3)
anova(m3)

m4=gls(mgCpergSoilP~ppt.cm+soil_texture_clay+
         tmeanC+aggregate_stability+
         active_carbon,
       data=data, na.action=na.exclude, method="ML")

anova(m3,m4)
anova(m4)

m5=gls(mgCpergSoilP~ppt.cm+soil_texture_clay+
         aggregate_stability+
         active_carbon,
       data=data, na.action=na.exclude, method="ML")

anova(m4,m5)
anova(m5)

#above is if you want to drop non significant predictors until all are significant as model selection, but there is really no need. 
#How to I assess these different model results??

#check assumptions, distrubution of residuals


m1P=gls(mgCpergSoilP~soil_texture_clay*ppt.cm+soil_texture_clay*tmeanC+ppt.cm*tmeanC+aggregate_stability*active_carbon+ 
         ph, data=data, na.action=na.exclude, method="ML")
summary(m1P)
anova(m1P)


F_Final <- fitted(m1P)
R_Final <- residuals(m1P, type = "pearson", scaled = TRUE)
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

ppt.cm.c <- summary(m1P)$coefficients[2] #predictor coefficient
ppt.cm.pr <- Rfull + ppt.cm.c*data$ppt.cm  #Residuals + pred coef * predictor value
{scatter.smooth(data$ppt.cm, ppt.cm.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(ppt.cm.c*data$ppt.cm ~ data$ppt.cm), col = "red")} 

soil_texture_clay.c <- summary(m1P)$coefficients[3] #predictor coefficient
soil_texture_clay.pr <- Rfull + soil_texture_clay.c*data$soil_texture_clay  #Residuals + pred coef * predictor value
{scatter.smooth(data$soil_texture_clay, soil_texture_clay.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(soil_texture_clay.c*data$soil_texture_clay ~ data$soil_texture_clay), col = "red")} 

aggregate_stability.c <- summary(m1P)$coefficients[5] #predictor coefficient
aggregate_stability.pr <- Rfull + aggregate_stability.c*data$aggregate_stability  #Residuals + pred coef * predictor value
{scatter.smooth(data$aggregate_stability, aggregate_stability.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(aggregate_stability.c*data$aggregate_stability ~ data$aggregate_stability), col = "red")} 

active_carbon.c <- summary(m1P)$coefficients[6] #predictor coefficient
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

#ppt and tmeanC
pred_ppt <- ggpredict(m1P, terms = c("ppt.cm"))
mgPOM_ppt <-data %>% 
  ggplot() +
  geom_point(aes(x = ppt.cm, y = mgCpergSoilP), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_ppt, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in POM per g soil"))+
  scale_x_continuous(expression("Mean Annual Precipitation (cm)"),
                     label = scales::comma) 
mgPOM_ppt
ggsave("mgPOM_ppt.jpeg", width = 4, height = 3)

#tmeanC

pred_tmeanC <- ggpredict(m1P, terms = c("tmeanC"))

POM_tmeanC <-data %>% 
  ggplot() +
  geom_point(aes(x = tmeanC, y = mgCpergSoilP), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_tmeanC, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in POM per g soil"))+
  scale_x_continuous(expression("Mean Annual Temperature (C)"),
                     label = scales::comma) 

POM_tmeanC

ggsave("POM_tmeanC.jpeg", width = 4, height = 3)


#ppt and tmeanC  #is this right?
summary(data$ppt.cm)
summary(data$tmeanC)
pred_pptC <- ggpredict(m1P, terms = c("ppt.cm", "tmeanC[5,8]"))
pred_tmeanC <- ggpredict(m1P, terms = c("tmeanC","ppt.cm[95,115]"))

mgPOM_pptC <-data %>% 
  ggplot() +
  geom_point(aes(x = ppt.cm, y = mgCpergSoilP), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_pptC, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in POM per g soil"))+
  scale_x_continuous(expression("Mean Annual Precipitation (cm) and Temp (C)"),
                     label = scales::comma) 

mgPOM_pptC

ggsave("mgPOM_ppt_tmeanC.jpeg", width = 4, height = 3)


#for agg stability 
pred_aggregate_stability <- ggpredict(m1P, terms = c("aggregate_stability"))

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


#for active carbon 
pred_active_carbon <- ggpredict(m1P, terms = c("active_carbon"))

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

ggsave("mgPOM_active_carbon.jpeg", width = 4, height = 3)


#for clay 
pred_soil_texture_clay <- ggpredict(m1P, terms = c("soil_texture_clay"))

mgPOM_soil_texture_clay <-data %>% 
  ggplot() +
  geom_point(aes(x = soil_texture_clay, y = mgCpergSoilP), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_soil_texture_clay, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in POM per g soil"))+
  scale_x_continuous(expression("clay"),
                     label = scales::comma) 

mgPOM_soil_texture_clay

ggsave("mgPOM_soil_texture_clay.jpeg", width = 4, height = 3)


#for ph
pred_ph <- ggpredict(m1P, terms = c("ph"))
mgPOM_ph <-data %>% 
  ggplot() +
  geom_point(aes(x = ph, y = mgCpergSoilP), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_ph, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in POM per g soil"))+
  scale_x_continuous(expression("ph"),
                     label = scales::comma) 
mgPOM_ph
ggsave("mgPOM_ph.jpeg", width = 4, height = 3)

#analyze by field type #this is code still in progress
pred_active_carbon <- ggpredict(m1P, terms = c("active_carbon"))
mgMAOM_active_carbon <-data %>% 
  ggplot() +
  geom_point(aes(x = active_carbon, y = mgCpergSoilM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_active_carbon, mapping = aes(x=x, y=predicted),color="Type.x",
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in MAOM per g soil"))+
  scale_x_continuous(expression("active_carbon"),
                     label = scales::comma) 
mgMAOM_active_carbon
ggsave("mgMAOM_active_carbon.jpeg", width = 4, height = 3)


