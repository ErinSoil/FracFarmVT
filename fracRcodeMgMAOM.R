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

##call in the analytical data
data <- read.csv("data.csv")


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
cordata <- cor(data[,c("mgCpergSoilM","ph","ppt.cm","tmeanC","aggregate_stability","soil_texture_clay","active_carbon")], use="pairwise.complete.obs", method="pearson")
corrplot(cordata)
view(cordata)

##Linear Mixed Model for dependent variable (mgCpergSoilM)
#test without random effect, because only one value per field

m1 = gls(mgCpergSoilM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
           aggregate_stability * soil_texture_clay + 
           active_carbon + 
           + + ph * soil_texture_clay, 
         data = data, 
         na.action = na.exclude, 
         method = "ML")
summary(m1)

#check if variance structure improves the model (to test, both methods were set to REML)

m2 = gls(mgCpergSoilM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
           active_carbon + ph * soil_texture_clay +aggregate_stability, 
         data = data, 
         na.action = na.exclude, 
         method = "ML")
summary(m2)


anova (m1,m2)

m3 = gls(mgCpergSoilM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
           active_carbon +aggregate_stability, 
         data = data, 
         na.action = na.exclude, 
         method = "ML")
summary(m3)
anova (m2,m3)


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
  scale_y_continuous(expression("mg C in MAOM per g soil"))+
  scale_x_continuous(expression("Aggregate Stability"),
              label = scales::comma) 
mgMAOM_aggregate_stability
ggsave("mgMAOM_aggregate_stability.jpeg", width = 4, height = 3)
                   
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
                     scale_y_continuous(expression("mg C in MAOM per g soil"))+
                     scale_x_continuous(expression("active carbon"),
                                        label = scales::comma) 
                   mgMAOM_active_carbon
                   ggsave("mgMAOM_active_carbon.jpeg", width = 4, height = 3)
                   
      #for 3 way interaction
                   
                   
   #analyze data by field type. group by and color by field type this code is in progress
    #exploration
                   mgMAOM_active_carbonbyField <-data %>% 
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
                   
                   