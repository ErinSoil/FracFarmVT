#exploring data of fractionation study

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

##TO CREATE FILE to work with 
##call in the analytical data
Frac <- read.csv(file="Frac.csv", header=TRUE, sep=",")
Loc <- read.csv(file="Location.csv", header=TRUE, sep=",")
Mang <- read.csv(file="Mang.csv", header=TRUE, sep=",") 
Master2 <- read.csv(file="Master2.csv", header=TRUE, sep=",", fileEncoding="latin1")
PRISM2annual <- read.csv(file="PRISM2annual.csv", header=TRUE, sep=",")

# join tables  
fracData2<-Frac %>%
  left_join(.,Master2,by=c("Field_Code", "soil_texture_class")) 
 fracData2<-fracData2 %>%
    left_join(.,Mang,by="Field_Code")
 fracData2<-fracData2 %>%
   left_join(.,PRISM2annual,by="Field_Code")
 names(fracData2)

#Write updated dataframe to CSV, overwriting the existing file
write.csv(fracData2, file = "C:/Users/F004SPC/Documents/GitHub/FracFarmVT/fracData2.csv", row.names = FALSE)
data <- read.csv(file="fracData2.csv", header=TRUE, sep=",")

data <- read.csv("data.csv")

View(data)

#view na for ph  # Z1, Z2 are truly NA as no CASH was completed for these
missing_ph <- subset(data, is.na(ph) | ph == "")
missing_field_codes <- missing_ph$Field_Code
print(missing_field_codes)

missing_mgCpergSoilM <- subset(data, is.na(mgCpergSoilM) | mgCpergSoilM == "")
missing_field_codes <- missing_mgCpergSoilM$Field_Code
print(missing_field_codes)

#correcting a location to get MAT and MAP
missing_ppt <- subset(data, is.na(ppt.cm) | ppt.cm == "")
missing_field_codes <- missing_ppt$Field_Code
print(missing_field_codes)

# Get summary statistics
data_summary <- summary(Mang)

data_min <- min(Mang$Acres,na.rm=TRUE)
data_max <- max(Mang$Acres,na.rm=TRUE)

data_summary <-summary(PRISMannual)
data_min <- min(PRISMannual$ppt..inches.,na.rm=TRUE)
data_max <- max(PRISMannual$ppt..inches.,na.rm=TRUE)

data_summary <-summary(PRISMannual)
data_min <- min(PRISMannual$tmean..degrees.F.,na.rm=TRUE)
data_max <- max(PRISMannual$tmean..degrees.F,na.rm=TRUE)


#First, graphically explore relationships 
#to look for interactions between independent variables that research shows could have interactions

View (data)
summary(data$soil_texture_clay)
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


m2 = gls(mgCpergSoilM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
          active_carbon + ph * soil_texture_clay +aggregate_stability, 
         data = data, 
         na.action = na.exclude, 
         method = "ML")
summary(m2)

m2 = gls(mgCpergSoilM ~active_carbon + ph * soil_texture_clay +
           ppt.cm * tmeanC+aggregate_stability+ppt.cm * soil_texture_clay * tmeanC , 
         data = data, 
         na.action = na.exclude, 
         method = "ML")
summary(m2)
anova (m1,m2)


m1test = gls(mgCpergSoilM ~ ppt.cm + tmeanC + 
           aggregate_stability * soil_texture_clay + 
           active_carbon + 
           ph,
         data = data, 
         na.action = na.exclude, 
         method = "ML")
summary(m1test)
anova(m1test)

drop1(m1)

#new mb test


mb=gls(mgCpergSoilM~ppt.cm*tmeanC+aggregate_stability+active_carbon+ 
         ph+ppt.cm*soil_texture_clay+
         soil_texture_clay*tmeanC, data=data, na.action=na.exclude, method="ML")
summary(mb)
anova(mb)


#anova for different field types 

mType=aov(aggregate_stability~Type.x, data=data, na.action=na.exclude) 
summary(mType)
TukeyHSD(mType)


mType=aov(mgCpergSoilM~Type.x, data=data, na.action=na.exclude) 
summary(mType)
TukeyHSD(mType)

#box plot

#check if variance structure improves the model, it does not (to test, both methods were set to REML)
m1a=gls(mgCpergSoilM~ppt.cm*soil_texture_clay+
         soil_texture_clay*tmeanC+ppt.cm*tmeanC+aggregate_stability+active_carbon+ 
         ph, data=data, na.action=na.exclude, weights = varFixed(~mgCpergSoilM), method="REML")
anova(m1, m1a) #adding variance structure does not improve the model

m2=gls(mgCpergSoilM~ppt.cm*soil_texture_clay+
         soil_texture_clay*tmeanC+ppt.cm*tmeanC+aggregate_stability+active_carbon,
       data=data, na.action=na.exclude, method="ML")

anova(m1,m2)
anova(m2)

m3=gls(mgCpergSoilM~ppt.cm+
         soil_texture_clay*tmeanC+ppt.cm*tmeanC+aggregate_stability+
         active_carbon,
       data=data, na.action=na.exclude, method="ML")

anova(m2,m3)
anova(m3)

m4=gls(mgCpergSoilM~ppt.cm+soil_texture_clay+
         tmeanC+aggregate_stability+
         active_carbon,
       data=data, na.action=na.exclude, method="ML")

anova(m3,m4)
anova(m4)

m5=gls(mgCpergSoilM~ppt.cm+soil_texture_clay+
         aggregate_stability+
         active_carbon,
       data=data, na.action=na.exclude, method="ML")

anova(m4,m5)
anova(m5)

#above is if you want to drop non significant predictors until all are significant as model selection, but there is really no need. 
#I would stick with model 1

#check assumptions, distrubution of residuals


m1 = gls(mgCpergSoilM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
           aggregate_stability * soil_texture_clay + 
           active_carbon + 
           + + ph * soil_texture_clay, 
         data = data, 
         na.action = na.exclude, 
         method = "REML")
summary(m1)

F_Final <- fitted(m1)
R_Final <- residuals(m1, type = "pearson", scaled = TRUE)
N = !is.na(data$mgCpergSoilM)
Rfull <- NA
Rfull[N] <- R_Final
op <- par(mfrow = c(2,2), mar = c(5,4,1,1))  #I can't figure this part out
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

ppt.cm.c <- summary(m1)$coefficients[2] #predictor coefficient
ppt.cm.pr <- Rfull + ppt.cm.c*data$ppt.cm  #Residuals + pred coef * predictor value
{scatter.smooth(data$ppt.cm, ppt.cm.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(ppt.cm.c*data$ppt.cm ~ data$ppt.cm), col = "red")} 

soil_texture_clay.c <- summary(m1)$coefficients[3] #predictor coefficient
soil_texture_clay.pr <- Rfull + soil_texture_clay.c*data$soil_texture_clay  #Residuals + pred coef * predictor value
{scatter.smooth(data$soil_texture_clay, soil_texture_clay.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(soil_texture_clay.c*data$soil_texture_clay ~ data$soil_texture_clay), col = "red")} 

aggregate_stability.c <- summary(m1)$coefficients[5] #predictor coefficient
aggregate_stability.pr <- Rfull + aggregate_stability.c*data$aggregate_stability  #Residuals + pred coef * predictor value
{scatter.smooth(data$aggregate_stability, aggregate_stability.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(aggregate_stability.c*data$aggregate_stability ~ data$aggregate_stability), col = "red")} 

active_carbon.c <- summary(m1)$coefficients[6] #predictor coefficient
active_carbon.pr <- Rfull + active_carbon.c*data$active_carbon  #Residuals + pred coef * predictor value
{scatter.smooth(data$active_carbon, active_carbon.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(active_carbon.c*data$active_carbon ~ data$active_carbon), col = "red")} 

tmeanC.c <- summary(m1)$coefficients[6] #predictor coefficient
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

<<<<<<< Updated upstream
#for aggregate stability
pred_aggregate_stability <- ggpredict(m1, terms = c("aggregate_stability"))
=======
pred_ppt <- ggpredict(m1, terms = c("ppt.cm", "soil_texture_clay[20]"))

mgMAOM_ppt <-data %>% 
  ggplot() +
  geom_point(aes(x = ppt.cm, y = mgCpergSoilM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_ppt, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in MAOM per g soil"))+
  scale_x_continuous(expression("Mean Annual Precipitation (cm)"),
                     label = scales::comma) 

mgMAOM_ppt

ggsave("mgMAOM_ppt.jpeg", width = 4, height = 3)


#for agg stability 

pred_aggregate_stability <- ggpredict(m1, terms = c("aggregate_stability", "soil_texture_clay[20]"))

>>>>>>> Stashed changes
mgMAOM_aggregate_stability <-data %>% 
  ggplot() +
  geom_point(aes(x = aggregate_stability, y = mgCpergSoilM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_aggregate_stability, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in MAOM per g soil"))+
<<<<<<< Updated upstream
  scale_x_continuous(expression("aggregate_stability"),
=======
  scale_x_continuous(expression("aggregate stability"),
>>>>>>> Stashed changes
                     label = scales::comma) 
mgMAOM_aggregate_stability
ggsave("mgMAOM_aggregate_stability.jpeg", width = 4, height = 3)


#ppt
pred_ppt <- ggpredict(m1, terms = c("ppt.cm"))
mgMAOM_ppt <-data %>% 
  ggplot() +
  geom_point(aes(x = ppt.cm, y = mgCpergSoilM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_ppt, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in MAOM per g soil"))+
  scale_x_continuous(expression("Mean Annual Precipitation (cm)"),
                     label = scales::comma) 
mgMAOM_ppt
ggsave("mgMAOM_ppt.jpeg", width = 4, height = 3)

#for aggregate stabilty
pred_aggregate_stability <- ggpredict(m1, terms = c("aggregate_stability"))
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
ggsave("mgMAOM_ppt.jpeg", width = 4, height = 3)

#for clay # not sig different from zero, a little negative
pred_soil_texture_clay <- ggpredict(m1, terms = c("soil_texture_clay"))
mgMAOM_soil_texture_clay <-data %>% 
  ggplot() +
  geom_point(aes(x = soil_texture_clay, y = mgCpergSoilM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_soil_texture_clay, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in MAOM per g soil"))+
  scale_x_continuous(expression("clay"),
                     label = scales::comma) 
mgMAOM_soil_texture_clay
ggsave("mgMAOM_soil_texture_clay.jpeg", width = 4, height = 3)


#for active_carbon
pred_active_carbon <- ggpredict(m1, terms = c("active_carbon"))
mgMAOM_active_carbon <-data %>% 
  ggplot() +
  geom_point(aes(x = active_carbon, y = mgCpergSoilM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_active_carbon, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg C in MAOM per g soil"))+
  scale_x_continuous(expression("active_carbon"),
                     label = scales::comma) 
mgMAOM_active_carbon
ggsave("mgMAOM_active_carbon.jpeg", width = 4, height = 3)

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

View (PRISM2annual)
View(Frac)
merged_data <- merge ((Frac), PRISM2annual, by = "Field_Code", all = FALSE)
matching_ids <- Frac$Field_Code %in% PRISM2annual$Field_Code

View(matching_ids$merged_data)

unmatched_ids <- anti_join(Frac, PRISM2annual, by = "Field_Code")

# View the unmatched IDs
print(unmatched_ids)



#view NA for ppt in data
missing_tmean <- subset(data, is.na(tmeanC) | tmeanC == "")

missing_field_codes <- missing_tmean$Field_Code
print(missing_field_codes)

missing_mgCpergSoilM <- subset(data, is.na(mgCpergSoilM) | mgCpergSoilM == "")
missing_field_codes <- missing_mgCpergSoilM$Field_Code
print(missing_field_codes)

