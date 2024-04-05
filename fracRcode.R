#exploring data of fractionation study

setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
#setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin

#load your libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(emmeans)
library(nlme)
library(ggeffects)

##call in the analytical data
Frac <- read.csv(file="Frac.csv", header=TRUE, sep=",")
Loc <- read.csv(file="Location.csv", header=TRUE, sep=",")
Mang <- read.csv(file="Mang.csv", header=TRUE, sep=",") 
Master <- read.csv(file="Master.csv", header=TRUE, sep=",", fileEncoding="latin1")
PRISM2annual <- read.csv(file="PRISM2annual.csv", header=TRUE, sep=",")

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

# join tables
data<-Frac %>%
  left_join(.,Mang,by="Field_Code")
data<-data %>%
    left_join(.,Master,by=c("Field_Code", "soil_texture_class")) 
data<-data %>%
  left_join(.,PRISM2annual,by="Field_Code")
names(data)

#linear model
#First, graphically explore relationships among variables


#look for interactions between independent variables
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
  
 #more interactions with ph
    data <- data %>% 
    mutate(phcategory=cut(ph, breaks=c(-Inf,6.17, 6.52, 6.93, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilM, col=ph))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ phcategory, ncol=1, scales="free_x")
  
  data <- data %>% 
    mutate(phcategory=cut(ph, breaks=c(-Inf,6.17, 6.52, 6.93, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=ppt.cm, y=mgCpergSoilM, col=ph))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ phcategory, ncol=1, scales="free_x")
  
  #interactions with ppt
  
  data <- data %>% 
    mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 106.4, 110.0, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilM, col=ppt.cm))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ pptcategory, ncol=1, scales="free_x")
  
  
  data <- data %>% 
    mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 106.4, 110.0, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=aggregate_stability, y=mgCpergSoilM, col=ppt.cm))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ pptcategory, ncol=1, scales="free_x")
  
  
  data <- data %>% 
    mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 106.4, 110.0, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=ph, y=mgCpergSoilM, col=ppt.cm))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ pptcategory, ncol=1, scales="free_x")
  
  data <- data %>% 
    mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 106.4, 110.0, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=active.carbon, y=mgCpergSoilM, col=ppt.cm))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ pptcategory, ncol=1, scales="free_x")
  
  #interactions with aggregate stability
  summary(data$aggregate_stability)
  
  data <- data %>% 
    mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.8, 46.7, 63.7, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=ph, y=mgCpergSoilM, col=aggregate_stability))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ agStcategory, ncol=1, scales="free_x")
 
  
  data <- data %>% 
    mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.8, 46.7, 63.7, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=ppt.cm, y=mgCpergSoilM, col=aggregate_stability))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ agStcategory, ncol=1, scales="free_x")
  
  data <- data %>% 
    mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.8, 46.7, 63.7, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilM, col=aggregate_stability))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ agStcategory, ncol=1, scales="free_x")
  
  data <- data %>% 
    mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.8, 46.7, 63.7, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=soil_texture_clay, y=mgCpergSoilM, col=aggregate_stability))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ agStcategory, ncol=1, scales="free_x")
  
  
  data <- data %>% 
    mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.8, 46.7, 63.7, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=active_carbon, y=mgCpergSoilM, col=aggregate_stability))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ agStcategory, ncol=1, scales="free_x")
   
#Models

#Correlation plot
cordata <- cor(data[,c("mgCpergSoilM","ph","ppt.cm","tmeanC","aggregate_stability","soil_texture_clay","active_carbon")], use="pairwise.complete.obs", method="pearson")
corrplot(cordata)
  
##Linear Mixed Model for dependent variable (mgCpergSoilM)

#test without random effect, because only one value per field

m1=gls(mgCpergSoilM~ppt.cm*soil_texture_clay+
        soil_texture_clay*tmeanC+ppt.cm*tmeanC+aggregate_stability+active_carbon+ 
      ph, data=data, na.action=na.exclude, method="ML")
summary(m1)
anova(m1)

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

m1=gls(mgCpergSoilM~ppt.cm*soil_texture_clay+
         soil_texture_clay*tmeanC+ppt.cm*tmeanC+aggregate_stability+active_carbon+ 
         ph, data=data, na.action=na.exclude, method="REML")
summary(m1)
anova(m1)

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

par(op)

#Visualize significant relationships

#own_theme below sets ggplot parameters for how plots should look. 
own_theme <- theme_bw(base_size = 11) +
  theme(rect = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank())

pred_ppt <- ggpredict(m1, terms = c("ppt.cm", "soil_texture_clay[20]"))

mgMAOM_ppt <-data %>% 
  ggplot() +
  geom_point(aes(x = ppt.cm, y = mgCpergSoilM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_ppt, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_x_continuous(expression("mg C in MAOM per g soil"))+
  scale_y_continuous(expression("Mean Annual Precipitation (cm)"),
                     label = scales::comma) 

mgMAOM_ppt

ggsave("mgMAOM_ppt.jpeg", width = 4, height = 3)


#for agg stability 
own_theme <- theme_bw(base_size = 11) +
  theme(rect = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank())

pred_aggregate_stability <- ggpredict(m1, terms = c("aggregate_stability", "soil_texture_clay[20]"))

mgMAOM_aggregate_stability <-data %>% 
  ggplot() +
  geom_point(aes(x = aggregate_stability, y = mgCpergSoilM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_aggregate_stability, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_x_continuous(expression("mg C in MAOM per g soil"))+
  scale_y_continuous(expression("aggregate stability"),
                     label = scales::comma) 

mgMAOM_aggregate_stability

ggsave("mgMAOM_aggregate_stability.jpeg", width = 4, height = 3)







#####OLD CODE########
#data exploration graphs

ggplot(data,aes(x=mgCpergSoilM, y=ph.x, color=Type.x)) + 
  geom_point () +
  geom_smooth(method="lm")

ggplot(data,aes(x=mgCpergSoilM, y=ph.x)) + geom_point ()

ggplot(data,aes(x=propM, y=ph.x, color=Type.x)) + 
  geom_point () +
  geom_smooth(method="lm")

ggplot(data,aes(x=ph.x, y=propM, color=Type.x)) + 
  geom_point () +
  geom_smooth(method="lm")

ggplot(data,aes(x=Type.x, y=ph.x, color=Type.x)) + 
  geom_point () +
 
  #add mean ph for each Type
 

str(data)





# count to look at summarized data
data %>%
  group_by(Type.x,Owned)%>%
  summarize(n=n())

data %>%
 count(Type.x,Owned)

data %>%
  count(Type.x,years.since.till)

data %>%
  count(Type.x,animal)

data %>%
  count(Type.x,animal)

data %>%
  count(Type.x,till.passes)

data %>%
  count(Type.x,till.depth)

data %>%
  count(Type.x,plastic.mulch)
data %>%
  count(Type.x,herbicides)
data %>%
  count(Type.x,insecticides)
data %>%
  count(Type.x,treated.seed)
data %>%
  count(Type.x,organic)
data %>%
  count(Type.x,Pattern.Drainage.Tile)
data %>%
  count(Type.x,irrigation)

data %>%
  count(Type.y,irrigation)

#summarize the data in a new dataframe

data.stats <- data %>% 
  group_by(Type.x) %>%
  summarize (n=n(),
            mean=mean(years.since.till, na.rm = TRUE), 
            min=min(years.since.till, na.rm = TRUE), 
            max=max(years.since.till, na.rm = TRUE), 
            sd=sd(years.since.till, na.rm = TRUE),
            mean=mean(years.since.till, na.rm = TRUE), 
  min=min(years.since.till, na.rm = TRUE), 
  max=max(years.since.till, na.rm = TRUE), 
  sd=sd(years.since.till, na.rm = TRUE))

# subset to get annual ppt only
PRISMannual <- subset(PRISM,Date=="Annual") 

#data exploration graphs

ggplot(data,aes(x=count till.passes, y=till.passes, color=Type.x)) + geom_point ()

ggplot(data,aes(x=Type.x, y=till.passes, color=Type.x)) + geom_bar ()

ggplot(data,aes(x=Type.x, y=till.depth, color=Type.x)) + geom_point ()

ggplot (PRISMannual, aes (x=ppt..inches.))+
  geom_histogram()

max()

ggplot(PRISMannual,aes(x=Name, y=ppt..inches, color=Name))+ geom_point ()

ggplot(data,aes(x=ppt..inches., y=mgCpergSoilP,color=ppt..inches.))+ geom_point ()
ggplot(data,aes(x=tmean..degrees.F., y=mgCpergSoilP,color=tmean..degrees.F.))+ geom_point ()
