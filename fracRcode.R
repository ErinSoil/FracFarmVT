#exploring data of fractionation study

#setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin

#load your libraries
library(tidyverse)
library(ggplot2)
library(dplyr)

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

# replace column name (new name=old name)
Mang<-Mang %>%
  rename(Field_Code=Field.Code)

PRISMannual<-PRISMannual %>%
  rename(Field_Code=Name)

mutate(Type.x=as.factor(Type.x),
       Owned=as.factor(Owned))

# join tables
data<-Frac %>%
  left_join(.,Mang,by="Field_Code")
data<-data %>%
    left_join(.,Master,by=c("Field_Code", "soil_texture_class")) 
data<-data %>%
  left_join(.,Master,by="Field_Code") 
data<-data %>%
  left_join(.,PRISM2annual,by="Field_Code")
names(data)

#linear mixed model
#check for realtionships amoung variables


library(corrplot)
library(emmeans)

#Correlation plot
cordata <- cor(data[,c("ph.x","ppt.cm","tmeanC","aggregate_stability.x","soil_texture_clay.x","active_carbon.x")], use="pairwise.complete.obs", method="pearson")
corrplot(cordata)

#look for interactions between independent variables
data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay.x, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))
  
  ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilM, col=soil_texture_clay.x))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")
  
  data <- data %>% 
    mutate(claycategory=cut(soil_texture_clay.x, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))
  
  ggplot(data=data, aes(x=ppt.cm, y=mgCpergSoilM, col=soil_texture_clay.x))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ claycategory, ncol=1, scales="free_x")
  
 #more interactions with ph
    data <- data %>% 
    mutate(phcategory=cut(ph.x, breaks=c(-Inf,6.17, 6.52, 6.93, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilM, col=ph.x))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ phcategory, ncol=1, scales="free_x")
  
  data <- data %>% 
    mutate(phcategory=cut(ph.x, breaks=c(-Inf,6.17, 6.52, 6.93, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=ppt.cm, y=mgCpergSoilM, col=ph.x))+ 
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
  
  ggplot(data=data, aes(x=aggregate_stability.x, y=mgCpergSoilM, col=ppt.cm))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ pptcategory, ncol=1, scales="free_x")
  
  
  data <- data %>% 
    mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 106.4, 110.0, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=ph.x, y=mgCpergSoilM, col=ppt.cm))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ pptcategory, ncol=1, scales="free_x")
  
  data <- data %>% 
    mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 106.4, 110.0, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=active.carbon.x, y=mgCpergSoilM, col=ppt.cm))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ pptcategory, ncol=1, scales="free_x")
  
  #interactions with aggregate stability
  summary(data$aggregate_stability.x)
  
  data <- data %>% 
    mutate(agStcategory=cut(aggregate_stability.x, breaks=c(-Inf,29.8, 46.7, 63.7, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=ph.x, y=mgCpergSoilM, col=aggregate_stability.x))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ agStcategory, ncol=1, scales="free_x")
 
  
  data <- data %>% 
    mutate(agStcategory=cut(aggregate_stability.x, breaks=c(-Inf,29.8, 46.7, 63.7, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=ppt.cm, y=mgCpergSoilM, col=aggregate_stability.x))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ agStcategory, ncol=1, scales="free_x")
  
  data <- data %>% 
    mutate(agStcategory=cut(aggregate_stability.x, breaks=c(-Inf,29.8, 46.7, 63.7, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=tmeanC, y=mgCpergSoilM, col=aggregate_stability.x))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ agStcategory, ncol=1, scales="free_x")
  
  data <- data %>% 
    mutate(agStcategory=cut(aggregate_stability.x, breaks=c(-Inf,29.8, 46.7, 63.7, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=soil_texture_clay.x, y=mgCpergSoilM, col=aggregate_stability.x))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ agStcategory, ncol=1, scales="free_x")
  
  
  data <- data %>% 
    mutate(agStcategory=cut(aggregate_stability.x, breaks=c(-Inf,29.8, 46.7, 63.7, Inf), labels=c("low","med","high", "veryhigh")))
  
  ggplot(data=data, aes(x=active_carbon.x, y=mgCpergSoilM, col=aggregate_stability.x))+ 
    geom_point()+
    geom_smooth(method=lm)+
    facet_wrap(~ agStcategory, ncol=1, scales="free_x")
   
#Models
##Linear Mixed Model for dependant variable mcCpergSoilM

```{r mgCpergSoilM Linear Mixed Effects model}
library(lme4)
library(lmerTest)
library(nlme)

m1=gls(mgCpergSoilM~ppt.cm*soil_texture_clay.x+
        soil_texture_clay.x*tmeanC+ppt.cm*tmeanC+aggregate_stability.x+active_carbon.x+ 
      ph.x, data=data, na.action=na.exclude, method="ML")
summary(m1)
anova(m1)

m2=gls(mgCpergSoilM~ppt.cm*soil_texture_clay.x+
         soil_texture_clay.x*tmeanC+ppt.cm*tmeanC+aggregate_stability.x+
         active_carbon.x,
       data=data, na.action=na.exclude, method="ML")

anova(m1,m2)
anova(m2)

m3=gls(mgCpergSoilM~ppt.cm+soil_texture_clay.x+
         soil_texture_clay.x*tmeanC+ppt.cm*tmeanC+aggregate_stability.x+
         active_carbon.x,
       data=data, na.action=na.exclude, method="ML")

anova(m2,m3)
anova(m3)

m4=gls(mgCpergSoilM~ppt.cm+soil_texture_clay.x+
         soil_texture_clay.x*tmeanC+aggregate_stability.x+
         active_carbon.x,
       data=data, na.action=na.exclude, method="ML")

anova(m3,m4)
anova(m4)

m5=gls(mgCpergSoilM~ppt.cm+soil_texture_clay.x+
         tmeanC+aggregate_stability.x+
         active_carbon.x,
       data=data, na.action=na.exclude, method="ML")

anova(m4,m5)
anova(m5)

#check assumptions, distrubution of residuals

```{r mgCpergSoilM Landscape model}
m1=lm(mgCpergSoilM~ppt.cm*soil_texture_clay.x*
  tmeanC*aggregate_stability.x*
  active_carbon.x, data=data, na.action=na.exclude, method="REML")
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
boxplot(Rfull ~ data$mgCpergSoilM)
boxplot(Rfull ~ data$aggregate_stability.x)
plot(Rfull ~ data$soil_texture_clay.x)
plot(Rfull ~ data$active_carbon.x)
plot(Rfull ~ data$tmeanC)
plot(Rfull ~ data$ppt.cm)
par(op)

#partial residuals to test that the relationship is linear
#partial residual plot, soil_texture_clay.x
soil_texture_clay.x.c <- summary(m1)$coefficients[2] #predictor coefficient
soil_texture_clay.x.pr <- Rfull + soil_texture_clay.x*data$soil_texture_clay.x  #Residuals + pred coef * predictor value
{scatter.smooth(data$soil_texture_clay.x, soil_texture_clay.x.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(soil_texture_clay.x.c*data$soil_texture_clay.x ~ data$soil_texture_clay.x), col = "red")} 


#Visualize the mgCpergSoilM by interaction
mgCpergSoilM.rg = ref_grid(m1, at = list(mgCpergSoilM=c(6,7,8,9,10)))
emmip(mgCpergSoilM.rg, tmeanC ~ mgCpergSoilM, style="factor") 
#There is a stronger effect of mgCpergSoilM at colder temps??
Collapse

#Visualize the mgCpergSoilM by pH interaction
mgCpergSoilM.rg = ref_grid(m1, at = list(mgCpergSoilM=c(6,7,8,9,10)))
emmip(mgCpergSoilM.rg, pH ~ mgCpergSoilM, style="factor") 
#There is a stronger effect of mgCpergSoilM at higher/lower pH??
Collapse







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
