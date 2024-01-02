#exploring data of fractionation study

#setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin

#load your libraries
library(tidyverse)

##call in the analytical data
Frac <- read.csv(file="Frac.csv", header=TRUE, sep=",")
Loc <- read.csv(file="Location.csv", header=TRUE, sep=",")
Mang <- read.csv(file="Mang.csv", header=TRUE, sep=",") 
Master <- read.csv(file="Master.csv", header=TRUE, sep=",") 

# Get summary statistics
data_summary <- summary(Mang)

data_min <- min(Mang$Acres,na.rm=TRUE)
data_max <- max(Mang$Acres,na.rm=TRUE)

# replace column name (new name=old name)
Mang<-Mang %>%
  rename(Field_Code=Field.Code)

data<-Frac%>%
  left_join(.,Mang,by="Field_Code")%>%
  mutate(Type.x=as.factor(Type.x),
         Owned=as.factor(Owned))

data %>%
  group_by(Type.x,Owned)%>%
  summarize(mean(perC_P, na.rm=TRUE))



#data exploration
ggplot(SiteData,aes(x=LD, y=bulkDensity, color=site)) + geom_point ()
ggplot(SiteData,aes(x=LD, y=perC, color=site)) + geom_point ()
ggplot(SiteData,aes(x=LD, y=perN, color=site)) + geom_point ()

#calculate carbon and nitrogen stocks in kg C (or N) per m2
SiteData<-SiteData %>%
  mutate(cStock=(perC/100*bulkDensity*(LD-UD)*100^2)/1000,
         nStock=(perN/100*bulkDensity*(LD-UD)*100^2)/1000)

#calculate total stocks in kg C per m2 per 100 cm
SiteDataTotal <- SiteData %>%
  group_by(SID, site, core) %>%
  summarize (totalCStock = sum (cStock), totalNStock = sum (nStock))

#calculate subsoil stocks in kg C per m2 per 70 cm
SiteDataSubsoil <- SiteData %>%
  filter(UD>30 | LD<100) %>% 
  group_by(SID, site, core) %>%
  summarize (subCStock = sum (cStock), subNStock = sum (nStock))

#calculate topsoil stocks in kg C per m2 per 30 cm
SiteDataTopsoil <- SiteData %>%
  filter(LD<30) %>% 
  group_by(SID, site, core) %>%
  summarize (topCStock = sum (cStock), topNStock = sum (nStock))

