

#setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin

##call in the analytical data
fracData <- read.csv(file="fracData.csv", header=TRUE, sep=",")

str(fracData)

# create a new df with select only continuous variables
fracDataNum <- fracData %>%
  select_if(is.numeric)

view(fracDataNum)


#remove all rows (farm fields) that have a missing variable
fracDataNum_noNA <- fracData %>%
  select_if(is.numeric) %>%
  drop_na()

view(fracDataNum_noNA)

#summary of dataframe
skimr::skim_without_charts(fracDataNum)


#remove the columns with limited data
fracData_rm <- fracDataNum %>%
  select(#list all the columns I want to keep with commas, no ") %>%
 #remove the rows with ~25ish missing data 
 drop_na()

 fracData_rm <- fracDataNum %>%
   select(active_carbon,ph,ppt.cm, mgCpergSoilM) %>%
     #remove the rows with ~25ish missing data 
     drop_na() 
 
 view(fracData_rm)
 
 
 # create a new dataframe with more variables for the rf
 fracData_rmALL <- fracDataNum %>%
   select(active_carbon,ph,ppt.cm,tmeanC,overall.score, soil_texture_sand, soil_texture_silt, soil_texture_clay, organic_matter, aggregate_stability, pred_water_capacity, mgCpergSoilM) %>%
   #remove the rows with ~25ish missing data 
   drop_na() 
 
 view(fracData_rmALL)
 
 
 
 # create a new dataframe with only a couple variables
 fracData_soilhealth <- fracDataNum %>%
   select(overall.score, mgCpergSoilM) %>%
   #remove the rows with ~25ish missing data 
   drop_na() 
 
 view(fracData_soilhealth)
 
#load library
library(ranger)
library(skimr)

#Random forest: generalizable models since it is an ensemble of multiple decorrelated trees.
#select variables of interest by using different dataframes created above


RFtest <- ranger(mgCpergSoilM ~ ., data = fracData_rm, importance = "permutation")

RFtest$variable.importance

#run with more variables
RFtest1 <- ranger(mgCpergSoilM ~ ., data = fracData_rmALL, importance = "permutation")

#run for soilHealth
RFtest2 <- ranger(mgCpergSoilM ~ ., data = fracData_soilhealth, importance = "permutation")
# this runs the stats on the random forest
RFtest2


#show each variable's relative importance
RFtest2$variable.importance



#grouped by farm type? #ask Sophie if I've done this correctly, need to figure out what to replace rf.afsis with
fracData <- rf.afsis %>% 
  dplyr::filter(Type.x == "Veg") %>% 
  select(-Type.x)

fracData <- rf.afsis %>% 
  dplyr::filter(Type.x == "Pasture") %>% 
  select(-Type.x)


head(fracData)
