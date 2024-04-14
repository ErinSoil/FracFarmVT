

#setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin

##call in the analytical data
fracData <- read.csv(file="fracData.csv", header=TRUE, sep=",")

str(fracData)

# create a new df with select only continuous variables
fracDataNum <- fracData 
  select_if(is.numeric)

view(fracDataNum)


#remove all rows (farm fields) that have a missing variable #this results in no data
fracDataNum_noNA <- fracData %>%
  select_if(is.numeric)  %>%
  drop_na()

view(fracDataNum_noNA)

#summary of dataframe
skimr::skim_without_charts(fracDataNum_noNA)


#remove the columns with limited data
fracData_rm <- fracData %>%
  select(#list all the columns I want to keep with commas, no ") %>%
 #remove the rows with ~25ish missing data 
 drop_na()

 fracData_rm <- fracData %>%
   select(active_carbon,ph,ppt.cm, mgCpergSoilP, Type.x) %>% #remove the rows with ~25ish missing data 
    drop_na() 
 
 view(fracData_rm)
 
 
 # create a new dataframe with more variables for the rf
 fracData_rmALL <- fracData %>%
   select(active_carbon,ph,ppt.cm,tmeanC,overall.score, soil_texture_sand, soil_texture_silt, soil_texture_clay, organic_matter, aggregate_stability, pred_water_capacity, mgCpergSoilM) %>%
   #remove the rows with ~25ish missing data 
   drop_na() 
 
 view(fracData_rmALL)
 
 
 
 # create a new dataframe with only a couple variables
 fracData_soilhealthM <- fracDataNum %>%
   select(overall.score, mgCpergSoilM) %>%
   #remove the rows with ~25ish missing data 
   drop_na() 
 
 view(fracData_soilhealthM)
 
 # create a new dataframe with only a couple variables (POM)
 fracData_soilhealthP <- fracDataNum %>%
   select(overall.score, mgCpergSoilP) %>%
   #remove the rows with ~25ish missing data 
   drop_na() 
 
 view(fracData_soilhealthP)
 
 #load library
library(ranger)
library(skimr)
library(iml)
library(tidyverse)

#Random forest: generalizable models since it is an ensemble of multiple decorrelated trees.
#select variables of interest by using different dataframes created above


RFtest <- ranger(mgCpergSoilM ~ ., data = fracData_rm, importance = "permutation")
RFtest$variable.importance

RFmgCperSoilP <- ranger(mgCpergSoilP ~ ., data = fracData_rm, importance = "permutation")
RFtest$variable.importance

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


#create partialdependance plots for mgCpergSoil(MAOM)

model_data_1 <- Predictor$new(RFtest1, data = fracData_rmALL %>%
                                dplyr::select(-mgCpergSoilM))

pdp_all <- FeatureEffects$new(model_data_1, method = "pdp")



plot(pdp_all)



#grouped by farm type? #ask Sophie if I've done this correctly, need to figure out what to replace rf.afsis with
#fracData <- rf.afsis %>% 
 ##select(-Type.x)

#fracData <- rf.afsis %>% 
 # dplyr::filter(Type.x == "Pasture") %>% 
  #select(-Type.x)
#fracData <- rf.afsis %>% 
 # dplyr::filter(Type.x == "Hay") %>% 
  #select(-Type.x)

#fracData <- rf.afsis %>% 
 # dplyr::filter(Type.x == "Field crops") %>% 
  #select(-Type.x)

# fracData <- rf.afsis %>% 
#   dplyr::filter(Type.x == "Corn") %>% 
#   select(-Type.x)

head(fracData)


#conduct RF for different variables as the predicted mgCpergSoilM, mgCpergSoilP, propM, total carbon, aggregate stablity
#add different variables, like management data to each analysis (when analysing the mang data, make the NAs "other")
# do the same as above for the lmm as well as the rf

