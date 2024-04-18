#setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin
##call in the analytical data
fracData <- read.csv(file="fracData.csv", header=TRUE, sep=",")
str(fracData) 
# create a new dataframe with more variables for the rf predictor (mgCpergSoilM)
fracData_rmALLgM<- fracData %>%
  select(active_carbon,ph,ppt.cm,tmeanC,overall.score, soil_texture_clay, organic_matter, aggregate_stability, pred_water_capacity, mgCpergSoilM) %>%
  #remove the rows with ~25ish missing data 
  drop_na() 
#create a new dataframe with more variables for the rf predictor (propM)
fracData_rmALLpm <- fracData %>%
  select(active_carbon,ph,ppt.cm,tmeanC,overall.score, soil_texture_clay, organic_matter, aggregate_stability, pred_water_capacity, propM) %>%
  #remove the rows with ~25ish missing data 
  drop_na() 
# create a new dataframe with more variables for the rf predictor (mgCpergSoilP)
fracData_rmALLgP<- fracData %>%
  select(active_carbon,ph,ppt.cm,tmeanC,overall.score, soil_texture_clay, organic_matter, aggregate_stability, pred_water_capacity, mgCpergSoilP) %>%
  #remove the rows with ~25ish missing data 
  drop_na() 

# create a new dataframe with only a couple variables (mgCpergSoilM)
fracData_soilhealthM <- fracData %>%
  select (overall.score, mgCpergSoilM) %>%
  #remove the rows with ~25ish missing data 
  drop_na() 

view(fracData_soilhealthM)
# create a new dataframe with only a couple variables (propM)
fracData_soilhealthpropM <- fracData %>%
  select(overall.score, propM) %>%
  #remove the rows with ~25ish missing data 
  drop_na() 
view(fracData_soilhealthpropM)

# create a new dataframe with only a couple variables (mgCpergSoilP)
fracData_soilhealthP <- fracData %>%
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
#show each variable's relative importance

RFtestSHM <- ranger(mgCpergSoilM ~ ., data = fracData_soilhealthM, importance = "permutation")
RFtestSHM$variable.importance

RFtestSHprop <- ranger(propM ~ ., data = fracData_soilhealthpropM, importance = "permutation")
RFtestSHprop$variable.importance
#negative relationship with Prop M and soil health. Interesting

RFtestSHP <- ranger(mgCpergSoilP ~ ., data = fracData_soilhealthP, importance = "permutation")
RFtestSHP$variable.importance

#run with more variables #change the dataset fracData_rmALL to include dependent variable each time
RFtestM <- ranger(mgCpergSoilM ~ ., data = fracData_rmALLgM, importance = "permutation")
RFtestM$variable.importance

RFtestP <- ranger(mgCpergSoilP ~ ., data = fracData_rmALLgP, importance = "permutation")
RFtestP$variable.importance

RFtestprop <- ranger(propM ~ ., data = fracData_rmALLpm, importance = "permutation")
RFtestprop$variable.importance

#create partial dependance plots for mgCpergSoil(MAOM)
model_data_1 <- Predictor$new(RFtestM, data =  fracData_rmALLgM %>%
                                dplyr::select(-mgCpergSoilM))
pdp_all <- FeatureEffects$new(model_data_1, method = "pdp")
plot(pdp_all)

#create partial dependance plots for mgCpergSoil(POM)
model_data_1 <- Predictor$new(RFtestP, data =  fracData_rmALLgP %>%
                                dplyr::select(-mgCpergSoilP))
pdp_all <- FeatureEffects$new(model_data_1, method = "pdp")
plot(pdp_all)

#create partial dependance plots for PropMAOM
model_data_1 <- Predictor$new(RFtestprop, data = fracData_rmALL %>%
                                dplyr::select(-propM))
pdp_all <- FeatureEffects$new(model_data_1, method = "pdp")
plot(pdp_all)
#create partial dependance plots for soil health RF
model_data_SHM <- Predictor$new(RFtestSHM, data =  fracData_soilhealthM%>%
                                  dplyr::select(-mgCpergSoilM))
pdp_all <- FeatureEffects$new(model_data_SHM, method = "pdp")
plot(pdp_all)

model_data_SHP <- Predictor$new(RFtestSHP, data =  fracData_soilhealthP %>%
                                  dplyr::select(-mgCpergSoilP))
pdp_all <- FeatureEffects$new(model_data_SHP, method = "pdp")
plot(pdp_all)

model_data_SHprop <- Predictor$new(RFtestSHprop, data =  fracData_soilhealthpropM %>%
                                     dplyr::select(-propM))
pdp_all <- FeatureEffects$new(model_data_SHprop, method = "pdp")
plot(pdp_all)

#conduct Random Forest for total Carbon and total active carbon



##Old Code and NOTES###############################################################
# create a new df with select only continuous variables this results in no data. 
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
  #select(#list all the columns I want to keep with commas, no ") %>%
    #remove the rows with ~25ish missing data 
    drop_na()
    
    fracData_rm <- fracData %>%
      select(active_carbon,ph,ppt.cm, mgCpergSoilP, Type.x) %>% #remove the rows with ~25ish missing data 
      drop_na() 
    
    view(fracData_rm)
    #grouped by farm type? 
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
    
    #head(fracData)
    
    #conduct RF for different variables as the predicted mgCpergSoilM, mgCpergSoilP, propM, total carbon, aggregate stability
    #add different variables, like management data to each analysis (when analyzing the mang data, make the NAs "other")
    # do the same as above for the lmm as well as the rf
    
    

