

#setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin

##call in the analytical data
data <- read.csv(file="data.csv", header=TRUE, sep=",")



#Random forest: generalizable models since it is an ensemble of multiple decorrelated trees.
#select variables of interest
rf.afsis <- data.afsis %>% 
  select(all columns)

#grouped by farm type?
data.top <- rf.afsis %>% 
  dplyr::filter(Depth == "Topsoil") %>% 
  select(-Depth)

data.bot <- rf.afsis %>% 
  dplyr::filter(Depth == "Subsoil") %>% 
  select(-Depth)

head(data.top)
head(data.bot)