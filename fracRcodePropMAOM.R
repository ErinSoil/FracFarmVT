#Creating Linear Models and Graphing Relationships with Proporation of MAOM

#setwd("/Users/f003833/Documents/GitHub/FracFarmVT") #caitlin
setwd("C:/Users/F004SPC/Documents/GitHub/FracFarmVT") #erin

#load your libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(emmeans)
library(nlme)
library(gtools)
library(ggeffects)

##call in the analytical data
data <- read.csv("data.csv")
data2 <- read.csv("data2.csv")
view(data)


#calculate propMOAM
data <- data %>%
  mutate(propM = gCarbonM/(gCarbonM+ gCarbonP))

#transform data logit
#data <- data %>%
 # dplyr::mutate(logitpropM = logit(propM))
#write.csv(data,"data.csv")
#hist(data$logitpropM)
#summary(data$logitpropM)
#hist(data$propM, main = "Histogram of propM", xlab = "propM Values", ylab = "Frequency")
#hist(data$logit, main = "Histogram of logitpropM", xlab = "propM Values", ylab = "Frequency")               



#soil health regression

# Perform linear regression
regression_model <- lm(logitpropM ~ overall.score, data = data)

# Summarize the regression model
summary(regression_model)

# Create a plot with the regression line
ggplot(data, aes(x = overall.score, y = logitpropM)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of mgCpergSoilM on overall.score",
       x = "Overall Score",
       y = "logit proportion of MAOM") +
  theme_minimal()

Color by soil_texture_class
# Perform linear regression
regression_model <- lm(logitpropM ~ overall.score, data = data)

# Summarize the regression model
summary(regression_model)

# Create a plot with the regression line and colored points
ggplot(data, aes(x = overall.score, y = logitpropM, color = soil_texture_class)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of logitpropM on overall.score",
       x = "Overall Score",
       y = "Logit Proportion of MAOM") +
  theme_minimal()


library(ggplot2)
library(dplyr)

# Create a new column for color based on soil_texture_class
data <- data %>%
  mutate(color = ifelse(grepl("Clay", soil_texture_class), "red", "black"))

# Create a plot with the regression line and colored points
ggplot(data, aes(x = overall.score, y = logitpropM, color = color)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of logitpropM on overall.score",
       x = "Overall Score",
       y = "Logit Proportion of MAOM") +
  theme_minimal() +
  scale_color_identity()





# total carbon regression
regression_model <- lm(logitpropM ~ (mgCpergSoilM+mgCpergSoilP), data = data)

# Summarize the regression model
summary(regression_model)

# Create a plot with the regression line
ggplot(data, aes(x = (mgCpergSoilM+mgCpergSoilP), y = logitpropM)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of mgCpergSoilM on total carbon",
       x = "Total Carbon",
       y = "logit proportion of MAOM") +
  theme_minimal()



# Perform linear regression
regression_model <- lm(overall.score ~ logitpropM, data = data)

# Summarize the regression model
summary(regression_model)

# Create a plot with the regression line
ggplot(data, aes(x = logitpropM, y = overall.score)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression of soil health score on logitpropM",
       x = "Proportion of MAOM",
       y = "overall soil health score") +
  theme_minimal()


#linear model
#First, graphically explore relationships among variables
#look for interactions between independent variables

summary(data$soil_texture_clay)
data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))
ggplot(data=data, aes(x=tmeanC, y=logitpropM, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))
ggplot(data=data, aes(x=ppt.cm, y=logitpropM, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))
ggplot(data=data, aes(x=ph, y=logitpropM, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))
ggplot(data=data, aes(x=aggregate_stability, y=logitpropM, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(claycategory=cut(soil_texture_clay, breaks=c(-Inf, 14, 24, Inf), labels=c("low","med", "high")))
ggplot(data=data, aes(x=active_carbon, y=logitpropM, col=soil_texture_clay))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ claycategory, ncol=1, scales="free_x")

#more interactions with ph
summary(data$ph)

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=tmeanC, y=logitpropM, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=ppt.cm, y=logitpropM, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=aggregate_stability, y=logitpropM, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=soil_texture_clay, y=logitpropM, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(phcategory=cut(ph, breaks=c(-Inf,6,7, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=active_carbon, y=logitpropM, col=ph))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ phcategory, ncol=1, scales="free_x")

#interactions with ppt
summary(data$ppt.cm)
data <- data %>% 
  mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 110.0, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=tmeanC, y=logitpropM, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")


data <- data %>% 
  mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 110.0, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=aggregate_stability, y=logitpropM, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")


data <- data %>% 
  mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 110.0, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=tmeanC, y=logitpropM, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")


data <- data %>% 
  mutate(pptcategory=cut(ppt.cm, breaks=c(-Inf,101.4, 110.0, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=active_carbon, y=logitpropM, col=ppt.cm))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ pptcategory, ncol=1, scales="free_x")

#interactions with aggregate stability
summary(data$aggregate_stability)

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=ph, y=logitpropM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=ppt.cm, y=logitpropM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=tmeanC, y=logitpropM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=soil_texture_clay, y=logitpropM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

data <- data %>% 
  mutate(agStcategory=cut(aggregate_stability, breaks=c(-Inf,29.3, 65.6, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=active_carbon, y=logitpropM, col=aggregate_stability))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ agStcategory, ncol=1, scales="free_x")

#tmeanC and active carbon
summary(data$tmeanC)

data <- data %>% 
  mutate(tmeanCcategory=cut(tmeanC, breaks=c(-Inf,6.8, 7.6, Inf), labels=c("low","med","high")))
ggplot(data=data, aes(x=active_carbon, y=logitpropM, col=tmeanC))+ 
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap(~ tmeanCcategory, ncol=1, scales="free_x")

#Correlation plot
cordata <- cor(data[,c("mgCpergSoilP", "mgCpergSoilM", "logitpropM","ph","ppt.cm","tmeanC","aggregate_stability","soil_texture_clay","active_carbon")], use="pairwise.complete.obs", method="pearson")
corrplot(cordata)
view(cordata)
head(cordata)

##Linear Mixed Model for dependent variable (logitpropM)
#test without random effect, because only one value per field
#drop non significant predictors until all are significant as model selection, m4M is the choice

#m1M=gls(logitpropM~ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
 #         aggregate_stability * soil_texture_clay + 
  #        active_carbon + 
   #       + ph * soil_texture_clay,  data=data, na.action=na.exclude, method="ML")
#summary(m1M)

#m2M=gls(C~ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
 #         aggregate_stability + 
  #        active_carbon + 
   #       ph * soil_texture_clay,  data=data, na.action=na.exclude, method="ML")
#summary(m2M)

#anova(m1M, m2M)

m3M=gls(logitpropM~ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
                  active_carbon + 
          + ph * soil_texture_clay, data=data, na.action=na.exclude, method="ML")
summary(m3M)
#anova(m2M, m3M)

# Fit the GLS model 
m4M <- gls(logitpropM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
             active_carbon, 
           data = data, 
           na.action = na.exclude, 
           method = "ML")
summary(m4M)
anova(m4M)

#pseudo R squared calculation (fit between model predicted data and actual data)
data$logitpropM.pred=as.vector(fitted(m4M))
R3=lm(logitpropM~logitpropM.pred, data=data, na.action=na.omit)
summary(R3) #r2= .1128


#yikes, nothing is significant and the AIC went up significantly
m5M=gls(logitpropM~ppt.cm * soil_texture_clay+ 
          active_carbon, 
        data=data, na.action=na.exclude, method="ML")
summary(m5M)
anova(m4M, m5M)

#check assumptions, distrubution of residuals

F_Final <- fitted(m4M)
R_Final <- residuals(m4M, type = "pearson", scaled = TRUE)
N = !is.na(data$logitpropM)
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

ppt.cm.c <- summary(m4M)$coefficients[2] #predictor coefficient
ppt.cm.pr <- Rfull + ppt.cm.c*data$ppt.cm  #Residuals + pred coef * predictor value
{scatter.smooth(data$ppt.cm, ppt.cm.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(ppt.cm.c*data$ppt.cm ~ data$ppt.cm), col = "red")} 

soil_texture_clay.c <- summary(m4M)$coefficients[3] #predictor coefficient
soil_texture_clay.pr <- Rfull + soil_texture_clay.c*data$soil_texture_clay  #Residuals + pred coef * predictor value
{scatter.smooth(data$soil_texture_clay, soil_texture_clay.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(soil_texture_clay.c*data$soil_texture_clay ~ data$soil_texture_clay), col = "red")} 

aggregate_stability.c <- summary(m4M)$coefficients[5] #predictor coefficient
aggregate_stability.pr <- Rfull + aggregate_stability.c*data$aggregate_stability  #Residuals + pred coef * predictor value
{scatter.smooth(data$aggregate_stability, aggregate_stability.pr, 
                lpars = list(col = "green", lwd = 3, lty = 3)) #residual loess
  abline(lm(aggregate_stability.c*data$aggregate_stability ~ data$aggregate_stability), col = "red")} 

active_carbon.c <- summary(m4M)$coefficients[6] #predictor coefficient
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


#for clay
pred_soil_texture_clay <- ggpredict(m4M, terms = c("soil_texture_clay"))
propMAOM_soil_texture_clay <-data %>% 
  ggplot() +
  geom_point(aes(x = soil_texture_clay, y = propM), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_soil_texture_clay, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("logit prop C in MAOM"))+
  scale_x_continuous(expression("percent clay texture"),
                     label = scales::comma) 
propMAOM_soil_texture_clay
ggsave("propMAOM_soil_texture_clay.jpeg", width = 4, height = 3)

#interaction ppt and clay
summary(data$ppt.cm)

summary(data$soil_texture_clay)

pred_pptClay <- ggpredict(m4M, terms = c("ppt.cm", "soil_texture_clay[14.158,23.925]"))

pred_pptClay$clay_group <- pred_pptClay$group
levels(pred_pptClay$clay_group) <- c("low (6-19)",  
                                   "high (19.01-54)")
data <- data %>%
  drop_na(soil_texture_clay) %>% 
  dplyr::mutate(clay_group = cut(soil_texture_clay, breaks = c(6,19,54)))

levels(data$clay_group) <- c("low (6-19)",  
                              "high (19.01-54)")

PropM_pptClay <-data %>% 
  ggplot() +
  geom_point(aes(x = ppt.cm, y = logitpropM, col = clay_group), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_pptClay, mapping = aes(x=x, y=predicted, col = clay_group), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  #theme(legend.position = "none") +
  scale_y_continuous(expression("logit Prop MAOM")) +
  scale_x_continuous(expression("Mean Annual Precipitation (cm)"),
                     label = scales::comma) +
  scale_color_manual(values = c("blue", "red")) # adjust colors if needed

PropM_pptClay

#now try clay and ppt reversed

pred_Clayppt <- ggpredict(m4M, terms = c("soil_texture_clay", "ppt.cm[101,106]"))

pred_Clayppt$ppt_group <- pred_Clayppt$group
levels(pred_Clayppt$ppt_group) <- c("low (92-103)",  
                                     "high (103-142)")
data <- data %>%
  drop_na(ppt.cm) %>% 
  dplyr::mutate(ppt_group = cut(ppt.cm, breaks = c(92,101,141)))

levels(data$ppt_group) <- c("low (92-103)",  
                                     "high (103-142)")
PropM_Clayppt <-data %>% 
  ggplot() +
  geom_point(aes(x = soil_texture_clay, y = logitpropM, col = ppt_group), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_Clayppt, mapping = aes(x=x, y=predicted, col = ppt_group), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  #theme(legend.position = "none") +
  scale_y_continuous(expression("logit Prop MAOM")) +
  scale_x_continuous(expression("Percent Clay- Soil Texture"),
                     label = scales::comma) +
  scale_color_manual(values = c("blue", "red")) # adjust colors if needed

PropM_Clayppt


#interaction tmean and clay
summary(data$tmeanC)

summary(data$soil_texture_clay)

pred_tempClay <- ggpredict(m4M, terms = c("tmeanC", "soil_texture_clay[14.158,23.925]"))

pred_tempClay$clay_group <- pred_tempClay$group
levels(pred_tempClay$clay_group) <- c("low (6-19)",  
                                     "high (19.01-54)")
data <- data %>%
  drop_na(soil_texture_clay) %>% 
  dplyr::mutate(clay_group = cut(soil_texture_clay, breaks = c(6,19,54)))

levels(data$clay_group) <- c("low (6-19)",  
                             "high (19.01-54)")

PropM_tempClay <-data %>% 
  ggplot() +
  geom_point(aes(x = tmeanC, y = logitpropM, col = clay_group), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_tempClay, mapping = aes(x=x, y=predicted, col = clay_group), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  #theme(legend.position = "none") +
  scale_y_continuous(expression("logit Prop MAOM")) +
  scale_x_continuous(expression("Mean Annual Temperature (C)"),
                     label = scales::comma) +
  scale_color_manual(values = c("blue", "red")) # adjust colors if needed

PropM_tempClay

#now try clay and tmean reversed

pred_Claytemp <- ggpredict(m4M, terms = c("soil_texture_clay", "tmeanC[6.7,7.6]"))

pred_Claytemp$temp_group <- pred_Claytemp$group
levels(pred_Claytemp$temp_group) <- c("low (4-7.2)",  
                                    "high (7.201-9)")
data <- data %>%
  drop_na(tmeanC) %>% 
  dplyr::mutate(temp_group = cut(tmeanC, breaks = c(4.5,7.2,8.5)))

levels(data$temp_group) <- c("low (4-7.2)",  
                             "high (7.201-9)")
PropM_Claytemp <-data %>% 
  ggplot() +
  geom_point(aes(x = soil_texture_clay, y = logitpropM, col = temp_group), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_Claytemp, mapping = aes(x=x, y=predicted, col = temp_group), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  #theme(legend.position = "none") +
  scale_y_continuous(expression("logit Prop MAOM")) +
  scale_x_continuous(expression("Percent Clay- Soil Texture"),
                     label = scales::comma) +
  scale_color_manual(values = c("blue", "red")) # adjust colors if needed

PropM_Claytemp


#for 3 way interaction of tmean, ppt, and texture

pred_3way <- ggpredict(m4M, terms = c("tmeanC","ppt.cm[101,110]", 
                                     "soil_texture_clay[10, 32]"))

pred_3way$ppt_group <- pred_3way$group
levels(pred_3way$ppt_group) <- c("low (92-104)",  
                                 "high (104-142)")
pred_3way$clay_facet <- pred_3way$facet
levels(pred_3way$clay_facet) <- c("low clay (6-19%)",  
                                  "high clay (19-54%)")

data <- data %>%
  drop_na(ppt.cm) %>% 
  drop_na(soil_texture_clay) %>% 
  dplyr::mutate(ppt_group = cut(ppt.cm, breaks = c(92,104,142)),
                clay_facet = cut(soil_texture_clay, breaks = c(6,19,54)))

levels(data$ppt_group) <- c("low (92-104)",  
                            "high (104-142)")

levels(data$clay_facet) <- c("low clay (6-19%)",  
                             "high clay (19-54%)")

propMAOM_3way <-data %>% 
  ggplot() +
  geom_point(aes(x = tmeanC, y = logitpropM, col = ppt_group), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_3way, mapping = aes(x=x, y=predicted, col = ppt_group), #plot the model's prediction (based on linear )
            lwd = 1) +
  facet_wrap(~clay_facet) +
  own_theme+
  #theme(legend.position = "none") +
  scale_y_continuous(expression(paste("logitpropM"))) +
  scale_x_continuous(expression("Mean Annual Temperature (°C)"),
                     label = scales::comma) +
  guides(col=guide_legend(title="MAP (cm)")) +
  scale_color_manual(values = c("lightblue", "blue")) # adjust colors if needed

propMAOM_3way
ggsave("propMAOM_3way.jpeg", width = 6.5, height = 3)



# Create a violin plot with individual data points
ggplot(data, aes(x = Type.x, y = logitpropM)) +
  geom_violin(trim = FALSE, fill = "lightblue") +  # Create the violin plot
  geom_jitter(width = 0.2, size = 1, color = "darkblue") +  # Add jittered points
  labs(title = "Proporation of MAOM by Field Type",
       x = "Field Type",
       y = "MAOM Proportion (logit)") +
  theme_minimal()  # Apply a minimal theme for a clean look     

# Create a violin plot with individual data points and mean line
ggplot(data, aes(x = Type.x, y = logitpropM, color = Type.x, fill = Type.x)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Create the violin plot with semi-transparent fill
  geom_jitter(width = 0.2, size = 1) +  # Add jittered points
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, color = "black", fill = "yellow") +  # Add mean points
  labs(title = "Proporation of MAOM by Field Type",
       x = "Field Type",
       y = "MAOM Proportion (logit)") +
  theme_minimal()  # Apply a minimal theme for a clean look

# Create a violin plot with individual data points and mean line for soil texture class
ggplot(data, aes(x = soil_texture_class, y = logitpropM, color = soil_texture_class, fill = soil_texture_class)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Create the violin plot with semi-transparent fill
  geom_jitter(width = 0.2, size = 1) +  # Add jittered points
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, color = "black", fill = "yellow") +  # Add mean points
  labs(title = "Distribution of % MAOM by Soil Texture Class",
       x = "Soil Texture Class",
       y = "Proportion of MAOM (logit)") +
  theme_minimal() +  # Apply a minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Adjust text angle, justification, and size
        plot.margin = margin(5, 5, 10, 5))  # Increase the bottom margin to give more space to labels


#R squared Code


# Ensure all necessary variables are numeric
data$logitpropM <- as.numeric(data$logitpropM)
data$ppt.cm <- as.numeric(data$ppt.cm)
data$soil_texture_clay <- as.numeric(data$soil_texture_clay)
data$tmeanC <- as.numeric(data$tmeanC)
data$active_carbon <- as.numeric(data$active_carbon)

# Remove rows with any NA values in the relevant columns before fitting the model
data_clean <- na.omit(data)

# Fit your GLS model with cleaned data
m4M <- gls(logitpropM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
             active_carbon, 
           data = data_clean, 
           method = "ML")


# Ensure all necessary variables are numeric
data$logitpropM <- as.numeric(data$logitpropM)
data$ppt.cm <- as.numeric(data$ppt.cm)
data$soil_texture_clay <- as.numeric(data$soil_texture_clay)
data$tmeanC <- as.numeric(data$tmeanC)
data$active_carbon <- as.numeric(data$active_carbon)

# Remove rows with any NA values in the relevant columns before fitting the model
data_clean <- na.omit(data)

# Fit your GLS model with cleaned data
m4M <- gls(logitpropM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
             active_carbon, 
           data = data_clean, 
           method = "ML")

# Define the rsquared_gls function
rsquared_gls <- function(model) {
  # Extract fitted values
  fitted_values <- fitted(model)
  
  # Extract response variable name
  response_variable <- as.character(formula(model)[[2]])
  
  # Extract response values from the cleaned data
  response_values <- model$data[[response_variable]]
  
  # Ensure the response variable is numeric
  if (!is.numeric(response_values)) {
    response_values <- as.numeric(as.character(response_values))
  }
  
  # Debugging: Print initial response values and fitted values
  cat("Initial response values:\n")
  print(response_values)
  cat("Fitted values:\n")
  print(fitted_values)
  
  # Ensure no NA values are in the response values or fitted values
  valid_indices <- !is.na(response_values) & !is.na(fitted_values)
  response_values <- response_values[valid_indices]
  fitted_values <- fitted_values[valid_indices]
  
  # Debugging: Print cleaned response values and fitted values
  cat("Cleaned response values:\n")
  print(response_values)
  cat("Fitted values:\n")
  print(fitted_values)
  
  # Verify lengths of response values and fitted values
  if (length(response_values) != length(fitted_values)) {
    stop("Length mismatch between response values and fitted values")
  }
  
  # Calculate residuals
  residuals <- response_values - fitted_values
  
  # Calculate the sum of squared residuals
  ss_res <- sum(residuals^2)
  
  # Calculate the total sum of squares
  ss_tot <- sum((response_values - mean(response_values))^2)
  
  # Debugging: Print sum of squared residuals and total sum of squares
  cat("SS_res:", ss_res, "\n")
  cat("SS_tot:", ss_tot, "\n")
  
  # Check for zero variance in response values
  if (ss_tot == 0) {
    warning("Total sum of squares is zero, resulting in NaN R-squared")
    return(NaN)
  }
  
  # Calculate R-squared
  rsq <- 1 - (ss_res / ss_tot)
  
  return(rsq)
}

# Calculate and print R-squared
rsquared_value <- rsquared_gls(m4M)
print(rsquared_value)


#anova by field type to see differences 
field_anova<- aov(propM~Type.x, data=data)
summary(field_anova)  

TukeyHSD(field_anova)
