
data <- read.csv("data_pH.csv")

#own_theme below sets ggplot parameters for how plots should look. 
# own_theme <- theme_bw(base_size = 11) +
#   theme(rect = element_blank(),
#         axis.ticks = element_line(color = "black"),
#         axis.text = element_text(color = "black"),
#         axis.line = element_line(color = "black"),
#         panel.grid.minor = element_blank())
# 

own_theme <- theme_bw(base_size = 11) +
  theme(rect = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(12,5.5,5.5,5.5), "points"))
m3 = gls(mgCpergSoilM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
           active_carbon + aggregate_stability,
         data = data, 
         na.action = na.exclude, 
         method = "REML")

m3P=gls(mgCpergSoilP~ppt.cm*tmeanC
        +aggregate_stability+active_carbon+ph, 
        data=data, na.action=na.exclude, method="REML")

#for aggregate stability
pred_aggregate_stability <- ggpredict(m3, terms = c("aggregate_stability"))

mgMAOM_aggregate_stability <-data %>% 
  ggplot() +
  geom_point(aes(x = aggregate_stability, y = mgCpergSoilM, color=AP), #plot your data
             alpha = 0.5) +
  geom_line(pred_aggregate_stability, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg MAOC g"^-1*"soil"))+
  scale_x_continuous(expression("Aggregate Stability (%)"),
                     label = scales::comma)+
  scale_color_manual(values = c("Annual" = "#cd853f", "Perennial" = "darkgreen"))  # Custom colors for annual and perennial

mgMAOM_aggregate_stability

#for active_carbon
pred_active_carbon <- ggpredict(m3, terms = c("active_carbon"))

mgMAOM_active_carbon <-data %>% 
  ggplot() +
  geom_point(aes(x = active_carbon, y = mgCpergSoilM, color= AP), #plot your data
             alpha = 0.5) +
  geom_line(pred_active_carbon, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(legend.position = "none") +
  scale_y_continuous(expression("mg MAOC g"^-1*"soil"))+
  scale_x_continuous(expression("Active Carbon (ppm)"),
                     label = scales::comma) +
  scale_color_manual(values = c("Annual" = "#cd853f", "Perennial" = "darkgreen"))  # Custom colors for annual and perennial

mgMAOM_active_carbon
#for agg stability color by AP()
pred_aggregate_stability <- ggpredict(m3P, terms = c("aggregate_stability"))
mgPOM_aggregate_stability <-data %>% 
  ggplot() +
  geom_point(aes(x = aggregate_stability, y = mgCpergSoilP, color= AP),
             alpha = 0.5) +
  geom_line(pred_aggregate_stability, mapping = aes(x=x, y=predicted), #plot the model's prediction (based on linear )
            lwd = 1) +
  own_theme+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8)
  )+
  scale_y_continuous(
    name = expression("mg POC " * g^{-1} * " soil")
  )+
scale_x_continuous(expression("Aggregate Stability (%)"),
                   label = scales::comma) +
  scale_color_manual(values = c("Annual" = "#cd853f", "Perennial" = "darkgreen"))  # Custom colors for annual and perennial


mgPOM_aggregate_stability
# Generate predictions from the model
pred_active_carbon <- ggpredict(m3P, terms = c("active_carbon"))
# Convert the ggpredict object to a data frame
pred_df <- as.data.frame(pred_active_carbon)

# Create the plot
mgPOM_active_carbon <- data %>%
  ggplot(aes(x = active_carbon, y = mgCpergSoilP, color= AP)) +
  geom_point(alpha = 0.5) +
  geom_line(data = pred_df, aes(x = x, y = predicted), color = "black", lwd = 1) +  # Change color of the line
  own_theme +  
  theme(legend.position = "none") +
  scale_y_continuous(
    name = expression("mg POC " * g^{-1} * " soil")
  ) +
  scale_x_continuous(
    name = expression("Active Carbon (ppm)"),
    labels = scales::comma
  )+
  scale_color_manual(values = c("Annual" = "#cd853f", "Perennial" = "darkgreen"))  # Custom colors for annual and perennial


mgPOM_active_carbon

# Arrange plots in a 2x2 layout without a legend
Figure2Final<- ggarrange(mgPOM_aggregate_stability, mgPOM_active_carbon, 
                         mgMAOM_aggregate_stability, mgMAOM_active_carbon, 
                         nrow = 2, ncol = 2, 
                         labels = c("a", "b", "c", "d"), 
                         common.legend = FALSE)
Figure2Final
# Save the plot using ggsave
ggsave("FinalFigures/Figure2submit.jpeg", plot = Figure2Final, width = 8, height = 5, dpi = 600)

