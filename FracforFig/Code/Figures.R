
library(ggpubr)
own_theme <- theme_bw(base_size = 11) +
  theme(rect = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank())



#Figure 3 ready to submit 5.7.2025
#for 3 way interaction of tmean, ppt, and texture Figure 3, this is b)
summary(data$soil_texture_clay)
summary(data$ppt.cm)
pred_3way <- ggpredict(m3, terms = c("tmeanC","ppt.cm[101,110]", 
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
mgMAOM_3way <-data %>% 
  ggplot() +
  geom_point(aes(x = tmeanC, y = mgCpergSoilM, col = ppt_group), #plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_3way, mapping = aes(x=x, y=predicted, col = ppt_group), #plot the model's prediction (based on linear )
            lwd = 1) +
  facet_wrap(~clay_facet) +
  own_theme+
  #theme(legend.position = "none") +
  scale_y_continuous(expression(paste("mg MAOC g"^-1,"soil"))) +
  scale_x_continuous(expression("Mean Annual Temperature (°C)"),
                     label = scales::comma) +
  guides(col=guide_legend(title="Mean Annual Precipitation (cm)")) +
  scale_color_manual(values = c("red", "blue")) 

mgMAOM_3way
ggsave("mgMAOM_3way.jpeg", width = 8, height = 4)

#get slope for the lines in the 3 way 
mylist<- list(ppt.cm=c(101,110),soil_texture_clay=c(10,32))




emtrends(m3,pairwise~ppt.cm*soil_texture_clay,var="tmeanC", at=mylist) 



#for 3 way interaction of tmean, ppt, and texture
m4M <- gls(logitpropM ~ ppt.cm * soil_texture_clay * tmeanC + ppt.cm * tmeanC + 
             active_carbon, 
           data = data, 
           na.action = na.exclude, 
           method = "REML")

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
  scale_y_continuous(expression(paste("Proportion of C as MAOC"))) +
  scale_x_continuous(expression("Mean Annual Temperature (°C)"),
                     label = scales::comma) +
  guides(col=guide_legend(title="Mean Annual Precipitation (cm)")) +
  scale_color_manual(values = c("red", "blue")) # adjust colors if needed

propMAOM_3way


#make pretty
# Example adjustment for individual plots (modify as necessary)
mgMAOM_3way <- mgMAOM_3way +
  theme(legend.text = element_text(size = 6),  # Adjust legend text size
        legend.title = element_text(size = 8))  # Adjust legend title size

propMAOM_3way <- propMAOM_3way +
  theme(legend.text = element_text(size = 6),  # Adjust legend text size
        legend.title = element_text(size = 8))  # Adjust legend title size

Figure5


#ppt and tmeanC, 
summary(data$ppt.cm)
pred_tmeanppt <- ggpredict(m3P, terms = c("tmeanC","ppt.cm[101,110]"))
pred_tmeanppt$ppt_group <- pred_tmeanppt$group
levels(pred_tmeanppt$ppt_group) <- c("low (92-104)",  
                                     "high (104-142)")
data <- data %>%
  drop_na(ppt.cm) %>% 
  dplyr::mutate(ppt_group = cut(ppt.cm, breaks = c(92,104,142)))
levels(data$ppt_group) <- c("low (92-104)",  
                            "high (104-142)")
mgPOM_tmeanppt <- data %>% 
  ggplot() +
  geom_point(aes(x = tmeanC, y = mgCpergSoilP, col = ppt_group),
             size = 1.5, alpha = 0.5) +
  geom_line(data = pred_tmeanppt, 
            mapping = aes(x = x, y = predicted, col = ppt_group),
            linewidth = 1) +
  scale_y_continuous(expression("mg POC g"^-1 * " soil")) +
  scale_x_continuous(expression("Mean Annual Temperature (°C)"),
                     labels = scales::comma) +
  scale_color_manual(name = "Mean Annual Precipitation (cm)", 
                     values = c("red", "blue")) +
  own_theme  # Assuming `own_theme` is a predefined theme object


mgPOM_tmeanppt




ggsave("mgPOM_Figure4.jpeg", width = 10, height = 6, units="cm", dpi=300)
#add axes lines
# Define the plot with axis lines added
mgPOM_tmeanppt_b <- data %>% 
  ggplot() +
  geom_point(aes(x = tmeanC, y = mgCpergSoilP, col = ppt_group), # Plot your data
             size = 1.5, alpha = 0.5) +
  geom_line(pred_tmeanppt, mapping = aes(x = x, y = predicted, col = ppt_group), # Plot the model's prediction
            lwd = 1) +
  own_theme +  # Apply your custom theme
  scale_y_continuous(expression("mg POC g"^-1 * "soil")) +
  scale_x_continuous(expression("Mean Annual Temperature (°C)"),
                     label = scales::comma) +
  scale_color_manual(name = "Mean Annual Precipitation (cm)", values = c("red", "blue")) 
 
# Print the plotgPOM_tmeanppt_b

ggsave("mgPOM_Figure4.jpeg", width = 10, height = 6, units="cm", dpi=300)


# Save the plot with adjusted legend
ggsave("mgPOM_Figure4.jpeg", plot = last_plot(), width = 10, height = 6, units="cm", dpi=300)


Figure6 <-ggarrange(mgPOM_tmeanppt_b, mgMAOM_3way,propMAOM_3way,nrow=2, common.legend=T, legend="right", labels=c("a","b", "c"))
Figure6
ggsave("Figure4_6.jpeg", width = 20, height = 6, units="cm", dpi=300)
 



# Adjust legend size for each individual plot
mgPOM_tmeanppt <- mgPOM_tmeanppt_b +
  theme(
    legend.text = element_text(size = 6),     # Smaller legend text
    legend.title = element_text(size = 7)     # Smaller legend title
  )

mgMAOM_3way <- mgMAOM_3way +
  theme(
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7)
  )

propMAOM_3way <- propMAOM_3way +
  theme(
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7)
  )

# Adjust legend size for each individual plot
mgPOM_tmeanppt <- mgPOM_tmeanppt +
  theme(
    legend.text = element_text(size = 5),     # Smaller legend text
    legend.title = element_text(size = 6)     # Smaller legend title
  )

mgMAOM_3way <- mgMAOM_3way +
  theme(
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 6)
  )

propMAOM_3way <- propMAOM_3way +
  theme(
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 6)
  )

# Adjust the legend size for each individual plot
mgPOM_tmeanppt <- mgPOM_tmeanppt +
  theme(
    legend.text = element_text(size = 8),  # Increase legend text size
    legend.title = element_text(size = 10)   # Increase legend title size
  )

mgMAOM_3way <- mgMAOM_3way +
  theme(legend.position = "none")  # Remove individual legends

propMAOM_3way <- propMAOM_3way +
  theme(legend.position = "none")  # Remove individual legends

# Create the top row with the plot "a" and the legend next to it
top_with_legend <- ggarrange(
  mgPOM_tmeanppt,                          # Top plot "a"
  ggplot() + theme_void(),                 # Empty space for the legend
  ncol = 2,                                 # Two columns: plot and legend
  widths = c(1.5, 0.4)                     # Make "a" wider
)

# Arrange the entire figure with "a" (top row) and "b" and "c" below
Figure3_submit <- ggarrange(
  top_with_legend,                          # Top plot "a" with legend
  ggarrange(mgMAOM_3way, propMAOM_3way,    # Bottom row with "b" and "c"
            ncol = 2,
            labels = c("b", "c"),
            label.x = 0.05,                # Adjust label x position for "b" and "c"
            label.y = 1.1),                 # Raise label y position
  nrow = 2,                                 # Arrange in 2 rows
  labels = "a",                             # Label the top plot "a"
  label.x = 0.05,                          # Adjust label x position for 'a'
  label.y = 1.05                            # Raise the label position for 'a'
)

# Center the legend above graph "c"
Figure3_submit <- ggarrange(
  Figure3_submit,
  nrow = 2,
  heights = c(0.9, 0.1)  # Adjust height allocation
)
Figure3_submit


# Save the arranged figure with updated legend position
ggsave("FinalFigures/Figure3.jpeg", plot = Figure6_v16, width = 20, height = 12, units = "cm", dpi = 600)


##############################################################################
# Create the top row with the plot "a" and the legend next to it
top_with_legend <- ggarrange(
  mgPOM_tmeanppt,                          # Top plot "a"
  ggplot() + theme_void(),                 # Empty space for the legend
  ncol = 2,                                # Two columns: plot and legend
  widths = c(1.5, 0.4)                     # Make "a" wider
)

# Arrange the entire figure with "a" (top row) and "b" and "c" below
Figure6_v17 <- ggarrange(
  top_with_legend,                         # Top plot "a" with legend
  ggarrange(mgMAOM_3way, propMAOM_3way,    # Bottom row with "b" and "c"
            ncol = 2,
            labels = c("b", "c"),
            label.x = 0.05,                # Adjust label x position for "b" and "c"
            label.y = 1.1),                # Raise label y position
  nrow = 2,                                # Arrange in 2 rows
  labels = "a",                            # Label the top plot "a"
  label.x = 0.05,                          # Adjust label x position for 'a'
  label.y = 1.15                            # Raise the label position further (if needed)
)

# Add margin to shift the entire plot down (increase top margin)
Figure6_v17 <- Figure6_v17 + 
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10))  # Add more margin at the top (t = top)

# Save the final figure
ggsave("Figure6_v17.jpeg", plot = Figure6_v17, width = 20, height = 13, units = "cm", dpi = 300)
#############################################
# Create the top row with the plot "a" and the legend next to it
top_with_legend <- ggarrange(
  mgPOM_tmeanppt,                          # Top plot "a"
  ggplot() + theme_void(),                 # Empty space for the legend
  ncol = 2,                                # Two columns: plot and legend
  widths = c(1.5, 0.4)                     # Increase width for the legend space
)

# Adjust the bottom row with "b" and "c", ensuring alignment
bottom_with_labels <- ggarrange(
  mgMAOM_3way, propMAOM_3way,              # Bottom row with "b" and "c"
  ncol = 2,
  labels = c("b", "c"),
  label.x = 0.05,                          # Adjust label x position for "b" and "c"
  label.y = 1.1                            # Raise label y position
)

# Arrange the entire figure with "a" (top row) and "b" and "c" below, ensuring alignment
Figure3_18 <- ggarrange(
  top_with_legend,                         # Top plot "a" with legend
  bottom_with_labels,                      # Bottom row with "b" and "c"
  nrow = 2,                                # Arrange in 2 rows
  labels = "a",                            # Label the top plot "a"
  label.x = 0.01,                          # Adjust label x position for 'a'
  label.y = 1.1                            # Raise the label position further (increase if necessary)
)

# Add margin to shift the entire plot down (increase top margin)
Figure3_18 <- Figure3_18 + 
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10))  # Add more margin at the top (t = top)

# Move the legend further to the right
Figure3_18 <- Figure3_18 + 
  theme(
    legend.position = c(1.9, 0.9),         # Move the legend to the top-right of the plot area
    legend.justification = c(1, 1),       # Adjusts the anchor point of the legend (aligns it at the top-right)
    legend.background = element_rect(fill = "white", color = "black")  # Optional: Add background to the legend
  )

Figure3_18

# Save the final figure with updated layout and legend
ggsave("FinalFigures/Figure3.jpeg", plot = Figure3_18, width = 20, height = 13, units = "cm", dpi = 600)
ggsave("FinalFigures/Figure3.pdf", plot = Figure3_18, width = 20, height = 13, units = "cm", dpi = 600)
