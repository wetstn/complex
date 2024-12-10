install.packages("ggsave")
library(ggplot2)
library(forestplot)
library(dplyr)

setwd("C:/Users/westo/OneDrive/Desktop/Complex Closure") # whatever yours is

# Data
frame_comp_forest <- data.frame(
  lower = c(
    1.31, 0.79, 1.78,
    1.14, 0.17, 2.53,
    0.58, 0.09, 0.91,
    0.49, 0.27, 0.91,
    0.23, 0.75, 0.9),
  upper = c(
    5.09, 4.11, 13.9,
    22.5, 9.89, 88.7,
    26.5, 1.21, 7.84,
    6.74, 10.2, 5.55,
    3, 11.02, 4.48),
  estimate = c(
    2.55, 1.82, 5.01,
    4.46, 1.46, 13.9,
    4.69, 0.34, 2.56,
    1.86, 2.04, 2.21,
    0.91, 3.09, 1.96),
  variable = c(
    "20-29 cm", "30-39 cm", "40-69 cm",
    "20-29 cm", "30-39 cm", "40-69 cm",
    "History of MI", "History of prior surgery",
    "20-29 cm", "30-39 cm", "40-69 cm",
    "20-29 cm", "30-39 cm", "40-69 cm",
    "Female sex")
)

# Custom tick marks
custom_ticks <- c(0.1, 1, 2, 4, 8, 16, 32, 64)

# Forest plot as a PNG
png("forestplot.png", width = 800, height = 700, res = 150)
forestplot(
  labeltext = frame_comp_forest$variable,
  mean = frame_comp_forest$estimate,
  lower = frame_comp_forest$lower,
  upper = frame_comp_forest$upper,
  xlab = "Log Odds (95% Confidence Interval)",
  zero = 1, 
  xlog = TRUE, # Logarithmic scale for the x-axis
  lwd.zero = 2,
  boxsize = 0.20,
  ci.vertices = TRUE,
  ci.vertices.height = 0.1,
  lwd.xaxis = 2,
  lwd.ci = 2,
  col = fpColors(box = "black", line = "black"),
  cex = 0.8, # Text size for other elements
  clip = c(0.0001, 100), # Clip values align with the xticks
  axis.line = gpar(lwd = 2),
  title = "Any Wound Complication",
  xticks = log(custom_ticks)  
)
dev.off()
