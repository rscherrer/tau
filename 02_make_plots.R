## ---------------------------
##
## Script name: 02_make_plots.R
##
## Purpose of script: Plot statistics against the tau parameter.
##
## How to use:
##
## Simply run it. The script loads the saved trees and produces a summary
## figure.
##
## Author: Raphael Scherrer
##
## Date Created: 2022-07-05
##
## This script comes with no guarantee whatsoever.
##
## Copyright (c) Raphael Scherrer, 2022
##
## Find me on GitHub at https://github.com/rscherrer
##
## Email:
## r.scherrer@rug.nl
## raphael.scherrer@evobio.eu
## raph.rjfs@hotmail.fr
##
## ---------------------------

rm(list = ls())

library(tidyverse)
library(patchwork)

theme_set(theme_classic())

# Load the simulated trees
data <- readRDS("trees.rds")

# Eyeball the range of tree sizes
range(data$size)

# Plot a histogram of tree sizes
plot1 <- data %>%
  ggplot(aes(x = size)) +
  geom_histogram(bins = 30) +
  xlab("Tree size") +
  ylab("Count")

# Plot Colless against tau
plot2 <- data %>%
  filter(size > 10) %>%
  ggplot(aes(x = tau, y = colless)) +
  geom_point(mapping = aes(size = size)) +
  geom_smooth(method = "loess") +
  xlab("Speciation duration (Myr)") +
  ylab("Colless index") +
  labs(size = "Tree size")

# Plot Sackin against tau
plot3 <- data %>%
  filter(size > 10) %>%
  ggplot(aes(x = tau, y = sackin)) +
  geom_point(mapping = aes(size = size)) +
  geom_smooth(method = "loess") +
  xlab("Speciation duration (Myr)") +
  ylab("Sackin index") +
  labs(size = "Tree size")

# Plot gamma against tau
plot4 <- data %>%
  filter(size > 10) %>%
  ggplot(aes(x = tau, y = gamma)) +
  geom_point(mapping = aes(size = size)) +
  geom_smooth(method = "loess") +
  xlab("Speciation duration (Myr)") +
  ylab("Gamma statistic") +
  labs(size = "Tree size")

# Combine the plots
plot <- wrap_plots(plot1, plot2, plot3, plot4, guides = "collect")

# Save
ggsave("summary_tau.png", width = 7, height = 5, dpi = 300)
