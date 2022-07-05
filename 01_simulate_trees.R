## ---------------------------
##
## Script name: 01_simulate_trees.R
##
## Purpose of script:
##
## Simulate PBD trees along a transect of tau values. The trees are saved into
## a .rds object at the end.
##
## How to use: Just run it (the seed is reset), but this may take a while.
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
library(PBD)
library(treestats)

set.seed(55)

# Pick the number of trees to simulate
n <- 1000

# Generate random values of tau (speciation duration in Myr)
tau <- runif(n, 0, 10)

# Pick constant, reasonable speciation and extinction rates
b1 <- b2 <- 0.1
mu1 <- mu2 <- 0

# Note: they are equal for good (#1) and incipient (#2) species

# Deduce speciation completion rate from speciation duration
lambda <- b2 / (exp(tau * b2) - 1)

# Note: this is after the fomula in Etienne and Rosindell 2012

# For each value of lambda...
data <- map2_dfr(lambda, tau, function(lambda, tau) {

  # Simulate a PBD tree
  tree <- pbd_sim(pars = c(b1, lambda, b2, mu1, mu2), age = 40)

  # Compute statistics on the reconstructed tree ("stree_random")
  sackin <- sackin(tree$stree_random, normalization = "yule")
  colless <- colless(tree$stree_random, normalization = "yule")
  gamma <- gamma_statistic(tree$stree_random)

  # How many extant species?
  size <- length(tree$stree_random$tip.label)

  # Assemble all the data in a tibble
  tibble(lambda, tau, sackin, colless, gamma, size, tree)

})

# Save the set of simulated trees
saveRDS(data, "trees.rds")
