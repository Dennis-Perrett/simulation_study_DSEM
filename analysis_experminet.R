# --------------------------------------------------------------------------- #
# Analysis of results of the simulation study. Requires running of
# data_simulation_no_ar.R (may get renamed!)
# --------------------------------------------------------------------------- #
setwd("/Users/philippholscher/Downloads/simulation_study_DSEM-main")
#setwd("/Users/dennisperrett/Documents/Uni/Semester 5/Research Seminar/simulation_study_DSEM")
rm(list=ls())
source("utils/utils.R")
library(ggplot2)
library(tidyverse)
#install.packages('latex2exp')
library(latex2exp)
library(gridExtra)

# Store true population values used in the simulation (may need to check these)
pop.vals <- list(
  phi_on_W1 = 0.10,
  phi_on_W2 = 0.05,
  beta_on_W1 = 0.30,
  beta_on_W2 = 0.20,
  lnV_on_W1 = 3,
  lnV_on_W2 = 7.5,
  alpha_on_W1 = 5,
  alpha_on_W2 = 3,
  int_beta = 0.2,
  int_phi= 0.2,
  alpha_var = 4,
  beta_var = 0.05,
  phi_var = 0.01,
  ln_var_var = 4
)

# Calculate the non-null detection rate. Requires the use of the actual results
# Method is visible in the utils file. Ultimately, it takes in the 2.5 and 97.5
# plausible interval ranges and checks to see if 0 is between these.
(nndr_philipp_IG_10 <- get.null.detection.rate("results/Model_PHILIPP_CUSTOM_PRIORS_EXACT/results_10_100.rds"))
(nndr_philipp_DD_10 <- get.null.detection.rate("results/Model_PHILIPP_DD/results_10_100.rds"))
(nndr_philipp_IG_20 <- get.null.detection.rate("results/Model_PHILIPP_CUSTOM_PRIORS_EXACT/results_20_100.rds"))
(nndr_philipp_DD_20 <- get.null.detection.rate("results/Model_PHILIPP_DD/results_20_100.rds"))
(nndr_philipp_IG_30 <- get.null.detection.rate("results/Model_PHILIPP_CUSTOM_PRIORS_EXACT/results_30_100.rds"))
(nndr_philipp_DD_30 <- get.null.detection.rate("results/Model_PHILIPP_DD/results_30_100.rds"))
(nndr_philipp_IG_40 <- get.null.detection.rate("results/Model_PHILIPP_CUSTOM_PRIORS_EXACT/results_40_100.rds"))
(nndr_philipp_DD_40 <- get.null.detection.rate("results/Model_PHILIPP_DD/results_40_100.rds"))
(nndr_philipp_IG_50 <- get.null.detection.rate("results/Model_PHILIPP_CUSTOM_PRIORS_EXACT/results_50_100.rds"))
(nndr_philipp_DD_50 <- get.null.detection.rate("results/Model_PHILIPP_DD/results_50_100.rds"))

# Store these values in a table
nndr.table <- cbind(
  nndr_philipp_IG_10,
  nndr_philipp_DD_10,
  nndr_philipp_IG_20,
  nndr_philipp_DD_20,
  nndr_philipp_IG_30,
  nndr_philipp_DD_30,
  nndr_philipp_IG_40,
  nndr_philipp_DD_40,
  nndr_philipp_IG_50,
  nndr_philipp_DD_50
)
# round and transform the rates into integers (for readability). Display results
round(nndr.table[c("phi_on_W1",
                   "phi_on_W2",
                   "beta_on_W1",
                   "beta_on_W2",
                   "lnV_on_W1",
                   "lnV_on_W2",
                   "alpha_on_W1",
                   "alpha_on_W2"),]*100)
# Note: ARR = Admissible-Range-Restricted, DD = Default Diffuse. Cell values
# represent percentages. The variance terms are not included because their
# definition is not consistent with the aim of the non-null detection rate.
# Variances are bounded below by 0, so their credible intervals necessarily do
# not include 0, and the non-null rate would uniformly be 100.

# Only needs to be run after fitting models.
# Model fit simulations save an RDS file with the results. One RDS file for each
# combination of N and T. The below reads these results into memory, extracts
# the means and saves these as separate RDS files for efficiency and storage.
{
  # Extract the means
  #mean_10_100_philipp_DD <- extract.params3("results/Model_PHILIPP_DD/results_10_100.rds","mean")
  #mean_20_100_philipp_DD <- extract.params3("results/Model_PHILIPP_DD/results_20_100.rds","mean")
  #mean_30_100_philipp_DD <- extract.params3("results/Model_PHILIPP_DD/results_30_100.rds","mean")
  #mean_40_100_philipp_DD <- extract.params3("results/Model_PHILIPP_DD/results_40_100.rds","mean")
  #mean_50_100_philipp_DD <- extract.params3("results/Model_PHILIPP_DD/results_50_100.rds","mean")

  #mean_10_100_philipp_IG <- extract.params3("results/Model_PHILIPP_CUSTOM_PRIORS_EXACT/results_10_100.rds","mean")
  #mean_20_100_philipp_IG <- extract.params3("results/Model_PHILIPP_CUSTOM_PRIORS_EXACT/results_20_100.rds","mean")
  #mean_30_100_philipp_IG <- extract.params3("results/Model_PHILIPP_CUSTOM_PRIORS_EXACT/results_30_100.rds","mean")
  #mean_40_100_philipp_IG <- extract.params3("results/Model_PHILIPP_CUSTOM_PRIORS_EXACT/results_40_100.rds","mean")
  #mean_50_100_philipp_IG <- extract.params3("results/Model_PHILIPP_CUSTOM_PRIORS_EXACT/results_50_100.rds","mean")


  #saveRDS(mean_10_100_philipp_DD, "simulation_means/mean_10_100_philipp_DD.rds")
  #saveRDS(mean_20_100_philipp_DD, "simulation_means/mean_20_100_philipp_DD.rds")
  #saveRDS(mean_30_100_philipp_DD, "simulation_means/mean_30_100_philipp_DD.rds")
  #saveRDS(mean_40_100_philipp_DD, "simulation_means/mean_40_100_philipp_DD.rds")
  #saveRDS(mean_50_100_philipp_DD, "simulation_means/mean_50_100_philipp_DD.rds")

  #saveRDS(mean_10_100_philipp_IG, "simulation_means/mean_10_100_philipp_IG.rds")
  #saveRDS(mean_20_100_philipp_IG, "simulation_means/mean_20_100_philipp_IG.rds")
  #saveRDS(mean_30_100_philipp_IG, "simulation_means/mean_30_100_philipp_IG.rds")
  #saveRDS(mean_40_100_philipp_IG, "simulation_means/mean_40_100_philipp_IG.rds")
  #saveRDS(mean_50_100_philipp_IG, "simulation_means/mean_50_100_philipp_IG.rds")
}

# Read in the means
mean_10_100_philipp_DD <- readRDS("simulation_means/mean_10_100_philipp_DD.rds")
mean_20_100_philipp_DD <- readRDS("simulation_means/mean_20_100_philipp_DD.rds")
mean_30_100_philipp_DD <- readRDS("simulation_means/mean_30_100_philipp_DD.rds")
mean_40_100_philipp_DD <- readRDS("simulation_means/mean_40_100_philipp_DD.rds")
mean_50_100_philipp_DD <- readRDS("simulation_means/mean_50_100_philipp_DD.rds")

mean_10_100_philipp_IG <- readRDS("simulation_means/mean_10_100_philipp_IG.rds")
mean_20_100_philipp_IG <- readRDS("simulation_means/mean_20_100_philipp_IG.rds")
mean_30_100_philipp_IG <- readRDS("simulation_means/mean_30_100_philipp_IG.rds")
mean_40_100_philipp_IG <- readRDS("simulation_means/mean_40_100_philipp_IG.rds")
mean_50_100_philipp_IG <- readRDS("simulation_means/mean_50_100_philipp_IG.rds")



# Create a list of each of mean result. Used below for getting relative bias.
list.IG.means <- list(
  mean_10_100_philipp_IG,
  mean_20_100_philipp_IG,
  mean_30_100_philipp_IG,
  mean_40_100_philipp_IG,
  mean_50_100_philipp_IG
)
# Same as above, but for the default diffuse (DD) prior
list.DD.means <- list(
  mean_10_100_philipp_DD,
  mean_20_100_philipp_DD,
  mean_30_100_philipp_DD,
  mean_40_100_philipp_DD,
  mean_50_100_philipp_DD
)

# Calculate the relative biases. Function can be found in utils.R
# Alpha_var
(rel.bias.DD.alpha_var <- extract.rel.bias.from.list.of.means(list.DD.means, "alpha_var"))
(rel.bias.IG.alpha_var <- extract.rel.bias.from.list.of.means(list.IG.means, "alpha_var"))
# Beta_var
(rel.bias.DD.beta_var <- extract.rel.bias.from.list.of.means(list.DD.means, "beta_var"))
(rel.bias.IG.beta_var <- extract.rel.bias.from.list.of.means(list.IG.means, "beta_var"))
# Phi_var
(rel.bias.DD.phi_var <- extract.rel.bias.from.list.of.means(list.DD.means, "phi_var"))
(rel.bias.IG.phi_var <- extract.rel.bias.from.list.of.means(list.IG.means, "phi_var"))
# ln_var_var
(rel.bias.DD.lnV_var <- extract.rel.bias.from.list.of.means(list.DD.means, "ln_var_var"))
(rel.bias.IG.lnV_var <- extract.rel.bias.from.list.of.means(list.IG.means, "ln_var_var"))

# Help function as ggplot works with dataframes.
df.alpha <- create.data.frame.from.results(rel.bias.IG.alpha_var, rel.bias.DD.alpha_var)
df.beta <- create.data.frame.from.results(rel.bias.IG.beta_var, rel.bias.DD.beta_var)
df.phi <- create.data.frame.from.results(rel.bias.IG.phi_var, rel.bias.DD.phi_var)
df.lnV <- create.data.frame.from.results(rel.bias.IG.lnV_var, rel.bias.DD.lnV_var)

# Create path plots for the relative biases
(p1 <- plot.RB.path(df.alpha,"Intercept"))
(p2 <- plot.RB.path(df.beta,"Intertia Slope"))
(p3 <- plot.RB.path(df.phi,"Time-Varying Covariate"))
(p4 <- plot.RB.path(df.lnV,"Residual"))

# Plot all 4 together in a grid for final figure
grid.arrange(p1, p2, p3, p4, ncol = 2)


