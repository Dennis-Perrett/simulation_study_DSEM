#setwd("/Users/philippholscher/Downloads/simulation_study_DSEM-main")
setwd("/Users/dennisperrett/Documents/Uni/Semester 5/Research Seminar/simulation_study_DSEM")
rm(list=ls())
source("utils/utils.R")
library(ggplot2)
library(tidyverse)
#install.packages('latex2exp')
library(latex2exp)
library(gridExtra)

pop.vals <- list(
  phi_on_W1 = 0.10,
  phi_on_W2 = 0.05,
  beta_on_W1 = 0.30,
  beta_on_W2 = 0.40,
  lnV_on_W1 = 0.30,
  lnV_on_W2 = 0.10,
  alpha_on_W1 = 0.50,
  alpha_on_W2 = 0.30,
  int_beta = 0.7,
  int_phi= 0.2,
  alpha_var = 0.3,
  beta_var = 0.5,
  phi_var = 0.01,
  ln_var_var = 0.1
)

get.null.detection.rate <- function(results.file){
  "Takes in a results file and returns the null null detection rates (as per McNeish)"
  res <- readRDS(results.file)
  a <- res[[1]]$BUGSoutput$summary[,c(3,7)]
  lo <- a[,1]
  up <- a[,2]
  output <- (lo <= 0  & 0 <= up)
  for (i in 2:length(res)){
    a <- res[[i]]$BUGSoutput$summary[,c(3,7)]
    lo <- a[,1]
    up <- a[,2]
    output <- Map(c,output,(lo <= 0  & 0 <= up))
  }
  
  return(1-sapply(output,mean))
}
(nndr_MN_IG_10 <- get.null.detection.rate("results/model_MN_IG/results_10_100.rds"))  
(nndr_MN_DD_10 <- get.null.detection.rate("results/model_MN/results_10_100.rds"))
(nndr_MN_IG_20 <- get.null.detection.rate("results/model_MN_IG/results_20_100.rds"))
(nndr_MN_DD_20 <- get.null.detection.rate("results/model_MN/results_20_100.rds"))
(nndr_MN_IG_30 <- get.null.detection.rate("results/model_MN_IG/results_30_100.rds"))
(nndr_MN_DD_30 <- get.null.detection.rate("results/model_MN/results_30_100.rds"))
(nndr_MN_IG_40 <- get.null.detection.rate("results/model_MN_IG/results_40_100.rds"))
(nndr_MN_DD_40 <- get.null.detection.rate("results/model_MN/results_40_100.rds"))
(nndr_MN_IG_50 <- get.null.detection.rate("results/model_MN_IG/results_50_100.rds"))
(nndr_MN_DD_50 <- get.null.detection.rate("results/model_MN/results_50_100.rds"))

nndr.table <- cbind(
  nndr_MN_IG_10,
  nndr_MN_DD_10,
  nndr_MN_IG_20,
  nndr_MN_DD_20,
  nndr_MN_IG_30,
  nndr_MN_DD_30,
  nndr_MN_IG_40,
  nndr_MN_DD_40,
  nndr_MN_IG_50,
  nndr_MN_DD_50
)

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



# Extract the means
mean_10_100_MN_DD <- extract.params3("results/model_MN/results_10_100.rds","mean")
mean_20_100_MN_DD <- extract.params3("results/model_MN/results_20_100.rds","mean")
mean_30_100_MN_DD <- extract.params3("results/model_MN/results_30_100.rds","mean")
mean_40_100_MN_DD <- extract.params3("results/model_MN/results_40_100.rds","mean")
mean_50_100_MN_DD <- extract.params3("results/model_MN/results_50_100.rds","mean")

mean_10_100_MN_IG <- extract.params3("results/model_MN_IG/results_10_100.rds","mean")
mean_20_100_MN_IG <- extract.params3("results/model_MN_IG/results_20_100.rds","mean")
mean_30_100_MN_IG <- extract.params3("results/model_MN_IG/results_30_100.rds","mean")
mean_40_100_MN_IG <- extract.params3("results/model_MN_IG/results_40_100.rds","mean")
mean_50_100_MN_IG <- extract.params3("results/model_MN_IG/results_50_100.rds","mean")


saveRDS(mean_10_100_MN_DD, "simulation_means/mean_10_100_MN_DD.rds")
saveRDS(mean_20_100_MN_DD, "simulation_means/mean_20_100_MN_DD.rds")
saveRDS(mean_30_100_MN_DD, "simulation_means/mean_30_100_MN_DD.rds")
saveRDS(mean_40_100_MN_DD, "simulation_means/mean_40_100_MN_DD.rds")
saveRDS(mean_50_100_MN_DD, "simulation_means/mean_50_100_MN_DD.rds")

saveRDS(mean_10_100_MN_IG, "simulation_means/mean_10_100_MN_IG.rds")
saveRDS(mean_20_100_MN_IG, "simulation_means/mean_20_100_MN_IG.rds")
saveRDS(mean_30_100_MN_IG, "simulation_means/mean_30_100_MN_IG.rds")
saveRDS(mean_40_100_MN_IG, "simulation_means/mean_40_100_MN_IG.rds")
saveRDS(mean_50_100_MN_IG, "simulation_means/mean_50_100_MN_IG.rds")

# Read in the means
mean_10_100_MN_DD <- readRDS("simulation_means/mean_10_100_MN_DD.rds")
mean_20_100_MN_DD <- readRDS("simulation_means/mean_20_100_MN_DD.rds")
mean_30_100_MN_DD <- readRDS("simulation_means/mean_30_100_MN_DD.rds")
mean_40_100_MN_DD <- readRDS("simulation_means/mean_40_100_MN_DD.rds")
mean_50_100_MN_DD <- readRDS("simulation_means/mean_50_100_MN_DD.rds")

mean_10_100_MN_IG <- readRDS("simulation_means/mean_10_100_MN_IG.rds")
mean_20_100_MN_IG <- readRDS("simulation_means/mean_20_100_MN_IG.rds")
mean_30_100_MN_IG <- readRDS("simulation_means/mean_30_100_MN_IG.rds")
mean_40_100_MN_IG <- readRDS("simulation_means/mean_40_100_MN_IG.rds")
mean_50_100_MN_IG <- readRDS("simulation_means/mean_50_100_MN_IG.rds")


# Remove non-converged samples / outliers

list.IG.means <- list(
  mean_10_100_MN_IG,
  mean_20_100_MN_IG,
  mean_30_100_MN_IG,
  mean_40_100_MN_IG,
  mean_50_100_MN_IG
)

list.DD.means <- list(
  mean_10_100_MN_DD,
  mean_20_100_MN_DD,
  mean_30_100_MN_DD,
  mean_40_100_MN_DD,
  mean_50_100_MN_DD
)


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




df.alpha <- create.data.frame.from.results(rel.bias.IG.alpha_var, rel.bias.DD.alpha_var)
df.beta <- create.data.frame.from.results(rel.bias.IG.beta_var, rel.bias.DD.beta_var)
df.phi <- create.data.frame.from.results(rel.bias.IG.phi_var, rel.bias.DD.phi_var)
df.lnV <- create.data.frame.from.results(rel.bias.IG.lnV_var, rel.bias.DD.lnV_var)


# Plot Relative Bias Paths

source("utils/utils.R")

# Print the plot

(p1 <- plot.RB.path(df.alpha,"Intercept"))
(p2 <- plot.RB.path(df.beta,"Intertia Slope"))
(p3 <- plot.RB.path(df.phi,"Time-Varying Covariate"))
(p4 <- plot.RB.path(df.lnV,"Residual"))

grid.arrange(p1, p2, p3, p4, ncol = 2)
