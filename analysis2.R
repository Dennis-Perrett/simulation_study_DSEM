#setwd("/Users/philippholscher/Downloads/simulation_study_DSEM-main")
setwd("/Users/dennisperrett/Documents/Uni/Semester 5/Research Seminar/simulation_study_DSEM")
rm(list=ls())
source("utils/utils.R")
library(ggplot2)
library(tidyverse)
#install.packages('latex2exp')
library(latex2exp)

pop.vals <- list(
  phi_on_W1 = 0.10,
  phi_on_W2 = 0.05,
  beta_on_W1 = 0.30,
  beta_on_W2 = 0.40,
  lvar_on_W1 = 0.30,
  lvar_on_W2 = 0.10,
  alpha_on_W1 = 0.50,
  alpha_on_W2 = 0.30,
  int_beta = 0.7,
  int_phi= 0.2,
  var_alpha = 0.3,
  var_beta = 0.5,
  var_phi = 0.01,
  var_lvar = 0.1
  
)



# Extract the means
med_10_50 <- extract.params3("results/model_MN/results_10_50.rds","mean")
med_20_50 <- extract.params3("results/model_MN/results_20_50.rds","mean")
med_30_50 <- extract.params3("results/model_MN/results_30_50.rds","mean")
med_40_50 <- extract.params3("results/model_MN/results_40_50.rds","mean")
med_50_50 <- extract.params3("results/model_MN/results_50_50.rds","mean")


saveRDS(med_10_50, "simulation_means/med_10_50_MN_uninf.rds")
saveRDS(med_20_50, "simulation_means/med_20_50_MN_uninf.rds")
saveRDS(med_30_50, "simulation_means/med_30_50_MN_uninf.rds")
saveRDS(med_40_50, "simulation_means/med_40_50_MN_uninf.rds")
saveRDS(med_50_50, "simulation_means/med_50_50_MN_uninf.rds")

# Read in the means
med_10_50 <- readRDS("simulation_means/med_10_50_MN_uninf.rds")
med_20_50 <- readRDS("simulation_means/med_20_50_MN_uninf.rds")
med_30_50 <- readRDS("simulation_means/med_30_50_MN_uninf.rds")
med_40_50 <- readRDS("simulation_means/med_40_50_MN_uninf.rds")
med_50_50 <- readRDS("simulation_means/med_50_50_MN_uninf.rds")


# Remove non-converged samples / outliers

res <- readRDS("results/model_MN/results_50_50.rds")
res[[1]]
# Calculate relative bias
(rel.bias_mu_10 <- mean(med_10_50$beta_var / pop.vals$var_beta))
(rel.bias_mu_20 <- mean(med_20_50$beta_var / pop.vals$var_beta))
(rel.bias_mu_30 <- mean(med_30_50$beta_var / pop.vals$var_beta))
(rel.bias_mu_40 <- mean(med_40_50$beta_var / pop.vals$var_beta))
(rel.bias_mu_50 <- mean(med_50_50$beta_var / pop.vals$var_beta))

(rel.bias_sig_10 <- mean(means_10_50$sigma / sigma))
(rel.bias_sig_20 <- mean(means_20_50$sigma / sigma))
(rel.bias_sig_30 <- mean(means_30_50$sigma / sigma))
(rel.bias_sig_40 <- mean(means_40_50$sigma / sigma))
(rel.bias_sig_50 <- mean(means_50_50$sigma / sigma))


df <- data.frame(N10 = means_10_100$sigma / sigma,
           N20 = means_20_50$sigma / sigma,
           N30 = means_30_50$sigma / sigma,
           N40 = means_40_50$sigma / sigma,
           N50 = means_50_50$sigma / sigma
           )

df.mu <- data.frame(N10 = means_10_100$mu / mu,
                 N20 = means_20_50$mu / mu,
                 N30 = means_30_50$mu / mu,
                 N40 = means_40_50$mu / mu,
                 N50 = means_50_50$mu / mu
)


# Warning here isn't an issue
long_data <- pivot_longer(df,cols = starts_with("N"))
long_data <- long_data %>%rename("N Size" = name,"Sigma" = value)
long_data.mu <- pivot_longer(df.mu,cols = starts_with("N"))
long_data.mu <- long_data.mu %>%rename("N Size" = name,"Mu" = value)

long_data$`N Size` <- as.factor(long_data$`N Size`)
long_data$Mu <- long_data.mu$Mu
rm(long_data.mu)
rm(df.mu)
# Plot distributions of relative biases
p.sigma <- ggplot(long_data, aes(x=`N Size`, y=`Sigma`)) + 
  geom_violin(
    fill = alpha("lightblue",.5),
    size=0.3
  ) + 
  geom_boxplot(
    width = 0.1,  # Adjust the width of the boxplot
    fill = alpha("white",.8),  # Adjust the fill color of the boxplot
    size=0.3
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size=0.4) +
  theme_linedraw() +
  ggtitle(
    label="Relative Bias of Sigma per Sample Size (N)",
    subtitle=TeX('Relative Bias $=\\left(\\hat{\\theta}/\\theta\\right)$, where 1 is optimal (indicated below with dashed black line).')
          ) +
  ylab("Relative Bias") +
  theme(
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size=8)
  )
p.sigma
p.mu <- ggplot(long_data, aes(x=`N Size`, y=`Mu`)) + 
  geom_violin(
    fill = alpha("lightblue",.5),
    size=0.3
  ) + 
  geom_boxplot(
    width = 0.1,  # Adjust the width of the boxplot
    fill = alpha("white",.8),  # Adjust the fill color of the boxplot
    size=0.3
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size=0.4) +
  theme_linedraw() +
  ggtitle(
    label="Relative Bias of Mu per Sample Size (N)",
    subtitle=TeX('Relative Bias $=\\left(\\hat{\\theta}/\\theta\\right)$, where 1 is optimal (indicated below with dashed black line).')
  ) +
  ylab("Relative Bias") +
  theme(
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size=8)
  )

p.mu

averages <- long_data %>%
  group_by(`N Size`) %>%
  summarize(ARBSigma = mean(c(Sigma), na.rm = TRUE),
            ARBMu = mean(c(Mu), na.rm = TRUE))
averages
# Plot Relative Bias Paths


plot(averages$ARBSigma,type="l")


# Assuming long_data is your tibble with columns 'X' and 'Y'
p <- ggplot(averages) +
  geom_line(aes(x = 1:5 * 10, y = ARBSigma), color=alpha("black", 0.4)) + 
  stat_smooth(aes(x = 1:5 * 10, y=ARBSigma), method = "loess", formula = y ~ x, span=1, se=F, color=alpha('lightblue',1)) +
  theme_linedraw() +
  labs(title = "Average Relative Bias Trajectory (Sigma)", x = "Level 2 Sample Size", y = "Relative Bias") + 
  geom_hline(yintercept = c(1.1,.9), linetype = "dashed", color = "black", size=0.4)

# Print the plot
p

p.mu <- ggplot(averages) +
  geom_line(aes(x = 1:5 * 10, y = ARBMu), color=alpha("black", 0.4)) + 
  stat_smooth(aes(x = 1:5 * 10, y=ARBMu), method = "loess", formula = y ~ x, span=1, se=F, color=alpha('lightblue',1)) +
  theme_linedraw() +
  labs(title = "Average Relative Bias Trajectory (Mu)", x = "Level 2 Sample Size", y = "Relative Bias") + 
  geom_hline(yintercept = c(1.1,.9), linetype = "dashed", color = "black", size=0.4)

p.mu
