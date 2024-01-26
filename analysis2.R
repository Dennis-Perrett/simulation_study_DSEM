#setwd("/Users/philippholscher/Downloads/simulation_study_DSEM-main")
setwd("/Users/dennisperrett/Documents/Uni/Semester 5/Research Seminar/simulation_study_DSEM")

source("utils/utils.R")
library(ggplot2)
library(tidyverse)
install.packages('latex2exp')
library(latex2exp)

# Official factor loadings
ly0  <- matrix(c(1,-1,-2),3,1,byrow=F) # factor loadings
# Official AR Coefficient
mu  <- c(0.2)       # ar(1) structure # Per Latent Factor
sigma <- c(0.5)




# Extract the means
means_10_100 <- extract.params2("results/model1_RE/results_10_100.rds")
means_20_100 <- extract.params2("results/model1_RE/results_20_100.rds")
means_30_100 <- extract.params2("results/model1_RE/results_30_100.rds")
means_40_100 <- extract.params2("results/model1_RE/results_40_100.rds")
#means_50_100 <- extract.params2("results/model1_RE/results_50_100.rds")


saveRDS(means_10_100, "simulation_means/means_10_100_RE_uninf.rds")
saveRDS(means_20_100, "simulation_means/means_20_100_RE_uninf.rds")
saveRDS(means_30_100, "simulation_means/means_30_100_RE_uninf.rds")
saveRDS(means_40_100, "simulation_means/means_40_100_RE_uninf.rds")
#saveRDS(means_10_100, "simulation_means/means_10_100_RE_uninf.rds")

# Read in the means
means_10_100 <- readRDS("simulation_means/means_10_100_RE_uninf.rds")
means_20_100 <- readRDS("simulation_means/means_20_100_RE_uninf.rds")
means_30_100 <- readRDS("simulation_means/means_30_100_RE_uninf.rds")
means_40_100 <- readRDS("simulation_means/means_40_100_RE_uninf.rds")

mean(means_40_100$sigma)
plot(means_40_100$sigma)
# Remove non-converged samples / outliers


# Calculate relative bias
(rel.bias_mu_10 <- mean(means_10_100$mu / mu))
(rel.bias_mu_20 <- mean(means_20_100$mu / mu))
(rel.bias_mu_30 <- mean(means_30_100$mu / mu))
(rel.bias_mu_40 <- mean(means_40_100$mu / mu))

(rel.bias_sig_10 <- mean(means_10_100$sigma / sigma))
(rel.bias_sig_20 <- mean(means_20_100$sigma / sigma))
(rel.bias_sig_30 <- mean(means_30_100$sigma / sigma))
(rel.bias_sig_40 <- mean(means_40_100$sigma / sigma))


df <- data.frame(N10 = means_10_100$sigma / sigma,
           N20 = means_20_100$sigma / sigma,
           N30 = means_30_100$sigma / sigma,
           N40 = means_40_100$sigma / sigma
           )

df.mu <- data.frame(N10 = means_10_100$mu / mu,
                 N20 = means_20_100$mu / mu,
                 N30 = means_30_100$mu / mu,
                 N40 = means_40_100$mu / mu
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




