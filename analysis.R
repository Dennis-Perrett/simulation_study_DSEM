#setwd("/Users/philippholscher/Downloads/simulation_study_DSEM-main")
setwd("/Users/dennisperrett/Documents/Uni/Semester 5/Research Seminar/simulation_study_DSEM")

# Official factor loadings
ly0  <- matrix(c(1,.78,-2.23),3,1,byrow=F) # factor loadings
# Official AR Coefficient
ar0  <- c(0.96)       # ar(1) structure # Per Latent Factor


# Read in the means
means_10_100 <- readRDS("means_10_100.rds")
means_20_100 <- readRDS("means_20_100.rds")
means_30_100 <- readRDS("means_30_100.rds")


# Remove non-converged samples / outliers
means_10_100_ar <- means_10_100$ar[!(abs(means_10_100$ar-ar0))>0.1]
means_20_100_ar <- means_20_100$ar[!(abs(means_20_100$ar-ar0))>0.1]
means_30_100_ar <- means_30_100$ar[!(abs(means_30_100$ar-ar0))>0.1]


means_10_100_ly <- means_10_100$ly[,!colMeans(abs(means_10_100$ly - rep(ly0,200))>0.1)>0]
means_20_100_ly <- means_20_100$ly[,!colMeans(abs(means_20_100$ly - rep(ly0,200))>0.1)>0]
means_30_100_ly <- means_30_100$ly[,!colMeans(abs(means_30_100$ly - rep(ly0,200))>0.1)>0]

# Calculate relative bias
(rel.bias_ar_10 <- mean(means_10_100_ar / ar0))
(rel.bias_ar_20 <- mean(means_20_100_ar / ar0))
(rel.bias_ar_30 <- mean(means_30_100_ar / ar0))

(rel.bias_ly_10 <- rowMeans(means_10_100_ly / rep(ly0,dim(means_10_100_ly)[2])))
(rel.bias_ly_20 <- rowMeans(means_20_100_ly / rep(ly0,dim(means_20_100_ly)[2])))
(rel.bias_ly_30 <- rowMeans(means_30_100_ly / rep(ly0,dim(means_30_100_ly)[2])))

       