rm(list=ls())
setwd("/Users/dennisperrett/Documents/Uni/Semester 5/Research Seminar/simulation_study_DSEM")

library(mvtnorm)
library(R2jags)
Sys.setenv(LANG = "en")

source("gen_data_version02.R")
##########################
phi0 <- diag(1)*.7+.3 # cov(eta) # Per latent factor
mu0  <- c(0)          # mean(eta). # Per Observed(?) factor
ar0  <- c(.5)         # ar(1) structure # Per Latent Factor

# Factor loadings must conform with means and td (the conditional variance of the items)
ly0  <- matrix(c(1,1,2),3,1,byrow=F) # factor loadings
td   <- diag(3)*.25 # cond. var(y) -> res var
##########################

#set core specific seed
set.seed(1234)

N <- 20 # Number of people
Nt <- 50 # Number of time points
J <- dim(ly0)[1]

length(mu0)

dat1 <- gendata02(N,Nt,phi0,mu0,ar0,ly0,td)
head(dat1)

dim(dat1)
data <- list(
  T = Nt,
  N = N,
  J = J,
  observed_data = dat1
)

# Initial values
initial_values <- list(
  list(alpha = matrix(rnorm(N*J,0,1),N,J, byrow=T), ly = rnorm(J,0,1), ar = rnorm(N,0,2)),
  list(alpha = matrix(rnorm(N*J,0,3),N,J, byrow=T), ly = rnorm(J,0,3), ar = rnorm(N,0,1))
)

params <- c("alpha","ly","ar")

#Note: Check working directory to find the model.file.
fit.fa <- jags(data,  parameters.to.save=params, model.file="model1.txt", n.chains=7, n.iter=8000,
               n.burnin = 100, n.thin=15)

fit.fa





mean(fit.fa$BUGSoutput$sims.list$alpha)
mean(fit.fa$BUGSoutput$sims.list$ar)

mean(fit.fa$BUGSoutput$mean$ar)
var(fit.fa$BUGSoutput$mean$ar)
mean(fit.fa$BUGSoutput$mean$alpha)



