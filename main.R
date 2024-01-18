setwd("/Users/dennisperrett/Documents/Uni/Semester 5/Research Seminar/Google Simulation Study")

library(mvtnorm)
library(R2jags)
Sys.setenv(LANG = "en")

source("gen_data_version02.R")
##########################
phi0 <- diag(1)*.7+.3 # cov(eta) # Per latent factor
mu0  <- c(0)          # mean(eta). # Per latent factor
ar0  <- c(.5)         # ar(1) structure # Per Latent Factor

# Factor loadings must conform with means and td (the conditional variance of the items)
ly0  <- matrix(c(1,1,2),3,1,byrow=F) # factor loadings
td   <- diag(3)*.25 # cond. var(y) -> res var
##########################

#set core specific seed
set.seed(1234)

N <- 50 # Number of people
Nt <- 100 # Number of time points
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
initial_values <- list(list(
  alpha = matrix(rep(0,N*J),N,J, byrow=T),
  ly = rep(0,J),
  ar = rep(0,N)

))

params <- c("alpha","ly","ar")

#Note: Check working directory to find the model.file.
fit.fa <- jags(data, inits=initial_values, params, model.file="model1.txt", n.chains=1, n.iter=45000,
               n.burnin = 44000, n.thin=1)

mean(fit.fa$BUGSoutput$sims.list$alpha)
mean(fit.fa$BUGSoutput$sims.list$ar)

mean(fit.fa$BUGSoutput$mean$ar)
var(fit.fa$BUGSoutput$mean$ar)
mean(fit.fa$BUGSoutput$mean$alpha)

fit.fa

