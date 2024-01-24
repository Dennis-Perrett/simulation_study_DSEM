rm(list=ls())
setwd("/Users/philippholscher/Downloads/simulation_study_DSEM-main")

library(mvtnorm)
library(R2jags)
Sys.setenv(LANG = "en")

source("gen_data_version02.R")
source("utils.R")
##########################
phi0 <- diag(1)*.7+.3# cov(eta) # Per latent factor
mu0  <- c(0)          # mean(eta). # Mean of latent (deactivated)
ar0  <- c(0.5)
# ar(1) structure # Per Latent Factor

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
inits <- list(list(
  ar = 0.5,  # Initial value for AR coefficient
  ly = c(1,1,2)),list(
    ar = 0.5,  # Initial value for AR coefficient
    ly =c(1,1,2) ))
#################################
# Test run
#################################
params <- c("ly","ar")

#Note: Check working directory to find the model.file.
fit.fa <- jags(data,  parameters.to.save=params, model.file="model1.txt", n.chains=2, n.iter=3000,
               n.burnin = 500, n.thin=1,inits = inits)



fit.fa

########## Proper Stuff ##########

N.List <- c(50,40,30,20,10)
T.List <- c(25, 50, 100)





# Ns to run: 10, 20, 30, 40, 50.
# We need at least 100 iterations of each. Philipp starts at 50 and works down.
# Dennis starts at 10 and works up. When we have simmed enough, move on to the next N.
# If we have 100 of each, do it again until we have 200 of each.
# We may need a few more, if for some reason the N.Effs are low for some samples. In this case, we would just delete these.
run.models(reps=100, model.file = "model1.txt",N=50,NT=100)

check.status("results_model1")

# Check the saved results
#------ USE THIS TO CONVERT FROM RDA TO RDS ----- #
load("./results_model1/results_10_100.rda")
saveRDS(list.to.save.to,"./results_model1/results_10_100.rds")
# ----------------------------------------------- #


rm(list.to.save.to)
resu <- readRDS("test.rds")

sim1 <- list.to.save.to[1]

# Access rjags objects with double square brackets eg:
sim1[[1]]$BUGSoutput$mean


