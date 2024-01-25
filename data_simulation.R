rm(list=ls())
#setwd("/Users/philippholscher/Downloads/simulation_study_DSEM-main")
setwd("/Users/dennisperrett/Documents/Uni/Semester 5/Research Seminar/simulation_study_DSEM")

library(mvtnorm)
library(R2jags)
library(lattice)
Sys.setenv(LANG = "en")

source("gen_data_version02.R")
source("genData.R")
source("utils.R")
##########################
phi0 <- diag(1)*15+0.0 # cov of latent variables # Per latent factor
mu0  <- c(4)          # mean of latent variables. # Mean of latent (deactivated)
ar0  <- c(0.96)       # ar(1) structure # Per Latent Factor

# Factor loadings must conform with means and td (the conditional variance of the items)
ly0  <- matrix(c(1,.78,-2.23),3,1,byrow=F) # factor loadings
td   <- diag(3)*.25 # cond. var(y) -> res var
##########################

#set core specific seed
set.seed(1234)

N <- 5 # Number of people
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
inits <- list(list(
  ar = 0.3,  # Initial value for AR coefficient
  ly = c(0.4,0.8,2.2)),list(
    ar = 0.7,  # Initial value for AR coefficient
    ly =c(1.5,1.1,-1.7) ))
#################################
# Test run
#################################
params <- c("ly","ar")

#Note: Check working directory to find the model.file.
fit.fa <- jags(data,  parameters.to.save=params, model.file="model1_unif.txt", n.chains=2, n.iter=4000,
               n.burnin = 500)



fit.fa

ar0
ly0

# Here we can see the relation between the loadings
person <- 5
{
  plot(dat1[person,,1],type="l", col="red", ylim=c(-20,20))
  lines(dat1[person,,2], col="blue")
  lines(dat1[person,,3], col="purple")
}

# From here we can create some semi-informative priors

########## Proper Stuff ##########

N.List <- c(50,40,30,20,10)
T.List <- c(25, 50, 100)


# Ns to run: 10, 20, 30, 40, 50.
# We need at least 100 iterations of each. Philipp starts at 50 and works down.
# Dennis starts at 10 and works up. When we have simmed enough, move on to the next N.
# If we have 100 of each, do it again until we have 200 of each.
# We may need a few more, if for some reason the N.Effs are low for some samples. In this case, we would just delete these.

#run.models(reps=100, model.file = "model1_unif.txt",N=20,NT=100)
run.models(reps=100, model.file = "model1_unif.txt",N=40,NT=100)

check.status("results_model1_unif")

# Extract means and save as rds
res <- readRDS("./results_model1_unif/results_30_100.rds")
means_30_100 <- extract.parameters(res)
saveRDS(means_30_100,"means_30_100.rds")




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


