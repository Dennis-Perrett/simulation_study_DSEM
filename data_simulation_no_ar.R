rm(list=ls())
#setwd("/Users/philippholscher/Downloads/simulation_study_DSEM-main")
setwd("/Users/dennisperrett/Documents/Uni/Semester 5/Research Seminar/simulation_study_DSEM")

library(mvtnorm)
library(R2jags)
library(lattice)
Sys.setenv(LANG = "en")

source("utils/gen_data_version02.R")
source("utils/genData.R")
source("utils/utils.R")
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

N <- 30 # Number of people
Nt <- 50 # Number of time points
J <- dim(ly0)[1]

length(mu0)

dat1 <- gendata02(N,Nt,phi0,mu0,ar0,ly0,td)
dat1 <- genData(N,Nt)
head(dat1)

dim(dat1)
data <- list(
  T = Nt,
  N = N,
  J = J,
  observed_data = dat1
)
inits <- list(list(
  ar = rep(0,N),  # Initial value for AR coefficient
  ly = c(1,-.9,-2.2)),list(
    ar = rep(0.1,N),  # Initial value for AR coefficient
    ly =c(1,-1.1,-1.7) ))
#################################
# Test run
#################################
params <- c("ly","ar.var","ar.mean")


N <- 50 # Number of people
Nt <- 100 # Number of time points
J <- dim(ly0)[1]

source("genData.R")
dat1 <- genData(N,Nt,latent.ar.mean = c(0.2), latent.ar.var = diag(1)*0.5) # Example usage:
plot(dat1[1,,1],type="l")
lines(dat1[2,,1], col="red")
lines(dat1[3,,1], col="blue")

#Note: Check working directory to find the model.file.
data <- list(T = Nt,N = N,J = J,observed_data = dat1)
fit.fa <- jags(data,  parameters.to.save=params, model.file="model1_RE.txt", n.chains=2, n.iter=5000,
               n.burnin = 4500, inits = inits, n.thin=15)

fit.fa
traceplot(fit.fa)

ar0
ly0



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
run.models(reps=5, model.file = "models/model1_RE.txt",N=10,NT=10)

check.status("results/model1_RE")


# Extract means and save as rds
res <- readRDS("./results_model1_RE/results_10_100.rds")
res
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


