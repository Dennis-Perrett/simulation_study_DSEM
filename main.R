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

#################################
# Test run 
#################################
params <- c("ly","ar")

#Note: Check working directory to find the model.file.
fit.fa <- jags(data,  parameters.to.save=params, model.file="model1.txt", n.chains=7, n.iter=1000,
               n.burnin = 100, n.thin=15)

fit.fa

########## Proper Stuff ##########

N.List <- c(10,20,30,40,50)
T.List <- c(25, 50, 100)

# Create Storage Matrix
results <- matrix(vector("list", length = 15),nrow=length(N.List),ncol=length(T.List))
row.names(results) <- N.List
colnames(results) <- T.List
results

# Run sim loop
for (N.size in N.List){
  for (T.size in T.List){
    
    dat1 <- gendata02(N.size,T.size,phi0,mu0,ar0,ly0,td)
    
    data <- list(
      T = T.size,
      N = N.size,
      J = J,
      observed_data = dat1
    )
    
    results[[as.character(N.size), as.character(T.size)]] <- jags(data, parameters.to.save=params, model.file="model1.txt", n.chains=7, n.iter=4000,
                                                                            n.burnin = 2500, n.thin=15)
  }
}


results[["50","50"]]
results[["50","25"]]


# Save and load the file
save(results, file = "results.rda")
load("results.rda")

