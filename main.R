rm(list=ls())
setwd("/Users/dennisperrett/Documents/Uni/Semester 5/Research Seminar/simulation_study_DSEM")

library(mvtnorm)
library(R2jags)
Sys.setenv(LANG = "en")

source("gen_data_version02.R")
##########################
phi0 <- diag(1)*.7+.3 # cov(eta) # Per latent factor
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

#################################
# Test run
#################################
params <- c("ly","ar")

#Note: Check working directory to find the model.file.
fit.fa <- jags(data,  parameters.to.save=params, model.file="model1.txt", n.chains=2, n.iter=3000,
               n.burnin = 500, n.thin=1)


fit.fa

########## Proper Stuff ##########

N.List <- c(10,20,30,40,50)
T.List <- c(25, 50, 100)



# Define a function for running JAGS models with repetitions
run.models <- function(reps = 5, model.file, N, NT) {
  
  model_name <- sub("\\.txt$", "", model.file)
  # Subfolder to save results to
  dir <- file.path(paste0("results_",model_name)) 
  if (!dir.exists(dir)) dir.create(dir)
  
  # Define cache file and result file names
  CACHE.FILE <- "results_cache.rda"
  SAVE.FILE.NAME <- paste0("results_", as.character(N), "_", as.character(NT), ".rda")
  
  # Load results from cache if available
  if (file.exists(file.path(dir, SAVE.FILE.NAME))) {
    load(file.path(dir, SAVE.FILE.NAME))
  } else {
    list.to.save.to = list()
  }
  
  # Record the starting length of the result list
  start.length <- length(list.to.save.to)
  
  # Loop through the specified number of repetitions
  for (i in 1:reps) {
    print(paste0("Iteration: ", i, "/", reps, ". Total: ", start.length + i, "/", start.length + reps))
    
    # Generate data for the JAGS model
    dat1 <- gendata02(N, NT, phi0, mu0, ar0, ly0, td)
    
    # Prepare data for JAGS
    data <- list(
      T = NT,
      N = N,
      J = J,
      observed_data = dat1
    )
    
    # Run the JAGS model
    res <- jags(data, parameters.to.save = params, model.file = model.file, n.chains = 2, n.iter = 5000,
                n.burnin = 500, n.thin = 1)
    
    # Append the results to the list
    list.to.save.to <- c(list.to.save.to, list(res))
    
    # Save progress every 5 iterations
    if ((i %% 5) == 0) {
      save(list.to.save.to, file = CACHE.FILE)
    }
  }
  
  # Remove the cache file
  if (file.exists(CACHE.FILE)) {
    file.remove(CACHE.FILE)
  }
  
  # Save the final result list
  save(list.to.save.to, file = file.path(dir, SAVE.FILE.NAME))
  
}

# Ns to run: 10, 20, 30, 40, 50.
# We need at least 100 iterations of each. Philipp starts at 50 and works down.
# Dennis starts at 10 and works up. When we have simmed enough, move on to the next N.
# If we have 100 of each, do it again until we have 200 of each.
# We may need a few more, if for some reason the N.Effs are low for some samples. In this case, we would just delete these. 
run.models(reps=5, model.file = "model1.txt",N=10,NT=100)

# Check the saved results
load("./results/results_10_100.rda")
sim1 <- list.to.save.to[1]

# Access rjags objects with double square brackets eg:
sim1[[1]]$BUGSoutput$mean


