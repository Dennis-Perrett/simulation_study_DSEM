rm(list=ls())
#setwd("/Users/philippholscher/Downloads/simulation_study_DSEM-main")
setwd("/Users/dennisperrett/Documents/Uni/Semester 5/Research Seminar/simulation_study_DSEM")

library(mvtnorm)
library(R2jags)
library(lattice)
Sys.setenv(LANG = "en")

source("utils/utils.R")
source("utils/genDataMcNeish.R")

datmn <- genDataMcNeish(N=25,Nt=100)
data <- list(N = dim(datmn$Y)[1],
             NT = dim(datmn$Y)[2],
             X = datmn$X,
             Y = datmn$Y,
             W = datmn$W)

pop.vals <- list(
  phi_on_W1 = 0.10,
  phi_on_W2 = 0.05,
  beta_on_W1 = 0.30,
  beta_on_W2 = 0.40,
  lnV_on_W1 = 0.30,
  lnV_on_W2 = 0.10,
  alpha_on_W1 = 0.50,
  alpha_on_W2 = 0.30,
  int_beta = 0.7,
  int_phi= 0.2,
  alpha_var = 0.3,
  beta_var = 0.5,
  phi_var = 0.01,
  ln_var_var = 0.1
)

inits <- list(
  pop.vals,
  pop.vals
)
params <- c('alpha_var',
            'beta_var',
            'phi_var',
            'ln_var_var',
            'alpha_on_W1',
            'alpha_on_W2',
            'beta_on_W1',
            'beta_on_W2',
            'phi_on_W1',
            'phi_on_W2',
            'lnV_on_W1',
            'lnV_on_W2',
            'int_beta',
            'int_phi')

fit.mn <- jags(data,  parameters.to.save=params, model.file="models/model_MN_IG.txt", n.chains=2, n.iter=2000,
               n.burnin = 500, n.thin=1)

fit.mn

# Average variance of Y
mean(apply(datmn$Y, MARGIN = 1, var))
check.fit(fit.mn, pop.vals)
source("utils/utils.R")

# Run McNeish Model # Uniform
#run.models(reps=200, model.file = "models/model_MN.txt",N=10,NT=100)
#run.models(reps=200, model.file = "models/model_MN.txt",N=20,NT=100)
#run.models(reps=200, model.file = "models/model_MN.txt",N=30,NT=100)
#run.models(reps=200, model.file = "models/model_MN.txt",N=40,NT=100)
#run.models(reps=200, model.file = "models/model_MN.txt",N=50,NT=100)

#run.models(reps=200, model.file = "models/model_MN_IG.txt",N=10,NT=100)
#run.models(reps=200, model.file = "models/model_MN_IG.txt",N=20,NT=100)
#run.models(reps=200, model.file = "models/model_MN_IG.txt",N=30,NT=100)
#run.models(reps=200, model.file = "models/model_MN_IG.txt",N=40,NT=100)
#run.models(reps=200, model.file = "models/model_MN_IG.txt",N=50,NT=100)

check.status("results/model_MN")


# PHILIPP START HERE #
source("mysteriousdata.R")

params <- c('alpha_var',
            'beta_var',
            'phi_var',
            'ln_var_var',
            'alpha_on_W1',
            'alpha_on_W2',
            'beta_on_W1',
            'beta_on_W2',
            'phi_on_W1',
            'phi_on_W2',
            'lnV_on_W1',
            'lnV_on_W2',
            'int_beta',
            'int_phi')

datmn <- mysterious.data(N=25,Nt=100)
data <- list(N = dim(datmn$Y)[1],
             NT = dim(datmn$Y)[2],
             X = datmn$X,
             Y = datmn$Y,
             W = datmn$W)

# Test model fit etc
fit.mn <- jags(data,  parameters.to.save=params, model.file="models/model_MN_IG.txt", n.chains=2, n.iter=2000,
               n.burnin = 500, n.thin=1)

fit.mn

# Run multiple iterations:
# Set number of reps, N, and NT.
# Update model.file as necessary
#
# Run this section only once!
run.models(reps=200, model.file = "models/Model_PHILIPP_DD.txt", data.gen.fn = mysterious.data ,N=10,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_DD.txt", data.gen.fn = mysterious.data ,N=20,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_DD.txt", data.gen.fn = mysterious.data ,N=30,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_DD.txt", data.gen.fn = mysterious.data ,N=40,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_DD.txt", data.gen.fn = mysterious.data ,N=50,NT=100)


# Use this to get / check the data to decide on priors
datmysterious <- mysterious.data(N=25,Nt=100)
datmysterious


run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS.txt", data.gen.fn = mysterious.data ,N=10,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS.txt", data.gen.fn = mysterious.data ,N=20,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS.txt", data.gen.fn = mysterious.data ,N=30,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS.txt", data.gen.fn = mysterious.data ,N=40,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS.txt", data.gen.fn = mysterious.data ,N=50,NT=100)

