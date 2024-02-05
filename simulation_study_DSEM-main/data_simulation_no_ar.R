rm(list=ls())
setwd("/Users/philippholscher/Downloads/simulation_study_DSEM-main 8")
#setwd("/Users/dennisperrett/Documents/Uni/Semester 5/Research Seminar/simulation_study_DSEM")

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

datmn <- mysterious.data(N=50,Nt=100)
data <- list(N = dim(datmn$Y)[1],
             NT = dim(datmn$Y)[2],
             X = datmn$X,
             Y = datmn$Y,
             W = datmn$W)

# Test model fit etc
fit.mn <- jags(data,  parameters.to.save=params, model.file="models/Model_PHILIPP_CUSTOM_PRIORS_EXACT.txt", n.chains=2, n.iter=5000,
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
set.seed(1234)
datmysterious <- mysterious.data(N=25,Nt=100)
datmysterious

# Decide on priors based on data
# Once, quickly (with the understanding you have right now!)
# Then do it again, using the paper as a reference until you've cracked it!

# Be aware, I had to increase the iterations from 2000 for convergence.
# Expect a run through of 5 "run.models" to take between 4 to 6 hours

# Lazy Priors
run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS.txt", data.gen.fn = mysterious.data ,N=10,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS.txt", data.gen.fn = mysterious.data ,N=20,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS.txt", data.gen.fn = mysterious.data ,N=30,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS.txt", data.gen.fn = mysterious.data ,N=40,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS.txt", data.gen.fn = mysterious.data ,N=50,NT=100)

# Exact priors!
run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS_EXACT.txt", data.gen.fn = mysterious.data ,N=10,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS_EXACT.txt", data.gen.fn = mysterious.data ,N=20,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS_EXACT.txt", data.gen.fn = mysterious.data ,N=30,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS_EXACT.txt", data.gen.fn = mysterious.data ,N=40,NT=100)
run.models(reps=200, model.file = "models/Model_PHILIPP_CUSTOM_PRIORS_EXACT.txt", data.gen.fn = mysterious.data ,N=50,NT=100)

# Once you get here, refer to analysis2.R for further analysis (plots etc)

# load irr for easy ICC
library(irr)

#making first guesses

## Intercept Variance


# VAR over first outcome observation
var_y = var(datmysterious[["Y"]][,1])

# ICC over our outcome
icc_y = as.numeric(icc(datmysterious[["Y"]])['value'])

#intercept variance estimate as product of var(y[1,]) and ICC(y)
intercept_variance = var_y * icc_y


## Interia Slope stays the same as we have similar assumptions


## Time-varying covariate slope variance


var_w = var(datmysterious[['W']][,1])
var_w_2 = var(datmysterious[['W']][,2])
# w variance around .70
# y total variance 734
#it would seem unlikely that the magnitude of a person-
# specific slope would exceed ~ 670 (3/3.25(ratio from paper)*734)

var_tvc = (0.5*670)^2 #variance we are aming for

#Log-residual random effect variance

#In the unconditional model, the intraclass correlation suggests that the average
# residual variance would be ~ 587.2 (0.80 × 734.01)

var_log_low = log(587.2)
var_log_up = log(5872)

#That is, if we are expecting the fixed effect for the log-residual variance to be .96 (from the descriptive statistics)
#-> standard deviation of 3.8 to match var_log_up
#-> see HDI tabel




# translating guesses to prior with HDI estimation

library(invgamma)
library(HDInterval)


#Intercept variance
int <- function(x)
qinvgamma(x,shape=0.665,rate=0.1665)
hdi(int)
# inverse gamma with (0.665,0.1665) as first prior

#TVC variance
tvc <- function(x)
qinvgamma(x,shape= 0.225, rate=0.1225)
hdi(tvc)
#inverse gamma with (0.225,0.1225)


#log residual variance
tvc <- function(x)
qinvgamma(x,shape=1,rate=.20)
hdi(tvc)
#inverse gamma with (1,0.2)


