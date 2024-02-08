rm(list=ls())
setwd("/Users/philippholscher/Downloads/simulation_study_DSEM-main 7")
#setwd("/Users/dennisperrett/Documents/Uni/Semester 5/Research Seminar/simulation_study_DSEM")




################################################################################
#################### PHILIP DO THIS SECTION TO FIND PRIORS #####################
################################################################################
source("utils/mysterious_vals.R")
source("mysteriousdata.R")
set.seed(1234)
datmn <- mysterious.data(N=50,Nt=200, phi_on_W1 = pop.vals$phi_on_W1,
                         phi_on_W2 = pop.vals$phi_on_W2,
                         beta_on_W1 = pop.vals$beta_on_W1,
                         beta_on_W2 = pop.vals$beta_on_W2,
                         lnV_on_W1 = pop.vals$lnV_on_W1,
                         lnV_on_W2 = pop.vals$lnV_on_W2,
                         alpha_on_W1 = pop.vals$alpha_on_W1,
                         alpha_on_W2 = pop.vals$alpha_on_W2,
                         int_beta = pop.vals$int_beta,
                         int_phi = pop.vals$int_phi,
                         alpha_var = pop.vals$alpha_var,
                         beta_var = pop.vals$beta_var,
                         phi_var = pop.vals$phi_var,
                         ln_var_var = pop.vals$ln_var_var)

datmn

######### Intercept
library(irr)
icc_y = as.numeric(icc(datmn[["Y"]])['value'])
estimated_variance_over_time=(var(datmn[['Y']][1,])+var(datmn[['Y']][2,])+var(datmn[['Y']][3,])+var(datmn[['Y']][4,])+var(datmn[['Y']][5,])+var(datmn[['Y']][6,])+var(datmn[['Y']][7,])+var(datmn[['Y']][8,])+var(datmn[['Y']][9,])+var(datmn[['Y']][10,])+var(datmn[['Y']][11,])+var(datmn[['Y']][12,])+var(datmn[['Y']][13,])+var(datmn[['Y']][14,])+var(datmn[['Y']][15,])+var(datmn[['Y']][16,])+var(datmn[['Y']][17,])+var(datmn[['Y']][18,])+var(datmn[['Y']][19,])+var(datmn[['Y']][20,]))/20
estimated_variance_across_person=(var(datmn[['Y']][,1])+var(datmn[['Y']][,2])+var(datmn[['Y']][,3])+var(datmn[['Y']][,4])+var(datmn[['Y']][,5])+var(datmn[['Y']][,6])+var(datmn[['Y']][,7])+var(datmn[['Y']][,8])+var(datmn[['Y']][,9])+var(datmn[['Y']][,10])+var(datmn[['Y']][,11])+var(datmn[['Y']][,12])+var(datmn[['Y']][,13])+var(datmn[['Y']][,14])+var(datmn[['Y']][,15])+var(datmn[['Y']][,16])+var(datmn[['Y']][,17])+var(datmn[['Y']][,18])+var(datmn[['Y']][,19])+var(datmn[['Y']][,20]))/20
total_variance = estimated_variance_across_person+estimated_variance_over_time
print(total_variance*icc_y) #variance of intercept upper bound

alpha = seq(0.50,0.75,0.005)
beta = 0.1*+alpha +0.1

int <- function(x)
qinvgamma(x,shape=alpha[9],rate=beta[9])
print(hdi(int))
print(alpha[9])
print(beta[9])
#invgamma(0.54,0.154)
######### Time varying
print((0.5*100)^2)
alpha_2 = seq(0.25,0.5,0.005)
beta_2= 0.1*+alpha_2 +0.1
int <- function(x)
qinvgamma(x,shape=alpha_2[14],rate=beta_2[14])
print(hdi(int))
print(alpha_2[14])
print(beta_2[14])
#invgamme(0.315,0.1315)

######### log_error
print((1-icc_y)*total_variance)
avg_res_variance = (1-icc_y)*total_variance
log_avg_res_variance = log((1-icc_y)*total_variance)
up_bound =avg_res_variance*10
log_up_bound = log(up_bound)
variance_error = 0.5*(log_up_bound-log_avg_res_variance)^2

alpha_3 = seq(1,1.25,0.005)
beta_3= 0.1*+alpha_3 +0.1
int <- function(x)
qinvgamma(x,shape=alpha_3[30],rate=beta_3[30])
print(hdi(int))
print(alpha_3[30])
print(beta_3[30])

#invgamma(1.145,0.2145)


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################






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
  phi_on_W1 = 0.2,
  phi_on_W2 = 0.1,
  beta_on_W1 = 0.3,
  beta_on_W2 = 0.3,
  lnV_on_W1 = 0.2,
  lnV_on_W2 = 0.3,
  alpha_on_W1 = 0.6,
  alpha_on_W2 = 0.2,
  int_beta = 0.7,
  int_phi= 0.2,
  alpha_var = 0.4,
  beta_var = 0.6,
  phi_var = 0.03,
  ln_var_var = 0.2
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

source("utils/mysterious_vals.R")
source("mysteriousdata.R")
datmn <- mysterious.data(N=50,Nt=100, phi_on_W1 = pop.vals$phi_on_W1,
                         phi_on_W2 = pop.vals$phi_on_W2,
                         beta_on_W1 = pop.vals$beta_on_W1,
                         beta_on_W2 = pop.vals$beta_on_W2,
                         lnV_on_W1 = pop.vals$lnV_on_W1,
                         lnV_on_W2 = pop.vals$lnV_on_W2,
                         alpha_on_W1 = pop.vals$alpha_on_W1,
                         alpha_on_W2 = pop.vals$alpha_on_W2,
                         int_beta = pop.vals$int_beta,
                         int_phi = pop.vals$int_phi,
                         alpha_var = pop.vals$alpha_var,
                         beta_var = pop.vals$beta_var,
                         phi_var = pop.vals$phi_var,
                         ln_var_var = pop.vals$ln_var_var

)
data <- list(N = dim(datmn$Y)[1],
             NT = dim(datmn$Y)[2],
             X = datmn$X,
             Y = datmn$Y,
             W = datmn$W)

# Test model fit etc
fit.mn <- jags(data,  parameters.to.save=params, model.file="models/Model_PHILIPP_DD.txt", n.chains=2, n.iter=2000,
               n.burnin = 500, n.thin=1)

fit.mn
check.fit(fit.mn, pop.vals)





fit.mn2 <- jags(data,  parameters.to.save=params, model.file="models/Model_PHILIPP_CUSTOM_PRIORS_EXACT.txt", n.chains=2, n.iter=5000,
               n.burnin = 500, n.thin=1)

fit.mn2

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

# Decide on priors based on data
# Once, quickly (with the understanding you have right now!)
# Then do it again, using the paper as a reference until you've cracked it!

# Be aware, I had to increase the iterations from 2000 for convergence.
# Expect a run through of 5 "run.models" to take between 4 to 6 hours
source("utils/mysterious_vals.R")
source("utils/utils.R")
source("mysteriousdata.R")
# Lazy Priors
run.models(reps=200, model.file = "Model_experiment_model.txt", data.gen.fn = mysterious.data ,N=10,NT=100)
run.models(reps=200, model.file = "Model_experiment_model.txt", data.gen.fn = mysterious.data ,N=20,NT=100)
run.models(reps=200, model.file = "Model_experiment_model.txt", data.gen.fn = mysterious.data ,N=30,NT=100)
run.models(reps=200, model.file = "Model_experiment_model.txt", data.gen.fn = mysterious.data ,N=40,NT=100)
run.models(reps=200, model.file = "Model_experiment_model.txt", data.gen.fn = mysterious.data ,N=50,NT=100)


# Exact priors!
run.models(reps=200, model.file = "models/model_PHILIPP_DD.txt", data.gen.fn = mysterious.data ,N=10,NT=100)
run.models(reps=200, model.file = "models/model_PHILIPP_DD.txt", data.gen.fn = mysterious.data ,N=20,NT=100)
run.models(reps=200, model.file = "models/model_PHILIPP_DD.txt", data.gen.fn = mysterious.data ,N=30,NT=100)
run.models(reps=200, model.file = "models/model_PHILIPP_DD.txt", data.gen.fn = mysterious.data ,N=40,NT=100)
run.models(reps=200, model.file = "models/model_PHILIPP_DD.txt", data.gen.fn = mysterious.data ,N=50,NT=100)

# Once you get here, refer to analysis2.R for further analysis (plots etc)


check.status("results/Model_experiment_model")
check.status("results/model_PHILIPP_DD")
