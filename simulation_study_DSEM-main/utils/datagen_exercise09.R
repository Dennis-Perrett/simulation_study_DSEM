setwd("C:\\holger\\Uni Tuebingen\\lehre\\WS2023\\Longitudinal\\assignments\\09")

library(mvtnorm)

source("gen_data_version02_orig.R")
##########################
phi0 <- diag(2)*.7+.3 # cov(eta)
mu0  <- c(0,0)        # mean(eta)
ar0  <- c(.5,.3)   # ar(1) structure
ly0  <- matrix(c(1,1,1,0,0,0,
                 0,0,0,1,1,1),6,2,byrow=F) # factor loadings
td   <- diag(6)*.25 # cond. var(y) -> res var
##########################
    
phi0

1-ar0[1:2]^2
phi0*(1-ar0^2)
phi0*(1-ar0%*%t(ar0))
#set core specific seed
set.seed(131212023)
1*(1-(0.3*0.3))

N <- 50
Nt <- 10

dat1 <- gendata02_orig(N,Nt,phi0,mu0,ar0,ly0,td)
dim(dat1)

saveRDS(dat1,"dsem_dat2.rds")
            