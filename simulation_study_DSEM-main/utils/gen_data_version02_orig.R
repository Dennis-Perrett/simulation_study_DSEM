# generate data
# ORIGIANL
gendata02_orig <- function(N,Nt,phi0,mu0,ar0,ly0,td){
  
  #SAMPLING <- 1
  eta0 <- rmvnorm(N,mu0,phi0)
  # empty matrices for lvs and ovs
  eta <- array(NA,c(N,Nt,2))
  #matrix[N,6] y[Nt]; declared
  y   <- array(NA,c(N,Nt,6))
  # latent variables
  # time point 1
  eta[,1,] <- eta0+rmvnorm(N,mu0,phi0)
  #cov(eta[,1,]) # check cov
  # NOTE: the total variance of the latent factors is currently not 1
  y[,1,] <- eta[,1,]%*%t(ly0)+rmvnorm(N,sigma = td)
  
  #t==2
  zeta <- rmvnorm(N,c(0,0),phi0*(1-ar0[1:2]^2)) # this is a residual with var (1-phi^2) sp tjat var(eta)==1
  eta[,2,1] <- eta0[,1] + ar0[1]*eta[,1,1] + zeta[,1]
  eta[,2,2] <- eta0[,2] + ar0[2]*eta[,1,2] + zeta[,2]
  y[,2,] <- eta[,2,]%*%t(ly0)+rmvnorm(N,sigma = td)
  
  #rest
  for(j in 3:Nt){#j<-2
    zeta <- rmvnorm(N,c(0,0),phi0*(1-ar0[1:2]^2)) # this is a residual with var (1-phi^2) sp tjat var(eta)==1
    eta[,j,1] <- eta0[,1] + ar0[1]*eta[,j-1,1] + ar0[3]*eta[,j-2,1] + zeta[,1]
    eta[,j,2] <- eta0[,2] + ar0[2]*eta[,j-1,2] + zeta[,2]
    y[,j,] <- eta[,j,]%*%t(ly0)+rmvnorm(N,sigma = td)
  }
  
  y
}
