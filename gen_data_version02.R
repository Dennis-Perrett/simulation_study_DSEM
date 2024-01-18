# generate data

gendata02 <- function(N,Nt,phi0,mu0,ar0,ly0,td){
  
  # Number of latent factors
  N.lf <- length(mu0)
  # Number of factor loadings
  N.eta <- dim(ly0)[1]

  
  #SAMPLING <- 1
  eta0 <- rmvnorm(N,mu0,phi0)
  # empty matrices for lvs and ovs
  eta <- array(NA,c(N,Nt,N.lf))
  #matrix[N,6] y[Nt]; declared
  y   <- array(NA,c(N,Nt,N.eta))
  # latent variables
  # time point 1
  
  eta[,1,] <- eta0 + rmvnorm(N,mu0,phi0)
  #cov(eta[,1,]) # check cov
  
  
  # NOTE: the total variance of the latent factors is currently not 1
  y[,1,] <- eta[,1,]%*%t(ly0) + rmvnorm(N,sigma = td)

  #t==2

  #rest
  for(j in 2:Nt){#j<-2
    
    zeta.var <- phi0*(1-ar0%*%t(ar0))
    mean <- integer(N.lf)
    zeta <- rmvnorm(N,mean,zeta.var) # this is a residual with var (1-phi^2) sp tjat var(eta)==1
    for (e in 1:N.lf){
      eta[,j,e] <- eta0[,e] + ar0[e]*eta[,j-1,e] + zeta[,e]
    }
    y[,j,] <- eta[,j,]%*%t(ly0)+rmvnorm(N,sigma = td)
  }
  
  y
}


gendata02(500)
