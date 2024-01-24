genData <- function(N,NT, latent.mu=c(0), latent.var = diag(1)*0.2, latent.ar.mean=c(0.8), latent.ar.var = diag(1)*0.2, l.loadings=c(1,1,2)){
  
  N.obs <- length(l.loadings)
  output <- array(NA,c(N,NT,N.obs))
  
  ars <- rmvnorm(N, latent.ar.mean, latent.ar.var)
  
  for (i in 1:N){
    errors.observed <- t(rmvnorm(NT,c(3,2,-1),diag(3)))
    latent <- t(rmvnorm(NT,latent.mu,latent.var))
  
    for (t in 2:NT){
      latent[t] <- latent[t-1]*ars[i] + latent[t]
      
    }
    
    obs <- l.loadings %*% latent + errors.observed
    output[i,,] <- l.loadings %*% latent
    
  }
  
  return(output)
  
  
}

(dat <- genData(4,5))
dat[1,,] # Person 1
dim(dat)
