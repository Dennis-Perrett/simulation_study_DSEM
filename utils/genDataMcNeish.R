
genDataMcNeish <- function(N, Nt){
  #
  # Simulate the data from McNeish 2019
  # Intput: 
  #        N: Number of people
  #        Nt: Number of Time points
  # Returns: 
  #          Y (N x Nt)
  #          X (N x Nt)
  #          W (N x 2)
  # 
  
  W1 <- rnorm(N, 0, 1) # this is correct for the output
  W2 <- rnorm(N, 0, 1)
  
  Y <- matrix(NA, nrow=N, ncol=Nt)
  X <- matrix(NA, nrow=N, ncol=Nt)
  
  for (i in 1:N){
    alpha  <-       (0.5 * W1[i]) + (.3 * W2[i]) + rnorm(1,0, sd =sqrt(.3))
    phi    <- 0.2 + (0.1 * W1[i]) + (.05 * W2[i]) + rnorm(1,0, sd =sqrt(.01))
    beta   <- 0.7 + (0.3 * W1[i]) + (.2 * W2[i]) + rnorm(1,0, sd = sqrt(.3))
    ln_var <-       (0.3 * W1[i]) + (.1 * W2[i]) + rnorm(1,0,sd = sqrt(.1))
    
    
    Yi <- rnorm(Nt+1,0,0)
    Yi[1] <- alpha + rnorm(1,0,sd=sqrt(exp(ln_var)))
    Xi <- rnorm(Nt+1, 5 , sd = 1) # Variance = 1 as per paper
    for (t in 2:(Nt+1)){
      Yi[t] <- alpha + phi * (Yi[t - 1]-alpha) + beta * Xi[t] + rnorm(1,0,sd=sqrt(exp(ln_var)))
      
    }
    
    Y[i,] <- Yi[2:(Nt+1)]
    X[i,] <- Xi[2:(Nt+1)]
    
    
  }
  W <- matrix(c(W1,W2), nrow=N, ncol=2, byrow=F)
  return(list(Y = Y,
              X = X,
              W = W))
  
}

dattest <- genDataMcNeish(1,100)
plot(dattest$Y[,],type="l",main="Example of simulated data",xlab = "Y")
dattest$Y

