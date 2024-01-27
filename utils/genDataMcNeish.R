
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
  
  W1 <- rnorm(N, 10, 1) # this is correct for the output
  W2 <- rnorm(N, 5, 1)
  
  Y <- matrix(NA, nrow=N, ncol=Nt)
  X <- matrix(NA, nrow=N, ncol=Nt)
  
  for (i in 1:N){
    alpha  <-       (0.5 * W1[i]) + (.3 * W2[i]) + rnorm(1,0, sd =sqrt(.3))
    phi    <- 0.2 + (0.1 * W1[i]) + (.05 * W2[i]) + rnorm(1,0, sd =sqrt(.01))
    beta   <- 0.7 + (0.3 * W1[i]) + (.2 * W2[i]) + rnorm(1,0, sd = sqrt(.3))
    ln_var <-       (0.3 * W1[i]) + (.1 * W2[i]) + rnorm(1,0,sd = sqrt(.1))
    
    Yi <- rnorm(Nt, 0 , sd = sqrt(exp(ln_var)))
    Xi <- rnorm(Nt, 0 , sd = 1)
    for (t in 2:Nt){
      Yi[t] <- alpha + phi * Yi[t - 1] + beta * Xi[t] + Yi[t]
      
    }
    Y[i,] <- Yi
    X[i,] <- Xi
    
    
  }
  W <- matrix(c(W1,W2), nrow=N, ncol=2, byrow=F)
  return(list(Y = Y,
              X = X,
              W = W))
  
}



ge <- genDataMcNeish(5,10)
plot(ge$Y[2,], type="l")
lines(ge$Y[5,], col="blue")
lines(ge$Y[3,], col="red")

