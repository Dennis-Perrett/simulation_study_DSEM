genData <- function(N,NT, latent.mu=c(0), latent.var = diag(1)*0.2, latent.ar.mean=c(0.8), latent.ar.var = diag(1)*0.2, l.loadings=c(1,-1,-2)){
  
  # Latent.mu: Mean of the latent variable(s)
  # Latnet.var: variance of the latent variable(s)
  # latent.ar.mean: mean of the autoregressive coefficients (across people)
  # latent.ar.var: variance of the autoregressive coefficients (across people)
  # l.loadings: fixed factor loadings
  
  
  # Save number of observed variables
  N.obs <- length(l.loadings)
  # initialise output matrix [people, time, obs.data]
  output <- array(NA,c(N,NT,N.obs))
  
  # sample some autoregressive coefficients. 1 Per person.
  time_coef <- rnorm(N, latent.ar.mean, sd = sqrt(latent.ar.var))

  time <- 1:NT
  # For each person
  for (i in 1:N){
    # create errors for observed data
    errors.observed <- t(rmvnorm(NT, mean = c(0,0,0), sigma = diag(3)*5))
    
    # for each time point
    
    latent <- time * time_coef[i]
    # observed data = loadings * latent + errors
    # [ obs x time] ie 3x100
    obs <- latent %*% t(l.loadings) + t(errors.observed)
    # save this per person
    output[i,,] <- obs
    
  }
  
  return(output)
  
  
}

library(MASS)  # For mvrnorm function

simulate_longitudinal_data <- function(n_individuals, n_timepoints, ar_coef_mean, ar_coef_sd, factor_loadings, error_sd) {
  # Simulate individual-specific AR(1) coefficients
  ar_coefs <- rnorm(n_individuals, mean = ar_coef_mean, sd = ar_coef_sd)
  
  # Simulate longitudinal data
  output <- array(NA,c(n_individuals,n_timepoints,length(factor_loadings)))
  
  for (i in 1:n_individuals) {
    ar_coef <- ar_coefs[i]
    
    # Generate AR(1) time series for each individual
    time_series <- arima.sim(model = list(ar = ar_coef), n = n_timepoints)
    
    # Simulate observed variables with factor loadings
    observed_vars <- mvrnorm(n_timepoints, mu = rep(0, length(factor_loadings)), Sigma = diag(length(factor_loadings))*error_sd^2)
    observed_vars <- observed_vars %*% factor_loadings
    
    # Combine AR(1) time series and observed variables
    longitudinal_data[((i - 1) * n_timepoints + 1):(i * n_timepoints), ] <- time_series + observed_vars
    
  }
  
  # Convert to data frame
  
  longitudinal_data <- data.frame(longitudinal_data)
  colnames(longitudinal_data) <- paste0("ObsVar", 1:length(factor_loadings))
  
  return(longitudinal_data)
}