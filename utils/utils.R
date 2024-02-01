run.models <- function(reps = 5, model.file, N, NT) {
  #params <- c("ly","ar.var","ar.mean")
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
  model_name <- sub("\\.txt$", "", basename(model.file))
  
  # Subfolder to save results to
  dir <- file.path(paste0("results/",model_name)) 
  if (!dir.exists("results")) dir.create("results")
  if (!dir.exists(dir)) dir.create(dir)
  # Define cache file and result file names
  CACHE.FILE <- "results_cache.rda"
  SAVE.FILE.NAME <- paste0("results_", as.character(N), "_", as.character(NT), ".rds")
  
  # Load results from cache if available
  if (file.exists(file.path(dir, SAVE.FILE.NAME))) {
    list.to.save.to <- readRDS(file.path(dir, SAVE.FILE.NAME))
  } else {
    list.to.save.to = list()
  }
  
  # Record the starting length of the result list
  start.length <- length(list.to.save.to)
  
  # Loop through the specified number of repetitions
  for (i in 1:reps) {
    print(paste0("Model: ", model.file, ". N: ",N,", NT: ",NT))
    print(paste0("Iteration: ", i, "/", reps, ". Total: ", start.length + i, "/", start.length + reps))
    
    # Generate data for the JAGS model
    #dat1 <- genData(N,NT,latent.ar.mean = c(0.2), latent.ar.var = diag(1)*0.5) # Example usage:
    dat1 <- genDataMcNeish(N,NT)
    
    # Prepare data for JAGS
    data <- list(N = dim(dat1$Y)[1],
                 NT = dim(dat1$Y)[2],
                 X = dat1$X,
                 Y = dat1$Y,
                 W = dat1$W)
    
    # Run the JAGS model
    res <- jags(data, parameters.to.save = params, model.file = model.file, n.chains = 2, n.iter = 2000,
                n.burnin = 500, n.thin = 1)
    
    # Append the results to the list
    list.to.save.to <- c(list.to.save.to, list(res))
    
    # Save progress every 5 iterations
    if ((i %% 50) == 0) {
      saveRDS(list.to.save.to, file = CACHE.FILE)
    }
  }
  
  # Remove the cache file
  if (file.exists(CACHE.FILE)) {
    file.remove(CACHE.FILE)
  }
  
  # Save the final result list
  saveRDS(list.to.save.to, file = file.path(dir, SAVE.FILE.NAME))
  
}

check.status <- function(directory_path) {
  # Get a list of .rda files in the directory
  rda_files <- list.files(directory_path, pattern = "\\.rds$", full.names = TRUE)
  
  # Initialize an empty list to store results
  result_list <- list()
  
  # Loop through each .rda file
  for (file_path in rda_files) {
    # Extract the file name without extension
    file_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Load the R object from the .rda file
    loaded_object <- tryCatch(
      expr = readRDS(file_path),
      error = function(e) {
        warning(paste("Error reading file:", file_path))
        NULL
      }
    )
    
    # Check if the object was successfully loaded
    if (!is.null(loaded_object)) {
      # Save the length of the object in the result list
      result_list[[file_name]] <- length(loaded_object)
    }
  }
  
  for (name in names(result_list)) {
    value <- result_list[[name]]
    cat(sprintf("%s: %s repetitions\n", name, value))
  }
}


extract.parameters <- function(sims.list){
  iters <- length(sims.list)
  ar <- vector(length=iters)
  ly <- matrix(nrow=3, ncol=iters)
  
  for (i in 1:iters){

    means <- sims.list[[i]]$BUGSoutput$mean
    ar[i] <- means$ar
    ly[,i] <- means$ly
    
  }
  
  return(list(ar = ar, ly = ly))
}

extract.params2 <- function(sims.list.rds){
  
  sims.list <- readRDS(sims.list.rds)
  
  iters <- length(sims.list)
  mu <- vector(length=iters)
  sigma <- vector(length=iters)
  ly <- matrix(nrow=3, ncol=iters)
  
  
  for (i in 1:iters){
    
    means <- sims.list[[i]]$BUGSoutput$mean
    mu[i] <- means$ar.mean
    sigma[i] <- means$ar.var
    ly[,i] <- means$ly
    
  }
  
  return(list(mu=mu, sigma = sigma, ly= ly))
}

extract.params3 <- function(sims.list, metric='mean'){
  sims.list <- readRDS(sims.list)
  params <- sims.list[[1]]$BUGSoutput[[metric]]
  for (i in 2:length(sims.list)){
    
    params <- Map(c,params,sims.list[[i]]$BUGSoutput[[metric]])
  }
  return(params)
}




check.fit <- function(model, pop.vals){
  
  for (est_name in names(model$BUGSoutput$mean)){
    if (est_name != "deviance"){
      print(paste0(est_name,": ", model$BUGSoutput$mean[[est_name]] / pop.vals[[est_name]]))
    }
  }
  
  
}


extract.rel.bias.from.list.of.means <- function(means, param){
  # takes in a list of results, and a parameter as a string
  # Returns an array of relative biases in the order of input list
  RLDD <- c()
  for (r in means){
    RLDD <- c(RLDD,(relative.bias(r, param)))
  }
  return (RLDD)
}


relative.bias <- function(results, param){
  # Calculates relative bias
  return ( mean(results[[param]] / pop.vals[[param]]))
  
}


plot.RB.path <- function(df, title="Average Relative Bias Trajectories"){
  # Input: Df of [ X , IG, DD]  
  # Assuming long_data is your tibble with columns 'X' and 'Y'
  p <- ggplot(df) +
    geom_line(aes(x = X, y = Y, group=Prior), color=alpha("black",0.4)) + 
    stat_smooth(aes(x = X, y=Y, colour=Prior), method = "loess", formula = y ~ x, span=1, se=F) +
    theme_linedraw() +
    labs(title = bquote(underline(.(title))), x = "Level 2 Sample Size", y = "Relative Bias",color="Lines") + 
    geom_hline(yintercept = c(1.1,.9), linetype = "dashed", color = "darkgrey", size=.6)+
    scale_color_manual(
      name=NULL,
      labels = c("Admissible-Range-Restricted", "Default Diffuse"),
      values = c('#34d5eb','#218491')
    ) +
    theme(axis.line = element_line(colour = "#9e9e9e"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.784,.907),
          axis.ticks.length.y=unit(c(-2.5,2.5), "pt"),
          axis.ticks.length.x=unit(c(-2.5,2.5), "pt"),
          axis.ticks.x = element_line(color = "#9e9e9e", size=.5),
          axis.ticks.y = element_line(color = "#9e9e9e", size=.5),
          plot.title = element_text(hjust = 0.5),
          text=element_text(family="Times New Roman", color="#4d4d4d")) +
    scale_y_continuous(breaks = seq(0.6, 3, by=0.2), limits = c(0.6, NA))
  
  return(p)}





rand.fun <- function(){
  # Plot distributions of relative biases
  p.sigma <- ggplot(d, aes(x=x, y=y)) + 
    geom_violin(
      fill = alpha("lightblue",.5),
      size=0.3
    ) + 
    geom_boxplot(
      width = 0.1,  # Adjust the width of the boxplot
      fill = alpha("white",.8),  # Adjust the fill color of the boxplot
      size=0.3
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", size=0.4) +
    theme_linedraw() +
    ggtitle(
      label="Relative Bias of Sigma per Sample Size (N)",
      subtitle=TeX('Relative Bias $=\\left(\\hat{\\theta}/\\theta\\right)$, where 1 is optimal (indicated below with dashed black line).')
    ) +
    ylab("Relative Bias") +
    theme(
      plot.title = element_text(size = 10),
      plot.subtitle = element_text(size=8)
    )
  p.sigma
  p.mu <- ggplot(long_data, aes(x=`N Size`, y=`Mu`)) + 
    geom_violin(
      fill = alpha("lightblue",.5),
      size=0.3
    ) + 
    geom_boxplot(
      width = 0.1,  # Adjust the width of the boxplot
      fill = alpha("white",.8),  # Adjust the fill color of the boxplot
      size=0.3
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", size=0.4) +
    theme_linedraw() +
    ggtitle(
      label="Relative Bias of Mu per Sample Size (N)",
      subtitle=TeX('Relative Bias $=\\left(\\hat{\\theta}/\\theta\\right)$, where 1 is optimal (indicated below with dashed black line).')
    ) +
    ylab("Relative Bias") +
    theme(
      plot.title = element_text(size = 10),
      plot.subtitle = element_text(size=8)
    )
  

}

create.data.frame.from.results <- function(IG.data,DD.data){
  d1 <- data.frame(x=(1:5)*10, y = IG.data, prior = "Admissible-Range-Restricted")
  d2 <- data.frame(x=(1:5)*10, y = DD.data, prior = "Default Diffuse")  
  df <- rbind(d1,d2)
  names(df) <- c("X","Y","Prior")
  return(df)
}


get.null.detection.rate <- function(results.file){
  "Takes in a results file and returns the null null detection rates (as per McNeish)"
  res <- readRDS(results.file)
  a <- res[[1]]$BUGSoutput$summary[,c(3,7)]
  lo <- a[,1]
  up <- a[,2]
  output <- (lo <= 0  & 0 <= up)
  for (i in 2:length(res)){
    a <- res[[i]]$BUGSoutput$summary[,c(3,7)]
    lo <- a[,1]
    up <- a[,2]
    output <- Map(c,output,(lo <= 0  & 0 <= up))
  }
  
  return(1-sapply(output,mean))
}