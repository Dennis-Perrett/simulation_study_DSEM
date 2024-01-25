# Define a function for running JAGS models with repetitions
run.models <- function(reps = 5, model.file, N, NT) {
  
  model_name <- sub("\\.txt$", "", model.file)
  # Subfolder to save results to
  dir <- file.path(paste0("results_",model_name)) 
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
    print(paste0("Iteration: ", i, "/", reps, ". Total: ", start.length + i, "/", start.length + reps))
    
    # Generate data for the JAGS model
    dat1 <- gendata02(N, NT, phi0, mu0, ar0, ly0, td)
    
    # Prepare data for JAGS
    data <- list(
      T = NT,
      N = N,
      J = J,
      observed_data = dat1
    )
    
    # Run the JAGS model
    res <- jags(data, parameters.to.save = params, model.file = model.file, n.chains = 2, n.iter = 5000,
                n.burnin = 500, n.thin = 1)
    
    # Append the results to the list
    list.to.save.to <- c(list.to.save.to, list(res))
    
    # Save progress every 5 iterations
    if ((i %% 5) == 0) {
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
