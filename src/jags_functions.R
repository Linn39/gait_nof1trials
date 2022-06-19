run_jags <- function(df, X, model_file) {
  # Arrange the data as a list (as required by JAGS)
  # X is the design matrix, including intercept
  # X <- append_time(X)  # append the time column to X, apply this only when considering time as covariate
  data.list <- with(df, list(y = y, X = X, n = nrow(df), ngroups = ncol(X)))
  
  # Define the nodes (parameters and derivatives) to monitor and the chain parameters.
  if (grepl("AR1", model_file, fixed = TRUE) || grepl("CS", model_file, fixed = TRUE) || grepl("lagged_res", model_file, fixed = TRUE)) {
    params <- c("beta", "sigma", "phi")
  }
  else {
    params <- c("beta", "sigma")
  }
 
  nChains = 2
  burnInSteps = 5000
  thinSteps = 1
  numSavedSteps = 10000  #across all chains
  nIter = ceiling(burnInSteps + (numSavedSteps * thinSteps)/nChains)
  
  data.r2jags <- jags.parallel(
    data = data.list, 
    inits = NULL, 
    # jags.module = c("glm","dic"),  # these are loaded by default anyways
    parameters.to.save = params,
    model.file = model_file, 
    n.chains = nChains, 
    n.iter = nIter,
    n.burnin = burnInSteps, 
    n.thin = thinSteps
  )
  # browser()
  cat("\n")
  print(data.r2jags)
  
  return(data.r2jags)
}

get_jags_table <- function(jags_data, sub, var) {
  ### extract the posterior distributions as a dataframe, 
  ### and add subject and variable information
  
  jags_df <- data.frame(jags_data$BUGSoutput$summary)
  jags_df$sub <- sub
  jags_df$var <- var
  
  # move index to column
  jags_df <- cbind(parameter = rownames(jags_df), jags_df)
  rownames(jags_df) <- 1:nrow(jags_df)
  
  return(jags_df)
}

append_time <- function(X) {
  ### append time column (incremental integers) for each walking session
  ### in the design matrix, i.e., regard time as covariate
  
  X_diff <- data.frame(diff(X))
  idx_df <- X_diff[X_diff$conditionst != 0 | X_diff$fatiguefatigue != 0,]
  
  # get all start and end points of time series
  idx_int <- as.integer(rownames(idx_df)) - 1
  idx_int <- append(idx_int, 0, after = 0)
  idx_int <- append(idx_int, dim(X)[1])
  
  X_time <- rep(NA, dim(X)[1])  # empty vector to host the times
  for (i in 1:(length(idx_int)-1)) {
    X_time[(idx_int[i] + 1):idx_int[i+1]] <- 1:(idx_int[i+1]-idx_int[i])
  }
  
  return(cbind(X, X_time))
}
