run_jags_parallel <- function(df, model_file) {
  # Arrange the data as a list (as required by JAGS)
  X <- model.matrix(~x, df)   # X is the design matrix, including intercept
  # X <- append_time(X)  # append the time column to X, apply this only when considering time as covariate
  data.list <- with(df, list(y = y, X = X, n = nrow(df), ngroups = ncol(X)))
  
  # Define the nodes (parameters and derivatives) to monitor and the chain parameters.
  params <- c("beta", "sigma", "phi")
  nChains = 2
  burnInSteps = 5000
  thinSteps = 1
  numSavedSteps = 10000  #across all chains
  nIter = ceiling(burnInSteps + (numSavedSteps * thinSteps)/nChains)
  
  data.r2jags <- jags.parallel(
    data = data.list, 
    inits = NULL, 
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
