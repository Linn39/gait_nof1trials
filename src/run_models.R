rm(list = ls())

library(tictoc)
library(ggplot2)
library(R2jags)
library(lattice)
library(knitr)
library(dplyr) 
library(tidyr)
# library(kableExtra)
# for nice display of inline code 
opts_chunk$set(echo=TRUE, comment='') 
# for nicely formatted tables 
options(knitr.table.format = "latex")

tic("start running")
print("start running...")

#### choose a model

model_n <- 3

likelihood_models <- list(
  "fact_anovaModel_default.txt",
  "mixed_model_lagged_res.txt",
  "mixed_model_AR1.txt",
  "mixed_model_CS.txt"
)

model_names <- list(
  "default",
  "lagged_res",
  "AR1",
  "CS"
)
print(likelihood_models[[model_n]])

#### load gait data
load_gait_parameters <- function (folder_path, keyword, subject, test, cond) {
  
  #### first find file name by keyward ####
  file_path <- c()
  kw_fn_list <- list()  # keyword - file name pairs
  kw_fn_list[[ "LF" ]] <- "left_foot_core_params_py_n.csv"
  kw_fn_list[[ "RF" ]] <- "right_foot_core_params_py_n.csv"
  kw_fn_list[[ "LFRF_windowed" ]] <- "agg_windows_control_fatigue_st_dt_10_2.csv"

  # look up file names
  if (keyword %in% names(kw_fn_list)) {
    file_name <- kw_fn_list[[keyword]]
  }
  else {
    return(paste("Keyword", keyword, "does not exist!"))
  }
  
  #### then load the data ####
  if (grepl("windowed", keyword, fixed = TRUE)) {
    folder_path <- file.path("data/processed", "features")
    data_path <- file.path(folder_path, file_name)
    dat_df <- readr::read_csv(data_path, col_names = T, show_col_types = FALSE)  
    # dat_df <- dat_df[(dat_df$)]
  }
  else {
    folder_path <- file.path("data/processed", paste0("OG_", cond, "_", test), sub)
    data_path <- file.path(folder_path, file_name)
    dat_df <- readr::read_csv(data_path, col_names = T, show_col_types = FALSE)
    # remove outlier strides
    loc_df <- loc_df[(dat_df$is_outlier == FALSE) & 
                     (dat_df$turning_interval == FALSE) & 
                     (dat_df$interrupted == FALSE),
                     ]
  }
  
  data_path <- file.path(folder_path, file_name)
  dat_df <- readr::read_csv(data_path, col_names = T, show_col_types = FALSE)
  
  return(dat_df)
}


## load & concat .csv file
# dataset <- "fatigue_dual_task"
cond_list <- list("dt", "st")
test_list <- list("fatigue", "control")
IMU_loc <- list('LFRF_windowed')  # LFRF_windowed, LF, RF
kw <- IMU_loc[[1]]  # safety measure, for now we only load one location at a time

sub_list <- list(  # for n-of-1 trials, select only one subject!
  # "sub_01"
  # "sub_02"
  "sub_03"
  # "sub_05",
  # "sub_06",
  # "sub_07",
  # "sub_08",
  # "sub_09",
  # "sub_10",
  # "sub_11",
  # "sub_12",
  # "sub_13",
  # "sub_14",
  # "sub_15",
  # "sub_17",
  # "sub_18"
  )

if (grepl("windowed", kw, fixed = TRUE)) {
  # read windowed parameters from only one file
  loc_df <- load_gait_parameters(folder_path, kw,  sub, test, cond)
  print(nrow(loc_df))
} else {
  # read all data for one IMU location
  loc_df <- data.frame()
  for (cond in cond_list) {
    for (test in test_list) {
      for (sub in sub_list) {
        for (kw in IMU_loc) {
          dat_df_temp <- load_gait_parameters(folder_path, kw, sub, test, cond)
          dat_df_temp["fatigue"] <- test
          dat_df_temp["condition"] <- cond
          loc_df <- bind_rows(loc_df, dat_df_temp)
        }
      }
    }
  }
}

# rename the label columns to 1 and 0
loc_df$fatigue[loc_df$fatigue == "control"] <- 0
loc_df$fatigue[loc_df$fatigue == "fatigue"] <- 1
loc_df$condition[loc_df$condition == "st"] <- 0
loc_df$condition[loc_df$condition == "dt"] <- 1

# select list of features
features_list <- c(
  'stride_lengths_avg'
  # 'clearances_min_avg', 
  # 'clearances_max_avg',
  # 'stride_times_avg',
  # 'swing_times_avg',
  # 'stance_times_avg',
  # 'stance_ratios_avg', 
  # 'cadence_avg',
  # 'speed_avg'
  # 'stride_lengths_CV',
  # 'clearances_min_CV', 
  # 'clearances_max_CV', 
  # 'stride_times_CV',
  # 'swing_times_CV', 
  # 'stance_times_CV', 
  # 'stance_ratios_CV', 
  # 'cadence_CV',
  # 'speed_CV', 
  # 'stride_lengths_SI', 
  # 'clearances_min_SI',
  # 'clearances_max_SI', 
  # 'stride_times_SI', 
  # 'swing_times_SI',
  # 'stance_times_SI', 
  # 'stance_ratios_SI', 
  # 'cadence_SI', 
  # 'speed_SI'
)



append_time <- function(X) {
  ### append time column (incremental integers) for each walking session

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

run_jags <- function(df, model_file) {
  # Arrange the data as a list (as required by JAGS)
  X <- model.matrix(~condition * fatigue, df)   # X is the design matrix, including intercept
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


# loop through all subjects and all features
all_estimate_df <- data.frame()
for (subject in sub_list) {
  print(subject)
  for (feature in features_list) {
    print(feature)
    # filter the dataframe
    dat_df <- filter(loc_df, sub == subject)
    dat_df <- dat_df[, c(feature, "fatigue", "condition")]
    dat_df <- rename(dat_df,c('y' = feature))
    
    # # viasualizing the data (optional)
    # with(dat_df, interaction.plot(fatigue, condition, y, main=paste(subject, feature)))
    # ggplot(dat_df, aes(y = y, x = condition, fill = fatigue), main=paste(subject, feature)) + geom_boxplot()
    
    # run jags
    # run_jags.options(silent.jags=TRUE, silent.runjags=TRUE)
    data.r2jags <- run_jags(dat_df, file.path("likelihood_models", likelihood_models[[model_n]]))
    
    # save jags data
    all_estimate_df <- bind_rows(all_estimate_df, get_jags_table(data.r2jags, subject, feature))
  }
}

output_folder <- file.path("data", "processed", "jags_output")
dir.create(output_folder, showWarnings = FALSE)

# save estimated parameters table
write.csv(
  all_estimate_df, 
  file = file.path(output_folder, paste0("all_estimates_", model_names[[model_n]], ".csv")), 
  row.names=FALSE
)

# save simulation outputs
save(data.r2jags, file = file.path(output_folder, paste0("data_r2jags_", model_names[[model_n]], ".RData")))

print("...finished running")
toc()
