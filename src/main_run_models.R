rm(list = ls())

library(tictoc)
library(ggplot2)
library(R2jags)
library(lattice)
library(knitr)
library(dplyr) 
library(tidyr)
library(foreach)
library(doParallel)

source("./src/data_loader.R")
source("./src/jags_functions.R")

#### select a feature/gait parameter (one at a time)
features <- list(
  # list("stride_lengths", "SL"),  
  list("stride_times", "ST")
  )

#### select a model
model_n <- 7 # choose from the list of models
downsample_step <- 5

model_names <- list(
  "basic",
  "basic_informative",
  "time_cov_basic",
  "time_cov_basic_informative",
  "AR1",
  "AR1_informative",
  "AR1_model_diff_prob"
)

sub_list <- list(
  "sub_01"
  # "sub_02",
  # "sub_03",
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

# read data from file
loc_df <- load_gait_parameters(folder_path, 'LFRF_all_strides')
loc_df <- loc_df[loc_df$foot == "left", ]
loc_df <- downsample_rows(loc_df[loc_df$foot == "left", ], downsample_step) # reduce data size
print(paste("Downsample by", downsample_step))

# rename the label columns to 1 and 0
loc_df$fatigue[loc_df$fatigue == "control"] <- 0
loc_df$fatigue[loc_df$fatigue == "fatigue"] <- 1
loc_df$condition[loc_df$condition == "st"] <- 0
loc_df$condition[loc_df$condition == "dt"] <- 1

# loop through all subjects and all features
output_folder <- file.path("data", "processed", paste0("all_left_foot_downsample_", as.character(downsample_step)))
dir.create(output_folder, showWarnings = FALSE)

for (feature in features) {
  all_estimate_df <- data.frame()
  print(feature[[1]])
  if (grepl("time_cov", model_names[[model_n]], fixed = TRUE)) {
    file_model_name <- substring(model_names[[model_n]], 10)  # time_cov also uses the basic model
  } else {
    file_model_name <- model_names[[model_n]]
  }
  if (grepl("informative", file_model_name, fixed = TRUE)) {
    model_file <- paste0(file_model_name, "_", feature[[2]], ".txt")
  } else {
    model_file <- paste0(file_model_name, ".txt")
  }
  print(model_names[[model_n]])
  
  for (subject in sub_list) {
    print(subject)
    # filter the dataframe
    dat_df <- filter(loc_df, sub == subject)
    dat_df <- dat_df[, c(feature[[1]], "fatigue", "condition")]
    print_data_summary(dat_df, feature[[1]])
    dat_df <- rename(dat_df,c('y' = feature[[1]]))
    print(paste("Current sample size:", nrow(dat_df)))
    
    # # viasualizing the data (optional)
    # with(dat_df, interaction.plot(fatigue, condition, y, main=paste(subject, feature[[1]])))
    # ggplot(dat_df, aes(y = y, x = condition, fill = fatigue), main=paste(subject, feature[[1]])) + geom_boxplot()
    
    # run jags
    X <- model.matrix(~condition * fatigue, dat_df)   # X is the design matrix, including intercept
    if (grepl("time_cov", model_names[[model_n]], fixed = TRUE)) {
      X <- append_time(X)  # append the time column to X, apply this only when considering time as covariate
    }
    tic("start running")
    print("start running...")
    data.r2jags <- run_jags(dat_df, X, file.path("models", model_file))
    print("...finished running")
    toc()
    # save simulation outputs
    save(data.r2jags, file = file.path(output_folder, paste0(
      "data_r2jags_",
      model_names[[model_n]],
      "_",
      feature[[2]],
      "_",
      subject,
      ".RData"
      )))
    
    # save jags output table
    all_estimate_df <- bind_rows(all_estimate_df, get_jags_table(data.r2jags, subject, feature[[1]]))
  }
  # save estimated parameters table
  write.csv(
  all_estimate_df, 
  file = file.path(output_folder, paste0("all_estimates_", feature[[1]], "_", model_names[[model_n]], ".csv")), 
  row.names=FALSE
  )
}
