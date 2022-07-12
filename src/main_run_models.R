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
# library(kableExtra)

source("./src/data_loader.R")
source("./src/jags_functions.R")

# for nice display of inline code 
opts_chunk$set(echo=TRUE, comment='') 
# for nicely formatted tables 
options(knitr.table.format = "latex")

#### choose a model
model_n <- 6 # choose from the list of models
downsample_step <- 5

model_names <- list(
  "default",
  "default_informed",
  "default_time_cov",
  "default_time_cov_informed",
  "AR1",
  "AR1_informed",
  "AR1_cauchy_t"
)

models <- list(
  "fact_anovaModel_default.txt",
  "fact_anovaModel_default_informed.txt",
  "fact_anovaModel_default.txt",
  "fact_anovaModel_default_informed.txt",
  "mixed_model_AR1.txt",
  "mixed_model_AR1_informed.txt",
  "mixed_model_AR1_cauchy_t.txt"
)

print(models[[model_n]])

## load & concat .csv file
# cond_list <- list("dt", "st")
# test_list <- list("fatigue", "control")
IMU_loc <- list('LFRF_all_strides')  # LFRF_windowed, LF, RF, LFRF_all_strides
kw <- IMU_loc[[1]]  # list as a safety measure, for now we only load one location at a time

sub_list <- list(  # for n-of-1 trials, select only one subject!
  "sub_01",
  "sub_02",
  "sub_03",
  "sub_05",
  "sub_06",
  "sub_07",
  "sub_08",
  "sub_09",
  "sub_10",
  "sub_11",
  "sub_12",
  "sub_13",
  "sub_14",
  "sub_15",
  "sub_17",
  "sub_18"
  )

# read data from file
loc_df <- load_gait_parameters(folder_path, kw)
loc_df <- loc_df[loc_df$foot == "left", ]
loc_df <- downsample_rows(loc_df[loc_df$foot == "left", ], downsample_step) # reduce data size
print(paste("Downsample by", downsample_step))
# print("Summary of data from all subjects after downsampling:")
# print_data_summary(loc_df, "stride_times")

# rename the label columns to 1 and 0
loc_df$fatigue[loc_df$fatigue == "control"] <- 0
loc_df$fatigue[loc_df$fatigue == "fatigue"] <- 1
loc_df$condition[loc_df$condition == "st"] <- 0
loc_df$condition[loc_df$condition == "dt"] <- 1

# select list of features / gait paarameters
features_list <- c(
  'stride_lengths'
  # 'stride_times'
  # "speed"
)

# loop through all subjects and all features
output_folder <- file.path("data", "processed", paste0("cauchy_conjugate_left_foot_downsample_", as.character(downsample_step)))
dir.create(output_folder, showWarnings = FALSE)

all_estimate_df <- data.frame()
for (feature in features_list) {
  for (subject in sub_list) {
    print(subject)
    print(feature)
    # filter the dataframe
    dat_df <- filter(loc_df, sub == subject)
    dat_df <- dat_df[, c(feature, "fatigue", "condition")]
    print_data_summary(dat_df, feature)
    dat_df <- rename(dat_df,c('y' = feature))
    print(paste("Current sample size:", nrow(dat_df)))
    
    # # viasualizing the data (optional)
    # with(dat_df, interaction.plot(fatigue, condition, y, main=paste(subject, feature)))
    # ggplot(dat_df, aes(y = y, x = condition, fill = fatigue), main=paste(subject, feature)) + geom_boxplot()
    
    # run jags
    # run_jags.options(silent.jags=TRUE, silent.runjags=TRUE)
    X <- model.matrix(~condition * fatigue, dat_df)   # X is the design matrix, including intercept
    if (grepl("time_cov", model_names[[model_n]], fixed = TRUE)) {
      X <- append_time(X)  # append the time column to X, apply this only when considering time as covariate
    }
    tic("start running")
    print("start running...")
    data.r2jags <- run_jags(dat_df, X, file.path("models", models[[model_n]]))
    print("...finished running")
    toc()
    # save simulation outputs
    save(data.r2jags, file = file.path(output_folder, paste0(
      "data_r2jags_",
      model_names[[model_n]],
      "_",
      feature,
      "_",
      subject,
      ".RData"
      )))
    
    # save jags output table
    # all_estimate_df <- get_jags_table(data.r2jags, subject, feature)  # use this line when running parallel with foreach
    all_estimate_df <- bind_rows(all_estimate_df, get_jags_table(data.r2jags, subject, feature))
    # get_jags_table(data.r2jags, subject, feature)
  }
  # save estimated parameters table
  write.csv(
  all_estimate_df, 
  file = file.path(output_folder, paste0("all_estimates_", feature, "_", model_names[[model_n]], ".csv")), 
  row.names=FALSE
  )
}
