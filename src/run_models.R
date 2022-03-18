# print(getwd())
# setwd("/dhc/home/lin.zhou/projects/gait_jags")
# print(getwd())
# .libPaths("/dhc/home/lin.zhou/projects/gait_jags/renv/library/R-4.1/x86_64-pc-linux-gnu")
# print(.libPaths())
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
tic("start running")
print("start running...")

#### choose a model

model_n <- 4
downsample_step <- 1

likelihood_models <- list(
  "fact_anovaModel_default.txt",
  "fact_anovaModel_default_time_cov.txt",
  "mixed_model_lagged_res.txt",
  "mixed_model_lagged_res_new.txt",  # swap model difinition and priors
  "mixed_model_AR1.txt",
  "mixed_model_CS.txt"
)

model_names <- list(
  "default",
  "default_time_cov",
  "lagged_res",
  "lagged_res_new",
  "AR1",
  "CS"
)
print(likelihood_models[[model_n]])

## load & concat .csv file
# dataset <- "fatigue_dual_task"
cond_list <- list("dt", "st")
test_list <- list("fatigue", "control")
IMU_loc <- list('LFRF_all_strides')  # LFRF_windowed, LF, RF, LFRF_all_strides
kw <- IMU_loc[[1]]  # safety measure, for now we only load one location at a time

sub_list <- list(  # for n-of-1 trials, select only one subject!
  # "sub_01"
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
loc_df <- load_gait_parameters(folder_path, kw,  sub, test, cond)
loc_df <- loc_df[loc_df$foot == "left", ]
loc_df <- downsample_rows(loc_df[loc_df$foot == "left", ], downsample_step) # reduce data size
print(paste("Downsample by", downsample_step))
print(paste("Dataset total sample size:", nrow(loc_df)))

# rename the label columns to 1 and 0
loc_df$fatigue[loc_df$fatigue == "control"] <- 0
loc_df$fatigue[loc_df$fatigue == "fatigue"] <- 1
loc_df$condition[loc_df$condition == "st"] <- 0
loc_df$condition[loc_df$condition == "dt"] <- 1

# select list of features
features_list <- c(
  'stride_lengths',
  # 'clearances_min_avg', 
  # 'clearances_max_avg',
  'stride_times'
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

# loop through all subjects and all features
output_folder <- file.path("data", "processed", paste0("jags_output_left_foot_downsample_", as.character(downsample_step)))
dir.create(output_folder, showWarnings = FALSE)

# registerDoParallel(cores = 5)
# 
all_estimate_df <- data.frame()
# all_estimated_df_list <- foreach (subject = sub_list) %:% 
#   foreach (feature = features_list) %dopar% {
for (subject in sub_list) {
  for (feature in features_list) {
    print(subject)
    print(feature)
    # filter the dataframe
    dat_df <- filter(loc_df, sub == subject)
    dat_df <- dat_df[, c(feature, "fatigue", "condition")]
    dat_df <- rename(dat_df,c('y' = feature))
    print(paste("Current sample size:", nrow(dat_df)))
    
    # # viasualizing the data (optional)
    # with(dat_df, interaction.plot(fatigue, condition, y, main=paste(subject, feature)))
    # ggplot(dat_df, aes(y = y, x = condition, fill = fatigue), main=paste(subject, feature)) + geom_boxplot()
    
    # run jags
    # run_jags.options(silent.jags=TRUE, silent.runjags=TRUE)
    X <- model.matrix(~condition * fatigue, dat_df)   # X is the design matrix, including intercept
    data.r2jags <- run_jags(dat_df, X, file.path("likelihood_models", likelihood_models[[model_n]]))
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
    
    # # save jags output table
    # all_estimate_df <- get_jags_table(data.r2jags, subject, feature)  # use this line when running parallel with foreach
    # all_estimate_df
    # all_estimate_df <- bind_rows(all_estimate_df, get_jags_table(data.r2jags, subject, feature))
    # get_jags_table(data.r2jags, subject, feature)
  }
}
# all_estimate_df <- bind_rows(all_estimated_df_list)

# save estimated parameters table
write.csv(
  all_estimate_df, 
  file = file.path(output_folder, paste0("all_estimates_", model_names[[model_n]], ".csv")), 
  row.names=FALSE
)

print("...finished running")
toc()
