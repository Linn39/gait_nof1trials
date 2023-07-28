#### Statistics summary and ANOVA of the data ####
rm(list = ls())

# load libraries and functions
library(ez)
source("./src/data_loader.R")

# load data
downsample_step <- 5
kw <- "LFRF_all_strides"

# select list of features / gait paarameters
features_list <- c(
  "stride_lengths",
  "stride_times"
)

# read data from file
folder_path <- "data/processed/features"
full_df <- load_gait_parameters(folder_path, kw)

# summary statistics of the participants
sub_characteristics <- c("age", "height(cm)", "weight(kg)", "leg_length(cm)")
sub_df <- unique(full_df[, append(sub_characteristics, "sub")])

for (sub_char in sub_characteristics) {
  print_subject_summary(sub_df, sub_char)
}

# summary statistics of the data
df <- downsample_rows(
  full_df[full_df$foot == "left", ], downsample_step,
  plot = FALSE
)
print(paste("Downsample by", downsample_step))
print("Summary of data from all subjects after downsampling:")

# two-way repeated measures ANOVAA
results_2_way_anova <- data.frame()

for (var_name in features_list) {
  print_data_summary(df, var_name)
  plot_over_time(df, var_name)

  colnames(df)[
    colnames(df) %in% c("height(cm)", "weight(kg)")
  ] <- c("height_cm", "weight_kg") # rename columns

  dat_df <- select(
    df,
    "var" = var_name,
    "sub", "condition", "fatigue", "age", "sex", "height_cm", "weight_kg"
  )

  res_aov <- ezANOVA(
    data = dat_df,
    dv = .(var),
    wid = .(sub),
    within = .(condition, fatigue),
    within_covariates = .(age, height_cm, weight_kg),
    detailed = TRUE,
    type = 2
  )

  anova_res <- data.frame(res_aov)
  anova_res$variable <- var_name
  results_2_way_anova <- dplyr::bind_rows(results_2_way_anova, anova_res)
}
options(width = 1000) # set the width of the console to display all columns
print("ANOVA Summary:")
print(results_2_way_anova)