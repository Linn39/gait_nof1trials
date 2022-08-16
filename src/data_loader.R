#### functions to load and process the data ####

library(dplyr)

#### load gait data
load_gait_parameters <- function (folder_path, keyword) {
  
  #### first find file name by keyword ####
  file_path <- c()
  kw_fn_list <- list()  # keyword - file name pairs
  kw_fn_list[[ "LFRF_all_strides" ]] <- "df_all.csv"
  
  # look up file names
  if (keyword %in% names(kw_fn_list)) {
    file_name <- kw_fn_list[[keyword]]
  }
  else {
    return(paste("Keyword", keyword, "does not exist!"))
  }
  
  #### then load the data ####
if (keyword == "LFRF_all_strides") {
    folder_path <- file.path("data/processed", "features")
    data_path <- file.path(folder_path, file_name)
    dat_df <- readr::read_csv(data_path, col_names = T, show_col_types = FALSE)  
    
    # remove outlier strides
    dat_df <- dat_df[(dat_df$is_outlier == FALSE) & 
                       (dat_df$turning_interval == FALSE) & 
                       (dat_df$interrupted == FALSE),
    ]
  }
  return(dat_df)
}

print_data_summary <- function (df, var) {
  ### num. of samples, in each category, mean +- SD
  print(paste("Dataset total sample size:", nrow(loc_df)))
  print(sprintf("Dataset summary for %s:", var))
  # rename variable of interest
  names(df)[names(df) == var] <- "var"
  df %>%
    group_by(fatigue, condition) %>%
    summarize(
      n = length(var),
      mean = mean(var),
      sd = sd(var)
      ) %>%
    as.data.frame(.) %>% 
    mutate_if(is.numeric, round, 4) %>%
    print()
}

print_subject_summary <- function (df, var) {
  print(sprintf("Summary for %s:", var))
  # rename variable of interest
  names(df)[names(df) == var] <- "var"
  df %>%
    summarize(
      n = length(var),
      mean = mean(var),
      sd = sd(var),
      min = min(var),
      max = max(var)
    ) %>%
    as.data.frame(.) %>% 
    mutate_if(is.numeric, round, 4) %>%
    print()
}

downsample_rows <- function (df, step) {
  ### select every nth row in order to reduce dataset size
  new_df <- df[seq(1, nrow(df), step), ]
  return(new_df)
}

summary_mean <- function (df) {
  means_df <- df %>%
    group_by(sub, fatigue, condition) %>% 
    summarise_each(funs(mean))
  return(means_df)
}