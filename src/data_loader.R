library(dplyr)

#### load gait data
load_gait_parameters <- function (folder_path, keyword, subject, test, cond) {
  
  #### first find file name by keyward ####
  file_path <- c()
  kw_fn_list <- list()  # keyword - file name pairs
  kw_fn_list[[ "LF" ]] <- "left_foot_core_params_py_n.csv"
  kw_fn_list[[ "RF" ]] <- "right_foot_core_params_py_n.csv"
  kw_fn_list[[ "LFRF_windowed" ]] <- "agg_windows_control_fatigue_st_dt_10_2.csv"
  kw_fn_list[[ "LFRF_all_strides" ]] <- "df_all.csv"
  
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
  else if (keyword == "LFRF_all_strides") {
    folder_path <- file.path("data/processed", "features")
    data_path <- file.path(folder_path, file_name)
    dat_df <- readr::read_csv(data_path, col_names = T, show_col_types = FALSE)  
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

print_data_summary <- function (df, var) {
  ### num. of samples, in each category, mean +- SD
  print(paste("Dataset total sample size:", nrow(loc_df)))
  print(sprintf("Dataset summary for %s:", var))
  # rename variable of interest
  names(df)[names(df) == var] <- "var"
  df %>%
    group_by(fatigue, condition) %>%
    summarize(
      n = n(),
      mean = mean(var),
      sd = sd(var)
      ) %>%
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

