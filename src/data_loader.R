#### functions to load and process the data ####

library(ggplot2)
library(plyr)
library(dplyr)

#### put data from all subjects and runs together, save as .csv
collect_all_data <- function(read_path, save_path, subs, runs) {
  all_df <- data.frame()
  for (sub in sub_list) {
    for (run in runs) {
      for (foot in list(list("left_foot", "LF"), list("right_foot", "RF"))) {
        df <- readr::read_csv(
          file.path(
            read_path,
            sub,
            run,
            paste0(foot[1], "_core_params_py_n.csv")
          ),
          col_names = TRUE,
          show_col_types = FALSE
        )
        df["sub"] <- sub
        df["run"] <- run
        df["foot"] <- foot[2]
        all_df <- bind_rows(all_df, df)
      }
    }
  }
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }
  write.csv(all_df, file.path(save_path, "df_all_strides.csv"))
}


#### load gait data
load_gait_parameters <- function(read_path, keyword) {

  #### first find file name by keyword ####
  file_path <- c()
  kw_fn_list <- list() # keyword - file name pairs
  kw_fn_list[["LFRF_all_strides"]] <- "df_all_strides.csv"

  # look up file names
  if (keyword %in% names(kw_fn_list)) {
    file_name <- kw_fn_list[[keyword]]
  } else {
    return(paste("Keyword", keyword, "does not exist!"))
  }

  #### then load the data ####
  if (keyword == "LFRF_all_strides") {
    dat_df <- readr::read_csv(
      file.path(read_path, file_name),
      col_names = TRUE,
      show_col_types = FALSE
    )

    # remove outlier strides
    dat_df <- dat_df[(dat_df$is_outlier == FALSE) &
      (dat_df$turning_interval == FALSE) &
      (dat_df$interrupted == FALSE), ]
  }
  return(dat_df)
}

print_data_summary <- function(df, var) {
  ### num. of samples, in each category, mean +- SD
  print(paste("Dataset total sample size:", nrow(df)))
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

  # print number of stride per person
  print("Number of strides per person under each walking condition:")
  count_per_sub_df <- df %>% dplyr::count(sub, fatigue, condition) # nolint
  count_per_sub_df %>%
    group_by(fatigue, condition) %>%
    summarize(
      mean = mean(n),
      sd = sd(n)
    ) %>%
    as.data.frame(.) %>%
    mutate_if(is.numeric, round, 4) %>%
    print()
}

plot_over_time <- function(df, var) {
  # plot the variable of interest by row index
  for (sub in unique(df$sub)) {
    sub_df <- df[df$sub == sub, ]
    scatter_plot <- ggplot(
      sub_df,
      aes(
        x = seq_len(nrow(sub_df)), y = sub_df[[var]],
        color = fatigue, shape = condition
      )
    ) +
      geom_point(size = 3) +
      labs(title = paste0("Data points ", sub), x = "Sample Number", y = var)

    # save the plot
    save_dir <- file.path("data", "figures", "dist_over_time")
    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
    }
    ggsave(
      file.path(save_dir, paste0("over_time_", var, "_", sub, ".pdf")),
      plot = scatter_plot,
      width = 6,
      height = 3
    )
  }
}

print_subject_summary <- function(df, var) {
  print(sprintf("Summary for %s:", var))
  # rename variable of interest
  names(df)[names(df) == var] <- "var"
  df %>%
    summarize(
      n = length(var),
      mean = mean(var),
      sd = sd(var),
      min = min(var),
      max = max(var),
      median = median(var)
    ) %>%
    as.data.frame(.) %>%
    mutate_if(is.numeric, round, 4) %>%
    print()
}

downsample_rows <- function(df, step, plot = FALSE) {
  ### select every nth row in order to reduce dataset size
  new_df <- df[seq(1, nrow(df), step), ]

  if (plot) {
    #### plot the distribution of the data before and after downsampling
    # add a new column to each dataframe to indicate the source
    df$source <- "original"
    new_df$source <- "downsampled"
    # combine the data from the two dataframes into a single dataframe
    combined_df <- rbind(df, new_df)

    for (col in c("stride_lengths", "stride_times")) {
      for (sub in unique(combined_df$sub)) {
        # filter data with one subject
        sub_df <- combined_df[combined_df$sub == sub, ]

        # rename the column to "var" for convenience
        names(sub_df)[names(sub_df) == col] <- "var"

        # calculate group means
        group_means <- ddply(sub_df, .(source), summarise, var.mean = mean(var))

        # create the plot
        dist_plot <- ggplot(sub_df, aes(x = var, fill = source)) +
          geom_density(alpha = 0.5) +
          geom_vline(
            data = group_means, aes(xintercept = var.mean, colour = source),
            linetype = "dashed", size = 1
          ) +
          labs(
            title = paste0("Distribution of Data ", sub),
            x = col,
            fill = "source"
          )

        # save the plot to a file
        save_dir <- file.path("data", "figures", "dist_downsample")
        if (!dir.exists(save_dir)) {
          dir.create(save_dir, recursive = TRUE)
        }
        ggsave(
          file.path(
            save_dir,
            paste0(
              "dist_downsample_",
              col,
              "_",
              sub,
              ".pdf"
            )
          ),
          plot = dist_plot, width = 4, height = 3
        )

        # restore the column name
        names(sub_df)[names(sub_df) == "var"] <- col
      }
    }
  }

  return(new_df)
}

summary_mean <- function(df) {
  means_df <- df %>%
    group_by(sub, fatigue, condition) %>%
    summarise_each(funs(mean))
  return(means_df)
}