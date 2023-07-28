rm(list = ls())

suppressMessages(library(dplyr))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(reshape2)
library(colorspace)
source("src/data_loader.R")
source("src/jags_functions.R")

#### import posterior estimates
sub_list <- list(
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

var_list <- list(
    list("stride_lengths", "SL"),
    list("stride_times", "ST")
)

model <- "AR1_diff_prob"

downsample_n <- 5
read_path <- file.path(
    "data",
    "processed",
    paste0("test_left_foot_downsample_", downsample_n)
)

save_fig_dir <- file.path(
    "data",
    "figures",
    "posteriors"
)
if (!dir.exists(save_fig_dir)) {
    dir.create(save_fig_dir)
}

#### load posterior estimates
if (file.exists(
    file.path( # if summary of the posterior estimates exists, load summary
        read_path,
        paste0("all_estimates_", model, ".csv")
    )
)) {
    all_estimate_df <- read.csv(
        file.path(
            read_path,
            paste0("all_estimates_", model, ".csv")
        )
    )
} else { # if summary of the posterior estimates doesn't exist, create summary
    all_estimate_df <- data.frame()
    for (variable in var_list) {
        for (sub in sub_list) {
            data.r2jags <- get(load(file.path(read_path, paste0("data_r2jags_AR1_diff_prob_", variable[[2]], "_", sub, ".RData"))))
            all_estimate_df <- bind_rows(all_estimate_df, get_jags_table(data.r2jags, sub, variable[[1]]))
        }
    }

    # save all posterior estimates into a .csv file
    write.csv(
        all_estimate_df,
        file = file.path(
            read_path,
            paste0("all_estimates_", model, ".csv")
        ),
        row.names = FALSE
    )
}

#### get distribution of all 4 conditions
beta1 <- filter(all_estimate_df, parameter == "beta[1]")
beta2 <- filter(all_estimate_df, parameter == "beta[2]")
beta3 <- filter(all_estimate_df, parameter == "beta[3]")
beta4 <- filter(all_estimate_df, parameter == "beta[4]")

conditions_df <- beta1[, c("sub", "var")]

value <- "mean"
conditions_df["st_control"] <- beta1[value]
conditions_df["st_fatigue"] <- beta1[value] + beta3[value]
conditions_df["dt_control"] <- beta1[value] + beta2[value]
conditions_df["dt_fatigue"] <- beta1[value] + beta2[value] + beta3[value] + beta4[value]
conditions_df_mean <- gather(conditions_df,
    key = "condition", value = "mean",
    st_control, st_fatigue, dt_control, dt_fatigue
)

value <- "sd"
conditions_df["st_control"] <- sqrt(beta1[value]^2)
conditions_df["st_fatigue"] <- sqrt(beta1[value]^2 + beta3[value]^2)
conditions_df["dt_control"] <- sqrt(beta1[value]^2 + beta2[value]^2)
conditions_df["dt_fatigue"] <- sqrt(beta1[value]^2 + beta2[value]^2 + beta3[value]^2 + beta4[value]^2)
conditions_df_sd <- gather(conditions_df,
    key = "condition", value = "sd",
    st_control, st_fatigue, dt_control, dt_fatigue
)

plot_df <- merge(conditions_df_mean, conditions_df_sd)

# add mean over all subjects
conditions_df_mean_all <- aggregate(mean ~ condition + var, conditions_df_mean, mean)
conditions_df_mean_all["sub"] <- "all"

# add sd of means over all subjects
conditions_df_sd_all <- aggregate(mean ~ condition + var, conditions_df_mean, sd) # SD of means
names(conditions_df_sd_all)[names(conditions_df_sd_all) == "mean"] <- "sd"
conditions_df_sd_all["sub"] <- "all"

conditions_df_all <- merge(conditions_df_mean_all, conditions_df_sd_all)

# put all dataframes together
plot_df <- bind_rows(plot_df, conditions_df_all)

# save estimated parameters table
write.csv(
    plot_df,
    file = file.path(read_path, paste0("all_estimates_gait_parameters_", model, ".csv")),
    row.names = FALSE
)

# rename for plotting
# Rename the columns using colnames
colnames(plot_df)[colnames(plot_df) == "sub"] <- "Participant"
colnames(plot_df)[colnames(plot_df) == "condition"] <- "Condition"
plot_df[plot_df == "stride_lengths"] <- "Stride Length [m]"
plot_df[plot_df == "stride_times"] <- "Stride Time [s]"
plot_df[plot_df == "st_control"] <- "ST-Control"
plot_df[plot_df == "st_fatigue"] <- "ST-Fatigue"
plot_df[plot_df == "dt_control"] <- "DT-Control"
plot_df[plot_df == "dt_fatigue"] <- "DT-Fatigue"
plot_df[plot_df == "all"] <- "All"

#### Plot posterior estimates of the gait parameters
fig_posterior <- ggplot(plot_df, aes(x = Participant, fill = Condition)) +
    geom_boxplot(aes(ymin = mean - 3 * sd, lower = mean - sd, middle = mean, upper = mean + sd, ymax = mean + 3 * sd),
        stat = "identity", width = 0.6, lwd = 0.3
    ) +
    ylab("Posterior Estimates") +
    scale_fill_brewer(palette = "Spectral") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.6))
fig_posterior2 <- fig_posterior + facet_wrap(vars(var), nrow = 2, strip.position = "left", scales = "free_y") +
    theme(
        strip.background = element_rect(fill = "white"),
        strip.placement = "outside",
        panel.spacing.y = unit(1, "lines")
    )

ggsave(
    filename = file.path(save_fig_dir, paste0("posterior_estimates_", model, ".pdf")),
    plot = fig_posterior2,
    width = 8,
    height = 6,
    # units = "in",
    dpi = 300
)

#### Plot observed values of the gait parameters
all_observations_df <- read_csv(file.path("data", "processed", "features", "df_all_strides.csv"), show_col_types = FALSE)
all_observations_df <- downsample_rows(all_observations_df[all_observations_df$foot == "left", ], downsample_n) # reduce data size to match the data used for JAGS models
all_observations_df["conditions"] <- paste(all_observations_df$condition, all_observations_df$fatigue, sep = "_")

# add mean over all subjects
all_observations_df_all <- all_observations_df
all_observations_df_all["sub"] <- "All"
all_observations_df <- rbind(all_observations_df, all_observations_df_all)

# melt gait parameters of interest into one column for facet plot
all_observations_df <- melt(all_observations_df, id.vars = c("sub", "conditions"), measure.vars = c("stride_lengths", "stride_times"))
all_observations_df$variable <- as.character(all_observations_df$variable)

# rename for plotting
colnames(all_observations_df)[colnames(all_observations_df) == "sub"] <- "Participant"
colnames(all_observations_df)[colnames(all_observations_df) == "conditions"] <- "Condition"
all_observations_df[all_observations_df == "st_control"] <- "ST-Control"
all_observations_df[all_observations_df == "st_fatigue"] <- "ST-Fatigue"
all_observations_df[all_observations_df == "dt_control"] <- "DT-Control"
all_observations_df[all_observations_df == "dt_fatigue"] <- "DT-Fatigue"
all_observations_df[all_observations_df == "stride_lengths"] <- "Stride Length [m]"
all_observations_df[all_observations_df == "stride_times"] <- "Stride Time [s]"

fig_observation <- ggplot(all_observations_df, aes(x = Participant, y = value, fill = Condition)) +
    geom_boxplot(width = 0.6, lwd = 0.3, outlier.size = 0.5) +
    ylab("Observed Values") +
    scale_fill_brewer(palette = "Spectral") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.6))

fig_observation2 <- fig_observation + facet_wrap(vars(variable), nrow = 2, strip.position = "left", scales = "free_y") +
    theme(
        strip.background = element_rect(fill = "white"),
        strip.placement = "outside",
        panel.spacing.y = unit(1, "lines")
    )

ggsave(
    filename = file.path(save_fig_dir, "observed_all_subs_SL_ST.pdf"),
    plot = fig_observation2,
    width = 8,
    height = 6,
    # units = "in",
    dpi = 300
)

#### Posterior Predictive Check: Combine Posterior with Observations
for (var in list("Stride Length [m]", "Stride Time [s]")) {
    # set the same y axis scale for both plots, take the larger one as the limit
    y_limits <- c(
        min(
            min(all_observations_df[all_observations_df$variable == var, ]$value),
            min(plot_df[plot_df$var == var, ]$mean - 3 * plot_df[plot_df$var == var, ]$sd)
        ),
        max(
            max(all_observations_df[all_observations_df$variable == var, ]$value),
            max(plot_df[plot_df$var == var, ]$mean + 3 * plot_df[plot_df$var == var, ]$sd)
        )
    )
    # boxplot for observed values
    fig_observation_var <- ggplot(
        all_observations_df[all_observations_df$variable == var, ],
        aes(
            x = Participant,
            y = value,
            fill = Condition
        )
    ) +
        geom_boxplot(width = 0.6, lwd = 0.3, outlier.size = 0.5) +
        ylim(y_limits) +
        ylab(var) +
        scale_fill_brewer(palette = "Spectral") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.6))

    # boxplot for posterior estimates
    fig_posterior_var <- ggplot(plot_df[plot_df$var == var, ], aes(x = Participant, fill = Condition)) +
        geom_boxplot(aes(ymin = mean - 3 * sd, lower = mean - sd, middle = mean, upper = mean + sd, ymax = mean + 3 * sd),
            stat = "identity", width = 0.6, lwd = 0.3
        ) +
        ylim(y_limits) +
        ylab(var) +
        scale_fill_brewer(palette = "Spectral") +
        theme_bw() +
        theme(
            axis.text.x = element_text(angle = 45, vjust = 0.6),
            plot.margin = unit(c(0.5, 0.5, 2, 0.5), "lines")
        )

    # combine both plots
    fig_combined <- ggarrange(fig_posterior_var, fig_observation_var,
        labels = c("A", "B"),
        ncol = 1, nrow = 2,
        heights = c(1, 0.9)
    )
    # save figure
    ggsave(
        filename = file.path(save_fig_dir, paste0("posterior_observation_", var, "_", model, ".pdf")),
        plot = fig_combined,
        width = 8,
        height = 6,
        # units = "in",
        dpi = 300
    )
}

# Heatmap of the gait parameter changes
vars <- list("stride_lengths", "stride_times")

# rename conditions for plotting
conditions_df_mean[conditions_df_mean == "st_control"] <- "ST-Control"
conditions_df_mean[conditions_df_mean == "st_fatigue"] <- "ST-Fatigue"
conditions_df_mean[conditions_df_mean == "dt_control"] <- "DT-Control"
conditions_df_mean[conditions_df_mean == "dt_fatigue"] <- "DT-Fatigue"

# generate unique combinations of the conditions
cond_table <- table(unique(conditions_df_all["condition"]))
idx_comb <- combn(unique(c(1, 2, 3, 4)), 2)

diff_df_subs <- list()
diff_df_params <- list()
subs <- unique(conditions_df_mean$sub)
for (j in 1:length(vars)) {
    for (i in 1:length(subs)) {
        sub_df <- conditions_df_mean[conditions_df_mean$sub == subs[i] & conditions_df_mean$var == vars[[j]], ]
        diff <- sub_df$mean[unlist(idx_comb[1, ])] - sub_df$mean[unlist(idx_comb[2, ])]
        diff_names <- paste(sub_df$condition[unlist(idx_comb[1, ])], sub_df$condition[unlist(idx_comb[2, ])], sep = " - ")
        sub_diff_df <- as.data.frame(setNames(diff, diff_names)) # create dataframe for this subject
        names(sub_diff_df) <- subs[i] # rename column to subject
        diff_df_subs[[i]] <- sub_diff_df
    }
    diff_df <- cbind.data.frame(diff_df_subs)
    diff_df$cond_diff <- rownames(diff_df)
    diff_df_plot_var <- gather(diff_df, sub, value, sub_01:sub_18, factor_key = TRUE)
    # browser()
    diff_df_plot_var["variable"] <- vars[[j]]
    diff_df_params[[j]] <- diff_df_plot_var
}
diff_df_plot <- bind_rows(diff_df_params)

# rename for plotting
# diff_df_plot <- diff_df_plot %>%
#   rename(
#     Participant = sub,
#     Conditions = cond_diff,
#     Difference = value
#     )
colnames(diff_df_plot)[colnames(diff_df_plot) == "sub"] <- "Participant"
colnames(diff_df_plot)[colnames(diff_df_plot) == "cond_diff"] <- "Conditions"
colnames(diff_df_plot)[colnames(diff_df_plot) == "value"] <- "Difference"
diff_df_plot[diff_df_plot == "stride_lengths"] <- "Stride Length [m]"
diff_df_plot[diff_df_plot == "stride_times"] <- "Stride Time [s]"

fig_diff <- ggplot(diff_df_plot, aes(Participant, Conditions, fill = Difference)) +
    geom_tile(color = "white") +
    coord_equal() +
    theme_bw() +
    scale_fill_continuous_divergingx(palette = "Spectral", mid = 0, l1 = 0, l3 = 0, p3 = 0.8, p4 = 0.6) +
    theme(axis.text.x = element_text(vjust = 0.6, angle = 45))

fig_diff2 <- fig_diff + facet_grid(rows = vars(variable), switch = "y") +
    theme(
        strip.background = element_rect(fill = "white"),
        strip.placement = "outside",
        panel.spacing.y = unit(1, "lines")
    )

# save figure
ggsave(
    filename = file.path(save_fig_dir, paste0("heatmap_diff_SL_ST_AR1.pdf")),
    plot = fig_diff2,
    width = 8,
    height = 6,
    # units = "in",
    dpi = 300
)

#### Probability of meaningful change
# rename variables for plotting
all_estimate_df[all_estimate_df == "stride_lengths"] <- "Stride Length"
all_estimate_df[all_estimate_df == "stride_times"] <- "Stride Time"

for (cond in c("fatigue", "cognitive_task")) {
    fig_posterior_prob <- ggplot(filter(all_estimate_df, (parameter == paste0("p_", cond)))) +
        geom_point(aes(x = sub, y = mean, colour = factor(var)), size = 3, shape = 16) +
        # facet_grid(rows = vars(var), switch = "y") +
        scale_color_manual(values = c("Stride Length" = "#009E73", "Stride Time" = "#E69F00")) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
        theme(
            strip.background = element_rect(fill = "white"),
            strip.placement = "outside",
            panel.spacing.y = unit(1, "lines"),
            # legend.position = "none"
        ) +
        labs(color = "Gait Parameter", x = "Participant", y = "Posterior Probability of Meaningful Change")

    # save figure
    ggsave(
        filename = file.path(save_fig_dir, paste0("posterior_prob_meaningful_change_", cond, "_AR1.pdf")),
        plot = fig_posterior_prob,
        width = 8,
        height = 4,
        # units = "in",
        dpi = 300
    )
}

# compare posterior probability of meaningful change with
# absolute change of the posterior estimates (mean values)

all_estimate_df[all_estimate_df == "stride_lengths"] <- "Stride Length"
all_estimate_df[all_estimate_df == "stride_times"] <- "Stride Time"

for (cond in c("fatigue", "cognitive_task")) {
    fig_posterior_prob <- ggplot(filter(all_estimate_df, (parameter == paste0("p_", cond)))) +
        geom_point(aes(x = sub, y = mean, colour = var), size = 2, shape = 16) +
        # facet_grid(rows = vars(var), switch = "y") +
        scale_color_manual(values = c("Stride Length" = "#009E73", "Stride Time" = "#E69F00")) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
        ylab("Posterior probability") +
        ggtitle("Posterior probability of meaningful change")

    # calculate mean changes from the observed data
    diff_fatigue_df <- data.frame()
    for (variable in var_list) {
        diff_fatigue <- list()
        for (subject in sub_list) {
            df <- filter(conditions_df_mean, (var == variable[[1]] & sub == subject))
            df_spread <- spread(df, key = condition, value = mean)
            diff_fatigue <- append(
                diff_fatigue,
                abs(df_spread[["ST-Fatigue"]] + df_spread[["DT-Fatigue"]] - df_spread[["ST-Control"]] - df_spread[["DT-Control"]]) / (df_spread[["ST-Control"]] + df_spread[["DT-Control"]])
            )
        }
        diff_fatigue_df <- bind_rows(diff_fatigue_df, as.data.frame(diff_fatigue, col.names = sub_list, row.names = variable[[1]]))
        diff_fatigue_df["var"] <- rownames(diff_fatigue_df)
    }

    diff_fatigue_plot_df <- melt(diff_fatigue_df, id.vars = "var", variable.name = "sub")
    diff_fatigue_plot_df$value <- diff_fatigue_plot_df$value * 100 # convert to percentage
    diff_fatigue_plot_df[diff_fatigue_plot_df == "stride_lengths"] <- "Stride Length"
    diff_fatigue_plot_df[diff_fatigue_plot_df == "stride_times"] <- "Stride Time"

    # plot mean changes from the posterior estimates
    fig_posterior_diff_fatigue <- ggplot(diff_fatigue_plot_df, aes(x = sub, y = value, color = var)) +
        geom_point(size = 2, shape = 16) +
        scale_color_manual(values = c("Stride Length" = "#009E73", "Stride Time" = "#E69F00")) +
        geom_hline(yintercept = 3, color = "#009E73") +
        geom_hline(yintercept = 2, color = "#E69F00") +
        ylab("Mean absolute change (%)") +
        ggtitle("Mean absolute change after fatigue from observed data") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.6))

    # combine the two plots
    fig_combined <- ggarrange(fig_posterior_prob, fig_posterior_diff_fatigue,
        labels = c("A", "B"),
        ncol = 1, nrow = 2,
        heights = c(1, 0.9)
    )

    # save figure
    ggsave(
        filename = file.path(save_fig_dir, paste0("compare_meaningful_change_", cond, "_AR1.pdf")),
        plot = fig_combined,
        width = 8,
        height = 6,
        # units = "in",
        dpi = 300
    )
}