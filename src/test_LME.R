# options(warn = -1) # supress warnings from pt function

library(nlme)
library(ggplot2)

#### generate data with AR1 correlation
sim_ar1_data <- function(rho, n = 100) {
    # simulate AR1 error using arima.sim function

    sigma_e <- 1 # standard deviation of the error term
    sigma_b <- sigma_e / (1 - rho^2)
    ar1_err <- sigma_b * arima.sim(n = n, model = list(ar = rho))
    return(ar1_err)
}

# treatment with 2 levels
n_trt <- 50
trt <- factor(c(rep("A", n_trt), rep("B", n_trt)))

#### test fit of AR1 model
true_beta <- 2 # the true treatment effect
true_rho <- 0.7 # the true AR1 correlation coefficient
beta <- rho <- ar1_error <- NULL
for (i in 1:1000) {
    #### generate error with AR1 correlation
    # # continuous correlation
    # ar1_error <- sim_ar1_data(rho = true_rho, n = 2 * n_trt)
    # correlation within each level of trt
    ar1_error <- ts(c(sim_ar1_data(rho = true_rho, n = n_trt), sim_ar1_data(rho = true_rho, n = n_trt)))

    #### construct dummy data
    # with random error
    # Y <- rnorm(2 * n_trt) + true_beta * as.numeric(trt) + ar1_error
    # without random error
    Y <- true_beta * as.numeric(trt) + ar1_error

    #### lme function (must include random effects)
    # # correlation within each level of trt
    # fit <- lme(Y ~ trt, random = ~ 1 | trt, correlation = corAR1(form = ~ 1 | trt))
    # correlation within all data (doesn't work)
    # fit <- lme(Y ~ trt, random = ~ 1, correlation = corAR1(form = ~1))

    #### gls function (no random effects)
    fit <- gls(Y ~ trt, correlation = corAR1()) # this works
    # fit <- gls(Y ~ trt, correlation = corAR1(form = ~ 1 | trt))

    # only print in the first loop
    if (i == 1) {
        print("summary of the first fit:")
        print(summary(fit))
        plot(ar1_error)
    }

    #### collect the estimates
    # both gls and lme should work with this line:
    rho[i] <- coef(fit$model, unconstrained = FALSE)[["corStruct.Phi"]]
    # for lme model:
    # beta[i] <- fit$coefficients$fixed["trtB"]
    # for gls model:
    beta[i] <- fit$coefficients[["trtB"]]
}
print("mean beta:")
print(mean(beta)) # is this close to 2?
print("mean rho:")
print(mean(rho)) # is this close to rho?

#### plot the distribution of beta and rho with density plots, and compare with the true values

# create a data frame with the beta and rho vectors
df <- data.frame(beta, rho)

options(repr.plot.width = 2, repr.plot.height = 6)

# create a density plot for beta
beta_plot <- ggplot(df, aes(x = beta)) +
    geom_density(fill = "blue", alpha = 0.3) +
    geom_vline(
        aes(xintercept = mean(beta)),
        color = "red",
        linetype = "dashed"
    ) +
    ggtitle("Distribution of beta") +
    xlab("beta") +
    ylab("Density") +
    geom_text(
        aes(
            x = mean(beta),
            y = 0.5,
            label = paste("Mean: ", round(mean(beta), 2))
        ),
        color = "red",
        size = 4
    )

# create a density plot for rho
rho_plot <- ggplot(df, aes(x = rho)) +
    geom_density(fill = "blue", alpha = 0.3) +
    geom_vline(
        aes(xintercept = mean(rho)),
        color = "red",
        linetype = "dashed"
    ) +
    ggtitle("Distribution of rho") +
    xlab("rho") +
    ylab("Density") +
    geom_text(
        aes(
            x = mean(rho),
            y = 0.5,
            label = paste("Mean: ", round(mean(rho), 2))
        ),
        color = "red",
        size = 4
    )

# display the plots side by side
gridExtra::grid.arrange(beta_plot, rho_plot, ncol = 2)
