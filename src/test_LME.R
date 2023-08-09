options(warn = -1) # supress warnings from pt function

library(nlme)

#### generate data with AR1 correlation
sim_ar1_data <- function(rho, n = 100) {
    # simulate AR1 error using arima.sim function

    sigma_e <- 1 # standard deviation of the error term
    sigma_b <- sigma_e / (1 - rho^2)
    ar1_err <- sigma_b * arima.sim(n = n, model = list(ar = rho))
    return(ar1_err)
}

dummy_ar1_data <- function(rho, n = 100) {
    # simulate AR1 error manually

    # du
    sigma_e <- 1 # standard deviation of the error term
    cor_ar1 <- corAR1(form = ~1, value = rho)
}

# treatment with 2 levels
n_trt <- 50
trt <- factor(c(rep("A", n_trt), rep("B", n_trt)))

#### test fit of AR1 model
true_beta <- 2 # the true treatment effect
true_rho <- 0.7 # the true AR1 correlation coefficient
beta <- rho <- ar1_error <- NULL
for (i in 1:1000) {
    # # continuous correlation
    # ar1_error <- sim_ar1_data(rho = true_rho, n = 2 * n_trt)
    # correlation within each level of trt
    ar1_error <- ts(c(sim_ar1_data(rho = true_rho, n = n_trt), sim_ar1_data(rho = true_rho, n = n_trt)))

    # dummy data
    # Y <- rnorm(2 * n_trt) + true_beta * as.numeric(trt) + ar1_error
    Y <- true_beta * as.numeric(trt) + ar1_error

    # # correlation within each level of trt
    # fit <- lme(Y ~ trt, random = ~ 1 | trt, correlation = corAR1(form = ~ 1 | trt))
    # correlation within all data (doesn't work)
    # fit <- lme(Y ~ trt, random = ~ 1, correlation = corAR1(form = ~1))

    # fit <- gls(Y ~ trt, correlation = corARMA(p = 1))   # this works
    fit <- gls(Y ~ trt, correlation = corAR1())     # this also works, same as using corARMA(p = 1)

    # only print in the first loop
    if (i == 1) {
        print("summary of the first fit:")
        print(summary(fit))
        plot(ar1_error)
    }

    # collect the estimates
    # beta[i] <- fit$coefficients$fixed["trtB"]  # for lme model
    rho[i] <- coef(fit$modelStruct$corStruct)[1]
    beta[i] <- fit$coefficients[["trtB"]]  # for gls model. The same as fit$coefficients$fixed["trtB"]
}
print("mean beta:")
print(mean(beta)) # is this close to 2?
print("mean rho:")
print(mean(rho)) # is this close to RHO?
