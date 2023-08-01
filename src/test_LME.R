library(nlme)

#### generate data with AR1 correlation
sim_ar1_data <- function(rho, n=100){
#   n <- 100
  sigma <- 1   # residual standard deviation
  sigma_e <- 1   # standard deviation of the error term
  sigma_b <- sigma_e / sqrt(1 - rho^2)
  sigma_b * arima.sim(n=n, list(ar=rho), sd=sigma)
}

dummy_ar1_data <- function(rho, n=100){

}

# treatment with 2 levels
trt <- factor(c(rep("A",50), rep("B",50)))

#### test fit of AR1 model
beta <- rho <- NULL
for(i in 1:10){
#   print(i)
  Y <- rnorm(100) + 2 * as.numeric(trt) + sim_ar1_data(rho=0.5, n=100)
  fit <- lme(Y ~ trt, random = ~1|trt, correlation = corAR1(form = ~ 1 | trt))  # form = ~ 1 | trt
  print(summary(fit))
  print(fit$corStruct)
  beta[i] <- fit$coefficients$fixed["trtB"]
  rho[i] <- coef(fit$modelStruct$corStruct)[1]
}
print("mean beta:")
print(mean(beta)) # is this close to 2?
print("mean rho:")
print(mean(rho)) # is this close to RHO?