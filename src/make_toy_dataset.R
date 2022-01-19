# create toy dataset
set.seed(126)
n = 50
a <- 20  #intercept
b <- 0.2  #slope
x <- round(runif(n, 1, n), 1)  #values of the year covariate
year <- 1:n
sigma <- 20
rho <- 0.8

library(nlme)
## define a constructor for a first-order
## correlation structure
ar1 <- corAR1(form = ~year, value = rho)
## initialize this constructor against our data
AR1 <- Initialize(ar1, data = data.frame(year))
## generate a correlation matrix
V <- corMatrix(AR1)
## Cholesky factorization of V
Cv <- chol(V)
## simulate AR1 errors
e <- t(Cv) %*% rnorm(n, 0, sigma)  # cov(e) = V * sig^2
## generate response
y <- a + b * x + e
data.temporalCor = data.frame(y = y, x = x, year = year)
write.table(data.temporalCor, file = "data.temporalCor.csv", 
                        sep = ",", quote = F, row.names = FALSE) 
pairs(data.temporalCor)


# run JAGS
library(R2jags)
source("src/jags_functions.R")  # import costum functions

# select a model
model_n <- 4

likelihood_models <- list(
  "fact_anovaModel_default.txt",
  "mixed_model_lagged_res.txt",
  "mixed_model_AR1.txt",
  "mixed_model_CS.txt"
)

model_names <- list(
  "default",
  "lagged_res",
  "AR1",
  "CS"
)
print(likelihood_models[[model_n]])

tic("start running")
print("start running...")

data.r2jags <- run_jags(data.temporalCor, file.path("likelihood_models", likelihood_models[[model_n]]))

print("...finished running")
toc()




