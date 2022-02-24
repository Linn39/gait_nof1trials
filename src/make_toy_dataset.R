library(nlme)
library(tictoc)

# create toy dataset
set.seed(126)
n = 5000
a <- 20  #intercept
b <- 0.2  #slope
x <- round(runif(n, 1, n), 1)  #values of the year covariate
year <- 1:n
sigma <- 20
rho <- 0.8

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
write.table(data.temporalCor, file = "src/data.temporalCor.csv", 
                        sep = ",", quote = F, row.names = FALSE) 
pairs(data.temporalCor)


# run JAGS
library(R2jags)
source("src/jags_functions.R")  # import costum functions

# select a model
model_n <- 3

likelihood_models <- list(
  "fact_anovaModel_default.txt",      #1 
  "new_fact_anovaModel_default.txt",  #2
  "mixed_model_lagged_res.txt",       #3
  "mixed_model_AR1.txt",              #4
  "new_mixed_model_AR1.txt",          #5
  "mixed_model_CS.txt"                #6
)

model_names <- list(
  "default",
  "lagged_res",
  "AR1",
  "CS"
)
print(likelihood_models[[model_n]])

run_jags_tictoc <- function(data, model_number, text){
  
  tic(sprintf("start running %s", text))
  print(sprintf("start running %s...", text))
  
  data.r2jags <- run_jags(data, file.path("likelihood_models", likelihood_models[[model_number]]))
  
  print(sprintf("...finished running %s", text))
  toc()
  return(data.r2jags)
}

jags_old_ar1 <- run_jags_tictoc(data.temporalCor, 1, "AR1 old")
jags_new_ar1 <- run_jags_tictoc(data.temporalCor, 2, "AR1 old")

tic("start running")
print("start running...")

data.r2jags <- run_jags(data.temporalCor, file.path("likelihood_models", likelihood_models[[model_n]]))

print("...finished running")
toc()




