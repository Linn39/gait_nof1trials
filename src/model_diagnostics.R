library(mcmcplots)
library(coda)

# import r2jags data
data.r2jags <- get(load(file.path("data", "processed", "jags_output", "data_r2jags_AR1.RData")))

# trace plot
traplot(data.r2jags, parms = c("beta", "sigma", "phi"))

# density plot
denplot(data.r2jags, parms = c("sigma"))


data.mcmc = as.mcmc(data.r2jags)

# raftery diagnostics
raftery.diag(data.mcmc)
