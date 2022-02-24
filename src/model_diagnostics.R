install.load::install_load("mcmcplots", "coda", "R2jags")

# import r2jags data

data.r2jags <- get(load(file.path("data", "processed", "jags_output", "data_r2jags_AR1.RData")))

data.mcmc = as.mcmc(data.r2jags)


# trace plot
traplot(data.r2jags.large, parms = c("beta", "sigma", "phi"))


# density plot
denplot(data.r2jags.large, parms = c("beta", "sigma", "phi"))
denplot(data.r2jags.large, parms = c("deviance"))

# raftery diagnostics
raftery.diag(data.mcmc.large)

### MORE DIAGNOSTICS TO CHECK

# I heavily relied on this: https://static1.squarespace.com/static/610c1467ea1d4a02e9c1a010/t/610c3e967c967e2ab20086d6/1628192407632/BayesLabDay3.pdf


# R > 1.1 is cause for concern over non-convergence
#  Approximate convergence is diagnosed when the 
# upper limit is close to 1
gelman.diag(data.mcmc.large, transform = T)
gelman.diag(samp)


x11()
gelman.plot(data.mcmc.large)
dev.off()

# test if the draws for each parameter come from a stationary 
# distribution
heidel.diag(data.mcmc.large)

# https://www.rensvandeschoot.com/tutorials/wambs-rjags/
# This syntax will provide a z-score, where a z-score
# higher than the absolute value of 1.96 is associated
# with a p-value of < .05 (two-tailed). These values 
# should therefore be lower than the absolute value of 1.96.
geweke.diag(data.mcmc.large)
x11()

# If Geweke indicates that the first and last part of a 
# sample from a Markov chain are not drawn from the same 
# distribution, it may be useful to discard the first few 
# iterations to see if the rest of the chain has “converged”. 
# This plot shows what happens to Geweke’s z-score when 
# successively larger numbers of iterations are discarded 
# from the beginning of the chain.
pdf("geweke_plot.pdf")
geweke.plot(data.mcmc.large)
dev.off()


### THE TUTORIALS BELOW WERE NOT THAT INFORMATIVE BUT
### AT LEAST SHOWED MORE THINGS WE COULD TRY

#http://www.jkarreth.net/files/bayes-cph_Tutorial-JAGS.pdf
install.load::install_load("superdiag", "lattice")

superdiag(data.mcmc.large, burnin = 1000)

xyplot(data.mcmc.large)

densityplot(data.mcmc.large)

#https://www.johnmyleswhite.com/notebook/2010/08/29/mcmc-diagnostics-in-r-with-the-coda-package/
recompile(data.r2jags.large)
samp <- coda.samples(data.r2jags.large$model, c("beta", "sigma", "phi"),
                     n.iter=1000)
png('plot_1.png')
plot(samp)
dev.off()
