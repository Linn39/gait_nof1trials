# create and save Bayesian models for Monte Carlo Markov Chain

#### define the default likelihood model
modelString = " 
  model {
  #Likelihood
  for (i in 1:n) {
  y[i]~dnorm(mean[i],tau)
  mean[i] <- inprod(beta[],X[i,])
  }
  #Priors
  beta[1] ~ dnorm(0,1.0E-3)
  for(i in 2:ngroups) {
  beta[i] ~ dnorm(0,1.0E-3)
  }
  sigma ~ dunif(0, 100)
  tau <- 1 / (sigma * sigma)
  }
"

modelString_new = "
  model {

  #Priors
  for(i in 1:ngroups) {
  beta[i] ~ dnorm(0,1.0E-3)
  }
  sigma ~ dunif(0, 100)
  tau <- 1 / (sigma * sigma)

  #Likelihood
  for (i in 1:n) {
  y[i]~dnorm(mean[i],tau)
  mean[i] <- inprod(beta[],X[i,])
  }

  }
"
# write the model to a text file
writeLines(modelString, con = file.path("models", "fact_anovaModel_default.txt")) 
writeLines(modelString_new, con = file.path("models", "new_fact_anovaModel_default.txt")) 


#### model incorporating first order autoregressive (AR1) residual autocorrelation structure
# following tutorial at: https://agabrioblog.onrender.com/tutorial/autocorrelation-jags/autocorrelation-jags/
modelString_cauchy_t = "
model {
#Likelihood
for (i in 1:n) {
mu[i] <- inprod(beta[],X[i,])
}
y[1:n] ~ dmnorm(mu[1:n],Omega)
for (i in 1:n) {
for (j in 1:n) {
Sigma[i,j] <- sigma2*(1- phi*phi)*(equals(i,j) + (1-equals(i,j))*pow(phi,abs(i-j))) 
}
}
Omega <- inverse(Sigma)

#Priors
phi ~ dunif(-1,1)
for (i in 1:ngroups) {
beta[i] ~ dnorm(0,1.0E-6)
}
tau ~ dscaled.gamma(25, 1)  # half-Cauchy distribution (t distribution with 1.d.f.), scale = 25
sigma2 = 1/tau
}
"

modelString = "
model {
#Likelihood
for (i in 1:n) {
mu[i] <- inprod(beta[],X[i,])
}
y[1:n] ~ dmnorm(mu[1:n],Omega)
for (i in 1:n) {
for (j in 1:n) {
Sigma[i,j] <- sigma2*(1- phi*phi)*(equals(i,j) + (1-equals(i,j))*pow(phi,abs(i-j))) 
}
}
Omega <- inverse(Sigma)

#Priors
phi ~ dunif(-1,1)
for (i in 1:ngroups) {
beta[i] ~ dnorm(0,1.0E-6)
}
sigma <- z/sqrt(chSq)    # prior for sigma; cauchy = normal/sqrt(chi^2)
z ~ dnorm(0, 0.16)I(0,)  # half-normal distribution with only positive part, Cauchy scale = 2.5
chSq ~ dgamma(0.5, 0.5)  # chi^2 with 1 d.f.
sigma2 = pow(sigma,2)
#tau.cor <- tau #* (1- phi*phi)
}
"

modelString_new = "
model {
#Priors
phi ~ dunif(-1,1)
for (i in 1:ngroups) {
beta[i] ~ dnorm(0,1.0E-6)
}
z ~ dnorm(0, 0.16)I(0,)  # half-normal distribution with only positive part, Cauchy scale = 2.5
chSq ~ dgamma(0.5, 0.5)  # chi^2 with 1 d.f.
sigma <- z/sqrt(chSq)    # prior for sigma; cauchy = normal/sqrt(chi^2)
sigma2 = pow(sigma,2)
#Likelihood
for (i in 1:n) {
mu[i] <- inprod(beta[i],X[i,])
}
Omega <- inverse(Sigma)
y[1:n] ~ dmnorm(mu[1:n],Omega)
for (i in 1:n) {
for (j in 1:n) {
Sigma[i,j] <- sigma2*(equals(i,j) + (1-equals(i,j))*pow(phi,abs(i-j))) 
}
}
}
"

# write the model to a text file
writeLines(modelString_cauchy_t, con = file.path("models", "mixed_model_AR1_cauchy_t.txt"))
writeLines(modelString, con = file.path("models", "mixed_model_AR1.txt"))
writeLines(modelString_new, con = file.path("models", "new_mixed_model_AR1.txt"))
