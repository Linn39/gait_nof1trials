# create and save Bayesian models

#### create folder to save the models
dir.create(file.path("./models/"), showWarnings = FALSE)

#### define the default likelihood model
model_string = " 
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

model_string_informative_priors = "
model {
#Likelihood
for (i in 1:n) {
y[i]~dnorm(mean[i],tau)
mean[i] <- inprod(beta[],X[i,])
}

  #Priors
beta[1] ~ dnorm(1.36,8.0E-2)
for(i in 2:ngroups) {
beta[i] ~ dnorm(0,1.0E-3)
}
sigma ~ dunif(0, 100)
tau <- 1 / (sigma * sigma)
}
"
# write the model to a text file
writeLines(model_string, con = file.path("models", "fact_anovaModel_default.txt")) 
writeLines(model_string_informative_priors, con = file.path("models", "fact_anovaModel_default_informed.txt")) 


#### model incorporating first order autoregressive (AR1) residual autocorrelation structure
# following tutorial at: https://agabrioblog.onrender.com/tutorial/autocorrelation-jags/autocorrelation-jags/
model_string_cauchy_t = "
model {
#Likelihood
for (i in 1:n) {
mean[i] <- inprod(beta[],X[i,])
}
y[1:n] ~ dmnorm(mean[1:n],Omega)
for (i in 1:n) {
for (j in 1:n) {
Sigma[i,j] <- sigma2*(1- phi*phi)*(equals(i,j) + (1-equals(i,j))*pow(phi,abs(i-j))) 
}
}
Omega <- inverse(Sigma)

#Priors
phi ~ dunif(-1,1)
for (i in 1:ngroups) {
beta[i] ~ dnorm(0,1.0E-3)
}
tau ~ dscaled.gamma(2.5, 1)  # half-Cauchy distribution (t distribution with 1.d.f.), scale = 2.5
sigma2 = 1/tau
}
"

model_string = "
model {
#Likelihood
for (i in 1:n) {
mean[i] <- inprod(beta[],X[i,])
}
y[1:n] ~ dmnorm(mean[1:n],Omega)
for (i in 1:n) {
for (j in 1:n) {
Sigma[i,j] <- sigma2*(1- phi*phi)*(equals(i,j) + (1-equals(i,j))*pow(phi,abs(i-j))) 
}
}
Omega <- inverse(Sigma)

#Priors
phi ~ dunif(-1,1)
for (i in 1:ngroups) {
beta[i] ~ dnorm(0,1.0E-3)
}
sigma <- z/sqrt(chSq)    # prior for sigma; cauchy = normal/sqrt(chi^2)
z ~ dnorm(0, 0.16)I(0,)  # half-normal distribution with only positive part, Cauchy scale = 2.5
chSq ~ dgamma(0.5, 0.5)  # chi^2 with 1 d.f.
sigma2 = pow(sigma,2)
#tau.cor <- tau #* (1- phi*phi)
}
"

model_string_informative_priors = "
model {
#Likelihood
for (i in 1:n) {
mean[i] <- inprod(beta[],X[i,])
}
y[1:n] ~ dmnorm(mean[1:n],Omega)
for (i in 1:n) {
for (j in 1:n) {
Sigma[i,j] <- sigma2*(1- phi*phi)*(equals(i,j) + (1-equals(i,j))*pow(phi,abs(i-j))) 
}
}
Omega <- inverse(Sigma)

#Priors
phi ~ dunif(-1,1)
beta[1] ~ dnorm(1.36,8.0E-2)
for(i in 2:ngroups) {
beta[i] ~ dnorm(0,1.0E-3)
}
sigma <- z/sqrt(chSq)    # prior for sigma; cauchy = normal/sqrt(chi^2)
z ~ dnorm(0, 0.16)I(0,)  # half-normal distribution with only positive part, Cauchy scale = 2.5
chSq ~ dgamma(0.5, 0.5)  # chi^2 with 1 d.f.
sigma2 = pow(sigma,2)
#tau.cor <- tau #* (1- phi*phi)
}
"

model_string_new = "
model {
#Priors
phi ~ dunif(-1,1)
for (i in 1:ngroups) {
beta[i] ~ dnorm(0,1.0E-3)
}
z ~ dnorm(0, 0.16)I(0,)  # half-normal distribution with only positive part, Cauchy scale = 2.5
chSq ~ dgamma(0.5, 0.5)  # chi^2 with 1 d.f.
sigma <- z/sqrt(chSq)    # prior for sigma; cauchy = normal/sqrt(chi^2)
sigma2 = pow(sigma,2)
#Likelihood
for (i in 1:n) {
mean[i] <- inprod(beta[i],X[i,])
}
Omega <- inverse(Sigma)
y[1:n] ~ dmnorm(mean[1:n],Omega)
for (i in 1:n) {
for (j in 1:n) {
Sigma[i,j] <- sigma2*(equals(i,j) + (1-equals(i,j))*pow(phi,abs(i-j))) 
}
}
}
"

# write the model to a text file
writeLines(model_string_cauchy_t, con = file.path("models", "mixed_model_AR1_cauchy_t.txt"))
writeLines(model_string, con = file.path("models", "mixed_model_AR1.txt"))
writeLines(model_string_informative_priors, con = file.path("models", "mixed_model_AR1_informed.txt"))
writeLines(model_string_new, con = file.path("models", "new_mixed_model_AR1.txt"))
