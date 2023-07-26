# create and save Bayesian models

#### create folder to save the models
dir.create(file.path("./models/"), showWarnings = FALSE)

#### basic model
basic_model = " 
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

basic_model_informative_priors_SL = "
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

basic_model_informative_priors_ST = "
model {
#Likelihood
for (i in 1:n) {
y[i]~dnorm(mean[i],tau)
mean[i] <- inprod(beta[],X[i,])
}

  #Priors
beta[1] ~ dnorm(1.05,6.0E-2)
for(i in 2:ngroups) {
beta[i] ~ dnorm(0,1.0E-3)
}
sigma ~ dunif(0, 100)
tau <- 1 / (sigma * sigma)
}
"

# write the model to a text file
writeLines(basic_model, con = file.path("models", "basic.txt")) 
writeLines(basic_model_informative_priors_SL, con = file.path("models", "basic_informative_SL.txt")) 
writeLines(basic_model_informative_priors_ST, con = file.path("models", "basic_informative_ST.txt")) 


#### model incorporating first order autoregressive (AR1) residual autocorrelation structure
# refer to tutorial at: https://agabrioblog.onrender.com/tutorial/autocorrelation-jags/autocorrelation-jags/

AR1_model = "
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
beta[1] ~ dnorm(0,1.0E-3)
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

AR1_model_diff_prob_SL = "
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
beta[1] ~ dnorm(0,1.0E-3)
for(i in 2:ngroups) {
beta[i] ~ dnorm(0,1.0E-3)
}
sigma <- z/sqrt(chSq)    # prior for sigma; cauchy = normal/sqrt(chi^2)
z ~ dnorm(0, 0.16)I(0,)  # half-normal distribution with only positive part, Cauchy scale = 2.5
chSq ~ dgamma(0.5, 0.5)  # chi^2 with 1 d.f.
sigma2 = pow(sigma,2)

# Probability effects of fatigue and cognitive task
threshold <- 0.03    # threshold for meaningful effect
diff_fatigue <- abs(beta[3] + 0.5*beta[4]) - (beta[1] + 0.5*beta[2])*threshold
diff_cognitive_task <- abs(beta[2] + 0.5*beta[4]) - (beta[1] + 0.5*beta[3])*threshold
p_fatigue <- step(diff_fatigue)    # probability of larger than the threshold
p_cognitive_task <- step(diff_cognitive_task)
}
"

AR1_model_diff_prob_ST = "
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
beta[1] ~ dnorm(0,1.0E-3)
for(i in 2:ngroups) {
beta[i] ~ dnorm(0,1.0E-3)
}
sigma <- z/sqrt(chSq)    # prior for sigma; cauchy = normal/sqrt(chi^2)
z ~ dnorm(0, 0.16)I(0,)  # half-normal distribution with only positive part, Cauchy scale = 2.5
chSq ~ dgamma(0.5, 0.5)  # chi^2 with 1 d.f.
sigma2 = pow(sigma,2)

# Probability effects of fatigue and cognitive task
threshold <- 0.02    # threshold for meaningful effect
diff_fatigue <- abs(beta[3] + 0.5*beta[4]) - (beta[1] + 0.5*beta[2])*threshold
diff_cognitive_task <- abs(beta[2] + 0.5*beta[4]) - (beta[1] + 0.5*beta[3])*threshold
p_fatigue <- step(diff_fatigue)    # probability of larger than the threshold
p_cognitive_task <- step(diff_cognitive_task)
}
"

AR1_model_informative_priors_SL = "
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

AR1_model_informative_priors_ST = "
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
beta[1] ~ dnorm(1.05,6.0E-2)
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

# write the model to a text file
writeLines(AR1_model, con = file.path("models", "AR1.txt"))
writeLines(AR1_model_diff_prob_SL, con = file.path("models", "AR1_model_diff_prob_SL.txt"))
writeLines(AR1_model_diff_prob_ST, con = file.path("models", "AR1_model_diff_prob_ST.txt"))
writeLines(AR1_model_informative_priors_SL, con = file.path("models", "AR1_informative_SL.txt"))
writeLines(AR1_model_informative_priors_ST, con = file.path("models", "AR1_informative_ST.txt"))
