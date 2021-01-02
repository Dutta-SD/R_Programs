library('rjags')
dat = read.csv('pctgrowth.csv', header = TRUE)
head(dat)
mod_string = "model {
    for(i in 1:length(y)){
        y[i] ~ dnorm(theta[grp[i]], precSig)
    }
    for(j in 1:max(grp)){
        theta[j] ~ dnorm(mu, precTau)
    }
    mu ~ dnorm(0, 1.0 / 1e6)
    precTau ~ dgamma(1.0/ 2.0, 1.0*3.0/2.0)
    precSig ~ dgamma(2.0/2.0, 2.0*1.0 / 2.0)
    sig2 = 1.0/precSig
    tau2 = 1.0/precTau
}"
data_jags = as.list(dat)
params_1 = c("theta", "mu", "sig2", "tau2")

model = jags.model(textConnection(mod_string),
 data = data_jags,
 n.chains = 3)

mod_sim = coda.samples(model = model, variable.names = params_1, n.iter = 1e4)

mod_csim = as.mcmc(do.call(rbind, mod_sim))
gelman.diag(mod_csim, 1e3)