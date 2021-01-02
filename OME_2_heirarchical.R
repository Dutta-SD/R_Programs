library("MASS")
data("OME")

dat = subset(OME, OME != "N/A")
dat$OME = factor(dat$OME) # relabel OME
dat$ID = as.numeric(factor(dat$ID)) # relabel ID so there are no gaps in numbers (they now go from 1 to 63)

## Original reference model and covariate matrix
mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
X = model.matrix(mod_glm)[,-1]

## Original model (that needs to be extended)
mod_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = a[ID[i]] + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
    for (k in 1:max(ID)){
        a[k] ~ dnorm(mu, precTau2)
    }
	
	mu ~ dnorm(0, 1.0/100)
    precTau2 ~ dgamma(1.0/2.0, 1.0/2.0)

    tau = sqrt(1.0 / precTau2)

	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
} "
library('rjags')
data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct
data_jags$n = dat$Trials
data_jags$ID = dat$ID

params = c("a", "b", "phi", "mu", "tau")
mod = jags.model(textConnection(mod_string), data = data_jags, n.chains = 3)
update(mod, 1e3)
mod_sim = coda.samples(model = mod, variable.names = params, n.iter = 5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))
#plot(mod_sim, ask = TRUE)
a = dic.samples(mod, n.iter = 1e3)
print(a)