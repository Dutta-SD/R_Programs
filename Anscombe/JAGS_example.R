## Model string
## -- data  -> Anscombe.csv
mod_string = " model {
     for (i in 1:length(education)) {
         education[i] ~ dnorm(mu[i], prec)
         mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
     }
     
     b0 ~ dnorm(0.0, 1.0/1.0e6)
     for (i in 1:3) {
         b[i] ~ dnorm(0.0, 1.0/1.0e6)
     }
     
     prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
     ## Initial guess of variance based on overall
     ## variance of education variable. Uses low prior
     ## effective sample size. Technically, this is not
     ## a true 'prior', but it is not very informative.
     sig2 = 1.0 / prec
     sig = sqrt(sig2)
} "

## Data to feed into Jags
data_jags = as.list(data)

### Inits
inits= function() {
     inits = list("b0" = rnorm(1, 0.0, 100.0), "b" = rnorm(3, 0.0, 100.0), "prec" = rgamma(1, 1.0, 1.0))
}
### Parameters
params1 = c("b0", "b", "sig")


### Configure Model
model_jags = jags.model(textConnection(mod_string), data = data_jags, inits = inits, n.chain = 3)

## Burn in
update(model_jags, 5e4)

### Simulated draws
mod_simulated = coda.samples(model = model_jags, variable.names = params1, n.iter = 5e5)

### Model Simulated effective Size
effectiveSize(mod_simulated)
### Gelman Diagonistic
gelman.diag(mod_simulated)

### DIC
dic.samples(model_jags, n.iter = 1e5)
 

modl_csim = do.call(rbind, mod_simulated)

## Income values
incomeVals = modl_csim[, 1] > 0.0

## probability
mean(incomeVals)

## Summary of the coefficients
summary(modl_csim)

