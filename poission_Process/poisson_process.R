# input the csv file

mod_string_2 = "model {
     for(i in 1:length(numvisit) ){
         numvisit[i] ~ dpois(lam[i])
         log(lam[i]) = int + b_badh * badh[i] + b_age*age[i]
     }
     int ~ dnorm(0.0, 1.0/1e6)
     b_badh ~ dnorm(0.0, 1.0/1e4)
     b_age ~ dnorm(0.0, 1.0/1e4)
}"


data_jags = as.list(badhealth)
params_2 = c("int", "b_badh", "b_age")

mod_2 = jags.model(textConnection(mod_string_2), data = data_jags, n.chains = 3)

update(mod_2, 1e3)

mod_sim_2 = coda.samples(model = mod_2, variable.names = params_2, n.iter = 5e3)

mod_csim_2 = as.mcmc(do.call(rbind, mod_sim_2))

dic_nrmal = dic.samples(mod, n.iter = 1e3)

### Calling Rate Analysis


