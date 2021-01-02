library('rjags')

### Read data in
data = read.csv('grav.csv', sep = ';')
head(data)
# Columns to keep
keeps = c("Lat","Long", "SeaLevel", "GravStation",  "GravSea")

## Drop columns other than keeps
data = data[,keeps]

# View top row
head(data)

# Pairs Plot
# pairs(data)

set.seed(0)
## We will predict gravity at each place "GravStation"
## Unbiased linear Model

mod = lm(GravStation ~ Lat + Long + SeaLevel, data = data)

summary(mod)

## Model 1 for MCMC sampling

mod_string = "model {
    for(i in 1:length(GravStation)){
        GravStation[i] ~ dnorm(theta[i], prec)
        theta[i] = b_0 + b[1]*SeaLevel[i] + b[2]*Lat[i] + b[3]*Long[i]               
    }

    b_0 ~ dnorm(0.0, 1.0/5e6)

    for(j in 1:4) {
        b[j] ~ dnorm(0.0, 1.0 / 5e6)
    }
    prec ~ dgamma(1.0, 1.0)
    sig = sqrt(1.0/prec)
}"


### Data and parameters to monitor
data_jags_1 = as.list(data)

params_1 = c("b_0", "sig", "b")


### Create Model 1
model_1 = jags.model(textConnection(mod_string), data = data_jags_1, n.chains = 3)
update(model_1, 1e4)

model_sim_1 = coda.samples(model_1,variable.names = params_1, n.iter = 5e4)
model_csim_1 = as.mcmc(do.call(rbind, model_sim_1))

## Gelman Diagnostics for convergence assessment
print(gelman.diag(model_sim_1, 1e3))

modelParams = colMeans(model_csim_1)
modelParams

### Plot the parameters to assess convergence
plot(model_sim_1, ask = TRUE)
summary(model_csim_1)


### Summary of model
summary(model_csim_1)

## qqnorm(model_csim_1)


### DIC of model, score of how well the model performs
model_1_dic = dic.samples(model_1, 1e4)

### model 2 - Interaction model for MCMC sampling

mod_string_2 = "model {
    for(i in 1:length(GravStation)){
        GravStation[i] ~ dnorm(theta[i], prec)
        theta[i] = b_0 + b[1]*SeaLevel[i] + b[2]*Lat[i] * Long[i]               
    }

    b_0 ~ dnorm(0.0, 1.0/5e6)

    for(j in 1:3) {
        b[j] ~ dnorm(0.0, 1.0 / 5e6)
    }
    prec ~ dgamma(1.0, 1.0)
    sig = sqrt(1.0/prec)
}"

model_2 = jags.model(textConnection(mod_string_2), data = data_jags_1, n.chains = 3)

update(model_2, 1e4)

model_sim_2 = coda.samples(model_2,variable.names = params_1, n.iter = 5e4)

model_csim_2 = as.mcmc(do.call(rbind, model_sim_2))


### Convergence Diagnostics
print(gelman.diag(model_sim_2, 1e3))

modelParams_2 = colMeans(model_csim_2)
modelParams

### Chain plots
plot(model_sim_2, ask = TRUE)


## DIC of second model
model_2_dic = dic.samples(model_2, 1e4)

model_2_dic

### We find that model_2 has a dic of about 16 while model_1 has a dic of about
### 12. So model_1 is our preferred model
