  #example from 578---

install.packages("R2jags")  
library(R2jags)
library(RMark) #just using a single function from RMark

rm(list=ls(all=TRUE))
  setwd("Downloads/")  
  
towa <- convert.inp("towa.inp")
y <- NULL
for(i in 1:34) {
	y <- c(y,as.numeric(strsplit(paste(rep(towa$ch[i],towa$freq[i]), collapse=''), split=NULL)[[1]]))
}

y <- matrix(y, ncol=21, byrow=TRUE)
nind        <- nrow(y)
n.occasions <- ncol(y)
f           <- rep(NA,nind)
Zst <- y

for (i in 1:nind){
	start    <- f[i] <- min(which(y[i,]==1))
	last     <- max(which(y[i,]==1))
	Zst[i,start:last] <- 1
	Zst[i,start] <- NA
	Zst[Zst==0]  <- NA
}


soi <- c(-0.43,0.07,-1.45,-0.95,-0.14,-0.02,-0.32,-1.48,0.74,0.62,-0.33,-1.01,-1.23,-1.11,-1.35,-0.33,0.56,-1.29,-0.23,0.79,0.85)
y <- list(y=y,f=f,nind=nind,n.occasions=n.occasions,x=soi)


###################################################################
##Model 0

sink("model0.jags")
cat("
model {

# Priors and constraints
for (i in 1:nind){
   for (t in f[i]:(n.occasions-1)){
      phi[i,t] <- mean.phi
      p[i,t] <- mean.p
      } #t
   } #i

mean.phi ~ dunif(0, 1)         # Prior for mean survival
mean.p ~ dunif(0, 1)           # Prior for mean recapture

# Likelihood 
for (i in 1:nind){
   # Define latent state at first capture
   z[i,f[i]] <- 1
   for (t in (f[i]+1):n.occasions){
      # State process
      z[i,t] ~ dbern(mu1[i,t])
      mu1[i,t] <- phi[i,t-1] * z[i,t-1]
      # Observation process
      y[i,t] ~ dbern(mu2[i,t])
      mu2[i,t] <- p[i,t-1] * z[i,t]
      } #t
   } #i
}
",fill = TRUE)
sink()

inits <- function() {list(z=Zst, mean.phi=runif(1,0,1), mean.p=runif(1,0,1))}
params <- c("mean.phi","mean.p")
model.0 <- jags(y, inits, params, "model0.jags", n.chains = 3, n.thin = 5, n.iter = 30000, n.burnin = 25000, working.directory = getwd())

###################################################################
##Model 1

sink("model1.jags")
cat("
model {
for(i in 1:nind) {
	for(t in f[i]:(n.occasions-1)) {
		logit(phi[i,t]) <- mu + beta*x[t]
		p[i,t] <- mean.p
		} #t
	} #i
for(t in 1:(n.occasions-1)) {
	phi.est[t] <- 1 / (1+exp(-mu-beta*x[t]))
	}

mu ~ dnorm(0,0.001)
mean.phi <- 1/(1+exp(-mu))
beta ~ dnorm(0, 0.001)I(-10,10)
sigma ~ dunif(0, 10)
tau <- pow(sigma, -2)
sigma2 <- pow(sigma, 2)
mean.p ~ dunif(0,1)

for (i in 1:nind) {
	z[i,f[i]] <- 1
	for(t in (f[i]+1):n.occasions) {
		z[i,t] ~ dbern(mu1[i,t])
		mu1[i,t] <- phi[i,t-1] * z[i,t-1]
		y[i,t] ~ dbern(mu2[i,t])
		mu2[i,t] <- p[i,t-1] * z[i,t]
		} #t
	} #i
}
", fill=TRUE)
sink()

inits <- function() {list(z=Zst, mu=rnorm(1), sigma=runif(1,0,5), beta=runif(1,-5,5), mean.p=runif(1,0,1))}
params <- c("mu", "mean.phi", "mean.p", "phi.est", "sigma2", "beta")
model.1 <- jags(y, inits, params, "model1.jags", n.chains = 3, n.thin = 5, n.iter = 30000, n.burnin = 25000, working.directory = getwd())

###################################################################
##Model 2

sink("model2.jags")
cat("
model {
for(i in 1:nind) {
	for(t in f[i]:(n.occasions-1)) {
		logit(phi[i,t]) <- mu + beta*x[t] + epsilon[t]
		p[i,t] <- mean.p
		} #t
	} #i
for(t in 1:(n.occasions-1)) {
	epsilon[t] ~ dnorm(0, tau)
	phi.est[t] <- 1 / (1+exp(-mu-beta*x[t]-epsilon[t]))

	}

mu ~ dnorm(0,0.001)
mean.phi <- 1/(1+exp(-mu))
beta ~ dnorm(0, 0.001)I(-10,10)
sigma ~ dunif(0, 10)
tau <- pow(sigma, -2)
sigma2 <- pow(sigma, 2)
mean.p ~ dunif(0,1)

for (i in 1:nind) {
	z[i,f[i]] <- 1
	for(t in (f[i]+1):n.occasions) {
		z[i,t] ~ dbern(mu1[i,t])
		mu1[i,t] <- phi[i,t-1] * z[i,t-1]
		y[i,t] ~ dbern(mu2[i,t])
		mu2[i,t] <- p[i,t-1] * z[i,t]
		} #t
	} #i
}
", fill=TRUE)
sink()

params <- c("mu", "mean.phi", "mean.p", "phi.est", "sigma2", "beta")
model.2 <- jags(y, inits, params, "model2.jags", n.chains = 3, n.thin = 5, n.iter = 55000, n.burnin = 50000, working.directory = getwd())

##################################################################################################################
##Problem 1. Write out, using the notation used in chapter 7, the Phi and p linear models for each of the three
##models fit by this R script. Describe, in no more than a few sentences, the assumption(s) of the CJS model
##most likely to be violated by this dataset.

##################################################################################################################
##Problem 2. Report, in a tabular form, the mean Phi and p parameter estimates, along with their 95% credible
##intervals, from each of the three models. Report, in no more than a few sentences, any changes you had to make
##to ensure model convergence, and how you assessed convergence. 

##################################################################################################################
##Problem 3. Create a graph, or a set of graphs, or table(s), that explicitly address the hypotheses: 
##1) mean SOI is positively related to Townsend's Warbler over-summer survival. 
##2) mean SOI is negatively related to Townsend's Warbler over-summer survival.
##In no more than a couple of sentences, describe support for these two hypotheses. 
##Hint: consider the beta parameters in models 1 & 2.


##end