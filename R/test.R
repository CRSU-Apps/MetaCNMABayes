library(rstan)
library(rio)

rstan_options(auto_write = TRUE)            # Cache compiled Stan programs
options(mc.cores = parallel::detectCores()) # Parallelize chains

data <- import("inst/data/data.csv")

Ntrials <- length(data$Study)
nt = 13

# Number of patients in each arm
n <- array(c(data$Total.TAU, data$Total.INT), dim=c(Ntrials, 2))

# Number of events in each arm
r <- array(c(data$Events.TAU, data$Events.INT), dim=c(Ntrials, 2))

d <- array(c(rep(as.integer(0), nt)))

C1 <- array(c(rep(0, Ntrials), data$C1), dim=c(Ntrials, 2))
C2 <- array(c(rep(0, Ntrials), data$C2), dim=c(Ntrials, 2))
C4 <- array(c(rep(0, Ntrials), data$C4), dim=c(Ntrials, 2))
C5 <- array(c(rep(0, Ntrials), data$C5), dim=c(Ntrials, 2))
C6 <- array(c(rep(0, Ntrials), data$C6), dim=c(Ntrials, 2))
C7 <- array(c(rep(0, Ntrials), data$C7), dim=c(Ntrials, 2))
C8 <- array(c(rep(0, Ntrials), data$C8), dim=c(Ntrials, 2))
C9 <- array(c(rep(0, Ntrials), data$C9), dim=c(Ntrials, 2))
C11 <- array(c(rep(0, Ntrials), data$C11), dim=c(Ntrials, 2))
C12 <- array(c(rep(0, Ntrials), data$C12), dim=c(Ntrials, 2))
C13 <- array(c(rep(0, Ntrials), data$C13), dim=c(Ntrials, 2))
C14 <- array(c(rep(0, Ntrials), data$C14), dim=c(Ntrials, 2))

stan_data <- list(
  n_trials = Ntrials,
  n_arms = data$na,
  n_components = nt,
  n = n,
  r = r,
  #d = d,
  c1 = C1,
  c2 = C2,
  c4 = C4,
  c5 = C5,
  c6 = C6,
  c7 = C7,
  c8 = C8,
  c9 = C8,
  c11 = C11,
  c12 = C12,
  c13 = C13,
  c14 = C14
)

n_chains = 1

d1 <- c(0, rep(0.1, nt-1))
d2 <- c(0, rep(0.2, nt-1))
d3 <- c(0, rep(-0.1, nt-1))

mu1 <- c(rep(0.1, Ntrials))
mu2 <- c(rep(0.3, Ntrials))
mu3 <- c(rep(0.2, Ntrials))

inits <- list(list(d=d1, mu=mu1),
              list(d=d2, mu=mu2),
              list(d=d3, mu=mu3))

stan_fit <- stan(
  file = "inst/test.stan",
  data = stan_data,
  chains = n_chains,
  warmup = 1000,
  iter = 4000,
  #refresh = 0,
  #init = inits,
  #algorithm = "HMC",
  control = list(max_treedepth = 10, adapt_delta = 0.95, stepsize = 0.01)
)
