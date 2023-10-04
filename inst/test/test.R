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
  c1 = C1,
  c2 = C2,
  c4 = C4,
  c5 = C5,
  c6 = C6,
  c7 = C7,
  c8 = C8,
  c9 = C9,
  c11 = C11,
  c12 = C12,
  c13 = C13,
  c14 = C14
)

n_chains = 3

# d1 <- c(0, rep(0.1, nt-1))
# d2 <- c(0, rep(0.2, nt-1))
# d3 <- c(0, rep(-0.1, nt-1))

d1 <- c(rep(0.1, nt-1))
d2 <- c(rep(0.2, nt-1))
d3 <- c(rep(-0.1, nt-1))

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
  warmup = 10000,
  iter = 100000,
  #refresh = 0,
  init = inits,
  seed = 12345,
  #algorithm = "HMC",
  control = list(max_treedepth = 10, adapt_delta = 0.95, stepsize = 0.01)
)

draws <- extract(stan_fit)
stan_summary <- as.data.frame(summary(stan_fit, pars = c("d"))$summary)

# To remove weighting from plot
symbol.size=c(rep(1,12))

#d <- stan_summary$mean
#se <- stan_summary$se_mean

comp.names <- c("Re-orientation & familiar objects", "Reducing sensory deprivation", "Cognitive stimulation",
                "Nutrition & hydration", "Identification of infection", "Mobilisation", "Sleep hygiene", "Oxygenation",
                "Pain control", "Medication review", "Bowel & Bladder care", "Assessment of Mood")

metafor::forest(x=stan_summary$mean, ci.lb=stan_summary$`2.5%`, ci.ub=stan_summary$`97.5%`, slab=comp.names, transf = exp,
       xlab="Odds Ratio", refline=1, xlim=c(-6,15), at=c(0,  1, 2, 5, 10),
       psize=symbol.size, cex=0.9, header=c("Component", "OR (95% CrI)"))
