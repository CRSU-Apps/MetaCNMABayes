library(rstan)
library(rio)

rstan_options(auto_write = TRUE)            # Cache compiled Stan programs
options(mc.cores = parallel::detectCores()) # Parallelize chains

data <- import("inst/data/data.csv")

Ntrials <- length(data$Study)
nt = 12

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

COMP <- array(0,dim=c(nt, Ntrials, 2))
COMP[1,,] <- C1
COMP[2,,] <- C2
COMP[3,,] <- C4
COMP[4,,] <- C5
COMP[5,,] <- C6
COMP[6,,] <- C7
COMP[7,,] <- C8
COMP[8,,] <- C9
COMP[9,,] <- C11
COMP[10,,] <- C12
COMP[11,,] <- C13
COMP[12,,] <- C14

stan_data <- list(
  n_trials = Ntrials,
  n_arms = data$na,
  n_components = nt,
  n = n,
  r = r,
  components = COMP
)

n_chains = 3

d1 <- c(rep(0.1, nt))
d2 <- c(rep(0.2, nt))
d3 <- c(rep(-0.1, nt))

mu1 <- c(rep(0.1, Ntrials))
mu2 <- c(rep(0.3, Ntrials))
mu3 <- c(rep(0.2, Ntrials))

inits <- list(list(d=d1, mu=mu1),
              list(d=d2, mu=mu2),
              list(d=d3, mu=mu3))

stan_fit <- stan(
  file = "inst/models/binary_FE.stan",
  data = stan_data,
  chains = n_chains,
  warmup = 10000,
  iter = 100000,
  init = inits,
  seed = 12345,
  control = list(max_treedepth = 10, adapt_delta = 0.95, stepsize = 0.01)
)

draws <- extract(stan_fit)
stan_summary <- as.data.frame(summary(stan_fit, pars = c("d"))$summary)

# To remove weighting from plot
symbol.size=c(rep(1,12))

comp.names <- c("Re-orientation & familiar objects", "Reducing sensory deprivation", "Cognitive stimulation",
                "Nutrition & hydration", "Identification of infection", "Mobilisation", "Sleep hygiene", "Oxygenation",
                "Pain control", "Medication review", "Bowel & Bladder care", "Assessment of Mood")

metafor::forest(x=stan_summary$mean, ci.lb=stan_summary$`2.5%`, ci.ub=stan_summary$`97.5%`, slab=comp.names, transf = exp,
                xlab="Odds Ratio", refline=1, xlim=c(-6,15), at=c(0,  1, 2, 5, 10),
                psize=symbol.size, cex=0.9, header=c("Component", "OR (95% CrI)"))
