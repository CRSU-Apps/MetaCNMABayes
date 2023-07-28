data {
  // Number of Trials
  int n_trials;
  // Number of Arms
  int n_arms[n_trials];
  int n_components[n_trials];
  real n_patients[n_trials,2];
  int n_events[n_trials,2];
}
parameters {
  real mu[n_trials];
}
model {

}
