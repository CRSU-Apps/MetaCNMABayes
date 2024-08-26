data {
  // Number of trials
  int n_trials;
  // Number of arms per trial
  int n_arms[n_trials];
  // number of components (+ Usual Care)
  int n_components;
  // number of patients per arm (n)
  int n[n_trials, max(n_arms)];
  // number of events per arm
  int r[n_trials, max(n_arms)];
  // Component Array
  real components[n_components, n_trials, max(n_arms)];
  //vector[max(n_arms)] components[n_components, n_trials];
}
parameters {
  vector [n_trials] mu;
  row_vector[n_components] d;
}
model {
  // Define Probability Vector
  vector [max(n_arms)] p[n_trials];
  // Define Tempory vector (to set control to 0)

  for(i in 1:(n_components)){
    d[i] ~ normal(0, 1000);
  }

  for (i in 1:n_trials) {
    mu[i] ~ normal(0, 1000);
    for (k in 1:n_arms[i]){
      p[i,k] = mu[i] + (d * to_vector(components[,i,k]));
      r[i,k] ~ binomial_logit(n[i,k], p[i,k]);
    }
  }

}
