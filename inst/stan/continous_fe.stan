data {
  // Number of trials
  int n_trials;
  // Number of arms per trial
  int n_arms[n_trials];
  // number of components (+ Usual Care)
  int n_components;
  // number of patients per arm (n)
  int n[n_trials, max(n_arms)];
  // means per arm
  int y[n_trials, max(n_arms)];
  // SDs per arm
  int sd[n_trials, max(n_arms)];
  // Component Array
  real components[n_components, n_trials, max(n_arms)];
  //vector[max(n_arms)] components[n_components, n_trials];
}
parameters {
  vector [n_trials] mu;
  vector[n_components] d;
}
model {
  // Define linear Prdictor
  vector [max(n_arms)] theta[n_trials];
  // Define Precision
  vector [max(n_arms)] prec[n_trials];
  // Define Tempory vector (to set control to 0)
  row_vector [n_components]D;

  for(i in 1:(n_components)){
    D[i] = d[i];
  }

  for(i in 1:(n_components)){
    d[i] ~ normal(0, 1000);
  }

  for (i in 1:n_trials) {
    mu[i] ~ normal(0, 1000);
    for (k in 1:n_arms[i]){
      prec[i,k] = n[i,k]/pow(sd[i,k],2);
      theta[i,k] = mu[i] + (D * to_vector(components[,i,k]));
      y[i,k] ~ normal(theta[i,k],prec[i,k]);
    }
  }

}
