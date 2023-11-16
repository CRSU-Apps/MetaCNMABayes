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
  real y[n_trials, max(n_arms)];
  // SDs per arm
  real sd[n_trials, max(n_arms)];
  // Component Array
  real components[n_components, n_trials, max(n_arms)];
  //vector[max(n_arms)] components[n_components, n_trials];
}
parameters {
  vector [n_trials] mu;
  vector[n_components] d;
}
transformed parameters {
  // Define prior for between trial sd
  real<lower=0, upper=10> sdbt;
}
model {
  // Define linear Prdictor
  vector [max(n_arms)] theta[n_trials];
  // Define Precision
  vector [max(n_arms)] prec[n_trials];
  // Define mean treatment effect distribution with multi-arm corrections vector
  vector [max(n_arms)] md[n_trials];
  // Define vector trial specific treatment effect vector
  vector [max(n_arms)] delta[n_trials];
  // Define vector for between trial sd
  vector [max(n_arms)] taud[n_trials];
  // Define vector for between trial adjustment
  vector [max(n_arms)] w[n_trials];
  // Define vector for between trial cumulative adjustment
  vector [max(n_arms)] sw[n_trials];
  // Define Tempory vector (to set control to 0)
  row_vector [n_components]D;

  sdbt ~ uniform(0, 10);
  
  for(i in 1:(n_components)){
    D[i] = d[i];
  }
  
  for(i in 1:(n_components)){
    d[i] ~ normal(0, 1000);
  }
  
  for (i in 1:n_trials) {
    mu[i] ~ normal(0, 1000);
    delta[i,1] = 0;
    w[i,1] = 0;
    
    for (k in 2:n_arms[i]) {
      taud[i,k] = sdbt * 2 * (k-1) / k;
      md[i,k] = (D * to_vector(components[,i,k])) - (D * to_vector(components[,i,1])) + sw[i,k];
      delta[i,k] ~ normal(md[i,k], taud[i,k]);
      w[i,k] = delta[i,k] - (D * to_vector(components[,i,k])) + (D * to_vector(components[,i,1]));
      sw[i,k] = sum(w[i,k:k-1]) / (k-1);
    }
    
    for (k in 1:n_arms[i]){
      theta[i,k] = mu[i] + delta[i,k];
      y[i,k] ~ normal(theta[i,k],sd[i,k]);
    }
  }
  
}
