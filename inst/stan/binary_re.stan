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
}
parameters {
  vector [n_trials] mu;
  row_vector [n_components] d;

  // Define prior for between trial sd

  real<lower=0, upper=10> sdbt;

  vector [max(n_arms)] delta[n_trials];
}
transformed parameters {
  vector [max(n_arms)] w[n_trials];
  vector [max(n_arms)] sw[n_trials];
  vector [max(n_arms)] md[n_trials];
  vector [max(n_arms)] taud[n_trials];
  vector [max(n_arms)] r_delta[n_trials];

  for (i in 1:n_trials) {
    w[i, 1] = 0;
    r_delta[i, 1] = 0;
    sw[i,1] = 0;
    md[i,1] = 0;
    taud[i,1] = 0;
    for (k in 2:n_arms[i]) {
      taud[i,k] = sdbt * 2 * (k-1) / k;
      w[i,k] = delta[i,k] - (d * to_vector(components[,i,k])) + (d * to_vector(components[,i,1]));
      sw[i,k] = sum(w[i,1:k-1]) / (k-1);
      md[i,k] = (d * to_vector(components[,i,k])) - (d * to_vector(components[,i,1])) + sw[i,k];
      r_delta[i,k] = delta[i,k];
    }
    for (k in (n_arms[i]+1):max(n_arms)) {
      w[i,k] = 0;
      sw[i,k] = 0;
      md[i,k] = 0;
      taud[i,k] = 0;
      r_delta[i,k] = 0;
    }

  }
  
}
model {
  vector [max(n_arms)] p[n_trials];
  sdbt ~ uniform(0,10);

  for (i in 1:n_trials) {
    mu[i] ~ normal(0, 1000);
    delta[i] ~ normal(0, 1000);

    for (k in 2:n_arms[i]) {
      delta[i,k] ~ normal(md[i,k], taud[i,k]);
    }

    for (k in 1:n_arms[i]){
      p[i,k] = mu[i] + r_delta[i, k];
      r[i,k] ~ binomial_logit(n[i,k], p[i,k]);

    }
  
  }

}
