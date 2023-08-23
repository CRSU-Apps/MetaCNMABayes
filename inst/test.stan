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
  // Component Arrays
  int c1[n_trials, max(n_arms)];
  int c2[n_trials, max(n_arms)];
  int c4[n_trials, max(n_arms)];
  int c5[n_trials, max(n_arms)];
  int c6[n_trials, max(n_arms)];
  int c7[n_trials, max(n_arms)];
  int c8[n_trials, max(n_arms)];
  int c9[n_trials, max(n_arms)];
  int c11[n_trials, max(n_arms)];
  int c12[n_trials, max(n_arms)];
  int c13[n_trials, max(n_arms)];
  int c14[n_trials, max(n_arms)];
}
parameters {
  vector [n_trials] mu;
  vector[n_components-1] d;
}
model {
  // Define Probability Vector
  vector [max(n_arms)] p[n_trials];
  // Define Tempory vector (to set control to 0)
  vector [n_components]D;

  // Control should be 0
  D[1] = 0;

  for(i in 2:(n_components)){
    // Set D[n] to d[n-1]
    D[i] = d[i-1];
  }

  for(i in 1:(n_components - 1)){
    d[i] ~ normal(0, 1000);
  }

  for (i in 1:n_trials) {
    mu[i] ~ normal(0, 1000);
    for (k in 1:n_arms[i]){
      p[i,k] = mu[i] + D[2]*c1[i, k] + D[3]*c2[i, k] + D[4]*c4[i, k] + D[5]*c5[i, k] + D[6]*c6[i, k]
      + D[7]*c7[i, k] + D[8]*c8[i, k] + D[9]*c9[i, k] + D[10]*c11[i, k] + D[11]*c12[i, k]
      + D[12]*c14[i, k] + D[13]*c13[i, k] - D[1];
      r[i,k] ~ binomial_logit(n[i,k], p[i,k]);
    }
  }

}
