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
//
//   vector [n_trials] n[max(n_arms)];
//   // number of events per arm
  // vector[n_components] d;
//
//   vector [n_trials] c1[max(n_arms)];
//   vector [n_trials] c2[max(n_arms)];
//   vector [n_trials] c4[max(n_arms)];
//   vector [n_trials] c5[max(n_arms)];
//   vector [n_trials] c6[max(n_arms)];
//   vector [n_trials] c7[max(n_arms)];
//   vector [n_trials] c8[max(n_arms)];
//   vector [n_trials] c9[max(n_arms)];
//   vector [n_trials] c11[max(n_arms)];
//   vector [n_trials] c12[max(n_arms)];
//   vector [n_trials] c13[max(n_arms)];
//   vector [n_trials] c14[max(n_arms)];

}
transformed data {
  //int p[n_trials, max(n_arms)];
}
parameters {

  vector [n_trials] mu;
  vector[n_components-1] d;
  //vector[n_components] D;
  //real theta[n_components];

}
transformed parameters {
  //vector[n_components] d;
  ///vector[n_components] beta;
  //real hemp = d[2] + d[3] + d[5] + d[6] + d[7] + d[8] + d[10] + d[11] + d[12] + d[13];
  // beta[1] = 0;
  // for (k in 2:n_components){
  //   beta[k] = d[k];
  // }
}
model {
  vector [max(n_arms)] p[n_trials];

  for(i in 1:(n_components - 1)){
    d[i] ~ normal(0, 1000);
  }

  for (i in 1:n_trials) {
    mu[i] ~ normal(0, 1000);
    for (k in 1:n_arms[i]){
      p[i,k] = mu[i] + d[1]*c1[i, k] + d[2]*c2[i, k] + d[3]*c4[i, k] + d[4]*c5[i, k] + d[5]*c6[i, k]
      + d[6]*c7[i, k] + d[7]*c8[i, k] + d[8]*c9[i, k] + d[9]*c11[i, k] + d[10]*c12[i, k]
      + d[11]*c14[i, k] + d[12]*c13[i, k] - 0;
      r[i,k] ~ binomial_logit(n[i,k], p[i,k]);
    }
  }

}
