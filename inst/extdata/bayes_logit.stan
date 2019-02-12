// Code for 0-1 loss Bayes Logistic Regression model
data {
  int<lower=0> n; // number of observations
  int<lower=0> p; // number of covariates
  matrix[n,p] x; // Matrix of covariates
  int<lower=0,upper=1> y[n]; // Responses
  real<lower=0> beta_sd; // Stdev of beta
}
parameters {
  vector[p] beta;
}
model {
  beta ~ normal(0,beta_sd);
  y ~ bernoulli_logit(x * beta); // Logistic regression
}
