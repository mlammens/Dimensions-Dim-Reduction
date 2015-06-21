data {
  int<lower=1> nSamp;  // number of observations
  int<lower=1> nDim;   // number of response variables
  int<lower=1> K;      // number of covariates
  vector[nDim] y[nSamp];  // response vectors
  vector[K] X[nSamp];     // covariates
}

parameters {
  matrix[nDim,K] theta;
  matrix<lower=0, upper=1>[nDim,K] gamma;  // selection probability
  vector<lower=0, upper=1>[nDim] pi;
  corr_matrix[nDim] Omega;   // prior correlation
  vector<lower=0>[nDim] tau; // prior scale
}

transformed parameters {
  vector[nDim] mu[nSamp];
  vector[K] beta[nDim];      // regression coefficients
  matrix[nDim,nDim] Sigma;   // error covariance

  // covariate selection
  for (i in 1:nDim) {
    for (k in 1:K) {
      beta[i,k] <- theta[i,k]*gamma[i,k];
    }
  }

  // predicted values
  for (i in 1:nSamp) {
    for (j in 1:nDim) {
      mu[i,j] <- 0.0;
      for (k in 1:K) {
        mu[i,j] <- mu[i,j] + X[i,k]*beta[j,k];
      }
    }
  }

  Sigma <- quad_form_diag(Omega, tau);
}

model {
  for (i in 1:nDim) {
    for (k in 1:K) {
      theta[i,k] ~ normal(0.0, 1.0);
      gamma[i,k] ~ beta(pi[i], (1-pi[i]));
    }
    pi[i] ~ uniform(0.0, 1.0);
  }

  tau ~ normal(0.0, 1.0);
  Omega ~ lkj_corr(2);

  // likelihood
  for (i in 1:nSamp) {
    y[i] ~ multi_normal(mu[i], Sigma);
  }
}


