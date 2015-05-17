data {
  int<lower=1> nSamp;  // number of observations
  int<lower=1> nDim;   // number of response variables
  int<lower=1> K;      // number of covariates
  int<lower=1> M;      // number of covariate categories
  matrix[nSamp,nDim] y;
  matrix[nSamp,K] X;
}

parameters {
  vector[K] beta[j];   // regression coefficients, beta[k,1] is intercept
  simplex[K] p[nDim];
  real<lower=0,upper=1> pi;
}

transformed parameters {
  matrix[nSamp,nDim] mu;
  matrix[nDim,K] theta;
  matrix[nDim,K] gamma;
  int<lower=1> S[nDim,K];

  // predicted values
  for (j in 1:nDim) {
    for (i in 1:nSamp) {
      mu[i,j] <- X[i]*beta[j];
    }
  }

  // selection and grouping
  for (k in 1:K) {
    for (j in 1:nDim) {
      theta[j,k] <- xi[S[j,k]];
      S[j,k] ~ categorical(p[j])
      gamma[j,k] ~ bernoulli(pi)
      beta[j,k] <- theta[j,k]*gamma[j,k];
    }
  }

  // coefficients for Dirichlet
  for (m in 1:M) {
    a.p[m] <- 1.0/M;
  }
  // coefficient clusters
  for (m in 1:M) {
    xi[m] ~ normal(0.0, 1.0);
  }
}

model {
  // priors
  for (j in 1:nDim) {
    p[j] ~ dirichlet(a.p)
  }
  Sigma ~ wishart(nu, Omega);
  pi ~ uniform(0.0,1.0);

  // likelihood
  for (i in 1:nSamp) {
    y[i] ~ multi_normal(mu, Sigma);
  }
}