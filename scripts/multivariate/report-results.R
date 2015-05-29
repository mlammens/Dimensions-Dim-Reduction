library(R2jags)

summarize.covar <- function(label, beta, gamma) {
  gamma.mu <- mean(gamma)
  beta.post <- beta[gamma>0]
  beta.mu <- mean(beta.post)
  beta.int <- quantile(beta.post, c(0.025, 0.975))
  line <- sprintf("%15s: %4.2 f     %6.3f (%6.3f, %6.3f)", label, gamma.mu, beta.mu, beta.int[1], beta.int[2])
  cat(line)
  if ((beta.int[2] < 0.0) || (beta.int[1] > 0.0)) {
    cat("*", sep="")
  }
  cat("\n")
}

load(file="scripts/multivariate/preliminary-results.Rsave")

beta <- fit$BUGSoutput$sims.list$beta
gamma <- fit$BUGSoutput$sims.list$gamma

covars <- c("pptcon", "summer", "map", "mat", "tminave07c", "tmaxave01c", "P_Bray_II_mg_kg",
            "alt")
response <- c("d13C_12C", "Percent_N", "LMA","Wood_density","area_lam","lam_width")

for (i in 1:length(response)) {
  cat(response[i], "\n")
  for (j in 1:length(covars)) {
    summarize.covar(covars[j], beta[,i,j], gamma[,i,j])
  }
}