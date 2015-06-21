library(R2jags)

summarize.covar <- function(label, beta, gamma, max.width) {
  gamma.mu <- mean(gamma)
  beta.post <- beta[gamma>0]
  beta.mu <- mean(beta.post)
  beta.int <- quantile(beta.post, c(0.025, 0.975))
  line <- sprintf("%*s: %4.2 f     %6.3f (%6.3f, %6.3f)",
                  max.width, label, gamma.mu, beta.mu, beta.int[1], beta.int[2])
  cat(line)
  if ((beta.int[2] < 0.0) || (beta.int[1] > 0.0)) {
    cat("*", sep="")
  }
  cat("\n")
}

source("scripts/multivariate/response-covars.R")

if (use.jags == TRUE) {
  beta <- fit$BUGSoutput$sims.list$beta
  gamma <- fit$BUGSoutput$sims.list$gamma
} else {
  beta <- extract(fit, "beta")$beta
  gamma <- extract(fit, "gamma")$gamma
}

max.width <- 0
for (covar in covars) {
  if (nchar(covar) > max.width) {
    max.width <- nchar(covar)
  }
}

if (length(response) > 1) {
  for (i in 1:length(response)) {
    cat(response[i], "\n")
    for (j in 1:length(covars)) {
      summarize.covar(covars[j], beta[,i,j], gamma[,i,j], max.width)
    }
  }
} else {
  cat(response[1], "\n")
  for (j in 1:length(covars)) {
    summarize.covar(covars[j], beta[,j], gamma[,j], max.width)
  }
}
