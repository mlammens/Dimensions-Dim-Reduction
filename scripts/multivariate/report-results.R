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

## load(file="scripts/multivariate/preliminary-results.Rsave")
## covars <- c("intercept", "pptcon", "summer", "map", "mat", "tminave07c", "tmaxave01c", "P_Bray_II_mg_kg",
##              "alt")
## response <- c("d13C_12C", "Percent_N", "LMA","Wood_density","area_lam","lam_width")

load(file="scripts/multivariate/preliminary-results-univariate.RSave")
covars <- c("intercept", "ratio.x","map","mat","tminave07c","tmaxave01c","P_Bray_II_mg_kg",
            "K_Exchangeable_cations_cmol_kg", "Ca_Exchangeable_cations_cmol_kg",
            "C", "temp_win", "temp_spr", "temp_sum", "temp_aut",
            "rain_win", "rain_spr", "rain_sum", "rain_aut",
            "apan_win", "apan_spr", "apan_sum", "apan_aut",
            "alt")
response <- c("LMA")

beta <- fit$BUGSoutput$sims.list$beta
gamma <- fit$BUGSoutput$sims.list$gamma

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
