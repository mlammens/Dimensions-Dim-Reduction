## Calculate the posterior mean only for samples where
## gamma=1, i.e., the coefficient is included in the 
## model
##
cond.mean <- function(beta, gamma) {
  beta[gamma < 1.0] <- NA
  mean(beta, na.rm=TRUE)
}