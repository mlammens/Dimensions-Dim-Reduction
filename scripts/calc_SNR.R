## ************************************************************************** ##
## dim_reduce_SNR.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2015-03-22
##
## Purpose:
## Calculate signal to noise (SNR) ratio for resulting Bayesian models.
##
#' Arguments:
#' @param X_obs A matrix of observed X values (i.e., predictor values)
#' @param y_obs A vector of observed y values
#' @param beta A vector of beta coefficients. Length of beta should equal the
#' number of columns of X_obs
## ************************************************************************** ##

# ## Development code
# X_obs <- as.matrix( filter( prot_pel_df, genus == "Protea" )[ clim_vars ] )
# beta <- sim_fit[[33]]$BUGSoutput$mean$beta
# y_obs <- sim_trait_df$sim_trait_noise_1noeq

calc_SNR <- function( X_obs, y_obs, beta ){
  
  # Calculating SNR
  # Following Xiaojing's suggestions of looking at 
  # http://www.stat.ucla.edu/~rosario/classes/091/112-1b/regression
  
  ## -------------------------------------------------------------------------- ##
  # 1. Calculate y_bar and y_hat 
  
  ## y_bar: Mean observed y value
  y_bar <- mean( y_obs )
  
  ## y_hat: Calculate estaimted y vals using regression coefficients
  y_hat <- beta %*% t(X_obs)
   
  ## -------------------------------------------------------------------------- ##
  # 2. Calcualte SSE and SS_reg
  
  ## ss_reg: Sums of squares regression
  ss_reg <- sum( (y_hat - y_bar)^2 )
  
  ## sse: Sums of squares error residual
  sse <- sum( (y_obs - y_hat)^2 )
  
  ## sst: Sums of squars total
  sst <- sum( (y_obs - y_bar)^2 )
  
  ## -------------------------------------------------------------------------- ##
  # 3. Calc MS_reg and MSE

  ## Set number of predictors
  k <- length( beta )
  
  ## ms_reg: Mean square error regression
  ms_reg <- ss_reg / k
  
  ## mse = Mean squared error residual
  mse <- sse / (nrow(X_obs) - k - 1)
  
  ## -------------------------------------------------------------------------- ##
  # 4. Calc SNR
  
  SNR <- ms_reg / mse 

  ## -------------------------------------------------------------------------- ##
  ## Return SNR
  return( SNR )
  
}
