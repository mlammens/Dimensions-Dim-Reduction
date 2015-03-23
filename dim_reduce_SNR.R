## ************************************************************************** ##
## dim_reduce_SNR.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2015-03-22
##
## Purpose:
## Calculate signal to noise (SNR) ratio for resulting Bayesian models.
##
## ************************************************************************** ##


dim_reduce_SNR <- function(){
  
  # Calculating SNR
  # Following Xiaojing's suggestions of looking at 
  # http://www.stat.ucla.edu/~rosario/classes/091/112-1b/regression
  
  # 1. Calculate y_bar and y_hat 
  # * y_bar from data
  # * y_hat using mean regression coefficients
  y_bar <- t( as.matrix( sim_fit[[1]]$BUGSoutput$mean$beta ) ) %*% t( as.matrix( filter( prot_pel_df, genus == "Protea" )[ clim_vars ] ) )
  
  
  # 2. Calcualte SSE and SS_reg
  
  # 3. Calc MS_reg and MSE
  
  # 4. Calc SNR
  
}
