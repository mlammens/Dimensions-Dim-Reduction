## ******************************************************************** ##
## generate_sim_data.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2014-12-30
##
## Purpose:
## Generate simulation data using a set of predictor variables (df).
## This was specifically written with the dimensions-za protea/pelie dataset 
## in ming.
##
## Args:
#' @param df A data.frame of predictor variables. Specifically written assuming
#' these variables are climate variables
#' @param clim_vars A vector of the environmental / climate variables
#' in 'df' used to simulate a new trait
#' @param clim_vars_beta Beta coefficient values to use in functional
#' relationship of environmental vars to a trait value
#' @param scale Default = TRUE. Scale predictor variables before simulating data
#' @param noise Standard deviation value to use in generation of random
#' error
## ******************************************************************** ##

generate_sim_data <- function( df, 
                               clim_vars, 
                               clim_vars_beta,
                               scale = TRUE,
                               noise ){

  ## Get predictors
  X <- as.matrix( df[ clim_vars ] )
  
  ## Scale predictors 
  if( scale ){
    X <- scale( X )
  }
  
  ## Generate new trait values using inner matrix product
  sim_trait <- 
     X %*% matrix( clim_vars_beta ) 
  
  ## Add random noise
  sim_trait <- sim_trait + rnorm( n = nrow( df ), mean = 0, sd = noise )
  
  ## Return the simulated trait
  return( as.vector( sim_trait ) )
 
}
