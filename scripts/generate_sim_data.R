## ******************************************************************** ##
## generate_sim_data.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2014-12-30
##
## Purpose:
## Generate simulation data using either protea or pelargonium data
##
## Args:
#' @param df A data.frame the includes all of the climate variables
#' measured
#' @param clim_vars A vector of the environmental / climate variables
#' used to simulate the data
#' @param clim_vars_beta Beta coefficient values to use in functional
#' relationship of environmental vars to a trait value
#' @param scale Scale predictor variables before simulating data
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
