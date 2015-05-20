## ******************************************************************** ##
## dim_reduce_run_CG_JAGS.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2015-01-05
##
## Purpose:
##
## -------------------------------------------------------------------- ##
## Args:
#' @param X Matrix of predictor values
#' @param Y Vector of response values
#' @param scale_x Logical indicating whether to scale the X values 
#' before analysis (default = FALSE)
#' @param scale_y Logical indicating whether to scale the y values 
#' before analysis (default = FALSE)
#' @param M_add Used to determine number of "labels", Curtis and Ghosh
#' recommend the number of predictors + 25
#' @param model_file File containing JAGS model
#' @param use_jags_test_pars Use JAGS test parameters, results in faster, but
#' less accurate model results (default = TRUE)
##   
## ******************************************************************** ##

dim_reduce_run_CG_JAGS <- function( X, y, 
                                    scale_x = FALSE, scale_y = FALSE,
                                    M_add = 25,
                                    model_file = "curtis_ghosh_example.jags",
                                    use_jags_test_pars = TRUE ){
    

  ## Check that X is a matrix
  if( !is.matrix( X ) ){
    X <- as.matrix( X ) 
  }
  
  ## Scale X and Y values if "scale" = TRUE
  if( scale_x ){
    X <- scale( X )
  }
  if( scale_y ){
    y <- scale( y )
  }
  
  ## Define model settings - number of samples
  n.samp <- nrow( X )
  
  ## The next parameter, M, is somewhat arbitrarily set. The authors 
  ## write use a "suitably large M", and define it as p + 25, where 
  ## p is the number of coefficients in thier examples
  M <- ncol( X ) + M_add
  
  ## Set number of regression coefficients
  K <- ncol( X )
  
  ## Define the variable names used for jags data
  jags.data <-
    c( "X", "n.samp", "M", "K", "y" )
      
  ## Set JAGs parameters
  if( use_jags_test_pars ){
    ## Testing parameters
    n.chains <- 2
    n.burnin <- 5*1000
    n.iter <- 5*7250
    n.thin <- 5*5
  } else {
    ## Final model pars
    n.chains <- 4
    n.burnin <- 5*10000
    n.iter <- 5*72500
    n.thin <- 5*50
  }
  
  ## Make a vector of parameters to track in JAGs
  jags.par <-
    c( "beta", "gamma", "S", "theta", "xi", "pi" )
  
  ## Run jags model
  fit <-   
    #     jags( data = jags.data,
    #           inits = NULL,
    #           parameters = jags.par,
    #           model.file = model_file, 
    #           n.chains = n.chains,
    #           n.burnin = n.burnin,
    #           n.iter = n.iter,
    #           n.thin = n.thin,
    #           DIC = TRUE,
    #           working.directory = "." )
    do.call( jags.parallel, 
             list ( data = jags.data,
                    inits = NULL,
                    parameters = jags.par,
                    model.file = model_file, 
                    n.chains = n.chains,
                    n.burnin = n.burnin,
                    n.iter = n.iter,
                    n.thin = n.thin,
                    DIC = TRUE,
                    working.directory = "."
                    ,envir = environment() 
             ) )
  
  ## Return the jags model fit
  return( fit )
  
}