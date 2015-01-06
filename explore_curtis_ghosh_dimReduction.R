## ******************************************************************** ##
## explore_curtis_ghosh_dimReduction.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2014-07-08
##
## Purpose:
##
## ******************************************************************** ##

## Source setup file
source( "dim_reduce_setup.R" )

## Set booleans
use_jags_test_pars <- FALSE

#genus <- "Protea"
genus <- "Pelargonium"

## -------------------------------------------------------------------- ##
## Get formated protea and pelargonium dataset
## -------------------------------------------------------------------- ##
prot_pel_df <- format_prot_pel_data( dim_red_basedir = "", 
                                     make_new_clim_df = FALSE, 
                                     clim_vars = clim_vars, 
                                     traits = traits )


## ******************************************************************** ##
## Run Curtis and Ghosh model using LMA and all climate values
## ******************************************************************** ##

if( genus == "Protea" ){
  df <- filter( prot_pel_df, genus == "Protea" )
} else if( genus == "Pelargonium" ){
  df <- filter( prot_pel_df, genus == "Pelargonium" )
}

## Scale the trait matrices
df[ traits ] <- scale( df[ traits ] )
  
## Extract predictos (i.e., climate) as a matrix
X <- as.matrix( df[ clim_vars ] )

## Define model settings
n.samp <- nrow( df )

## The next parameter, M, is somewhat arbitrarily set. The authors 
## write use a "suitably large M", and define it as p + 25, where 
## p is the number of coefficients in thier examples
M <- ncol( X ) + 25

## Set number of regression coefficients
K <- ncol( X )

## Define the variable names used for jags data
jags.data <-
  c( "X", "n.samp", "M", "K", "y" )

## Set model file
model.file <- "curtis_ghosh_example.jags"

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

## Use for loop to run different traits overnight

traits <- c( "LMA", "Canopy_area", "FWC", "LWratio" )
fit <- vector( mode = "list" )

for( i in traits ){
  
  ## Set the response variable 
  y <- df[[ i ]]  
  
  ## Run jags model
  fit[[ i ]] <-   
    do.call( jags.parallel, 
             list ( data = jags.data,
                    inits = NULL,
                    parameters = jags.par,
                    model.file = model.file, 
                    n.chains = n.chains,
                    n.burnin = n.burnin,
                    n.iter = n.iter,
                    n.thin = n.thin,
                    DIC = TRUE,
                    working.directory = "." ) )
  
}  

## -------------------------------------------------------------------- ##



