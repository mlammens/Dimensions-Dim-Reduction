## ******************************************************************** ##
## dim_reduce_main.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2014-12-30
##
## Purpose:
## Main script for dimension reduction and variable selection analysis
##
## ******************************************************************** ##

## -------------------------------------------------------------------- ##
## Source setup and function files
## -------------------------------------------------------------------- ##
source( "dim_reduce_setup.R" )

## -------------------------------------------------------------------- ##
## Get formated protea and pelargonium dataset
## -------------------------------------------------------------------- ##
prot_pel_df <- format_prot_pel_data( dim_red_basedir = "", 
                                     make_new_clim_df = FALSE, 
                                     clim_vars = clim_vars, 
                                     traits = traits )

## -------------------------------------------------------------------- ##
## Examine the correlation structure of the environmental variables 
## -------------------------------------------------------------------- ##

## Calculate correlations
prot_pel_env_cor <- 
  cor( select( prot_pel_df, which( names( prot_pel_df ) %in% clim_vars ) ) )


## ******************************************************************** ##
## Generate simulated data based on correlation relationships
## ***
## As per discussions with Kent and Xiaojing, we want to simulate a 
## trait that is related to only three variables, but those variables
## are highly correlated with other variables. We also want to examine
## the effects of various levels of noise.
## ******************************************************************** ##

## -------------------------------------------------------------------- ##
## Generate a simulated Protea trait
## -------------------------------------------------------------------- ##

## Set default trait regression coefficients
clim_vars_beta <- rep( 0, length( clim_vars ) )

## Make vector of noise levels
noise_vect <- seq( from = 0, to = 10, by = 0.5 )

## Use dim_reduce_generate_sim_data function to create a dataset of 
## simulated values with various degrees of noise

## -------------------------------------------------------------------------- ##
## First create trait for scenario with three environtmental
## variables, all which have the same influence
clim_vars_beta[ which( clim_vars == "MMP.07" ) ] <- 0.8
clim_vars_beta[ which( clim_vars == "MTmin.07" ) ] <- 0.8
clim_vars_beta[ which( clim_vars == "Elevation" ) ] <- 0.8


sim_trait_df_3_eq <- lapply( noise_vect, dim_reduce_generate_sim_data, 
                        df = filter( prot_pel_df, genus == "Protea" ), 
                        clim_vars = clim_vars, 
                        clim_vars_beta = clim_vars_beta,
                        scale = TRUE )

## -------------------------------------------------------------------------- ##
## Create trait for scenario with three environmental variables,
## two with equal incfluence
clim_vars_beta[ which( clim_vars == "MMP.07" ) ] <- 0.8
clim_vars_beta[ which( clim_vars == "MTmin.07" ) ] <- 0.2
clim_vars_beta[ which( clim_vars == "Elevation" ) ] <- 0.2

sim_trait_df_2_eq <- lapply( noise_vect, dim_reduce_generate_sim_data, 
                             df = filter( prot_pel_df, genus == "Protea" ), 
                             clim_vars = clim_vars, 
                             clim_vars_beta = clim_vars_beta,
                             scale = TRUE )

## -------------------------------------------------------------------------- ##
## Create trait for scenario with three environmental variables,
## all with different influence
clim_vars_beta[ which( clim_vars == "MMP.07" ) ] <- 0.8
clim_vars_beta[ which( clim_vars == "MTmin.07" ) ] <- 0.5
clim_vars_beta[ which( clim_vars == "Elevation" ) ] <- 0.2

sim_trait_df_no_eq <- lapply( noise_vect, dim_reduce_generate_sim_data, 
                             df = filter( prot_pel_df, genus == "Protea" ), 
                             clim_vars = clim_vars, 
                             clim_vars_beta = clim_vars_beta,
                             scale = TRUE )


## -------------------------------------------------------------------------- ##
## Merge simulate traits

sim_trait_df <- c( sim_trait_df_3_eq, sim_trait_df_2_eq, sim_trait_df_no_eq )


## Format into a data.frame and name columns
sim_trait_df <- ldply( sim_trait_df )
sim_trait_df <- as.data.frame( t( sim_trait_df ) )
sim_trait_names <- paste0( "sim_trait_noise_", noise_vect )
sim_trait_names <- paste0( sim_trait_names, 
                           rep( c( "_3eq", "_2eq", "_noeq" ), each = length( noise_vect ) ) )
names( sim_trait_df ) <- sim_trait_names

## Merge the simulated trait values with the environmental predictors
sim_trait_df <- cbind( filter( prot_pel_df, genus == "Protea" ), sim_trait_df )

## -------------------------------------------------------------------- ##
## Calculate signal to noise ratio
## -------------------------------------------------------------------- ##

## Calculate mean and SD for each species within sites
sig_noise_df <- group_by( sim_trait_df, Site_location, Species_name) %>%
  summarize( mean_noise_0.5 = mean( sim_trait_noise_0.5_3eq ),
             sd_noise_0.5 = sd( sim_trait_noise_0.5_3eq ) )

mean( sig_noise_df$sd_noise_0.5, na.rm = TRUE ) / 0.25

## ******************************************************************** ##
## ******************************************************************** ##
## Run Curtis & Ghosh algorithm for simulated data
## ******************************************************************** ##
## ******************************************************************** ##

## -------------------------------------------------------------------------- ##
## To-Do:
## We want to run the simulations so that idealy we have at least 50-100
## replications for each scenario, for each noise level. Currently,
## the fit object is > 400 Mb, so we need to reconsider this if we are
## going to produce 100X more runs.
##
## I think I'll begin by ramping up to 10X so that we can decide 
## on exactly what metrics should be saved.

## Create and empty list object to store JAGS model fit results in
sim_fit <- vector( mode = "list" )

## Use a for loop to loop through all of the simulated traits
for ( i in 1:length( sim_trait_names ) ){ 
      #length( sim_trait_names ) ){
  
  sim_fit[[ sim_trait_names[ i ] ]] <-
    dim_reduce_run_CG_JAGS( X = sim_trait_df[ clim_vars ], 
                            y = sim_trait_df[[ sim_trait_names[ i ] ]], 
                            scale_x = TRUE, 
                            use_jags_test_pars = TRUE )
}

save.image( file = "Preliminary.RData" )
  