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
source( "scripts/dim_reduce_setup.R" )

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
  cor( filter( prot_pel_df, genus == "Protea" ) %>% 
         dplyr::select( which( names( prot_pel_df ) %in% clim_vars ) ) )


## ******************************************************************** ##
## Generate simulated data based on correlation relationships
## ***
## We want to simulate a trait that is a function of three  predictor 
## variables. These variables should be highly correlated with other 
## variables. We also want to examine the effects of various levels of 
## noise.
##
## We use six different simulation schemes, as documented in the 
## manuscript. For each scheme, we examine the influence of multiple
## different levels of noise. Each scheme x noise pair is replicated
## 50 times.
## ******************************************************************** ##

## -------------------------------------------------------------------------- ##
## Noise
## ***
## Make vector of noise levels to be used for all simulations
## -------------------------------------------------------------------------- ##
noise_vect <- seq( from = 0, to = 10, by = 0.5 )

## -------------------------------------------------------------------------- ##
## Default environmental variable coefficients
## ***
## Set the coefficients for all environemntal variables to 0, indicating
## that they have no effect on the simulated trait
## -------------------------------------------------------------------------- ##
clim_vars_coef <- rep( 0, length( clim_vars ) )

## -------------------------------------------------------------------------- ##
## Simulation scheme 1
## ***
## Three standardized coefficients = 0.8
## Each of the three variables are relatively uncorrelated with the 
## other two, but higly correlated with several other vars.
##            MMP.07  Elev
## MTmax.01   -0.17   -0.51
## MMP.07             -0.46
## -------------------------------------------------------------------------- ##

## Set simulation 1 coefficients
sim1_coef <- clim_vars_coef
sim1_coef[ which( clim_vars == "MMP.07" ) ] <- 0.8
sim1_coef[ which( clim_vars == "MTmax.01" ) ] <- 0.8
sim1_coef[ which( clim_vars == "Elevation" ) ] <- 0.8

## Generate simulated traits for each level of noise, as per 
## 'noise_vect' set above
sim1_trait <- lapply( noise_vect, generate_sim_data, 
                      df = filter( prot_pel_df, genus == "Protea" ), 
                      clim_vars = clim_vars, 
                      clim_vars_beta = sim1_coef,
                      scale = TRUE )

## Make the list of simulated traits X noise levels into a data.frame
## This requires transposing, `t`, the data.frame returned from `ldply`
sim1_trait_df <- t( ldply( sim1_trait ) )
sim1_trait_df <- as.data.frame( sim1_trait_df )

## Assign column names to the data.frame
sim1_trait_names <- paste0( "noise_", noise_vect )
names( sim1_trait_df ) <- sim1_trait_names

## Merge the simulated trait values with the environmental predictors
sim1_trait_df <- cbind( filter( prot_pel_df, genus == "Protea" ), 
                        sim1_trait_df )

## Run C&G JAGS model
## -------------------------------------------------------------------------- ##

## Create and empty list object to store JAGS model fit results in
sim1_fit <- vector( mode = "list" )

## Use a for loop to loop through all of the simulated traits
system.time(
for ( i in 1:length( sim1_trait_names ) ){ 
  
  sim1_fit[[ sim1_trait_names[ i ] ]] <-
    run_CG_JAGS( X = sim1_trait_df[ clim_vars ], 
                 y = sim1_trait_df[[ sim1_trait_names[ i ] ]], 
                 scale_x = TRUE, 
                 use_jags_test_pars = TRUE )
}
)




## -------------------------------------------------------------------------- ##
## Simulation scheme 2
## ***
## -------------------------------------------------------------------------- ##


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
  