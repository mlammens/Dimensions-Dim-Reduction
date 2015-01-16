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

## Plot for visual interpretation
corrplot.mixed( prot_pel_env_cor, upper = "ellipse" )

## This shows the following variables to by highly **positively** correlated ( >0.8 )
## * CSU - Tmax.01
## * GDD - Tmin.07
## * GDD - MAT
## * MAP - rr2
## * MAP - sdii
## * MAP - MMP.07
## * rr2 - sdii
## * rr2 - MMP.07
## * sdii - MMP.07
## * Tmax.01 - MAT

## And the following are highly **negatively** correlated ( < -0.8 )
## * CDD - rr2
## * CFD - GDD
## * CFD - Tmin.07
## * GDD - Elevation
## * Tmin.07 - Elevation


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

clim_vars_beta <- rep( 0, length( clim_vars ) )
clim_vars_beta[ which( clim_vars == "MMP.07" ) ] <- 0.8
clim_vars_beta[ which( clim_vars == "MTmin.07" ) ] <- 0.8
clim_vars_beta[ which( clim_vars == "Elevation" ) ] <- 0.8

noise_vect <- seq( from = 0, to = 1, by = 0.1 )

## Use dim_reduce_generate_sim_data function to create a dataset of 
## simulated values with various degrees of noise
sim_trait_df <- lapply( noise_vect, dim_reduce_generate_sim_data, 
                        df = filter( prot_pel_df, genus == "Protea" ), 
                        clim_vars = clim_vars, 
                        clim_vars_beta = clim_vars_beta,
                        scale = TRUE )

## Format into a data.frame and name columns
sim_trait_df <- ldply( sim_trait_df )
sim_trait_df <- as.data.frame( t( sim_trait_df ) )
sim_trait_names <- paste0( "sim_trait_noise_", noise_vect )
names( sim_trait_df ) <- sim_trait_names

## Merge the simulated trait values with the environmental predictors
sim_trait_df <- cbind( filter( prot_pel_df, genus == "Protea" ), sim_trait_df )

## -------------------------------------------------------------------- ##
## Calculate signal to noise ratio
## -------------------------------------------------------------------- ##

## Calculate mean value for each species within sites
sig_noise_df <- group_by( sim_trait_df, Site_location, Species_name) %>%
  summarize( mean_noise_0.5 = mean( sim_trait_noise_0.5 ),
             sd_noise_0.5 = sd( sim_trait_noise_0.5 ) )

sig_noise_df$SNR <- sig_noise_df$mean_noise_0.5 / sig_noise_df$sd_noise_0.5

mean( sig_noise_df$SNR, na.rm = TRUE )

## ******************************************************************** ##
## ******************************************************************** ##
## Run Curtis & Ghosh algorithm for simulated data
## ******************************************************************** ##
## ******************************************************************** ##

## Create and empty list object to store JAGS model fit results in
sim_fit <- vector( mode = "list" )

## Use a for loop to loop through all of the simulated traits
for ( i in 11:11 ){ 
      #length( sim_trait_names ) ){
  
  sim_fit[[ sim_trait_names[ i ] ]] <-
    dim_reduce_run_CG_JAGS( X = sim_trait_df[ clim_vars ], 
                            y = sim_trait_df[[ sim_trait_names[ i ] ]], 
                            scale_x = TRUE, 
                            use_jags_test_pars = TRUE )
}


  