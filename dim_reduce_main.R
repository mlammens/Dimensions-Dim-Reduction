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
source( "format_prot_pel_data.R" )
source( "dim_reduce_generate_sim_data.R" )

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


## -------------------------------------------------------------------- ##
## Call dim_reduce_generate_sim_data function
## -------------------------------------------------------------------- ##

clim_vars_beta <- rep( 0, length( clim_vars ) )
clim_vars_beta[ which( clim_vars == "MMP.07" ) ] <- 0.8
clim_vars_beta[ which( clim_vars == "MTmin.07" ) ] <- 0.8
clim_vars_beta[ which( clim_vars == "Elevation" ) ] <- 0.8

sim_trait <- dim_reduce_generate_sim_data( df = filter( prot_pel_df, genus == "Protea" ), 
                                           clim_vars = clim_vars, 
                                           clim_vars_beta = clim_vars_beta, 
                                           noise = 0.2 )

