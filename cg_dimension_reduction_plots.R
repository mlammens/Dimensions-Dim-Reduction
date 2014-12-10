## ******************************************************************** ##
## cg_dimension_reduction_plots.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2014-12-10
##
## Purpose:
## Make plots and figures following dimension reduction by the methods
## of Curtis and Ghosh 2011
##
## ******************************************************************** ##

require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2) 
require(knitr)
require(pander)
require(vegan)
require(fields)
require(data.table)
require(R2jags)


## -------------------------------------------------------------------- ##
## FUNCTION: make_jags_results_list
##
## Author: Matthew Aiello-Lammens
## Date Created: ￼2014-12-10
##
## Purpose:
##
## -------------------------------------------------------------------- ##
make_jags_results_list <- function( fit ){
  library( reshape2 )
  
  ## Make a function to extract the model outputs
  get_mod_outs <- function( x ){ lapply( X = x$BUGSoutput$sims.list, FUN = as.data.frame ) }
  
  ## Extract model outputs for all trait values
  all_mod_outs <- lapply( fit, FUN = get_mod_outs )
  
  
  make_mod_out_df <- function( mod_out_dfs ){
    ## Make a vector of climate vars
    clim_vars <- c("CDD","CFD","CSU","GDD",
                   "MAP","MMP.07","MMP.01",
                   "MAT","MTmin.07","MTmax.01",
                   "rr2","sdii","ratio",
                   "Elevation", "Insolation")
    ## Add climate vars to beta
    names( mod_out_dfs$beta ) <- clim_vars
    ## Melt beta matrix
    beta_m <- melt( mod_out_dfs$beta, value.name = "beta", variable.name = "clim_var" )
    ## Melt S matrix
    S_melt <- melt( mod_out_dfs$S, value.name = "S" )
    ## Melt the theta matrix
    theta_melt <- melt( mod_out_dfs$theta, value.name = "theta" )
    ## Melt the pi matrix
    pi_melt <- melt( mod_out_dfs$pi, value.name = "pi" )
    
    ## Combine the melted matrix values
    jags_vals_mat <- beta_m
    jags_vals_mat$S <- S_melt$S
    jags_vals_mat$theta <- theta_melt$theta
    jags_vals_mat$pi <- pi_melt$pi
    
    return( jags_vals_mat )
  }
  
  
  ## Make a data.frame of the outputs for each of the traits
  all_mod_outs_df <- lapply( all_mod_outs, make_mod_out_df )
  
  ## Return this list object
  return( all_mod_outs_df )
}



## Plot the histograms of beta values
ggplot() + geom_histogram( data = protea_model_res$LMA, aes( x = beta, fill = factor( clim_var ) ) ) +
  facet_wrap( ~clim_var, scales = "free" ) +
  geom_vline( xintercept = 0, colour = "red" ) +
  scale_fill_discrete( name = "Clim Var" )
ggsave( filename = "figures/prot_lma_beta_hist.pdf", units = "in", width = 10, height = 10 )

## Plot the histograms of beta values, faceted by S values
ggplot() + geom_density( data = protea_model_res$LMA, aes( x = beta ) ) +
  facet_wrap( ~ S )
#geom_vline( xintercept = 0, colour = "red" ) +
#scale_fill_discrete( name = "Clim Var" )
ggsave( filename = "figures/prot_lma_beta_hist_by_S.pdf", units = "in", width = 10, height = 10 )

## Plot histograms of pi values
ggplot() + geom_histogram( data = protea_model_res$LMA, aes( x = pi, fill = factor( clim_var ) ) ) +
  facet_wrap( ~clim_var, scales = "free" ) +
  scale_fill_discrete( name = "Clim Var" )


## Plot beta (coeff value) versus S (label ID)
ggplot() + geom_point( data = protea_model_res$LMA, aes( x = S, y = beta, colour = factor( clim_var ) ) ) + 
  facet_grid( clim_var~.) + 
  xlab( "S" ) + 
  ylab( "beta" ) + 
  scale_colour_discrete( name = "Clim Var" )
ggsave( filename = "figures/prot_lma_beta_vs_S.pdf", units = "in", width = 10, height = 10 )

## Plot theta vesus S
ggplot() + geom_point( data = protea_model_res$LMA, aes( x = S, y = theta, colour = factor( clim_var ) ) ) + 
  facet_grid( clim_var~., scales = "free" ) + 
  xlab( "S" ) + 
  ylab( "theta" ) + 
  scale_colour_discrete( name = "Clim Var" )
ggsave( filename = "figures/prot_lma_theta_vs_S.pdf", units = "in", width = 10, height = 10 )

## Plot theta vesus beta
ggplot() + geom_point( data = protea_model_res$LMA, aes( x = beta, y = theta, colour = factor( clim_var ) ) ) + 
  facet_grid( clim_var~., scales = "free" ) + 
  xlab( "beta" ) + 
  ylab( "theta" ) + 
  scale_colour_discrete( name = "Clim Var" )
ggsave( filename = "figures/prot_lma_theta_vs_beta.pdf", units = "in", width = 10, height = 10 )
