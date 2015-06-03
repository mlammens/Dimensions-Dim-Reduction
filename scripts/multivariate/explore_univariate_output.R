## ************************************************************************** ##
## explore_output.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2015-06-03
##
## Purpose:
##
## ************************************************************************** ##

library( plyr )
library( dplyr )

load( "scripts/multivariate/preliminary-results-univariate.RSave" )

## Rearrange results from all chains into a single data.frame
sims <- fit$BUGSoutput$sims.array
sims <- adply( sims, .margins = 2 )

## Give the data.frame column names
names( sims ) <- c( "chain", row.names(fit$BUGSoutput$summary) ) 

sims_w_corr <-
  sims %>% 
  select( chain, contains( "beta" ) ) %>%
  group_by( chain, add = FALSE ) %>%
  do( corr_mat = cor(.[-1]) )

## Make diagonals of cor mats = NA
na_mat <- matrix( data = 1, 
                  nrow = nrow( sims_w_corr$corr_mat[[1]] ), 
                  ncol = nrow( sims_w_corr$corr_mat[[1]] ) )

diag( na_mat ) <- NA

sims_w_corr$corr_mat_diag_na <-
  lapply( sims_w_corr$corr_mat, FUN = function(x){ x * na_mat } )

lapply( sims_w_corr$corr_mat_diag_na, FUN = function(x){ which( x < -0.8, arr.ind = TRUE ) } )
