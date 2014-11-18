## ******************************************************************** ##
## explore_curtis_ghosh_dimReduction.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2014-07-08
##
## Purpose:
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
## Make a vector of the climate variable names
clim_vars <- c("CDD","CFD","CSU","GDD",
               "MAP","MMP.07","MMP.01",
               "MAT","MTmin.07","MTmax.01",
               "rr2","sdii","ratio")
#,"SU","FD","MATmax","MATmin","ECAr20mm","MAP",

## ******************************************************************** ##
## Get Protea and Pelie datasets
## ******************************************************************** ##

## -------------------------------------------------------------------- ##
## Read in Protea data.frame
protea_df <- read.csv( "data/Protea_CFR_DATA.csv", na.strings = "." )

prot_trait <- c( "LMA", "Canopy_area", "LWratio", "FWC", 
                 "LDMC", "Succulence", "Height", "lamina_thickness" )
# prot_id <- c( "Year", "Site_location", "Species_name" )
# 
# protea_df <- protea_df[ c( prot_id, prot_trait ) ]

## -------------------------------------------------------------------- ##
## Read in Pellie data.frame
pel_df <- read.csv( "data/Pel_CFR_2011_2012.csv" )

## -------------------------------------------------------------------- ##
## Get climate data

## Source the Extract_Climate_Vars function
# source( "~/Dropbox/UConn-PostDoc/Projects/Dimensions-GCFR/Dimensions-Data/scripts/Util_ExtractPoints_WilsonSummary.r" )

## Get protea location climate values
#protea_clim <- 
#  Util_ExtractPoints_WilsonSummary( lon = protea_df$LONGITUDE, lat = protea_df$LATITUDE )
## Write the climate dataset to file
# write.csv( protea_clim, file = "protea_clim.csv", row.names = FALSE )
protea_clim <- read.csv( "protea_clim.csv" )

#protea_clim <- protea_clim[ clim_vars ]

## Scale the climate variables
protea_clim <- scale( protea_clim )

## Add climate to protea_df
protea_df <- cbind( protea_df, protea_clim )


# protea_df <- protea_df[ complete.cases( protea_df ), ]

## Scale protea traits
protea_df[ prot_trait ] <- scale( protea_df[ prot_trait ] )


## ******************************************************************** ##
## Run Curtis and Ghosh model using LMA and all climate values
## ******************************************************************** ##

## Alternatively extract data as a matrix
#crime.pred.mat <- 
X <-
  as.matrix( protea_df[ clim_vars ] )

## Separate response variable into it's own vector
y <- protea_df$LMA

## Define model settings
n.samp <- nrow( protea_df )

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
n.chains <- 4
n.burnin <- 5*10000
n.iter <- 5*72500
n.thin <- 5*50

# n.chains <- 2
# n.burnin <- 5*1000
# n.iter <- 5*7250
# n.thin <- 5*5


jags.par <-
  c( "beta", "gamma", "S", "theta", "xi" )

## Using a similar setup as used by Kent Holsinger in our
## crime data example, below I set **four** separate 
## jags runs for this model fit.
## In order to make this run faster, I'll implement a 
## local cluster.

fit.lma <- vector( mode = "list" )

library( foreach )
library( doSNOW )

cl <- makeCluster( 4, "SOCK" )
registerDoSNOW( cl )

fit.lma <- foreach( i = 1:4, .combine = "list", .packages = c("R2jags"), .export = jags.data ) %dopar% {
  
  ## Run jags model
  jags( data = jags.data,
        inits = NULL,
        parameters = jags.par,
        model.file = model.file, 
        n.chains = n.chains,
        n.burnin = n.burnin,
        n.iter = n.iter,
        n.thin = n.thin,
        DIC = TRUE,
        working.directory = "." )
  
}

stopCluster( cl )


## -------------------------------------------------------------------- ##

## Get model outputs
beta.vals <- as.data.frame( fit.lma$BUGSoutput$sims.list$beta )
gamma.vals <- as.data.frame( fit.lma$BUGSoutput$sims.list$gamma )
S.vals <- as.data.frame( fit.lma$BUGSoutput$sims.list$S )
theta.vals <- as.data.frame( fit.lma$BUGSoutput$sims.list$theta )
xi.vals <- as.data.frame( fit.lma$BUGSoutput$sims.list$xi )
deviance.vals <- as.data.frame( fit.lma$BUGSoutput$sims.list$deviance )

## Add names of variables, where appropriate
names( beta.vals ) <- clim_vars
names( S.vals ) <- clim_vars

## Make plots to look at S versus Beta

## Melt beta matrix
beta_m <- melt( beta.vals, value.name = "beta", variable.name = "clim_var" )

## Melt S matrix
S_melt <- melt( S.vals, value.name = "S" )

## Melt the theta matrix
theta_melt <- melt( theta.vals, value.name = "theta" )

## Combine the melted matrix values
jags_vals_mat <- beta_m
jags_vals_mat$S <- S_melt$S
jags_vals_mat$theta <- theta_melt$theta

## Plot the histograms of beta values
ggplot() + geom_histogram( data = jags_vals_mat, aes( x = beta, fill = factor( clim_var ) ) ) +
  facet_wrap( ~clim_var, scales = "free" ) +
  geom_vline( xintercept = 0, colour = "red" ) +
  scale_fill_discrete( name = "Clim Var" )
ggsave( filename = "figures/prot_lma_beta_hist.pdf", units = "in", width = 10, height = 10 )

## Plot the histograms of beta values, faceted by S values
ggplot() + geom_density( data = jags_vals_mat, aes( x = beta ) ) +
  facet_wrap( ~ S )
  #geom_vline( xintercept = 0, colour = "red" ) +
  #scale_fill_discrete( name = "Clim Var" )
ggsave( filename = "figures/prot_lma_beta_hist.pdf", units = "in", width = 10, height = 10 )


## Plot beta (coeff value) versus S (label ID)
ggplot() + geom_point( data = jags_vals_mat, aes( x = S, y = beta, colour = factor( clim_var ) ) ) + 
  facet_grid( clim_var~.) + 
  xlab( "S" ) + 
  ylab( "beta" ) + 
  scale_colour_discrete( name = "Clim Var" )
ggsave( filename = "figures/prot_lma_beta_vs_S.pdf", units = "in", width = 10, height = 10 )

## Plot theta vesus S
ggplot() + geom_point( data = jags_vals_mat, aes( x = S, y = theta, colour = factor( clim_var ) ) ) + 
  facet_grid( clim_var~., scales = "free" ) + 
  xlab( "S" ) + 
  ylab( "theta" ) + 
  scale_colour_discrete( name = "Clim Var" )
ggsave( filename = "figures/prot_lma_theta_vs_S.pdf", units = "in", width = 10, height = 10 )

## Plot theta vesus beta
ggplot() + geom_point( data = jags_vals_mat, aes( x = beta, y = theta, colour = factor( clim_var ) ) ) + 
  facet_grid( clim_var~., scales = "free" ) + 
  xlab( "beta" ) + 
  ylab( "theta" ) + 
  scale_colour_discrete( name = "Clim Var" )
ggsave( filename = "figures/prot_lma_theta_vs_beta.pdf", units = "in", width = 10, height = 10 )


## ******************************************************************** ##
## ******************************************************************** ##
### !!! DEVELOPMENT COMMENT !!! ###
## Need to make all the changes that were carried out above, for the rest
## of the variables.
## ******************************************************************** ##
## ******************************************************************** ##



## -------------------------------------------------------------------- ##
## Run Curtis and Ghosh model using Canopy and all climate values
## -------------------------------------------------------------------- ##

## Re-define "y"
y <- protea_df$Canopy_area

fit.canopy <- 
  jags( data = jags.data,
        inits = NULL,
        parameters = jags.par,
        model.file = model.file, 
        n.chains = n.chains,
        n.burnin = n.burnin,
        n.iter = n.iter,
        n.thin = n.thin,
        DIC = TRUE,
        working.directory = "." )

## Get model output
canopy.mu.mean <- fit.canopy$BUGSoutput$mean$mu
canopy.mu <- fit.canopy$BUGSoutput$sims.list$mu
canopy.beta.mean <- fit.canopy$BUGSoutput$mean$beta
rownames( canopy.beta.mean ) <- clim_vars
canopy.beta <- fit.canopy$BUGSoutput$sims.list$beta
canopy.beta <- as.data.frame( canopy.beta )
names( canopy.beta ) <- clim_vars

## Look at boxplots
ggplot( data = melt( canopy.beta ),
        aes( x = variable, y = value ) ) +
  geom_boxplot() +
  xlab( "Climate predictor variable" ) +
  ylab( "Regression coefficient" )

## Look at density pltos
ggplot( data = melt( canopy.beta ),
        aes( x = value ) ) +
  geom_density() +
  facet_wrap( ~variable ) +
  ylab( "Beta value" )

## -------------------------------------------------------------------- ##
## Run Curtis and Ghosh model using Canopy and all climate values
## -------------------------------------------------------------------- ##

## Re-define "y"
y <- protea_df$LWratio

fit.lwr <- 
  jags( data = jags.data,
        inits = NULL,
        parameters = jags.par,
        model.file = model.file, 
        n.chains = n.chains,
        n.burnin = n.burnin,
        n.iter = n.iter,
        n.thin = n.thin,
        DIC = TRUE,
        working.directory = "." )

## Get model output
lwr.mu.mean <- fit.lwr$BUGSoutput$mean$mu
lwr.mu <- fit.lwr$BUGSoutput$sims.list$mu
lwr.beta.mean <- fit.lwr$BUGSoutput$mean$beta
rownames( lwr.beta.mean ) <- clim_vars
lwr.beta <- fit.lwr$BUGSoutput$sims.list$beta
lwr.beta <- as.data.frame( lwr.beta )
names( lwr.beta ) <- clim_vars

## Look at boxplots
ggplot( data = melt( lwr.beta ),
        aes( x = variable, y = value ) ) +
  geom_boxplot() +
  xlab( "Climate predictor variable" ) +
  ylab( "Regression coefficient" )

## Look at density pltos
ggplot( data = melt( lwr.beta ),
        aes( x = value ) ) +
  geom_density() +
  facet_wrap( ~variable ) +
  ylab( "Beta value" )
