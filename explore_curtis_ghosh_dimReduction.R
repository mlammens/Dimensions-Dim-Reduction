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
protea_df <- read.csv( "/Users/mlammens/Dropbox/UConn-PostDoc/Projects/Dimensions-GCFR/Dimensions---Traits-analysis/Protea_CFR_DATA.csv", na.strings = "." )

prot_trait <- c( "LMA", "Canopy_area", "LWratio", "FWC", 
                 "LDMC", "Succulence", "Height", "lamina_thickness" )
# prot_id <- c( "Year", "Site_location", "Species_name" )
# 
# protea_df <- protea_df[ c( prot_id, prot_trait ) ]

## -------------------------------------------------------------------- ##
## Read in Pellie data.frame
pel_df <- read.csv( "/Users/mlammens/Dropbox/UConn-PostDoc/Projects/Dimensions-GCFR/Dimensions---Traits-analysis/Pel_CFR_2011_2012.csv" )

## -------------------------------------------------------------------- ##
## Get climate data

## Source the Extract_Climate_Vars function
source( "/Users/mlammens/Dropbox/UConn-PostDoc/Projects/Dimensions-GCFR/Dimensions-Data/scripts/Util_ExtractPoints_WilsonSummary.r" )

## Get protea location climate values
protea_clim <- 
  Util_ExtractPoints_WilsonSummary( lon = protea_df$LONGITUDE, lat = protea_df$LATITUDE )
## Write the climate dataset to file
write.csv( protea_clim, file = "protea_clim.csv", row.names = FALSE )

#protea_clim <- protea_clim[ clim_vars ]

## Scale the climate variables
protea_clim <- scale( protea_clim )

## Add climate to protea_df
protea_df <- cbind( protea_df, protea_clim )


# protea_df <- protea_df[ complete.cases( protea_df ), ]

## Scale protea traits
protea_df[ prot_trait ] <- scale( protea_df[ prot_trait ] )


## -------------------------------------------------------------------- ##
## Run Curtis and Ghosh model using LMA and all climate values
## -------------------------------------------------------------------- ##

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


jags.data <-
  c( "X", "n.samp", "M", "K", "y" )

## Set model file
model.file <- "curtis_ghosh_example.jags"

## Set JAGs parameters
n.chains <- 4
n.burnin <- 1000
n.iter <- 5000
n.thin <- 10

jags.par <-
  c( "mu", "beta" )

fit.lma <- 
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
lma.mu.mean <- fit.lma$BUGSoutput$mean$mu
lma.mu <- fit.lma$BUGSoutput$sims.list$mu
lma.beta.mean <- fit.lma$BUGSoutput$mean$beta
rownames( lma.beta.mean ) <- clim_vars
lma.beta <- fit.lma$BUGSoutput$sims.list$beta
lma.beta <- as.data.frame( lma.beta )
names( lma.beta ) <- clim_vars

## Look at boxplots
ggplot( data = melt( lma.beta ),
        aes( x = variable, y = value ) ) +
  geom_boxplot() +
  xlab( "Climate predictor variable" ) +
  ylab( "Regression coefficient" )

## Look at density pltos
ggplot( data = melt( lma.beta ),
        aes( x = value ) ) +
  geom_density() +
  facet_wrap( ~variable ) +
  ylab( "Beta value" )

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
