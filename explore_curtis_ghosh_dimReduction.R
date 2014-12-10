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

make_new_clim_df <- FALSE
use_jags_test_pars <- FALSE

#genus <- "Protea"
genus <- "Pelargonium"

## -------------------------------------------------------------------- ##
## Make a vector of the climate variable names
clim_vars <- c("CDD","CFD","CSU","GDD",
               "MAP","MMP.07","MMP.01",
               "MAT","MTmin.07","MTmax.01",
               "rr2","sdii","ratio",
               "Elevation", "Insolation")
#,"SU","FD","MATmax","MATmin","ECAr20mm","MAP",

## ******************************************************************** ##
## Get Protea and Pelie datasets
## ******************************************************************** ##

## Read in Protea data.frame
protea_df <- read.csv( "data/Protea_CFR_DATA.csv", na.strings = "." )

## Read in Pellie data.frame
pel_df <- read.csv( "data/Pel_CFR_2011_2012.csv", na.strings = "."  )

## Make a vector of the trait names
traits <- c( "LMA", "Canopy_area", "LWratio", "FWC", 
             "Height", "lamina_thickness" )


## -------------------------------------------------------------------- ##
## Combine protea and pellie datasets
prot_pel_df <- rbind( dplyr::select( protea_df, Site_location, LONGITUDE, LATITUDE,
                              genus, LMA, Canopy_area, LWratio, FWC, 
                              Height, lamina_thickness ),
                      dplyr::select( pel_df, Site_location, LONGITUDE, LATITUDE,
                              genus, LMA, Canopy_area, LWratio, FWC, 
                              Height, lamina_thickness ) )

## -------------------------------------------------------------------- ##
## Get climate data

if( make_new_clim_df ){
  ## Extract lat/lon values for unique Site_locations
  sites <- filter( prot_pel_df, !duplicated( Site_location ) ) %>% 
    dplyr::select( Site_location, LONGITUDE, LATITUDE )
  
  ## Source the Extract_Climate_Vars function
  source( "~/Dropbox/UConn-PostDoc/Projects/Dimensions-GCFR/Dimensions-Data/scripts/Util_ExtractPoints_WilsonSummary.r" )
  ## Get protea location climate values
  prot_pel_clim <- 
   Util_ExtractPoints_WilsonSummary( lon = sites$LONGITUDE, lat = sites$LATITUDE )
  ## Write the climate dataset to file
  write.csv( prot_pel_clim, file = "prot_pel_clim.csv", row.names = FALSE )
}

## Read the saved climate values
prot_pel_clim <- read.csv( "prot_pel_clim.csv" )

## Read in the additional climate values, from Mitchell et al. 2014
prot_pel_clim_add <- read.csv( "data/Protea_Pellie_Climate.csv" )

## Merge the two climate data.frames
prot_pel_clim <- 
  merge( prot_pel_clim, 
         dplyr::select( prot_pel_clim_add, -(CDD:MAT) ),
         by.x = c( "lon", "lat"),
         by.y = c( "LONGITUDE", "LATITUDE" ) )

## Scale the climate variables
prot_pel_clim[ clim_vars ] <- scale( prot_pel_clim[ clim_vars ] )

## Merge the trait and climate variables
prot_pel_df <- 
  merge( dplyr::select( prot_pel_df, -genus ), 
         dplyr::select( prot_pel_clim, -(lon:lat) ),
         by = "Site_location" )

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



