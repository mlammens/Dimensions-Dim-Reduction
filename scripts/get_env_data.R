## ************************************************************************** ##
## get_env_data.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2015-05-18
##
## Purpose:
## Exract the full set of Wilson and Schulze climate variables for the 
## full set of protea occurences. These occurrences include several sites
## outside of the spatial extent of the Wilson dataset.
##
## ************************************************************************** ##

## -------------------------------------------------------------------------- ##
# Source climate extractions scripts and load required packages
## -------------------------------------------------------------------------- ##
library( dplyr )
library( raster )
library( rgdal )
library( sp )

## Set the location of the Dimensions-Data repository - currently setup for
## MA-L computer
DIM_DATA_REPO <- "~/Dropbox/UConn-PostDoc/Projects/Dimensions-GCFR/Dimensions-Data/"

## Source climate extraction scripts
source( paste0( DIM_DATA_REPO, "scripts/Util_ExtractPoints_WilsonSummary.r" ) )
source( paste0( DIM_DATA_REPO, "scripts/Util_ExtractPoints_Schulze2007.R" ) )

## -------------------------------------------------------------------------- ##
## Read in site data and use lat/lon to extract climate vars
## -------------------------------------------------------------------------- ##
sites <- read.csv( "data/NewData/Protea_Site_Data.csv", na.strings = ".", as.is = TRUE )

## Get Wilson measures
clim_wilson <- Util_ExtractPoints_WilsonSummary( lon = sites$longitude, lat = sites$latitude )

## Get Schultze measures
clim_schulze <- Util_ExtractPoints_Schulze2007( lon = sites$longitude, lat = sites$latitude )

## -------------------------------------------------------------------------- ##
## Merge the two datasets - requires first finding and removing lat/lon duplicates
## -------------------------------------------------------------------------- ##

## Remove duplicate lat/lon sets from the two datasets
dups_wilson <- which( duplicated( paste0( clim_wilson$lat, clim_wilson$lon ) ) )
if( length( dups_wilson > 0 ) ){
  clim_wilson <- clim_wilson[ -dups_wilson, ]
}

dups_schulze <- which( duplicated( paste0( clim_schulze$lat, clim_schulze$lon ) ) )
if( length( dups_schulze > 0 ) ){
  clim_schulze <- clim_schulze[ -dups_schulze, ]
}

## Merge the plot_latlon dataset with the two climate data sets
plot_clim <- merge( sites, clim_wilson,
                    by.x = c("latitude", "longitude" ),
                    by.y = c("lat", "lon"),
                    all.x = TRUE )

plot_clim <- merge( plot_clim, clim_schulze,
                    by.x = c("latitude", "longitude" ),
                    by.y = c("lat", "lon"),
                    all.x = TRUE )

write.csv( plot_clim, file = "data/NewData/Protea_Site_Data_FULL.csv", row.names = FALSE )
