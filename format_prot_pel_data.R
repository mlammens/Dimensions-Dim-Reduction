## ******************************************************************** ##
## format_prot_pel_data.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2014-12-30
##
## Purpose:
## Read in and format the protea and pellargonium datasets to be used in 
## the dimension reduction and variable selection work.
##
## ******************************************************************** ##

## -------------------------------------------------------------------- ##
## Set script params
## -------------------------------------------------------------------- ##
format_prot_pel_data <- function( dim_red_basedir,
                                  make_new_clim_df = FALSE,
                                  clim_vars,
                                  traits ){


  ## -------------------------------------------------------------------- ##
  ## Import pellie/prot data set
  ## -------------------------------------------------------------------- ##

  ## Read in Protea data.frame
  protea_df <- read.csv( paste0( dim_red_basedir, "data/Protea_CFR_DATA.csv" ), na.strings = "." )

  ## Read in Pellie data.frame
  pel_df <- read.csv( paste0( dim_red_basedir, "data/Pel_CFR_2011_2012.csv" ), na.strings = "."  )

  ## -------------------------------------------------------------------- ##
  ## Combine protea and pellie datasets
  prot_pel_df <- rbind( dplyr::select( protea_df, Site_location, LONGITUDE, LATITUDE,
                                       genus, Species_name, 
                                       LMA, Canopy_area, LWratio, FWC,
                                       Height, lamina_thickness ),
                        dplyr::select( pel_df, Site_location, LONGITUDE, LATITUDE,
                                       genus, Species_name,
                                       LMA, Canopy_area, LWratio, FWC,
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
    write.csv( prot_pel_clim, file = paste0( dim_red_basedir, "data/prot_pel_clim.csv" ), row.names = FALSE )
  }

  ## Read the saved climate values
  prot_pel_clim <- read.csv( paste0( dim_red_basedir, "data/prot_pel_clim.csv" ) )

  ## Read in the additional climate values, from Mitchell et al. 2014
  prot_pel_clim_add <- read.csv( paste0( dim_red_basedir, "data/Protea_Pellie_Climate.csv" ) )

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
  
  ## Return only the traits and climate variables that have been selected
  prot_pel_df <- 
    dplyr::select( prot_pel_df, Site_location:Species_name, genus,
                   which( names( prot_pel_df ) %in% traits ), 
                   which( names( prot_pel_df ) %in% clim_vars ) )

  return( prot_pel_df )
}
