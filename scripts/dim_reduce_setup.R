## ******************************************************************** ##
## dim_reduce_setup.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2014-12-30
##
## Purpose:
## Basic setup for dimension reduction work
##
## ******************************************************************** ##

## -------------------------------------------------------------------- ##
## Load R packages
## -------------------------------------------------------------------- ##
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2) 
library(knitr)
library(pander)
library(vegan)
library(fields)
library(data.table)
library(R2jags)
library( corrplot )

## -------------------------------------------------------------------- ##
## Make a vector of the climate variable names
## -------------------------------------------------------------------- ##
clim_vars <- c("CDD","CFD","CSU","GDD",
               "MAP","MMP.07","MMP.01",
               "MAT","MTmin.07","MTmax.01",
               "rr2","sdii","ratio",
               "Elevation", "Insolation")
#,"SU","FD","MATmax","MATmin","ECAr20mm","MAP",

## -------------------------------------------------------------------- ##
## Make a vector of the trait names
## -------------------------------------------------------------------- ##
traits <- c( "LMA", "Canopy_area", "LWratio", "FWC",
             "Height", "lamina_thickness" )

## -------------------------------------------------------------------- ##
## Load custum funtions
## -------------------------------------------------------------------- ##
source( "scripts/format_prot_pel_data.R" )
source( "scripts/generate_sim_data.R" )
source( "scripts/run_CG_JAGS.R" )
source( "scripts/calc_SNR.R" )

