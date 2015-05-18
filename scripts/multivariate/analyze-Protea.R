## ******************************************************************** ##
## analyze-Protea.R
##
## Author: Kent Holsinger
## Date created: 2015-05-14
##
## Derived from: dim_reduce_run_CG_JAGS.R
## Author: Matthew Aiello-Lammens
## Date Created: 2015-01-05
##
## Purpose:
##
## -------------------------------------------------------------------- ##
## Args:
#' @param X Matrix of predictor values
#' @param Y Vector of response values
#' @param M_add Used to determine number of "labels", Curtis and Ghosh
#' recommend the number of predictors + 25
#' @param model_file File containing JAGS model
#' @param use_jags_test_pars Use JAGS test parameters, results in faster, but
#' less accurate model results (default = TRUE)
##
## ******************************************************************** ##

rm(list=ls())

covars <- c("Stone_Vol", "P_Bray_II_mg_kg", "K_mg_kg", "Na_Exchangeable_cations_cmol_kg",
            "K_Exchangeable_cations_cmol_kg", "Ca_Exchangeable_cations_cmol_kg",
            "Mg_Exchangeable_cations_cmol_kg", "C", "Na", "K", "Ca", "Mg")
response <- c("LMA","LWratio","LDMC","Succulence","Canopy_area")

is.complete <- function(x) {
  return(sum(is.na(x)) == 0)
}

standardize <- function(x) {
  mu <- mean(x, na.rm=TRUE)
  sdev <- sd(x, na.rm=TRUE)
  return((x-mu)/sdev)
}

dim_reduce <- function(X, y,
                       M_add = 25,
                       use_jags_test_pars=TRUE,
                       use_jags=TRUE)
{

  ## Check that X is a matrix
  if( !is.matrix( X ) ){
    X <- as.matrix( X )
  }
  ## Add intercept column to X
  X <- cbind(rep(1,nrow(x)), X)

  ## Define model settings - number of samples
  n.samp <- nrow( X )

  ## The next parameter, M, is somewhat arbitrarily set. The authors
  ## write use a "suitably large M", and define it as p + 25, where
  ## p is the number of coefficients in thier examples
  M <- ncol( X ) + M_add

  ## Set number of regression coefficients
  K <- ncol( X )

  ## Set number of response variables
  ##
  n.dim <- ncol( y )

  ## Set parameters for Wishart prior
  ##
  Omega <- diag(x=1.0, nrow=n.dim, ncol=n.dim)
  wish.nu <- nrow(Omega) + 2

  ## Define the variable names used for jags data
  jags.data <-
    c( "X", "n.samp", "M", "K", "y", "Omega", "wish.nu", "n.dim" )

  if (use_jags) {
    library(R2jags)

    model_file="scripts/multivariate/analyze-protea.jags"
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
      c( "beta", "gamma", "S", "theta", "xi", "pi", "Sigma" )

    ## Run jags model
    fit <-  jags(data = jags.data,
                 inits = NULL,
                 parameters = jags.par,
                 model.file = model_file,
                 n.chains = n.chains,
                 n.burnin = n.burnin,
                 n.iter = n.iter,
                 n.thin = n.thin,
                 DIC = TRUE,
                 working.directory = ".")
  } else {
    library(rstan)

    model_file="scripts/multivariate/analyze-protea.stan"
    fit <- stan(model_file,
                data=list(nSamp=n.samp,
                          nDim=n.dim,
                          K=K,
                          M=M,
                          y=y,
                          X=X,
                          Omega=Omega,
                          nu=wish.nu),
                pars=c("beta",
                       "gamma",
                       "S",
                       "theta",
                       "xi",
                        "pi",
                       "Sigma"))
  }

  ## Return the jags model fit
  return( fit )

}

## Read trait and site data and merge into single data frame
##
traits <- read.csv("data/NewData/Protea_Trait_Data.csv", na.strings=".")
sites <- read.csv("data/NewData/Protea_site_Data.csv", na.strings=".")
protea <- merge(traits, sites, by="Site_name")

## Exclude individuals with unscored traits or covariates
##
select <- apply(protea[,c(covars,response)], 1, is.complete)
protea <- protea[select,,drop=TRUE]

## select covariates
##
x <- protea[,covars]
x <- apply(x, 2, standardize)

## select response variables
##
y <- protea[,response]
y <- apply(y, 2, standardize)

fit <- dim_reduce(x, y, use_jags=FALSE)
