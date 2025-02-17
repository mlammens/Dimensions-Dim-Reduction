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
#' @param use_jags_test_pars Use JAGS test parameters, results in faster, but
#' less accurate model results (default = TRUE)
#' @param use_jags (default = TRUE). If FALSE, use Stan for inference
##
## ******************************************************************** ##

rm(list=ls())

use.test.pars <- TRUE
use.jags <- FALSE

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
                       use_test_pars=TRUE,
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
  if (is.matrix(y)) {
    n.dim <- ncol( y )
  } else {
    n.dim <- 1
  }

  ## Set parameters for Wishart prior
  ##
  Omega <- diag(x=1.0, nrow=n.dim, ncol=n.dim)
  wish.nu <- nrow(Omega) + 2

  ## Define the variable names used for jags data
  if (is.matrix(y)) {
    jags.data <-
      c( "X", "n.samp", "M", "K", "y", "Omega", "wish.nu", "n.dim" )
  } else {
    jags.data <-
      c( "X", "n.samp", "M", "K", "y", "n.dim" )
  }

  if (use_jags) {
    library(R2jags)

    if (is.matrix(y)) {
      model_file="scripts/multivariate/analyze-protea.jags"
    } else {
      model_file="scripts/multivariate/analyze-Protea-univariate.jags"
    }
    ## Set JAGs parameters
    if( use_test_pars ){
      ## Testing parameters
      n.chains <- 2
      n.burnin <- 5*1000
      n.iter <- 5*7250
      n.thin <- 5*5
    } else {
      ## Final model pars
      n.chains <- 5
      n.burnin <- 10000
      n.iter <- 60000
      n.thin <- 50
    }

    ## Make a vector of parameters to track in JAGs
    jags.par <-
##       c( "beta", "gamma", "S", "theta", "xi", "pi", "var.y", "var.theta" )
    c( "beta", "gamma", "theta", "pi" )

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
    if( use_test_pars ){
      ## Testing parameters
      chains <- 1
      iter <- 100

    } else {
      ## Final model pars
      chains <- 5
      iter <- 2000
    }
    fit <- stan(model_file,
                data=list(nSamp=n.samp,
                          nDim=n.dim,
                          K=K,
                          y=y,
                          X=X),
                pars=c("beta",
                       "gamma",
                       "theta",
                       "pi",
                       "Sigma",
                       "Omega"),
                iter=iter,
                chains=chains)
  }

  ## Return the jags model fit
  return( fit )

}

## Read trait and site data and merge into single data frame
##
traits <- read.csv("data/NewData/Protea_Trait_Data.csv", na.strings=c("NA","."))
sites <- read.csv("data/NewData/Protea_Site_Data_FULL.csv", na.strings=c("NA","."))
protea <- merge(traits, sites, by="Site_name")

## Exclude individuals with unscored traits or covariates
##
source("scripts/multivariate/response-covars.R")
select <- apply(protea[,c(covars,response)], 1, is.complete)
protea <- protea[select,,drop=TRUE]

## select covariates
##
x <- protea[,covars]
x <- apply(x, 2, standardize)

## select response variables
##
y <- protea[,response]
if (is.data.frame(y)) {
  y <- apply(y, 2, standardize)
} else {
  y <- standardize(y)
}

fit <- dim_reduce(x, y, use_test_pars=use.test.pars, use_jags=use.jags)
