## ******************************************************************** ##
## curtis_ghosh_example.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2014-07-07
##
## Purpose:
## Re-run the analyis example using Ehrlich Crime Data presented in 
## Curtis and Ghosh 2011 J. Stat. Theory and Practice.
##
## ******************************************************************** ##

## Load necessary packages
library( R2jags )
library( MASS )
library( dplyr )
library( car )

## Load data set
crime <- UScrime 

## Log-transform variables, as Curtis and Ghosh did. 
## They also dropped So, presumably because it is categorical
crime <- select( crime, -So )
crime <- log( crime )

## -------------------------------------------------------------------- ##
## Construct simple linear regression model
## -------------------------------------------------------------------- ##

#crime_lm <- lm( formula = y ~  . , data = crime )
crime_lm <- lm( formula = y ~ -1 + . , data = crime ) ## No intercept model
summary( crime_lm ) ## *DOES NOT* match results presented in Curtish and Ghosh (w or w/o intercept)
vif( crime_lm ) ## Needs model *with* interecept to make sense. Approximately matches Curtis and Ghosh

## -------------------------------------------------------------------- ##
## Construct Curtis and Ghosh Baysian dimension reduction model
## -------------------------------------------------------------------- ##

## Separate predictors into their own vectors
M <- crime$M
Ed <- crime$Ed
Po1 <- crime$Po1
Po2 <- crime$Po2
LF <- crime$LF
M.F <- crime$M.F
Pop <- crime$Pop
NW <- crime$NW
U1 <- crime$U1
U2 <- crime$U2
GDP <- crime$GDP
Ineq <- crime$Ineq
Prob <- crime$Prob
Time <- crime$Time

## Define the parameters in a list structure
crime.data <- 
  list( "M", "Ed", "Po1", "Po2", "LF", "M.F",
        "Pop", "NW", "U1", "U2", "GDP", "Ineq",
        "Prob", "Time" )

## Alternatively extract data as a matrix
#crime.pred.mat <- 
X <-
  as.matrix( select( crime, -y ) )

## Separate response variable into it's own vector
y <- crime$y

## Define model settings
n.samp <- nrow( crime )
## The next parameter, M, is somewhat arbitrarily set. The authors 
## write use a "suitably large M", and define it as p + 25, where 
## p is the number of coefficients in thier examples
M <- ( ncol( crime ) - 1 ) + 25
## Set number of regression coefficients
K <- ncol( crime ) - 1

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

fit <- 
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
mu.mean <- fit$BUGSoutput$mean$mu
mu <- fit$BUGSoutput$sims.list$mu
beta <- fit$BUGSoutput$mean$beta
rownames( beta ) <- c( "M", "Ed", "Po1", "Po2", "LF", "M.F",
                       "Pop", "NW", "U1", "U2", "GDP", "Ineq",
                       "Prob", "Time" )