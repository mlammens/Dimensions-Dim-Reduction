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
library( plyr )
library( dplyr )
library( car )

standardize <- function(x) {
  if (is.numeric(x)) {
    y <- (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
  } else {
    y <- x
  }
  y
}

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
#crime_lm <- lm( formula = y ~ -1 + . , data = crime ) ## No intercept model
#summary( crime_lm ) ## *DOES NOT* match results presented in Curtish and Ghosh (w or w/o intercept)
#vif( crime_lm ) ## Needs model *with* interecept to make sense. Approximately matches Curtis and Ghosh

## -------------------------------------------------------------------- ##
## Construct Curtis and Ghosh Baysian dimension reduction model
## -------------------------------------------------------------------- ##

## Separate predictors into their own vectors
M <- standardize(crime$M)
Ed <- standardize(crime$Ed)
Po1 <- standardize(crime$Po1)
Po2 <- standardize(crime$Po2)
LF <- standardize(crime$LF)
M.F <- standardize(crime$M.F)
Pop <- standardize(crime$Pop)
NW <- standardize(crime$NW)
U1 <- standardize(crime$U1)
U2 <- standardize(crime$U2)
GDP <- standardize(crime$GDP)
Ineq <- standardize(crime$Ineq)
Prob <- standardize(crime$Prob)
Time <- standardize(crime$Time)

## Define the parameters in a list structure
crime.data <-
  list( "M", "Ed", "Po1", "Po2", "LF", "M.F",
        "Pop", "NW", "U1", "U2", "GDP", "Ineq",
        "Prob", "Time" )

## Alternatively extract data as a matrix
#crime.pred.mat <-
X <- apply(as.matrix( select( crime, -y ) ), 2, standardize)

## Separate response variable into it's own vector
y <- standardize(crime$y)

## Define model settings
n.samp <- nrow( crime )
## The next parameter, M, is somewhat arbitrarily set. The authors
## write use a "suitably large M", and define it as p + 25, where
## p is the number of coefficients in thier examples
M <- ( ncol( crime ) - 1 ) + 25
## Set number of regression coefficients
K <- ncol( crime ) - 1

jags.data <- c( "X", "n.samp", "M", "K", "y" )

anchored = FALSE

## Set model file
if (anchored) {
  model.file <- "curtis_ghosh_anchored.jags"
} else {
  model.file <- "curtis_ghosh_example.jags"
}

## Set JAGs parameters
n.chains <- 4
n.burnin <- 10000
n.iter <- 72500
n.thin <- 50
# n.burnin <- 5*1000
# n.iter <- 5*7250
# n.thin <- 5*50

jags.par <- c( "beta", "gamma", "S", "theta", "xi", "id")

fit <- jags(data = jags.data,
            inits = NULL,
            parameters = jags.par,
            model.file = model.file,
            n.chains = n.chains,
            n.burnin = n.burnin,
            n.iter = n.iter,
            n.thin = n.thin,
            DIC = TRUE,
            working.directory = ".")

old.opt <- options(width=180)
print(fit, 3)
options(old.opt)


