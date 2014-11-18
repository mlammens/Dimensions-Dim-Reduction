### Simulated datasets for variable selection of multicollinearity
library(MASS)
library(gtools)
library(Rlab)


### generate simulated data from Y=XB+E ####

betat<-c(3,2,1.5,0,0,0,0,0, 0, 0,3,0, 2, 0, 1.5, 0,0,2, 5, 6);              # coefficients' true value


p=length(betat);                                      #number of coefficients
n=50;                                     #number of observations
V<-matrix (rep(0,p*p), nrow=p, ncol=p);   # Covariance matrix of the design matrix X


for (i in 1:p){
  for (j in 1:p) {
    V[i,j]=0.7^(abs(i-j));
  }
}



sigmat<-3;                     
epsilon<-rnorm(n,0,sigmat);

Xt<-mvrnorm(n,rep(0,p), V);
yt<-Xt%*%betat+epsilon;

L=25+p;                         #Truncated numbers
alpha=1;                      #hyperparameters for DP
a_eta=0.1;
b_eta=0.1;
a_sigma=0.1;
b_sigma=0.1;

X=Xt;

for (i in 1:p)
{
  X[,i]=(Xt[,i]-mean(Xt[,i]))/as.numeric(sqrt(var(Xt[,i])));
}

y=(yt-mean(yt))/as.numeric(sqrt(var(yt)));

######### function for MCMC ############

update.pi0=function (S, pi0) {
  
  # pi0 is the probability beta=0
  m_0=length(which(S==1));
  p=length(S);
  pi0=rbeta(1,m_0+1, p-m_0+1);
  
  return(pi0)
  
}

update.S=function(theta, sigma, S, X, y, pi0, pim){
  
  p=length(S);
  
  for (j in 1:p) {
    
    tempy=y-X[,-j]%*%theta[S[-j]];
    
    lg=length(theta);
    
    temp_p=rep(0,lg)
    
    temp_above=rep(0,lg);
    
    for (h in 1:lg) {
      
      temp_above[h]=-sigma*sum((tempy-X[,j]*theta[h])^2)/2;
      
      if (h==1) {
        temp_p[h]=pi0;
      } else {
        temp_p[h]=(1-pi0)*pim[h-1];
      }
    }
    
    tpistar=rep(0,lg);
    
    for (h in 1:lg){
        tpistar[h]=temp_p[h]/sum(temp_p*exp(temp_above-temp_above[h]));
    }
    
    S_idx=rmultinom(1,1,tpistar);
    S[j]=which(S_idx!=0);
  }
  
  return(S)
}


update.pim=function(S, alpha, pim) {
  
  L=length(pim);
  
  m=rep(0,L);
  for (h in 1:L) {
    m[h]=alpha/L+length(which(S==(h+1)));
  }
  
  pim=rdirichlet(1, m);
  
  return(pim)
}


update.theta=function(theta, sigma, eta, S, X, y) {
  
  L=length(theta);
  
  for (h in 2:L) {
    
    m_idx=which(S==h);
    lg_m=length(m_idx);
    
    if (lg_m!=0) {
      mn_idx=which(S!=h);
      lg_mn=length(mn_idx);
      if (lg_mn!=0) {
        if (lg_mn>1) {
          tempz=y-X[, mn_idx]%*%theta[S[mn_idx]];
        } else {
          tempz=y-X[, mn_idx]*theta[S[mn_idx]];
        }
      } else {
        tempz=y;
      }
      if (lg_m==1) {
        tempx=X[, m_idx];
      } else {
        tempx=rowSums(X[, m_idx]);
      }
      tempx_sum=sum(tempx^2);
      varth=1/(sigma*tempx_sum+eta);
      meanth=varth*sigma*sum(tempz*tempx);
      theta[h]=rnorm(1,meanth, sqrt(varth));
    } else {
      theta[h]=rnorm(1,0, 1/sqrt(eta));
    }
  }
  return (theta)
}


update.beta=function(theta, S) {
  
  beta=theta[S];
  return (beta)
  
}


update.eta=function (theta, a_eta, b_eta){
  
  L=length(theta)-1;
  eta=rgamma(1, shape=L/2+a_eta, rate=sum(theta*theta)/2+b_eta);
  return (eta)
  
}

update.sigma=function ( beta, a_sigma, b_sigma, X, y) {
  tempxy=y-X%*%beta;
  sigma=rgamma(1, shape=n/2+a_sigma, rate=sum(tempxy*tempxy)/2+b_sigma);
  #  sigma=1/9;
}



##### Initial values based on MLE's

M=50;

beta.sim=matrix(0,nrow=p, ncol=M);

pim.sim=matrix(1/L, nrow=L, ncol=M);

S.sim=matrix(1, nrow=p, ncol=M);

eta.sim=matrix(1,nrow=1, ncol=M);

theta.sim=matrix(0, nrow=L+1, ncol=M);

sigma.sim=matrix(1, nrow=1, ncol=M);

pi0.sim=1/2*rep(1,M);




########### RUN MCMC ###############

for (i in 2:M) {
  
  print(i)
  
  ### Sample theta #######
  
  theta.sim[, i]=update.theta(theta.sim[, i-1], sigma.sim[i-1], eta.sim[i-1], S.sim[, i-1], X, y);
  
  ### Sample S #########
  
  S.sim[, i]=update.S(theta.sim[,i], sigma.sim[i-1], S.sim[,i-1], X, y, pi0.sim[i-1], pim.sim[,i-1]);
  
  ### Sample pi0 #####
  
  pi0.sim[i]=update.pi0(S.sim[,i], pi0.sim[i-1]);
  
  
  ### Sample pim ########
  
  pim.sim[,i]=update.pim(S.sim[,i], alpha, pim.sim[,i-1]);
  
  
  ### Sample beta #######
  
  beta.sim[, i]=update.beta(theta.sim[, i], S.sim[, i]);
  
  ### Sample eta ######
  
  eta.sim[,i]=update.eta(theta.sim[, i], a_eta, b_eta);
  
  ### Sample sigma ####
  
  sigma.sim[, i]=update.sigma( beta.sim[, i], a_sigma, b_sigma, X, y);
}

#############Summary MCMC Results##################

for (i in 1:p) {
  hist(beta.sim[i,(M/2+1):M])
}

beta_est=rep(0,p);
for (i in 1:p) {
  beta_est[i]=median(beta.sim[i,(M/2+1):M])*sqrt(as.numeric(var(yt))/as.numeric(var(X[,1])))               
}

for (h in 1:L)
{
  plot(theta.sim[h+1,(M/2+1):M])
}




