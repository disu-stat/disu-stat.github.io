#####----------------------------------------------------------#####
#       STAT4010  tutorial 10 
#####----------------------------------------------------------#####
# Import and extract data
data = read.csv("enhanced_sur_covid_19_eng.csv")
sewt0 = as.numeric(as.Date("01/02/2020","%d/%m/%Y"))
t1 = as.numeric(as.Date("10/04/2020","%d/%m/%Y"))
x = t = rep(NA,t1-t0+1)
for (i in t0:t1){
    x[i-t0+1] = sum(as.Date(data$Report.date,"%d/%m/%Y")==i)
    t[i-t0+1] = as.character(as.Date("01/02/2020","%d/%m/%Y")+i-t0)
}
names(x) = t

# Ex6.2.2: sample from posterior
gibbs_step = function(param, x, alpha, beta) {
    n = length(x)
    tau = 2:n
    cs = cumsum(x)
    lp = -tau*param[2] -(n-tau)*param[3] +cs[1:(n-1)]*log(param[2]) +
        (sum(x)-cs[1:(n-1)])*log(param[3])
    p = exp(lp-max(lp))
    param[1] = sample(tau, 1, prob=p/sum(p))
    param[2] = rgamma(1, alpha+sum(x[1:(param[1]-1)]), beta+param[1])
    param[3] = rgamma(1, alpha+sum(x[param[1]:n]), beta+n-param[1])
    param
}
set.seed(4010)
J = 2^13
out = matrix(nrow=J+1, ncol=3)
colnames(out) = c("tau","theta1","theta2")
alpha = 1
beta = 1
out[1,] = c(sample(2:length(x),1), rgamma(2, alpha, beta))
for (j in 1:J) {
    out[j+1,] = gibbs_step(out[j,], x, alpha, beta)
}
out = out[-1,] #remove initialization
iUse = (J/2+1):J

# Ex6.2.3: trace plot
transCol = function(color, percent=50) {
    v = col2rgb(color)
    newCol = rgb(v[1],v[2],v[3],max=255,alpha=(1-percent/100)*255)
    invisible(newCol)
}
par(mfrow=c(1,2), mar=c(4.5,5,3,2))
col = c(transCol("red", percent=50),
        transCol("blue", percent=50),
        transCol("green", percent=50))
matplot(1:200, out[1:200,], col=col, lwd=2, type="l", lty=1, 
        ylab="", xlab="Time", main="First 200 samples")
legend("topright", c("tau","theta1","theta2"), col=col, pch=15, cex=1.2)
matplot(iUse, out[iUse,], col=col, lwd=2, type="l", lty=1, 
        ylab="", xlab="Time", main="Used sample")
acf(out, mar=c(3,2.5,2,0.5))
acf(out[iUse,], mar=c(3,2.5,2,0.5))


#####----------------------------------------------------------#####
#
#####----------------------------------------------------------#####
log_pi_lambda1 <- function(Pn,Nn,n1,n0,lambda1,lambda0,p,h,k){
    n1*log(p)+n0*log(1-p)-(h+n1+1)*log(lambda1)-n0*log(lambda0)-(Pn+k)/lambda1+Nn/lambda0
}

log_pi_lambda0 <- function(Pn,Nn,n1,n0,lambda1,lambda0,p,h,k){
    n1*log(p)+n0*log(1-p)-n1*log(lambda1)-(h+n0+1)*log(lambda0)-Pn/lambda1-(k-Nn)/lambda0
}

log_pi_p <- function(Pn,Nn,n1,n0,lambda1,lambda0,p,a,b){
    (n1+a-1)*log(p)+(n0+b-1)*log(1-p)-n1*log(lambda1)-n0*log(lambda0)-Pn/lambda1+Nn/lambda0
}

get_invgamma_para <- function(mu,sd){
    h = mu^2/sd^2+2
    k = mu*(h-1)
    c(h,k)
}

get_beta_para <- function(mu,sd){
    a = mu^2*(1-mu)/sd^2-mu
    b = a*(1/mu-1)
    c(a,b)
}

##MH one step
MH_lambda1 <- function(Pn,Nn,n1,n0,lambda1,lambda0,p,sd_lambda1 = 0.01){
    ##represent the parameters
    #the argument lambda1 is the old value of the rv lambda1
    para.p = get_invgamma_para(lambda1,sd_lambda1)
    #parameters for proposal
    h = para.p[1]
    k = para.p[2]
    lambda1_p = rinvgamma(1,h,k)
    #denisty conditioned on theta_{-k}
    log_target_odd = log_pi_lambda1(Pn,Nn,n1,n0,lambda1 = lambda1_p,lambda0,p,h,k)-log_pi_lambda1(Pn,Nn,n1,n0,lambda1 = lambda1,lambda0,p,h,k)
    #density conditioned on theta_k
    log_proposal_odd = log(dinvgamma(lambda1_p,h,k)) - log(dinvgamma(lambda1,h,k))  
    
    accept_prob = exp(min(0,log_target_odd-log_proposal_odd))
    u = runif(1)
    lambda1_p*(u<=accept_prob)+lambda1*(u>accept_prob)
}

MH_lambda0 <- function(Pn,Nn,n1,n0,lambda1,lambda0,p,sd_lambda0 = 0.01){
    para.p = get_invgamma_para(lambda0,sd_lambda0)
    h = para.p[1]
    k = para.p[2]
    lambda0_p = rinvgamma(1,h,k)
    log_target_odd = log_pi_lambda0(Pn,Nn,n1,n0,lambda1,lambda0 = lambda0_p,p,h,k)-log_pi_lambda0(Pn,Nn,n1,n0,lambda1,lambda0,p,h,k)
    log_proposal_odd = log(dinvgamma(lambda0_p,h,k)) - log(dinvgamma(lambda0,h,k))  
    accept_prob = exp(min(0,log_target_odd-log_proposal_odd))
    u = runif(1)
    lambda0_p*(u<=accept_prob)+lambda0*(u>accept_prob)
}

MH_p <- function(Pn,Nn,n1,n0,lambda1,lambda0,p,sd_p = 0.1){
    para.p = get_beta_para(p,sd_p)
    a = para.p[1]
    b = para.p[2]
    p_p = rbeta(1,a,b)
    log_target_odd = log_pi_p(Pn,Nn,n1,n0,lambda1,lambda0,p = p_p,a,b)-log_pi_p(Pn,Nn,n1,n0,lambda1,lambda0,p,a,b)
    log_proposal_odd = log(dbeta(p_p,a,b)) - log(dbeta(p,a,b))
    accept_prob = exp(min(0,log_target_odd-log_proposal_odd))
    u = runif(1)
    p_p*(u<=accept_prob)+p*(u>accept_prob)
}

#Gibbs sampler
gibbs <- function(J,Pn,Nn,n1,n0,burn_frac = 0.5, keep = F, ##general para for Gibbs
                  sd_lambda1 = 0.01,sd_lambda0 = 0.01,sd_p = 0.1, ##para for step size
                  mu_lambda_ini = 0.05,sd_lambda_ini = 0.01, ##para for initial proposal
                  mu_p_ini = 0.5,sd_p_ini = 0.1){
  nsim = ceiling(J/burn_frac)+1
  theta = array(NA,c(nsim,3))
  colnames(theta) = c('lambda1','lambda0','p')
  ##sample from the initial proposal
  para = get_invgamma_para(mu_lambda_ini,sd_lambda_ini)
  h = para[1]
  k = para[2]
  para = get_beta_para(mu_p_ini,sd_p_ini)
  a = para[1]
  b = para[2]
  theta[1,] = c(rinvgamma(2,h,k),rbeta(1,a,b))
  for (j in 2:nsim) {
    theta_use = theta[j-1,]
    ##update lambda1
    theta_use[1] = MH_lambda1(Pn,Nn,n1,n0,
                              theta_use[1],theta_use[2],theta_use[3],sd_lambda1)
    ##update lambda0
    theta_use[2] = MH_lambda0(Pn,Nn,n1,n0,
                              theta_use[1],theta_use[2],theta_use[3],sd_lambda0)
    ##update p
    theta_use[3] = MH_p(Pn,Nn,n1,n0,
                              theta_use[1],theta_use[2],theta_use[3],sd_p)
    theta[j,] = theta_use
  }
  if (keep) {
    return(theta)
  }else{
    return(tail(theta,J)) ##burn the first fraction
  }
}

##sample parameters from Gibbs
set.seed(410)
J_sim = 2^12
n1 = 55
n0 = 45
Pn = 2.2
Nn = -2.7
theta_sim = gibbs(J_sim,Pn,Nn,n1,n0,keep=F)
theta_name = c('lambda1','lambda0','p')
colnames(theta_sim) = theta_name

##MCMC Diagnostic
##1) Density plot
#windows(height=15,width = 15)
par(mfrow = c(3,3))
for (i in 1:3) {
  plot(density(theta_sim[,i], kernel="epanechnikov"),main = paste('Density plot for',theta_name[i]),
       ylab = 'Density',xlab = expression(theta))
}
##2) ACF plot
for (i in 1:3) {
  acf = acf(theta_sim[,i],plot=F)
  plot(acf,main = paste('ACF of',theta_name[i]),xlab = 'Lag')
}

##3) Trace plot
for (i in 1:3) {
  plot((J_sim-500+1):J_sim,tail(theta_sim[,i],500),type = 'l',
       main = paste('Trace plot of',theta_name[i]),ylab = expression(theta),xlab = 'j')
}

##generate posterior predictive samples
set.seed(4010)
u = rbinom(J_sim,1,theta_sim[,'p'])
r = rexp(J_sim,1/theta_sim[,'lambda1'])*u-rexp(J_sim,1/theta_sim[,'lambda0'])*(1-u)
##Compute option payoff
S0 = 373
K = 380
c = S0*exp(r)-K
c = c*(c>0)
mean(c)
