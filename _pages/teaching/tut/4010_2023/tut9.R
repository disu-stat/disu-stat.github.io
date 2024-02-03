###---------------------------------------------------------------###
#   Tutorial 9 - STAT 4010 Spring 2022 
###---------------------------------------------------------------###
###-----------------------Example 1.1---------------------------###
target_den <- function(theta,b=3){
    log_d = -abs(theta)/b
    exp(log_d - max(log_d))
}

target_g <- function(theta){
    theta^2
}

##RIS
RIS <- function(J,sd = 4, b= 3){ ##sd being the sd of the proposal
    tilde_theta = rnorm(J,0,sd)
    w = target_den(tilde_theta,b)/dnorm(tilde_theta,0,sd)
    sample(tilde_theta,size = J,replace=T,prob = w) ##output the theta_{1;J}
}

sim_I_RIS <- function(J,sd=4,b=3){
    theta_sim = RIS(J,sd,b)
    mean(target_g(theta_sim))
}

J_sim = 2^14
sim_I_RIS(J_sim,sd = 2)
sim_I_RIS(J_sim,sd = 3)
sim_I_RIS(J_sim,sd = 4)
sim_I_RIS(J_sim,sd = 5)
sim_I_RIS(J_sim,sd = 6)
sim_I_RIS(J_sim,sd = 7)
sim_I_RIS(J_sim,sd = 8)

x = seq(-15,15,0.01)
d = exp(-abs(x)/3)/(2*3)
plot(x,d,type='l',ylim=c(0,0.2),col='red',main = 'Target Density vs Proposal')
lines(x,dnorm(x,0,2),col='blue4')
lines(x,dnorm(x,0,3),col='blue3')
lines(x,dnorm(x,0,4),col='blue2')
lines(x,dnorm(x,0,5),col='blue1')
lines(x,dnorm(x,0,6),col='blue')
legend('topright',legend=c('Target'),col='red',lwd=1.5)



###-----------------------Example 2.1---------------------------###
##MH algorithm
##sd0 is the sd of the initial proposal
##sd1 is the sd of the proposal
pi_u <- function(theta,b=3){
    exp(-abs(theta)/b)
}

target_g <- function(theta){
    theta^2
}

MH <- function(J,burn_frac = 0.5, keep = F,sd0 = 8, sd1 = 8,b = 3){
    nsim = ceiling(J/burn_frac)+1
    theta = array(NA,nsim) 
    theta[1] = rnorm(1,0,sd0) ##sample from the initial proposal
    for (j in 2:nsim) {
        tilde_theta = rnorm(1,theta[j-1],sd1) ##sample from the proposal
        u = runif(1)
        log_target_odd = log(pi_u(tilde_theta,b)) - log(pi_u(theta[j-1],b))
        a = exp(min(0,log_target_odd))
        theta[j] = tilde_theta*(u <= a) + theta[j-1]*(u> a)
    }
    if (keep) {
        return(theta)
    }else{
        return(tail(theta,J)) ##burn the first fraction
    }
}

J_sim = 2^14
theta_MH = MH(J_sim,sd0 = 8,sd1=8)

##MCMC Diagostic
pi = density(theta_MH, kernel="epanechnikov")
acf = acf(theta_MH,plot=F)
par(mfrow = c(1,3))
plot(pi,main = 'Density plot',ylab = 'Density',xlab = expression(theta))
plot(acf,main = 'ACF plot',ylab = 'ACF',xlab = 'Lag')
plot((J_sim-500+1):J_sim,tail(theta_MH,500),type = 'l',
     main = 'Trace plot of last 500 samples',ylab = expression(theta),xlab = 'j')
##Produce estimate for I
mean(target_g(theta_MH))
hist(theta_MH,freq = F)


###---------------------Example 2.2------------------------------###
An = 2
n = 30

log_pi_u2 = function(sigma,An,n,a= 0.4,b = 2){
    (a-n-1)*log(sigma) - b*sigma - sqrt(2)*An/sigma
}

##visualize the target density
sigma_grid = seq(0.01,0.5,length.out = 101)
log_den = log_pi_u2(sigma_grid,An,n)
plot(sigma_grid,exp(log_den - max(log_den)),type = 'l',
     main = 'Density Plot',ylab ='density',xlab = expression(sigma))

MH2 <- function(J,An,n,burn_frac = 0.5, keep = F,a0 = 4,b0=40){
    nsim = ceiling(J/burn_frac)+1
    theta = array(NA,nsim) 
    theta[1] = rgamma(1,a0,b0) ##sample from the initial proposal
    for (j in 2:nsim) {
        a1 = 400*theta[j-1]^2
        b1 = 400*theta[j-1]
        tilde_theta = rgamma(1,a1,b1) ##sample from the proposal
        u = runif(1)
        log_target_odd = log_pi_u2(tilde_theta,An,n) - log_pi_u2(theta[j-1],An,n)
        a2 = 400*tilde_theta^2
        b2 = 400*tilde_theta
        log_proposal_odd = log(dgamma(tilde_theta,a1,b1)) - log(dgamma(theta[j-1],a2,b2))
        a = exp(min(0,log_target_odd-log_proposal_odd))
        theta[j] = tilde_theta*(u <= a) + theta[j-1]*(u> a)
    }
    if (keep) {
        return(theta)
    }else{
        return(tail(theta,J)) ##burn the first fraction
    }
}

J_sim = 2^14
theta_MH2 = MH2(J_sim,An,n,keep = T)

##MCMC Diagostic
pi = density(theta_MH2, kernel="epanechnikov")
acf = acf(theta_MH2,plot=F)
#windows(height = 5,width = 15)
par(mfrow = c(1,3))
plot(pi,main = 'Density plot',ylab = 'Density',xlab = expression(theta))
plot(acf,main = 'ACF plot',ylab = 'ACF',xlab = 'Lag')
plot((J_sim-500+1):J_sim,tail(theta_MH2,500),type = 'l',
     main = 'Trace plot of last 500 samples',ylab = expression(theta),xlab = 'j')
#alternative R functions to produce the same plot
par(mfrow = c(1,3))
plot(pi,main = 'Density plot',ylab = 'Density',xlab = expression(theta))
acf = acf(theta_MH2,plot=T)
ts.plot(theta_MH2[1:10],type = 'l',
     main = 'Trace plot of last 500 samples',ylab = expression(theta),xlab = 'j')

##Option pricing
get_call <- function(sigma_hat,s0 = 373,k = 380,t = 0.5,r = 0){
    d1 = (log(s0/k)+(r+sigma_hat^2/2)*t)/(sigma_hat*sqrt(t))
    d2 = d1-sigma_hat*sqrt(t)
    c = s0*pnorm(d1)-k*exp(-r*t)*pnorm(d2)
    c
}
mean(get_call(theta_MH2))
