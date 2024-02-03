###---------example 1.2-------------###
##Data
x = c(6,7,5,4)

##Parameters
a = 12
b = 2

##Compute a_4 and b_4
a_4 = a + sum(x)
b_4 = b + 4

##Simulate log theta
set.seed(4010)
nRep = 2^20
theta = rgamma(nRep,shape = a_4, rate = b_4)
ln_theta = log(theta)

##Compute sample mean as an estimate of log theta given data
mean(ln_theta)
##the answer is about 1.72.

###-------------------###
png('tut1_rmk.png',width=600,height=300)
{
    par(mar = c(5, 5, 2, 2))
    x = rnorm(10^6,5/2,1/2)
    y = rnorm(10^6,5,1/2)
    pdf1 = hist(x,plot = F)
    pdf2 = hist(y,plot = F)
    plot(xlab=expression(theta),ylab=expression(paste('f(',theta,'|x)')),main = 'Posterior Density Conditioned on Different Data',
         xlim=c(0,8),pdf1$mids,pdf1$density,type='l',
         lwd = 2,col='purple',cex.axis=1.5,cex.lab=1.5)
    lines(pdf2$mids,pdf2$density,type='l',lwd=2,col='orange')
    abline(h=0,col='grey',lty=2)
    legend('topright',legend = c('x=5','x=10'),col=c('purple','orange'),lwd=2,cex=1.75)
}
dev.off()

###-------------------###
#sampling standard normal
set.seed(4010)
x = rnorm(10^4,0,1) #x is a sample of 10^4 IID standard normal random variables
z = qnorm(0.975,0,1) # z = 1.959964
p = pnorm(1.96,0,1) # p = 0.9750021
d = dnorm(0,0,1) # d = 0.3989423
# using Monte Carlo to find quantities related to a certain random variable
expectation = mean(x)  # expectation = -0.0100194
variance = var(x) # variance = 1.006437
quart = quantile(x,0.25) # quart = -0.6778637 
prob = mean(x<1.96) # prob = 0.975
