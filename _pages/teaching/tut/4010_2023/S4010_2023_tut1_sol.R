## true DGP, pretend you do not know it
set.seed(999)
theta = 5
x = rpois(5,theta)
x

#prior and posterior plots
n     = length(x)
alpha = 12
beta = 4


# Step 1: compute density for a grid of theta 
theta=seq(1, 8,length.out=100)
post=array(NA, dim=c(100,n+1))
##prior density
post[,1] = dgamma(theta,shape = alpha,rate = beta)
#posterior density
for(i in 1:length(x)){
  alpha_n = alpha+sum(x[1:i])
  beta_n = beta+i
  post[,i+1] = dgamma(theta,shape = alpha_n,rate = beta_n)
}

# Step 2: plot
col= colorRampPalette(c("red","blue"))(ncol(post))
windows(height=5,width = 5)
matplot(theta, post, col=col, type="l", lwd=3, lty=1, xaxt='n',
        xlab=expression(theta),ylab=bquote(f~"("~theta~"|"~x[1:n]~")"))
axis(side = 1, at = 1:8)
abline(v = 5,col = 'green')
legend("topright", paste0("n=",0:length(x)), lty=1, lwd=3, col=col)

#prior predictive and posterior predictive
# Step 1, Density computation
## for numerical stability, compute log density first and then take exp
dprior_pred <- function(y,alpha=4, beta=2){
  n = length(y)
  lnf = array(NA,n)
  for (i in 1:n) {
    lnf[i] = alpha*log(beta)-sum(log(gamma(1:(y[i]))))-log(gamma(alpha))+
      log(gamma(alpha+y[i]))-(alpha+y[i])*log(beta+1)
  }
  return(exp(lnf))
}

dpost_pred <- function(y,d,alpha=4, beta=2){ ##d is array of data
  alphaN = alpha + sum(d)
  betaN = beta+length(d)
  return(dprior_pred(y,alphaN,betaN))
}

# Step 2 plot
y=1:8
pred_den = array(NA,c(8,2))
pred_den[,1] = dprior_pred(y,alpha,beta)
pred_den[,2] = dpost_pred(y,x,alpha,beta)


col= colorRampPalette(c("red","blue"))(ncol(pred_den))
windows(height=5,width = 5)
matplot(y, pred_den, col=col, type="b", lwd=3, lty=1, xaxt = 'n',
        xlab=expression(x[n+1]),ylab=bquote(f~"("~x[n+1]~"|"~x[1:n]~")"))
axis(side = 1, at = y)
legend("topright", paste0("n=",c(0,5)), lty=1, lwd=3, col=col)

## Experiment
## standard but fail
exp(4010)/(gamma(100)*(10^1585))
# [1] NaN

## take log and then take exp
exp(4010-log(gamma(100))-1585*log(10))
# [1] 3.555239
