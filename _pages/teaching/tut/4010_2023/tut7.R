###-----------------------------###
#
#            Q1a~~~k=1
#
###-----------------------------###
### Step 1: Initialization
# data = read.table("MCQ.txt", header = T);
# X = as.matrix(data);
# a = 0.5; b=0.5;
# n = dim(X)[1];
# m = dim(X)[2];
# alpha.b = sum(X) + a;
# beta.b = n*m - sum(X) + b
alpha.b = 140.5
beta.b = 196.5

### Step 2: Find Bayes estimator
theta = seq(0, 1, length = 5001)
# notice that the posterior has a closed form
d = dbeta(theta, alpha.b, beta.b)
L = theta[which.min(abs(d[1:which.max(d)]-1))]
U = theta[which.min(abs(d[which.max(d)+1:length(d)]-1))+which.max(d)]
L
U

### Step 3: Find Credible level
pbeta(U,alpha.b,beta.b)-pbeta(L,alpha.b,beta.b)

###-----------------------------###
#
#          Q1b~~~95% HPD
#
###------------------------------###

# Method 1
{
    alpha = 0.05
    ### step 1: values of posterior at different values of theta in [a,b]
    theta = seq(from=0.3, to=0.5, length=501)
    d = dbeta(theta, alpha.b, beta.b)
    
    ### step 2: find the theta that satisfy the credibility requirement 
    # O stores indices
    O = order(d,decreasing=TRUE)
    # confidence w.r.t. all interval candidates
    
    ##---Learning Moment: use Riemann sum to approximate integral---##
    step = theta[2]-theta[1]
    conf = cumsum(d[O]*step)
    
    # check confidence condition
    N = sum(conf<1-alpha)+1
    selected.index = O[1:N]
    selected = theta[selected.index]
    # results
    k = d[O[N]]
    selected[1]
    selected[N]
    alpha.hat = conf[N]
    alpha.hat
    
    ### step 3: plot 
    plot(theta,d, type="l", lwd=2, col="red4", 
         xlab=expression(theta),
         ylab=expression(pi(theta~"|"~italic(x[1:n]))))
    abline(v=selected,col="pink")
    abline(h=c(0,k),v=c(a,b),lty=3, lwd=.75)
    
    ### exploration
    abline(v = theta[O[1:10]],col='yellow')
    abline(v = theta[O[1:50]],col='yellow')
    abline(v = theta[O[1:100]],col='yellow')
    abline(v = theta[O[1:150]],col='yellow')
    
    abline(h = d[O[10]],lty = 2,col='orange')
    abline(h = d[O[50]],lty = 2,col='orange')
    abline(h = d[O[100]],lty = 2,col='orange')
    abline(h = d[O[150]],lty = 2,col='orange')
    
    conf[1:50]
    
}
    
# Method 2
{
    ### search for all possible HPD set (with different credible level)
    i.max = which.max(d)
    out = array(NA, dim=c(i.max,3))
    colnames(out) = c("Lower-bound","Upper-bound","Credible-level")
    for(i.left in 1:i.max){
        delta = abs(d[i.left]-d[-(1:i.max)])
        out[i.left,1] = theta[i.left]
        i.right = i.max+which.min(delta)
        out[i.left,2] = theta[i.right]
        out[i.left,3] = pbeta(out[i.left,2],alpha.b,beta.b)-pbeta(out[i.left,1],alpha.b,beta.b)
    }
    
    ### select the HPD set with a desired creible level
    (result = out[which.min(abs(out[,3]-0.95)),])
    
    ### plot
    abline(v=c(result[1:2]),col="blue4",lwd=3)
    
    # exploration
    plot(theta,d, type="l", lwd=2, col="red4", 
         xlab=expression(theta),
         ylab=expression(pi(theta~"|"~italic(x[1:n]))))
    abline(h=c(0,k),v=c(a,b),lty=3, lwd=.75)
    abline(v=c(result[1:2]),col="blue4",lwd=3)
    abline(v = c(out[50,1],out[50,2]),col='blue',lty=4)
    abline(v = c(out[100,1],out[100,2]),col='blue',lty=4)
    abline(v = c(out[150,1],out[150,2]),col='blue',lty=4)
}

##---Learning Moment: use Riemann sum to approximate integral---##
riemann = function(f, left, right, step){
    theta = seq(from = left,to = right,by = step)
    f.all = f(theta)
    sum1 = sum(f.all*step)
    sum2 = mean(f.all)*(right-left)
    sum3 = pbeta(right,alpha.b,beta.b)-pbeta(left,alpha.b,beta.b)
    out = array(c(sum1,sum2,sum3),dimnames=list(c('Riemann Sum','Sample average','pbeta')))
    out
}
post = function(x) dbeta(x,alpha.b,beta.b)
l=0
r=1
riemann(f = post,left = l,right = r,step = 0.01)

###-----------------------------###
#
#          Q1c~~~95% ET
#
###------------------------------###
qbeta(c(0.025,0.975),alpha.b,beta.b)

