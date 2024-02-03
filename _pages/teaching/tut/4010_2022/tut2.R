###-----------------------------------------###
#
#                Example 1.1
#
###-----------------------------------------###
#data
x = c(1.92, 0.59, 0.52, 2.30, 0.39)

#the main function
pdf.post = function(m = 1,x){
    n = length(x)
    alpha.n = n/2 +1
    beta.n = sum(x^2)/4+4
    p = 1-pf(alpha.n*( mean(x^2)+1 )/2/beta.n, m, 2*alpha.n)
    p
}

#application to data
pm = sapply(1:100, pdf.post, x, simplify=TRUE)
plot = 0
if(plot){
    png('./tut2.png') #save the plot
    plot(1:100, pm, type="l", ylim=c(0,1),
         ylab=bquote(italic(p[m])), xlab=bquote(italic(m)))
    dev.off()
}

###-----------------------------------------###
#
#                Example 1.2
#
###-----------------------------------------###
##Write step 1 and 2 as a function
sim_gamma <- function(alpha=1,beta=1){
    v = runif(alpha) ##generate alpha uniform(0,1) random samples
    g = -1/beta*sum(log(v))
    return(g)
}

##monte carlo simulation to estimate the population mean
n = 100000
sim = array(NA,n) ##An array to store sample
a=4
b=2
set.seed(4010)
for (i in 1:n) {
    sim[i] = sim_gamma(a,b)
}

##Sample mean vs true mean a/b = 2
mean(sim) ##2.005


###-----------------------------------------###
#
#                Example 2.3
#
###-----------------------------------------###
# uncomment the following line if the package hasn't been installed
# install.packages('invgamma')
library(invgamma)
pinvgamma(20,6,120)

#hw2
x = c(0,rep(1,14))
x = c(rep(1,3),rep(0,17))
jack.var = function(x){
    out = c()
    for(i in 1:length(x)){
        out = c(out,mean(x[-i]))
    }
    var(out)
}
m = mean(x)
v = jack.var(x)
(alpha = m^2*(1-m)/v-m)
(beta = alpha*(1/m-1))
