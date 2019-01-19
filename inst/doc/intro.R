## ------------------------------------------------------------------------
mat<- matrix(1:4,2,2)
mat
layout (mat)
layout.show(4)

## ------------------------------------------------------------------------
layout (matrix(1:6,3,2))
layout.show(6)

## ------------------------------------------------------------------------
m<- matrix(c(1:3,3),2,2)
layout (m)
layout.show(3)

## ------------------------------------------------------------------------
m<- matrix(c(1,1,2,1),2,2)
layout (m,widths = c(2,1),heights = c(1,2))
layout.show(2)

## ------------------------------------------------------------------------
x<-rnorm(10)
y<-rnorm(10)
plot(x,y)

## ------------------------------------------------------------------------
plot(x,y,xlab = "Ten random values",ylab = "Ten other values", xlim = c(-2,2), ylim = c(-2,2), pch=22, col="red", bg="yellow", bty="7",tcl=0.4, main="How to customize a plot with R", las=1,cex=1.5)

## ------------------------------------------------------------------------
opar<-par()
par(bg="lightyellow", col.axis="blue", mar=c(4,4,2.5,0.25))
plot(x,y,xlab = "Ten random values",ylab = "Ten other values", xlim = c(-2,2), ylim = c(-2,2), pch=22, col="red", bg="yellow",bty="7", tcl=-0.25, las=1, cex=1.5)
title("How to customize a plot with R(bias)",font.main=3, adj=1)
par(opar)

## ------------------------------------------------------------------------
x<-rnorm(10,-5,0.1)
y<-rnorm(10,5,2)
X<-cbind(x,y)
apply(X, 2,mean)
apply(X, 2, sd)

## ------------------------------------------------------------------------
forms<-list(y~x,y~poly(x,2))
lapply(forms,lm)

## ------------------------------------------------------------------------
mat<- matrix(1:4,2,2)
mat
layout (mat)
layout.show(4)

## ------------------------------------------------------------------------
layout (matrix(1:6,3,2))
layout.show(6)

## ------------------------------------------------------------------------
m<- matrix(c(1:3,3),2,2)
layout (m)
layout.show(3)

## ------------------------------------------------------------------------
m<- matrix(c(1,1,2,1),2,2)
layout (m,widths = c(2,1),heights = c(1,2))
layout.show(2)

## ------------------------------------------------------------------------
x<-rnorm(10)
y<-rnorm(10)
plot(x,y)

## ------------------------------------------------------------------------
plot(x,y,xlab = "Ten random values",ylab = "Ten other values", xlim = c(-2,2), ylim = c(-2,2), pch=22, col="red", bg="yellow", bty="7",tcl=0.4, main="How to customize a plot with R", las=1,cex=1.5)

## ------------------------------------------------------------------------
opar<-par()
par(bg="lightyellow", col.axis="blue", mar=c(4,4,2.5,0.25))
plot(x,y,xlab = "Ten random values",ylab = "Ten other values", xlim = c(-2,2), ylim = c(-2,2), pch=22, col="red", bg="yellow",bty="7", tcl=-0.25, las=1, cex=1.5)
title("How to customize a plot with R(bias)",font.main=3, adj=1)
par(opar)

## ------------------------------------------------------------------------
x<-rnorm(10,-5,0.1)
y<-rnorm(10,5,2)
X<-cbind(x,y)
apply(X, 2,mean)
apply(X, 2, sd)

## ------------------------------------------------------------------------
forms<-list(y~x,y~poly(x,2))
lapply(forms,lm)

## ------------------------------------------------------------------------
x <- seq(.1,0.9, length = 9) 
m <- 10000 
u <- runif(m) 
cdf <- numeric(length(x)) 
for (i in 1:length(x)) { 
  g <- x[i] ^3*u^2* (1-x[i]^2*u^2) 
  cdf[i] <- 30*mean(g) 
}

## ------------------------------------------------------------------------
Phi <- pbeta(x,3,3) 
print(round(rbind(x, cdf, Phi), 3)) 

## ------------------------------------------------------------------------
MC.Phi <- function(x, R = 10000, antithetic = TRUE) {
  u <- runif(R/2) 
  sigma <- 4
  if (!antithetic) v <- runif(R/2) else 
    v <- 1 - u 
  u <- c(u, v) 
  cdf <- numeric(length(x)) 
  for (i in 1:length(x)) { 
    g <- x[i]^2*u/sigma^2 * exp(-(u* x[i])^2 / (2*sigma^2) )
    cdf[i] <- mean(g)  
  } 
  cdf 
}


## ------------------------------------------------------------------------
x <- seq(1,10, length=10) 
sigma <- 4
Phi <- x/sigma^2*exp(-x^2/(2*sigma^2))
set.seed(123)
MC1 <- MC.Phi(x, anti = FALSE) 
set.seed(123)
MC2 <- MC.Phi(x) 
print(round(rbind(x, MC1, MC2, Phi), 5))

## ------------------------------------------------------------------------
m <- 1000 
MC1 <- MC2 <- numeric(m) 
x <- 2
for (i in 1:m) { 
  MC1[i] <- MC.Phi(x, R = 1000, anti = FALSE) 
  MC2[i] <- MC.Phi(x, R = 1000)
  }
print(sd(MC1)) 
print(sd(MC2)) 
print((var(MC1) - var(MC2))/var(MC1)) 

## ------------------------------------------------------------------------
m <- 10000 
theta.hat <- se <- numeric(2) 
g <- function(x) { 
 x^2/sqrt(2*pi)*exp(-x^2/2) * (x >1) 
 }

x <- rnorm(m) #using f1
fg <- sqrt(2*pi)*g(x)/exp(-x^2/2)
theta.hat[1] <- mean(fg) 
se[1] <- sd(fg)

x <- rchisq(m,4) #using f2
fg <- 4*g(x)/(x^2*exp(-x/2))
theta.hat[1] <- mean(fg) 
se[2] <- sd(fg)

rbind(theta.hat, se) 

## ------------------------------------------------------------------------
x <- seq(0, 1, .01)
w <- 2 
f1 <- exp(-x^2/2)/sqrt(2*pi)
f2 <- x^2*exp(-x/2)/4
g <- x^2/sqrt(2*pi)*exp(-x^2/2)

#figure (a) 
plot(x, g, type = "l", main = "", ylab = "", ylim = c(0,2), lwd = w) 
lines(x, f1, lty = 2, lwd = w) 
lines(x, f2, lty = 3, lwd = w) 
legend("topright", legend = c("g", 0:2), lty = 1:3, lwd = w, inset = 0.02)

#figure (b) 
plot(x, g, type = "l", main = "", ylab = "", ylim = c(0,3.2), lwd = w, lty = 2) 
lines(x, g/f1, lty = 3, lwd = w) 
lines(x, g/f2, lty = 4, lwd = w)
legend("topright", legend = c(0:2), lty = 2:4, lwd = w, inset = 0.02)

## ------------------------------------------------------------------------
m <- 10000 
theta.hat <- se <- numeric(1) 
g <- function(x) { 
  x^2/sqrt(2*pi)*exp(-x^2/2) * (x >1) 
 }
x <- rnorm(m) 
fg <- sqrt(2*pi)*g(x)/exp(-x^2/2)
theta.hat <- mean(fg) 
theta.hat


## ------------------------------------------------------------------------
n <- 20  ## numbers of samples
m <- 1000  ## experiment times
G <- numeric(m) 
for (i in 1:m) {
  x <- sort(rlnorm(n)) ## X is standard lognormal
  mu <- mean(x)  ## let the mu be the mean values of the samples
  for (j in 1:n) {
    t <- (2*j-n-1)*x[j]
  }
  G[i] <- sum(t)/(mu*n^2)
}
Ghat <- mean(G) ## compute the estimate values(mean) of G
MGhat <- median(G) ## compute the estimate values(median) of G
DGhat <- quantile(G,probs=0.1) ## compute the estimate values(deciles) of G
c(Ghat,MGhat,DGhat)
hist(G,prob=TRUE) ## density histogram of sample

## ------------------------------------------------------------------------
n <- 20  ## numbers of samples
m <- 1000  ## experiment times
G <- numeric(m) 
for (i in 1:m) {
  x <- sort(runif(n)) ## X has the uniform distribution
  mu <- mean(x)  ## let the mu be the mean values of the samples
  for (j in 1:n) {
    t <- (2*j-n-1)*x[j]
  }
  G[i] <- sum(t)/(mu*n^2)
}
Ghat <- mean(G) ## compute the estimate values(mean) of G
MGhat <- median(G) ## compute the estimate values(median) of G
DGhat <- quantile(G,probs=0.1) ## compute the estimate values(deciles) of G
c(Ghat,MGhat,DGhat)
hist(G,prob=TRUE) ## density histogram of sample

## ------------------------------------------------------------------------
n <- 20  ## numbers of samples
m <- 1000  ## experiment times
G <- numeric(m) 
for (i in 1:m) {
  x <- sort(rbinom(n,size=1,prob=0.1)) ## X has the Bernoulli(0.1) distribution
  mu <- mean(x)  ## let the mu be the mean values of the samples
  for (j in 1:n) {
    t <- (2*j-n-1)*x[j]
  }
  G[i] <- sum(t)/(mu*n^2)
}
Ghat <- mean(G) ## compute the estimate values(mean) of G
MGhat <- median(G) ## compute the estimate values(median) of G
DGhat <- quantile(G,probs = seq(0,1,0.1),na.rm = TRUE) ## compute the estimate values(deciles) of G
c(Ghat,MGhat,DGhat)
hist(G,prob=TRUE) ## density histogram of sample

## ------------------------------------------------------------------------
n <- 20  ## numbers of samples
m <- 1000  ## experiment times
G <- numeric(m) 
for (i in 1:m) {
  x <- sort(rlnorm(n)) ## X is standard lognormal
  mu <- mean(x)  ## let the mu be the mean values of the samples
  for (j in 1:n) {
    t[j] <- (2*j-n-1)*x[j]
    sum(t)
  }
  G[i] <- sum(t)/(mu*n^2)
  G[i]
}
mean(G) ## compute the estimate values(mean) of G
alpha <- 0.05 
sd <- 1
EG <- 2*pnorm(sd/sqrt(2))-1 ## compute the integration of G to get the real mean values
  UCL <- mean(G)+sd(G)*qt(alpha/2, df = n-1)/sqrt(n) ## compute the upper confidence limit
  LCL <- mean(G)-sd(G)*qt(alpha/2, df = n-1)/sqrt(n) ## compute the lower confidence limit
c(UCL,LCL)


## ------------------------------------------------------------------------
n <- 20  ## numbers of samples
m <- 1000  ## experiment times
G <- numeric(m) 
UCL=numeric(1000)
LCL=numeric(1000)
for(ii in 1:1000) {
for (i in 1:m) {
  x <- sort(rlnorm(n)) ## X is standard lognormal
  mu <- mean(x)  ## let the mu be the mean values of the samples
  for (j in 1:n) {
    t[j] <- (2*j-n-1)*x[j]
    sum(t)
  }
  G[i] <- sum(t)/(mu*n^2)
  G[i]
}
mean(G) ## compute the estimate values(mean) of G
alpha <- 0.05 
sd <- 1
EG <- 2*pnorm(sd/sqrt(2))-1 ## compute the integration of G to get the real mean values
  UCL[ii] <- mean(G)+sd(G)*qt(alpha/2, df = n-1)/sqrt(n) ## compute the upper confidence limit
  LCL[ii] <- mean(G)-sd(G)*qt(1-alpha/2, df = n-1)/sqrt(n) ## compute the lower confidence limit
}
sum(EG<UCL & EG>LCL)  #count the number of intervals that satisfy the conditions
mean(UCL>EG & EG>LCL) ## compute the coverage rate

## ------------------------------------------------------------------------
library(bootstrap) #for the law data
data(law, package = "bootstrap") 
n <- nrow(law) # for all data available in the samples popuation
theta.hat <- cor(law$LSAT,law$GPA) #compute the correlation between law$LSAT and law$GPA
print(sprintf('theta.hat=%f',theta.hat))
#compute the jackknife replicates, leave-one-out estimates 
theta.jack <- numeric(n) 
for (i in 1:n) 
  theta.jack[i] <- cor(law$LSAT[-i],law$GPA[-i])
bias <- (n - 1) * (mean(theta.jack) - theta.hat)
print(sprintf('bias=%f',bias)) #jackknife estimate of bias 
se <- sqrt((n-1) * mean((theta.jack - mean(theta.jack))^2)) 
print(sprintf('se=%f',se)) #jackknife estimate of standard error

## ------------------------------------------------------------------------
library(boot) #for boot and boot.ci 
data(aircondit, package = "boot") #get the data set and package
theta.boot <- function(x,ind) { 
  #function to compute the target values 
  x <- x[ind]
  mean(x) #acording to the exponetial distribution,the $\frac1\lambda$ is exactly the values of the mean x
}
#Run the bootstrap and compute con???dence interval estimates for the bioequivalence ratio
x <- aircondit$hours #give x the values in the data set aircondit to denote the time intervals
dat <- x
boot.obj <- boot(dat, statistic = theta.boot, R = 2000)
#The output for the bootstrap and bootstrap con???dence intervals is below.
print(boot.obj) 
print(boot.ci(boot.obj, type = c("basic", "norm", "perc","bca")))

## ------------------------------------------------------------------------
library(bootstrap) #for the sample data
data(scor) # duplicate the data scor to "data"
str(scor) # the structure of the sample data
n <- nrow(scor) # for all data available in the samples popuation
cov <- cov(scor) # compute the covariance matrix
cov
e <- eigen(cov) #compute the eigenvalues of the covariance matrix
e
x <- e$values[1] # The maximum eigenvaues of the covariance matrix
print(sprintf('x=%f',x))
y <- sum(e$values[1:5]) # The sum values of all eigenvaues of the covariance matrix
print(sprintf('y=%f',y))
theta.hat <- x/y #The contribution rate of the covariance matrix
print(sprintf('theta.hat=%f',x/y))
#compute the jackknife replicates, leave-one-out estimates 
theta.jack <- numeric(n)
for (i in 1:n) 
  cov.jack <- cov(scor[-i])  # compute the covariance matrix in the jackknife
  e <- eigen(cov.jack) #compute the eigenvalues of the covariance matrix in the jackknife
  x <- e$values[1] # The maximum eigenvaues of the jackknife covariance matrix
  y <- sum(e$values[1:5]) # The sum values of all eigenvaues of the jackknife covariance matrix
  theta.jack[i] <- x/y #The contribution rate of the jackknife covariance matrix
bias <- (n - 1) * (mean(theta.jack) - theta.hat)
print(sprintf('bias=%f',bias)) #jackknife estimate of bias 
se <- sqrt((n-1) * mean((theta.jack - mean(theta.jack))^2))
print(sprintf('se=%f',se)) #jackknife estimate of standard error


## ------------------------------------------------------------------------
library(DAAG)
data(ironslag, package = "DAAG")
attach(ironslag)
n <- length(magnetic) #in DAAG ironslag 
e1<- e11<- e12<- e2<- e21<- e22<- e3<- e31<- e32<- e4<- e41<- e42 <- numeric(n)
# for n-fold cross validation 
# fit models on leave-two-out samples 
for (k in 1:n) { 
  y <- magnetic[{-k}&{-(n-k+1)}] ## leave-two-out samples
  x <- chemical[{-k}&{-(n-k+1)}] ## leave-two-out samples
  
  J1 <- lm(y ~ x) #the Linear model
  yhat11 <- J1$coef[1] + J1$coef[2] * chemical[k] 
  e11[k] <- magnetic[k] - yhat11
  yhat12 <- J1$coef[1] + J1$coef[2] * chemical[n-k+1] 
  e12[k] <- magnetic[n-k+1] - yhat12
  e1[k] <- e11[k]+e12[k]
  
  J2 <- lm(y ~ x + I(x^2)) #the Quadratic model
  yhat21 <- J2$coef[1] + J2$coef[2] * chemical[k] + J2$coef[3] * chemical[k]^2 
  e21[k] <- magnetic[k] - yhat21
  yhat22 <- J2$coef[1] + J2$coef[2] * chemical[n-k+1] + J2$coef[3] * chemical[n-k+1]^2 
  e22[k] <- magnetic[n-k+1] - yhat22
  e2[k] <- e21[k]+e22[k]
  
  J3 <- lm(log(y) ~ x) #the Exponential model
  logyhat31 <- J3$coef[1] + J3$coef[2] * chemical[k] 
  yhat31 <- exp(logyhat31) 
  e31[k] <- magnetic[k] - yhat31
  logyhat32 <- J3$coef[1] + J3$coef[2] * chemical[n-k+1] 
  yhat32 <- exp(logyhat32) 
  e32[k] <- magnetic[n-k+1] - yhat32
  e3[k] <- e31[k]+e32[k]
  
  J4 <- lm(log(y) ~ log(x)) #the Log-Log model
  logyhat41 <- J4$coef[1] + J4$coef[2] * log(chemical[k]) 
  yhat41 <- exp(logyhat41) 
  e41[k] <- magnetic[k] - yhat41
  logyhat42 <- J4$coef[1] + J4$coef[2] * log(chemical[n-k+1]) 
  yhat42 <- exp(logyhat42) 
  e42[k] <- magnetic[n-k+1] - yhat42
  e4[k] <- e41[k]+e42[k]
} 
 c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2)) 

## ------------------------------------------------------------------------

attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)

R <- 999#number of replicates
z <- c(x,y)#pooled sample
K <- 1:length(z)
D <- numeric(R) #storage for replicates

CM <- function(x,y){#a function to compute the statistic of Cramer-von Mises
  ecdfx <- ecdf(x)
  ecdfy <- ecdf(y)
  n1 <- length(x)
  n2 <- length(y)
  
  sum1 <- sum((ecdfx(x)-ecdfy(x))^2)
  sum2 <- sum((ecdfx(y)-ecdfy(y))^2)
  w <- n1*n2/(n1+n2)^2*(sum1+sum2)
  return(w)
}

D0 <- CM(x,y)

for(i in 1:R) {
  k <- sample(K, size = length(x), replace = F)
  x1 <- z[k]
  y1 <- z[-k] #complements of X1
  D[i] <- CM(x1,y1)
}

p <- mean(c(D0, D) >= D0)
print(p)

hist(D, main = "", freq = FALSE, xlab = "D (p = 0.412)", breaks = "scott")
points(D0, 0, cex = 1, pch = 16)


## ------------------------------------------------------------------------
library(RANN)## for nn function
library(boot)
library(energy)
library(Ball)
library(ggplot2)

m <- 30#times to loop
k<-3
p<-2#ncol
# mu <- 0.5
n1 <- n2 <- 50#nrow
R<-999#the number of replications in the bd.test function
n <- n1+n2
N = c(n1,n2)

Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0)
  z <- z[ix, ]
  NN <- nn2(data=z, k=k+1)
  block1 <- NN$nn.idx[1:n1,-1] 
  block2 <- NN$nn.idx[(n1+1):n,-1] 
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5) 
  (i1 + i2) / (k * n)
}

eqdist.nn <- function(z,sizes,k){#NN
  boot.obj <- boot(data=z,statistic=Tn,R=R,
  sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}

p.values <- matrix(NA,m,3)#to store p values

for(i in 1:m) {
  x <- matrix(rnorm(n1 * p, sd = 1), ncol = p)#unequal variances
  y <- matrix(rnorm(n2 * p, sd = 1.4), ncol = p)
  z <- rbind(x, y)
  p.values[i, 1] <- eqdist.nn(z, N, k)$p.value#NN
  p.values[i, 2] <- eqdist.etest(z, sizes = N, R = R)$p.value#in the energy package
  p.values[i, 3] <- bd.test(x = x, y = y, R = 999, seed = i)$p.value#"Ball Divergence" in the ball package
}

alpha <- 0.1#confidence level
pow <- apply(p.values<alpha,2,mean)#compute the number of p.values which is less than 0.1 in each column
print(pow)
power <- data.frame(methods = c('NN','energy','Ball'),pow)
ggplot(power,aes(methods,pow))+#plot
  geom_col(fill = 'orange')+
  coord_flip()


## ------------------------------------------------------------------------

for(i in 1:m) {
  x <- matrix(rnorm(n1 * p, mean = 0.4, sd = 1), ncol = p)#unequal variances and unequal expectations
  y <- matrix(rnorm(n2 * p, mean = 0, sd = 1.4), ncol = p)
  z <- rbind(x, y)
  p.values[i, 1] <- eqdist.nn(z, N, k)$p.value
  p.values[i, 2] <- eqdist.etest(z, sizes = N, R = R)$p.value
  p.values[i, 3] <- bd.test(x = x,  y = y,  R = 999,  seed = i)$p.value
}
alpha <- 0.1
pow <- apply(p.values<alpha,2,mean)
print(pow)
power <- data.frame(methods = c('NN','energy','Ball'),pow)
ggplot(power,aes(methods,pow))+
  geom_col(fill = 'lightslategrey')+
  coord_flip()


## ------------------------------------------------------------------------

for(i in 1:m) {
  x <- matrix(rt(n1 * p,df = 1), ncol = p)# t distribution
  y <- matrix(rnorm(n2 * p,sd = sample(c(1,1.3),size = n2*p, prob = c(0.5,0.5),replace = T)), ncol = p)#bimodel distribution
  z <- rbind(x, y)
  p.values[i, 1] <- eqdist.nn(z, N, k)$p.value
  p.values[i, 2] <- eqdist.etest(z, sizes = N, R = R)$p.value
  p.values[i, 3] <- bd.test(x = x, y = y, R = 999, seed = i)$p.value
}
alpha <- 0.01
pow <- apply(p.values<alpha,2,mean)
print(pow)
power <- data.frame(methods = c('NN','energy','Ball'),pow)
ggplot(power,aes(methods,pow))+
  geom_col(fill = 'mediumpurple')+
  coord_flip()


## ------------------------------------------------------------------------

n1 <- 50
n2 <- 5
n <- n1+n2
N = c(n1,n2)
for(i in 1:m) {
  x <- matrix(rnorm(n1*p,mean = 1), ncol = p)#100 samples
  y <- matrix(rnorm(n2*p,mean = 2), ncol = 2)#10 samples
  z <- rbind(x, y)
  p.values[i, 1] <- eqdist.nn(z, N, k)$p.value
  p.values[i, 2] <- eqdist.etest(z, sizes = N, R = R)$p.value
  p.values[i, 3] <- bd.test(x = x, y = y, R = 999, seed = i)$p.value
}
alpha <- 0.1
pow <- apply(p.values<alpha,2,mean)
print(pow)
power <- data.frame(methods = c('NN','energy','Ball'),pow)
ggplot(power,aes(methods,pow))+
  geom_col(fill = 'sienna2')+
  coord_flip()


## ------------------------------------------------------------------------
N <- 5000
my_vector <- numeric(N)
my_vector[1] <- rnorm(1)
counter <- 0
#generates random deviates
u <- runif(N)

for (i in 2:N) 
{
    xt <- my_vector[i-1]
    y <- rnorm(1, mean = xt)
    #Density Cauchy distribution
    numerator <- dcauchy(y)*dnorm(xt, mean = y)
    denominator <- dcauchy(xt)*dnorm(y, mean = xt)
    
    if(u[i] <= numerator/denominator) 
    {
        my_vector[i] <- y
    }
    else 
    {
        my_vector[i] <- xt
        counter <- counter+1
    }
}

plot(my_vector,type = "l")

After1kDiscard <- my_vector[1000:N]


## ------------------------------------------------------------------------
#Sequence Generation
a <- ppoints(100)
sequence <- seq(0,1,0.01)
standardCauchy <- qcauchy(sequence)
standardCauchy <- standardCauchy[(standardCauchy> -Inf) & (standardCauchy< Inf)]
Q <- quantile(my_vector, a)
qqplot(standardCauchy, Q, main="",xlab="standardCauchy Quantiles", ylab="Sample Quantiles")
hist(After1kDiscard, freq = FALSE,main = "after discard the first 1000 of the chain")
lines(standardCauchy, dcauchy(standardCauchy), lty = 2)

## ------------------------------------------------------------------------
p1=125/197
p2=18/197
p3=20/197
p4=34/197

my_prob <- c(p1,p2,p3,p4)
number_of_experiments <- 10
number_of_samples <- 197

experiments <- rmultinom(n=number_of_experiments, size=number_of_samples, prob=my_prob)
experiments




## ------------------------------------------------------------------------
# defube expectation function 

estep <- function(theta,z2){
z2 = 125*(0.25*theta/(0.5+0.25*theta))
# z1 = 125*(0.5/(0.5+0.25*theta))
return(z2)
}

mstep <- function(theta,z2){
theta <- (z2+34)/(z2+34+18+20)
return(theta)
}

# set initial value for iteration

cur_theta = 0.5
maxit = 100
maxit=1000
tol=1e-6

# start iteration

for(i in 1:maxit){
  new_z2 <- estep(cur_theta,cur_z2)
  new_theta <- mstep(cur_theta,new_z2)
  
  # Stop iteration if the difference between the current and new estimates is less than a tolerance level
  if( all(abs(cur_theta - new_theta) < tol) ){ flag <- 1; break}
  
  
  # Otherwise continue iteration
  cur_theta <- new_theta
  cur_z2 <- new_z2
}
if(!flag) warning("Didn??t converge\n")

final_theta = cur_theta
paste0("Final Theta = ", format(round(cur_theta, 4), nsmall = 4))
paste0("Final Z2 = ", format(round(cur_z2, 4), nsmall = 4))

## ------------------------------------------------------------------------
p1=125/197
p2=18/197
p3=20/197
p4=34/197

my_prob <- c(p1,p2,p3,p4)


number_of_experiments <- 1
number_of_samples <- 1000

experiments <- rmultinom(n=number_of_experiments, size=number_of_samples, prob=my_prob)
experiments




## ------------------------------------------------------------------------
p1=0.5
p2=0.25*final_theta
p3=0.25-p2
p4=p3
p5=p2

my_prob <- c(p1,p2,p3,p4,p5)
number_of_experiments <- 10
number_of_samples <- 1000

experiments <- rmultinom(n=number_of_experiments, size=number_of_samples, prob=my_prob)
experiments



number_of_experiments <- 1
number_of_samples <- 1000

experiments <- rmultinom(n=number_of_experiments, size=number_of_samples, prob=my_prob)
experiments



## ------------------------------------------------------------------------
set.seed(1)
Gelman.Rubin <- function(psi) {
  # psi[i,j] is the statistic psi(X[i,1:j])
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)

  psi.means <- rowMeans(psi) #row means
  B <- n * var(psi.means) #between variance est.
  psi.w <- apply(psi, 1, "var") #within variances
  W <- mean(psi.w) #within est.
  v.hat <- W*(n-1)/n + (B/n) #upper variance est.
  r.hat <- v.hat / W #G-R statistic
  return(r.hat)
}

theta <- .2 #parameter of proposal distribution
k <- 4 #number of chains to generate
N <- 5000 #length of chains
b <- 500 #burn-in length
M <- 5000
groupsize <- c(125,18,20,34) #group size

prob <- function(theta,group){
  if(theta<0||theta>=0.9)
    return(0)
  return((1/2+theta/4)^groupsize[1]*((1-theta)/4)^groupsize[2]*((1-theta)/4)^groupsize[3]*(theta/4)^groupsize[4])
}


mn.chain <- function(groupsize,N,X1) {
  #generates a Metropolis chain 
  #with Multinomial proposal distribution
  #and starting value X1
  x <- numeric(N)
  x[1] <- X1#initialize x[1]
  w <- 0.25
  u <- runif(N)
  v <- runif(M, -w, w)

  for (i in 2:N) {
    theta <- x[i - 1] + v[i]
    if (u[i] <= prob(theta, groupsize) / prob(x[i - 1], groupsize))
    x[i] <- theta
    else
    x[i] <- x[i - 1]
  }
  return(x)
}

#choose overdispersed initial values
x0 <- c(0.2, 0.4, 0.6, 0.8)

#generate the chains
X <- matrix(0, nrow=k, ncol=N)
for (i in 1:k)
  X[i, ] <- mn.chain(groupsize,N,x0[i])

#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))
print(Gelman.Rubin(psi))



## ------------------------------------------------------------------------
set.seed(1)
findIntersection = function (k) {
  s.k.minus.one = function (a) {
    1-pt(sqrt(a^2 * (k - 1) / (k - a^2)), df = k-1)
  }
  
  s.k = function (a) {
    1-pt(sqrt(a^2 * k / (k + 1 - a^2)), df = k)
  }
  
  f = function (a) {
    s.k(a) - s.k.minus.one(a)
  }
  
  eps = 1e-2
  return(uniroot(f, interval = c(eps, sqrt(k)-eps))$root)
}

rs = sapply(c(4:25, 100, 500, 1000), function (k) {
  findIntersection(k)
  })
cbind(k=c(4:25, 100, 500, 1000),a=rs)


## ------------------------------------------------------------------------
my.dcauchy = function (x, eta, theta) {
  stopifnot(theta > 0)
  return(1/(theta*pi*(1 + ((x - eta)/theta)^2)))
}

my.pcauchy = function (x, eta, theta) {
  stopifnot(theta > 0)
  
  integral = function (x) {
    my.dcauchy(x, eta, theta)
  }
  
  return(integrate(integral, lower = -Inf, upper = x)$value)
}

eta = 0
theta = 2
xs = seq(-10, 10)
estimate = sapply(xs, function(x) my.pcauchy(x, eta, theta))
truth = sapply(xs, function(x) pcauchy(x, eta, theta))
round(cbind(estimate, truth),4)

## ------------------------------------------------------------------------
#set the likelihood function 
lnL <- function(p, q, nA =28, nB = 24, nAB = 70, nO = 41) {
  r = 1.0 - p - q
  nA * log(p^2 + 2*p*r) + nB * log(p^2 + 2 * q * r) + nAB * log(2 * p * q) + 2 * nO * log(r)
}

EM <- function (p,q,nA =28, nB = 24, nAB = 70, nO = 41, debug = FALSE) {
  
  # Evaluate the likelihood using initial estimates
  llk <- lnL(p, q, nA, nB, nAB, nO)
  
  # Count the number of iterations so far
  iter <- 1
  
  # Loop until convergence ...
  while (TRUE)
  {
    # Estimate the frequency for allele O
    r= 1.0 - p - q
    
    # First we carry out the E-step
    
    # The counts for genotypes O/O and A/B are effectively observed
    # Estimate the counts for the other genotypes
    nAA <- nA * p / (p + 2*r)
    nAO <- nA - nAA
    nBB <- nB * q / (q + 2*r)
    nBO <- nB - nBB
    
    # Print debugging information
    if (debug)
    {
      cat("Round #", iter, "lnLikelihood = ", llk, "\n\n")
      cat("          Allele frequencies:      p = ",   p, ",   q = ",   q, ",   r = ",   r, "\n\n")
      cat("          Genotype  counts:      nAA = ", nAA, ", nAO = ", nAO, ", nBB = ", nBB, ", nBO = ", nBO, "\n\n")
    }
    
    # Then the M-step
    p <- (2 * nAA + nAO + nAB) / (2 * (nA + nB + nO + nAB))
    q <- (2 * nBB + nBO + nAB) / (2 * (nA + nB + nO + nAB))
    
    
    # Then check for convergence ...
    llk1 <- lnL(p, q, nA, nB, nAB, nO)
    if (abs(llk1 - llk) < (abs(llk) + abs(llk1)) * 1e-6) break       
    
    # Otherwise keep going
    llk <- llk1
    iter <- iter + 1
  }
  
 cbind(p = p, q = q)
}
EM(0.3,0.2,nA =28, nB = 24, nAB = 70, nO = 41, debug = TRUE)


## ------------------------------------------------------------------------
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

 # with lapply()

print("***************************************** with lapply() ***********************************************************")

lapply(formulas, lm, data = mtcars)

 # with a for loop 

print("***************************************** with a for loop *********************************************************")

out_formulas <- vector('list', length(formulas))
   for(i in seq_along(formulas)) {
       out_formulas[[i]] <- lm(formulas[[i]], data = mtcars)
   }
   out_formulas

## ----bootstrap-----------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})

 # with lapply()

print("***************************************** with lapply() ***********************************************************")

lapply(bootstraps, lm, formula = mpg~disp)

 # with a for loop 

print("***************************************** with a for loop *********************************************************")

 out_bootstraps <- vector('list', length(bootstraps))
   for(i in seq_along(bootstraps)) {
       out_bootstraps[[i]] <- lm(mpg~disp, data = bootstraps[[i]])
   }
   out_bootstraps


## ----r-sqaure------------------------------------------------------------

rsq <- function(mod) summary(mod)$r.squared

bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})

models <- lapply(bootstraps, function(x) lm(mpg ~ disp, data = x))
unlist(lapply(models, rsq))


## ------------------------------------------------------------------------

trials <- replicate(
     100, 
     t.test(rpois(10, 10), rpois(7, 10)),
     simplify = FALSE
     )

print("***************************************** with sapply() *******************************************************")

sapply(trials, function(mod) mod$p.value)

#Extra challenge: get rid of the anonymous function by using `[[` directly.
print("*************************************** get rid of the anonymous function ********************************************")

sapply(trials, `[[`, 'p.value')

## ------------------------------------------------------------------------

library(parallel)
mcvMap <- function(f, FUN.VALUE , ...) {
  out <- mcMap(f, ...)
  vapply(out, identity, FUN.VALUE)
   }


## ------------------------------------------------------------------------
 getAnywhere(chisq.test)

## ------------------------------------------------------------------------
chisq.test2 <- function(x, y){

  # Input
  if (!is.numeric(x)) {
    stop("x must be numeric")}
  if (!is.numeric(y)) {
    stop("y must be numeric")}
  if (length(x) != length(y)) {
    stop("x and y must have the same length")}
  if (length(x) <= 1) {
    stop("length of x must be greater one")}
  if (any(c(x, y) < 0)) {
    stop("all entries of x and y must be greater or equal zero")}
  if (sum(complete.cases(x, y)) != length(x)) {
    stop("there must be no missing values in x and y")}
  if (any(is.null(c(x, y)))) {
    stop("entries of x and y must not be NULL")}

  # Help variables
  m <- rbind(x, y)
  margin1 <- rowSums(m)
  margin2 <- colSums(m)
  n <- sum(m)
  me <- tcrossprod(margin1, margin2) / n

  # Output
  x_stat = sum((m - me)^2 / me)
  dof <- (length(margin1) - 1) * (length(margin2) - 1)
  p <- pchisq(x_stat, df = dof, lower.tail = FALSE)

  return(list(x_stat = x_stat, df = dof, `p-value` = p))
}

## ------------------------------------------------------------------------
a <- 21:25
b <- c(21, 23, 25, 27, 29)
m <- cbind(a, b)
m

chisq.test(m)
chisq.test2(a, b)


## ------------------------------------------------------------------------
chisq.test2c <- compiler::cmpfun(chisq.test2)

microbenchmark::microbenchmark(
  chisq.test(m),
  chisq.test2(a, b),
  chisq.test2c(a, b)
)

## ------------------------------------------------------------------------
getAnywhere(table)

## ------------------------------------------------------------------------
table2 <- function(x, y) {

  x_val <- unique(x)

  y_val <- unique(y)

  mat <- matrix(0L, length(x_val), length(y_val))

  for (i in seq_along(x)) {

    mat[which(x_val == x[[i]]), which(y_val == y[[i]])] <-

      mat[which(x_val == x[[i]]),  which(y_val == y[[i]])] + 1L

  }

  dimnames <- list(x_val, y_val)

  names(dimnames) <- as.character(as.list(match.call())[-1])  # R has names for dimnames... :/

  tab <- array(mat, dim = dim(mat), dimnames = dimnames)

  class(tab) <- "table"

  tab

}

## ------------------------------------------------------------------------
a <- c(1, 2, 3)

identical(table(a, a), table2(a, a))

## ------------------------------------------------------------------------
microbenchmark::microbenchmark(table(a, a), table2(a, a))


## ------------------------------------------------------------------------
b <- c(2, 3, 4)
identical(table(a, b), table2(a, b))

c <- c(1, 2, 3, 1, 2, 3)
d <- c(2, 3, 4, 2, 3, 4)
identical(table(c, d), table2(c, d))

e <- c(1, 2, 2)
identical(table(a, e), table2(a, e))

identical(table(b, e), table2(b, e))

identical(table(e, e), table2(e, e))

f <- c(1, 1, 1)
identical(table(f, f), table2(f, f))


identical(table(e, f), table2(e, f))

g <- c(1, 4, 9)
identical(table(g, g), table2(g, g))
identical(table(g, f), table2(g, f))


h <- c(10, 20, 30)
identical(table(h, h), table2(h, h))

identical(table(h, f), table2(h, f))

identical(table(h, g), table2(h, g))

i <- c(0, 0, 0)

identical(table(i, i), table2(i, i))

identical(table(h, i), table2(h, i))


## ------------------------------------------------------------------------
j <- c(0, 10, 100, 1000, 10000)

identical(table(j, j), table2(j, j))

microbenchmark::microbenchmark(table(j, j), table2(j, j))

