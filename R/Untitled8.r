mvn.transf<-function(X)
{
# compute transformation to multivariate normal
n<-nrow(X)
p<-ncol(X)
m<-colMeans(X)
sd<-apply(X,2,var)*(n-1)/n
sd<-sd^0.5
delta<-1/((4*n^0.25)*(pi*log(n))^0.5)
Y<-apply(X,2,my.ecdf,delta)
X<-apply(Y,2,qnorm)
X<-sweep(X,2,sd,"*")
X<-sweep(X,2,m,"+")
X
}
################
my.ecdf<-function(x,delta)
{
deltac<-1-delta
a<-ecdf(x)
f<-a(x)
f[f<delta]<-delta
f[f>deltac]<-deltac
f
}

