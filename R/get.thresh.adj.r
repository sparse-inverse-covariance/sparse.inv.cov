# computes adjacency matrix from a correlation thresholding object
get.thresh.adj<-function(obj,topno,p,include.diagonal=TRUE)
{
# obj is a corr.thresh object
# topno is how many of the largest absolute correlations to choose
# p is number of variables (ncol(x))
ind<-1:topno
di<-new("dsTMatrix",i=as.integer(obj$itop[ind]-1),j=as.integer(obj$jtop[ind]-1),x=rep(1,topno),Dim=as.integer(c(p,p)))
di<-as(di,"dsCMatrix")
if(include.diagonal)diag(di)<-1
di
}

