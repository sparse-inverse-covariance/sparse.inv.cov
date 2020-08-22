perm.constr<-function(A,perm,igf)
{
A@x<-igf
A<-A[perm,perm]
ind<-unique(A@x)
n<-length(ind)
z<-rep(0,n)
z[ind]<-1:n
igf<-z[A@x]
igf
}


