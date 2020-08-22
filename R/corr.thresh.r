corr.thresh<-function(x,nlarge,block.size,nbin2=501,use.ginverse=FALSE)
{
# function to compute nlarge absolute correlations given a raw n by p matrix x
# without storing the entire correlation matrix (diagonals not computed)
# blocksize is number of rows to process at once  (max p usually much less)
# nbin2 is the number of bins in the histogram of the correlation matrix
# use.ginverse=T - use Moore Penrose inverse correlation matrices for threshloding and histograms
        n<-as.integer(nrow(x))
        p<-as.integer(ncol(x))
#
        x<-scale(x,center=TRUE,scale=TRUE)
        x<-x/(nrow(x)-1)^0.5
#
        if(use.ginverse){
        res<-svd(x)
        d<-res$d
        ind<-d<1e-8
        d<-1/d
        d[ind]<-0
        x<-t(res$v%*%diag(d))
        d<-apply(x,2,norm2)
        if(any(d>1))return("error:diagonals of inverse greater than one")
        x<-sweep(x,2,1/d^0.5,"*")  # compute "partial" correlations
        }
        x<-as.vector(x)
#
        nlarge<-as.integer(nlarge)
        nb<-seq(1,p+1,block.size)
        nb<-unique(c(nb,p+1))
        nb<-as.integer(nb)
        inb<-as.integer(length(nb))
#
        if(inb>1)r<-nb[2]-1
        else r<-nb-1
        dr<-r*p-r*(r+1)/2
        nlarge<-as.integer(min(nlarge,dr)) # stop potential fortran crash
        nzs<-as.integer(nlarge+dr)
        zstor<-as.double(rep(0,nzs))
        istor<-as.integer(rep(0,nzs))
        jstor<-istor
#
        perm<-as.integer(rep(0,nzs))
        itop<-as.integer(rep(0,nlarge))
#
        jtop<-itop
        ztop<-as.double(rep(0,nlarge))
        ndz<-as.integer(nzs-nlarge)
        z<-as.double(rep(0,ndz))
#
        tmp<-as.double(rep(0,nzs))
#
        if(nbin2%%2==0)nbin2<-nbin2+1
        nbin2<-as.integer(nbin2)
        iy<-as.integer(rep(0,nbin2))
        nbin<-(nbin2-1)/2
        hx<-seq(-1,1,1/nbin)
#
        res<-.Fortran("threshit",
        x,n,p,
        nlarge,nb,inb,
        zstor,istor,jstor,
        perm, nzs,itop,
        jtop,ztop,z,ndz,
        tmp,iy,nbin2)
#
        list(itop=res[[12]],jtop=res[[13]],rtop=res[[14]],hx=hx,hy=res[[18]])
}

norm2<-function(x)
{ x
sum(x*x)
}
