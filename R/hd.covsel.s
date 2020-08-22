hd.covsel<-function(X,A,nsamp,eps=1e-4,corr=FALSE,method="lbfgs",m=20,
                    verbose=-1,init.A=FALSE)
  {
    # function to fit high dimensional covariance selection models
    # X - is n by p raw data matrix
    # A - is a p by p sparse incidence matrix (dgCMatrix)defining the non zeros in sigma inverse
    # including upper and lower triangular elements and diagonal  (nonzero elements are equal to 1)
    # eps - is tolerance on first derivatvies of likelihood function to determine convergence
    # use correlation matrix for estimtion if corr=T, covariance matrix if corr=F
    # method - lbfgs for limited memory Fletcher Powell,  TN for truncated newton
    # m - number of terms to save in second derivative approximation for method lbfgs
    #   - number of inner conjugate gradient iterations for method TN
    #
    # get sparse matrix representations and permutation and write out data in form
    # for optimisation code
    A<-as(A,"dgCMatrix")   # added to force this
    p<-ncol(A)
    if(!init.A){
    A<-1e-4*A
    diag(A)<-1
    }
    
    
    symmetric.A<-as(A,"symmetricMatrix")  # fills in both upper and lower triangle to make "a" symmetric
    res<-Cholesky(symmetric.A,LDL=FALSE,perm=TRUE)
    L<-expand(res)$L
    P<-(expand(res)$P)
    perm<-P@perm
    # permute a for minimum fillin
    symmetric.A<-symmetric.A[perm,perm]
    L<-as(t(L),"dgCMatrix") # row rep = col rep of transpose
    # what is L
    
    X<-scale(X,center=TRUE,sc=corr)
    if(corr){
      X<-X*(nsamp/(nsamp-1))^0.5
    }
    # reorder X
    X<-X[,perm]
    
    n<-length(symmetric.A@i)
    naf<-length(L@i)
    nas<-length(A@i)
    ip<- -1
    if (verbose>0){
      print(paste("naf =",naf, sep= ""))
      print(paste("nas =",nas, sep= ""))
      ip<-as.integer(verbose)
    }
#    if(!is.loaded("driver")) dyn.load("driver.o")
    z<- .Fortran("driver",
                 as.integer(p),
                 as.integer(n),
                 as.integer(naf),
                 as.integer(nsamp),
                 as.integer(m),
                 as.integer(maxit<-40000),
                 as.integer(lsi<-0),
                 as.integer( nwork<- n*(2*m +1)+2*m ),
                 as.integer(symmetric.A@p+1),   #IAO
                 as.integer(symmetric.A@i+1),   #JAO
                 as.double(symmetric.A@x),   #A0
                 as.integer(L@p+1),    #IAF
                 as.integer(L@i+1),    #JAF
                 as.double(X),
                 as.integer(iprint<-c(ip,0)),
                 as.integer(iflag<-0),
                 as.double(cputime<-0.0),
                 as.double(XX<-rep(0,n)),  #18
                 as.double(F<-0),
                 as.double(G<-rep(0,n)),
                 as.double(xtol<-10e-5),
                 as.double(diag<-rep(0,n)),
                 as.double(ww<-rep(0,n*(2*m +1)+2*m )),
                 as.double(g1<-rep(0,n)),
                 as.double(lolg<-0.0),
                 as.double(eps),
                 as.double(udef<-1.0e38),
                 as.double(dmax<-0),
                 as.double(bbb<-rep(0,n)),
                 as.double(af<-rep(0,naf)),     #30
                 as.double(ll<-rep(0,naf)),
                 as.double(ss<-rep(0,n)),
                 as.double(cumula<-rep(0,n)),
                 as.double(lt<-rep(0,naf)),
                 as.double(A<-rep(0,naf)),
                 as.double(At<-rep(0,naf)),
                 as.double(DA<-rep(0,naf)),
                 as.double(tmp<-rep(0,naf)),
                 as.integer(iter<-0),
                 as.integer(nfun<-0),          
                 as.double(gnorm<-0.0),
                 as.double(stp<-0.0),
                 as.integer(init.A)
                 )
    
    ss<-new("dgRMatrix",p=as.integer(z[[9]]-1),j=as.integer(z[[10]]-1),x=z[[32]],Dim=as.integer(c(p,p)))
    sinv<-new("dgRMatrix",p=as.integer(z[[9]]-1),j=as.integer(z[[10]]-1),x=z[[18]],Dim=as.integer(c(p,p)))
    
    ss<-ss+t(ss)
    diag(ss)<-diag(ss)/2
    si<-sinv+t(sinv)
    diag(si)<-diag(si)/2
    si<-as(si,"symmetricMatrix") #as(.,"dsCMatrix") is deprecated; do use as(., "symmetricMatrix")
    # undo permutation
    P<-t(P)
    perm<-P@perm
    sigma<-ss[perm,perm]
    sigma.inv<-si[perm,perm]

    list(sigma=as(sigma,"symmetricMatrix"),sigma.inv=sigma.inv,iflag=z[[16]],niter=z[[39]],nfunc=z[[40]],
         cputime=z[[17]],gnorm=z[[41]],stp=z[[42]],naf=naf,nas=nas,F=z[[19]])
  }
