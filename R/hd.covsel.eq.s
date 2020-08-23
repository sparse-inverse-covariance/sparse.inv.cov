hd.covsel.eq<-function(X,A,igf,nsamp,eps=1e-4,corr=FALSE,method="lbfgs",m=20,
                    verbose=-1,init.A=FALSE,type="inv.cov")
  {
    # function to fit high dimensional covariance selection models
    # X - is n by p raw data matrix
    # A - is a p by p sparse incidence matrix (dgCMatrix)defining the non zeros in sigma invers
    # igf is grouping factor of length = no of unique nonzero entries in sigma inverse
    # including upper and lower triangular elements and diagonal  (nonzero elements are equal to 1)
    # eps - is tolerance on first derivatvies of likelihood function to determine convergence
    # use correlation matrix for estimtion if corr=T, covariance matrix if corr=F
    # method - lbfgs for limited memory Fletcher Powell,  TN for truncated newton
    # m - number of terms to save in second derivative approximation for method lbfgs
    #   - number of inner conjugate gradient iterations for method TN
    #
    # get sparse matrix representations and permutation and write out data in form
    # for optimisation code
    nsel<-1
    if(type=="part.corr")nsel<-2
    if(type=="regn")nsel<-3
    nsel<-as.integer(nsel)
    #
    A<-as(A,"dgCMatrix")   # added to force this
    p<-ncol(A)
    if(!init.A){
    A<-1e-4*A
    diag(A)<-1
    }

    symmetric.A<-as(A,"symmetricMatrix")  # fills in both upper and lower triangle to make "a" symmetric
    if(length(igf)!=length(symmetric.A@x))return("Error: igf must be of length A@x for A symmetric")
    nr<-as.integer(length(unique(igf)))   # ?? check igf is valid as well

    res<-Cholesky(symmetric.A,LDL=FALSE,perm=TRUE)
    L<-expand(res)$L
    P<-(expand(res)$P)
    perm<-P@perm
    igf<-perm.constr(symmetric.A,perm,igf)
    # permute a for minimum fillin
    symmetric.A<-symmetric.A[perm,perm]
    L<-as(t(L),"dgCMatrix") # row rep = col rep of transpose
    # what is L
    # reorder constraints as well
    X<-scale(X,center=TRUE,scale=corr)
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
# set up additonal arrays needed  in driver for contraints
# doubles
      gg<-rep(0,n)
      xr<-rep(0,nr)
      gr<-xr
      gam<-gg
      div<-gr
# integer
      igf<-as.integer(igf)
      igg<-as.integer(rep(0,n+1))
      jgg<-as.integer(rep(0,n))
#    if(!is.loaded("drivereq")) dyn.load("drivereq.o")
    z<- .Fortran("drivereq",
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
                 as.integer(init.A),
                 as.integer(nr),
                 as.double(gg),
                 as.double(xr),
                 as.double(gr),
                 as.double(gam),
                 as.double(div),
                 as.integer(igf),
                 as.integer(igg),
                 as.integer(jgg),nsel
                 )
    
    ss<-new("dgRMatrix",p=as.integer(z[[9]]-1),j=as.integer(z[[10]]-1),x=z[[32]],Dim=as.integer(c(p,p)))
    ss<-ss+t(ss)
    diag(ss)<-diag(ss)/2
    ss<-as(ss,"symmetricMatrix")
    #
    if(nsel==1){
    sinv<-new("dgRMatrix",p=as.integer(z[[9]]-1),j=as.integer(z[[10]]-1),x=z[[18]],Dim=as.integer(c(p,p)))
    sinv<-sinv+t(sinv)
    diag(sinv)<-diag(sinv)/2
    sinv<-as(sinv,"symmetricMatrix")
    fitted<-NULL
    }
    if(nsel==2){
    fitted<-new("dgRMatrix",p=as.integer(z[[9]]-1),j=as.integer(z[[10]]-1),x=z[[18]],Dim=as.integer(c(p,p)))
    fitted<-fitted+t(fitted)
    diag(fitted)<-diag(fitted)/2
    fitted<-as(fitted,"symmetricMatrix")
    d<-diag(fitted)
    D<-Diagonal(p,d^0.5)
    sinv<-D%*%fitted%*%D
    diag(sinv)<-d
    }
    if(nsel==3){
    beta<-new("dgRMatrix",p=as.integer(z[[9]]-1),j=as.integer(z[[10]]-1),x=z[[18]],Dim=as.integer(c(p,p)))
# construct matrix of regression coefficients and residual variances
   d<-diag(beta)
   beta.c<-dgr2dgc(beta) # Matrix library doesnt have functions without converting
   diag(beta.c)<-0
   D<-Diagonal(p,1/d)
   beta<-D%*%beta.c
   D<-Diagonal(p,d)
   beta<-beta%*%D
   beta<-beta.c+t(beta)
   diag(beta)<-d
# compute sigma inverse
   d<-diag(beta)
   D<-Diagonal(p,1/d)
   sinv<-D%*%beta*(-1)
   diag(sinv)<-1/d
   fitted<-beta
   }
# undo permutation
    perm.sav<-perm
    P<-t(P)
    perm<-P@perm
    sigma<-ss[perm,perm]
    if(!is.null(fitted))fitted<-fitted[perm,perm]
    sinv<-sinv[perm,perm]
    sinv<-as(sinv,"symmetricMatrix")
    list(sigma=sigma,sigma.inv=sinv,fitted=fitted,iflag=z[[16]],niter=z[[39]],nfunc=z[[40]],
         cputime=z[[17]],gnorm=z[[41]],stp=z[[42]],naf=naf,nas=nas,perm=perm.sav,type=type,F=z[[19]])
  }
