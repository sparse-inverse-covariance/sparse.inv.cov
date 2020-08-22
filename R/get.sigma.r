get.sigma<-function (A)
{
    iflag<-0
    A <- as(A, "dgCMatrix")
    p <- ncol(A)
    symmetric.A <- as(A, "symmetricMatrix")
    check<-try(res <- Cholesky(symmetric.A, LDL = FALSE, perm = TRUE),silent=T)
    if(class(check)=="try-error")return("matrix not positive definite")
    L <- expand(res)$L
    P <- (expand(res)$P)
    perm <- P@perm
    symmetric.A <- symmetric.A[perm, perm]
    L <- as(t(L), "dgCMatrix")
    n <- length(symmetric.A@i)
    naf <- length(L@i)
    nas <- length(A@i)
    z <- .Fortran("getsigma", as.integer(n), as.integer(naf), as.integer(p),
         as.double(x<-symmetric.A@x),as.double(f<-0),
         as.integer(ia0<-symmetric.A@p + 1),as.integer(ja0<-symmetric.A@i + 1), as.double(a0<-symmetric.A@x),
         as.integer(iaf<-L@p + 1), as.integer(jaf<-L@i + 1),as.double(af<-rep(0,naf)),
         as.double(l<-L@x),as.integer(iasub<-rep(0,n)),
         as.double(cmulta<-rep(0,n)),as.integer(perm<-rep(0,naf)),as.integer(permi<-rep(0,naf)),
         as.integer(iaft<-rep(0,p+1)),as.integer(jaft<-rep(0,naf)),as.double(a<-rep(0,naf)),
         as.double(at<-rep(0,naf)),as.double(lt<-rep(0,naf)))
         
    if(z[[5]]>0.99e38){iflag<-1
    return("matrix not positive definite","\n")
    }
         
    ss <- new("dgRMatrix", p = as.integer(z[[9]] - 1), j = as.integer(z[[10]] -
        1), x = z[[19]], Dim = as.integer(c(p, p)))
        ss <- ss + t(ss)
    diag(ss) <- diag(ss)/2

    P <- t(P)
    perm <- P@perm
    sigma <- ss[perm, perm]

    list(sigma = as(sigma, "symmetricMatrix"),n=n , naf = naf,iflag=iflag)
}

