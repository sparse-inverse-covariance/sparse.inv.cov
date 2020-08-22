C***********************************************************************
C CUSTOMIZED, NO BOUNDS
C***********************************************************************
C     MAIN PROGRAM TO MINIMIZE A FUNCTION (REPRESENTED BY THE ROUTINE SFUN)
C OF N VARIABLES X - CUSTOMIZED VERSION
C eps controls convergence (maxval(abs(g))<eps)
C reads data from INFILE and X.DAT -check names below!!!!
C
      implicit none
      INTEGER P,M,N,NAF,NSAMP,NWORK,NDIM
      integer MAXIT, LSI, I,  ISTARTIT, MAXFUN ,NC,MSAVE
      PARAMETER(P=6,N=10,NAF=10,NSAMP=100)
      PARAMETER(M=20,MAXIT=40000,LSI=0)
      DOUBLE PRECISION  X(N), F, G(N),XTOL 
      DOUBLE PRECISION DIAG(N),W(N*(2*M +1)+2*M),G1(N),LOGL
      DOUBLE PRECISION EPS ,UNDEF,DMX ,BBB(N)
      INTEGER IPRINT(2),ICALL,J
      
      DOUBLE PRECISION  AF(NAF),L(NAF),S(N),A0(N),XX(NSAMP,P),
     1                 CMULTA(N),LT(NAF),A(NAF),AT(NAF),
     2                 DA(NAF),TMP(NAF)
    
      INTEGER IAF(P+1),JAF(NAF),IFLAG ,IASUB(N),PERM(NAF),PERMI(NAF),
     1        IA0(P+1),JA0(N),IDIA(P),IAFT(P+1),
     2        JAFT(NAF),NFTOTL
    
      REAL TIME0,TIME1
      EXTERNAL          SFUN
    
C LBFGS arrays 
           
      LOGICAL DIAGCO
      integer ITER,NFUN
      double precision GNORM,STP,cputime
      NDIM = N
      MSAVE = M
      NWORK = NDIM*(2*MSAVE +1)+2*MSAVE

C
C     READ IN DATA AND SET UP ARRAYS
C
C SET IEXACT=1 FOR ANALYTICAL HESSIAN MULTIPLICATION
       IPRINT(1)=10000
       IPRINT(2)=0
c       MAXFUN = 40000   -- not used
       EPS=1e-04 
       OPEN(UNIT=3,FILE="INFILE.check")
       OPEN(UNIT=4,FILE="X.DAT.check")
       READ(3,*)(IA0(I),I=1,P+1)
       READ(3,*)(JA0(I),I=1,N)
       READ(3,*)(A0(I),I=1,N)
       READ(3,*)(IAF(I),I=1,P+1)
       READ(3,*)(JAF(I),I=1,NAF)
C
       DO 10 I=1,P
 10    READ(4,*)(XX(J,I),J=1,NSAMP)
 
       CLOSE(UNIT=3)
       CLOSE(UNIT=4)
 
c      sets TMP,A, DA to zero
       CALL RZERO(NAF,TMP)
       CALL RZERO(NAF,A)
       CALL RZERO(NAF,DA)



C        write (6,*)"IA0"
C        write (6,*)(IA0(i),i=1,5)
C        write (6,*)"JA0"
C        write (6,*)(JA0(i),i=1,5)
C        write (6,*) "XX"
C        write (6,*)(XX(i,1),i=1,5)
C        write (6,*) "IAF"
C        write (6,*)(IAF(i),i=1,5)
C        write (6,*)"JAF"
C        write (6,*)(JAF(i),i=1,5)


        call driver(P,N,NAF,NSAMP,M,MAXIT,LSI,NWORK,IA0,JA0,A0,IAF,JAF,
     +              XX,IPRINT,IFLAG,cputime,
     +             X, F, G,XTOL, DIAG,W,G1,LOGL,EPS ,UNDEF,DMX ,BBB,
     +             AF,L,S,CMULTA,LT,A,AT,DA,TMP,
     +                  ITER,NFUN,GNORM,STP)


        write (6,*)"IFLAG"
        write (6,*) IFLAG 
        write (6,*) "cputime"
        write (6,*) cputime















        stop
        end

c  NDIM = N
C      MSAVE = M
C      NWORK = NDIM*(2*MSAVE +1)+2*MSAVE


C P=13610integer 
C N=25314  integer 
C NAF=33083   integer 
C NSAMP=45  integer 
C M=20  integer 
C MAXIT=40000
C LSI=0
C NWORK = NDIM*(2*MSAVE +1)+2*MSAVE   integer      NDIM=N MSAVE =M 
C IA0(P+1)  integer 
C JA0(N)    integer 
C IAF(P+1)  integer 
C JAF(NAF)   integer  
C XX(NSAMP,P) double
C NDIM = N   integer 
C MSAVE = M   integer 
C IPRINT(2)   integer 
C MAXFUN = 40000



C X(N)   workspace
C  F   not used
C  G(N)   workspace
C XTOL - DESIRED ACCURACY FOR THE SOLUTION X* --  set in driver
C  DIAG(N)  workspace 
C W(N*(2*M +1)+2*M) workspace
C G1(N)
C LOGL
C EPS=1e-04 
C UNDEF=1.0d38     set in driver
C DMX   not used
C BBB(N)  workspace 
C AF(NAF)  workspace 
C L(NAF)  workspace 
C S(N)  workspace 
C A0(N)  workspace 
C CMULTA(N),  workspace 
C LT(NAF)  workspace 
C A(NAF)  workspace 
C AT(NAF)  workspace 
C DA(NAF)  workspace 
C TMP(NAF)  workspace 
