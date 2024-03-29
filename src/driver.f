        subroutine driver(P,N,NAF,NSAMP,M,MAXIT,LSI,NWORK,IA0,JA0,A0,
     +              IAF,JAF,XX,IPRINT,IFLAG,cputime,
     +             X, F, G,XTOL, DIAG,W,G1,LOGL,EPS ,UNDEF,DMX ,BBB,
     +             AF,L,S,CMULTA,LT,A,AT,DA,TMP,
     +                  ITER,NFUN,GNORM,STP,ISTARTIT)

       implicit none
 
      integer MAXIT,LSI,I, ISTARTIT,NC
      DOUBLE PRECISION time0, time1, cputime
      INTEGER P,M,N,NAF,NSAMP,NWORK,NDIM
c      DOUBLE PRECISION  X(*), F, G(*),XTOL 
c      DOUBLE PRECISION DIAG(*),W(*),G1(*),LOGL
c      DOUBLE PRECISION EPS ,UNDEF,DMX ,BBB(*)
      INTEGER IPRINT(2),ICALL,J
c      DOUBLE PRECISION  AF(*),L(*),S(*),A0(*),XX(NSAMP,P),
c     1     CMULTA(*),LT(*),A(*),AT(*),
c     2     DA(*),TMP(*)
      INTEGER IAF(P+1),JAF(NAF),IFLAG ,IASUB(N),PERM(NAF),PERMI(NAF),
     1     IA0(P+1),JA0(N),IDIA(P),IAFT(P+1),
     2     JAFT(NAF),NFTOTL
      LOGICAL DIAGCO
      integer ITER,NFUN
      double precision GNORM,STP

C     INTEGER P,M,N,NAF,NSAMP,NWORK,NDIM
      DOUBLE PRECISION  X(N), F, G(N),XTOL 
      DOUBLE PRECISION DIAG(N),W(NWORK),G1(N),LOGL
      DOUBLE PRECISION EPS ,UNDEF,DMX ,BBB(N)
C     INTEGER IPRINT(2),ICALL,J
      DOUBLE PRECISION  AF(NAF),L(NAF),S(N),A0(N),XX(NSAMP,P),
     1      CMULTA(N),LT(NAF),A(NAF),AT(NAF),
     2      DA(NAF),TMP(NAF)
      
C          INTEGER IAF(P+1),JAF(NAF),IFLAG ,IASUB(N),PERM(NAF),PERMI(NAF),
C       1       IA0(P+1),JA0(N),IDIA(P),IAFT(P+1),
C       2       JAFT(NAF),NFTOTL
   
C          LOGICAL DIAGCO

C
C COMPUTE SPARSE SAMPLE COVARIANCE MATRIX
C       






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


       CALL SPCOVMAT(P,IA0,JA0,XX,NSAMP,S,IDIA)
c       write(6,*) "I got here"
c       write(6,*)(s(i),i=1,N)
C      
C
C SET UP ARRAYS FOR SPARSE MATRICES
C
       CALL MAPIT(P,IA0,JA0,A0,IAF,JAF,AF,IASUB,CMULTA)
C       
       CALL GETPERMS(P,IAF,JAF,IAFT,JAFT,PERM,PERMI,NAF) 
C
         IF(ISTARTIT.EQ.0)THEN
         CALL RZERO(N,X)
C         DO 95 I=1,N
C 95      X(I)=10.0D0
         DO 100 I = 1,P
C         S(IDIA(I))=S(IDIA(I))
          X(IDIA(I)) =1/S(IDIA(I))
C          X(IDIA(I))=1000.0D0
 100    CONTINUE
         ELSE
         CALL RZERO(N,X)
         DO 110 I=1,N
 110     X(I)=A0(I)
         ENDIF
c        OPEN(UNIT=25,FILE="C:\ITER.DAT")
c         read(25,*)(X(I),I=1,N)
C
C INITIAL GUESS AT FUNCTION VALUE
C

      call cpu_time(time0)
      CALL SFUN (N, X, F, G,IAF,JAF,AF,L,S,IASUB,CMULTA,
     1                  PERM,PERMI,IAFT,JAFT,LT,A,AT,P)
     
      NFTOTL=1
      
      IF(F.GT.1.0D36)THEN
      WRITE(6,'("INITIAL VALUE NOT POSITIVE DEFINITE")')
      IFLAG=1
      RETURN
      ENDIF
     
C
C SET UP CUSTOMIZING PARAMETERS                                                 
C ETA    - SEVERITY OF THE LINESEARCH
C MAXFUN - MAXIMUM ALLOWABLE NUMBER OF FUNCTION EVALUATIONS
C XTOL   - DESIRED ACCURACY FOR THE SOLUTION X*
C STEPMX - MAXIMUM ALLOWABLE STEP IN THE LINESEARCH
C ACCRCY - ACCURACY OF COMPUTED FUNCTION VALUES
C MSGLVL - DETERMINES QUANTITY OF PRINTED OUTPUT
C          0 = NONE, 1 = ONE LINE PER MAJOR ITERATION.
C MAXIT  - MAXIMUM NUMBER OF INNER ITERATIONS PER STEP
     
C      MAXIT  = 1000
      
      XTOL   = 1D-8
      
C SET INIT=1 in loop below to start CG algorithm from previous direction instead of 0 
C after first iteration
C
C MINIMIZE THE FUNCTION
C
C
C     WE DO NOT WISH TO PROVIDE THE DIAGONAL MATRICES HK0, AND
C     THEREFORE SET DIAGCO TO FALSE.
C      DIAGCO=.TRUE. is slower
      DIAGCO= .FALSE.
      
      XTOL= 1.0D-16
      ICALL=0
      IFLAG=0
      ITER=0
      NFUN=0
      GNORM=0.0
      STP=0.0
     
     
      UNDEF=1.0d38
      do 15 I=1,N
c      a(i)=1.0d0
  15  diag(i)=1.0d0
C
c       CALL SFUN (N, X, F, G,IAF,JAF,AF,L,S,IASUB,CMULTA,
c     1                  PERM,PERMI,IAFT,JAFT,LT,A,AT,P)
 20   CONTINUE
       If(IFLAG.EQ.2)THEN
        DO 210 J=1,N
 210    A0(J)=A(IASUB(J))
        CALL DHESS(IA0,JA0,A0,P,N,DIAG)
        DO 215 J=1,N
 215    DIAG(J)=1/DIAG(J)
        write(6,'("IM here")')
        ELSE
       Call RZERO(N,BBB)
   
        
            CALL SFUN (N, X, LOGL,BBB,IAF,JAF,AF,L,S,IASUB,CMULTA,
     1                  PERM,PERMI,IAFT,JAFT,LT,A,AT,P)
c  
c       if(LOGL.gt.1.0d37)iflag=1     
C
c      IF(IFLAG.eq.1)THEN
c      WRITE(6,'("LIKELIHOOD VALUE NOT DEFINED AT TEST POINT")')
c      LOGL=UNDEF
c      END IF
C
c      DMX=0
c      DO 27 I=1,N
c      IF(ABS(G(I)).GT.DMX)DMX=ABS(G(I))
c 27   CONTINUE
c      WRITE(6,'("MAX GRADIENT")')
c      WRITE(6,*)DMX
c      DMX=MAXVAL(ABS(G))
c      WRITE(6,*)DMX
      F= LOGL
      DO 30 J=1,N
      G(J)=BBB(J)
 30   CONTINUE
         ENDIF
C       CALL LBFGS(N,M,X,F,G,DIAGCO,DIAG,IPRINT,EPS,XTOL,W,IFLAG,G1
C      1           ,UNDEF,LSI,ITER,NFUN,GNORM,STP)
C      CALL LBFGS(N,M,X,F,G,DIAGCO,DIAG,IPRINT,EPS,XTOL,W,IFLAG,G1
C     1       ,UNDEF,LSI,ITER)
      CALL LBFGS(N,M,X,F,G,DIAGCO,DIAG,IPRINT,EPS,XTOL,W,IFLAG,G1
     1       ,UNDEF,LSI,ITER,NFUN,GNORM,STP)



      IF(IFLAG.LE.0) GO TO 50
      ICALL=ICALL + 1
C     WE ALLOW AT MOST MAXIT EVALUATIONS OF F AND G
      IF(ICALL.GT.MAXIT) GO TO 50
      GO TO 20
  50  CONTINUE

      call cpu_time(time1)
      cputime=(time1-time0)/60.0

      if (IPRINT(1).GE.0) then
         write(6,*)'Total cputime in minutes',cputime
         WRITE(6,'(" Number of function calls",I8)')ICALL+1
         NC=P+1
         OPEN(UNIT=12,FILE="resultslbf.dat")
         CALL IWRITE(NC,NC,IA0,12)
         CALL IWRITE(NC,N,JA0,12)
         CALL DWRITE(NC,N,X,12)
         OPEN(UNIT=14,FILE="slbf.dat")
         CALL IWRITE(NC,NC,IA0,14)
         CALL IWRITE(NC,N,JA0,14)
         CALL DWRITE(NC,N,S,14)
c         WRITE(6,'("OUTPUT IN FILES slbf.dat and resultslbf.dat")')
      end if


        write (6,*)"ITER"
        write (6,*) ITER
        write (6,*) "NFUN"
        write (6,*) NFUN
        write (6,*) "GNORM"
        write (6,*) GNORM
        write (6,*) "STP"
        write (6,*) STP
        write (6,*) "time0"
        write (6,*) time0
        write (6,*) "time1"
        write (6,*) time1
        write (6,*) "cputime"
        write (6,*) cputime



      END
C

C(time1-time0)/60
CICALL+1
CIFLAG.LE.0)
C
Cniter=niter
Cnfunc=nfunc+1,cputime=cputime)
