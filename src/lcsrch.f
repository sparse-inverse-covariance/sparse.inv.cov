      SUBROUTINE LCSRCH(N,X,F,G,S,STP,FTOL,XTOL,MAXFEV,INFO,NFEV,WA)
      INTEGER N,MAXFEV,INFO,NFEV
      DOUBLE PRECISION F,F0,STP,FTOL,GTOL,XTOL,STPMIN,STPMAX
      DOUBLE PRECISION X(N),G(N),S(N),WA(N)
      COMMON /LB3/MP,LP,GTOL,STPMIN,STPMAX
      SAVE
C SIMPLE LINE SEARCH ALGORITHM FOR COVARIANCE SELECTION
C ASSUMES THAT UNDEFINED FUNCTION VALUES ARE GIVEN A VERY LARGE VALUE
      IF(INFO.EQ.-1)GO TO 20
      NFEV=0
      DO 10 j=1,N
 10   WA(J)=X(J)
      F0=F
 20   CONTINUE
      IF((F.GE.F0).AND.(NFEV.LE.MAXFEV))THEN
        IF(INFO.EQ.-1) STP=STP/2
c        write(6,*)STP,F,F0
        DO 30 J=1,N
 30     X(J)=WA(J)+STP*S(J)
        NFEV=NFEV+1
        INFO=-1
      RETURN
        ELSE
        INFO=1
        IF(NFEV.GT.MAXFEV)INFO=3
      ENDIF
      RETURN
      END
