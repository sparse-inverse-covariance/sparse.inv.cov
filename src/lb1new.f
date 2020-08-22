      SUBROUTINE LB1NEW(IPRINT,ITER,NFUN,
     *               GNORM,N,M,X,F,G,STP,FINISH)
C
C     -------------------------------------------------------------
C     THIS ROUTINE PRINTS MONITORING INFORMATION. THE FREQUENCY AND
C     AMOUNT OF OUTPUT ARE CONTROLLED BY IPRINT modifed by hk to outut in R
C     -------------------------------------------------------------
C
      INTEGER IPRINT(2),ITER,NFUN,LP,MP,N,M
      DOUBLE PRECISION X(N),G(N),F,GNORM,STP,GTOL,STPMIN,STPMAX,STOR(5)
      LOGICAL FINISH
      character *80 labl
      COMMON /LB3/MP,LP,GTOL,STPMIN,STPMAX
C
      LABL='    ITER         NFUN         F            GNORM        STP'
      IF((MOD(ITER,IPRINT(1)).NE.0).AND.(ITER.NE.1))RETURN
              STOR(1)=ITER
              STOR(2)=NFUN
              STOR(3)=F
              STOR(4)=GNORM
              STOR(5)=STP
              IF(ITER.EQ.1) CALL DBLEPR(LABL,80,STOR,0)
              CALL DBLEPR(LABL,0,STOR,5)
              RETURN
              END
