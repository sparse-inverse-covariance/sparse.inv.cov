         SUBROUTINE GETSIGMA(N,NAF,P,X,F,IA0,JA0,A0,IAF,JAF,AF,L,
     1                       IASUB,CMULTA,PERM,PERMI,IAFT,JAFT,A,AT,LT)
C
         INTEGER N,NAF,P,IA0(*),JA0(*),IAF(*),JAF(*),IASUB(*),PERM(*),
     1             PERMI(*),IAFT(*),JAFT(*)
         DOUBLE PRECISION X(*),F,A0(*),AF(*),L(*),CMULTA(*),
     1                    A(*),AT(*),LT(*)
C
            CALL MAPIT(P,IA0,JA0,A0,IAF,JAF,AF,IASUB,CMULTA)
C
            CALL GETPERMS(P,IAF,JAF,IAFT,JAFT,PERM,PERMI,NAF)
            
            CALL SIGMA (N, X, F,IAF,JAF,AF,L,IASUB,CMULTA,
     1                  PERM,PERMI,IAFT,JAFT,LT,A,AT,P)
     
C RETURN SIGMA IN IAF,JAF A

        RETURN
        
        END
