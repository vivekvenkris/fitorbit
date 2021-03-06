*DECK FOLDS
C
C ****************************************************************
      SUBROUTINE FOLDS ( DAT, ND, P, SCALE, PROF, N )
C ****************************************************************
C
C FOLDS THE BYTE ARRAY DAT(ND) AT PERIOD P, PRODUCING A PROFILE OF
C     LENGTH N=1.25*P+0.5 RETURNING THE RESULT IN PROF(N), WITH THE
C     MEAN SUBTRACTED AND SCALED BY SCALE AND THE SQUARE ROOT
C     OF THE NUMBER OF PERIODS.
C
      BYTE DAT(*)
      DIMENSION PROF(*)
      DOUBLE PRECISION P,XK
C
C     COMPUTE THE NUMBER OF BINS IN THE PROFILE AND THE NUMBER OF
C     PERIODS IN THE DATA SPAN.
C
      N = 1.25*P+0.5
      JP = P+0.99999
      IF (N.LE.JP) N = JP+1
      NPRD = (ND-N+JP-1)/P
C
C     COPY THE FIRST PERIOD.
C
      DO 10 J=1,N
         PROF(J) = DAT(J)
   10 CONTINUE
C
C     ADD SUBSEQUENT PERIODS.
C
      XK=P
      DO 20 IPRD=2,NPRD
         K=XK
         DO 30 J=1,N
            PROF(J) = PROF(J)+DAT(K+J)
   30    CONTINUE
         XK=XK+P
   20 CONTINUE
C
C     COMPUTE THE MEAN.
C
      PMEAN = 0.0
      DO 40 I=1,N
         PMEAN = PMEAN+PROF(I)
   40 CONTINUE
      PMEAN = PMEAN/N
C
C     SUBTRACT THE MEAN AND SCALE.
C
      FAC = 1.0/(NPRD*SCALE)
      DO 50 I=1,N
         PROF(I) = (PROF(I)-PMEAN)*FAC
   50 CONTINUE
C
      RETURN
C
C END OF SUBROUTINE FOLDS.
C
      END
