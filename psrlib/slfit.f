 
        SUBROUTINE SLFIT(X,Y,NDATA,SIG,MWT,A,B,SIGA,SIGB,CHI2,Q,SIGDAT)
C
C       Press et al. Numerical Recipes p 508
C
C       Implemented by R. Prestage, Steward Observatory 3/21/86
 
C  Given a set of NDATA points X(I), Y(I) with standard deviations
C  SIG(I), fit them to a straight line Y = A + BX by minimising
C  chi-squared. Returned are A, B and their respective probable
C  uncertainties SIGA, SIGB and the chi-square CHI2. If MWT=0 on
C  input, then the standard deviations are assumed to be unavailable,
C  the normalisation of chi-squared is to unit standard deviation
C  on all points. In this case, an estimate of the sigma of the
C  data points SIGDAT is returned.
C (The formula quoted by Press et al is wrongly quoted as Y = AX + B
 
C  N.B. I HAVE NOT IMPLEMENTED THE Q (=GOODNESS OF FIT PROBABILITY)
C  Q IS RETURNED = 0 FOR SUPPLIED SIG(I), 1 FOR NO SIG(I)
 
      DIMENSION X(NDATA), Y(NDATA), SIG(NDATA)
 
C Initialise sums to zero.
 
      SX = 0.0
      SY = 0.0
      ST2 = 0.0
      B = 0.0
 
C Accumulate sums.
 
      IF (MWT.NE.0) THEN
 
         SS = 0.0
         DO I = 1,NDATA
            WT = 1.0/(SIG(I)**2)
            SS = SS + WT
            SX = SX + X(I) * WT
            SY = SY + Y(I) * WT
         END DO
 
      ELSE
 
         DO I = 1,NDATA
            SX = SX + X(I)
            SY = SY + Y(I)
         END DO
         SS = FLOAT(NDATA)
 
      END IF
 
      SXOSS = SX/SS
 
      IF (MWT.NE.0) THEN
 
         DO I = 1,NDATA
            T = (X(I)-SXOSS)/SIG(I)
            ST2 = ST2 + T * T
            B = B + T * Y(I)/SIG(I)
         END DO
 
      ELSE
 
         DO I = 1,NDATA
            T = X(I) - SXOSS
            ST2 = ST2 + T * T
            B = B + T * Y(I)
         END DO
 
      END IF
 
C Solve for A, B, sigma(A), sigma(B).
 
      B = B/ST2
      A = (SY - SX * B) / SS
      SIGA = SQRT((1.0 + SX * SX / (SS * ST2)) / SS)
      SIGB = SQRT(1.0 / ST2)
 
      CHI2 = 0.0
 
C Calculate chi-squared.
 
      IF (MWT.EQ.0) THEN
 
C For unweighted data evaluate typical sig using CHI2,
C and adjust the standard deviations. Return Q = 1.0
C and SIGDAT (typical sigma of data) calculated from CHI2.
 
         DO I = 1,NDATA
            CHI2 = CHI2 + (Y(I) - A - B * X(I)) ** 2
         END DO
 
         Q = 1.0
         SIGDAT = SQRT(CHI2 / (NDATA - 2))
         SIGA = SIGA * SIGDAT
         SIGB = SIGB * SIGDAT
 
      ELSE
 
C For weighted data return Q = 0.0, and SIGDAT = 0.0.
 
         DO I = 1,NDATA
            CHI2 = CHI2 + ((Y(I) - A - B *X(I))/SIG(I))**2
         END DO
 
         Q = 0.0
         SIGDAT = 0.0
 
      END IF
 
      RETURN
      END
