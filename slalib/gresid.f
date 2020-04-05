      REAL FUNCTION sla_GRESID (S)
*+
*     - - - - - - -
*      G R E S I D
*     - - - - - - -
*
*  Generate pseudo-random normal deviate ( = 'Gaussian residual')
*  (single precision)
*
*  !!! Sun 4 specific !!!
*
*  Given:
*     S      real     standard deviation
*
*  The results of many calls to this routine will be
*  normally distributed with mean zero and standard deviation S.
*
*  The Box-Muller algorithm is used.  This is described in
*  Numerical Recipes, section 7.2.
*
*  Called:  RAND (a REAL function from the Sun Fortran Library)
*
*  P.T.Wallace   Starlink   14 October 1991
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL S

      REAL X,Y,R,W,GNEXT,G
      LOGICAL FTF,FIRST

      REAL RAND

      SAVE GNEXT,FIRST
      DATA FTF,FIRST / .TRUE.,.TRUE. /



*  First time through, initialise the random-number generator
      IF (FTF) THEN
         X = RAND(123456789)
         FTF = .FALSE.
      END IF

*  Second normal deviate of the pair available?
      IF (FIRST) THEN

*     No - generate two random numbers inside unit circle
         R = 2.0
         DO WHILE (R.GE.1.0)

*        Generate two random numbers in range +/- 1
            X = 2.0*RAND(0)-1.0
            Y = 2.0*RAND(0)-1.0

*        Try again if not in unit circle
            R = X*X+Y*Y
         END DO

*     Box-Muller transformation, generating two deviates
         W = SQRT(-2.0*LOG(R)/MAX(R,1E-20))
         GNEXT = X*W
         G = Y*W

*     Set flag to indicate availability of next deviate
         FIRST = .FALSE.
      ELSE

*     Return second deviate of the pair & reset flag
         G = GNEXT
         FIRST = .TRUE.
      END IF

*  Scale the deviate by the required standard deviation
      sla_GRESID = G*S

      END
