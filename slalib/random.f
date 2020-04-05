      REAL FUNCTION sla_RANDOM (SEED)
*+
*     - - - - - - -
*      R A N D O M
*     - - - - - - -
*
*  Generate pseudo-random real number in the range 0 <= X < 1.
*  (single precision)
*
*  !!! Sun 4 dependent !!!
*
*  Given:
*     SEED     real     an arbitrary real number
*
*  Notes:
*
*  1)  The result is a pseudo-random REAL number in the range
*      0 <= sla_RANDOM < 1.
*
*  2)  SEED is used first time through only.
*
*  Called:  RAND (a REAL function from the Sun Fortran Library)
*
*  P.T.Wallace   Starlink   14 October 1991
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL SEED

      REAL RAND

      REAL AS
      INTEGER ISEED
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./



*  If first time, turn SEED into a large, odd integer, and start the
*  generator
      IF (FIRST) THEN
         AS=ABS(SEED)+1.0
         ISEED=NINT(AS/10.0**(NINT(ALOG10(AS))-6))
         IF (MOD(ISEED,2).EQ.0) ISEED=ISEED+1
         FIRST=.FALSE.
         AS=RAND(ISEED)
      END IF

*  Next pseudo-random number
      sla_RANDOM=RAND(0)

      END
