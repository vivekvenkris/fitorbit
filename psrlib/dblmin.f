*DECK DBLMIN
C
C
C **********************************************************************
      DOUBLE PRECISION FUNCTION DBLMIN ( )
C **********************************************************************
C
C THIS FUNCTION RETURNS THE MINIMUM ALLOWABLE POSITIVE DOUBLE PRECISION
C     VALUE.
C
C THIS ROUTINE IS INSTALLATION DEPENDENT.
C
C VAX-11 FORTRAN VERSION.
C
      DOUBLE PRECISION DMIN
      PARAMETER (DMIN=0.3D-38)
C
C RETURN THE MINIMUM VALUE.
C
      DBLMIN=DMIN
      RETURN
C
C END OF REAL FUNCTION DBLMIN.
C
      END
