*DECK REAMIN
C
C
C **********************************************************************
      FUNCTION REAMIN ( )
C **********************************************************************
C
C THIS FUNCTION RETURNS THE MINIMUM ALLOWABLE POSITIVE SINGLE PRECISION
C     VALUE.
C
C THIS ROUTINE IS INSTALLATION DEPENDENT.
C
C VAX-11 FORTRAN VERSION.
C
      PARAMETER (RMIN=0.3E-38)
C
C RETURN THE MINIMUM VALUE.
C
      REAMIN=RMIN
      RETURN
C
C END OF REAL FUNCTION REAMIN.
C
      END
