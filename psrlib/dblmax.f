cDECK DBLMAX
c
c **************************************************************
c **************************************************************
c
c THIS FUNCTION RETURNS THE MAXIMUM ALLOWABLE POSITIVE DOUBLE
c     PRECISION VALUE.
c
c THIS ROUTINE IS INSTALLATION DEPENDENT.
c
c VAX-11 FORTRAN VERSION.
c
      double precision function dblmax()
      double precision dmax
c
c RETURN THE MAXIMUM VALUE.
c
      parameter (dmax = 1.0e38)
c
      dblmax = dmax
c
c END OF DOUBLE PRECISION FUNCTION DBLMAX.
c
      return 
      end
