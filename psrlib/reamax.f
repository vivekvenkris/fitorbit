cDECK REAMAX
c
c
c
c
c This function returns the maximum allowable positive single precision
c value.  In this generic version, it is assumed that one byte is used
c for the exponent.  It does not matter if the value is too small.
c     Version 1.1   10th September, 1986   Generic
c
c Define the value to be returned.
c
      real function reamax()
      real rmax
c
c Return the maximum value.
c
      parameter (rmax = 1.0e38)
c
c End of REAL function REAMAX.
c
      reamax = rmax
      end
