c
c Returns the maximum possible usable positive integer value.
c     In this generic version, 32 bit integers are assumed.  It does
c not really matter if the maximum positive integer value is really
c greater than this implies.
c
c     Version 1.1    8th September, 1986   Generic
c
c Define the largest usable integer.
c
      integer function maxint()
      integer maxi
c
c Set MAXINT.
c
      parameter (maxi = 2147483647)
c
c End of INTEGER function MAXINT.
c
      maxint = maxi
      end
