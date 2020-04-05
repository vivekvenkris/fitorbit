c
c Returns the current time as hhmmss.
c
c Argument:
c  ATIME   Output  INTEGER  The current time in the form hhmmss.
c
c     Version 1.2    19th November, 1986   Alliant FX-FORTRAN.
c
c Declare the routine's argument .
c
      subroutine oltime(atime)
c
c Declare local variables.
c
      integer atime
c
c Get the system time.
c
      call curtim(atime)
      end
