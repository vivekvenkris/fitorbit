c
c Returns the current date as an integer coded as yymmdd.
c
c Argument:
c  ID      Output  INTEGER  The current date in the form yymmdd.
c
c     This is installation dependent.
c     Version 1.2    19th November, 1986   Alliant FX-FORTRAN.
c
c Declare the routine's argument:
c
      subroutine oldate(id)
c
c Declare local variables.
c
      integer id
c
c Get the system date in the form day month year
c
      integer array(3)
c
c Convert to appropriate format.
c
      call idate(array)
c
c End of subroutine OLDATE.
c
      id = (array(1) + (100 * array(2))) + (10000 * mod(array(3),100))
      end
