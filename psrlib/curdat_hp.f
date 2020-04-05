cDECK CURDAT
c
c
c
c
c RETURNS THE CURRENT DATE AS YYMMDD.  THIS ROUTINE IS INSTALLATION
c DEPENDENT.
c    Paul Harrison 4Jan90 VERSION 1.0    9NOV81     Alliant FX/FORTRAN
c   
c   
      subroutine curdat(id)
c FORM ID.
c   
      integer date(3), id
      call idate(date(2),date(1),date(3))
      id = ((100 * date(2)) + date(1)) + (10000 * mod(date(3),100))
c   
c END OF SUBROUTINE CURDAT.
c
      return 
      end
