cDECK LUFREE
c
c
c **************************************************************
c **************************************************************
c
c RETURNS A FREE LOGICAL UNIT NUMBER FROM THOSE SUPPORTED BY THE
c     LIBRARY FILE HANDLING ROUTINES.
c IF NONE IS AVALIABLE, -1 IS RETURNED AND IFAIL = 59,
c     OTHERWISE IFAIL = 0
c
c Mod 10/03/2006 Correct data type in call to pserr
c
      integer function lufree(ifail)
      include 'PSRLIB.DEF'
c DEFINITIONS OF THE PULSAR LIBRARY GLOBALS
c
c     PROGRAM LOGICAL UNITS
c
      integer ifail,i,ios
      logical op
c
c     SEARCH FOR AN UNASSIGNED LU, UP TO THE MAXIMUM MAXLU.
c
      ifail = 0
      do 10 i = 7, maxlu
      inquire(unit=i, opened=op, iostat=ios) 
      if (ios .eq. 0) then
      if (.not. op) goto 100
      end if
c
c     FREE LU NOT FOUND.
c
   10 continue
      lufree = -1
      ifail = 59
      call psrerr('LUFREE', ifail, 0, 0., ' ')
c
c     FREE LU FOUND.
c
      return 
  100 continue
c
      lufree = i
c
c     END OF INTEGER FUNCTION LUFREE
c
      return 
      end
