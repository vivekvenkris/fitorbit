cDECK TERMINL
c
c **************************************************************
c **************************************************************
c
c IF LU IS POSITIVE INSPECTS LOGICAL UNIT LU, OTHERWISE INSPECTS
c     LOGICAL NAME LOGNAM.
c RETURNS THE VALUE .TRUE. IF AND ONLY IF LOGICAL UNIT OR NAME
c     IS ASSIGNED TO A TERMINAL DEVICE.
c IF POSSIBLE, ALSO RETURNS THE PAGE LENGTH AND RECORD LENGTH
c     FOR THE DEVICE IN LPAG AND LREC RESPECTIVELY.
c NOTE THAT THE LOGICAL UNIT MUST HAVE BEEN OPENED PREVIOUS TO
c     CALLING THIS ROUTINE.
c
c Paul Harrison 10-NOV-1987 I am sure that this subroutine works
c for the case LU=0 and LOGNAM=SYS$INPUT or SYS$OUTPUT
c BUT I am not convinced that it will work if called with LU<>0
c ALLIANT FORTRAN 
c Mod 10/03/2006 Correct data type in call to pserr
c
      logical function terminl(lu, lognam, lpag, lrec)
      character lognam*(*)
      logical isatty
c
c     CLEAR FUNCTION VALUE AND LOGICAL NAMES.
c
      integer*4 lpag, lrec, lu, lutemp
      terminl = .false.
      logfil = ' '
c
      logtt = ' '
c
c        FIND OUT THE STATUS OF LOGICAL UNIT LU.
c
      if (lu .gt. 0) then
      terminl = isatty(lu)
c
c        USE LOGICAL NAME INSTEAD OF THE FILENAME.
c
      else
      if (lognam .eq. 'SYS$INPUT') then
      lutemp = 5
      else if (lognam .eq. 'SYS$OUTPUT') then
      lutemp = 6
      else
      inquire(file=lognam, number=lutemp, err=9999) 
      end if
      terminl = isatty(lutemp)
c
      end if
      if (terminl) then
      lpag = 24
      lrec = 80
      else
      lpag = 66
      lrec = 132
      end if
c
c     LABEL 9999 IS THE EXIT POINT FOR ERROR IN INQUIRE.
c
      return 
 9999 continue
      call psrerr('TERMINL', 49, istat, 0., file)
c
c END OF LOGICAL FUNCTION TERMINL
c
      return 
      end
