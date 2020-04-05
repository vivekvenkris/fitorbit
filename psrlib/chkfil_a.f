cDECK CHKFIL
c
c
c
c
c THIS ROUTINE CHECKS THE FILE NAME IN PARS(I) AND IF VALID LOADS IT
c INTO NAME.  ON EXIT IPAR CONTAINS ZERO IF NO ERRORS WERE FOUND, BUT
c IS EQUAL TO THE MAXIMUM NUMBER OF CHARACTERS ALLOWED IF THE NAME IS
c TOO LONG, OR THE PARAMETER NUMBER IF THE NAME IS BLANK.
c      VERSION 1.1   11OCT82
c
c Alliant fortran version
      subroutine chkfil(pars, i, name, ipar, ifail)
      character pars(*)*(*), name*(*), def*(*)
      parameter (def = 'DEFAULT')
c
c CHECK THE SUPPLIED NAME.
c
      logical comstr
c
c IT IS NOT BLANK, IS IT 'DEFAULT'?
c
      if (pars(i) .ne. ' ') then
c
c IT IS, LOAD THE DEFAULT NAME.
c
      if (comstr(def,pars(i))) then
      name = ' '
      ipar = 0
      ifail = 0
c
c IT IS NOT, OBTAIN THE MAXIMUM FILE NAME LENGTH ALLOWED.
c
      else
c
c CHECK THE SUPPLIED LENGTH.
c
      maxfl = min(len(name),maxfil())
c
c IT IS WITHIN RANGE, LOAD THE FILE NAME (CHECKING FOR ENVIRONMENT VARIA
cBLE)
c
      if (length(pars(i)) .le. maxfl) then
      call getenv(pars(i), name)
      if (name .eq. ' ') then
      name = pars(i)
      end if
      ipar = 0
      ifail = 0
c
c THE NAME IS TOO LONG, FLAG AN ERROR.
c
      else
      call liberr('CHKFIL', 11, 0, maxfl, 0.0, ' ', ifail)
      ipar = maxfl
      name = ' '
      end if
      end if
c
c THE SUPPLIED NAME IS BLANK, FLAG AN ERROR.
c
      else
      call liberr('CHKFIL', 12, 1, 0, 0.0, ' ', ifail)
      ipar = i
      name = ' '
      end if
c
c END OF SUBROUTINE CHKFIL.
c
      return 
      end
