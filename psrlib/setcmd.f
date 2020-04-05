cDECK SETCMD
c
c **************************************************************
c **************************************************************
c
c HANDLES THE SET COMMAND.
c CMDINP CONTAIN THE COMMANDS GIVEN BY THE USER, WHERE NINP
c     ARE THEIR NUMBER AND JINP POINTS TO THE NEXT COMMAND TO BE
c     INTERPRETED.
c PARLST CONTAINS THE LIST OF PARAMETERS AND PARDSC THEIR DESCRIPTIONS,
c     WITH TWO DESCRIPTION LINES PER PARAMETER.
c PARID CONTAINS THE LISTS OF PARAMETER IDENTIFIERS AS REQUIRED BY THE
c     PARAMETER HANDLING ROUTINES.THERE MUST BE A PARAMETER IDENTIFIER
c     FOR EACH PARAMETER IN THE LIST.
c IPAR,RPAR,LPAR,DPAR,CPAR,IIUNITS,IRUNITS,IDUNITS
c     ARE THE PARAMETER LISTS AND THEIR UNITS. THEIR USE IS DESCRIBED
c     IN THE PARAMETER HANDLING ROUTINES.
c IFAIL IS ZERO UNLESS AN ERROR OCCURRED.
c 
      subroutine setcmd(cmdinp, ninp, jinp, parlst, pardsc, parid, ipar
     &, rpar, dpar, lpar, cpar, iiunits, irunits, idunits, ifail)
      character cmdinp(*)*(*), parlst(*)*(*), pardsc(*)*(*), cpar(*)*(*)
     &, parid(*)*(*), ptype*1
      integer ipar(*), iiunits(*), irunits(*), idunits(*), ip, ip2, nn1
     &, nn2
      real rpar(*)
      double precision dpar(*)
c
c     CLEAR THE ERROR FLAG.
c
      logical lpar(*), comstr2, param
c
c     CHECK FOR NO INPUT COMMANDS.
c
      ifail = 0
      if (jinp .gt. ninp) then
      ifail = 23
      call psrerr('SETCMD', ifail, 0, 0.0, ' ')
      return 
c
c     LOOK FOR THE PARAMETER IN THE PARAMETER LIST.
c
      end if
      call fndpar(cmdinp(jinp), parlst, parid, ip, ifail)
c
c     INCREMENT THE COMMAND INPUT POINTER
c     AND CHECK THE NUMBER OF COMMANDS.
c
      if (ifail .ne. 0) return 
      jinp = jinp + 1
      if (jinp .gt. ninp) then
      ifail = 23
      call psrerr('SETCMD', ifail, 0, 0.0, ' ')
      return 
c
c     IF 'TO' OR '=' WAS GIVEN, STEP OVER IT.
c
      end if
      if (comstr2('to',2,cmdinp(jinp),length(cmdinp(jinp)))
     &    .or. (cmdinp(jinp)(1:1) .eq. '=')) then
c
c        CHECK THE NUMBER OF COMMANDS AGAIN.
c
      jinp = jinp + 1
      if (jinp .gt. ninp) then
      ifail = 23
      call psrerr('SETCMD', ifail, 0, 0.0, ' ')
      return 
      end if
c
c     SET THE PARAMETER.
c
c     check if trying to do a set of one parameter to another
      end if
      if (cmdinp(jinp)(1:1) .eq. '@') then
      call fndpar(cmdinp(jinp)(2:), parlst, parid, ip2, ifail)
      end if
      if ((ifail .eq. 0) .and. (cmdinp(jinp)(1:1) .eq. '@')) then
      cmdinp(jinp) = cmdinp(jinp)(2:)
      jinp = jinp + 1
      if (parid(ip)(1:1) .eq. parid(ip2)(1:1)) then
      ptype = parid(ip)(1:1)
      read(unit=parid(ip), fmt='(X,I2)') nn1
      read(unit=parid(ip2), fmt='(X,I2)') nn2
      if (ptype .eq. 'I') then
      ipar(nn1) = ipar(nn2)
      else if (ptype .eq. 'R') then
      rpar(nn1) = rpar(nn2)
      else if (((ptype .eq. 'D') .or. (ptype .eq. 'P')) .or. (ptype
     & .eq. 'T')) then
      dpar(nn1) = dpar(nn2)
      else if ((ptype .eq. 'C') .or. (ptype .eq. 'F')) then
      cpar(nn1) = cpar(nn2)
      else if (ptype .eq. 'W') then
      lpar(nn1) = lpar(nn2)
      else if (ptype .eq. 'L') then
      if (ipar(nn1) .le. ipar(nn2)) then
      do i = 1, ipar(nn2 + 1) + 1
      ipar(nn1 + i) = ipar(nn2 + i)
      end do
      else
      call outmon(' too many values in list')
      jinp = ninp + 1
      end if
      else if (ptype .eq. 'S') then
      if (rpar(nn1) .le. rpar(nn2)) then
      do i = 1, int(rpar(nn2 + 1)) + 1
      rpar(nn1 + i) = rpar(nn2 + i)
      end do
      else
      call outmon(' too many values in list')
      jinp = ninp + 1
      end if
      end if
      else
      call outmon(' parameters incompatible')
      ifail = 1
      end if
      else
      ifail = 0
      call setpar(cmdinp, jinp, ninp, ipar, rpar, dpar, lpar, cpar, 
     &parid(ip), iiunits, irunits, idunits, -1.0d0, ifail)
c
      end if
c
c END OF SUBROUTINE SETCMD.
c
      return 
      end





