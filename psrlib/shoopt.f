cDECK SHOOPT
c
c **************************************************************
c **************************************************************
c
c HANDLES THE SHOW OPTIONS COMMAND.
c CMDINP CONTAIN THE COMMANDS GIVEN BY THE USER, WHERE NINP
c     ARE THEIR NUMBER AND JINP POINTS TO THE NEXT COMMAND TO BE
c     INTERPRETED.
c PARLST CONTAINS THE LIST OF PARAMETERS.
c PARID CONTAINS THE LISTS OF PARAMETER IDENTIFIERS AS REQUIRED BY THE
c     PARAMETER HANDLING ROUTINES.THERE MUST BE A PARAMETER IDENTIFIER
c     FOR EACH PARAMETER IN THE LIST.
c IPAR,RPAR,LPAR,DPAR,CPAR,IIUNITS,IRUNITS,IDUNITS
c     ARE THE PARAMETER LISTS AND THEIR UNITS. THEIR USE IS DESCRIBED
c     IN THE PARAMETER HANDLING ROUTINES.
c IFAIL IS ZERO UNLESS AN ERROR OCCURRED.
c 
      subroutine shoopt(cmdinp, ninp, jinp, parlst, parid, ipar, rpar, 
     &dpar, lpar, cpar, iiunits, irunits, idunits, ifail)
      character cmdinp(*)*(*), parlst(*)*(*), cpar(*)*(*), parid(*)*(*)
     &, outbuf*81
      integer ipar(*), iiunits(*), irunits(*), idunits(*)
      real rpar(*)
      double precision dpar(*)
c
c     CLEAR THE ERROR FLAG.
c
      logical lpar(*), comstr
c
c     IF 'FOR' IS PRESENT, STEP OVER IT.
c
      ifail = 0
      if ((jinp .le. ninp) .and. comstr('FOR',cmdinp(jinp))) then
      jinp = jinp + 1
c
c     CHECK FOR NO INPUT COMMANDS.
c
      end if
      if (jinp .gt. ninp) then
      ifail = 23
      call psrerr('SHOOPT', ifail, 0, 0.0, ' ')
      return 
c
c     LOOK FOR THE REQUESTED PARAMETER IN THE LIST.
c
      end if
      call fndpar(cmdinp(jinp), parlst, parid, ip, ifail)
c
c     IF THE PARAMETER IS NOT AN OPTION, SAY SO.
c
      if (ifail .ne. 0) return 
      if (parid(ip)(1:1) .ne. 'O') then
      ifail = 15
      call psrerr('SHOOPT', ifail, 0, 0.0, cmdinp(jinp))
      return 
c
c     EXTRACT THE POSITION FROM THE IDENTIFIER.
c
      end if
c
c     PRODUCE THE DISPLAY.
c
      read(unit=parid(ip), fmt='(1X,I2)') iposn
c
c     UPDATE COMMAND POINTER.
c
      call sholst(cpar(ipar(iposn + 1)), ('List of options for ' // 
     &parlst(ip)(1:length(parlst(ip)))) // ' :')
c
      jinp = jinp + 1
c
c END OF SUBROUTINE SHOOPT.
c
      return 
      end
