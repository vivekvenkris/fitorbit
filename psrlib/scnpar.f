cDECK SCNPAR
c
c ***********************************************************
c ***********************************************************
c
c HANDLES THE SCAN COMMAND.
c CMDINP CONTAIN THE COMMANDS GIVEN BY THE USER, WHERE NINP
c     ARE THEIR NUMBER AND JINP POINTS TO THE NEXT COMMAND TO BE
c     INTERPRETED.
c IDENT CONTAINS THE NAME OF THE PARAMETER LIST.
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
      subroutine scnpar(cmdinp, ninp, jinp, ident, parlst, pardsc, parid
     &, ipar, rpar, dpar, lpar, cpar, iiunits, irunits, idunits)
      character cmdinp(*)*(*), ident*(*), parlst(*)*(*), pardsc(*)*(*), 
     &parid(*)*(*), cpar(*)*(*)
      dimension ipar(*), rpar(*), iiunits(*), irunits(*), idunits(*)
      double precision dpar(*), pos
c
      logical lpar(*)
c
c     DECLARE EXTERNAL FUNCTIONS
c
      include 'PSRLIB.DEF'
c DEFINITIONS OF THE PULSAR LIBRARY GLOBALS
c
c     PROGRAM LOGICAL UNITS
c
      logical comstr
c
c     INTERNAL BUFFERS
c
      character cparam*81, prmtcc*1, uppcase*80
      parameter (maxpar = 20)
c
c     IF NO PARAMETERS ARE IN THE LIST, FLAG AN ERROR AND RETURN.
c
      character inbuf*80, outbuf*81, fmt*20, buf*25, cmdpar(maxpar)*40
      if (parlst(1) .eq. ' ') then
      ifail = 42
      call psrerr('SCNPAR', ifail, 0, 0.0, ' ')
      return 
c
c     THE START OF THE SCAN IS TO BE THE FIRST PARAMETER
c     UNLESS A 'FROM' CLAUSE HAS BEEN ENTERED.
c
      end if
c
c        STEP OVER 'FROM' IF SUPPLIED.
c
      if (jinp .le. ninp) then
c
c        CHECK THAT A PARAMETER WAS SUPPLIED.
c
      if (comstr('FROM',cmdinp(jinp))) jinp = jinp + 1
      if (jinp .gt. ninp) then
      ifail = 23
      call psrerr('SCNPAR', ifail, 0, 0.0, ' ')
      return 
c
c        LOOK FOR THE PARAMETER NAME IN THE PARAMETER LIST.
c
      end if
      call fndpar(cmdinp(jinp), parlst, parid, i, ifail)
      if (ifail .ne. 0) return 
      jinp = jinp + 1
c
c        NO 'FROM' WAS SUPPLIED, START AT THE BEGINNING OF THE LIST.
c
      else
      i = 1
c
c     CONSTRUCT THE PROMPT FORMAT
c
      end if
c
c     OUTPUT THE PARAMETER HEADING
c
      fmt = ('(A,'' ? '',' // prmtcc()) // ')'
      outbuf = (' Scan through ' // ident(1:length(ident))) // 
     &' parameters:'
      call outmon(outbuf)
      outbuf = ' Enter new value or return for each parameter :'
      call outmon(outbuf)
c
c     LABEL 10 IS THE START OF THE PARAMETER SCANNING LOOP
c
      call outmon(' ')
c
c        END THE SCAN WHEN THE PARAMETER LIST HAS RUN OUT.
c
   10 continue
c
c        PROMPT FOR A NEW VALUE, IF THE PARAMETER IS IN USE.
c
      if (parlst(i) .eq. ' ') goto 20
c
c           OBTAIN THE PARAMETER LINE FOR OUTPUT
c
      if (parid(i) .ne. '*') then
c
c           OUTPUT THE PARAMETER LINE, WITH ITS SECOND DESCRIPTION LINE
c           IF NON-EMPTY
c
      outbuf = cparam(' ',pardsc((2 * i) - 1),parid(i),ipar,rpar,dpar,
     &lpar,cpar,iiunits,irunits,idunits)
      if (pardsc(i * 2) .ne. ' ') then
      call outmon(outbuf)
      outbuf = ' ' // pardsc(i * 2)(1:length(pardsc(i * 2)))
      end if
c
c           READ NEW VALUE
c
      write(unit=lumon, fmt=fmt) outbuf(1:61)
c
c           CONVERT TO UPPERCASE
c
      read(unit=lucmd, fmt=999, end=20) inbuf
c
c           SPLIT INTO COMMAND PARAMETERS
c
      inbuf = uppcase(inbuf)
      call parse(inbuf, cmdpar, npar, maxpar, ' ')
c
c              IF A SINGLE PARAMETER OF '-' WAS ENTERED,
c              THEN A BACKSTEP IN THE PARAMETER LIST IS DESIRED.
c
      if (npar .gt. 0) then
c
c                 DECREMENT THE PARAMETER COUNTER
c
      if ((npar .eq. 1) .and. (cmdpar(1) .eq. '-')) then
c
c                 IF THE PREVIOUS PARAMETER IS NOT IN USE,
c                 SEARCH BACKWARDS FOR ONE THAT IS.
c
      i = max(1,i - 1)
      if (parid(i) .eq. '*') then
      inext = i
      do 25 j = i - 1, 1, -1
      if (parid(j) .ne. '*') then
      inext = max(1,j)
      goto 30
      end if
c
c                    LABEL 30 IS FOR EXIT FROM THE BACKWARD SEARCH LOOP.
c
   25 continue
   30 continue
      i = inext
c
c                 RETURN TO THE START OF THE LOOP
c
      end if
      goto 10
c
c              USE SETPAR TO RESET THE PARAMETER.
c
      end if
      jpar = 1
      call setpar(cmdpar, jpar, npar, ipar, rpar, dpar, lpar, cpar, 
     &parid(i), iiunits, irunits, idunits, 0.0d0, ifail)
      end if
c
c        IF AN ERROR OCCURRED, CLEAR THE ERROR FLAG AND LOOP
c        SO THAT THE SAME PARAMETER MAY BE SET.
c        OTHERWISE INCREMENT THE LOOP COUNTER.
c
      end if
      if (ifail .ne. 0) then
      ifail = 0
      else
      i = i + 1
      end if
c
c     LABEL 20 IS THE LOOP EXIT POINT
c
      goto 10
   20 continue
c
      call outmon(' ')
      return 
c
c END OF SUBROUTINE SCNPAR
c
  999 format(a80)
      end
