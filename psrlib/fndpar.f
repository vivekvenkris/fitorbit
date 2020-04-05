cDECK FNDPAR
c
c **************************************************************
c **************************************************************
c
c ATTEMPTS TO FIND STRING AMONG THE LIST OF PATAMETERS IN PARLST.
c PARID CONTAINS THE LISTS OF PARAMETER IDENTIFIERS AS REQUIRED BY THE
c     PARAMETER HANDLING ROUTINES.THERE MUST BE A PARAMETER IDENTIFIER
c     FOR EACH PARAMETER IN THE LIST.
c IF SUCCESSFUL, IPAR CONTAINS THE PARAMETER NUMBER.
c IFAIL IS ZERO UNLESS AN ERROR OCCURRED.
c 
c Mod 10/03/2006 Correct data type in call to pserr
c
      subroutine fndpar(string, parlst, parid, ipar, ifail)
      character string*(*), parlst(*)*(*), parid(*)*(*)
      integer length
c
c     CLEAR THE ERROR FLAG.
c
      logical comstr2, param
c
c     LOOK FOR THE PARAMETER NAME.
c
      ifail = 0
      param = .false.
      i = 0
c
c     LABEL 10 IS THE START OF THE PARAMETER SEARCH LOOP.
c
      ip = 0
   10 continue
c
c        IF THE NAME IS EMPTY, THEN THAT IS THE END OF THE LIST.
c
      i = i + 1
c
c        IF THE PARAMETER ID IS '*' THEN IGNORE IT.
c
      if (parlst(i) .eq. ' ') goto 20
c
c           SET PARAM, BECAUSE THE LIST DOES CONTAIN A PARAMETER.
c
      if (parid(i) .ne. '*') then
c
c           CHECK AGAINST THE INPUT COMMAND.
c
      param = .true.
c
c              IF IP WAS ALREADY SET THEN THE PARAMETER HAS
c              BEEN RECOGNIZED TWICE AND IS AMBIGUOUS.
c
      if (comstr2(parlst(i),length(parlst(i)),string,
     &    length(string))) then
      if (ip .gt. 0) then
      ifail = 6
      call psrerr('FNDPAR', ifail, 0, 0., string)
      return 
c
c                 SET IP.
c
      else
      ip = i
      end if
      end if
c
c     LOOP.
c
      end if
c
c     LABEL 20 IS THE EXIT POINT FROM THE LOOP.
c
      goto 10
c
c     IF PARAM WAS NOT SET, THE PARAMETER LIST IS EMPTY.
c
   20 continue
      if (.not. param) then
      ifail = 42
      call psrerr('FNDPAR', ifail, 0, 0.0, ' ')
      return 
c
c     IF IP IS STILL NOT SET THEN THE COMMAND WAS NOT FOUND.
c
      end if
      if (ip .eq. 0) then
      ifail = 18
      call psrerr('FNDPAR', ifail, 0, 0., string)
      return 
c
c     PARAMETER FOUND, RETURN ITS POSITION.
c
      end if
c
      ipar = ip
c
c END OF SUBROUTINE FNDPAR.
c
      return 
      end
