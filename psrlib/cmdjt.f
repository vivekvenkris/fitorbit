cDECK CMDJT
c
c
c ****************************************************************
c ****************************************************************
c
c PARSES CMD(ICMD) TO CMD(NCMD) FOR A JULIAN TIME.
c FORMAT MAY BE  <DATE> <TIME>
c            OR  <TIME> <DATE>
c            OR  <REAL> <TIME>
c            OR  <REAL>
c WHERE <DATE> IS YY/MM/DD
c       <TIME> IS HH:MM:SS
c   AND <REAL> IS A VALID REAL NUMBER WHICH IS INTERPRETED AS
c                 (JULIAN DATE - 2400000.5) + UT IN DAYS
c
c IF A DATE WAS SPECIFIED, THEN DATE IS .TRUE.
c IF A TIME WAS SPECIFIED, THEN TIME IS .TRUE.
c IF A JULIAN DATE IS SPECIFIED, DATE AND TIME ARE BOTH .TRUE.
c DJT IS THE JULIAN TIME IN THE AFOREMENTIONED FORM.
c IF ONLY <TIME> OR <DATE> IS SPECIFIED THE UNSPECIFIED DATE OR
c     TIME IS RETAINED FROM THE SUPPLIED VALUE OF DJT.
c
c THE FUNCTION IS .TRUE. UNLESS AN ERROR WAS DETECTED IN THE
c     SPECIFIED VALUES IN WHICH CASE DJT IS UNALTERED.
c
      logical function cmdjt(cmd, ncmd, icmd, date, time, djt)
      character cmd(*)*(*)
      logical date, time
c
c     DECLARE EXTERNAL ROUTINES.
c
      double precision djt, dval, tim, dte, datejd
c
c     SET RETURN FLAG AND DATE/TIME FLAGS TO FALSE.
c
      logical partim, pardat, pardbl
      cmdjt = .false.
      date = .false.
c
c     RETURN IF NO PARAMETERS WERE PROVIDED.
c
      time = .false.
c
c     SPLIT DJT INTO DATE AND TIME.
c
      if (icmd .gt. ncmd) return 
      if (djt .gt. 0.0) then
      dte = int(djt)
      tim = mod(djt,1.d0) * 86400.0
      else
      dte = 0
      tim = 0
c
c     EXAMINE FIRST PARAMETER.
c
      end if
c
c        DATE FOUND.
c
      if (pardat(cmd(icmd),idte)) then
      dte = datejd(idte)
      date = .true.
c
c        TIME FOUND.
c
      else if (partim(cmd(icmd),tim)) then
      time = .true.
c
c        REAL JULIAN TIME FOUND.
c
      else if (pardbl(cmd(icmd),dval)) then
      dte = dval
      tim = 0
      time = .true.
      date = .true.
c
c        '<NONE>' INDICATES FLAGGED OFF TIME.
c
      else if (cmd(icmd)(1:1) .eq. '<') then
      djt = -1.0
c
c        NOT RECOGNIZED, RETURN.
c
      else
      return 
c
c     INCREMENT COMMAND POINTER.
c
      end if
c
c     IF A DATE OR A TIME WAS FOUND EXAMINE SECOND COMMAND, IF PRESENT,
c     FOR A TIME OR A DATE.
c
      icmd = icmd + 1
      if ((date .or. time) .and. (icmd .le. ncmd)) then
c
c           DATE FOUND, INCREMENT THE COMMAND POINTER.
c
      if (pardat(cmd(icmd),idte)) then
      dte = datejd(idte)
      date = .true.
      icmd = icmd + 1
c
c           TIME FOUND.
c
      else if (partim(cmd(icmd),tim)) then
      time = .true.
      icmd = icmd + 1
      end if
c
c     IF DATE OR TIME WAS FOUND, REFORM DJT.
c
      end if
      if (date .or. time) then
      djt = dte + (tim / 86400.0d0)
c
c     SUCCESS.
c
      end if
c
      cmdjt = .true.
c
c END OF LOGICAL FUNCTION CMDJT
c
      return 
      end
