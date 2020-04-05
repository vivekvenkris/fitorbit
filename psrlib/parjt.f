cDECK PARJT
c
c
c **************************************************************
c **************************************************************
c
c PARSES STRING FOR A JULIAN TIME.
c FORMAT MAY BE  <DATE> <TIME>
c            OR  <TIME> <DATE>
c            OR  <REAL>
c            OR '<NONE>'
c WHERE <DATE> IS YY/MM/DD
c       <TIME> IS HH:MM:SS
c   AND <REAL> IS A VALID REAL NUMBER WHICH IS INTERPRETED AS
c                 (JULIAN DATE - 2400000.5) + UT IN DAYS.
c
c DJT IS THE JULIAN TIME IN THE AFOREMENTIONED FORM.
c IF ONLY <TIME> OR <DATE> IS SPECIFIED THE UNSPECIFIED DATE OR
c TIME IS RETAINED FROM THE SUPPLIED VALUE OF DJT.
c
c THE FUNCTION IS .TRUE. UNLESS AN ERROR WAS DETECTED IN THE
c SPECIFIED VALUES IN WHICH CASE DJT IS UNALTERED.
c
      logical function parjt(string, djt)
      character string*(*)
      character cmdpar(2)*40
      logical cmdjt, date, time
c
c     SPLIT STRING INTO MAXIMUM OF TWO PARAMS.
c
      double precision d, djt
c
c     USE CMDJT TO GET THE DATE/TIME VALUE.
c
      call parse(string, cmdpar, npar, 2, ' ')
      jpar = 1
      d = djt
c
c     IF THERE IS AN UNUSED PARAMETER, IT IS BECAUSE IT WAS FAULTY.
c
      parjt = cmdjt(cmdpar,npar,jpar,date,time,d)
      if (jpar .le. npar) then
      parjt = .false.
c
c        INTERPRETATION SUCCESSFUL, RETURN THE NEW VALUE.
c
      else
      djt = d
c
      end if
c
c END OF LOGICAL FUNCTION PARJT.
c
      return 
      end
