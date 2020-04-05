*DECK LPARSE
C
C
C
      SUBROUTINE LPARSE(PARS,NPAR,IP,FNAME,PRINT,QUIET,IPAR,IFAIL)
C
C THIS ROUTINE PARSES LIST AND SIMILAR COMMANDS.  IT STARTS AT PARS(IP)
C AND ON EXIT FNAME IS THE LISTING FILE NAME (FILE = ), PRINT IS .TRUE.
C IF PRINT WAS SPECIFIED, AND QUIET IS .TRUE. IF QUIET WAS SPECIFIED.
C ON EXIT IPAR CONTAINS ZERO, BUT IF AN ERROR IS DETECTED IT IS EITHER
C THE FAULTY PARAMETER NUMBER, OR THE MAXIMUM NUMBER OF PARAMETERS
C ALLOWED PER FILE NAME IF IT WAS TOO LONG.  ON EXIT IP POINTS TO THE
C LAST PARAMETER PROCESSED.
C     VERSION 1.0   10MAR82
C
      CHARACTER*(*) PARS(*),FNAME
      LOGICAL PRINT,QUIET,BATCH,COMSTR
C
C TEST THE NUMBER OF PARAMETERS REMAINING.
C
      IF(IP.LE.NPAR) THEN
C
C CLEAR THE FAILURE INDICATOR.
C
        IPAR=0
        IFAIL=0
C
C NOW SCAN THROUGH THE REMAINING PARAMETERS.
C
 1000   if(ip.le.npar) then
c
c there are some, check the current one.
c
          if(comstr('print',pars(ip))) then
c
c it is 'print', set the print flag.
c
            print=.true.
          elseif(comstr('quiet',pars(ip))) then
c
c it is 'quiet', set the quiet flag.
c
            quiet=.true.
          elseif(comstr('file',pars(ip))) then
c
c it is file, is there another parameter?
c
            if(ip.lt.npar) then
C
C YES, IS IT '='?
C
              IP=IP+1
              IF(PARS(IP).EQ.'=') THEN
C
C IT IS, TEST THAT THERE IS ANOTHER PARAMETER?
C
                IF(IP.LT.NPAR) THEN
C
C THERE IS, SET IT AS THE FILE NAME.
C
                  IP=IP+1
                  CALL CHKFIL(PARS,IP,FNAME,IPAR,IFAIL)
                ELSE
C
C ILLEGAL '=' ERROR, VALUE MISSING.
C
                  CALL LIBERR('LPARSE',9,0,0,0.0,' ',IFAIL)
                  IPAR=IP
                ENDIF
              ELSE
C
C ILLEGAL SEPARATOR, FLAG AN ERROR.
C
                CALL LIBERR('LPARSE',10,1,0,0.0,PARS(IP),IFAIL)
                IPAR=IP
              ENDIF
            ELSE
C
C NO '=' SPECIFIED, FLAG AN ERROR.
C
              CALL LIBERR('LPARSE',14,2,0,0.0,PARS(IP),IFAIL)
              IPAR=IP
            ENDIF
          ELSE
C
C THE CURRENT PARAMETER IS SOMETHING ELSE, FLAG AN ERROR.
C
            CALL LIBERR('LPARSE',22,3,0,0.0,PARS(IP),IFAIL)
            IPAR=IP
          ENDIF
C
C TRY THE NEXT PARAMETER IF THERE HAVE BEEN NO ERRORS.
C
          IF(IFAIL.EQ.0) THEN
            IP=IP+1
            GOTO 1000
          ENDIF
        ENDIF
      ELSE
C
C NO MORE PARAMETERS LEFT, FLAG AN ERROR.
C
        CALL LIBERR('LPARSE',23,4,0,0.0,' ',IFAIL)
        IPAR=0
      ENDIF
      RETURN
C
C END OF SUBROUTINE LPARSE.
C
      END

