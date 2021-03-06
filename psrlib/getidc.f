*DECK GETIDC
C
C
C
      SUBROUTINE GETIDC(PARS,NPAR,IP,IFAIL)
C
C THIS ROUTINE RETURNS THE OPTION SPECIFIED BY USE OF THE IDENTIFIER IN
C PARS(IP).  ON EXIT IP POINTS TO THE LAST PARAMETER PROCESSED ( = THE
C OPTION IF IFAIL = 0); AND IFAIL IS 0 IF THE OPTION WAS SUCCESSFULLY
C OBTAINED, -1 IF NO OPTION WAS SPECIFIED, OR IS GREATER THAN 0 IF AN
C ERROR WAS DETECTED.
C     VERSION 1.0    1JUL83
C
      CHARACTER*(*) PARS(*),SEP,ROUTN
      PARAMETER (SEP='=',ROUTN='GETIDC')
C
C TEST IF THERE ARE ANY MORE PARAMETERS.
C
      IF(IP.LT.NPAR) THEN
C
C YES, IS THE NEXT ONE A SEPARATOR?
C
        IF(PARS(IP+1).EQ.SEP) THEN
C
C IT IS, CHECK THAT THERE IS ANOTHER PARAMETER.
C
          IP=IP+1
          IF(IP.LT.NPAR) THEN
C
C THERE IS, RETURN ITS POSITION.
C
            IP=IP+1
            IFAIL=0
          ELSE
C
C THERE IS NO VALUE FOLLOWING THE SEPARATOR, MONITOR AN ERROR.
C
            CALL LIBERR(ROUTN,9,0,0,0.0,' ',IFAIL)
          ENDIF
        ELSE
C
C THE NEXT VALUE IS NOT A SEPARATOR, RETURN NO OPTION.
C
          IFAIL=-1
        ENDIF
      ELSE
C
C THERE ARE NO MORE PARAMETERS AFTER THE IDENTIFIER, RETURN NO OPTION.
C
        IFAIL=-1
      ENDIF
      RETURN
C
C END OF SUBROUTINE GETIDC.
C
      END
