*DECK SHOSYM
C
C
C
      SUBROUTINE SHOSYM(PARS,NPAR,IP,OUTBUF,IFAIL)
C
C THIS ROUTINE HANDLES THE SYMBOLS OPTION ON A SHOW COMMAND.
C INPUT ARGUMENTS:
C  PARS     - THE PARAMETERS FROM THE COMMAND LINE.
C  NPAR     - THE NUMBER OF THESE.
C  IP       - THE LOCATION IN PARS OF THE 'SYMBOLS' OPTION.
C  OUTBUF   - A CHARACTER WORK SPACE ARRAY, THIS MUST BE SUFFICIENT TO
C             HOLD ONE LINE OF OUTPUT.
C OUTPUT ARGUMENTS:
C  IP       - POINTS TO THE LAST PARAMETER PROCESSED.
C  IFAIL    - ZERO IF SUCCESSFUL, ELSE > 0.
C     VERSION 1.0   15MAY85
C
      CHARACTER*(*) PARS(*),OUTBUF,ALL
      PARAMETER (ALL='ALL')
      LOGICAL COMSTR
C
C IS A SYMBOL NAME PROVIDED?
C
      CALL GETIDC(PARS,NPAR,IP,IFAIL)
      IF(IFAIL.EQ.0) THEN
C
C YES, MIGHT IT BE ALL?
C
        IF(COMSTR(ALL,PARS(IP))) THEN
C
C YES, THIS IS TREATED AS FOLLOWS: IF THE SYMBOL NAME AS SUPPLIED EXISTS
C IT ALONE IS SHOWN; IF IT DOES NOT EXIST THEN ALL SYMBOLS ARE SHOWN.
C FIRST TURN OFF ERROR 74 (SYMBOL DOES NOT EXIST) AFTER NOTING ITS
C CURRENT STATUS.
C
          CALL GETLER(74,I)
          CALL SETLER(74,2)
C
C NOW ATTEMPT TO SHOW THIS SYMBOL.
C
          CALL SYMSHO(PARS(IP),OUTBUF,IFAIL)
C
C RESTORE THE PREVIOUS STATUS OF ERROR 74.
C
          CALL SETLER(74,3-I)
C
C DID THE SYMBOL EXIST?
C
          IF(IFAIL.EQ.74) THEN
C
C NO, SHOW ALL SYMBOLS INSTEAD.
C
            CALL SYMLST(OUTBUF)
            IFAIL=0
          ENDIF
        ELSE
C
C THE SYMBOL CANNOT BE CONFUSED WITH 'ALL', JUST ATTEMPT TO SHOW IT.
C
          CALL SYMSHO(PARS(IP),OUTBUF,IFAIL)
        ENDIF
      ELSEIF(IFAIL.LT.0) THEN
C
C NO SYMBOL NAME IS SUPPLIED, SHOW ALL SYMBOLS.
C
        CALL SYMLST(OUTBUF)
        IFAIL=0
      ENDIF
      RETURN
C
C END OF SUBROUTINE SHOSYM.
C
      END
