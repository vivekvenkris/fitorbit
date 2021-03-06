*DECK CHKYMD
C
C
C
      SUBROUTINE CHKYMD(PAR,LP,DATE,IDATE)
C
C THIS ROUTINE CHECKS IF THE DATE IN PAR IS A VALID DATE IN THE
C FORMAT YY/MM/DD.  IF IT IS DATE IS RETURNED AS .TRUE. AND IDATE
C CONTAINS THE DATE AS YYMMDD ON EXIT, OTHERWISE THESE VALUES ARE
C NOT CHANGED.  ON ENTRY LP SHOULD BE THE EFFECTIVE LENGTH OF PAR.
C
      CHARACTER PAR*(*),STR(3)*2
      LOGICAL DATE
C
C SET THE CONTENTS OF STR TO SOMETHING SILLY TO FORCE AN ERROR
C IF AN ELEMENT IS NOT SET.
C
      DO 1 I=1,3
    1 STR(I)='?'
      J=1
      IC=0
C
C SCAN THROUGH PAR AND ATTEMPT TO LOAD ITS COMPONENT BITS INTO STR.
C
      DO 2 I=1,LP
        IF(PAR(I:I).EQ.'/') THEN
C
C '/' ENCOUNTERED, INCREMENT THE YMD COUNTER.
C
          J=J+1
          IF(J.GT.3) THEN
C
C TOO MANY '/''S, THERFORE NOT A TIME.
C
            RETURN
          ENDIF
C
C CLEAR THE CHARACTER COUNTER.
C
          IC=0
        ELSEIF(PAR(I:I).NE.' '.AND.(IC.NE.0.OR.PAR(I:I).NE.'+')) THEN
C
C CHARACTER OTHER THAN A SPACE OR '+', LOAD IT INTO STR.
C
          IC=IC+1
          IF(IC.GT.2) THEN
C
C STRING IS TOO LONG, NOT A DATE.
C
            RETURN
          ENDIF
C
C LOAD INTO STR.
C
          STR(J)(IC:IC)=PAR(I:I)
        ENDIF
    2 CONTINUE
C
C NOW ATTEMPT TO READ STR AS INTEGERS.
C
      READ(STR,800,ERR=100,IOSTAT=IFAIL) I,J,K
C
C READ SUCCESSFUL, CHECK THE VALUES OF THE DAY, MONTH, AND YEAR.
C
      CALL CHKDMY(K,J,I,DATE,IDATE)
      RETURN
C
C READ UNSUCCESSFUL.
C
  100 CONTINUE
      RETURN
C
C FORMAT STATEMENT.
C
  800 FORMAT(BN,3(I2:/))
C
C END OF SUBROUTINE CHKYMD.
C
      END
