*DECK PARDAT
C
C
C
      LOGICAL FUNCTION PARDAT(PAR,IDATE)
C
C TRYS TO INTERPRET THE PARAMETER STRING PAR AS A DATE.  RETURNS THE
C VALUE .TRUE. IF SUCCESSFUL AND .FALSE. OTHERWISE.  ON EXIT IDATE
C CONTAINS THE DATE AS YYMMDD IF PARDAT IS .TRUE. AND IS UNCHANGED OTHERWISE.
C
      CHARACTER PAR*(*),DIGITS*(*),MLIST(13)*9
      LOGICAL PARINT
      PARAMETER (DIGITS='0123456789+')
      SAVE MLIST
      DATA MLIST/'JANUARY','FEBRUARY','MARCH','APRIL','MAY','JUNE',
     &           'JULY','AUGUST','SEPTEMBER','OCTOBER','NOVEMBER',
     &           'DECEMBER',' '/
C
C SET THE INITIAL VALUES.
C
      PARDAT=.FALSE.
      if (par(1:1).eq.'<') return
C
C REMOVE ANY EMBEDDED SPACES IN THE PARAMETER.
C
      LP=LENGTH(PAR)
      I=1
 1000 IF(I.LT.LP) THEN
        IF(PAR(I:I).EQ.' ') THEN
C
C REMOVE THIS SPACE.
C
          LP=LP-1
          DO 1 J=I,LP
            PAR(J:J)=PAR(J+1:J+1)
    1     CONTINUE
        ELSE
C
C TEST THE NEXT CHARACTER.
C
          I=I+1
        ENDIF
        GOTO 1000
      ENDIF
C
C NOW ATTEMPT TO INTERPRET THE DATE IN DAY_MONTH_YEAR FORMAT.
C FIRST TRY TO LOCATE THE START OF THE MONTH.
C
      DO 2 I=2,LP
C
C IS THE CURRENT CHARACTER A DIGIT?
C
        IF(INDEX(DIGITS,PAR(I:I)).EQ.0) THEN
C
C NO, CHECK IF IT IS A '_'.
C
          IF(PAR(I:I).EQ.'_') THEN
C
C IT IS, SKIP IT.
C
            K=I+1
          ELSEIF(PAR(I:I).EQ.'/') THEN
C
C THE CHARACTER IS '/', TRY TO INTERPRET THE DATE AS THE
C FORMAT YY/MM/DD.
C
            CALL CHKYMD(PAR,LP,PARDAT,IDATE)
c
c Check if from 2000 to 2060 set date to be 1000101 etc.
c
            if (idate.lt.600000) then
               idate = idate + 1000000
            endif
            RETURN
          ELSE
C
C IT IS SOMETHING ELSE, THE MONTH STARTS HERE.
C
            K=I
          ENDIF
C
C NOW TRY TO LOCATE THE START OF THE YEAR.
C
          GOTO 1001
        ENDIF
    2 CONTINUE
C
C NO MONTH WAS FOUND, THE PARAMETER IS NOT A DATE, EXIT
C
      RETURN
C
C FOUND THE MONTH, ATTEMPT TO LOCATE THE START OF THE YEAR.
C
 1001 DO 3 L=K+1,LP
C
C IS THE CURRENT PARAMETER A DIGIT?
C
        IF(INDEX(DIGITS,PAR(L:L)).NE.0) THEN
C
C YES, THE YEAR STARTS HERE.
C
          J=L
          GOTO 1002
        ELSEIF(PAR(L:L).EQ.'_') THEN
C
C IT IS A '_', THE YEAR STARTS AT THE NEXT CHARACTER.
C
          J=L+1
          GOTO 1002
        ENDIF
    3 CONTINUE
C
C NO YEAR COULD BE FOUND, THE PARAMETER IS NOT A DATE, EXIT.
C
      RETURN
C
C IS THE MONTH A REAL UNAMBIGUOUS ONE?
C
 1002 IM=INTCMD(MLIST,PAR(K:L-1))
      IF(IM.GT.0) THEN
C
C YES, IS THE DAY PART AN INTEGER?
C
        IF(PARINT(PAR(1:I-1),ID)) THEN
C
C IT IS (IT BETTER HAD BE); IS THE YEAR AN INTGER?
C
          IF(PARINT(PAR(J:LP),IY)) THEN
C
C IT IS, TEST THE RANGE OF THE YEAR.
C
            IF(IY.GE.1950.AND.IY.LE.2049) THEN
C
C FORGET THE MILLENIUM.
C 
              IY=MOD(IY,100)
            ENDIF
C
C NOW WE HAVE THE DAY, MONTH, AND THE YEAR AS INTEGERS TEST IF
C THEY ARE CONSISTENT AND WITHIN RANGE.
C
            CALL CHKDMY(ID,IM,IY,PARDAT,IDATE)
c
c check for millenium to give correct format
c
            if (idate.lt. 600000) then
               idate = idate + 1000000
            endif
          ENDIF
        ENDIF
      ENDIF
      RETURN
C
C END OF LOGICAL FUNCTION PARDAT.
C
      END