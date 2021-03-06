*DECK PAREAL
C
C
C
      LOGICAL FUNCTION PAREAL(PAR,VAL)
C
C ATTEMPTS TO INTERPRET THE PARAMETER STRING PAR AS A REAL VALUE.
C RETURNS THE VALUE .TRUE. IF SUCCESSFUL AND .FALSE. OTHERWISE.  ON EXIT
C VAL CONTAINS THE REAL VALUE IF PAREAL IS .TRUE. AND IS UNCHANGED OTHERWISE.
C
      CHARACTER PAR*(*),FMT*10
C
C SET UP THE INITIAL VALUES.
C
      PAREAL=.FALSE.
C
C SCAN THROUGH PAR TO LOCATE THE FIRST NON-SPACE CHARACTER.
C
      LP=LENGTH(PAR)
      DO 1 I=1,LP
        IF(PAR(I:I).NE.' ') GOTO 1000
    1 CONTINUE
      RETURN
C
C EVALUATE THE NUMBER OF CHARACTERS IN THE PARAMETER.
C
 1000 NC=LP-I+1
C
C FRIG THE FORMAT STATEMENT TO GIVE THIS FIELD SIZE.
C
      WRITE(FMT,800) NC
C
C NOW ATTEMPT TO READ THE PARAMETER AS A REAL.
C
      READ(PAR(I:LP),FMT,ERR=100,IOSTAT=NC) V
      IF(ABS(V).LE.REAMAX()) THEN
C
C READ SUCCESSFUL.
C
        PAREAL=.TRUE.
        VAL=V
      ENDIF
      RETURN
C
C READ UNSUCCESSFUL.
C
  100 CONTINUE
      RETURN
C
C FORMAT STATEMENT.
C
  800 FORMAT(SS,'(BN,E',I2,'.0)')
C
C END OF LOGICAL FUNCTION PAREAL.
C
      END
