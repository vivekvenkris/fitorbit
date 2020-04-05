*DECK SETPAR
C
C **************************************************************
      SUBROUTINE SETPAR ( CMDINP, JINP, NINP,
     &     IPAR, RPAR, DPAR, LPAR, CPAR, CPARID, 
     &     IIUNITS, IRUNITS, IDUNITS, DEFJT, IFAIL )
C *************************************************************
C
C PERFORMS THE OPERATION OF SETTING A PROGRAM PARAMETER TO A NEW VALUE.
C
C CMDINP(JINP) TO CMDINP(NINP) CONTAIN THE CHARACTER STRINGS WHICH ARE
C     TO BE USED FOR SETTING THE NEW VALUE.
C XPAR ARE ARRAYS CONTAINING THE PARAMETERS
C      WHERE X IS I,R,C,L,D FOR THE DIFFERENT FORTRAN DATATYPES.
C CPARID CONTAINS THE CHARACTER STRING WHICH IDENTIFIES THE TYPE AND
C      THE STATUS OF THE PARAMETER TO BE SET.
C      '*' INDICATES THAT THE PARAMETER IS NOT CURRENTLY IN USE.
C      'XNN' INDICATES THAT THE PARAMETER IS OF TYPE X
C            AND NN POINTS TO AN ELEMENT OF THE APPROPRIATE ARRAY.
C
C      PARAMETER TYPES ARE :
C         I FOR THE INTEGER VARIABLE IPAR(NN).
C         R         REAL             RPAR(NN).
C         D         DOUBLE PRECISION DPAR(NN).
C         C         CHARACTER        CPAR(NN).
C         F         FILENAME         CPAR(NN).
C                    ( FORMAT OF THE PARAMETER OUTPUT IS SLIGHLTY
C                      DIFFERENT TO CHARACTER TYPE ).
C         L         LIST OF INTEGER VARIABLES.
C                    IPAR(NN)   IS MAXIMUM NUMBER OF VALUES IN THE LIST.
C                    IPAR(NN+1) IS ACTUAL  NUMBER OF VALUES IN THE LIST.
C                    IPAR(NN+2) ... CONTAIN THE ACTUAL LIST OF VALUES.
C         S         LIST OF REAL VARIABLES.
C                    RPAR(NN)   IS MAXIMUM NUMBER OF VALUES IN THE LIST.
C                    RPAR(NN+1) IS ACTUAL  NUMBER OF VALUES IN THE LIST.
C                    RPAR(NN+2) ... CONTAIN THE ACTUAL LIST OF VALUES.
C         P         DOUBLE PRECISION DPAR(NN).
C                    ( INTERPRETED AS A POSITION ).
C         T         DOUBLE PRECISION DPAR(NN).
C                    ( INTERPRETED AS A JULIAN DATE/TIME ).
C         W         LOGICAL TREATED AS A SWITCH
C                    WHICH MAY BE SET 'ON' OR 'OFF'.
C         O         LIST OF OPTIONS ( CHARACTER STRINGS ).
C                      IPAR(NN)   IS THE NUMBER OF THE CURRENT OPTION.
C                      IPAR(NN+1) POINTS TO THE FIRST ELEMENT IN CPAR OF
C                      THE LIST OF CHARACTER STRINGS TERMINATED BY AN
C                      EMPTY STRING.
C
C IXUNITS CONTAIN THE UNIT NUMBER FOR EACH I,R AND D PARAMETER.
C
C DEFJT IS A DEFAULT JULIAN TIME. IF GREATER THAN ZERO IT WILL BE USED
C     TO SET THE DATE OR TIME OF A JULIAN TIME IF EITHER IS NOT SUPPLIED
C     OTHERWISE THEY DEFAULT TO THE CURRENT VALUE OF THE PARAMETER.
C IFAIL IS ZERO UNLESS AN ERROR HAS OCCURRED.
C
c Mod 10/03/2006 Correct data type in call to pserr
c
      CHARACTER*(*) CMDINP(*),CPARID,CPAR(*)
      INTEGER IPAR(*),IIUNITS(*),IRUNITS(*),IDUNITS(*)
     &       ,IFAIL,JINP,NINP,IUNIT,IUPAR,IVAL,LENGTH,I,J
     &       ,INTCMD,IOPT,IPOSN,IERR
      REAL*4 RPAR(*),rval
      REAL*8 SUNIT,DPAR(*),DEFJT,POS,DVAL,SCALE
      LOGICAL LPAR(*),CMDJT,DATE,TIME
     &       ,COMSTR,PARINT,PAREAL,PARJT,PARPOS,PARDBL,PARUNT,PARSWT
      CHARACTER CHARDATE*80,CFAIL*80
C
C     CLEAR THE ERROR FLAG.
C
      IFAIL = 0
      CFAIL = ' '
      CALL TIMING (' ')
C
C     CHECK THAT THERE ARE COMMAND PARAMETERS TO BE INSPECTED.
C
      IF ( JINP.LE.NINP ) THEN
C
C        DECODE THE PARAMETER IDENTIFICATION.
C        IPOSN IS THE POSITION OF THE PARAMETER IN THE COMMON ARRAYS.
C
         READ (CPARID,999) IPOSN
         CALL TIMING ('SETPAR reading the ID')
C
C        FIND OUT WHETHER A UNIT IS SPECIFIED.
C        FOR STRAIGHT INTEGER, REAL OR DOUBLE PARAMETERS,
C        THIS IS ANYTHING AFTER THE FIRST COMMAND.
C        FOR THE LISTS THE LAST COMMAND IS INTERPRETED AS A UNIT,
C        IF IT CAN NOT BE INTERPRETED AS A NUMBER.
C
         IUNIT = 0
         IF ( (CPARID(1:1).EQ.'I'.OR.CPARID(1:1).EQ.'R'
     &     .OR.CPARID(1:1).EQ.'D') .AND.JINP+1.LE.NINP ) THEN
            IUPAR = JINP+1
         ELSEIF (CPARID(1:1).EQ.'L'
     &        .AND..NOT.PARINT(CMDINP(NINP),IVAL)) THEN
            IUPAR = NINP
         ELSEIF (CPARID(1:1).EQ.'S'
     &           .AND..NOT.PAREAL(CMDINP(NINP),rVAL)) THEN
            IUPAR = NINP
         ELSE
C
C           FLAG NO UNIT REQUIRED OR SPECIFIED.
C
            IUNIT = -1
         ENDIF
C
C        IF A UNIT IS SPECIFIED, THEN EXAMINE IT.
C
         IF ( IUNIT.EQ.0 ) THEN
            IF ( .NOT.PARUNT(CMDINP(IUPAR),IUNIT) ) THEN
               IFAIL = 17
               CFAIL = CMDINP(IUPAR)
               GOTO 1000
            ENDIF
            NINP = NINP
         ENDIF
C
C        IF A NEW UNIT HAS BEEN CORRECTLY IDENTIFIED,
C        LOAD IT INTO THE APPROPRIATE UNIT ARRAY,
C        OTHERWISE OBTAIN THE OLD UNIT.
C
         IF ( CPARID(1:1).EQ.'I'.OR.CPARID(1:1).EQ.'L' ) THEN
            IF ( IUNIT.LE.0 ) THEN
               IUNIT = IIUNITS(IPOSN)
            ELSE
               IIUNITS(IPOSN) = IUNIT
            ENDIF
         ELSEIF ( CPARID(1:1).EQ.'R'.OR.CPARID(1:1).EQ.'S' ) THEN
            IF ( IUNIT.LE.0 ) THEN
               IUNIT = IRUNITS(IPOSN)
            ELSE
               IRUNITS(IPOSN) = IUNIT
            ENDIF
         ELSEIF ( CPARID(1:1).EQ.'D' ) THEN
            IF ( IUNIT.LE.0 ) THEN
               IUNIT = IDUNITS(IPOSN)
            ELSE
               IDUNITS(IPOSN) = IUNIT
            ENDIF
         ENDIF
C
C        SET THE SCALING FACTOR.
C
         IF ( IUNIT.GT.0 ) THEN
            SCALE = SUNIT(IUNIT)
         ELSE
            SCALE = 1.0
         ENDIF
         CALL TIMING ('SETPAR unit handling')
C
C        EXAMINE THE NEW PARAMETER VALUE.
C
C        IF IT IS A '*', THEN ONLY THE UNIT WAS INTENDED TO BE CHANGED,
C        SO RETURN.
C
         IF ( CMDINP(JINP).EQ.'*' ) THEN
            JINP = NINP+1
            RETURN
         ENDIF
C
C        EXAMINE THE NEW PARAMETER, THE INTERPRETATION DEPENDING UPON
C        THE PARAMETER TYPE.
C
C        INTEGER PARAMETER.
C
         IF     ( CPARID(1:1).EQ.'I' ) THEN
            IF ( PARINT(CMDINP(JINP),IVAL) ) THEN
               IPAR(IPOSN) = IVAL*SCALE
            ELSE
               IFAIL = 19
            ENDIF
C
C        REAL PARAMETER.
C
         ELSEIF ( CPARID(1:1).EQ.'R' ) THEN
            IF ( PAREAL(CMDINP(JINP),rVAL) ) THEN
               RPAR(IPOSN) = rVAL*SCALE
            ELSE
               IFAIL = 19
            ENDIF
C
C        DOUBLE PRECISION PARAMETER.
C
         ELSEIF ( CPARID(1:1).EQ.'D' ) THEN
            IF ( PARDBL(CMDINP(JINP),DVAL) ) THEN
               DPAR(IPOSN) = DVAL*SCALE
            ELSE
               IFAIL = 19
            ENDIF
C
C        POSITION PARAMETER.
C
         ELSEIF ( CPARID(1:1).EQ.'P' ) THEN
            IF ( PARPOS(CMDINP(JINP),POS) ) THEN
               DPAR(IPOSN) = POS
            ELSE
               IFAIL = 19
            ENDIF
C
C        DATE/TIME PARAMETER.
C
         ELSEIF ( CPARID(1:1).EQ.'T' ) THEN
            IF ( NINP.GT.JINP ) THEN
               CHARDATE = CMDINP(JINP)(1:LENGTH(CMDINP(JINP)))//' '
     &             //CMDINP(JINP+1)(1:LENGTH(CMDINP(JINP+1)))
            ELSE
               CHARDATE = CMDINP(JINP)
            ENDIF
            DVAL = DPAR(IPOSN)
            IF ( DVAL.LT.0.0 ) DVAL = DEFJT
            IF ( CMDJT(CMDINP,NINP,JINP,DATE,TIME,DVAL) ) THEN
               DPAR(IPOSN) = DVAL
               JINP = JINP+2
            ELSE
               IFAIL = 57
               CFAIL = CHARDATE
            ENDIF
C
C        LIST OF INTEGERS
C
         ELSEIF ( CPARID(1:1).EQ.'L' ) THEN
            IF ( CMDINP(JINP)(1:1).EQ.'<' ) THEN
               IPAR(IPOSN+1) = 0
            ELSEIF ( CMDINP(JINP).EQ.'-' ) THEN
  100          CONTINUE
               JINP = JINP+1
               IF ( JINP.LE.NINP ) THEN
                  IF ( .NOT.PARINT(CMDINP(JINP),IVAL) ) THEN
                     IFAIL = 19
                  ELSE
                     IVAL = IVAL*SCALE
                     DO 10 I=1,IPAR(IPOSN+1)
                        IF ( IPAR(IPOSN+I+1).EQ.IVAL ) THEN
                           DO 20 J=I,IPAR(IPOSN+1)-1
                              IPAR(IPOSN+J+1) = IPAR(IPOSN+J+2)
   20                      CONTINUE
                           IPAR(IPOSN+1) = IPAR(IPOSN+1)-1
                           GOTO 100
                        ENDIF
   10                CONTINUE
                     IFAIL = 55
                  ENDIF
               ENDIF
            ELSE
               IF ( CMDINP(JINP).NE.'+' ) THEN
                  IPAR(IPOSN+1) = 0
                  JINP = JINP-1
               ENDIF
  200          CONTINUE
               JINP = JINP+1
               IF ( JINP.LE.NINP ) THEN 
                  IF ( .NOT.PARINT(CMDINP(JINP),IVAL) ) THEN
                     IFAIL = 19
                  ELSE
                     IVAL = IVAL*SCALE
                     IF ( IPAR(IPOSN+1)+1.GT.IPAR(IPOSN) ) THEN
                        IERR = IPAR(IPOSN)
                        IFAIL = 41
                        CFAIL = 'Parameter'
                     ELSE
                        IPAR(IPOSN+1) = IPAR(IPOSN+1)+1
                        IPAR(IPOSN+IPAR(IPOSN+1)+1) = IVAL
                        GOTO 200
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
C
C        LIST OF REALS
C
         ELSEIF ( CPARID(1:1).EQ.'S' ) THEN
            IF ( CMDINP(JINP)(1:1).EQ.'<' ) THEN
               RPAR(IPOSN+1) = 0.0
            ELSEIF ( CMDINP(JINP).EQ.'-' ) THEN
  300          CONTINUE
               JINP = JINP+1
               IF ( JINP.LE.NINP ) THEN
                  IF ( .NOT.PAREAL(CMDINP(JINP),rVAL) ) THEN
                     IFAIL = 19
                  ELSE
                     rVAL = rVAL*SCALE
                     DO 30 I=1,INT(RPAR(IPOSN+1))
                        IF ( RPAR(IPOSN+I+1).EQ.rVAL ) THEN
                           DO 40 J=I,INT(RPAR(IPOSN+1)-1)
                              RPAR(IPOSN+J+1) = RPAR(IPOSN+J+2)
   40                      CONTINUE
                           RPAR(IPOSN+1) = RPAR(IPOSN+1)-1
                           GOTO 300
                        ENDIF
   30                CONTINUE
                     IFAIL = 55
                  ENDIF
               ENDIF
            ELSE
               IF ( CMDINP(JINP).NE.'+' ) THEN
                  RPAR(IPOSN+1) = 0
                  JINP = JINP-1
               ENDIF
  400          CONTINUE
               JINP = JINP+1
               IF ( JINP.LE.NINP ) THEN 
                  IF ( .NOT.PAREAL(CMDINP(JINP),rVAL) ) THEN
                     IFAIL = 19
                  ELSE
                     rVAL = rVAL*SCALE
                     IF ( RPAR(IPOSN+1)+1.GT.RPAR(IPOSN) ) THEN
                        IERR = RPAR(IPOSN)
                        IFAIL = 41
                        CFAIL = 'Parameter'
                     ELSE
                        RPAR(IPOSN+1) = RPAR(IPOSN+1)+1
                        RPAR(IPOSN+RPAR(IPOSN+1)+1) = rVAL
                        GOTO 400
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
C
C        CHARACTER PARAMETER.
C
         ELSEIF ( CPARID(1:1).EQ.'C' ) THEN
            CPAR(IPOSN) = CMDINP(JINP)
C
C        FILENAME PARAMETER.
C
         ELSEIF ( CPARID(1:1).EQ.'F' ) THEN
            CPAR(IPOSN) = CMDINP(JINP)
C
C        SWITCH PARAMETER.
C
         ELSEIF ( CPARID(1:1).EQ.'W' ) THEN
            IF ( .NOT.PARSWT(CMDINP(JINP),LPAR(IPOSN)) ) THEN
               IFAIL = 26
            ENDIF
C
C        OPTION PARAMETER.
C
         ELSEIF ( CPARID(1:1).EQ.'O' ) THEN
            IF ( CMDINP(JINP)(1:1).EQ.'<' ) THEN
               IPAR(IPOSN) = 0
            ELSE
               IOPT = INTCMD(CPAR(IPAR(IPOSN+1)),CMDINP(JINP))
               IF ( IOPT.EQ.0 ) THEN
                  IFAIL = 26
               ELSE
                  IPAR(IPOSN) = ABS(IOPT)
               ENDIF
            ENDIF
         ENDIF
      ELSE
         IFAIL = 23
      ENDIF
      CALL TIMING ('SETPAR parameter decoding')
C
C     LABEL 1000 IS THE ROUTINE EXIT POINT
C
 1000 CONTINUE
C
C     IF AN ERROR WAS DETECTED CALL THE ERROR ROUTINE.
C
      IF ( IFAIL.NE.0 ) THEN
         IF ( CFAIL.EQ.' ' ) THEN
            CFAIL = CMDINP(JINP)
         ENDIF
         CALL PSRERR ('SETPAR',IFAIL,IERR,0.,CFAIL)
      ELSE
C
C        SET POINTER TO AFTER THE LAST PARAMETER EXAMINED.
C
         JINP = JINP+1
         IF ( IUNIT.GT.0 ) JINP = JINP+1
      ENDIF
      CALL TIMING ('SETPAR error handling')
C
      RETURN
C
C FORMAT.
C
  999 FORMAT ( 1X,I2 )
C
C END OF SUBROUTINE SETPAR.
C
      END
