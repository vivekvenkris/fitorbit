*DECK EDIT
C
C **************************************************************
      SUBROUTINE EDIT ( CMD, NCMD, MAXPAR, IEDIT, IFAIL )
C **************************************************************
C
C CMD(NCMD,*) CONTAINS NCMD LINES FOR THE EDITOR CONSISTING
C     OF UP TO MAXPAR PARAMETERS EACH.
C IF IEDIT = 0 THESE LINES ARE INTERPRETED AND ANY ERROR
C              RETURNED IN IFAIL, BUT NO EDITTING IS PERFORMED.
C IF IEDIT < 0 THEN EDITS ARE TO BE PERFORMED WHICH MUST BE DONE
C              BETWEEN READING OF THE DATA HEADER AND THE DATA
C               ( PRE-EDITTING ). THIS MAY BE REQUIRED FOR EXAMPLE,
C              WHEN DELETING A DATA CHANNEL. IT IS FLAGGED OFF
C              AT THIS STAGE SO THAT THE DATA IS NOT DECODED INTO
C              THE INTERNAL STORAGE.
C IF IEDIT > 0 THEN ALL OTHER EDITS ARE PERFORMED ( POST-EDITTING ).
C WHEN RECOGNIZED, THE COMMANDS ARE COPIED BACK INTO THE ARRAY CMD
C     SO THAT ANY ABBREVIATIONS USED ARE EXPANDED TO THEIR FULL
C     FORMS FOR MONITORING PURPOSES.
C
C THIS ROUTINE IS INSTALLATION DEPENDENT.
C     ( BECAUSE CMD(MAXPAR,*)*(*) BREAKS THE FORTRAN 200 COMPILER )
C
C VAX-11 FORTRAN VERSION.
C
c Mod 10/03/2006 Correct data type in call to pserr
c
      INCLUDE 'PSRDAT.DEF'
C
      PARAMETER ( LENPAR=40 )
      CHARACTER CMD(MAXPAR,*)*(*)
      CHARACTER*(LENPAR) EDCMD(4)
      DIMENSION ICH(MAXCHAN)
C
      DATA EDCMD / 'SWAP','DELETE','SET',' ' /
C
C     CLEAR ERROR FLAG
C
      IFAIL = 0
C
C     RETURN IF THERE ARE NO COMMANDS
C
      IF ( NCMD.LE.0 ) RETURN
C
C     COMMAND INTERPRETATION LOOP
C
      DO 20 ICMD=1,NCMD
C
C        DETERMINE HOW MANY PARAMETERS ARE IN THE COMMAND
C
         DO 30 NPAR=1,MAXPAR
            IF ( CMD(NPAR,ICMD).EQ.' ' ) GOTO 10
   30    CONTINUE
   10    CONTINUE
         NPAR = NPAR-1
C
C        INTERPRET THE COMMANDS
C
         IF ( NPAR.GT.0 ) THEN
C
C           LOOK FOR FIRST PARAMETER IN THE EDIT COMMAND LIST
C
            I = INTCMD(EDCMD,CMD(1,ICMD))
            IF ( I.LT.0 ) THEN
C
C              AMBIGUOUS COMMAND
C
               IFAIL = 6
               CALL PSRERR ('EDIT',IFAIL,0,0.,CMD(1,ICMD))
               RETURN
            ELSEIF ( I.EQ.0 ) THEN
C
C              UNRECOGNIZED COMMAND
C
               IFAIL = 5
               CALL PSRERR ('EDIT',IFAIL,0,0.,CMD(1,ICMD))
               RETURN
            ELSE
C
C              COMMAND RECOGNIZED
C              COPY COMMAND BACK INTO LIST
C
               CMD(1,ICMD) = EDCMD(I)
            ENDIF
C
C           RETURN IF NO MORE PARAMETERS
C
            IF ( NPAR.LE.1 ) THEN
               IFAIL = 23
               CALL PSRERR ('EDIT',IFAIL,0,0.,' ')
               RETURN
            ENDIF
C
C           PERFORM THE REQUIRED EDIT COMMAND
C
            IF ( I.EQ.1 ) THEN
C
C              SWAP COMMAND - GET SPECIFIED CHANNEL RANGE
C
               IPAR = 2
               CALL CHNCMD( CMD(1,ICMD),NPAR,IPAR,ICH,NICH,MAXCHAN,
     +                      IFAIL)
               IF ( IFAIL.NE.0 ) RETURN
C
C              ERROR IF OTHER THAN TWO CHANNELS ARE SPECIFIED
C
               IF ( NICH.NE.2 ) THEN
                  IFAIL = 45
                  CALL PSRERR ('EDIT',IFAIL,0,0.,' ')
                  RETURN
               ENDIF
C
C              IF POST-EDITTING, THEN SWAP THE DATA CHANNEL NUMBERS
C
               IF ( IEDIT.LT.0 ) THEN
                  K = ICCURR(ICH(1))
                  ICCURR(ICH(1)) = ICCURR(ICH(2))
                  ICCURR(ICH(2)) = K
               ENDIF
            ELSEIF ( I.EQ.2 ) THEN
C
C              DELETE CHANNELS - GET CHANNEL SPECIFICATION
C
               IPAR = 2
               CALL CHNCMD (CMD(1,ICMD),NPAR,IPAR,ICH,NICH,MAXCHAN,
     +                      IFAIL)
               IF ( IFAIL.NE.0 ) RETURN
C
C              IF PRE-EDITTING, THEN FLAG OFF SPECIFIED CHANNELS
C
               IF ( IEDIT.LT.0 ) THEN
                  DO 40 K=1,NICH
C
C
C                    DECREMENT ALL CHANNEL NUMBERS HIGHER THAN
C                    THE ONE TO BE DELETED
C
                     DO 50 J=1,NCH
                        IF ( ICCURR(J).GT.ICCURR(ICH(K)) ) THEN
                           ICCURR(J) = ICCURR(J)-1
                        ENDIF
   50                CONTINUE
C
C                    FLAG OFF THE CHANNEL
C
                     ICCURR(ICH(K)) = -ICCURR(ICH(K))
   40             CONTINUE
               ENDIF
            ELSEIF ( I.EQ.3 ) THEN
C
C              SET COMMAND - CHECK THE PARAMETER NAME
C
            ENDIF
         ENDIF
   20 CONTINUE
C
      RETURN
C
C END OF SUBROUTINE EDIT
C
      END
