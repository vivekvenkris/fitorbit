*DECK SHOCMD
C
C *********************************************************
      SUBROUTINE SHOCMD ( CMDLST,CMDDSC,GBLLST,GBLDSC )
C *********************************************************
C
      CHARACTER*(*) CMDLST(*),CMDDSC(*),GBLLST(*),GBLDSC(*)
      CHARACTER*81 OUTBUF
C
C     LIST THE LOCAL COMMAND GROUP
C     LOOP UNTIL EITHER THE COMMAND OR THE DESCRIPTION IS BLANK
C
      I = 0
   10 CONTINUE
         I = I+1
         IF (CMDLST(I).EQ.' '.OR.CMDDSC(I*2-1).EQ.' ' ) GOTO 20
         IF (I.EQ.1 ) CALL OUTMON(' Current commands :-')
         WRITE (OUTBUF,999) CMDLST(I),CMDDSC(I*2-1)
         CALL OUTMON(OUTBUF)
         IF (CMDDSC(I*2).NE.' ' ) THEN
            WRITE (OUTBUF,998) CMDDSC(I*2)
            CALL OUTMON(OUTBUF)
         ENDIF
      GOTO 10
   20 CONTINUE
C
C     DO THE SAME FOR THE GLOBAL COMMANDS
C
      J = 0
   30 CONTINUE
         J = J+1
         IF (GBLLST(J).EQ.' '.OR.GBLDSC(J*2-1).EQ.' ' ) GOTO 40
         IF (J.EQ.1 ) CALL OUTMON(' Global commands :-')
         WRITE (OUTBUF,999) GBLLST(J),GBLDSC(J*2-1)
         CALL OUTMON(OUTBUF)
         IF (GBLDSC(J*2).NE.' ' ) THEN
            WRITE (OUTBUF,998) GBLDSC(J*2)
            CALL OUTMON(OUTBUF)
         ENDIF
      GOTO 30
   40 CONTINUE
C
C     IF THEIR ARE ANY UNUSED DESCRIPTION LINES FROM THE
C     LOCAL COMMAND LIST, OUTPUT THEM
C
      I = I*2-1
   50 CONTINUE
         I = I+1
         IF (CMDDSC(I).EQ.' ' ) GOTO 60
         WRITE (OUTBUF,997) CMDDSC(I)
         CALL OUTMON(OUTBUF)
      GOTO 50
   60 CONTINUE
C
      RETURN
C
  999 FORMAT (6X,A10,5X,A60 )
  998 FORMAT (21X,A60 )
  997 FORMAT (1X,A60,20X )
C
C END OF SUBROUTINE SHOCMD
C
      END
