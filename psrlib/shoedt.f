*DECK SHOEDT
C
C **************************************************************
      SUBROUTINE SHOEDT ( LU,IFLAG )
C **************************************************************
C
C Produces a display on logical unit lu of the current data block
C     Editting commands.
C  If IFLAG is non-zero then the display is on a single line.
C
      INCLUDE 'PSRDAT.DEF'
C

      CHARACTER OUTBUF*132,FMT*40,ISIZE*1
C
      IF (%LOC(IFLAG).NE.0.AND.IFLAG.NE.0) GOTO 100
C
      IF ( NEDT.EQ.0 ) THEN
C
C        MONITOR NO ENTRIES IN THE EDIT LIST
C
         CALL OUTPUT (LU,' No edit commands set')

      ELSEIF ( NEDT.GT.0 ) THEN
C
C        OUTPUT LIST
C
         CALL OUTPUT (LU,' Edit commands:')
         FMT = '(1X,I'//ISIZE(NEDT)//','' :'')'
         DO 20 I=1,NEDT
C
C           START OF LINE IS EDIT LINE NUMBER
C
            WRITE (OUTBUF,FMT) I
C
C           ADD EACH PARAMETER OF THE EDIT COMMAND TO OUTBUF
C           UNTIL A BLANK PARAMETER IS FOUND
C
            DO 30 J=1,MAXEDP
               IF ( EDTLST(J,I).EQ.' ' ) GOTO 10
               OUTBUF = OUTBUF(1:LENGTH(OUTBUF))//' '//EDTLST(J,I)
   30       CONTINUE
   10       CONTINUE
C
C           OUTPUT LINE
C
            CALL OUTPUT (LU,OUTBUF(1:LENGTH(OUTBUF)))
   20    CONTINUE
      ENDIF
C
      RETURN
C
  100 IF ( NEDT.EQ.0 ) THEN
C
C        MONITOR NO ENTRIES IN THE EDIT LIST
C
         CALL OUTPUT (LU,' No edit commands set')
C
      ELSEIF ( NEDT.GT.0 ) THEN
C
C        OUTPUT LIST
C
         RECL = 132
         WRITE (OUTBUF,'(A)') ' Edit commands:'
C
         DO 200 I=1,NEDT
C
C  Add each parameter of the edit command to outbuf
C   Until a blank parameter is found,
C   or next command is too long to fit onto output line.
C
           DO 300 J=1,MAXEDP
             IF ( EDTLST(J,I).EQ.' ' ) GOTO 110
             ILEN = LENGTH(OUTBUF)
             ILST = LENGTH(EDTLST(J,I))
             IF (ILEN+ILST+1.GT.RECL) THEN
               CALL OUTPUT (LU,OUTBUF(1:ILEN))
               WRITE (OUTBUF,'(A)') ' '
               ILEN = 1
             ENDIF
C
             OUTBUF = OUTBUF(1:ILEN)//' '//EDTLST(J,I)
  300      CONTINUE
  110      CONTINUE
  200    CONTINUE
C
C  Output line
C
        CALL OUTPUT (LU,OUTBUF(1:LENGTH(OUTBUF)))
      ENDIF
C
      RETURN
C
C END OF SUBROUTINE SHOEDT
C
      END


