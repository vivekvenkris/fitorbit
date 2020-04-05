*DECK ICORIG
C
C **************************************************************
      INTEGER FUNCTION ICORIG ( ICH )
C **************************************************************
C
C RETURNS THE ORIGINAL CHANNEL NUMBER OF THE CURRENT DATA
C     CHANNEL ICH.
C
      INCLUDE 'PSRDAT.DEF'
C
C     CLEAR ICORIG
C
      ICORIG = 0
C
C     LOOK FOR THE ICH IN THE LIST OF ORIGINAL CHANNEL NUMBERS
C
      DO 10 I=1,NCORIG
         IF ( ICCURR(I).EQ.ICH ) THEN
C
C           RETURN WITH THE CHANNEL NUMBER
C
            ICORIG = I
            RETURN
         ENDIF
   10 CONTINUE
C
      RETURN
C
C END OF INTEGER FUNCTION ICORIG
C
      END





