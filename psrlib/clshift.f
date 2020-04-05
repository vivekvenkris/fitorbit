* DECK CLSHIFT
C **********************************************************************
      CHARACTER*(*) FUNCTION CLSHIFT (STRING,CHAR)
C **********************************************************************
C
C RETURNS  STRING LEFT-SHIFTED TO REMOVE LEADING CHARACTERS CHAR
C
      CHARACTER STRING*(*), CHAR*1
C
      L=LEN (STRING)
      I=1
      DO WHILE (I.LT.L.AND.STRING(I:I).EQ.CHAR)
         I=I+1
      ENDDO
C
C     NOW I POINTS TO THE FIRST CHARACTER IN STRING .NE. CHAR
C
      EFFLENGTH = L-I+1
C
C     SHIFT STRING INTO CLSHIFT AND PAD OUT WITH TRAILING SPACES
C
      CLSHIFT  = STRING (I:L)
C
      RETURN
C
C     END OF SUBROUTINE CLSHIFT
C
      END
