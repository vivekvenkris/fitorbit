*DECK CCJUST
C
C **************************************************************
      CHARACTER*(*) FUNCTION CCJUST ( STRING, N )
C **************************************************************
C
C RETURNS A CENTRE-JUSTIFIED FORM OF STRING, WITH STRING AT THE
C     CENTRE OF N CHARACTERS.
C
      CHARACTER*(*) STRING
C
      L = MAX((N-LENGTH(STRING))/2,0)
      DO 10 I=1,L
         CCJUST(I:I) = ' '
   10 CONTINUE
      CCJUST(L+1:) = STRING
C
      RETURN
C
C END OF CHARACTER FUNCTION CCJUST
C
      END
