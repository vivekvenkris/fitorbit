*DECK SPECHR
C
C
C
      LOGICAL FUNCTION SPECHR(CHR)
C
C This routine returns .true. If the character chr is one of the
C "special" command line characters: these act as separators and also
C as parameters in themselves.
C     Version 1.1  27th january, 1987
C
      CHARACTER CHR*1,SPECS*(*),LITDEL*1,NUPDEL*1
C
C Define the special characters (note that the literal delimiter litdel
C is also one).
C
      PARAMETER (SPECS='=()')
C
C Set spechr.
C
      SPECHR=INDEX(SPECS,CHR).GT.0.OR.CHR.EQ.LITDEL()
     &       .OR.CHR.EQ.NUPDEL()
      RETURN
C
C End of logical function spechr.
C
      END
