*DECK NUPDEL
C
C
C
      CHARACTER*1 FUNCTION NUPDEL()
C
C This routine returns the no upper case conversion delimiter
C character.  It differs from LITDEL in that the delimiter is
C not preserved as a parameter in the output from GETCMD.
C     Version 1.0   27th January, 1987
C
C Define the delimiter.
C
      CHARACTER  DELIM*1
      PARAMETER (DELIM = '''')
C
C Return the no upper case conversion delimiter.
C
      NUPDEL = DELIM
C
C End of character*1 function NUPDEL.
C
      END

