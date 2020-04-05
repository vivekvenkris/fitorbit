*DECK IDYDIF
C
C
C
      FUNCTION IDYDIF(IDATE1,IDATE2)
C
C RETURNS THE NUMBER OF DAYS BETWEEN IDATE1 AND IDATE2, BOTH IN YYMMDD.
C
      IDYDIF = DATEJD(IDATE2)-DATEJD(IDATE1)+0.5
      RETURN
C
C END OF INTEGER FUNCTION IDYDIF.
C
      END
