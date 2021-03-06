*DECK GASTMD
C
C
C
      DOUBLE PRECISION FUNCTION GASTMD(IDATE)
C
C CALCULATE APPARENT GST AT 0 UT ON SPECIFIED DATE.
C
      DOUBLE PRECISION T, EQNXMD
C
C NO. OF CENTURIES FROM 1900.
C
      T=(DBLE(IDYDIF(500101,IDATE))+DBLE(18262.5))/DBLE(36525.0)
C
C CALCULATE GAST.
C THIS IS EQUAL TO THE MEAN SIDEREAL TIME PLUS THE EQUATION OF
C THE EQUINOXES.. THE LATTER IS A HORRIBLE THING TO CALCULATE.
C
      T=DBLE(23925.836)+DBLE(8640184.542)*T+DBLE(0.0929)*T*T
      GASTMD=MOD(T,DBLE(86400.0))+EQNXMD(IDATE)
      RETURN
C
C END OF FUNCTION GASTMD.
C
      END
