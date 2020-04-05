*DECK ELPSEC
C
C ********************************************************************
      REAL FUNCTION ELPSEC ( INIT )
C ********************************************************************
C
C INIT = 0 INITIALISES ROUTINE
C INIT = 1 RETURNS ELAPSED TIME SINCE INITIALISATION
C
C THIS ROUTINE IS INSTALLATION DEPENDENT
C
C VAX-11 FORTRAN VERSION
C
C     INITIALIZE SINIT OR RETURN TIME SINCE SINIT
C
c
c     DRL 93/10/09 Disabled this routine temporarily to
c     run on Solaris OS
c
c      IF(INIT.EQ.0) THEN
c         SINIT = SECNDS(0.0)
         ELPSEC = 0.0
c      ELSE
c         ELPSEC = SECNDS(SINIT)
c      ENDIF
C
      RETURN
C
C END OF REAL FUNCTION ELPSEC
C
      END
