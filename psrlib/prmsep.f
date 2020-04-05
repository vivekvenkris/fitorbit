*DECK PRMSEP
C
C
C
      LOGICAL FUNCTION PRMSEP(CHR)
C
C This routine returns .true. If the character chr is a valid parameter
C separator, otherwise it returns .false..
C     Version 1.1  27th january, 1987
C
      CHARACTER CHR*1,SEPS*(*)
C
C Define the separators.
C
      PARAMETER (SEPS=' ,')
C
C Set prmsep.
C
      PRMSEP=INDEX(SEPS,CHR).GT.0
      RETURN
C
C End of logical function prmsep.
C
      END
