*DECK WRBLOCK
C
C ***********************************************************
      SUBROUTINE WRBLOCK ( IFAIL )
C ***********************************************************
C
C     WRITES THE DATA BLOCK FROM THE VARIABLES DECLARED IN PSRLIB.CMN
C     IN MAGNETIC TAPE FORMAT TO LOGICAL UNIT LUDOUT.
C
C     IFAIL=0 UNLESS THERE IS AN ERROR
C
      CALL WRDAT(IFAIL)
      IF ( IFAIL.EQ.0 ) CALL WRHEAD(IFAIL)
      RETURN
C
C END OF SUBROUTINE WRBLOCK.
C
      END
