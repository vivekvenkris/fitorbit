*DECK SHOLUS
C
C
C **************************************************************
      SUBROUTINE SHOLUS (LU)
C **************************************************************
C
C MONITORS ON LOGICAL UNIT LU THOSE LOGICAL UNIT NUMBERS WHICH 
C     ARE ALREADY ASSIGNED AMONGST THOSE SUPPORTED BY THE LIBRARY
C     FILE HANDLING ROUTINES.
C
      INCLUDE 'PSRLIB.DEF'
      LOGICAL OP,EXI,NAM
      CHARACTER*60 FILNAM,CBUFF*80
C
C     SEARCH FOR AN OPEN LU, UP TO THE MAXIMUM MAXLU.
C
      DO 10 I=1,MAXLU
         INQUIRE (I,NAME=FILNAM,OPENED=OP)
         IF ( OP ) THEN
            IOPEN=IOPEN+1
            WRITE(CBUFF,'(I4,2x)') I
            CBUFF = CBUFF(1:6) // FILNAM
            CALL OUTPUT(LU,CBUFF)
         ENDIF
   10 CONTINUE
C
C     END OF SUBROUTINE SHOLUS
C
      END