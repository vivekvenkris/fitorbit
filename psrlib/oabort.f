*DECK OABORT
C
C
C
      SUBROUTINE OABORT (IDENT, ROUTN, NERR)
C
C Terminates program execution by generating a severe error.  This
C routine is installation dependent.
C Input arguments:
C  IDENT   - Character string describing section where OABORT was
C            called (e.g., '/OLAF/').
C  ROUTN   - Name of the routine detecting the error.
C  NERR    - The error number.
C     Version 1.2   5th September, 1986    Generic
C
C Declare the routine's arguments.
C
      CHARACTER*(*) IDENT, ROUTN
      INTEGER       NERR
C
C Terminate execution.
C
      STOP 'Aborted'
C
C End of subroutine ABORT.
C
      END
