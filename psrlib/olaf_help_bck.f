*DECK OLAF_HELP_BACK
C
C
C
      SUBROUTINE OLAF_HELP_BACK (UNIT, CURRENT_LEVEL, LEVEL_POSITIONS,
     &                           FOPEN, STATUS)
C
C This routine moves back a level in the help file.  If the current
C level is 1, then the file is closed.
C
C Arguments:
C  UNIT             Input         INTEGER  The help file unit number.
C  CURRENT_LEVEL    Input/output  INTEGER  The current level.
C  LEVEL_POSITIONS  Input/output  INTEGER  The positions of the levels
C                                          in the file.
C  FOPEN            Output        LOGICAL  .FALSE. if the file was
C                                          closed, else unchanged.
C  STATUS           Output        INTEGER  Zero if no errors were
C                                          detected, else greater than
C                                          zero.
C
C     Version 1.0   4th November, 1987   Alliant
C
C Declare the routine's arguments.
C
      INTEGER UNIT, LEVEL_POSITIONS(*), CURRENT_LEVEL, STATUS
      LOGICAL FOPEN
C
C Declare external references.
C
      INTEGER FSEEK
C
C Move back one level.
C
      CURRENT_LEVEL = CURRENT_LEVEL - 1
C
C Have we reached the beginning of the file?
C
      IF (CURRENT_LEVEL .LE. 0) THEN
C
C Yes, close the file.
C
        CLOSE (UNIT = UNIT, IOSTAT = STATUS)
        FOPEN = .FALSE.
      ELSE
C
C No, position the file at the start of the new current level.
C
        STATUS = FSEEK (UNIT, LEVEL_POSITIONS(CURRENT_LEVEL), 0)
      END IF
C
C End of subroutine OLAF_HELP_BACK.
C
      END

