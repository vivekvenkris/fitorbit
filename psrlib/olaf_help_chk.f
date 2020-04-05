*DECK OLAF_HELP_CHECK_LEVEL
C
C
C
      SUBROUTINE OLAF_HELP_CHECK_LEVEL (BUFFER, IS_A_LEVEL, LEVEL, NAME)
C
C This routine checks if the line held in BUFFER is the start of a new
C help level.
C
C Arguments:
C  BUFFER      Input   CHARACTER*(*)  The buffer holding the line.
C  IS_A_LEVEL  Output  LOGICAL        .TRUE. if the line is the start of
C                                     a level, else .FALSE..
C  LEVEL       Output  INTEGER        If IS_A_LEVEL is .TRUE. this is
C                                     the level number.
C  NAME        Output  CHARACTER*(*)  If IS_A_LEVEL is .TRUE. this is
C                                     the level's name.
C
C     Version 1.0   4th November, 1987
C
C Declare the routine's arguments.
C
      CHARACTER*(*) BUFFER, NAME
      LOGICAL       IS_A_LEVEL
      INTEGER       LEVEL
C
C Declare external references.
C
      LOGICAL       PARINT
C
C Declare local variables.
C
      INTEGER       PS, PF
C
C Assume that the line is not the start of a buffer.
C
      IS_A_LEVEL = .FALSE.
C
C Locate the first parameter in BUFFER.
C
      CALL OLAF_HELP_SPLIT_INPUT (BUFFER, 1, PS, PF)
C
C Does it start at the beginning of the line?
C
      IF (PS .EQ. 1) THEN
C
C Yes, is it an integer?
C
        IF (PARINT (BUFFER(PS:PF), LEVEL)) THEN
C
C It is: obtain its name.
C
          CALL OLAF_HELP_SPLIT_INPUT (BUFFER, PF+1, PS, PF)
C
C Is there one?
C
          IF (PS .GT. 0) THEN
C
C There is: return it.
C
            NAME = BUFFER(PS:PF)
C
C Note that the line is the start of a level.
C
            IS_A_LEVEL = .TRUE.
          END IF
        END IF
      END IF
C
C End of subroutine OLAF_HELP_CHECK_LEVEL.
C
      END
