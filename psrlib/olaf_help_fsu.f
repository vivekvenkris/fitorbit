*DECK OLAF_HELP_FIND_SUBTOPIC
C
C
C
      SUBROUTINE OLAF_HELP_FIND_SUBTOPIC (UNIT, NAME, MAX_LEVEL,
     &                                    CURRENT_LEVEL,
     &                                    LEVEL_POSITIONS, LEVEL_NAMES,
     &                                    NONE, BUFFER, NAMEBUF1,
     &                                    NAMEBUF2, OUTBUF, STATUS)
C
C This routine searches for the subtopic NAME of the current help
C level.  If found, it updates CURRENT_LEVEL, LEVEL_POSITIONS, and
C LEVEL_NAMES.
C
C Arguments:
C  UNIT             Input         INTEGER        The help file's unit
C                                                number.
C  NAME             Input         CHARACTER*(*)  The subtopic's name in
C                                                lower case.
C  MAX_LEVEL        Input         INTEGER        The maximum number of
C                                                levels.
C  CURRENT_LEVEL    Input/output  INTEGER        The current help level.
C  LEVEL_POSITIONS  Input/output  INTEGER        The file position for
C                                                each level.
C  LEVEL_NAMES      Input/output  CHARACTER*(*)  The names of the
C                                                levels.
C  NONE             Output        LOGICAL        .TRUE. if the requested
C                                                level did not exist,
C                                                else .FALSE..
C  BUFFER           Output        CHARACTER*(*)  The input buffer.
C  NAMEBUF1         Output        CHARACTER*(*)  A subtopic name buffer.
C  NAMEBUF2         Output        CHARACTER*(*)  A subtopic name buffer.
C  OUTBUF           Output        CHARACTER*(*)  The output buffer.
C  STATUS           Output        INTEGER        Zero if successful,
C                                                else greater than zero.
C
C Declare the routine's arguments.
C
      CHARACTER*(*) NAME, LEVEL_NAMES(*), BUFFER, NAMEBUF1, NAMEBUF2,
     &              OUTBUF
      INTEGER       UNIT, MAX_LEVEL, CURRENT_LEVEL, LEVEL_POSITIONS(*),
     &              STATUS
      LOGICAL       NONE
C
C Declare external references.
C
      LOGICAL       COMSTR
      INTEGER       FTELL, FSEEK
C
C Declare local variables.
C
      INTEGER       LEVEL, POSITION
      character*80 dummy
      LOGICAL       NEW
C
C Go for the next level.
C
      IF (CURRENT_LEVEL .LT. MAX_LEVEL) THEN
        CURRENT_LEVEL = CURRENT_LEVEL + 1
C
C Skip the current help file record if we are not at its beginning.
C
        IF (CURRENT_LEVEL .GT. 1) THEN
          READ (UNIT = UNIT, FMT = 100, IOSTAT = STATUS) dummy
        ELSE
          STATUS = 0
        END IF
C
C Search for the requested subtopic at the current level.
C
        LEVEL = CURRENT_LEVEL
        NAMEBUF1 = ' '
 1000   IF (STATUS .EQ. 0 .AND. LEVEL .GE. CURRENT_LEVEL) THEN
C
C Are we at the required level?
C
          IF (LEVEL .EQ. CURRENT_LEVEL) THEN
C
C Yes, check if the level has the requested name.
C
            NAMEBUF2 = NAMEBUF1
            CALL LOCASE (NAMEBUF2, 1, 1)
            IF (COMSTR (NAMEBUF2, NAME)) THEN
C
C It has: reposition the file at its start.
C
              STATUS = FSEEK (UNIT, POSITION, 0)
              IF (STATUS .EQ. 0) THEN
C
C Remember its position.
C
                LEVEL_POSITIONS(CURRENT_LEVEL) = POSITION
C
C Remember its name.
C
                LEVEL_NAMES(CURRENT_LEVEL) = NAMEBUF1
                NONE = .FALSE.
              END IF
              GOTO 7777
            END IF
          END IF
C
C The name or level does not match: locate a line that is the start of
C a new level.
C
          NEW = .FALSE.
 2000     IF (STATUS .EQ. 0 .AND. .NOT.NEW) THEN
C
C Read the next line from the help file.
C
            POSITION = FTELL (UNIT)
            IF (POSITION .GE. 0) THEN
              READ (UNIT = UNIT, FMT = 100, IOSTAT = STATUS) BUFFER
              IF (STATUS .EQ. 0) THEN
C
C Is it the start of a new level?
C
                CALL OLAF_HELP_CHECK_LEVEL (BUFFER, NEW, LEVEL,
     &                                      NAMEBUF1)
              END IF
            ELSE
              STATUS = ABS (POSITION)
            END IF
            GOTO 2000
          END IF
          GOTO 1000
        END IF
C
C Test if successful.
C
        IF (STATUS .LE. 0) THEN
C
C Yes, but we failed to find the requested level: reposition the file.
C
          CURRENT_LEVEL = CURRENT_LEVEL - 1
          IF (CURRENT_LEVEL .GT. 0) THEN
            STATUS = FSEEK (UNIT, LEVEL_POSITIONS(CURRENT_LEVEL), 0)
          ELSE
            STATUS = FSEEK (UNIT, 0, 0)
          END IF
C
C Note that the subtopic was not found.
C
          NONE = .TRUE.
C
C Complain to the user.
C
          CALL OLAF_HELP_NO_INFO (LEVEL_NAMES, CURRENT_LEVEL, NAME,
     &                            OUTBUF)
        END IF
      ELSE
C
C Too many help levels.
C
        STATUS = 1
      END IF
C
C Format statements.
C
  100 FORMAT (A)
  101 FORMAT ()
C
C End of subroutine OLAF_HELP_FIND_SUBTOPIC.
C
 7777 END


