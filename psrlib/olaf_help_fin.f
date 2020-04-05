*DECK OLAF_HELP_FIND_INFO
C
C
C
      SUBROUTINE OLAF_HELP_FIND_INFO (UNIT, PATH, PARS, NPAR, PROG,
     &                                MAX_LEVEL, CURRENT_LEVEL,
     &                                LEVEL_POSITIONS, LEVEL_NAMES,
     &                                FOPEN, BUFFER, NAMEBUF1, NAMEBUF2,
     &                                OUTBUF, STATUS)
C
C This routine positions the help file at the subtopic requested in
C PARS(1) to PARS(NPAR).
C
C Arguments:
C  UNIT             Input         INTEGER        The help file unit
C                                                number.
C  PATH             Input         CHARACTER*(*)  The help directory
C                                                path name.
C  PARS             Input         CHARACTER*(*)  The topics/subtopics.
C  NPAR             Input         INTEGER        The number of these.
C  PROG             Input         CHARACTER*(*)  The program's name.
C  MAX_LEVEL        Input         INTEGER        The maximum help level
C                                                allowed.
C  CURRENT_LEVEL    Input/output  INTEGER        The current help
C                                                level.
C  LEVEL_POSITIONS  Input/output  INTEGER        The position of each
C                                                level in the file.
C  LEVEL_NAMES      Input/output  CHARACTER*(*)  The level names.
C  FOPEN            Input/output  LOGICAL        .TRUE. if a help file
C                                                is open.
C  BUFFER           Output        CHARACTER*(*)  The input buffer.
C  NAMEBUF1         Output        CHARACTER*(*)  A subtopic name buffer.
C  NAMEBUF2         Output        CHARACTER*(*)  A subtopic name buffer.
C  OUTBUF           Output        CHARACTER*(*)  The output buffer.
C  STATUS           Output        INTEGER        Zero if successful,
C                                                else greater than
C                                                zero.
C
C This routine searches for the help text associated with PARS(1)
C to PARS(NPAR).  If no help file is currently open, its name is
C taken from PROG if not blank, otherwise from PARS(1).  The file
C is left positioned at the last match found.
C
C     Version 1.0   3rd November, 1987   Alliant
C
C Declare the routine's arguments.
C
      INTEGER        UNIT, NPAR, MAX_LEVEL, CURRENT_LEVEL,
     &               LEVEL_POSITIONS(*), STATUS
      CHARACTER*(*)  PATH, PARS(*), PROG, LEVEL_NAMES(*), BUFFER,
     &               NAMEBUF1, NAMEBUF2, OUTBUF
      LOGICAL        FOPEN
C
C Declare external references.
C
      INTEGER        LENGTH
C
C Declare local variables.
C
      INTEGER        IP
      LOGICAL        NONE
C
C Check the current level.
C
      IF (CURRENT_LEVEL .LE. 0) THEN
C
C Zero or less: check that a file name has been given.
C
        IF (NPAR .GT. 0 .OR. PROG .NE. ' ') THEN
C
C One has been, close any open help file.
C
          IF (FOPEN) THEN
            CLOSE (UNIT = UNIT, IOSTAT = STATUS)
            FOPEN = .FALSE.
          END IF
C
C Open the one specified by PROG or by PARS(1).
C
          IF (PROG .NE. ' ') THEN
            CALL OLAF_HELP_OPEN (UNIT, PATH, PROG, FOPEN, OUTBUF,
     &                           STATUS)
C
C Did we succeed?
C
            IF (STATUS .EQ. 0) THEN
C
C Yes, search for the first topic.
C
              CALL OLAF_HELP_FIND_SUBTOPIC (UNIT, PROG, MAX_LEVEL,
     &                                      CURRENT_LEVEL,
     &                                      LEVEL_POSITIONS,
     &                                      LEVEL_NAMES, NONE, BUFFER,
     &                                      NAMEBUF1, NAMEBUF2, OUTBUF,
     &                                      STATUS)
C
C Did we find it?
C
              IF (STATUS .EQ. 0) THEN
                IF (NONE) THEN
C
C No.
C
                  STATUS = 1
                END IF
              END IF
            END IF
          ELSE
            CALL OLAF_HELP_OPEN (UNIT, PATH, PARS(1), FOPEN, OUTBUF,
     &                           STATUS)
          END IF
        ELSE
C
C No file name is provided: fail silently.
C
          STATUS = -1
        END IF
      END IF
C
C Search through the file starting at its current position until
C we run out of parameters, or find one that does not match.
C
      IP = 1
 1000 IF (IP .LE. NPAR .AND. STATUS .EQ. 0) THEN
C
C Find the requested topic at the next level.
C
        CALL OLAF_HELP_FIND_SUBTOPIC (UNIT, PARS(IP), MAX_LEVEL,
     &                                CURRENT_LEVEL, LEVEL_POSITIONS,
     &                                LEVEL_NAMES, NONE, BUFFER,
     &                                NAMEBUF1, NAMEBUF2, OUTBUF,
     &                                STATUS)
        IF (STATUS .EQ. 0) THEN
C
C Was it found?
C
          IF (NONE) THEN
C
C No, set status.
C
            STATUS = 1
          END IF
        END IF
C
C Keep looking.
C
        IP = IP + 1
        GOTO 1000
      END IF
C
C Return a sensible status.
C
      STATUS = MAX (0, STATUS)
C
C End of subroutine OLAF_HELP_FIND_INFO.
C
      END
