*DECK OLAF_HELP_PRINT_LEVEL
C
C
C
      SUBROUTINE OLAF_HELP_PRINT_LEVEL (UNIT, LEVEL_NAMES,
     &                                  CURRENT_LEVEL, LEVEL_POSITIONS,
     &                                  NONE, BUFFER, FILEBUF, NAMEBUF,
     &                                  OUTBUF, STATUS)
C
C This routine prints the current level's help text, followed by a
C list of the subtopics if any.
C
C Arguments:
C  UNIT             Input   INTEGER        The help file unit number.
C  LEVEL_NAMES      Input   CHARACTER*(*)  The list of level names.
C  CURRENT_LEVEL    Input   INTEGER        The current level number.
C  LEVEL_POSITIONS  Input   INTEGER        The positions of the levels
C                                          in the help file.
C  NONE             Output  LOGICAL        .TRUE. if there are any
C                                          subtopics, else .FALSE..
C  BUFFER           Output  CHARACTER*(*)  The input buffer.
C  FILEBUF          Output  CHARACTER*(*)  The file name buffer.
C  NAMEBUF          Output  CHARACTER*(*)  The subtopic name buffer.
C  OUTBUF           Output  CHARACTER*(*)  The output buffer.
C  STATUS           Output  INTEGER        Zero if successful, else
C                                          greater than zero.
C
C     Version 1.0   4th November, 1987   Alliant
C     Mod 2007 - Remove non-standard Carriagecontrol qualifier on OPEN CAJ
C Declare the routine's arguments.
C
      INTEGER        UNIT, CURRENT_LEVEL, LEVEL_POSITIONS(*), STATUS
      CHARACTER*(*)  LEVEL_NAMES(*), BUFFER, FILEBUF, NAMEBUF, OUTBUF
      LOGICAL        NONE
C
C Declare external references.
C
      INTEGER    LENGTH, FSEEK
C
C Declare local variables.
C
      INTEGER    SUNIT, LEVEL, IC, SUB_LEVEL, MAX_OUT
      LOGICAL    NEW
C
C Define the increment between subtopic names on the output line.
C
      INTEGER    SUBINC
      PARAMETER (SUBINC = 15)
C
C Set the maximum output buffer size.
C
      MAX_OUT = MIN (LEN (OUTBUF), 80)
C
C Open a scratch file to receive the help output.
C
      SUNIT = UNIT - 1
      OPEN (UNIT = SUNIT, STATUS = 'SCRATCH', 
c     &                          CARRIAGECONTROL = 'LIST',
     &      IOSTAT = STATUS)
      IF (STATUS .EQ. 0) THEN
C
C Obtain its name.
C
        INQUIRE (UNIT = SUNIT, NAME = FILEBUF, IOSTAT = STATUS)
        IF (STATUS .EQ. 0) THEN
C
C First write the header to the file.
C
          WRITE (UNIT = SUNIT, FMT = 100, IOSTAT = STATUS)
     &          (LEVEL_NAMES(I)(1:LENGTH(LEVEL_NAMES(I))),
     &           I = 1, CURRENT_LEVEL)
          IF (STATUS .EQ. 0) THEN
            WRITE (UNIT = SUNIT, FMT = 101, IOSTAT = STATUS)
            IF (STATUS .EQ. 0) THEN
C
C Skip the current line on the help file (it has got the level name on
C it.
C
              READ (UNIT = UNIT, FMT = 101, IOSTAT = STATUS)
C
C Now read from the help file and write to the scratch file until we
C come across a new level (or end of file).
C
              NEW = .FALSE.
 1000         IF (STATUS .EQ. 0 .AND. .NOT.NEW) THEN
C
C Read the current line from the help file.
C
                READ (UNIT = UNIT, FMT= 201, IOSTAT = STATUS) BUFFER
                IF (STATUS .EQ. 0) THEN
C
C Is it the start of a new level?
C
                  CALL OLAF_HELP_CHECK_LEVEL (BUFFER, NEW, LEVEL,
     &                                        NAMEBUF)
                  IF (.NOT.NEW) THEN
C
C It is not: print the line.
C
                    WRITE (UNIT = SUNIT, FMT = 201, IOSTAT = STATUS)
     &                    BUFFER(1:LENGTH(BUFFER))
                  END IF
                END IF
C
C Read the next record.
C
                GOTO 1000
              END IF
C
C Test if successful.
C
              IF (STATUS .EQ. 0) THEN
C
C Yes, was a new level encountered?
C
                IF (NEW) THEN
C
C One was, is it below the current level?
C
                  IF (LEVEL .GT. CURRENT_LEVEL) THEN
C
C It is: print a heading for the subtopics.
C
                    WRITE (UNIT = SUNIT, FMT = 200, IOSTAT = STATUS)
     &                    'Additional information is available on:'
C
C Note that there are some subtopics.
C
                    NONE = .FALSE.
C
C Now keep going through the help file loading and printing any new
C subtopics that we find at the present subtopic's level until we
C find one at CURRENT_LEVEL or less, or until we drop off the end of
C the file.
C
                    SUB_LEVEL = LEVEL
                    IC = 1
 2000               IF (STATUS .EQ. 0 .AND.
     &                  SUB_LEVEL .GT. CURRENT_LEVEL) THEN
C
C Is the current subtopic at the right level?
C
                      IF (SUB_LEVEL .EQ. LEVEL) THEN
C
C Load the output buffer with the current subtopic's name.  First check
C if there will be room for it.
C
                        IF (IC+LENGTH(NAMEBUF) .GE. MAX_OUT) THEN
C
C There is not: print the current contents of the buffer.
C
                          WRITE (UNIT = SUNIT, FMT = 201,
     &                           IOSTAT = STATUS)
     &                       OUTBUF(1:LENGTH(OUTBUF))
                          IC = 1
                        END IF
C
C Now load the current name into the buffer.
C
                        OUTBUF(IC:) = NAMEBUF
C
C Work out where the next name is to go.
C
                        IC = LENGTH (OUTBUF) + 3
                        IF (MOD (IC - 1, SUBINC) .NE. 0) THEN
                          IC = ((IC - 1) / SUBINC + 1) * SUBINC + 1
                        END IF
                      END IF
C
C Read through the file until we find a new level.
C
                      NEW = .FALSE.
 2001                 IF (STATUS .EQ. 0 .AND. .NOT.NEW) THEN
C
C Read the next record.
C
                        READ (UNIT = UNIT, FMT = 201, IOSTAT = STATUS)
     &                       BUFFER
                        IF (STATUS .EQ. 0) THEN
C
C Is it the start of a new level?
C
                          CALL OLAF_HELP_CHECK_LEVEL (BUFFER, NEW,
     &                                                SUB_LEVEL,
     &                                                NAMEBUF)
                        END IF
                        GOTO 2001
                      END IF
C
C Load the new level's name.
C
                      GOTO 2000
                    END IF
C
C Test if successful so far.
C
                    IF (STATUS .LE. 0) THEN
C
C Print the output buffer.
C
                      WRITE (UNIT = SUNIT, FMT = 201, IOSTAT = STATUS)
     &                      OUTBUF(1:LENGTH(OUTBUF))
                    END IF
                  ELSE
C
C There are no subtopics.
C
                    NONE = .TRUE.
                  END IF
                ELSE
C
C There are no subtopics.
C
                  NONE = .TRUE.
                END IF
              ELSE
C
C There are no subtopics.
C
                NONE = .TRUE.
              END IF
C
C Test if successful.
C
              IF (STATUS .LE. 0) THEN
C
C Yes, reposition the help file back to where it was.
C
                IF (LEVEL_POSITIONS(CURRENT_LEVEL) .EQ. 0) THEN
                  REWIND (UNIT = UNIT, IOSTAT = STATUS)
                ELSE
                  STATUS = FSEEK (UNIT, LEVEL_POSITIONS(CURRENT_LEVEL),
     &                            0)
                END IF
                IF (STATUS .EQ. 0) THEN
C
C Print the scratch file on the terminal.
C
                  CALL GIVEOS ('more '//FILEBUF, STATUS)
                END IF
              END IF
            END IF
          END IF
        END IF
C
C Close the scratch file.
C
        CLOSE (UNIT = SUNIT, IOSTAT = STATUS)
      END IF
C
C Format statements.
C
  100 FORMAT (/100(A: 1X))
  101 FORMAT ()
  200 FORMAT (/A/)
  201 FORMAT (A)
C
C End of subroutine OLAF_HELP_PRINT_LEVEL.
C
      END
