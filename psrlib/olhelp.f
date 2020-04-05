*DECK OLHELP
C
C
C
      SUBROUTINE OLHELP (INUNIT, MAX_PARS, PARS, NPAR, OUTBUF, PROG,
     &                   IFAIL)
C
C This routine prints out help information on the requested topics
C in pars.  It is installation dependent.
C
C Arguments:
C  INUNIT    Input         INTEGER        The current command input unit
C                                         (-1 for standard input).
C  MAX_PARS  Input         INTEGER        The maximum number of
C                                         parameters.
C  PARS      Input/output  CHARACTER*(*)  The parameters from the
C                                         command line.
C  NPAR      Input         INTEGER        The number of these.
C  OUTBUF    Output        CHARACTER*(*)  The output buffer.
C  PROG      Input         CHARACTER*(*)  The name of the calling
C                                         program.
C  IFAIL     Output        INTEGER        Zero if successful, else
C                                         greater than zero.
C
C This routine reads help files in the same format as those used by the
C VAX/VMS help facility, except that each main section is in a separate
C file, rather than all sections being formed into a library.
C
C     Version 2.0   3rd November, 1987   Alliant
C! Modified for sun by RCD, 24/3/92
C
C Declare the routine's arguments.
C
      INTEGER       INUNIT, NPAR, IFAIL
      CHARACTER*(*) PARS(*), OUTBUF, PROG, FILE1
C
C Declare external references.
C
      LOGICAL       BATCH
C
C Define the maximum level of help nesting and the maximum length of
C a level name.
C
      INTEGER       MAX_LEVEL, MAX_NAME
      PARAMETER    (MAX_LEVEL = 64,
     &              MAX_NAME = 64)
C
C Declare local variables.
C
      INTEGER       ISTAT, CURRENT_LEVEL, LEVEL_POSITIONS(MAX_LEVEL),
     &              NPARS
      LOGICAL       NONE, FOPEN, PRINT
      CHARACTER     LEVEL_NAMES(MAX_LEVEL)*(MAX_NAME), BUFFER*132,
     &              FILEBUF*128, NAMEBUF1*(MAX_NAME),
     &              NAMEBUF2*(MAX_NAME), LPROG*32, PATH*64
C
C Define the help directory environment variable.
C
      CHARACTER     HELP_DIR_ENVIR*(*)
      PARAMETER    (HELP_DIR_ENVIR = 'PSRHELPDIR')
C
C Define the default help directory and the unit number to use for
C reading the help files.  Note that HELP_UNIT - 1 is also used.
C
      CHARACTER     HELP_DIR*64
      INTEGER       HELP_UNIT
      SAVE          HELP_DIR
      PARAMETER    (HELP_UNIT = 99)
C!      DATA          HELP_DIR / '/usr1/olaf/help/' /
C! On sun...
      DATA          HELP_DIR / '/psr/help/' /
C
C Test if in batch mode.
C
      IF (BATCH()) THEN
C
C Help is not supported in batch mode.
C
        CALL OUTLIN ('      No help is available in batch mode', -1)
        IFAIL = 1
      ELSE
C
C Obtain the help directory path name from the environment variable.
C
        CALL GETENV (HELP_DIR_ENVIR, PATH)
C
C Is there one?
C
        IF (PATH .EQ. ' ') THEN
C
C No, use the default one.
C
          PATH = HELP_DIR
        END IF
C
C Convert the input parameters into lower case.
C
        CALL LOCASE (PARS, 1, NPAR)
C
C Convert the program name into lower case.
C
        LPROG = PROG
        CALL LOCASE (LPROG, 1, 1)
C
C Get to the requested information.
C
        CURRENT_LEVEL = 0
        FOPEN = .FALSE.
        CALL OLAF_HELP_FIND_INFO (HELP_UNIT, PATH, PARS, NPAR,
     &                            LPROG, MAX_LEVEL, CURRENT_LEVEL,
     &                            LEVEL_POSITIONS, LEVEL_NAMES, FOPEN,
     &                            BUFFER, NAMEBUF1, NAMEBUF2, OUTBUF,
     &                            ISTAT)
C
C Print the initial help information.  First check the current level.
C
        PRINT = .TRUE.
 1000   IF (ISTAT .EQ. 0 .AND. CURRENT_LEVEL .GE. 0) THEN
          IF (CURRENT_LEVEL .EQ. 0) THEN
C
C Zero level, print the list of topics available.
C
            IF (PRINT) CALL OLAF_HELP_PRINT_TOPICS (PATH, ISTAT)
            IF (ISTAT .EQ. 0) THEN
C
C Prompt for action.
C
              CALL OLAF_HELP_PROMPT (INUNIT, LEVEL_NAMES, CURRENT_LEVEL,
     &                               'Topic? ', OUTBUF)
            END IF
          ELSE
C
C Not the zero level, print the current level's text and subtopics.
C
            IF (PRINT) THEN
              CALL OLAF_HELP_PRINT_LEVEL (HELP_UNIT, LEVEL_NAMES,
     &                                    CURRENT_LEVEL,
     &                                    LEVEL_POSITIONS, NONE, BUFFER,
     &                                    FILEBUF, NAMEBUF1, OUTBUF,
     &                                    ISTAT)
            ELSE
              NONE = .FALSE.
            END IF
            IF (ISTAT .EQ. 0) THEN
C
C Are there any subtopics?
C
              IF (NONE) THEN
C
C No, drop back a level.
C
                CALL OLAF_HELP_BACK (HELP_UNIT, CURRENT_LEVEL,
     &                               LEVEL_POSITIONS, FOPEN, ISTAT)
C
C Suppress printing.
C
                PRINT = .FALSE.
              END IF
C
C Prompt for action.
C
              IF (CURRENT_LEVEL .EQ. 0) THEN
                CALL OLAF_HELP_PROMPT (INUNIT, LEVEL_NAMES,
     &                                 CURRENT_LEVEL, 'Topic? ', OUTBUF)
              ELSE IF (CURRENT_LEVEL .GT. 0) THEN
                CALL OLAF_HELP_PROMPT (INUNIT, LEVEL_NAMES,
     &                                 CURRENT_LEVEL, 'Subtopic? ',
     &                                 OUTBUF)
              END IF
            END IF
          END IF
C
C Test if successful so far.
C
 2000     IF (ISTAT .EQ. 0) THEN
C
C Yes, obtain the topic or subtopic requested.
C
            CALL OLAF_HELP_GET_INPUT (INUNIT, MAX_PARS, PARS, NPARS,
     &                                BUFFER, ISTAT)
            IF (ISTAT .EQ. 0) THEN
C
C Was anything typed?
C
              IF (NPARS .LE. 0) THEN
C
C No, drop back a level.
C
                CALL OLAF_HELP_BACK (HELP_UNIT, CURRENT_LEVEL,
     &                               LEVEL_POSITIONS, FOPEN, ISTAT)
C
C Suppress printing.
C
                PRINT = .FALSE.
              ELSE
C
C Something was typed, what was it?
C
                IF (PARS(1) .NE. '?') THEN
C
C It is not a question mark: attempt to locate the requested topic or
C subtopic.
C
                  IF (CURRENT_LEVEL .EQ. 0) THEN
C
C Topic: attempt to open the requested file.
C
                    CALL OLAF_HELP_OPEN (HELP_UNIT, PATH, PARS(1),
     &                                   FOPEN, OUTBUF, ISTAT)
                    IF (ISTAT .EQ. 0) THEN
C
C Set the new help level.
C
                      CURRENT_LEVEL = 1
C
C Set the topic's name.
C
                      LEVEL_NAMES(1) = PARS(1)
                    ELSE
C
C There is no such topic: ask again.
C
                      ISTAT = 0
                      CALL OLAF_HELP_PROMPT (INUNIT, LEVEL_NAMES,
     &                                       CURRENT_LEVEL, 'Topic? ',
     &                                       OUTBUF)
                      GOTO 2000
                    END IF
                  ELSE
C
C Subtopic: attempt to find it.
C
                    CALL OLAF_HELP_FIND_SUBTOPIC (HELP_UNIT, PARS(1),
     &                                            MAX_LEVEL,
     &                                            CURRENT_LEVEL,
     &                                            LEVEL_POSITIONS,
     &                                            LEVEL_NAMES, NONE,
     &                                            BUFFER, NAMEBUF1,
     &                                            NAMEBUF2, OUTBUF,
     &                                            ISTAT)
                    IF (ISTAT .EQ. 0) THEN
C
C Was the subtopic found?
C
                      IF (NONE) THEN
C
C No, ask again.
C
                        CALL OLAF_HELP_PROMPT (INUNIT, LEVEL_NAMES,
     &                                         CURRENT_LEVEL,
     &                                         'Subtopic? ', OUTBUF)
                        GOTO 2000
                      END IF
                    END IF
                  END IF
                END IF
C
C Allow printing.
C
                PRINT = .TRUE.
              END IF
            END IF
          END IF
C
C Keep going round.
C
          GOTO 1000
        END IF
C
C Return IFAIL.
C
        IFAIL = MAX (0, ISTAT)
C
C Close any open help file.
C
        CLOSE (UNIT = HELP_UNIT, IOSTAT = ISTAT)
      END IF
      RETURN
C
C
C The following entry point can be used to set a different help
C directory name.
C
C
      ENTRY SETHLP (FILE1)
C
C Define a new help directory name.
C Input argument:
C  FILE1  - The name of the directory containing the help data.
C
C Set the directory name.
C
      HELP_DIR = FILE1
C
C End of subroutine OLHELP.
C
      END
