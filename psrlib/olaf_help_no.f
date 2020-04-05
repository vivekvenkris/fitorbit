*DECK OLAF_HELP_NO_INFO
C
C
C
      SUBROUTINE OLAF_HELP_NO_INFO (LEVEL_NAMES, CURRENT_LEVEL, TOPIC,
     &                              OUTBUF)
C
C This routine prints a message when the specified help
C topic or subtopic could not be found.
C
C Arguments:
C  LEVEL_NAMES    Input   CHARACTER*(*)  The level names.
C  CURRENT_LEVEL  Input   INTEGER        The current level.
C  TOPIC          Input   CHARACTER*(*)  The topic or subtopic.
C  OUTBUF         Output  CHARACTER*(*)  The output buffer.
C
C     Version 1.0   4th November, 1987
C
C Declare the routine's arguments.
C
      CHARACTER*(*) LEVEL_NAMES(*), TOPIC, OUTBUF
      INTEGER       CURRENT_LEVEL
C
C Declare external references.
C
      INTEGER       LENGTH
C
C Declare local variables.
C
      INTEGER       MAX_BUF, I, IC
C
C Load the output buffer with the level names.
C
      MAX_BUF = MIN (80, LEN (OUTBUF)) - LENGTH (TOPIC) - 1
      I = 1
      OUTBUF = ' Sorry, there is no information on'
      IC = LENGTH (OUTBUF) + 2
 1000 IF (I .LE. CURRENT_LEVEL  .AND. IC .LE. MAX_BUF) THEN
C
C Is there room for the level name?
C
        IF (IC + LENGTH (LEVEL_NAMES(I)) .LE. MAX_BUF) THEN
C
C There is: load it.
C
          OUTBUF(IC:) = LEVEL_NAMES(I)
          IC = LENGTH (OUTBUF) + 2
        END IF
C
C Do the next level.
C
        I = I + 1
        GOTO 1000
      END IF
C
C Print the warning.
C
      OUTBUF(IC:) = TOPIC
      CALL OUTLIN (OUTBUF, -1)
C
C End of subroutine OLAF_HELP_NO_INFO.
C
      END
