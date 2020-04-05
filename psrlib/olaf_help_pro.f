*DECK OLAF_HELP_PROMPT
C
C
C
      SUBROUTINE OLAF_HELP_PROMPT (INUNIT, LEVEL_NAMES, CURRENT_LEVEL,
     &                             PROMPT, OUTBUF)
C
C This routine prompts for input for a help topic or subtopic.
C
C Arguments:
C  INUNIT         Input   INTEGER        The input unit (-1 for
C                                        standard).
C  LEVEL_NAMES    Input   CHARACTER*(*)  The level names.
C  CURRENT_LEVEL  Input   INTEGER        The current level number.
C  PROMPT         Input   CHARACTER*(*)  The prompt string.
C  OUTBUF         Output  CHARACTER*(*)  The output buffer.
C
C     Version 1.0   25th March, 1992  Sun f77
C
C Declare the routine's arguments.
C
      INTEGER       INUNIT, CURRENT_LEVEL
      CHARACTER*(*) LEVEL_NAMES(*), PROMPT, OUTBUF
C
C Declare external references.
C
C      CHARACTER     PRMTCC*1
      INTEGER       LENGTH
C
C Declare local variables.
C
      INTEGER       MAX_BUF, IC, I
C
C Check the input unit number: we only issue a prompt if reading
C from standard input.
C
      IF (INUNIT .LE. -1) THEN
C
C Load the output buffer with the level names.
C
        MAX_BUF = MIN (80, LEN (OUTBUF))
        I = 1
        IC = 1
        OUTBUF = ' '
 1000   IF (I .LE. CURRENT_LEVEL .AND. IC .LE. MAX_BUF) THEN
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
C Issue the prompt.
C
        IF (LENGTH (OUTBUF) .EQ. 1) THEN
          WRITE (*, 100) OUTBUF(1:1), PROMPT
        ELSE
          WRITE (*, 100) OUTBUF(1:IC-1), PROMPT
        END IF
      END IF
C
C Format statement.
C
  100 FORMAT (/2A,$)
C
C End of subroutine OLAF_HELP_PROMPT.
C
      END
