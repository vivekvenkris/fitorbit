*DECK OLAF_HELP_SPLIT_INPUT
C
C
C
      SUBROUTINE OLAF_HELP_SPLIT_INPUT (BUFFER, START_AT, START, END)
C
C This routine splits a space separated parameter from the contents of
C BUFFER.
C
C Arguments:
C  BUFFER    Input   CHARACTER*(*)  The input buffer.
C  START_AT  Input   INTEGER        The starting location in BUFFER.
C  START     Output  INTEGER        The location of the start of the
C                                   parameter in BUFFER, -1 if there is
C                                   none.
C  END       Output  INTEGER        The location of the end of the
C                                   parameter in BUFFER.
C
C     Version 1.0   4th November, 1987
C
C Declare the routine's arguments.
C
      CHARACTER BUFFER*(*)
      INTEGER   START_AT, START, END
C
C Declare external references.
C
      INTEGER   LENGTH
C
C Declare local variables.
C
      INTEGER   BUFLEN, I, J
C
C Obtain the effective length of the buffer.
C
      BUFLEN = LENGTH (BUFFER)
C
C Search for the start of the parameter.
C
      DO 1 I = START_AT, BUFLEN
        IF (BUFFER(I:I) .NE. ' ') THEN
C
C Found it: locate its end.
C
          START = I
          DO 2 J = I+1, BUFLEN
            IF (BUFFER(J:J) .EQ. ' ') GOTO 1000
    2     CONTINUE
C
C Got the end.
C
 1000     END = J - 1
          GOTO 7777
        END IF
    1 CONTINUE
C
C There is no parameter.
C
      START = -1
C
C End of subroutine OLAF_HELP_SPLIT_INPUT.
C
 7777 END
