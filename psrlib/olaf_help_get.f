*DECK OLAF_HELP_GET_INPUT
C
C
C
      SUBROUTINE OLAF_HELP_GET_INPUT (INUNIT, MAX_PARS, PARS, NPAR,
     &                                BUFFER, STATUS)
C
C This routine obtains the user's response to a help prompt and
C splits it into the the array PARS in lower case.
C
C Arguments:
C  INUNIT    Input   INTEGER        The input unit number, -1 fo
C                                   standard input.
C  MAX_PARS  Input   INTEGER        The maximum number of parameters
C                                   allowed.
C  PARS      Output  CHARACTER*(*)  The parameters array.
C  NPAR      Output  INTEGER        The number of parameters.
C  BUFFER    Output  CHARACTER*(*)  The input buffer.
C  STATUS    Output  INTEGER        Zero if successful, else greater
C                                   than zero or less than zero if end
C                                   of file was encountered.
C
C     Version 1.0   4th November, 1987
C
C Declare the routine's arguments.
C
      INTEGER       INUNIT, MAX_PARS, NPAR, STATUS
      CHARACTER*(*) PARS(*), BUFFER
C
C Declare external references.
C
      LOGICAL       ISATTY
C
C Declare local variables.
C
      INTEGER       PS, PF
C
C Obtain a line of input.
C
      CALL RDWLTH (INUNIT, BUFFER, LEN (BUFFER), PS, STATUS)
      IF (STATUS .EQ. 0) THEN
C
C Split this into separate parameters.
C
        PS = 1
        PF = 0
        NPAR = 1
 1000   IF (PS .GT. 0 .AND. NPAR .LE. MAX_PARS) THEN
C
C Obtain the current parameter.
C
          CALL OLAF_HELP_SPLIT_INPUT (BUFFER, PF+1, PS, PF)
C
C Is there one?
C
          IF (PS .GT. 0) THEN
C
C Yes, load it into PARS.
C
            PARS(NPAR) = BUFFER(PS:PF)
            NPAR = NPAR + 1
          END IF
C
C Do the next parameter.
C
          GOTO 1000
        END IF
C
C Set the correct number of parameters.
C
        NPAR = NPAR - 1
C
C Convert them all into lower case.
C
        CALL LOCASE (PARS, 1, NPAR)
      ELSE IF (STATUS .LT. 0) THEN
C
C End of file was encountered: if the input unit is a terminal send a
C new line to it to keep things tidy.
C
        IF (INUNIT .EQ. -1) THEN
          IF (ISATTY (0)) THEN
            WRITE (*, 100)
          END IF
        END IF
      END IF
C
C Format statement.
C
  100 FORMAT ()
C
C End of subroutine OLAF_HELP_GET_INPUT.
C
      END
