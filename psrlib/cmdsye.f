*DECK CMDSYE
C
C
C
      SUBROUTINE CMDSYE (PARS, OUTBUF, IPBUF, NC, SILFLG, PRMPT, EPRMPT,
     &                   FIRST, IS, IF, IERR, IPAR, IFAIL)
C
C This routine monitors a command syntax error detected in GETCMD.
C
C Arguments:
C  PARS    Input/output  CHARACTER*(*)  The parameters array.  On exit,
C                                       PARS(1) is set to the faulty
C                                       parameter.
C  OUTBUF  Output        CHARACTER*(*)  The output buffer.
C  IPBUF   Input         CHARACTER*(*)  The input buffer containing the
C                                       error.
C  NC      Input         INTEGER        The number of characters in the
C                                       input buffer.
C  SILFLG  Input         LOGICAL        .TRUE. if command line echoing
C                                       is disabled.
C  PRMPT   Input         CHARACTER*(*)  The command prompt.
C  EPRMPT  Input         CHARACTER*(*)  The effective command prompt.
C  FIRST   Input         LOGICAL        .TRUE. if the current command is
C                                       the first one on a line.
C  IS      Input         INTEGER        The location of the start of the
C                                       faulty parameter in IPBUF.
C  IF      Input         INTEGER        The location of the fault in
C                                       IPBUF.
C  IERR    Input         INTEGER        The error number.
C  IPAR    Input         INTEGER        An integer parameter for the
C                                       reporting.
C  IFAIL   Output        INTEGER        The error number.
C
C     Version 1.1   12th July, 1988
C
C Declare the routine's arguments.
C
      CHARACTER*(*) PARS(*), OUTBUF, IPBUF, PRMPT, EPRMPT
      INTEGER       NC, IS, IF, IERR, IPAR, IFAIL
      LOGICAL       SILFLG, FIRST
C
C Declare external references.
C
      INTEGER       LENGTH
      LOGICAL       BATCH
C
C Declare local variables.
C
      INTEGER       L
C
C Define the indicator character used to mark the error location.
C
      CHARACTER*(*) INDIC
      PARAMETER    (INDIC = '^')
C
C Define the routine's name.
C
      CHARACTER*(*) ROUTN
      PARAMETER    (ROUTN = 'CMDSYE')
C
C Obtain the status of the error.
C
      CALL GETLER (IERR, L)
      IF (L .NE. 1) THEN
C
C It is fatal or non-fatal and print, print the command line if the user
C does not know what it looks like.
C
        IF (SILFLG .AND. BATCH()) THEN
          OUTBUF = EPRMPT(:LENGTH (EPRMPT)) // ' ' // IPBUF(:NC)
          CALL OUTLIN (OUTBUF, -1)
        END IF
C
C Is the current command the first one on a line?
C
        IF (FIRST) THEN
C
C It is: work out what the printed line is prefixed with.
C
          IF (.NOT. SILFLG) THEN
C
C The 'echo' prompt.
C
            L = LENGTH (EPRMPT)
          ELSE IF (.NOT. BATCH()) THEN
C
C The command prompt.
C
            L = LENGTH (PRMPT) + 1
          ELSE
C
C The 'echo' prompt.
C
            L = LENGTH (EPRMPT)
          END IF
C
C Is the output buffer large enough to indicate where the error is?
C
          IF (LEN (OUTBUF) .GT. L + IF) THEN
C
C Yes, do so.
C
            OUTBUF = ' '
            OUTBUF(L+IF+1:) = INDIC
            CALL OUTLIN (OUTBUF, -1)
C
C Flag the error.
C
            CALL LIBERR (ROUTN, IERR, 0, IPAR, 0.0, INDIC, IFAIL)
          ELSE
C
C No, just flag the error.
C
            CALL LIBERR (ROUTN, IERR, 1, IPAR, 0.0, IPBUF(IS:IF), IFAIL)
          END IF
        ELSE
C
C The command is the second or subsequent one on a command line - just
C flag the error.
C
          CALL LIBERR (ROUTN, IERR, 2, IPAR, 0.0, IPBUF(IS:IF), IFAIL)
        END IF
      ELSE
C
C The error is non-fatal, just flag it.
C
        CALL LIBERR (ROUTN, IERR, 3, IPAR, 0.0, IPBUF(IS:IF), IFAIL)
      END IF
C
C Return the faulty parameter in PARS(1).
C
      PARS(1) = IPBUF(IS:IF)
C
C End of subroutine CMDSYE.
C
      END
