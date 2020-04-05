      SUBROUTINE OLAF_WRITE_C (TEXT, SUPPRESS)
C
C This routine writes a text string to standard output using
C the C-type routines, hence bypassing normal FORTRAN I/O.
C
C Arguments:
C  TEXT      Input  CHARACTER*(*)  The string to write.
C  SUPPRESS  Input  LOGICAL        .TRUE. to suppress a
C                                  newline after the string.
C
C     Version 1.0   17th August, 1987   Alliant FX/FORTRAN
C
C Declare the routine's arguments.
C
      CHARACTER TEXT*(*)
      LOGICAL   SUPPRESS
C
C Declare external references.
C
      INTEGER   PUTC
C
C Declare local variables.
C
      INTEGER   I, STATUS
C
C Send a line feed.
C
      STATUS = PUTC (CHAR (10))
C
C Check the string.
C
      IF (TEXT.NE.' ') THEN
C
C There is something to write.
C
        DO 1 I = 1, LEN(TEXT)
          STATUS = PUTC (TEXT(I:I))
    1   CONTINUE
      END IF
C
C Send a carriage return if not suppressed.
C
      IF (.NOT.SUPPRESS) THEN
        STATUS = PUTC (CHAR (13))
      END IF
C
C Make sure that everthing is printed.
C
      CALL FLUSH (6)
C
C End of subroutine OLAF_WRITE_C.
C
      END
