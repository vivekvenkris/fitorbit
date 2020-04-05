      SUBROUTINE OLAF_READ_C (TEXT, STATUS)
C
C This routine reads a record from standard input using
C the C-type routines, thus bypassing normal FORTRAN I/O.
C
C Arguments:
C  TEXT    Output  CHARACTER*(*)  The record read.
C  STATUS  Output  INTEGER        The status of the read:
C                                   0 if successful
C                                  -1 if EOF
C                                  >0 if failed
C
C     Version 1.0   18th August, 1987   Alliant FX/FORTRAN
C
C Declare the routine's arguments.
C
      CHARACTER TEXT*(*)
      INTEGER   STATUS
C
C Declare external references.
C
      INTEGER GETC
C
C Declare local variables.
C
      INTEGER   I, LENGTH
C
C Read until either a new line is encountered, TEXT
C is full, or some sort of error happens.
C
      LENGTH = LEN (TEXT)
      I = 1
      TEXT = ' '
      STATUS = GETC (TEXT(1:1))
 1000 IF (I.LT.LENGTH .AND. STATUS.EQ.0 .AND.
     &    TEXT(I:I).NE.CHAR(10)) THEN
C
C Obtain the next character.
C
        I = I + 1
        STATUS = GETC (TEXT(I:I))
        GOTO 1000
      END IF
C
C Remove any new line.
C
      IF (TEXT(I:I).EQ.CHAR(10)) TEXT(I:I) = ' '
C
C End of subroutine OLAF_READ_C.
C
      END
