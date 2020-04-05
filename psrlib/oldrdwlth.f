      SUBROUTINE RDWLTH (IUNIT, IPBUF, MAXBUF, NC, IFAIL)
C
C This routine reads a record into a buffer and returns the
C actual record length.
C
C Arguments:
C  IUNIT   Input   INTEGER             The unit number to read from,
C                                      -ve for standard input.
C  IPBUF   Output  CHARACTER*(MAXBUF)  A buffer to receive the record,
C                                      the read record is truncated if
C                                      NC > MAXBUF.
C  MAXBUF  Input   INTEGER             The maximum size of the buffer.
C  NC      Output  INTEGER             The number of characters in the
C                                      record.
C  IFAIL   Output  INTEGER             The status return from the read:
C                                      zero if successful, negative if
C                                      end of file was encountered,
C                                      positive for any other error.
C
C     This generic version always returns a value of NC that is equal
C to the number of characters in IPBUF ignoring trailing spaces.
C
C     Version 1.2 8-8-90 mxb for screen handler
C
C Declare the routine's arguments.
C
        CHARACTER*(*) IPBUF
        INTEGER       IUNIT, MAXBUF, NC, IFAIL
C
C Declare external references.
C
        INTEGER       LENGTH
C
        LOGICAL INIT,SCREEN
C
        DATA INIT,SCREEN / .FALSE.,.FALSE./
        COMMON/SCR_HND/ INIT,SCREEN
C
C Test the unit number.
C
      IF (IUNIT .LT. 0) THEN
C
C Read from the default unit.
        IF (SCREEN .AND. (.NOT. INIT)) THEN
           CALL INIT_SCREEN
           INIT = .TRUE.
        END IF
        IF (SCREEN) THEN
         CALL SCREEN_HANDLER(IPBUF)
         IFAIL = 0
        ELSE
         READ (*, 100, IOSTAT = IFAIL) IPBUF(:MAXBUF)
        END IF
      ELSE
C
C Read from unit IUNIT.
C
        READ (IUNIT, 100, IOSTAT = IFAIL) IPBUF(:MAXBUF)
      ENDIF
C
C Return the number of characters in IPBUF, but ignoring any trailing
C spaces.
C
      NC = LENGTH (IPBUF(:MAXBUF))
C
C Format statement.
C
  100 FORMAT (A)
C
C End of subroutine RDWLTH.
C
      END
