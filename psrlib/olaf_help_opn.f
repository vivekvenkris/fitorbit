*DECK OLAF_HELP_OPEN
C
C
C
      SUBROUTINE OLAF_HELP_OPEN (UNIT, PATH, FILE, FOPEN, OUTBUF,
     &                           STATUS)
C
C This routine opens an help file.
C
C Arguments:
C  UNIT    Input   INTEGER        The unit number for the file.
C  PATH    Input   CHARACTER*(*)  The path name of the help directory.
C  FILE    Input   CHARACTER*(*)  The help file name.
C  FOPEN   Output  LOGICAL        .TRUE. if the file was opened.
C  OUTBUF  Output  CHARACTER*(*)  The output buffer.
C  STATUS  Output  INTEGER        Zero if successful, else greater than
C                                 zero.
C
C     Version 1.0   4th November, 1987
C
C Declare the routine's arguments.
C
      CHARACTER*(*) PATH, FILE, OUTBUF
      INTEGER       UNIT, STATUS
      LOGICAL       FOPEN
C
C Declare external references.
C
      INTEGER       LENGTH
C
C Attempt to open the file.
C
      OPEN (UNIT = UNIT, FILE = PATH(1:LENGTH(PATH))//FILE,
     &      STATUS = 'OLD', action='READ', IOSTAT = STATUS)
c     &      STATUS = 'OLD', READONLY, IOSTAT = STATUS)
C
C Test if successful.
C
      IF (STATUS .EQ. 0) THEN
C
C Yes, note that a file is open.
C
        FOPEN = .TRUE.
      ELSE
C
C No, say that the topic does not exist.
C
        CALL OLAF_HELP_NO_INFO (' ', 0, FILE, OUTBUF)
      END IF
C
C End of subroutine OLAF_HELP_OPEN.
C
      END
