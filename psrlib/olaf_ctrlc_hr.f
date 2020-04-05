*DECK OLAF_CTRLC_HANDLER
C
C
C
C!      SUBROUTINE OLAF_CTRLC_HANDLER (SIGNAL, CODE)
      SUBROUTINE OLAF_CTRLC_HANDLER (SIGNAL)
C
C This routine is called when "unsafe" mode is selected and a
C software interrupt signal is received.  It offers the options
C of pausing, ignoring the signal, or of terminating the process.
C
C Arguments:
C  SIGNAL  Input  INTEGER  The signal code.
C  CODE    Input  INTEGER  Further details about the signal.
C
C     Version 2.0   18th August, 1987  Alliant FX/Fortran
C! Modified for sun by RCD, 26/3/92
C
C Declare the routine's arguments.
C
      INTEGER   SIGNAL, CODE
C
C Declare local variables.
C
      CHARACTER REPLY*10
      INTEGER   STATUS, I
C
C Define the routine's name.
C
      CHARACTER  ROUTINE*(*)
      PARAMETER (ROUTINE = 'OLAF_CTRLC_HANDLER')
C
C Warn the user that he might mangle his file if he exits now.
C
 1000 CALL OLAF_WRITE_C (
     &     'Exiting now may corrupt your files.  Do you want to exit? ',
     &     .TRUE.)
      CALL OLAF_READ_C (REPLY, STATUS)
      IF (STATUS.NE.0) GOTO 1000
C
C Check his reply.
C
      IF (REPLY(1:1).EQ.'Y' .OR. REPLY(1:1).EQ.'y') THEN
C
C It is Yes: abort.
C
c        CALL ABORT ('/OLAF/', ROUTINE, 0)
        CALL ABORT
        STOP
      END IF
C
C Print a new line to avoid messing up the output.
C
      CALL OLAF_WRITE_C (' ', .FALSE.)
C
C End of subroutine OLAF_CTRLC_HANDLER.
C
      END
