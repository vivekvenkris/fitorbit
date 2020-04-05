      SUBROUTINE UNSAFE
C
C This routine is called when an OLAF file is about to go into
C a state where it would be dangerous to allow a user interrupt.
C     Version 1.0   27th January, 1987  Alliant FX/Fortran 
C
C Include the signal definitions.
C
C!     INCLUDE '/usr/include/fortran/signal.h'
C! See "man 3v signal"...
      INTEGER SIGINT
      PARAMETER (SIGINT = 2)
C
C Declare external references.
C
C!      INTEGER  SIGVEC
      INTEGER  SIGNAL
      EXTERNAL OLAF_CTRLC_HANDLER
C
C Declare local variables.
C
      INTEGER STATUS, OLDCODE, OLDADDRESS, I, J
      SAVE    OLDCODE, OLDADDRESS
      DATA    OLDADDRESS / -1 /
C
C Has SAFE been called?
C
      IF (OLDADDRESS.EQ.-1) THEN
C
C It has, declare a handler routine for software interrupt signal.
C
C!        STATUS = SIGVEC (SIGINT, SIG_CALL, OLAF_CTRLC_HANDLER, OLDCODE,
C!     &                   OLDADDRESS)
C! See "man 3f signal"...
c        STATUS = SIGNAL (SIGINT, OLAF_CTRLC_HANDLER, -1)
        STATUS = SIGNAL (SIGINT, OLAF_CTRLC_HANDLER)
        OLDADDRESS = STATUS
      END IF
      RETURN
C
C
C The following entry point is called when the OLAF file is safe.
C
C
      ENTRY SAFE
C
C This routine is called when it is safe to abort an OLAF file.
C
C Has UNSAFE been called?
C
      IF (OLDADDRESS.NE.-1) THEN
C
C It has, restore the previous handler.
C
C!        STATUS = SIGVEC (SIGINT, OLDCODE, OLDADDRESS, I, J)
C! See "man 3f signal"...
c        STATUS = SIGNAL (SIGINT, OLAF_CTRLC_HANDLER, OLDADDRESS)
        STATUS = SIGNAL (SIGINT, OLAF_CTRLC_HANDLER)
C
C Note that the default action has been restored.
C
        OLDADDRESS = -1
      END IF
C
C End of subroutine UNSAFE.
C
      END
