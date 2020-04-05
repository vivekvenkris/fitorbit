c *********************************************************************
      SUBROUTINE WRITEPH(NAMEINEPH, PULSAR_NAME,VALS,ERRS)
c *********************************************************************
c
c Subroutine to write an ephemeris file.
c
c Written by F.H.Jowett & D.Law-Green. October/November 1992.
c Jan 1999    CAJ Writes new (parkes style) eph files
c Nov 2001    Add BINARY BT line to eph.
c Turn off implicit naming conventions
      IMPLICIT NONE

c  new .eph file definitions
      include 'keyinfo.com'
      integer wr_eph
      INTEGER parmStatus(NUM_KEYS)
      CHARACTER*32 value_str(NUM_KEYS)
      REAL*8 value_double(NUM_KEYS)
      INTEGER value_integer(NUM_KEYS)
      REAL*8 error_double(NUM_KEYS)
c
c Declare variables:
c Name of pulsar:
      CHARACTER*15 NAMEINEPH

c Ephemeris file name
      CHARACTER*40 PULSAR_NAME
      CHARACTER*100 EPH_NAME
c The number of keywords
      INTEGER NO_KEYWORDS
      PARAMETER (NO_KEYWORDS=64)

c Logical flag for checking file existence
      LOGICAL LEXIST
c Variable which contains error number if error occurs opening file
      INTEGER FILE_ERROR
c Variables used to obtain available file unit number
      INTEGER FILE_UNIT,GETLUN
c String containing an error statement, which is then written to screen
      CHARACTER*80 ERROR

c Used to enter a menu choice.
      INTEGER CHOICE
      character*1 schoice
c Integer declaration to enable subroutine LENGTH to be used.
      INTEGER LENGTH, IFAIL
c Character string used to verify if the user wants to perform an action.
      CHARACTER*3 SURE
c Character string used when outputting a string to the screen.
      CHARACTER*80 OUTLINE
c Character string used to pass a UNIX command to the system.
      CHARACTER*80 COMMAND
c String containing a new ephemeris name (if any).
      CHARACTER*100 NEW_EPH
c String containing directory path
      CHARACTER*60 DIRECTORY
c Arrays used to return the values [VALS] and errors [ERRS]. These
c   have been split from VALUES to enable easier future integration
c   with C++ programs which handle two-dimensional arrays differently
c   to FORTRAN.
      DOUBLE PRECISION VALS(NO_KEYWORDS)
      DOUBLE PRECISION ERRS(NO_KEYWORDS)
      logical parint
c
      integer chlen,i

c Open the ephem file.
c Get the directory path from the environment variable
      CALL GETENV('PWD',DIRECTORY)
c
c Add .eph to PULSAR_NAME
c
      EPH_NAME=PULSAR_NAME(1:LENGTH(PULSAR_NAME))
      chlen = length(eph_name)
c
c Add the directory path to the filename
c
c      EPH_NAME=DIRECTORY(1:LENGTH(DIRECTORY))//'/'//EPH_NAME
c
 500  CONTINUE
c
c Check whether there is already a .eph file.
c
      CALL CHECKB4WRITE(EPH_NAME(1:chlen), '.eph', new_eph)
      if (new_eph .eq. ' ') return
      if (new_eph .ne. EPH_NAME) THEN 
        WRITE(*,*)' NOTE: Pulsar Name has NOT changed. If you '//
     &          'want to reload this ephemeris later, you will have '//
     &          'to change the Pulsar Name to: '//
     &          NEW_EPH(1:LENGTH(NEW_EPH))
        WRITE(*,*) OUTLINE
      ENDIF
      EPH_NAME=NEW_EPH(1:LENGTH(NEW_EPH))//'.eph'
c
c Get the next available unit number.
      FILE_UNIT=GETLUN()

c convert data in VALS and ERRS arrays to parmStatus and value_* arrays
c for new style eph. file
      CALL OLDEPH2NEW(parmStatus, value_str, value_double,
     &             value_integer, error_double, VALS, ERRS)

c load the pulsar name (probably from readeph!)
      if (NAMEINEPH(1:1) .EQ. 'J') THEN
        value_str(EPH_PSRJ) = NAMEINEPH
        parmstatus(EPH_PSRJ) = 1
        parmstatus(EPH_PSRB) = 0

      elseif (NAMEINEPH(1:1) .EQ. 'B') THEN
        value_str(EPH_PSRB) = NAMEINEPH
        parmstatus(EPH_PSRB) = 1
        parmstatus(EPH_PSRJ) = 0
      else
c Name not known - use the ephfile name and assume J
        value_str(EPH_PSRJ)= EPH_NAME(1:LENGTH(EPH_NAME)-4)
        parmstatus(EPH_PSRJ) = 1
        parmstatus(EPH_PSRB) = 0
      endif

c BINARY parameter - set to 'BT'
      if ( value_double(EPH_A1) .ne. 0d0) THEN
         value_str(EPH_BINARY) = 'BT'
         parmStatus(EPH_BINARY) = 1

c If not binary, zero the binary parameters
      else
         do i= EPH_BINARY, EPH_BPP
           parmStatus(i) = 0
           value_double(i) = 0d0
         enddo
         parmstatus(EPH_TASC) =0
         value_double(EPH_TASC) = 0d0
      endif

cc Call the new eph writing routine - file EPHNAME
      IFAIL = WR_EPH(EPH_NAME, parmStatus, value_str, value_double,
     &             value_integer, error_double)

      RETURN
      END





