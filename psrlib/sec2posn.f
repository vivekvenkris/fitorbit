c *******************************************************************
      LOGICAL FUNCTION SEC2POSN(SECONDS,POSITION)
c *******************************************************************
c
c Attempts to convert a number of seconds (i.e. arcseconds) into
c   a position of the form DD:MM:SS.SSSS....
c
c Written by DLG & FHJ. November 1992.
c
c Declare variables
c String to be returned. Contains the position in the form DD:MM:SS.SS..
      CHARACTER*40 POSITION
c The number of (arc)seconds
      DOUBLE PRECISION SECONDS
c The number of degrees
      INTEGER DEGREES
c The number of arcminutes
      INTEGER MINUTES, INTSEC
c Variable used as a remainder during the division
      DOUBLE PRECISION REMAIN
c The number of seconds
      DOUBLE PRECISION NEW_SECONDS
c String used as internal file
      CHARACTER*40 INTER
c
c
c Divide SECONDS by 3600 to get it into degrees
      DEGREES=INT(SECONDS/3600.0D0)
c Get the number of seconds left over
      REMAIN=ABS(MOD(SECONDS,3600.0D0))
c Divide remainder by 60 to get the number of minutes
      MINUTES=INT(REMAIN/60.0D0)
c Get the number of seconds left over
      NEW_SECONDS=MOD(REMAIN,60.0D0)
c.      IF (ABS(NEW_SECONDS).LT.10.0D0) THEN
c Write degrees, minutes, seconds into internal file
c.         WRITE(INTER,200)DEGREES,MINUTES,NEW_SECONDS
c.      ELSE
         INTSEC = INT(NEW_SECONDS)
         WRITE(INTER,100)DEGREES,MINUTES,INTSEC,NEW_SECONDS-INTSEC
c.      ENDIF
c Read string position from internal file
         READ(INTER,'(A)')POSITION
c Format statement
 100  FORMAT(SS,I3.2,':',I2.2,':',I2.2,F15.14)
c
c. 200  FORMAT(SS,I3.2,':',I2.2,':',F16.13)
c Return to main program
      SEC2POSN=.TRUE.
      RETURN
      END
