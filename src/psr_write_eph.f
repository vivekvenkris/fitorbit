C **********************************************************************
      SUBROUTINE PSR_WRITE_EPH(WPSRNAM,WP0,WPDOT,WPORB,WTASC,WASINI,
     & WOMEGA,WECC,WRAFIT,WDECFIT)
C **********************************************************************
C
C Subroutine to write ephemeris data to PSRNAM.eph file
C
C Declarations:
C Pulsar Name
      CHARACTER WPSRNAM*20
C Filename ( = PSRNAME.eph)
      CHARACTER WEPHFIL*40
C Period in seconds
      REAL*8 WP0
C Period Derivative in 10**-15 seconds/second
      REAL*8 WPDOT
C Orbital Period in days
      REAL*8 WPORB
C MJD of period and ascending node
      REAL*8 WTASC
C A sin(i) in seconds
      REAL*8 WASINI
C Longitude of periastron (days?)
      REAL*8 WOMEGA
C Eccentricity
      REAL*8 WECC
C Best fit pulsar co-ordinates
      CHARACTER WRAFIT*20
      CHARACTER WDECFIT*20
      CHARACTER WLINE*80
C Variables for assigning unit number to open file
      INTEGER GETLUN,WUNITNO
C
C Format overlay for writing ephemeris data
 1000   FORMAT(
     &  1X,F15.12,'       ! Mean BC period (sec)'/
     &  1X,F15.6,'       ! Pdot (10**-15 seconds/second)'/
     &  1X,F15.6,'       ! Orbital period (days)'/
     &  1X,F15.6,'       ! MJD of period and ascending node'/
     &  1X,F15.6,'       ! A sin (i)  (sec)'/
     &  1X,F15.6,'       ! Longitude of periastron (days)'/
     &  1X,F15.6,'       ! Eccentricity')
C Convert WRAFIT, WDECFIT into single string for writing
          WLINE=WRAFIT//' '//WDECFIT//'  ! RAJ,DECJ(fit)'
C Convert pulsar name to file name by adding '.eph'
          WEPHFIL=WPSRNAM(1:LENGTH(WPSRNAM))//'.eph'
C
C Get next available unit number
          WUNITNO=GETLUN()
C
C Open file
          OPEN(UNIT=WUNITNO,FILE=WEPHFIL,STATUS='UNKNOWN')
C
C Write ephemeris data
          WRITE(WUNITNO,1000)
     &        WP0,WPDOT,WPORB,WTASC,WASINI,WOMEGA,WECC
          WRITE(WUNITNO,'(A)')WLINE
          WRITE(UNITNO,*)
C
C Close file
          CLOSE(WUNITNO)
C
C End Subroutine
          RETURN
          END




