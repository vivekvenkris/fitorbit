C **********************************************************************
      SUBROUTINE PSR_READ_EPH(SPSRNAM,SP0,SPDOT,SPORB,STASC,SASINI,
     & SOMEGA,SECC,SLINE)
C **********************************************************************
C
C Subroutine to load in ephemeris data from PSRNAM.eph
C
C Declarations:
C Pulsar Name
      CHARACTER SPSRNAM*20
C Filename ( = PSRNAME.eph)
      CHARACTER EPHFIL*40
C Period in seconds
      REAL*8 SP0
C Period Derivative in 10**-15 seconds/second
      REAL*8 SPDOT
C Orbital Period in days
      REAL*8 SPORB
C MJD of period and ascending node
      REAL*8 STASC
C A sin(i) in seconds
      REAL*8 SASINI
C Longitude of periastron (days?)
      REAL*8 SOMEGA
C Eccentricity
      REAL*8 SECC
C A line of RA and DEC data
      CHARACTER*80 SLINE
C
C Variables for assigning unit number to open file
      INTEGER GETLUN,UNITNO, ISTAT
C
C Convert pulsar name to file name by adding '.eph'
          EPHFIL=SPSRNAM(1:LENGTH(SPSRNAM))//'.eph'
C
C Get next available unit number
          UNITNO=GETLUN()
C
C Open file
          OPEN(UNIT=UNITNO,FILE=EPHFIL,STATUS='OLD',IOSTAT=ISTAT)
          IF (ISTAT .NE. 0) THEN
            WRITE (6,*) 'Could not open ".eph" file!'
            RETURN
          ENDIF
C
C Read ephemeris data
          READ(UNITNO,*)SP0
          READ(UNITNO,*)SPDOT
          READ(UNITNO,*)SPORB
          READ(UNITNO,*)STASC
          READ(UNITNO,*)SASINI
          READ(UNITNO,*)SOMEGA
          READ(UNITNO,*)SECC
          READ(UNITNO,'(A)')SLINE
C
C Close file
          CLOSE(UNITNO)
C
C End Subroutine
          RETURN
          END





