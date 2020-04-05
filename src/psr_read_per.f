C *********************************************************************
      SUBROUTINE PSR_READ_PER(XPSRNAM,XNEW_MJDS,XMJDF,XNPTMAX,XMJD,
     &    XPERIOD,XPERR,XIFLAG,XNPTS,XLINE)
C *********************************************************************
C Subroutine to load in period data from PSRNAM.per
C LN modif 22/09/93. Allow a # as first character of a line to comment it out.
C AGL 17/06/94.      Use *,+ or o as first character to define plot symbol.
C AGL 27/10/97.      Improve logic of first character handling.
C CAJ 03/02/03       Traps for faulty input
C
C Declare variables:
C Maximum Size of arrays
      INTEGER XNPTMAX
C Pulsar name
      CHARACTER XPSRNAM*20
C Pulsar data filename = PSRNAM.per
      CHARACTER PERFIL*40
C Start of MJD range
      REAL*8 XNEW_MJDS
C End of MJD range
      REAL*8 XMJDF
C Array containing MJDs
      REAL*8 XMJD(XNPTMAX)
C Array containing periods in milliseconds
      REAL*8 XPERIOD(XNPTMAX)
C Array containing period errors? in milliseconds?
      REAL*8 XPERR(XNPTMAX)
C Array ???
      INTEGER XIFLAG(XNPTMAX)
C Number of points in the data file
      INTEGER XNPTS
C Line containing RA and Dec and the temporary row
      CHARACTER*80 XLINE, ROW
C Dummy variable
      INTEGER XII,IROW, ISTAT
C Integers needed for obtaining available unit number
      INTEGER XUNITNO,GETLUN
C
C Get filename by adding '.per' to PSRNAM
      PERFIL=XPSRNAM(1:LENGTH(XPSRNAM))//'.per'
C
C Get available unit number
      XUNITNO=GETLUN()
C
C Open file
      OPEN(UNIT=XUNITNO,FILE=PERFIL,STATUS='OLD',IOSTAT=ISTAT)
          IF (ISTAT .NE. 0) THEN
            WRITE (6,*) 'Could not open ".per" file!'//CHAR(7)
            RETURN
          ENDIF
C Reset MJD range
      XMJDF=0.0
      XNEW_MJDS=99999.0
      IROW  = 0
C Get RA, Dec
  100 READ(XUNITNO,'(A)')XLINE
      IROW  = IROW + 1
      IF (XLINE(1:1).EQ.'#') GOTO 100
C Begin a loop to read in MJD, Periods, and errors
      XNPTS = 0
      DO 10 XII=1,XNPTMAX
        XNPTS = XNPTS + 1
        IROW = IROW + 1
        READ (XUNITNO,'(A)',END=500,ERR=510,IOSTAT=ISTAT) ROW
        IF (ROW(1:1).EQ.'#') GOTO 10
        IF (ROW.EQ.' ') GOTO 10

 120    IF(ROW(1:1).EQ.' '.OR.ROW(1:1).EQ.'*'.OR.ROW(1:1).EQ.'+'.OR.
     &       ROW(1:1).EQ.'o')THEN
          READ (ROW(2:),*,ERR=510,IOSTAT=ISTAT) XMJD(XNPTS),
     &                    XPERIOD(XNPTS),XPERR(XNPTS)
          IF(ROW(1:1).EQ.'*')THEN
             XIFLAG(XNPTS)=-18
          ELSEIF(ROW(1:1).EQ.'+')THEN
             XIFLAG(XNPTS)=-2
          ELSEIF(ROW(1:1).EQ.'o')THEN
             XIFLAG(XNPTS)=-22
          ELSE
             XIFLAG(XNPTS)=-17
          ENDIF
        ELSE
          READ (ROW(1:),*,ERR=510,IOSTAT=ISTAT) XMJD(XNPTS),
     &                    XPERIOD(XNPTS),XPERR(XNPTS)
c mod to check data OK
          IF (XMJD(XNPTS).eq.0.0) THEN
            WRITE (6,'(a,i5)') ' Skipping line ',irow
            XNPTS = XNPTS-1
            GOTO 10
          ENDIF
        ENDIF

 130    XPERIOD(XNPTS)=XPERIOD(XNPTS)/1000.0
        XPERR(XNPTS)=XPERR(XNPTS)/1000.0

C Specify new MJD range as obtained from the data
         XMJDF=MAX(XMJDF,XMJD(XNPTS))
         XNEW_MJDS=MIN(XNEW_MJDS,XMJD(XNPTS))
C Give the number of data points
C Close the loop
   10 CONTINUE
C Close the File
  500 CLOSE(XUNITNO)
C
      XNPTS=XNPTS-1
      WRITE (6,512) XNPTS,'data points read.'
  512 FORMAT (i4,x,a)

C End Subroutine
      RETURN
*
  510 CONTINUE
      CLOSE(XUNITNO)
      WRITE (6,511) IROW,ISTAT
  511 FORMAT ('Error reading data from the ".per" file!'/,
     .        ' Line #',I5,'  IOSTAT=',I4/)
      RETURN
      END
