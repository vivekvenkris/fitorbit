c *********************************************************************
      SUBROUTINE LIBREADEPH(NAMEINEPH,EPH_NAME,OVALS,ERRS,IFAIL)
c *********************************************************************
c
c A subroutine to load an ephemeris file and pass the data back to the
c   main program.
c
c Written by F.H.Jowett & D.Law-Green, October 1992
c
c 6-June-1994 MXB Changed so that it doesn't bomb if no error mentioned.
c 22-Sep-1995 AGL/RSP meld of PSRCLOCK and Jodrell versions.
c                   Functionality unchanged except for call.
c Dec 1998    CAJ Reads new (parkes style) eph files
c Apr 2000    CAJ Add pulsar name to call 
c Apr 2005    CAJ Set TASC(done at unknown time) 
c                 & PMEPOCH to PEPOCH if not set.
c Mar 2006    CAJ Change logical .eq. and .ne. to .eqv. and .neqv for
c                 g77 compatability!
c Aug 2006    CAJ rename to libreadeph to avoid confusion with ephio function
c Disable implicit naming conventions:
      IMPLICIT NONE

c  new .eph file definitions
      include 'keyinfo.com'
      integer rd_eph
      logical old
      INTEGER parmStatus(NUM_KEYS)
      CHARACTER*32 value_str(NUM_KEYS)
      REAL*8 value_double(NUM_KEYS)
      INTEGER value_integer(NUM_KEYS)
      REAL*8 error_double(NUM_KEYS)

c and old file key name assignments and arrays
      include 'OEPH_KEY.COM'

c constants
      include 'PSRMATH.DEF;2'

c
c Declare variables:
c String containing pulsar name.
      CHARACTER*15 NAMEINEPH
c !JFB1 changed to indefinite to allow calls
c String containing ephemeris filename
      CHARACTER*80 EPH_NAME
c String read from ephemeris file
      CHARACTER*250 EPH_LINE(100),teph_line
      character     delim
c Strings containing keyowrds
      CHARACTER*100 KEYWORD(100)
c Strings containing pulsar data values
      CHARACTER*100 VALUE_STRING(100,2)
c Error statement string
      CHARACTER*80 ERROR
c Two part character array used by PARSE to return words.
      CHARACTER*30 WORDS(6)
c Variable giving the number of keyword types
      INTEGER NO_KEYWORDS
      PARAMETER (NO_KEYWORDS=64)
c String array containing keyword references
      CHARACTER*100 ST_KEYWORD(NO_KEYWORDS,2)
c String array containing results in keyword number order
      CHARACTER*100 RESULTS(NO_KEYWORDS,2)
c String containing working directory
      CHARACTER*60 DIRECTORY
c Fail flag, 0 if OK, non-zero if an error occured.
      INTEGER IFAIL
c Unit number assigned to file but GETLUN routine.
      INTEGER FILE_UNIT
c Integer variable needed for GETLUN routine.
      INTEGER GETLUN,LENGTH
c Variable containing number if an error occurs in opening file.
      INTEGER FILE_ERROR
c Dummy variables for array read/write loops
      INTEGER DUMMY,DUMMY_2
c Length of ephemeris file (lines)
      INTEGER FILE_LENGTH
c The number of words returned by the PARSE routine.
      INTEGER NUM_WORDS
c Number of keyword when found
      INTEGER KEY_NUMBER
c Position of the exclamation mark before a comment
      INTEGER EXCL_POSN
c Array of real numbers containing final values.
      DOUBLE PRECISION VALUES(NO_KEYWORDS,2)
c Arrays used to return the values [OVALS] and errors [ERRS]. These
c   have been split from VALUES to enable easier future integration
c   with C++ programs which handle two-dimensional arrays differently
c   to FORTRAN.
      DOUBLE PRECISION OVALS(NO_KEYWORDS)
      DOUBLE PRECISION ERRS(NO_KEYWORDS)
c
c Array of character strings containing: Program Name in (x,1) part
c                                        Date in (x,2) part
c                                        User in (x,3) part
c NOT YET IMPLEMENTED
c      CHARACTER*10 USER_INFO(NO_KEYWORDS,3)
c Logical flag for use when searching for keywords
      LOGICAL LKEY
c Logical declarations to enable PARDBL and PARPOS to be correctly
c   invoked.
      LOGICAL PARDBL,PARPOS
c Logical declaration for CMDJT
      LOGICAL CMDJT
c Logical flags returned by CMDJT
      LOGICAL DATE,TIME
c Variable containing Julian Date, returned by CMDJT
      DOUBLE PRECISION JULDAT
c Integers needed for CMDJT
      INTEGER NCMD,ICMD,istline
c temporary places for calc. variables
      DOUBLE PRECISION RAB, RAJ, DECB, DECJ, R41950, D41950, DR, DD 

      IFAIL = 0
c
c Call a subroutine to put all the keywords into the ST_KEYWORD array
      CALL KEY_WORD(NO_KEYWORDS,ST_KEYWORD)
c
c
c Go through the VALUES array and explicitly make each part of the 
c   array equal to 0.0D0 -- the reason for this is as follows:
c   If an ephemeris file is missing a line, which is perfectly OK, this
c   can cause problems in the parent program if it expecting that line.
c   If the value associated with that line already has a values stored
c   in it by the program, then READEPH will return a null for the value
c   which the PROGRAM WILL IGNORE. I.e. the variable will remain its 
c   original value and won't be set to zero, which is what we want.
c   To correct this problem, it is easiest just to make sure that if 
c   a value does not exist on the ephemeris, then it should equal zero.
      DO DUMMY=1,2
         DO DUMMY_2=1,NO_KEYWORDS
            VALUES(DUMMY_2,DUMMY)=0.0D0
         ENDDO
      ENDDO
c
c Obtain the next available unit number.
      FILE_UNIT=GETLUN()
c Get the current working directory from the environment variable PWD
      CALL GETENV('PWD',DIRECTORY)
c
c Tag a .eph on the end of pulsar name. The function LENGTH, returns the
c length of a string, ignoring trailing spaces and nulls.
      EPH_NAME=EPH_NAME(1:LENGTH(EPH_NAME))//'.eph'
c
c Check whether the ephemeris file exists
c      INQUIRE(FILE=EPH_NAME,EXIST=LEXIST,IOSTAT=FILE_ERROR)
c      IF (.NOT.LEXIST) THEN
c         ERROR='File called: '//EPH_NAME(1:LENGTH(EPH_NAME))//
c     &       ' does not exist!'
c         WRITE(*,*)ERROR
c        JFB1 make ephemeris from catalog
c         write(*,*) 'calling makeph for ',
c     &          pulsar_name(1:length(pulsar_name))
c         call makeph(pulsar_name(1:length(pulsar_name)),
c     &          values,errs,no_keywords)
c JFB1 end
c      ENDIF

c Open the file.
      OPEN(UNIT=FILE_UNIT,IOSTAT=FILE_ERROR,FILE=EPH_NAME,
     &    STATUS='OLD')
      IF(FILE_ERROR.NE.0) THEN
         ERROR='File called: '//EPH_NAME(1:LENGTH(EPH_NAME))//
     &       ' does not exist!'
         WRITE(*,*)ERROR
         IFAIL=51
         RETURN
      ENDIF
c
c Read in complete lines until EOF
      DUMMY=0
      DO WHILE (.TRUE.)
         DUMMY=DUMMY+1
         READ(FILE_UNIT,'(A)',END=888)EPH_LINE(DUMMY)
      ENDDO
c
c Jumps to here at EOF
 888  CONTINUE
      FILE_LENGTH=DUMMY-1
c
c Close the ephemeris file.
      CLOSE(FILE_UNIT)

c see if we can figure out whether it an old or new style .eph file!
      old= .false.
      do dummy = 1,file_length
        if (index(eph_line(dummy), 'RAB') .ne.0 ) old = .true. 
      enddo

C This is what we do for a new .eph file
      if (.not. old) then
        write(*,*)'New style (dec 1998) .eph file'

cc Call the new eph reading routine - file EPHNAME
        IFAIL = RD_EPH(EPH_NAME, parmStatus, value_str, value_double,
     &             value_integer, error_double)
        IF (IFAIL .eq. 0)        RETURN
        IFAIL = 0

cc Now convert the values (frequency to period etc.) and put in old array
        CALL NEWEPH2OLD(parmStatus, value_str, value_double,
     &             value_integer, error_double, OVALS, ERRS)

c and put the name from the eph somewhere safe
        IF (parmstatus(EPH_PSRJ).GT. 0) then
          NAMEINEPH = value_str(EPH_PSRJ)
        ELSE
          NAMEINEPH = value_str(EPH_PSRB)
        ENDIF

c and get out of here
        RETURN
      endif

c BUT if you think it's an older style .eph file.... do it the old way
c choose delimeter for parsing
      istline = 2
      teph_line = eph_line(1)
      if (teph_line(1:3).eq.'PSR') then
        delim = '	'
      elseif(teph_line(1:3).eq.'NAM') then
        delim = ' '
      else
        write(*,*)' old style eph format'
        delim = ' '
        istline = 1
      endif

c Begin a long loop. This loop will go through all the entries on the
c   ephemeris file, parse them, and try to place values into an array
      DO DUMMY=istline,FILE_LENGTH
c Trim all lines of comments ( text following '!' )
         EXCL_POSN=INDEX(EPH_LINE(DUMMY),'!')
c EXCL_POSN is the position of the '!'. Returns 0 if none.
         IF (EXCL_POSN.GT.0) THEN
            EPH_LINE(DUMMY)=EPH_LINE(DUMMY)(1:EXCL_POSN-1)
         ENDIF
c Call the PARSE routine to chop into words: keyword + value + error.
         CALL PARSE(EPH_LINE(DUMMY),WORDS,NUM_WORDS,3,delim)
c         write(*,*)dummy,words(1),words(2)
c Check that PARSE has correctly parsed EPH_LINE into keyword, value.
         IF (NUM_WORDS.lt.2) THEN
            ERROR='No values supplied!'
            WRITE(*,*)ERROR,dummy
c Go back to the main program, since there's a mistake.
            RETURN
         ENDIF
c Keyword
         KEYWORD(DUMMY)=WORDS(1)
c Value (still a string)
         VALUE_STRING(DUMMY,1)=WORDS(2)
c Error (still a string)
         if (num_words.eq.2) words(3)='0.0'
         VALUE_STRING(DUMMY,2)=WORDS(3)
c
c Look through the KEYWORD array --- i.e. the keywords loaded
c   in from the ephemeris file, and try to match it with a standard
c   keyword contained in the array ST_KEYWORD. If can't find a match
c   then tell the user. If can find a match, then put the value string
c   associated with that keyword into another string array called
c   RESULTS. Note, however, that the order of these values may be
c   different to that in the ephemeris, i.e. the value of RESULTS(1)
c   will always be RAB, since that is the first standard keyword in
c   psrinfo.h. Note that this means that some entries of RESULTS()
c   be null, since the ephemeris may be incomplete. Nota also, that
c   the main program calling PSRREAD (when it is subroutinised) may
c   only need to use some of the entries of RESULTS(). For example,
c   FITORBIT will only need P, Pdot, PB etc. It will not need RAB.
c
         DUMMY_2=0
         LKEY=.FALSE.
c Now for each ephemeris-keyword, look through all the standard 
c    keywords and try to make a match. If there is a match, then
c    set the flag LKEY to true and exit the loop.
         DO WHILE ((LKEY.EQV..FALSE.).AND.(DUMMY_2.LE.NO_KEYWORDS))
            DUMMY_2=DUMMY_2+1
            IF (KEYWORD(DUMMY).EQ.ST_KEYWORD(DUMMY_2,1)) THEN
               LKEY=.TRUE.
c Pass the number of the standard keyword in KEY_NUMBER
               KEY_NUMBER=DUMMY_2
            ENDIF
         ENDDO
c If the loop was completed (i.e. it looked through all the standard
c    keywords) without matching then print an error.
         IF (LKEY.EQV..FALSE.) THEN
            ERROR='Keyword: '//KEYWORD(DUMMY)
     &          (1:LENGTH(KEYWORD(DUMMY)))//' -- has not been'//
     &          ' recognised.'
            WRITE(*,'(a)')ERROR
c If there was a match, then pigeon-hole the value into the element
c    of the array RESULTS corresponding to the keyword.
         ELSE
c Value
            RESULTS(KEY_NUMBER,1)=VALUE_STRING(DUMMY,1)
c Error
            RESULTS(KEY_NUMBER,2)=VALUE_STRING(DUMMY,2)
         ENDIF
c         write(*,*)RESULTS(KEY_NUMBER,1)
c
c Now convert character strings into real numbers.
         IF (LKEY .NEQV. .FALSE.) THEN
         DO DUMMY_2=1,2
            IF (.NOT.PARDBL(RESULTS(KEY_NUMBER,DUMMY_2),
     &          VALUES(KEY_NUMBER,DUMMY_2))) THEN
c If PARDBL can't turn RESULTS into a number, then check to see if
c   RESULTS is a position instead.
               IF (.NOT.PARPOS(RESULTS(KEY_NUMBER,DUMMY_2),
     &             VALUES(KEY_NUMBER,DUMMY_2))) THEN
c If neither PARDBL nor PARPOS can function, see if it is in the
c   <date>-<time> form
                  CALL PARSE(RESULTS(KEY_NUMBER,DUMMY_2),WORDS,
     &                NUM_WORDS,2,'-')
                  IF (NUM_WORDS.EQ.2) THEN
                     NCMD=2
                     ICMD=1
                     IF (.NOT.CMDJT(WORDS,NCMD,ICMD,DATE,TIME,JULDAT))
     &                   THEN
                        ERROR='Tried to read: '//
     &                      RESULTS(KEY_NUMBER,DUMMY_2)//
     &                      ' as a date/time and failed.'
                     ELSE
                        VALUES(KEY_NUMBER,DUMMY_2)=JULDAT
                     ENDIF
                  ELSE
c If it is not a date/time either, then print an error
                     ERROR='Cannot Interpret: '//
     &                   RESULTS(KEY_NUMBER,DUMMY_2)
c                     WRITE(*,*)ERROR
                  ENDIF
c If PARPOS has been successful, then VALUES should be in seconds.
c   Unfortunately, RA seconds are 15 times larger than arcseconds.
c   Therefore, must convert both to RA, DEC to degrees.
c
c Check if declination.
               ELSE IF (ST_KEYWORD(KEY_NUMBER,1)(1:3).EQ.'DEC') THEN
                  VALUES(KEY_NUMBER,1)=VALUES(KEY_NUMBER,1)/3600.0D0
                  VALUES(KEY_NUMBER,2)=VALUES(KEY_NUMBER,2)/3600.0D0
c Check if right ascension.
               ELSE IF (ST_KEYWORD(KEY_NUMBER,1)(1:2).EQ.'RA') THEN
                  VALUES(KEY_NUMBER,1)=VALUES(KEY_NUMBER,1)/240.0D0
                  VALUES(KEY_NUMBER,2)=VALUES(KEY_NUMBER,2)/240.0D0
               ENDIF
            ENDIF
         ENDDO
       END IF
c This is the end of the loop.
      ENDDO
c
c Due to changes in the subroutine's spec, it is necessary to split
c   VALUES() into TWO arrays.
c
      DO DUMMY=1,NO_KEYWORDS
         OVALS(DUMMY)=VALUES(DUMMY,1)
         ERRS(DUMMY)=VALUES(DUMMY,2)
      ENDDO

c if TASC = 0d0 then set it to PEPOCH
      IF(OVALS(OEPH_TASC) .eq. 0d0) 
     &        OVALS(OEPH_TASC) = OVALS(OEPH_PEPOCH)
c
c if POSEPOCH = 0d0 then set it to PEPOCH
      IF(OVALS(OEPH_PMEPOCH) .eq. 0d0) 
     &        OVALS(OEPH_PMEPOCH) = OVALS(OEPH_PEPOCH)

c if no RAJ and DECJ calc them
      IF (OVALS(OEPH_RAJ) .EQ. 0d0 .and. OVALS(OEPH_DECJ) .EQ. 0d0) then
        RAB = OVALS(OEPH_RAB)/raddeg
        DECB = OVALS(OEPH_DECB)/raddeg

c add earth terms
        CALL SLA_ADDET(RAB, DECB, 1950.0d0, R41950, D41950)

c convert to J2000
        CALL SLA_FK45Z(R41950, D41950, 1950.0d0, RAJ, DECJ, DR, DD)

c change to degrees
        OVALS(OEPH_RAJ) = RAJ*raddeg
        OVALS(OEPH_DECJ) = DECJ*raddeg
      endif

c End subroutine
      RETURN
      END








