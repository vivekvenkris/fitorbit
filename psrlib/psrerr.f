*DECK PSRERR
C
C **************************************************************
      SUBROUTINE PSRERR ( ROUTN, NERR, IPAR1, RPAR, CPAR )
C **************************************************************
C
C PULSAR ERROR ROUTINE.
C
C THE ERROR TABLE ENTRIES (IERTAB) HAVE THE FORM ABCD WHERE :
C A IS THE ERROR STATUS. A=0 FATAL AND PRINT.
C                        A=1 NON FATAL AND NO PRINT.
C                        A=2 NON-FATAL AND SHORT PRINT.
C      ( NOTE THAT THESE INTERNAL VALUES ARE NOT THE SAME AS THOSE
C        USED BY SETPER/GETPER )
C B IS DEFAULT VALUE OF A.
C C=0 STATUS CHANGEABLE, C=1 STATUS FIXED.
C D=0 NO PARAMETER, D=1 INTEGER, D=2 REAL, D=3 CHARACTER,
C                   D=4 CHARACTER AND INTEGER,
C                   D=5 CHARACTER AND REAL.
C
C MODIFICATION RECORD
C
C   88/12/13 JDB     ALL OUTPUT NOW GOES TO THE ERROR FILE AND NOT TO
C                    THE MONITOR FILE AS PREVIOUSLY. PSRLIB.DEF HAS
C                    BEEN INCLUDED SO AS TO MAKE LUERR AVAILABLE TO
C                    THIS ROUTINE.
C
      INCLUDE 'PSRLIB.DEF'
C
      SAVE IERTAB,ERRMES,LFATAL
      LOGICAL LFATAL,TMP
      DATA LFATAL /.FALSE./
      PARAMETER (NUMERR=72,MAXML=50)
      DIMENSION IERTAB(NUMERR)
      CHARACTER ERRMES(NUMERR)*(MAXML),FMT*(MAXML+15),CPAR*(*)
     &         ,ISIZE*1,RSIZE*2,ROUTN*(*),OUTBUF*150,RTETXT*60
C
C     SET THE DEFAULT ERROR TABLE ENTRIES ...
C
      DATA (IERTAB(I),I=1,NUMERR)
     &  / 3*1,3*3,1,3,3*4,2*5,0,8*3,0,3,0,2*3,1,0,2*3,3*1,5*3,1,4
     &   ,0,1,4,3,1,3*3,1,3,4,5,5*3,0,5*3,0,3*3,0,3*3 /
C
C     ... AND THE ERROR MESSAGES.
C
      DATA (ERRMES(I),I=1,10)
     &  / '''End of file detected on unit '',I?',
     &    '''Continuation line missing on unit '',I?',
     &    '''Too many commands maximum is '',I?',
     &    '''Command syntax error : '',A',
     &    '''Command '''''',A,'''''' not recognized''',
     &    '''Command '''''',A,'''''' is ambiguous''',
     &    '''Internal error - illegal error number '',I?',
     &    '''Invalid file identifier : '',A',
     &    '''Error on file '',A,'' at record number '',I?',
     &    '''Value for '',A,'' too small : '',I?'
     &  /
      DATA (ERRMES(I),I=11,20)
     &  / '''Value for '',A,'' too large : '',I?', 
     &    '''Value for '',A,'' too small : '',F??.3', 
     &    '''Value for '',A,'' too large : '',F??.3', 
     &    '''Command line too long''',
     &    '''Parameter '',A,'' is not an option''',
     &    '''Non-integral '',A,'' value''',
     &    '''Unit '''''',A,'''''' not recognized''',
     &    '''Parameter '''''',A,'''''' not recognized''',
     &    '''Invalid numeric value : '',A',
     &    '''Illegal From/To syntax, value missing : '',A'
     &  /
      DATA (ERRMES(I),I=21,30)
     &  / '''Date less than the starting date '',A',
     &    '''Unrecognized value/option : '',A',
     &    '''No parameters left on command line''',
     &    '''Starting point greater than finishing point : '',A',
     &    ''''''',A,'''''' clause not recognized in this context''',
     &    '''Unrecognized option : '',A',
     &    '''Non-numeric '',A,''value''',
     &    '''No spare file (>'',I?,'' nested Obey command!''',
     &    '''No file name specified on Obey command''',
     &    '''Unable to open file '''''',A,'''''' - '''
     &  /
      DATA (ERRMES(I),I=31,40)
     &  / '''File '''''',A,'''''' does not exist''',
     &    '''Too many output channels requested (>'',I?,'')''',
     &    '''Requested output channel '',I?,'' not set up''',
     &    '''Too many telescopes or data sources (>'',I?,'')''',
     &    'A,'' not available in batch mode''',
     &    'A,'' not available in interactive mode''',
     &    'A,'' temporarily withdrawn''',
     &    'A,'' no longer available''',
     &    'A,'' not yet available''',
     &    '''Data channel number out of range : '',I?'
     &  /
      DATA (ERRMES(I),I=41,50)
     &  / 'A,'' array capacity exceeded : '',I?',
     &    '''Current parameter list empty''',
     &    '''No file assigned to logical unit '',I?',
     &    'A,'' buffer capacity exceeded : '',I?',
     &    '''Error in data channel specifier :'',A',
     &    '''Maximum number of data channels exceeded : '',I?',
     &    '''Unable to read record from file '''''',A,'''''' - ''',
     &    '''Unable to write record to file '''''',A,'''''' - ''',
     &    '''Unable to inquire about file '''''',A,'''''' - ''',
     &    '''Unknown datatype encountered : '',I?'
     &  /
      DATA (ERRMES(I),I=51,60)
     &  / 'A'
     &   ,'''Invalid value for '',A,'' : '',I?'
     &   ,'''Invalid value for '',A,'' : '',F??.3'
     &   ,'''No data found on file '',A'
     &   ,'A,'' missing'''
     &   ,'''Invalid date/time value : '',A'
     &   ,'''Invalid date/time or Julian time value : '',A'
     &   ,'''No '',A,'' in specified date range'''
     &   ,'''No free logical unit'''
     &   ,'A,'' not found in catalog'''
     &  /
      DATA (ERRMES(I),I=61,70)
     &  / 'A,'' not present in catalog'''
     &   ,'''Error while reading catalog, invalid text is : '',A'
     &   ,'''Unable to close file '''''',A,'''''' - '''
     &   ,'''Unable to send print job to queue '''''',A,'''''''''
     &   ,'''All data channels have been deleted'''
     &   ,'''No '',A,'' devices are available'''
     &   ,'''Terminal '',A,'' is not available'''
     &   ,'''There is no '',A,'' terminal available'''
     &   ,'''Inconsistent terminal device and type requested'''
     &   ,'''You must supply a device type for ,'',A'
     &  /
      DATA (ERRMES(I),I=71,NUMERR)
     &  / 'A,'' is not an allowed plotting device'''
     &   ,'''Invalid switch value : '',A'
     &  /
C
C     CHECK THE ERROR NUMBER.
C
      IF (NERR.LE.0.OR.NERR.GT.NUMERR) THEN
C
C       ERROR NUMBER IS OUT OF RANGE.
C
        IPAR=NERR
        NERR=7
      ELSE
C
C       ERROR NUMBER IS IN RANGE.
C
        IPAR=IPAR1
      ENDIF
C
C     OBTAIN THE FIELDS FROM THE ERROR TABLE.
C
      I=IERTAB(NERR)
      IESTAT=I/1000
      IPARAM=MOD(I,10)
C
C     IF A RECURSIVE CALL TO PSRERR SET ERROR FATAL.
C
      IF (LFATAL) THEN
        IESTAT=0
      ENDIF
C
C     PRINT ERROR NUMBER.
C
      IF (IESTAT.EQ.0) THEN
        CALL SETSUB(LUERR,'/PULSAR/ Error report')
        CALL PAGE(LUERR,0)
        FMT='(SS,'' /PULSAR/ Error number '',I'//ISIZE(NERR)//')'
        WRITE(OUTBUF,FMT) NERR
        CALL OUTPUT(LUERR,OUTBUF)
        CALL MESAGE(OUTBUF)
        CALL OUTPUT(LUERR,' ')
      ENDIF
C
C     FORM FORMAT FOR MESSAGE.
C
      IF (IESTAT.EQ.0.OR.IESTAT.EQ.2) THEN
C
C       TEST IF SHORT PRINT.
C
        IF (IESTAT.EQ.2) THEN
C
C         PUT '     ' AT THE BEGINNING SO THAT IT AGREES
C         WITH PROGRAM DETECTED ERRORS.
C
          FMT='(SS,''      '','
     &        //ERRMES(NERR)(1:LENGTH(ERRMES(NERR)))//')'
        ELSE
          FMT='(SS,1X,'
     &        //ERRMES(NERR)(1:LENGTH(ERRMES(NERR)))//')'
        ENDIF
C
C       TEST IF THERE IS A PARAMETER TO PRINT.
C
        IF (IPARAM.EQ.1) THEN
C
C         INTEGER, ADJUST THE FORMAT STATEMENT.
C
          CALL SETIFM(IPAR,FMT)
          WRITE(OUTBUF,FMT) IPAR
        ELSEIF (IPARAM.EQ.2) THEN
C
C         REAL, ADJUST THE FORMAT.
C
          CALL SETRFM(RPAR,FMT)
          WRITE(OUTBUF,FMT) RPAR
        ELSEIF (IPARAM.EQ.3) THEN
C
C         CHARACTER.
C
          WRITE(OUTBUF,FMT) CPAR(1:MAX(1,LENGTH(CPAR)))
        ELSEIF (IPARAM.EQ.4) THEN
C
C         CHARACTER FOLLOWED BY INTEGER.
C
          CALL SETIFM(IPAR,FMT)
          WRITE (OUTBUF,FMT) CPAR(1:MAX(1,LENGTH(CPAR))),IPAR
        ELSEIF (IPARAM.EQ.5) THEN
C
C         CHARACTER FOLLOWED BY REAL.
C
          CALL SETRFM(RPAR,FMT)
          WRITE (OUTBUF,FMT) CPAR(1:MAX(1,LENGTH(CPAR))),RPAR
        ELSE
C
C         NO PARAMETER.
C
          WRITE (OUTBUF,FMT)
        ENDIF
C
C       FOR CERTAIN ERROR NUMBERS, THE FORTRAN RUN-TIME ERROR STATUS
C       HAS BEEN PASSED THROUGH IN IPAR.
C
        IF ( NERR.EQ.30.OR.NERR.EQ.47.OR.NERR.EQ.48
     &       .OR.NERR.EQ.49.OR.NERR.EQ.63 ) THEN
C
C          ADD THE TEXT TO THE ERROR MESSAGE.
C
           OUTBUF = OUTBUF(1:LENGTH(OUTBUF))//' '//RTETXT(IPAR)
        ENDIF
C
C       PRINT THE MESSAGE.
C
        CALL OUTPUT(LUERR,OUTBUF)
        IF ( IESTAT.EQ.0 ) CALL MESAGE(OUTBUF)
      ENDIF
C
C     RETURN IF NON-FATAL.
C
      IF (IESTAT.NE.0) THEN
         RETURN
      ENDIF
C
C     PRINT THE ROUTINE NAME
C
      CALL OUTPUT(LUERR,' ')
      WRITE(OUTBUF,200) ROUTN(1:LENGTH(ROUTN))
      CALL OUTPUT(LUERR,OUTBUF)
      CALL MESAGE(OUTBUF)
      CALL OUTPUT(LUERR,' ')
C
C     CALL THE USER ERROR ROUTINE.  FIRST SET THE FATAL ERROR FLAG TO
C     PREVENT RECURSIVE CALLS OF USRLER.
C
      TMP=LFATAL
      LFATAL=.TRUE.
      IF (.NOT.TMP) CALL USRLER(ROUTN,NERR)
C
C     TERMINATE EXECUTION.
C
      CALL OABORT('/PULSAR/',ROUTN,NERR)
      STOP
C
C END OF SUBROUTINE PSRERR.
C
C THE NEXT ENTRY POINT IS USED TO CHANGE VALUES IN THE ERROR TABLE.
C
      ENTRY SETPER(NERR,IOPT)
C
C CHANGES THE ERROR STATUS FIELD IN THE ERROR TABLE FOR ERROR NUMBER NERR.
C IF NERR IS ZERO THEN ALL ERROR NUMBERS ARE CHANGED.
C IOPT=0 SET TO DEFAULT
C IOPT=1 SET TO NON-FATAL AND NO PRINT
C IOPT=2 SET TO NON FATAL AND PRINT
C IOPT=3 SET TO FATAL
C
C     CHECK THE REQUESTED ERROR NUMBER.
C
      IF (NERR.EQ.0) THEN
C
C       CHANGE ALL ERRORS.
C
        IS=1
        IF=NUMERR
      ELSEIF (NERR.LE.NUMERR) THEN
C
C       CHANGE THE SPECIFIED ERROR.
C
        IS=NERR
        IF=NERR
      ELSE
C
C       ERROR NUMBER IS OUT OF RANGE.
C
        RETURN
      ENDIF
C
C     CHECK THE REQUIRED OPTION.
C
      IF (IOPT.LT.0.OR.IOPT.GT.3) RETURN
C
C     CHANGE THE REQUESTED ENTRIES.
C
      DO 1 I=IS,IF
C
C       CHECK THAT ERROR MAY BE CHANGED
C
        IERR=MOD(IERTAB(I),1000)
        IPAR=MOD(IERR,100)/10
        IF (IPAR.EQ.0) THEN
C
C         SET TO DEFAULT.
C
          IF (IOPT.EQ.0) THEN
            IERTAB(I)=1000*(IERR/100)+IERR
C
C         SET SPECIFIED STATUS.
C
          ELSE
            IERTAB(I)=1000*MOD(IOPT,3)+IERR
          ENDIF
        ENDIF
    1 CONTINUE
      RETURN
C
C END OF SUBROUTINE SETPER.
C
C THE NEXT ENTRY POINT IS USED TO OBTAIN THE CURRENT ERROR STATUS.
C
      ENTRY GETPER(NERR,ISTAT)
C
C RETURNS THE CURRENT STATUS OF ERROR NUMBER NERR.  ON EXIT
C ISTAT=1  NON-FATAL AND NO PRINT
C ISTAT=2  NON-FATAL AND PRINT
C ISTAT=3  FATAL
C ISTAT=-1 ERROR DOES NOT EXIST
C
C     TEST THE REQUIRED ERROR NUMBER.
C
      IF (NERR.LT.1.OR.NERR.GT.NUMERR) THEN
C
C       NO SUCH ERROR.
C
        ISTAT=-1
      ELSE
C
C       OBTAIN THE CURRENT STATUS.
C
        ISTAT=IERTAB(NERR)/1000
C
C       FATAL.
C
        IF ( ISTAT.EQ.0 ) THEN
           ISTAT = 3
        ENDIF
      ENDIF
C
      RETURN
C
C     FORMAT.
C
  200 FORMAT(SS,' PULSAR fatal error detected in routine ',A)
C
C END OF SUBROUTINE GETPER.
C
      END
