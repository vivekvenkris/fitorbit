*DECK LIBERR
C
C
C
      SUBROUTINE LIBERR (ROUTN, NERR, ILOC, IPAR1, RPAR, CPAR, IFAIL)
C
C Library error routine.
C Input arguments:
C  ROUTN   - The name of the routine detecting the error.
C  NERR    - The error number.
C  ILOC    - The location at which the error was detected.
C  IPAR1   - The integer parameter for printing.
C  RPAR    - The real parameter for printing.
C  CPAR    - The character parameter for printing.
C Output argument:
C  IFAIL   - The error number (usually NERR).
C
C     The error table entries (IERTAB) have the form ABCD where
C A = 0 fatal and print, A = 1 non-fatal and short print,
C A = 2 non-fatal and no print

C     B is default value of A
C C = 0 status changeable, C = 1 status fixed
C D = 0 no parameter, D = 1 integer, D = 2 real, D = 3 character.
C     Version 2.3   25th July, 1985
C
      SAVE IERTAB, ERRMES, LFATAL
      LOGICAL LFATAL, TMP
      DATA LFATAL /.FALSE./
      PARAMETER (NUMERR=85, MAXML=55)
      DIMENSION IERTAB(NUMERR)
      CHARACTER ERRMES(NUMERR)*(MAXML),FMT*(MAXML+15),CPAR*(*),ISIZE*1,
     &          RSIZE*2,ROUTN*(*),OUTBUF*150
C
C Set the default error table entries.
C
      DATA IERTAB/4*1,2*3,11,3,0,3,1,0,10*3,0,3,0,3,
     &3,1,3*3,3*1,6*3,2*1,6*3,1,0,3*3,3*0,3,4*1,0,10,1,2*3,4*0,3,0,1,
     &3,1,0,2*1,0,3,2*0,1,3,1/
C
C Set the error messages.
C
      DATA (ERRMES(I),I=1,19)/
     &'''End of file detected on unit '',I?',
     &'''Continuation line missing on unit '',I?',
     &'''Too many parameters, maximum is '',I?',
     &'''Parameter is too long (>'',I?,'' character!)''',
     &'''Command '''''',A,'''''' not recognized''',
     &'''Command '''''',A,'''''' is ambiguous''',
     &'''Internal error - illegal error number '',I?',
     &'''Invalid file identifier: '',A',
     &'''Value missing following an ''''=''''''',
     &'''Illegal separator: '',A',
     &'''Name too long (>'',I?,'' character!)''',
     &'''Blank names are not allowed''',
     &'''Non-integral or invalid stack value: '',A',
     &'''Illegal identifier syntax: '',A',
     &'''Invalid data stream code: '',A',
     &'''Non-integral '',A,'' value''',
     &'''Unrecognized frequency unit: '',A',
     &'''Illegal frequency: '',A',
     &'''Invalid numeric value: '',A'/
      DATA (ERRMES(I),I=20,38)/
     &'''Illegal From/To syntax, value missing: '',A',
     &'''Date less than the starting date '',A',
     &'''Unrecognized value/option: '',A',
     &'''No parameters left on command line''',
     &'''Starting point greater than finishing point: '',A',
     &'''Specified password is too long (>10 characters)''',
     &'''Unrecognized option: '',A',
     &'''Non-numeric '',A,''value''',
     &'''No spare file (>'',I?,'' nested Obey command!''',
     &'''A '',A,'' file name must be supplied''',
     &'''Unable to open file '',A',
     &'''File '',A,'' does not exist''',
     &'''Too many output channels requested (>'',I?,'')''',
     &'''Requested output channel '',I?,'' not set up''',
     &'''Too many telescopes or data sources (>'',I?,'')''',
     &'''Invalid option in data source definition: '',A',
     &'A,'' parenthesis missing in data source definition''',
     &'''Ambiguous option: '',A',
     &'''Invalid data stream size: '',A'/
      DATA (ERRMES(I),I=39,57)/
     &'''Invalid polarization code: '',A',
     &'''Invalid channel width value: '',A',
     &'''Unknown channel type code: '',I?',
     &'''Too many options/parameters, only '',I?,'' are allowed''',
     &'''Invalid comment importance: '''''',A,''''''''',
     &'''Final comment text delimiter '''''',A,'''''' missing''',
     &'''Invalid text delimiter, it should be '',A',
     &'''Text word '''''',A,'''''' is too long''',
     &'''Comment text buffer full, no room for '''''',A,''''''''',
     &'''Missing ''''='''' after '',A,'' option''',
     &'''Too many data streams (>'',I?,'')''',
     &'''Invalid date/time: time difference too large''',
     &'''Invalid repeat count: '''''',A,''''''''',
     &'''Unrecognized device: '''''',A,''''''''',
     &'''Ambigous device: '''''',A,''''''''',
     &'''No hard copy devices are available''',
     &'''DDB device name incorrect length''',
     &'''Too many devices on DDB file''',
     &'''Read error on file '',A'/
      DATA (ERRMES(I),I=58,76)/
     &'''Unable to open scratch file: error code '',I?',
     &'''Scratch file write error: code '',I?',
     &'''Scratch file read error: code '',I?',
     &'''Unable to close scratch file: error code '',I?',
     &'''Invalid input arguments to routine''',
     &'''The array processor is not available in batch''',
     &'''Command line is too long - exceeds '',I?,'' character!''',
     &'''Terminating delimiter is missing ('',A,'')''',
     &'''Unable to close '',A,'' file''',
     &'''Unable to repeat Obey file: re-position failed''',
     &'''Command input read error''',
     &'''Save file write error''',
     &'''Failed to send command to operating system''',
     &'''Invalid number of data streams per channel: '',A',
     &'''No room for symbol: symbols table full''',
     &'''No room for symbol: more than '',I?,'' symbol! defined''',
     &'''Symbol '''''',A,'''''' is not defined''',
     &'''Symbol value of '',I?,'' character! is too long to use''',
     &'''There are no symbols to save'''/
      DATA (ERRMES(I),I=77,NUMERR)/
     &'''Symbols'''' file write error: code '',I?',
     &'''Symbols'''' file read error: code '',I?',
     &'''Inconsistent table sizes on symbols'''' file''',
     &'A,'' is not a symbols'''' file''',
     &'''A repeat count is not allowed with a symbols'''' file''',
     &'''No free logical units are available''',
     &'''Unrecognized logical unit number for freeing: '',I?',
     &'''Recursive symbol definition encountered: '',A',
     $'''Maximum depth of symbol tranlations reached ('',I?,'')'''/
C
C Temporary Kluge to get some work done
C
      IF (LFATAL.neqv..false.) LFATAL=.false.
C
C Check the error number.
C
      IF(NERR.LE.0.OR.NERR.GT.NUMERR) THEN
C
C Error number is out of range.
C
        IERR=7
        IPAR=NERR
      ELSE
C
C Error number is in range.
C
        IERR=NERR
        IPAR=IPAR1
      ENDIF
C
C Obtain the fields from the error table.
C
      I=IERTAB(IERR)
      IESTAT=I/1000
      IPARAM=MOD(I,10)
C
C If a recursive call to liberr set error fatal.
C
      IF(LFATAL) THEN
        IESTAT=0
      ENDIF
C
C Print error number.
C
      IF(IESTAT.EQ.0) THEN
        CALL SETSTL('/LIBR/ Error report')
        CALL NEWLIN(-5)
        CALL NEWPAG
        FMT='(SS,'' /LIBR/ Error number '',I'//ISIZE(IERR)//')'
        WRITE(OUTBUF,FMT) IERR
        CALL OUTLIN(OUTBUF,-1)
        CALL NEWLIN(-1)
      ENDIF
C
C Form format for message.
C
      IF(IESTAT.LE.1) THEN
C
C Test if short print.
C
        IF(IESTAT.EQ.1) THEN
C
C It is, put '     ' at the beginning so that it agrees with program
C detected errors.
C
          FMT='(SS,''      '','//ERRMES(IERR)//')'
        ELSE
C
C It is not.
C
          FMT='(SS,1X,'//ERRMES(IERR)//')'
        ENDIF
C
C Test if there is a parameter to print.
C
        IF(IPARAM.EQ.1) THEN
C
C Integer, adjust the format statement.
C
          CALL SETIFM(IPAR,FMT)
          WRITE(OUTBUF,FMT) IPAR
        ELSEIF(IPARAM.EQ.2) THEN
C
C Real, adjust the format.
C
          CALL SETRFM(RPAR,FMT)
          WRITE(OUTBUF,FMT) RPAR
        ELSEIF(IPARAM.EQ.3) THEN
C
C Character.
C
          WRITE(OUTBUF,FMT) CPAR(1:MAX(1,LENGTH(CPAR)))
        ELSE
C
C No parameter.
C
          WRITE(OUTBUF,FMT)
        ENDIF
C
C Print the message.
C
        CALL OUTLIN(OUTBUF,-1)
      ENDIF
C
C Return if non-fatal.
C
      IF(IESTAT.NE.0) THEN
C
C Return the error number to the program.
C
        IFAIL=IERR
        RETURN
      ENDIF
C
C Print the routine name and location.
C
      CALL NEWLIN(-1)
      WRITE(OUTBUF,200) ROUTN(1:LENGTH(ROUTN)),ILOC
      CALL OUTLIN(OUTBUF,-1)
      CALL NEWLIN(-2)
C
C Call the user error routine.  First set the fatal error flag to
C prevent recursive calls of usrler.
C
      TMP=LFATAL
      LFATAL=.TRUE.
      IF(.NOT.TMP) CALL USRLER(ROUTN,IERR)
C
C Terminate execution.
C
      CALL OABORT('/LIBR/',ROUTN,IERR)
      STOP
C
C End of subroutine liberr.
C
C The next entry point is used to change values in the error table.
C
      ENTRY SETLER(NERR,IOPT)
C
C Changes the error status field in the error table.
C Iopt=0 set to default
C iopt=1 set to non-fatal and print
C iopt=2 set to non fatal and no print
C iopt=3 set to fatal
C     version 2.0   28Jun82
C
C Check the requested error number.
C
      IF(NERR.EQ.0) THEN
C
C Change all errors.
C
        IS=1
        IF=NUMERR
      ELSEIF(NERR.LE.NUMERR) THEN
C
C Change the specified error.
C
        IS=NERR
        IF=NERR
      ELSE
C
C Error number is out of range.
C
        RETURN
      ENDIF
C
C Check the required option.
C
      IF(IOPT.LT.0.OR.IOPT.GT.3) RETURN
C
C Change the requested entries.
C
      DO 1 I=IS,IF
C
C Check that error may be changed
C
        IERR=MOD(IERTAB(I),1000)
        IPAR=MOD(IERR,100)/10
        IF(IPAR.EQ.0) THEN
C
C Set to default.
C
          IF(IOPT.EQ.0) THEN
            IERTAB(I)=1000*(IERR/100)+IERR
C
C Set specified status.
C
          ELSE
            IERTAB(I)=1000*MOD(IOPT,3)+IERR
          ENDIF
        ENDIF
    1 CONTINUE
      RETURN
C
C End of subroutine setler.
C
C The next entry point is used to obtain the current error status.
C
      ENTRY GETLER(NERR,ISTAT)
C
C Returns the current status of error number nerr.  On exit
C istat=0 fatal
C istat=1 non-fatal and no print
C istat=2 non-fatal and print
C istat=-1 error does not exist
C     version 2.0   28Jun82
C
C Test the required error number.
C
      IF(NERR.LT.1.OR.NERR.GT.NUMERR) THEN
C
C No such error.
C
        ISTAT=-1
      ELSE
C
C Obtain the current status.
C
        ISTAT=IERTAB(NERR)/1000
C
C Non-fatal.
C
        IF(ISTAT.NE.0) THEN
          ISTAT=3-ISTAT
        ENDIF
      ENDIF
      RETURN
C
C Format.
C
  200 FORMAT(SS,' Library fatal error detected in routine ',A,
     &' at location ',I2)
C
C End of subroutine getler.
C
      END
