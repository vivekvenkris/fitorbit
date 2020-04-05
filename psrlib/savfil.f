*DECK SAVFIL
C
C
C
      SUBROUTINE SAVFIL(PARS,NPAR,INBUF,IUPTR,IUNIT,NUNITS,ISAVE,DCHAR,
     &                  CCHAR,LBUF,OBESAV,OUTBUF,IFAIL)
C
C This routine parses a 'save' command line, and if a file is requested
C it checks if a unit is available and if so attempts to open the
C requested file for writing.
C Input arguments:
C  PARS     - The parameters from the command line.
C  NPAR     - The number of these, if < 0 then it is assumed that there
C             is no command line and a save file specified by pars(1)
C             is to be opened (or closed if pars(1) is blank).
C  INBUF    - A character buffer which can hold at least one file name.
C  IUPTR    - The current input unit pointer.
C  IUNIT    - An array containing the unit numbers available.
C  NUNITS   - The number of these.
C  ISAVE    - The number of units used from the top of the unit numbers
C             array for any open save file (always either zero or one).
C  DCHAR    - The parameter delimiter character.
C  CCHAR    - The command line continuation character.
C  LBUF     - The usable size of the input buffer.
C  OUTBUF   - The output buffer.
C Output arguments:
C  ISAVE    - Set to one if a save file is opened, set to zero if a
C             save file is closed, else unchanged.
C  OBESAV   - Set to .true. If a save file was opened from within a
C             command file, set to .false. If not or a save file was
C             closed.  This parameter is not changed if a symbols'
C             file only is written.
C  IFAIL    - Zero if successful, else > 0.
C     version 2.0   14May85
C
      CHARACTER*(*) PARS(*),INBUF,DCHAR,CCHAR,OUTBUF,ROUTN,OLIST(6)*12
      DIMENSION IUNIT(*)
      LOGICAL OBESAV,COMSTR
      PARAMETER (ROUTN='SAVFIL')
      SAVE OLIST
      DATA OLIST/'COMMAND_FILE','SYMBOLS_FILE','END','DEFAULT','NONE',
     &           ' '/
C
C Check the number of parameters supplied.
C
      IF(NPAR.GE.0) THEN
C
C One is provided, set the default options.
C
        IPCFIL=-1
        IPSFIL=-1
C
C Start to parse the command line.
C
        IFAIL=0
        IP=2
 1000   IF(IP.LE.NPAR.AND.IFAIL.LE.0) THEN
C
C Check the current parameter.
C
          II=INTCMD(OLIST,PARS(IP))
          IF(II.GT.0) THEN
C
C It is a recognized parameter, case it.
C
            GOTO (2000,2001,2002,2002,2002) II
C
C Command_file, attempt to obtain the following value.
C
 2000       CALL GETIDC(PARS,NPAR,IP,IFAIL)
C
C Test if successful.
C
            IF(IFAIL.EQ.0) THEN
C
C Yes, check the file name.
C
              CALL CHKFIL(PARS,IP,INBUF,I,IFAIL)
C
C Test if sucessful.
C
              IF(IFAIL.EQ.0) THEN
C
C Yes, trap for default or 'none'.
C
                IF(INBUF.EQ.' '.OR.COMSTR(OLIST(5),INBUF)) THEN
C
C It is been given, note that a request has been made to close the save
C file.
C                 Ipcfil=0
                ELSE
C
C A file name is supplied, remember its position.
C
                  IPCFIL=IP
                ENDIF
              ENDIF
            ELSEIF(IFAIL.LT.0) THEN
C
C No file name is supplied, note that a request has been made to close
C the save file.
C
              IPCFIL=0
            ENDIF
            GOTO 2777
C
C Symbols_file, attempt to obtain the following value.
C
 2001       CALL GETIDC(PARS,NPAR,IP,IFAIL)
C
C Test if successful.
C
            IF(IFAIL.EQ.0) THEN
C
C Yes, check the file name.
C
              CALL CHKFIL(PARS,IP,INBUF,I,IFAIL)
C
C Test if successful.
C
              IF(IFAIL.EQ.0) THEN
C
C Possibly, but trap for default or 'none'.
C
                IF(INBUF.EQ.' '.OR.COMSTR(OLIST(5),INBUF)) THEN
C
C One of these options has been given, flag an error.
C
                  CALL LIBERR(ROUTN,29,0,0,0.0,'Symbols''',IFAIL)
                ELSE
C
C A file name has been supplied, note its position.
C
                  IPSFIL=IP
                ENDIF
              ENDIF
            ELSEIF(IFAIL.LT.0) THEN
C
C No file name was supplied, monitor an error.
C
              CALL LIBERR(ROUTN,29,1,0,0.0,'Symbols''',IFAIL)
            ENDIF
            GOTO 2777
C
C End, default, or none: just note that a request has been made to close
C the save file.
C
 2002       IPCFIL=0
          ELSEIF(II.EQ.0) THEN
C
C Unrecognized parameter, try to treat it as a valid save file name.
C
            CALL CHKFIL(PARS,IP,INBUF,I,IFAIL)
C
C Test if successful.
C
            IF(IFAIL.EQ.0) THEN
C
C Yes, note its position.
C
              IPCFIL=IP
            ELSE
C
C No, flag unrecognized option.
C
              CALL LIBERR(ROUTN,26,2,0,0.0,PARS(IP),IFAIL)
            ENDIF
          ELSE
C
C Ambiguous option (not actually possible at the moment).
C
            CALL LIBERR(ROUTN,37,3,0,0.0,PARS(IP),IFAIL)
          ENDIF
C
C Continue with the parse.
C
 2777     IP=IP+1
          GOTO 1000
        ENDIF
      ELSE
C
C Only a save file 'name' is supplied in pars(1), check it.
C
        CALL CHKFIL(PARS,1,INBUF,I,IFAIL)
C
C Test if successful.
C
        IF(IFAIL.EQ.0) THEN
C
C Yes, trap for 'end', 'none', or default.
C
          I=INTCMD(OLIST(3),PARS(1))
c          IF(I.EQ.0.OR.IPBUF.EQ.' ') THEN
          IF(I.EQ.0) THEN
C
C Request that the save file be closed.
C
            IPCFIL=0
          ELSE
C
C Request that it be opened.
C
            IPCFIL=1
          ENDIF
C
C Set ipsfil to a harmless value.
C
          IPSFIL=-1
        ENDIF
      ENDIF
C
C Test if successful so far.
C
      IF(IFAIL.LE.0) THEN
C
C Yes, was a request made or implied to close the save file?
C
        IF(IPCFIL.EQ.0.OR.IPCFIL.LT.0.AND.IPSFIL.LT.0) THEN
C
C Yes, check if a save file is open.
C
          IFAIL=0
          IF(ISAVE.NE.0) THEN
C
C One is, close the save file and stop any saving.
C
            CLOSE(IUNIT(NUNITS),IOSTAT=I)
            ISAVE=0
C
C Test if successful.
C
            IF(I.NE.0) THEN
C
C No, flag an error.
C
              CALL LIBERR(ROUTN,66,4,I,0.0,'Save',IFAIL)
            ELSE
C
C Yes, clear 'save file opened in obey file' flag.
C
              OBESAV=.FALSE.
            ENDIF
          ENDIF
        ENDIF
C
C Test if sucessful so far.
C
        IF(IFAIL.LE.0) THEN
C
C Yes, was a request made to write out symbols' file?
C
          IF(IPSFIL.GT.0) THEN
C
C One was, check that there is a unit available to do so.
C
            IF(IUPTR.LT.NUNITS-ISAVE) THEN
C
C There is, write the file.
C
              CALL SYMSAV(IUNIT(IUPTR+1),PARS(IPSFIL),IFAIL)
              IF(IFAIL.NE.0.AND.NPAR.GT.0) PARS(1)=PARS(IPSFIL)
            ELSE
C
C There are no spare units, flag an error.
C
              CALL LIBERR(ROUTN,28,5,IUPTR-1,0.0,' ',IFAIL)
            ENDIF
          ENDIF
C
C Test if successful so far.
C
          IF(IFAIL.LE.0) THEN
C
C Yes, was a request made to open a save file?
C
            IF(IPCFIL.GT.0) THEN
C
C Yes, check that there is a unit available for it.
C
              IF(IUPTR.GE.NUNITS) THEN
C
C No, flag an error.
C
                CALL LIBERR(ROUTN,28,6,IUPTR-1,0.0,' ',IFAIL)
              ELSE
C
C Yes, close the previous file if any.
C
                IF(ISAVE.NE.0) THEN
C
C Close the file.
C
                  CLOSE(IUNIT(NUNITS),IOSTAT=I)
C
C Test if successful.
C
                  IF(IFAIL.NE.0) THEN
C
C No, flag an error and stop any saving.
C
                    CALL LIBERR(ROUTN,66,7,I,0.0,'Save',IFAIL)
                    ISAVE=0
                  ENDIF
                ENDIF
C
C Test if sucessful.
C
                IF(IFAIL.EQ.0) THEN
C
C Yes, try to open the file.
C
                  OPEN(IUNIT(NUNITS),FILE=PARS(IPCFIL),STATUS='NEW',
     &                 IOSTAT=I)
C
C Test if successful.
C
                  IF(I.EQ.0) THEN
C
C Yes, set the save unit decrement.
C
                    ISAVE=1
C
C Set the 'save file opened in obey file flag'.
C
                    IF(NPAR.GE.0) THEN
                      OBESAV=IUPTR.GT.1
                    ELSE
                      OBESAV=.FALSE.
                    ENDIF
                  ELSE
C
C Unable to open the file, flag an error.
C
                    CALL LIBERR(ROUTN,30,8,I,0.0,INBUF,IFAIL)
                    IF(NPAR.GE.2) PARS(1)=PARS(IPCFIL)
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
C
C Test if successful.
C
            IF(IFAIL.LE.0) THEN
C
C Yes, was a symbols' file written and are we saving?
C
              IF(IPSFIL.GT.0.AND.ISAVE.GT.0) THEN
C
C Yes, shift the parameters relating to the symbols' file down.
C
                DO 1 I=2,4
                  PARS(I)=PARS(IPSFIL+I-4)
    1           CONTINUE
C
C Now write this to the save file.
C
                CALL SAVWRT(PARS,4,ISAVE,IUPTR-1,IUNIT(NUNITS),DCHAR,
     &                      OUTBUF,LBUF,IFAIL)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      RETURN
C
C End of subroutine savfil.
C
      END
