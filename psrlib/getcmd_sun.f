*DECK GETCMD
C
C
C
      SUBROUTINE GETCMD (CLIST, PROMPT, PARS, MAXP, NP, ICOM, PROGN,
     &                   IFAIL)
C$pragma C(new_ol_rdlin)
C
C This routine reads a command line and splits it into its component
C parameters.
C
C The following entry points are also defined:
C  QUIET  - Enables or suppresses command line echoing.
C  MACCMD - Sets an obey file.
C  SAVCMD - Sets a save file.
C  CLRCMD - Clears all pending command levels.
C  SPARSE - Parses a SHOW command.
C
C Arguments:
C  CLIST   Input   CHARACTER*(*)  A list of allowed commands, terminated
C                                 with a blank entry.
C  PROMPT  Input   CHARACTER*(*)  The command prompt to issue (blank if
C                                 none).
C  PARS    Output  CHARACTER*(*)  An array to receive the parameters
C                                 from the command line.
C  MAXP    Input   INTEGER        The maximum number of parameters that
C                                 can be returned.  Note that PARS
C                                 should be dimensioned PARS(MAXP+1).
C  NP      Output  INTEGER        The number of parameters (including
C                                 the command).
C  ICOM    Output  INTEGER        The position of the command in CLIST
C                                 (the first entry is numbered 1).  ICOM
C                                 is returned as zero if no command
C                                 match was found, or as minus the
C                                 number of the first command found if
C                                 ambiguous.
C  PROGN   Input   CHARACTER*(*)  The name of the calling program.
C  IFAIL   Output  INTEGER        The failure indicator: zero if no
C                                 errors were detected, else greater
C                                 than zero.
C
C     The commands HELP, OBEY, SAVE, SYMBOLS, and EXPLAIN are handled
C automatically by this routine.  In addition command lines starting
C with the character returned by OSCHAR are passed to the operating
C system for execution.
C
C     Version 5.2   29th July, 1988
C
C Declare the routine's arguments.
C
      CHARACTER*(*) CLIST(*), PROMPT, PARS(*), PROGN
      INTEGER       MAXP, NP, ICOM, IFAIL
C
C Declare additional arguments for the alternative entry points.
C
      CHARACTER*(*) FNAME, BUFFER
      INTEGER       IRPT, IFUNIT, ICP, ICMD
      LOGICAL       SILNT
C
C The parameters below are:
C LBUF   - The length of the input buffer, this must be one more than
C           the maximum command line length that is to be supported.
C NUNITS - The number of input/output units defined for use with obey,
C           save, and symbols files (the first of these is for the
C           default input unit).
C CCHAR  - The continuation character.
C CPCHAR - The continuation prompt character.
C ESTRNG - The string to prefix a command line with when echoing it.
C DCHAR  - The delimiter character.
C CMTCHR - The comment starting character.
C HCHAR  - The help command equivalent character.
C
C     Version 5.0   8th July, 1988
C     Version 6.0  18th Mar 2002 try and make call to ol_rdlin work 
c                                under curent OS
C put in hte reentrant stuff pah 
      INCLUDE 'REENT_COMM.DEF'

      CHARACTER*(*) CCHAR,CPCHAR,ESTRNG,DCHAR,CMTCHR,HCHAR
      PARAMETER (LBUF=132,
     &           NUNITS=5,
     &           CCHAR='-',
     &           CPCHAR='_',
     &           ESTRNG='>',
     &           DCHAR='$',
     &           CMTCHR='*',
     &           HCHAR='?')
C
C Define parameters associated with the explain command handling:
C EUNIT  - The unit number to read explain files on.
C FUNIT  - The unit number to write explain files on.
C PUNIT  - The unit number to print explain files on.
C TPAGE  - The number of lines on a terminal screen.
C PPAGE  - The number of lines on a printer page.
C LINEW  - The maximum line width in characters.
C
      INTEGER    EUNIT, FUNIT, PUNIT, TPAGE, PPAGE, LINEW
      PARAMETER (EUNIT = 20,
     &           FUNIT = 21,
     &           PUNIT = 22,
     &           TPAGE = 24,
     &           PPAGE = 62,
     &           LINEW = 132)
C
C Declare external references.
C
      CHARACTER*1   PRMTCC, LITDEL, OSCHAR, SUPCHR, NUPDEL
c      CHARACTER*132  OL_RDLIN
      INTEGER       NEW_OL_RDLIN
      INTEGER       LENGTH, INTDCM
      LOGICAL       BATCH, PRMSEP, SPECHR, EOLCHR, COMSTR
C
C Declare local variables.
C
      CHARACTER*(*) IPBUF(NUNITS)*(LBUF), PRMPT*20, EPRMPT*22, CC*1,
     &              OUTBUF*180, GLIST(7)*7,CBUFF*13, RBUFF*133
      INTEGER       IUNIT(NUNITS), NRPT(2:NUNITS), BUFPOS(2,NUNITS),
     &              IUPTR, ISAVE, IS, LPRMT, LEPRMT, MAXPL, NC, ISS,
     &              IC, ILIST, I, J, RETLEN
      LOGICAL       DELFLG, SILFLG, SILENT, OBESAV, CNTFLG, LITFLG,
     &              FIRST
      BYTE          CBUFF_EQUIV(133), RBUFF_EQUIV(133)
      EQUIVALENCE   (CBUFF_EQUIV, CBUFF)
      EQUIVALENCE   (RBUFF_EQUIV, RBUFF)
      SAVE          SILENT, IUNIT, IUPTR, ISAVE, GLIST, OBESAV, NRPT,
     &              BUFPOS, IPBUF, NC
 
C
C Define the routine's name.
C
      CHARACTER     ROUTN*(*)
      PARAMETER    (ROUTN = 'GETCMD')
C
C Initialise the silent flag, the input unit pointer, the save unit unit
C pointer, and the starting values of the buffer positions.
C
      DATA          SILENT, IUPTR, ISAVE, BUFPOS(1,1), BUFPOS(2,1)
     &              / .FALSE., 1, 0, 0, -1/
C                                            
C Define the I/O units numbers (-1 = default input unit).
C
      DATA          IUNIT /-1, 1, 2, 3, 4 /
C
C Define the commands handled by GETCMD.
C
      DATA          glist / HCHAR, 'help', 'obey', 'save', 'symbols',
     &                      'explain', ' '/
C
C Set the prompt and echo strings.
C
      PRMPT = PROMPT(:MIN(LEN(PRMPT)-1,MAX(1,LENGTH(PROMPT))))
      EPRMPT = ' ' // PRMPT(:LENGTH(PRMPT)) // ESTRNG
C
C Evaluate the length of these.
C
      LPRMT = LENGTH (PRMPT)
      LEPRMT = LENGTH (EPRMPT)
C
C Obtain the maximum parameter length.
C
      MAXPL = LEN (PARS(1))
C
C Start to parse the command line(s).
C
      IFAIL = -1
 1000 IF (IFAIL .LT. 0) THEN
C
C Clear the final character in the input buffer.
C
        IPBUF(IUPTR)(LBUF:) = ' '
C
C Set the value of the silent flag for the current command line.  If
C this is .TRUE. then commands are not echoed.  Note that command lines
C are always echoed when processing an obey file.
C
        SILFLG = SILENT .AND. IUPTR .LE. 1
C
C Initialize the command number, continuation flag, and continuation
C prompt character.
C
        ICOM = 0
        CNTFLG = .TRUE.
        CC = ' '
C
C Clear the parameter counter.
C
        NP = 0
C
C Start to parse the input line(s) making up the current command line.
C
 1001   IF (IFAIL .LE. 0 .AND. CNTFLG) THEN
C
C Clear the continuation flag.
C
          CNTFLG = .FALSE.
C
C Obtain the current position within the current input buffer.
C
          IS = BUFPOS(1,IUPTR)
C
C Are we at its end?
C
          IF (IS .GT. BUFPOS(2,IUPTR)) THEN
C
C Yes; use the interface to GNU readline if we're not in batch, and
C not reading from an obey file.
C
            IF ((.NOT.BATCH ()) .AND. (IUPTR .LE. 1)
     &           .AND. REECNT.LE.0) THEN
c
c               WRITE (*, '(A)') PRMTCC () // PRMPT(:LPRMT) // CC
c            END IF
c              IPBUF(IUPTR) = OL_RDLIN(PRMPT, LPRMT, IFAIL)
c mods to get ol_rdlin to work! put it into a byte array and zero terminate!
              cbuff = prmpt(1:lprmt)
              cbuff_equiv(lprmt+1)=0
              IFAIL = NEW_OL_RDLIN(CBUFF_EQUIV, %VAL(LPRMT), 
     &                                    RBUFF_EQUIV, %VAL(LBUF))
              IPBUF(IUPTR) = RBUFF
            else
C
C Read the next input line using the generic routine.
C
              CALL RDWLTH (IUNIT(IUPTR), IPBUF(IUPTR), LBUF - 1, NC,
     &                   IFAIL)
            endif
C
C Set the start and the nominal end of the input buffer.
C
            IS = 1
            BUFPOS(1,IUPTR) = IS
            BUFPOS(2,IUPTR) = 0
C
C Note that the current command is the first on the line.
C
            FIRST = .TRUE.
C
C Test if successful.
C
            IF (IFAIL .EQ. 0) THEN
C
C Yes, echo the line if requested.
C
              IF (.NOT.SILFLG) THEN
                CALL OUTLIN (EPRMPT(:LEPRMT) // CC //
     &                       IPBUF(IUPTR)(:MIN(MAX(1,NC),LBUF-1)), 1)
              END IF
C
C Check the line length.
C
              IF (NC .GE. LBUF) THEN
C
C It was too long, flag an error.
C
                CALL LIBERR (ROUTN ,64, 0, LBUF-1, 0.0, ' ', IFAIL)
              ELSE
C
C It is not too long, find its start.
C
                NC = LENGTH(IPBUF(IUPTR))
 1002           IF (IS .LE. NC) THEN
                  IF (PRMSEP (IPBUF(IUPTR)(IS:IS))) THEN
                    IS = IS + 1
                    GOTO 1002
                  END IF
C
C Found it, test if the line is a comment.
C
                  IF (IPBUF(IUPTR)(IS:IS) .EQ. CMTCHR .AND.
     &                PRMSEP (IPBUF(IUPTR)(IS+1:IS+1))) THEN
C
C It is, write it to the save file if one is open and we are not in an
C obey file.
C
                    CALL SAVWRT (IPBUF(IUPTR)(:NC), 0, ISAVE, IUPTR,
     &                           IUNIT(NUNITS), DCHAR, CCHAR, OUTBUF, 
     &                           LBUF-1, IFAIL)
C
C Force a new line to be read.
C
                    IFAIL = -1
                  ELSE IF (IPBUF(IUPTR)(IS:IS) .EQ. OSCHAR ()) THEN
C
C The command line starts with "oschar", send it to the operating
C system.
C                                                         
                    CALL GIVEOS (IPBUF(IUPTR)(IS+1:), I)
C
C Test if successful.
C
                    IF (I .NE. 0) THEN
C
C No, monitor an error.
C                            
                      CALL LIBERR (ROUTN, 70, 8, 0, 0.0, ' ', IFAIL)
                    END IF
C
C Force a new line to be read.
C
                    IFAIL = -1
                  ELSE
C
C The line is not a comment and is not an operating system command:
C set where it ends into the buffer positions array.
C
                    BUFPOS(2,IUPTR) = NC
                  END IF
                END IF
              END IF
            ELSE IF (IFAIL .LT. 0) THEN
C
C Input commands end of file encountered, was the last input line a
C continuation?  (Note that we cannot use CNTFLG to test for this
C because it is reset at the start of a command section.)
C
              IF (CC .EQ. ' ') THEN
C
C No, were we reading from an obey file?
C
                IF (IUPTR .GT. 1) THEN
C
C We were, decrement its repeat count.
C
                  NRPT(IUPTR) = NRPT(IUPTR) - 1
C
C Have we finished repeating it?
C
                  IF (NRPT(IUPTR) .LE. 0) THEN
C
C Yes, close the file.
C
                    CLOSE (UNIT = IUNIT(IUPTR), IOSTAT = I)
C
C Test if successful.
C
                    IF (I .EQ. 0) THEN
C
C Yes, clear the buffer pointers for this unit.
C
                      BUFPOS(1,IUPTR) = 1
                      BUFPOS(2,IUPTR) = 0
C
C Decrement the unit pointer to continue with the previous file.
C
                      IUPTR = IUPTR - 1
C
C Clear the 'save file opened in obey file' flag if we are back at the
C main command level.
C
                      IF (IUPTR .LE. 1) OBESAV = .FALSE.
C
C Clear the continuation flag to force a new command line.
C
                      CNTFLG = .FALSE.
                      IFAIL = -1
                    ELSE
C
C Unable to close the file, flag an error.
C
                      CALL LIBERR (ROUTN, 66, 1, I, 0.0, 'Obey', IFAIL)
                    END IF
                  ELSE
C
C Repeat the file again.
C
                    REWIND (UNIT = IUNIT(IUPTR), IOSTAT = I)
C
C Test if successful.
C
                    IF (I .NE. 0) THEN
C
C No, flag an error.
C
                      CALL LIBERR (ROUTN, 67, 2, I, 0.0, ' ', IFAIL)
                   ELSE
C
C Yes, clear the continuation flag to force a new command line.
C
                      CNTFLG = .FALSE.
                      IFAIL = -1
                    END IF
                  END IF
                ELSE
C
C We have reached end of file on the main command file, flag an error.
C
                  CALL LIBERR (ROUTN, 1, 3, IUNIT(IUPTR), 0.0, ' ',
     &                         IFAIL)
                END IF
              ELSE
C
C An end of file has been encountered when a the previous line expected
C a continuation, flag an error.
C
                CALL LIBERR (ROUTN, 2, 4, IUNIT(IUPTR), 0.0, ' ', IFAIL)
              END IF
            ELSE
C
C Command line read error, flag an error.
C
              I = IFAIL
              CALL LIBERR (ROUTN, 68, 5, I, 0.0, ' ', IFAIL)
            END IF
          ELSE
C
C There is still something left in the current buffer: search for the
C start of the next command.
C
 1003       IF (IS .LE. NC) THEN
              IF (PRMSEP (IPBUF(IUPTR)(IS:IS))) THEN
                IS = IS + 1
                GOTO 1003
              END IF
            END IF
C
C Indicate that the current command is not the first on a line.
C
            FIRST = .FALSE.
C
C Indicate success.
C
            IFAIL = 0
          END IF
C
C Test if successful so far.
C
          IF (IFAIL .EQ. 0) THEN
C
C Yes, start to split the current command line into parameters.
C
 1004       IF (IFAIL .EQ. 0 .AND. IS .LE. NC) THEN
C
C Locate the start of the next parameter.
C
              IF (PRMSEP (IPBUF(IUPTR)(IS:IS))) THEN
                IS = IS + 1
                GOTO 1004
              END IF
C
C Found one, is there room for it?
C
              IF (NP .LE. MAXP) THEN
C
C Yes, increment the parameter counter, clear its parameter count, and
C remember where it starts.
C
                NP = NP + 1
                IC = 0
                ISS = IS
C
C Set the parameter delimited flag.
C
                DELFLG = IPBUF(IUPTR)(IS:IS) .EQ. DCHAR .AND.
     &                   IPBUF(IUPTR)(IS+1:IS+1) .NE. DCHAR
C
C Skip to the next character if the first one is a delimiter.
C
                IF (DELFLG) IS = IS + 1
C
C Now load the characters into the current parameter until the next
C terminator.
C
 1005           IF (IFAIL .EQ. 0 .AND. (DELFLG .AND. IS .LE. NC .OR.
     &              .NOT. (PRMSEP (IPBUF(IUPTR)(IS:IS)) .OR.
     &                     SPECHR (IPBUF(IUPTR)(IS:IS)) .OR.
     &                     EOLCHR (IPBUF(IUPTR)(IS:IS))))) THEN
C
C Is the current parameter a terminating delimiter?
C
                  IF (DELFLG .AND. IPBUF(IUPTR)(IS:IS) .EQ. DCHAR .AND.
     &                (PRMSEP (IPBUF(IUPTR)(IS+1:IS+1)) .OR.
     &                 SPECHR (IPBUF(IUPTR)(IS+1:IS+1)) .OR.
     &                 EOLCHR (IPBUF(IUPTR)(IS+1:IS+1)))) THEN
C
C It is, clear the delimiter flag.
C
                    DELFLG = .FALSE.
                  ELSE
C
C It is not, if it is a "double delimiter" skip the first character in
C it.
C
                    IF (IPBUF(IUPTR)(IS:IS+1) .EQ. DCHAR // DCHAR) THEN
                      IS = IS + 1
                    END IF
C
C Check that there is room for the current character in the parameter.
C
                    IF (IC .LT. MAXPL) THEN
C
C There is, load it into the parameter.
C
                      IC = IC + 1
                      PARS(NP)(IC:IC) = IPBUF(IUPTR)(IS:IS)
                    ELSE
C
C The parameter is full, flag an error.
C
                      CALL CMDSYE (PARS, OUTBUF, IPBUF(IUPTR), NC,
     &                             SILFLG, PRMPT, EPRMPT, FIRST, ISS,
     &                             IS, 4, MAXPL, IFAIL)
                    END  IF
                  END IF
C
C Process the next character.
C
                  IS = IS + 1
                  GOTO 1005
                END IF
C
C Was the parameter loaded successfully?
C
                IF (IFAIL .EQ. 0) THEN
C
C Yes, pad it out with blanks.
C
                  IF (IC .LT. MAXPL) PARS(NP)(IC+1:) = ' '
C
C Test the terminating character.
C
                  IF (DELFLG) THEN
C
C Missing delimiter, flag an error.
C
                    CALL CMDSYE (PARS, OUTBUF, IPBUF(IUPTR), NC, SILFLG,
     &                           PRMPT, EPRMPT, FIRST, ISS, IS, 65, 0,
     &                           IFAIL)
                  ELSE IF (SPECHR (IPBUF(IUPTR)(IS:IS))) THEN
C
C It is one of the "special" characters: load it into a parameter of its
C own.  First test if anything has been written into the current
C parameter.
C
                    IF (IC .GT. 0) THEN
C
C It has, use the next parameter, but first check if there is one.
C
                      IF (NP .LT. MAXP) THEN
C
C There is, load it with the special character.
C
                        NP = NP + 1
                        PARS(NP) = IPBUF(IUPTR)(IS:IS)
                      ELSE
C
C There is not, flag an error.
C
                        CALL CMDSYE (PARS, OUTBUF, IPBUF(IUPTR), NC,
     &                               SILFLG, PRMPT, EPRMPT, FIRST, IS,
     &                               IS, 3, MAXP, IFAIL)
                      END IF
                    ELSE
C
C Load the special character into the unused current parameter.
C
                      PARS(NP) = IPBUF(IUPTR)(IS:IS)
                    END IF
                  ELSE IF (EOLCHR (IPBUF(IUPTR)(IS:IS))) THEN
C
C The terminating character is an end-of-command character: force an end
C to the current command line scan.
C
                    IFAIL = -1
C
C Is the entire parameter the end of command character?
C
                    IF (IC .EQ. 0) THEN
C
C It is: remove it from the parameters.
C
                      NP = NP - 1
                    END IF
                  END IF
                END IF
              ELSE
C
C Too many parameters have been specified, flag an error on the maxp+1th
C parameter.
C
                CALL CMDSYE (PARS, OUTBUF, IPBUF(IUPTR), NC, SILFLG,
     &                       PRMPT, EPRMPT, FIRST, ISS, ISS, 3, MAXP,
     &                       IFAIL)
              END IF
C
C Skip to the next character and process the next parameter.
C
              IS = IS + 1
              GOTO 1004
            END IF
C
C Remember the current buffer location.
C
            BUFPOS(1,IUPTR) = IS
C
C We have finished processing the current command line, test if
C successful.
C
            IF (IFAIL .LE. 0 .AND. NP .GT. 0) THEN
C
C Yes, set the continuation flag.
C
              CNTFLG = PARS(NP) .EQ. CCHAR
C
C Is a continuation line to follow?
C
              IF (CNTFLG) THEN
C
C Yes, decrement the parameter count (the last one is the continuation
C character).
C
                NP = NP - 1
C
C Set the continuation prompt character.
C
                CC = CPCHAR
              ELSE IF (NP .GT. MAXP) THEN
C
C Too many parameters have been supplied, flag an error on the maxp+1th
C parameter.
C
                CALL CMDSYE (PARS, OUTBUF, IPBUF(IUPTR), NC, SILFLG,
     &                       PRMPT, EPRMPT, FIRST, ISS, ISS, 3, MAXPL,
     &                       IFAIL)
              END IF
            END IF
          END IF
C
C Continue reading the current command.
C
          GOTO 1001
        END IF
C
C Have we obtained a command line successfully?
C
        IF (IFAIL .LE. 0) THEN
C
C Yes, are there any parameters in it?
C
          IF (NP .GT. 0) THEN
C
C There are, do symbol substitution on the first parameter before
C checking the command.
C
            IP = 1
            LITFLG = .FALSE.
            CALL SYMTRN (PARS, NP, IP, MAXP, LITFLG, OUTBUF, IFAIL)
C
C Test if successful.
C
            IF (IFAIL .EQ. 0) THEN
C
C Yes, obtain the command's position (if any) in clist and glist.  Note
C that commands which have become comments or operating system commands
C are as a result of symbol substitution are not treated as such at this
C late stage because the reconstruction of the command line could be
C ambigous.
C
              IFAIL = 0
              ICOM = INTDCM (CLIST, GLIST, PARS(1), ILIST)
C
C Trap for ambiguity.
C
              IF (ICOM .LT. 0) THEN
C
C Ambiguous command, flag an error.
C
                CALL LIBERR (ROUTN, 6, 6, 0, 0.0, PARS(1), IFAIL)
              ELSE IF (ICOM .GT. 0 .AND. ILIST .GE. 2) THEN
C
C It is one of the commands handled by GETCMD.  Complete the symbol
C substitution if it is not the SYMBOLS command, otherwise just parse
C it for case conversion.
C
                IF (ICOM .NE. 5) THEN
                  CALL SYMSUB (PARS, NP, IP, MAXP, LITFLG, OUTBUF,
     &                         IFAIL)
                ELSE
                  CALL CASPRS (PARS, NP, IP, LITFLG)
                END IF
C
C Test if successful.
C
                IF (IFAIL .EQ. 0) THEN
C
C Yes, case the command.
C
                  GOTO (2000, 2000, 2001, 2002, 2003, 2004) ICOM
C
C Help.
C
 2000             CALL OLHELP (IUNIT(IUPTR), MAXP, PARS(2), NP-1,
     &                         OUTBUF, PROGN, I)
                  GOTO 2777
C
C Obey, handle this command.
C
 2001             I = IUPTR
                  CALL INCUNI (PARS, NP, OUTBUF, IUPTR, IUNIT,
     &                         NUNITS - ISAVE, NRPT, -1, IFAIL)
C
C Test if successful.
C
                  IF (IFAIL .EQ. 0) THEN
C
C Yes, check if a new command file was opened.
C
                    IF (IUPTR .GT. I) THEN
C
C One was, initialise the buffer pointers for this file.
C
                      BUFPOS(1,IUPTR) = 1
                      BUFPOS(2,IUPTR) = 0
                    END IF
C
C Save the command line if required.
C
                    CALL SAVWRT (PARS, NP, ISAVE, IUPTR - 1,
     &                           IUNIT(NUNITS), DCHAR, CCHAR, OUTBUF,
     &                           LBUF - 1, IFAIL)
                   END IF
                  GOTO 2777
C
C Save, handle this command.
C
 2002             CALL SAVFIL (PARS, NP, OUTBUF, IUPTR, IUNIT, NUNITS,
     &                         ISAVE, DCHAR, CCHAR, LBUF - 1, OBESAV,
     &                         OUTBUF, IFAIL)
                  GOTO 2777
C
C Symbols, handle this command.
C
 2003             CALL SYMCMD (PARS, NP, IFAIL)
C
C Save the command line if successful and if required.
C
                  IF (IFAIL .EQ. 0) THEN
                    CALL SAVWRT (PARS, NP, ISAVE, IUPTR - 1,
     &                           IUNIT(NUNITS), DCHAR, CCHAR, OUTBUF,
     &                           LBUF - 1, IFAIL)
                  END IF
                  GOTO 2777
C
C Explain, handle this command.
C                                                                  
 2004             CALL OLEXPL (PARS, NP, PROGN, EUNIT, FUNIT, PUNIT,
     &                         TPAGE, PPAGE, LINEW, OUTBUF, IFAIL)
C
C Force another command to be read if successful.
C
 2777             IF (IFAIL .EQ. 0) IFAIL = -1
                END IF
              ELSE IF (ICOM .GT. 0) THEN
C
C It is a command handled by the calling program: complete the symbol
C subsitution.
C
                CALL SYMSUB (PARS, NP, IP, MAXP, LITFLG, OUTBUF,
     &                       IFAIL)
C
C Write the command line to the save file if successful and if required.
C
                IF (IFAIL .EQ. 0) THEN
                  CALL SAVWRT (PARS, NP, ISAVE, IUPTR, IUNIT(NUNITS),
     &                         DCHAR, CCHAR, OUTBUF, LBUF -1, IFAIL)
                END IF
              ELSE
C
C Unrecognized command, complete the symbol substitution and flag an
C error.
C
                CALL SYMSUB (PARS, NP, IP, MAXP, LITFLG, OUTBUF, IFAIL)
                CALL LIBERR (ROUTN, 5, 7, 0, 0.0, PARS(1), IFAIL)
              END IF
            END IF
          ELSE
C
C The command line was empty, set IFAIL to force processing to continue.
C
            IFAIL = -1
          END IF
        END IF
C
C Get the next command if necessary.
C
        GOTO 1000
      END IF
      RETURN
C
C
C
C
      ENTRY QUIET (SILNT)
C
C This entry point is used to prevent or enable the echoing of command
C lines.
C
C Argument:
C  SILNT   Input  LOGICAL  .TRUE. if echoing is to be disabled, else
C                          .FALSE..
C
C Set the silent request: .TRUE. = no echo, .FALSE. = echo.
C
      SILENT = SILNT
      RETURN
C
C
C
C
      ENTRY MACCMD (FNAME, IRPT, IFUNIT, IFAIL)
C
C This entry point is used to force the obeying of a command file.
C
C Arguments:
C  FNAME   Input   CHARACTER*(*)  The name of the command file.
C  IRPT    Input   INTEGER        The number of times that the file is
C                                 to be obeyed.
C  IFUNIT  Output  INTEGER        The unit number used for the file, -1
C                                 if a file was not opened.
C  IFAIL   Output  INTEGER        Zero if successful, else >0.
C
C Check the file name and repeat count, increment the unit pointer, and
C open the file.
C
      CALL INCUNI (FNAME, -1, OUTBUF, IUPTR, IUNIT, NUNITS, NRPT, IRPT,
     &             IFAIL)
C
C Test if successful.
C
      IF (IFAIL .EQ. 0) THEN
C
C Yes, return the unit number used.
C
        IFUNIT = IUNIT(IUPTR)
C
C Initialise the buffer pointers for the new file.
C
        BUFPOS(1,IUPTR) = 1
        BUFPOS(2,IUPTR) = 0
      ELSE
C
C No, return a negative unit number.
C
        IFUNIT = -1
      END IF
      RETURN
C
C
C
C
      ENTRY SAVCMD (FNAME, IFUNIT, IFAIL)
C
C This entry point is used to force a new save file.
C
C Arguments:
C  FNAME   Input   CHARACTER*(*)  The file name.  If this is blank,
C                                 'END', 'NONE', 'DEFAULT', or a
C                                 shortened form of these, then any
C                                 previously open save file is closed.
C  IFUNIT  Output                 The unit number used for the file, or
C                                 -1 if a file was not opened.
C  IFAIL   Output                 Zero if successful, else >0.
C
C Set or clear the save file.
C
      CALL SAVFIL (PARS, -1, OUTBUF, IUPTR, IUNIT, NUNITS, ISAVE, DCHAR,
     &             CCHAR, LBUF - 1, OBESAV, OUTBUF, IFAIL)
C
C Test if successful.
C
      IF (IFAIL .EQ. 0 .AND. ISAVE .GE. 1) THEN
C
C A new file has been set, return its unit number.
C
        IFUNIT = IUNIT(NUNITS)
      ELSE
C
C The set failed, or the save file was cancelled, return a negative unit
C number.
C
        IFUNIT = -1
      END IF
      RETURN
C
C
C
C
      ENTRY CLRCMD
C
C This entry point is used to set the command input file back to the
C default, and to close the save file if necessary.  Any remaining
C commands on the same line(s) are ignored.  CLRCMD is normally called
C following an error.
C
C Close any open obey files, and clear the associated buffer positions.
C
      DO 1 I = IUPTR, 2, -1
        CLOSE (UNIT = IUNIT(I), IOSTAT = J)
        BUFPOS(1,I) = 0
        BUFPOS(2,I) = -1
    1 CONTINUE
C
C Set the unit pointer back to the main command file.
C
      IUPTR = 1
C
C Clear the buffer positions for the main command file.
C
      BUFPOS(1,1) = 0
      BUFPOS(2,1) = -1
C
C Is a save file open?
C
      IF (ISAVE .GT. 0) THEN
C
C Yes, was it opened in an obey file?
C
        IF (OBESAV) THEN
C
C It was, close it.  Save files opened at the main command level or by
C program are the responsibility of the user or the program repectively,
C so these are not closed.
C
          CLOSE (UNIT = IUNIT(NUNITS), IOSTAT = I)
C
C Test if successful.
C
          IF (I .EQ. 0) THEN
C
C Yes, say that it has been closed.
C
            CALL OUTLIN ('      Save file has been closed', -1)
          END IF
C
C Clear the save unit decrement.
C
          ISAVE = 0
        END IF
      END IF
      RETURN
C
C
C
C
      ENTRY SPARSE (PARS, NP, ICP, CLIST, ICMD, BUFFER, IFAIL)
C
C This entry point is used to parse a segment of a SHOW command line.
C Any commands that are handled by GETCMD are handled internally, others
C are passed back to the caller to handle.
C
C Arguments:
C  PARS    Input         CHARACTER*(*)  The parameters from the SHOW
C                                       command line.
C  NP      Input         INTEGER        The number of these.
C  ICP     Input/output  INTEGER        The current parameter position,
C                                       if this is > NP it is assumed
C                                       that the command line is empty.
C                                       On exit, ICP points to the last
C                                       parameter processed.
C  CLIST   Input         CHARACTER*(*)  The calling program's command
C                                       list.
C  ICMD    Output        INTEGER        The command pointer:
C                                        >0 = The command number to
C                                             print information for.
C                                         0 = Print no information.
C                                        -1 = Print information for
C                                             all commands.
C                                        -2 = Print the default
C                                             information.
C  BUFFER  Output        CHARACTER*(*)  A character work space buffer
C                                       used to hold an output line.
C  IFAIL   Output        INTEGER        Zero if successful, else >0.
C
C Check which parameter we are starting with.
C
      IF (ICP .LE. NP) THEN
C
C There is one, start to parse the line.
C
        IFAIL = 0
 3000   IF (ICP .LE. NP .AND. IFAIL .EQ. 0) THEN
C
C Check the current parameter.
C
          ICMD = INTDCM (CLIST, GLIST, PARS(ICP), ILIST)
          IF (ICMD .GT. 0) THEN
C
C It is recognized command, check which command list it is in.
C
            IF (ILIST .LE. 1) THEN
C
C It is in the program's list, just force an exit.
C
              IFAIL = -1
            ELSE
C
C It is a command that is handled by GETCMD, case it after printing its
C name.
C
              CALL OUTLIN (' ' // GLIST(ICMD), 1)
              GOTO (4000, 4000, 4001, 4002, 4003, 4000) ICMD
C
C 'HELP' or 'EXPLAIN', there is no information we can print for this.
C Tell him about the HELP command in case he is in trouble.
C
 4000         CALL OUTLIN ('   Use the HELP command for help', 1)
              GOTO 4777
C
C 'OBEY', print out the current obey file name.
C
 4001         CALL SHOOBE (BUFFER, IUPTR, IUNIT)
              GOTO 4777
C
C 'SAVE', print out the current save file name.
C
 4002         CALL SHOSAV (BUFFER, ISAVE, NUNITS, IUNIT)
              GOTO 4777
C
C 'SYMBOLS', parse this construct.
C
 4003         CALL SHOSYM (PARS, NP, ICP, BUFFER, IFAIL)
C
C Clear the command number and continue with the parse.
C
 4777         ICMD = 0
              ICP = ICP + 1
            END IF
          ELSE IF(ICMD .LT. 0) THEN
C
C Ambiguous option, flag an error.
C
            CALL LIBERR (ROUTN, 37, 9, 0, 0.0, PARS(ICP), IFAIL)
          ELSE
C
C Unrecognized option, check if it is 'ALL'.
C
            IF (COMSTR ('ALL', PARS(ICP))) THEN
C
C It is, print the obey file, save file, and symbols.
C
              CALL OUTLIN (' ' // GLIST(3), 1)
              CALL SHOOBE (BUFFER, IUPTR, IUNIT)
              CALL OUTLIN (' ' // GLIST(4), 1)
              CALL SHOSAV (BUFFER, ISAVE, NUNITS, IUNIT)
              CALL OUTLIN (' ' // GLIST(5), 1)
              CALL SHOSYM (PARS, ICP, ICP, BUFFER, IFAIL)
C
C Let the calling program know that 'ALL' is requested and force an
C exit.
C
              ICMD = -1
              IFAIL = -1
            ELSE
C
C It really is an unrecognized option, flag an error.
C
              CALL LIBERR (ROUTN, 26, 10, 0, 0.0, PARS(ICP), IFAIL)
            END IF
          END IF
C
C Continue with the parse.
C
          GOTO 3000
        END IF
      ELSE
C
C There are no parameters on the command line, print the default ones.
C
        CALL OUTLIN (' ' // GLIST(3), 1)
        CALL SHOOBE (BUFFER, IUPTR, IUNIT)
        CALL OUTLIN (' ' // GLIST(4), 1)
        CALL SHOSAV (BUFFER, ISAVE, NUNITS, IUNIT)
C
C Let the calling program know that an implied default was requested and
C force an exit.
C
        ICMD = -2
        IFAIL = -1
      END IF
C
C Adjust the failure indicator.
C
      IFAIL = MAX (0, IFAIL)
      RETURN
C
C End of subroutine GETCMD.
C
      END
