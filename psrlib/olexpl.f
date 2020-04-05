*DECK OLEXPL
C
C
C
      SUBROUTINE OLEXPL (PARS, NPAR, PROG, EUNIT, FUNIT, PUNIT, TPAGE,
     &                   PPAGE, LINE, OUTBUF, IFAIL)
C
C This routine prints out the explain information for the current
C program.  It is installation dependent.
C
C Arguments:
C  PARS    Input   CHARACTER*(*)  The parameters from the command line.
C  NPAR    Input   INTEGER        The number of these.
C  PROG    Input   CHARACTER*(*)  The name of the calling program.
C  EUNIT   Input   INTEGER        A unit number for the explain file.
C  FUNIT   Input   INTEGER        A unit number to use for file output.
C  PUNIT   Input   INTEGER        A unit number to use for printer
C                                 output.
C  TPAGE   Input   INTEGER        The number of lines on a terminal.
C  LPAGE   Input   INTEGER        The number of lines in a printer page.
C  LINE    Input   INTEGER        The maximum line length in characters.
C  OUTBUF  Output  CHARACTER*(*)  A buffer to hold one line from the
C                                 explain file.
C  IFAIL   Output  INTEGER        The failure indicator: zero if
C                                 successful, else > 0.
C     Version 1.0   29th April, 1987   Alliant FX/FORTRAN
C! Modified for sun by RCD, 24/3/92
C
C Declare the routine's arguments.
C
      CHARACTER*(*) PARS(*), PROG, OUTBUF
      INTEGER       NPAR, EUNIT, FUNIT, PUNIT, TPAGE, PPAGE, LINE, IFAIL
C
C Declare external references.
C
      LOGICAL       BATCH
      INTEGER       LENGTH
      CHARACTER*1   PRMTCC
C
C Include the FORTRAN I/O status code definitions.
C
C! The following line not required on sun:
C!      INCLUDE       '/usr/include/fortran/error.h'
C
C Declare local variables.
C
      INTEGER       IP, IPAR, CURLIN
      CHARACTER     LSTFIL*100
      LOGICAL       PRNTOP, SILENT
C
C Define the name of the directory that contains the explain files.
C
      CHARACTER*(*) EXPDIR
C!      PARAMETER    (EXPDIR = '/usr1/olaf/explain/')
C! On sun...
      PARAMETER    (EXPDIR = '/usr/local/olaf/lib/explain/')
C
C Define the file type for explain files.
C
      CHARACTER*(*) EXPTYP
      PARAMETER    (EXPTYP = '.exp')
C
C Define the routine's name.
C
      CHARACTER*(*) ROUTN
      PARAMETER    (ROUTN = 'OLEXPL')
C
C Set up the initial values for the parser.
C
      LSTFIL = ' '
      PRNTOP = .FALSE.
      SILENT = .FALSE.
      IP = 2
C
C Parse the command line (if any).
C
      IF (NPAR.GT.1) THEN
        CALL LPARSE (PARS, NPAR, IP, LSTFIL, PRNTOP, SILENT, IPAR,
     &               IFAIL)
      ELSE
        IFAIL = 0
      END IF
C
C Test if successful.
C
      IF (IFAIL.EQ.0) THEN
C
C Yes, is an output file requested?
C
        IF (LSTFIL.NE.' ') THEN
C
C There is: open it.
C
          OPEN (UNIT = FUNIT, FILE = LSTFIL, STATUS = 'NEW',
     &          IOSTAT = IP)
C
C Test if successful.
C
          IF (IP.NE.0) THEN
C
C Unable to open the file: flag an error.
C
            CALL LIBERR (ROUTN, 30, 0, 0, 0.0, LSTFIL, IFAIL)
          ELSE
C
C The file was opened successfully: set up the listing unit.
C
            CALL SETLIN (FUNIT, 1, PPAGE, LINE, IP)
          END IF
        ELSE
C
C No output file is requested: simulate success.
C
          IP = 0
        END IF
C
C Test if successful.
C
        IF (IP.EQ.0) THEN
C
C Yes, is the print option selected?
C
          IF (PRNTOP) THEN
C
C It is: test if in batch mode.
C
            IF (BATCH ()) THEN
C           
C The program is in batch mode, force the listing to the main output
C channel.
C
              SILENT = .FALSE.
              PRNTOP = .FALSE.
            ELSE
C
C The program is in interactive mode: set up the print unit.
C                         
              OPEN (UNIT = PUNIT, STATUS = 'NEW', IOSTAT = IP)
C
C Test if successful.
C
              IF (IP.NE.0) THEN
C
C No, close any listing file and flag an error.
C
                IF (LSTFIL.NE.' ') THEN
                  CLOSE (UNIT = FUNIT, STATUS='DELETE', IOSTAT = IFAIL)
                  CALL OPSTAT (FUNIT, -1, IFAIL)
                END IF
                CALL LIBERR (ROUTN, 30, 1, 0, 0.0, '<Printer>', IFAIL)
              ELSE
C
C Yes, set up the output channel.
C
                CALL SETLIN (PUNIT, 1, PPAGE, LINE, IP)
              END IF
            END IF
          END IF
C
C Test if successful.
C
          IF (IP.EQ.0) THEN
C
C Test if terminal output is required.
C
            IF (SILENT) THEN
C
C No, turn off the terminal output.
C
              CALL OPSTAT (-1, 0, IP)
            END IF
C
C Now attempt to open the explain file for the specified program.
C
            OUTBUF = PROG
            CALL LOCASE (OUTBUF, 1, 1)
            OPEN (UNIT = EUNIT, FILE = EXPDIR//
     &                          OUTBUF(1:LENGTH(OUTBUF))//EXPTYP,
     &            STATUS = 'OLD', IOSTAT = IP)
C
C Test if successful.
C
            IF (IP.EQ.0) THEN
C
C Yes, read and print it line by line.
C
              CURLIN = 2
 1000         IF (IP.EQ.0) THEN
C
C Read the next line.
C
                READ (UNIT = EUNIT, FMT = '(A)', IOSTAT = IP) OUTBUF
C
C Test if successful.
C
                IF (IP.EQ.0) THEN
C
C Are we sending output to an interactive terminal?
C
                  IF (.NOT.(BATCH ().OR.SILENT)) THEN
C
C We are, check the current line number.
C
                    IF (CURLIN.GE.TPAGE) THEN
C
C We are near the end of the current page: ask and wait for the user to
C be ready for the next one.
C
                      WRITE (*, FMT = '(/'''//PRMTCC()//
     &                                'Press <Return> to continue'')')
                      READ (*, FMT = '()', IOSTAT = IP)
                      CURLIN = 2
                    ELSE
C
C We are not near the end of the page: just increment the line number.
C
                      CURLIN = CURLIN + 1
                    END IF
                  END IF
C
C Print the current line.
C
                  IF (IP.EQ.0) CALL OUTLIN (' '//OUTBUF, 1)
                ELSE IF (IP.NE.-1) THEN
C
C Explain file read error: flag it.
C
                  CALL LIBERR (ROUTN, 57, 2, 0, 0.0, PROG, IFAIL)
                END IF
C
C Do the next line.
C
                GOTO 1000
              END IF
C
C Close the explain file.
C
              CLOSE (UNIT = EUNIT, IOSTAT = IP)
C!            ELSE IF (IP.EQ.E_OPEN_CANNOTOPENFILE) THEN
C! On sun (see Sun FORTRAN User's Guide, p209)...
            ELSE IF (IP.EQ.118) THEN
C! Tried to open a nonexistent file with STATUS='OLD'
C
C The explain file does not exist: say so.
C
              OUTBUF = ' Sorry, there is no explain information for '
     &                 // PROG
              CALL OUTLIN (OUTBUF, -1)
            ELSE
C
C Explain file open error.
C                                   
              CALL LIBERR (ROUTN, 30, 3, 0, 0.0, PROG, IFAIL)
            END IF
          END IF
C
C Reset the output channels.
C
          CLOSE (UNIT = FUNIT, IOSTAT = IP)
          IF (SILENT) CALL OPSTAT (-1, 1, IP)
          IF (LSTFIL.NE.' ') CALL OPSTAT (FUNIT, -1, IP)
          IF (PRNTOP) THEN
            CALL PRINT (PUNIT)
            CALL OPSTAT (PUNIT, -1, IP)
          END IF
        END IF
      END IF
C 
C End of subroutine OLEXPL.
C
      END
