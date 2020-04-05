*DECK SYMTRN
C
C
C
      SUBROUTINE SYMTRN (PARS, NPAR, IP, MAXPAR, LITFLG, BUFFER, IFAIL)
C
C This routine recursively translates a symbol in PARS(IP) until either
C the result is not a symbol, or a symbol name previously referenced is
C encountered, or until a maximum depth of recursion is reached.  On
C successful exit, PARS contains the results of the translations with
C all symbols fully expanded.  Suppress substitution characters are
C removed and values are converted into upper case if LITFLG is .FALSE..
C
C Arguments:
C  PARS    Input/output  CHARACTER*(*)  The parameters from the command
C                                       line.
C  NPAR    Input/output  INTEGER        The number of parameters in
C                                       PARS.
C  IP      Input/output  INTEGER        The current location in PARS.
C                                       On exit, IP points to the next
C                                       parameter to process.
C  MAXPAR  Input         INTEGER        The maximum number of parameters
C                                       supported.
C  LITFLG  Input/output  LOGICAL        .TRUE. if case conversion is
C                                       currently suppressed.
C  BUFFER  Output        CHARACTER*(*)  A work space buffer capable of
C                                       holding one parameter.
C  IFAIL   Output        INTEGER        Zero if successful, else > 0.
C
C      Version 1.1   26th July, 1988
C
C Declare the routine's arguments.
C
      CHARACTER*(*) PARS(*), BUFFER
      INTEGER       NPAR, IP, MAXPAR, IFAIL
      LOGICAL       LITFLG
C
C Define the maximum recursion depth allowed.
C
      INTEGER       MAXDEP
      PARAMETER    (MAXDEP = 32)
C
C Define the effective maximum length of a symbol name (used to store
C symbol names for recursive definitions)
C
      INTEGER       MAXLEN
      PARAMETER    (MAXLEN = 64)
C
C Declare external references.
C
      CHARACTER*1   SUPCHR, LITDEL, NUPDEL
      INTEGER       LENGTH
C
C Define the routine's name.
C
      CHARACTER     ROUTN*(*)
      PARAMETER    (ROUTN = 'SYMTRN')
C
C Declare local variables:
C  PRVSYM - Holds the names of symbols previously encountered during
C           the current translations.  This is used to check for
C           recursive definitions.
C  PRVIP  - Holds the previous values of the current parameter.
C  SCHAR  - The character used to suppress symbol substitution.
C  LCHAR  - The character used to indicate the start of a literal.
C  LCHAR1 - The character used to indicate the start of no case
C           conversion.
C  DEPTH  - The current translation depth.
C  IPL    - Used to hold the value of IP locally.
C  IPEND  - The number of the last parameter to process.
C  SEXI   - .TRUE. if the parameter is a symbol.
C
      CHARACTER     PRVSYM(MAXDEP)*(MAXLEN), SCHAR*1, LCHAR*1, LCHAR1*1
      INTEGER       PRVIP(0:MAXDEP), DEPTH, IPL, IPEND, I
      LOGICAL       SEXI
C
C Obtain the suppress substitution character.
C
      SCHAR = SUPCHR ()
C
C Obtain the literal delimiter characters.
C
      LCHAR = LITDEL ()
      LCHAR1 = NUPDEL ()
C
C Remember the current parameter number.
C
      PRVIP(0) = IP
C
C Set the initial value of the last parameter to process, this is
C updated as symbols are translated.
C
      IPEND = IP
C
C Initialise the translation depth.
C
      DEPTH = 0
C
C Recursively translate the symbol.
C
      IFAIL = 0
 1000 IF (IP .LE. IPEND .AND. IFAIL .LE. 0) THEN
C
C Translate the current parameter.  This loop normally exits when IFAIL
C goes to -1, indicating that the parameter is not a symbol.
C
        IFAIL = MAX (0, IFAIL)
 2000   IF (DEPTH .LT. MAXDEP .AND. IFAIL .EQ. 0) THEN
C
C Convert the current value of the parameter to upper case if this is
C not suppressed.
C
          IF (.NOT. LITFLG) CALL UPCASE (PARS, IP, IP)
C
C Does the current parameter start with the suppress substitution
C character?
C
          IF (PARS(IP)(1:1) .NE. SCHAR
     &        .OR. LENGTH (PARS(IP)) .LE. 1) THEN
C
C It does not: check that it is not the same as a previous symbol.
C
            DO 1 I = 1, DEPTH
              IF (PARS(IP) .EQ. PRVSYM(I)) THEN
C
C The current parameter is the same as a previous symbol - this means
C that we have an attempt at a recursive definition.  Flag an error and
C exit.
C
                CALL LIBERR (ROUTN, 84, 0, 0, 0.0, PARS(IP), IFAIL)
                GOTO 2000
              END IF
    1       CONTINUE
C
C The current parameter is not the same as any previous symbol
C encountered: attempt to substitute it.
C
            BUFFER = PARS(IP)
            IPL = IP
            CALL SYMGET (BUFFER, PARS, IPL, NPAR, MAXPAR, IFAIL)
C
C At this point, IFAIL is -1 if the parameter was not a symbol, and is
C zero if it was and was substituted successfully.  In this latter case,
C PARS(IP) will now contain the first parameter of the symbol's value.
C Test if the parameter was a symbol.
C
            IF (IFAIL .EQ. 0) THEN
C
C It was: adjust the previous depth ends in PRVIP by the number of
C parameters added.
C
              DO 2 I = 1, DEPTH
                PRVIP(I) = PRVIP(I) + IPL - IP
    2         CONTINUE
C
C It was: load it into the previous symbols list.
C
              DEPTH = DEPTH + 1
              PRVSYM(DEPTH) = BUFFER
C
C Remember where the current depth ends.
C
              PRVIP(DEPTH) = IPL
C
C Adjust the finishing parameter number.
C
              IPEND = MAX (IPEND, IPL)
              PRVIP(0) = IPEND
            END IF
          ELSE
C
C The parameter starts with the suppress substitution character: check
C if it is actually a symbol.
C
            CALL SYMEXI (PARS(IP)(2:), SEXI)
            IF (SEXI) THEN
C
C It is: remove the suppress substitution character.
C
              DO 3 I = 1, LENGTH (PARS(IP)) - 1
                PARS(IP)(I:I) = PARS(IP)(I+1:I+1)
    3         CONTINUE
              PARS(IP)(I:I) = ' '
            END IF
C
C Set the failure indicator to -1 to indicate that the parameter is not
C a symbol.
C
            IFAIL = -1
          END IF
C
C Continue translating symbols.
C
          GOTO 2000
        END IF
C
C Test if successful.
C
        IF (IFAIL .EQ. -1) THEN
C
C At this point the current parameter has been fully translated: test
C what it is.
C
          IF (PARS(IP) .EQ. LCHAR) THEN
C
C It is a literal delimiter: toggle the literal flag.
C
            LITFLG = .NOT. LITFLG
          ELSE IF (PARS(IP) .EQ. LCHAR1) THEN
C
C It is a "no case conversion" delimiter: toggle the literal flag and
C remove the parameter.
C
            LITFLG = .NOT. LITFLG
            NPAR = NPAR - 1
            DO 4 I = IP, NPAR
              PARS(I) = PARS(I+1)
    4       CONTINUE
C
C Adjust the parameter pointers to allow for the removal.
C
            IPEND = MAX (MIN (IP, NPAR), IPEND - 1)
            DO 5 I = 1, DEPTH
              PRVIP(I) = PRVIP(I) - 1
    5       CONTINUE
            IP = IP - 1
          END IF
        END IF
C
C Now set things up ready for translating the next parameter.  First
C test if we have reached the end of the current depth.
C
        IF (IP .GE. PRVIP(DEPTH)) THEN
C
C We have, go down one translation depth.
C
          DEPTH = DEPTH - 1
        END IF
C
C Do the next parameter.
C
        IP = IP + 1
        GOTO 1000
      END IF
C
C Test if successful.
C
      IF (IFAIL .EQ. 0) THEN
C
C Possibly, but check for the maximum recursion depth.
C
        IF (DEPTH .GE. MAXDEP) THEN
C
C It was reached: flag an error.
C
          CALL LIBERR (ROUTN, 85, 1, MAXDEP, 0.0, ' ', IFAIL)
        END IF
      END IF
C
C Adjust the failure indicator to ensure that it is always zero or
C greater.
C
      IFAIL = MAX (0, IFAIL)
C
C End of subroutine SYMTRN.
C
      END
