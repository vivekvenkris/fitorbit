*DECK CASPRS
C
C
C
      SUBROUTINE CASPRS (PARS, NPAR, IP, LITFLG)
C
C This routine parses a number of parameters for case conversions.  It
C it called by GETCMD before processing a SYMBOLS command.
C
C Arguments:
C  PARS    Input/output  CHARACTER*(*)  The parameters from the command
C                                       line.
C  NPAR    Input/output  INTEGER        The number of these.
C  IP      Input/output  INTEGER        The current parameter.
C  LITFLG  Input/output  LOGICAL        .TRUE. if case conversion is
C                                       currently suppressed.
C
C     Version 1.1   30th June, 1993
C
C Declare the routine's arguments.
C
      CHARACTER*(*) PARS(*)
      INTEGER       NPAR, IP
      LOGICAL       LITFLG
C
C Declare external references.
C
      CHARACTER*1   LITDEL, NUPDEL
C
C Define the routine's name.
C
      CHARACTER*(*) ROUTN
      PARAMETER    (ROUTN = 'CASPRS')
C
C Declare local variables.
C
      CHARACTER*1   LCHAR, LCHAR1
      INTEGER       I
      LOGICAL       INLIT
C
C Obtain the literal delimiter characters.
C
      LCHAR = LITDEL ()
      LCHAR1 = NUPDEL ()

C
C Note that we are not currently in a literal.
C
      INLIT = .FALSE.
C
C Check each parameter in turn.
C
 1000 IF (IP .LE. NPAR) THEN
C
C Convert the current parameter to upper case if this is not disabled.
C
        IF (.NOT. LITFLG) CALL UPCASE (PARS, IP, IP)
C
C Check what it is.
C
        IF (PARS(IP) .EQ. LCHAR) THEN
C
C It is a literal delimiter: toggle the literal flag.
C
          LITFLG = .NOT. LITFLG
          INLIT = .NOT. INLIT
        ELSE IF (PARS(IP) .EQ. LCHAR1) THEN
C
C It is a "no case conversion" delimiter.  This is more complicated:
C if we are already in a literal (INLIT = .TRUE.) then we should keep
C in in the parameters because the user obviously (?) wants it in the
C symbol's value; we should not change the case conversion either.
C Otherwise treat it as a normal case-conversion character, swap the
C case handling and remove it from the parameters.
C
           IF (.NOT. INLIT) THEN
C
C We are not in a literal.
C
            LITFLG = .NOT. LITFLG
            NPAR = NPAR - 1
            DO 1 I = IP, NPAR
             PARS(I) = PARS(I+1)
    1       CONTINUE
            IP = IP - 1
          END IF
        END IF
C
C Do the next parameter.
C
        IP = IP + 1
        GOTO 1000
      END IF
C
C End of subroutine CASPRS.
C
      END








