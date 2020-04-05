*DECK SYMSUB
C
C
C
      SUBROUTINE SYMSUB (PARS, NPAR, IP, MAXPAR, LITFLG, BUFFER, IFAIL)
C
C This routine completes the symbol substitution on a GETCMD command
C line.  Symbols which start with the character returned by the
C routine SUPCHR are not substituted, but the first character is
C stripped off.
C
C Arguments:
C  PARS    Input/output  CHARACTER*(*)  The parameters from the command
C                                       line.
C  NPAR    Input/output  INTEGER        The number of parameters in
C                                       PARS.
C  IP      Input/output  INTEGER        The next parameter to process.
C  MAXPAR  Input         INTEGER        The maximum number of parameters
C                                       supported.
C  LITFLG  Input/output  LOGICAL        .TRUE. if case conversion is
C                                       currently suppressed.
C  BUFFER  Output        CHARACTER*(*)  A work space buffer capable of
C                                       holding one parameter.
C  IFAIL   Output        INTEGER        Zero if successful, else > 0.
C
C     Version 2.0   7th July, 1988
C
C Declare the routine's arguments.
C
      CHARACTER*(*) PARS(*), BUFFER
      INTEGER       NPAR, IP, MAXPAR, IFAIL
      LOGICAL       LITFLG
C
C Declare external references.
C
      CHARACTER*1   LITDEL, NUPDEL
C
C Define the routine's name.
C
      CHARACTER*(*) ROUTN
      PARAMETER    (ROUTN = 'SYMSUB')
C
C Start to do the substitutions.
C
      IFAIL = 0
 1000 IF (IP .LE. NPAR .AND. IFAIL .EQ. 0) THEN
C
C Recursively translate the current parameter.
C
        CALL SYMTRN (PARS, NPAR, IP, MAXPAR, LITFLG, BUFFER, IFAIL)
C
C Do the next parameter.
C
        GOTO 1000
      END IF
C
C Were we successful?
C
      IF (IFAIL .EQ. 0) THEN
C
C Yes, but check that there is not an unpaired literal delimiter.
C
        IF (LITFLG) THEN
C
C There is: flag an error.
C
          CALL LIBERR (ROUTN, 65, 0, 0, 0.0, NUPDEL() // ' or ' //
     &                 LITDEL(), IFAIL)
        END IF
      END IF
      RETURN
C
C End of subroutine symsub.
C
      END
