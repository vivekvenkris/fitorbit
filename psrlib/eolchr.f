*DECK EOLCHR
C
C
C
      LOGICAL FUNCTION EOLCHR (CHR)
C
C This routine returns .TRUE. if its argument is a character used to
C terminate a command.
C
C Argument:
C  CHR     Input  CHARACTER*1  The character to test.
C
C     Version 1.0   11th July, 1988
C
C Declare the routine's arguments.
C
      CHARACTER*1 CHR
C
C Define the character used as an end of command indicator.
C
      CHARACTER*1 EOCMDS
      PARAMETER  (EOCMDS = ';')
C
C Set EOLCHR.
C
      EOLCHR = CHR .EQ. EOCMDS
C
C End of logical function EOLCHR.
C
      END
