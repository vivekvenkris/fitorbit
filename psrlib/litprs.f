*DECK LITPRS
C
C
C
      SUBROUTINE LITPRS(PARS,NPAR,IP,IS,IF,IFAIL)
C
C This routine parses a literal construct in a command line; e.g.
C " <parameter> <parameter> ... ".  Double delimiters are treated
C as one ordinary parameter rather than a terminating delimiter, and
C the parameters array shifted down to get rid of the second one.
C Input arguments:
C  PARS     - The parameters from the command line.  Pars(ip) should
C             contain the starting delimiter of the string.
C  NPAR     - The number of these.
C  IP       - The position of the first delimiter in pars.
C Output arguments:
C  PARS     - May be shifted down if double delimiters occur (see
C             above).
C  NPAR     - The new number of parameters in the command line (this
C             will be less than the original value if pars has been
C             shifted down.
C  IP       - Points to the last delimiter in pars.
C  IS       - The position of the first parameter in the literal.
C  IF       - The position of the last parameter in the literal.  If
C             if < is then the literal contains no parameters.
C  IFAIL    - Zero if successful, else > 0.
C     Version 1.1   8th July, 1988
C
      CHARACTER PARS(*)*(*)
C
C Define the routine's name.
C
      CHARACTER*(*) ROUTN
      PARAMETER    (ROUTN = 'LITPRS')
C
C Note the nominal position of the start of the literal.
C
      IPT=IP+1
      IS=IPT
C
C Search for the closing delimiter.
C
 1000 IF(IPT.LE.NPAR) THEN
        IF(PARS(IPT).EQ.PARS(IP)) THEN
C
C We have found another delimiter, note the possible location of the
C end of the literal.
C
          IF=IPT-1
C
C Is the next parameter a delimiter as well?
C
          IF(IPT.LT.NPAR) THEN
            IF(PARS(IPT+1).EQ.PARS(IP)) THEN
C
C Yes, shift the whole parameters array down to remove the second
C delimiter.
C
              NPAR=NPAR-1
              DO 1 I=IPT+1,NPAR
                PARS(I)=PARS(I+1)
    1         CONTINUE
            ELSE
C
C We have found the closing delimiter.
C
              IP=IPT
              IFAIL=0
              GOTO 7777
            ENDIF
          ELSE
C
C We have found the closing delimiter.
C
            IP=IPT
            IFAIL=0
            GOTO 7777
          ENDIF
        ENDIF
C
C Continue searching.
C
        IPT=IPT+1
        GOTO 1000
      ENDIF
C
C If we get here we have failed to locate the closing delimiter, flag an
C error
C
      CALL LIBERR(ROUTN,65,0,0,0.0,PARS(IP),IFAIL)
      PARS(1)=PARS(IP)
 7777 RETURN
C
C End of subroutine litprs.
C
      END
