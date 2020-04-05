cDECK OUTMON
c
c **************************************************************
c **************************************************************
c
c WRITES OUT A LINE OF TEXT CONTAINED IN THE CHARACTER ARRAY LINE
c     TO THE MONITOR LOGICAL UNIT LUMON
c     , PROVIDED THAT IT IS ENABLED ( LU > 0 ).
c A LINE WHICH IS LONGER THAN THE RECORD LENGTH OF THE UNIT IS
c     CONTINUED BY HYPHENATING. IF THERE IS INSUFFICIENT SPACE ON
c     THE CURRENT PAGE, THEN A NEW PAGE IS GENERATED.
c LOGICAL UNIT LUMON SHOULD PREVIOUSLY HAVE BEEN OPENED FOR OUTPUT 
c     USING THE LIBRARY ROUTINE OPNFIL.
c
      subroutine outmon(line)
c
      include 'PSRLIB.DEF'
c DEFINITIONS OF THE PULSAR LIBRARY GLOBALS
c
c     PROGRAM LOGICAL UNITS
c
      character line*(*)
c
      call outptn(lumon, line, 1)
c
c END OF SUBROUTINE OUTMON
c
      return 
      end
