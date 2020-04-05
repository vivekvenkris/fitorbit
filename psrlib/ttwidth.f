********************************************************************
      INTEGER FUNCTION TTWIDTH
********************************************************************
*
*     Returns the current width of a terminal.
*
      INCLUDE '($DVIDEF)'
      INCLUDE '($SSDEF)'
      INTEGER*4 WIDTH,STATUS,RETLEN,SYS$GETDVIW,LIB$SIGNAL
      STRUCTURE /ITMLST/
        UNION
          MAP
            INTEGER*2 BUFLEN,ITMCOD
            INTEGER*4 BUFADR
            INTEGER*4 RETADR
          END MAP
          MAP
            INTEGER*4 ENDLST
          END MAP
        END UNION
      END STRUCTURE
      RECORD /ITMLST/ GETDVI(2)
      GETDVI(1).ITMCOD = DVI$_DEVBUFSIZ
      GETDVI(1).BUFADR = %LOC(WIDTH)
      GETDVI(1).BUFLEN = 4
      GETDVI(1).RETADR = %LOC(RETLEN)
      GETDVI(2).ENDLST = 0
C
       STATUS = SYS$GETDVIW(,,'TT',GETDVI,,,,)
       IF (STATUS .NE. SS$_NORMAL) CALL LIB$SIGNAL(%VAL(STATUS))
       TTWIDTH = WIDTH
       END

