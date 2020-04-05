*DECK SYMPRT
C
C
C
      SUBROUTINE SYMPRT(SYMTBL,PTRTBL,NAMTBL,NAMPTR,BITSYM,OUTBUF)
C
C This routine prints a symbol value.
C Input arguments:
C  SYMTBL   - The symbols table.
C  PTRTBL   - The pointers table.
C  NAMTBL   - The symbol names table.
C  NAMPTR   - The entry in this containing the symbol required.
C  BITSYM   - The size of a single entry in symtbl.
C  OUTBUF   - A character work space array already loaded with the
C             symbol name and separator.  It and the output line length
C             must be long enough to hold the longest parameter in the
C             symbol value plus 10 characters.
C     Version 1.0   13May85
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER*(*) SYMTBL(*),OUTBUF
      DIMENSION PTRTBL(2,*),NAMTBL(12,*)
C
C Obtain the length of the output line.
C
      CALL GETLIN(-1,ISTAT,LENPAG,LENLIN,NUMPAG,NUMLIN,IFAIL)
C
C Set the maximum length of an output line.
C
      L=MIN(LEN(OUTBUF),LENLIN)+1
C
C Load the symbol value into the output buffer bitsym characters at a
C time.
C
      IC=LENGTH(OUTBUF)+1
      NEXTE1=-NAMTBL(5,NAMPTR)
 1000 IF(NEXTE1.NE.0) THEN
C
C Is this the start of a new parameter?
C
        IF(NEXTE1.LT.0) THEN
C
C Yes, remember where this parameter starts.
C
          IS=IC
C
C Leave a space.
C
          IC=IC+1
          NEXTE1=ABS(NEXTE1)
        ENDIF
C
C Truncate the current bit if it is the end of a parameter.
C
        IF(PTRTBL(2,NEXTE1).LE.0) THEN
          LP=LENGTH(SYMTBL(NEXTE1))
        ELSE
          LP=BITSYM
        ENDIF
C
C Will the current bit fit into the output buffer?
C
        IF(IC+LP.GT.L) THEN
C
C It will not, print the line up to the start of the current parameter.
C
          CALL OUTLIN(OUTBUF(1:IS-1),1)
C
C Shift the output buffer down.
C
          DO 1 I=IS,IC-1
            OUTBUF(I-IS+9:I-IS+9)=OUTBUF(I:I)
    1     CONTINUE
C
C Adjust the character pointer.
C
          IC=IC-IS+9
        ENDIF
C
C Load the current bit of the symbol value.
C
        OUTBUF(IC:)=SYMTBL(NEXTE1)(1:LP)
C
C Adjust the character pointer.
C
        IC=LENGTH(OUTBUF)+1
C
C Load the next bit of the symbol value.
C
        NEXTE1=PTRTBL(2,NEXTE1)
        GOTO 1000
      ENDIF
C
C Print the final line.
C
      CALL OUTLIN(OUTBUF,1)
      RETURN
C
C End of subroutine symprt.
C
      END
