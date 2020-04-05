*DECK SYMAVL
C
C
C
      SUBROUTINE SYMAVL(PARS,NPAR,PTRFHD,PTRFTL,PTRTBL,SYMTBL,BITSYM,
     &                  FIRST,LAST,NUMBER)
C
C This routine adds a symbol name or value to the symbols table.  There
C must be enough free table entries to store the value.
C Input arguments:
C  PARS     - The symbol name or the symbol value parameters.
C  NPAR     - The number of parameters (1 for a symbol name).
C  PTRFHD   - The symbol table free entries list head.
C  PTRFTL   - The symbol table free entries list tail.
C  PTRTBL   - The pointers table (see symcom).
C  SYMTBL   - The symbols table (see symcom).
C  BITSYM   - The length of a single entry in symtbl.
C Output arguments:
C  PTRFHD   - The new symbol table free entries list head.
C  PTRFTL   - The new symbol table free entries list tail.
C  FIRST    - The first symbol table entry used for the name or value.
C  LAST     - The last symbol table entry used for the name value.
C  NUMBER   - The number of symbol table entries used for the name or
C             value.
C     Version 1.0   14May85
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER*(*) PARS(*),SYMTBL(*)
      DIMENSION PTRTBL(2,*)
C
C Test if the value to be added is blank.
C
      IF(NPAR.LE.1.AND.PARS(1).EQ.' ') THEN
C
C The value to be added is blank, do not use up a symbol table entry.
C
        FIRST=0
        LAST=0
        NUMBER=0
      ELSE
C
C The value to be added is not blank, start to load it into the symbols
C table.
C
        NEXTE=PTRFHD
        FIRST=NEXTE
        NUMBER=0
        DO 1 I=1,NPAR
C
C Load the current parameter.  First obtain its length.
C
          L=MAX(1,LENGTH(PARS(I)))
          IC=1
 1000     IF(IC.LE.L) THEN
C
C Load the next bit of the parameter into the symbols table.
C
            SYMTBL(NEXTE)=PARS(I)(IC:MIN(IC+BITSYM-1,L))
C
C Increment the entries used counter.
C
            NUMBER=NUMBER+1
C
C Get the location of the next symbol table entry.
C
            LAST=NEXTE
            NEXTE=PTRTBL(2,NEXTE)
C
C Do the next bit of the parameter.
C
            IC=IC+BITSYM
            GOTO 1000
          ENDIF
C
C The current parameter has been loaded, negate the value of the next
C entry pointer to indicate the start of the next parameter.
C
          PTRTBL(2,LAST)=-PTRTBL(2,LAST)
    1   CONTINUE
C
C All parameters have been loaded, mark the end of the string stored.
C
        PTRTBL(2,LAST)=0
C
C Update the free entries list.
C
        IF(NEXTE.LE.0) THEN
C
C The list is now empty.
C
          PTRFHD=0
          PTRFTL=0
        ELSE
C
C There is something left in the free list.
C
          PTRTBL(1,NEXTE)=0
          PTRFHD=NEXTE
        ENDIF
      ENDIF
      RETURN
C
C End of subroutine symavl.
C
      END
