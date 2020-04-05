*DECK SYMFSY
C
C
C
      SUBROUTINE SYMFSY(SYMBOL,SYMTBL,PTRTBL,NAMTBL,HSHTBL,BITSYM,
     &                  MAXHSH,NAMPTR,HSHPTR)
C
C This routine searches for a symbol.  If it is found its position is
C returned, otherwise the position of the symbol preceeding where the
C requested symbol value should be inserted is returned.
C Input arguments:
C  SYMBOL   - The symbol name.
C  SYMTBL   - The symbol table.
C  PTRTBL   - The pointers table.
C  NAMTBL   - The symbol names table.
C  HSHTBL   - The symbol names hash table.
C  BITSYM   - The length of a single entry in symtbl.
C  MAXHSH   - The size of the hash table expressed as a power of two.
C Output arguments:
C  NAMPTR   - The position of the symbol:
C              >0 = The symbol was found, namptr is its position in
C                   namtbl
C              =0 = The symbol was not found, it should be placed in the
C                   first position in the hash list
C              <0 = The symbol was not found, it should be placed after
C                   the symbol referenced by abs(namptr) in namtbl
C  HSHPTR   - The position of the symbol in the hash table if it was
C             found, or the position that it should be if it was not
C             found.
C     For the contents of symtbl, ptrtbl, namtbl, and hshtbl see the
C routine symcom.
C     Version 1.1   23Oct85
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER*(*) SYMBOL,SYMTBL(*)
      DIMENSION PTRTBL(2,*),NAMTBL(12,*),HSHTBL(2,*)
C
C Obtain the length of the symbol name; note that trailing spaces are
C ignored.
C
      L=LENGTH(SYMBOL)
C
C Hash the symbol name and obtain its hash table offset.
C
      CALL HASH(SYMBOL(1:L),MAXHSH,HSHPTR)
C
C Increment the hash table pointer so that the first entry is numbered
C one.
C
      HSHPTR=HSHPTR+1
C
C Set the initial value of the name table pointer to the contents of the
C hash table at the specified offset.
C
      NEXT=HSHTBL(1,HSHPTR)
C
C Clear the returned name table pointer.
C
      NAMPTR=0
C
C Scan through the name table blocks in the list starting at the hash
C table entry.
C
 1000 IF(NEXT.GT.0.AND.NAMPTR.EQ.0) THEN
C
C Check the length of the symbol in the current name table block.
C
        IF(NAMTBL(9,NEXT).LT.L) THEN
C
C The length of the name in the current name block is shorter than the
C requested one, keep searching.
C
          NEXT=NAMTBL(2,NEXT)
        ELSEIF(NAMTBL(9,NEXT).EQ.L) THEN
C
C The current name has the same length as the requested one, check the
C name itself.
C
          IC=1
          NEXTE=NAMTBL(3,NEXT)
 1001     IF(NEXTE.GT.0.AND.IC.LE.L) THEN
            ICC=MIN(IC+BITSYM-1,L)
            IF(SYMTBL(NEXTE).LT.SYMBOL(IC:ICC)) THEN
C
C We are before the place where the current name would be, continue
C with the next name table block.
C
              NEXT=NAMTBL(2,NEXT)
              GOTO 1000
            ELSEIF(SYMTBL(NEXTE).GT.SYMBOL(IC:ICC)) THEN
C
C We have gone past the point where the symbol could be, set namptr to
C indicate where to put the requested symbol and force an exit.
C
              NAMPTR=-NAMTBL(1,NEXT)
              NEXT=-1
              GOTO 1000
            ENDIF
C
C Continue comparing the symbol names.
C
            NEXTE=PTRTBL(2,NEXTE)
            IC=IC+BITSYM
            GOTO 1001
          ENDIF
C
C The symbols are the same, set namptr to the current name block and
C focre an exit.
C
          NAMPTR=NEXT
          NEXT=-1
        ELSE
C
C The length of the name in the current name block is longer than the
C the requested one: we have gone too far.  Set namptr to indicate where
C the requested symbol should go and force an exit.
C
          NAMPTR=-NAMTBL(1,NEXT)
          NEXT=-1
        ENDIF
C
C Check the next name block.
C
        GOTO 1000
      ELSEIF(NEXT.EQ.0) THEN
C
C We have reached the end of the list.
C
        NAMPTR=-HSHTBL(2,HSHPTR)
      ENDIF
      RETURN
C
C End of subroutine symfsy.
C
      END
