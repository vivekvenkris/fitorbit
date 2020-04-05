*DECK SYMINI
C
C
C
      SUBROUTINE SYMINI(PTRTBL,NAMTBL,HSHTBL,PTRFHD,PTRFTL,NAMFHD,
     &                  NAMFTL,NAMAHD,NAMATL,NFENTP,MAXSYM,MAXNAM,
     &                  MAXHSH)
C
C This routine initializes the symbol storage data structures.
C Input arguments:
C  MAXSYM   - The size of the pointers table.
C  MAXNAM   - The size of the symbol names table.
C  MAXHSH   - The size of the symbol names hash table.
C Output arguments:
C  PTRTBL   - The pointers table (linked into the free list).
C  NAMTBL   - The symbol names table (linked into the free list).
C  HSHTBL   - The symbol names hah table (cleared).
C  PTRFHD   - The pointers table free list head.
C  PTRFTL   - The pointers table free list tail.
C  NAMFHD   - The symbol names table free list head.
C  NAMFTL   - The symbol names table free list tail.
C  NAMAHD   - The 'alpha' order name block list head (cleared).
C  NAMATL   - The 'alpha' order name block list tail (cleared).
C  NFENTP   - The number of free pointer table entries.
C     Version 1.0   13May85
C
      IMPLICIT INTEGER (A-Z)
      DIMENSION PTRTBL(2,*),NAMTBL(12,*),HSHTBL(2,*)
C
C Link the pointers tables entries together.
C
      DO 1 I=1,MAXSYM
        PTRTBL(1,I)=I-1
        PTRTBL(2,I)=I+1
    1 CONTINUE
C
C Clear the last of these.
C
      PTRTBL(2,MAXSYM)=0
C
C Set the free list head and tail.
C
      PTRFHD=1
      PTRFTL=MAXSYM
C
C Set the number of free entries.
C
      NFENTP=MAXSYM
C
C Link the symbol names table blocks together.
C
      DO 2 I=1,MAXNAM
        NAMTBL(1,I)=I-1
        NAMTBL(2,I)=I+1
    2 CONTINUE
C
C Clear the last of these.
C
      NAMTBL(2,MAXNAM)=0
C
C Set the free list head and tail.
C
      NAMFHD=1
      NAMFTL=MAXNAM
C
C Clear the 'alpha' order list pointers.
C
      NAMAHD=0
      NAMATL=0
C
C Clear the symbol names hash table.
C
      DO 3 I=1,MAXHSH
        HSHTBL(1,I)=0
        HSHTBL(2,I)=0
    3 CONTINUE
      RETURN
C
C End of subroutine symini.
C
      END
