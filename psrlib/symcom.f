*DECK SYMCOM
C
C
C
      SUBROUTINE SYMCOM
C
C This routine contains the common code for the GETCMD symbol handling.
C The following entry points are provided:
C  SYMADD - Add a new symbol
C  SYMDEL - Delete a symbol
C  SYMCLR - Clear all symbols
C  SYMGET - Get the value of a symbol
C  SYMEXI - Returns whether a symbol exists
C  SYMSHO - Show the value of a symbol
C  SYMLST - List the values of all symbols
C  SYMSAV - Save all symbols to a file
C  SYMLOD - Load all symbols from a file
C     The data structures used to describe the symbols are as follows:
C  SYMTBL - A character array which acts as a pool for the storage of
C           both symbol names and symbol values.  A single name or value
C           can take up more than one element of this array.
C  PTRTBL - An integer array of pointers used to link elements of SYMTBL
C           together:
C            PTRTBL(1, ) = Pointer to previous element
C            PTRTBL(2, ) = Pointer to next element (negative if the next
C                          element is the start of a new parameter)
C           There is a one to one correspondence between PTRTBL and
C           SYMTBL.
C  NAMTBL - An integer array of name blocks, one for each symbol:
C            NAMTBL(1,  ) = Pointer to previous block
C            NAMTBL(2,  ) = Pointer to next block
C            NAMTBL(3,  ) = Pointer to start of symbol name in PTRTBL
C            NAMTBL(4,  ) = Pointer to end of symbol name in PTRTBL
C            NAMTBL(5,  ) = Pointer to start of symbol value in PTRTBL
C            NAMTBL(6,  ) = Pointer to end of symbol value in PTRTBL
C            NAMTBL(7,  ) = Pointer to previous block ('alpha' order)
C            NAMTBL(8,  ) = Pointer to next block ('alpha' order)
C            NAMTBL(9,  ) = Number of characters in symbol name
C            NAMTBL(10, ) = Number of ptrtbl entries used for the symbol
C                           value
C            NAMTBL(11, ) = The maximum parameter value length in the
C                           symbol value (characters)
C            NAMTBL(12, ) = The number of parameters in the symbol value
C  HSHTBL - An integer array containing the name table list head and
C           tail pointers for each symbol name hash value:
C            HSHTBL(1, ) = Pointer to first block in NAMTBL
C            HSHTBL(2, ) = Pointer to last block in NAMTBL
C     All lists start with element number 1.  Pointer values of zero
C indicate the end of the list.
C     The following list heads and tails are maintained:
C  PTRFHD = Pointer to first free entry in PTRTBL (zero if none)
C  PTRFTL = Pointer to last free entry in PTRTBL
C  NAMFHD = Pointer to first free block in NAMTBL (zero if none)
C  NAMFTL = Pointer to last free block in NAMTBL
C  NAMAHD = Pointer to first 'alpha' order allocated block in NAMTBL
C  NAMATL = Pointer to last 'alpha' order allocated block in NAMTBL
C  NFENTP = Number of free entries left in PTRTBL
C     For each hash value name blocks are linked in increasing symbol
C name length order, and for each name length in 'alpha' order.
C     Symbol values are stored as separate parameters in SYMTBL, each
C parameter always starting with a new element.  The start of the
C second and subsequent parameters is indicated by a negated pointer
C value in PTRTBL.  The end of a name or value is indicated by a zero
C pointer value.  Names and parameter values are padded with blanks, and
C so trailing blanks are always non-significant.
C     The following parameters define the sizes of the tables and
C constant values:
C  MAXSYM - The maximum size of SYMTBL (in elements).
C  BITSYM - The size of one element in SYMTBL (in characters).
C  MAXNAM - The maximum number of symbol names supported.
C  MAXHSH - The size of the hash table (HSHTBL) expressed as a power of
C           two.
C  IDBPAT - A bit pattern used to identify symbols files.
C  SYFVER - The symbol file version number.
C     Version 1.1   13Jun85
C
C Declare everything as integer.
C
      IMPLICIT INTEGER (A-Z)
C
C Define the parameter values.
C
      PARAMETER (MAXSYM=2000,
     &           BITSYM=8,
     &           MAXNAM=500,
     &           MAXHSH=6,
     &           IDBPAT=31316,
     &           SYFVER=1)
C
C Declare the table arrays.
C
      CHARACTER SYMTBL(MAXSYM)*(BITSYM)
      DIMENSION PTRTBL(2,MAXSYM),
     &          NAMTBL(12,MAXNAM),
     &          HSHTBL(2,2**MAXHSH)
C
C The next value is .TRUE. if the tables have yet to be initialized.
C
      LOGICAL INITAB
C
C Define the routine name.
C
      CHARACTER ROUTN*(*)
      PARAMETER (ROUTN='SYMCOM')
C
C Declare the routine arguments.
C
      CHARACTER*(*) SYMBOL,PARS(*),SFILE,OUTBUF
      LOGICAL SEXI
C
C Make sure that the tables and pointers are not forgotten.
C
      SAVE SYMTBL,PTRTBL,NAMTBL,HSHTBL,PTRFHD,PTRFTL,NAMFHD,NAMFTL,
     &     NAMAHD,NAMATL,NFENTP,INITAB
C
C Initialize initab.
C
      DATA INITAB/.TRUE./
C
C Exit in case this routine has been called (it does nothing).
C
      RETURN
C
C
C
C
      ENTRY SYMADD(SYMBOL,PARS,NPAR,IFAIL)
C
C This routine adds a new symbol to the tables.  If a symbol with the
C same name already exists it is deleted.
C Input arguments:
C  SYMBOL   - The symbol name.
C  PARS     - The parameters making up the symbol value.
C  NPAR     - The number of these.
C Output argument:
C  IFAIL    - Zero if the symbol was added successfully, else > 0.
C
C Initialize the tables if this has not already been done.
C
C  27 Jun - paul Harrison - correct error in linked lists
C
      IF(INITAB) THEN
        CALL SYMINI(PTRTBL,NAMTBL,HSHTBL,PTRFHD,PTRFTL,NAMFHD,NAMFTL,
     &              NAMAHD,NAMATL,NFENTP,MAXSYM,MAXNAM,2**MAXHSH)
        INITAB=.FALSE.
      ENDIF
C
C Count the number of symbol table entries that will be needed to store
C the symbol value, and obtain the maximum parameter length while we are
C doing it.  Note that if there is more than one parameter then blank
C symbols take up one entry each.
C
      IF(NPAR.LE.0) THEN
        MINLEN=0
      ELSE
        MINLEN=1
      ENDIF
      MAXL=MINLEN
      VENT=0
      DO 1 I=1,NPAR
        L=MAX(MINLEN,LENGTH(PARS(I)))
        MAXL=MAX(MAXL,L)
        VENT=VENT+L/BITSYM+MIN(1,MOD(L,BITSYM))
    1 CONTINUE
C
C Check if the symbol already exists.
C
      CALL SYMFSY(SYMBOL,SYMTBL,PTRTBL,NAMTBL,HSHTBL,BITSYM,MAXHSH,
     &            NAMPTR,HSHPTR)
      IF(NAMPTR.GT.0) THEN
C
C It does, check that there will be room to store the new symbol value.
C
        IF(VENT.LE.NFENTP+NAMTBL(10,NAMPTR)) THEN
C
C There is, add the existing symbol value entries (if any) to the head
C of the free list.
C
          CALL SYMADL(PTRFHD,PTRFTL,PTRTBL,2,NAMTBL(5,NAMPTR),
     &                NAMTBL(6,NAMPTR))
C
C Adjust the number of free symbol table entries.
C
          NFENTP=NFENTP+NAMTBL(10,NAMPTR)-VENT
C
C Now add the new symbol value using the existing name block.
C
          CALL SYMAVL(PARS,NPAR,PTRFHD,PTRFTL,PTRTBL,SYMTBL,BITSYM,
     &                NAMTBL(5,NAMPTR),NAMTBL(6,NAMPTR),
     &                NAMTBL(10,NAMPTR))
C
C Update the maximum parameter length and the number of parameters in
C the name block.
C
          NAMTBL(11,NAMPTR)=MAXL
          NAMTBL(12,NAMPTR)=NPAR
C
C Indicate success.
C
          IFAIL=0
        ELSE
C
C There is insufficient room to store the new symbol value, flag an
C error.
C
          CALL LIBERR(ROUTN,72,0,0,0.0,' ',IFAIL)
        ENDIF
      ELSE
C
C The symbol does not already exist, check that there is a spare name
C table block.
C
        IF(NAMFHD.GT.0) THEN
C
C There is, now check that there is room in the symbol table to store
C both the symbol name and its value.
C
          IF(LENGTH(SYMBOL)/BITSYM+MIN(1,MOD(LENGTH(SYMBOL),BITSYM))+
     &       VENT.LE.NFENTP) THEN
C
C There is room, obtain the next name table block from the free list.
C
            NEWENT=NAMFHD
            NAMFHD=NAMTBL(2,NEWENT)
            IF(NAMFHD.GT.0) THEN
              NAMTBL(1,NAMFHD)=0
            ELSE
              NAMFTL=0
            ENDIF
C
C Link this entry into the name table at the correct point.
C
            IF(HSHTBL(1,HSHPTR).LE.0) THEN
C
C It is the first entry for the current hash table index, just link it
C to the hash table.
C
              HSHTBL(1,HSHPTR)=NEWENT
              HSHTBL(2,HSHPTR)=NEWENT
              NAMTBL(2,NEWENT)=0
            ELSE
	     IF(NAMPTR.NE.0) THEN
C
C It is not the first entry, namptr contains the entry number after
C which the new block is to be placed.
C
              NAMPTR=ABS(NAMPTR)
              IF(NAMTBL(2,NAMPTR).GT.0) THEN
C
C It is to placed between existing entries.
C
                NAMTBL(2,NEWENT)=NAMTBL(2,NAMPTR)
                NAMTBL(1,NAMTBL(2,NEWENT))=NEWENT
              ELSE
C
C It is to be placed after the existing entries.
C
                NAMTBL(2,NEWENT)=0
                HSHTBL(2,HSHPTR)=NEWENT
              ENDIF
C
C Finish off the linking in.
C
              NAMTBL(1,NEWENT)=NAMPTR
              NAMTBL(2,NAMPTR)=NEWENT
            ELSE
C
C It is to be placed before the existing entries.
C
              NAMTBL(1,HSHTBL(1,HSHPTR))=NEWENT
              NAMTBL(2,NEWENT)=HSHTBL(1,HSHPTR)
              HSHTBL(1,HSHPTR)=NEWENT
            ENDIF
	    ENDIF
C
C Add the symbol name into the symbols table.
C
            CALL SYMAVL(SYMBOL,1,PTRFHD,PTRFTL,PTRTBL,SYMTBL,BITSYM,
     &                  NAMTBL(3,NEWENT),NAMTBL(4,NEWENT),I)
C
C Store its length in the name block.
C
            LNAME=LENGTH(SYMBOL)
            NAMTBL(9,NEWENT)=LNAME
C
C Store the maximum parameter length and the number of parameters in the
C name block.
C
            NAMTBL(11,NEWENT)=MAXL
            NAMTBL(12,NEWENT)=NPAR
C
C Add the symbol value into the symbols table.
C
            CALL SYMAVL(PARS,NPAR,PTRFHD,PTRFTL,PTRTBL,SYMTBL,BITSYM,
     &                  NAMTBL(5,NEWENT),NAMTBL(6,NEWENT),
     &                  NAMTBL(10,NEWENT))
C
C Now link the new entry into the 'alpha' order list.
C
            IF(NAMAHD.LE.0) THEN
C
C There are no entries yet.
C
              NAMAHD=NEWENT
              NAMATL=NEWENT
              NAMTBL(7,NEWENT)=0
              NAMTBL(8,NEWENT)=0
            ELSEIF(LNAME.LE.0) THEN
C
C The new symbol is blank, add it at the head of the list.
C
              NAMTBL(7,NAMAHD)=NEWENT
              NAMTBL(7,NEWENT)=0
              NAMTBL(8,NEWENT)=NAMAHD
              NAMAHD=NEWENT
            ELSE
C
C Scan through the existing entries until we find one that is 'greater'
C than the new entry, or until the entire list has been searched.
C
              NEXT=NAMAHD
 1000         IF(NEXT.GT.0) THEN
C
C Compare the current entry up to the length of the shortest name.
C
                NEXTE1=NAMTBL(3,NEWENT)
                NEXTE2=NAMTBL(3,NEXT)
 1001           IF(NEXTE1.GT.0.AND.NEXTE2.GT.0) THEN
                  IF(SYMTBL(NEXTE2).LT.SYMTBL(NEXTE1)) THEN
C
C The current symbol is less than the new one, try the next entry.
C
                    NEXT=NAMTBL(8,NEXT)
                    GOTO 1000
                  ELSEIF(SYMTBL(NEXTE2).EQ.SYMTBL(NEXTE1)) THEN
C
C The current symbol is the same so far, test the next bit of it.
C
                    NEXTE1=PTRTBL(2,NEXTE1)
                    NEXTE2=PTRTBL(2,NEXTE2)
                    GOTO 1001
                  ELSE
C
C The current symbol is greater than the new one, exit.
C
                    NEXT=NAMTBL(7,NEXT)
                    GOTO 1002
                  ENDIF
                ENDIF
C
C The symbols are identical up to the length of the shortest one, which
C is the shortest?
C
                IF(NAMTBL(9,NEWENT).LT.NAMTBL(9,NEXT)) THEN
C
C The new entry is, we have found a greater entry: exit.
C
                  NEXT=NAMTBL(7,NEXT)
                  GOTO 1002
                ELSE
C
C The current entry is shorter, keep searching for a greater entry.
C
                  NEXT=NAMTBL(8,NEXT)
                  GOTO 1000
                ENDIF
              ELSE
C
C We have run out of symbols, put the new entry after the last one.
C
                NEXT=NAMATL
              ENDIF
C
C Add the new entry after the one identified by the current value of
C next.
C
 1002         NAMTBL(7,NEWENT)=NEXT
              IF(NEXT.GT.0) THEN
                NAMTBL(8,NEWENT)=NAMTBL(8,NEXT)
                NAMTBL(8,NEXT)=NEWENT
              ELSE
                NAMTBL(8,NEWENT)=NAMAHD
                NAMAHD=NEWENT
              ENDIF
              IF(NAMTBL(8,NEWENT).GT.0) THEN
                NAMTBL(7,NAMTBL(8,NEWENT))=NEWENT
              ELSE
                NAMATL=NEWENT
              ENDIF
C
C Indicate success.
C
              IFAIL=0
            ENDIF
          ELSE
C
C There is insufficient room in the symbols table for the name and/or
C value, flag an error.
C
            CALL LIBERR(ROUTN,72,1,0,0.0,' ',IFAIL)
          ENDIF
        ELSE
C
C There are no free name table blocks free, flag an error.
C
          CALL LIBERR(ROUTN,73,2,MAXNAM,0.0,' ',IFAIL)
        ENDIF
      ENDIF
      RETURN
C
C
C
C
      ENTRY SYMDEL(SYMBOL,IFAIL)
C
C This routine deletes a symbol from the tables.
C Input argument:
C  SYMBOL   - The symbol name.
C Output argument:
C  IFAIL    - Zero if no errors were detected, else > 0.
C
C Have the tables been initialized yet?
C
      IF(INITAB) THEN
C
C No, so the symbol cannot exist.
C
        CALL LIBERR(ROUTN,74,3,0,0.0,SYMBOL,IFAIL)
      ELSE
C
C Yes, attempt to find the symbol.
C
        CALL SYMFSY(SYMBOL,SYMTBL,PTRTBL,NAMTBL,HSHTBL,BITSYM,MAXHSH,
     &              NAMPTR,HSHPTR)
C
C Does it exist?
C
        IF(NAMPTR.GT.0) THEN
C
C It does, count the number of symbol table entries allocated to its
C name.
C
          NENT=0
          NEXTE1=NAMTBL(3,NAMPTR)
 2000     IF(NEXTE1.GT.0) THEN
            NENT=NENT+1
            NEXTE1=PTRTBL(2,NEXTE1)
            GOTO 2000
          ENDIF
C
C Free these entries.
C
          CALL SYMADL(PTRFHD,PTRFTL,PTRTBL,2,NAMTBL(3,NAMPTR),
     &                NAMTBL(4,NAMPTR))
C
C Free the symbol table entries used for the symbol value.
C
          CALL SYMADL(PTRFHD,PTRFTL,PTRTBL,2,NAMTBL(5,NAMPTR),
     &                NAMTBL(6,NAMPTR))
C
C Adjust the number of free entries available.
C
          NFENTP=NFENTP+NENT+NAMTBL(10,NAMPTR)
C
C Remove the name table block from the hash table list.
C
          IF(NAMTBL(1,NAMPTR).GT.0) THEN
            NAMTBL(2,NAMTBL(1,NAMPTR))=NAMTBL(2,NAMPTR)
          ELSE
            HSHTBL(1,HSHPTR)=NAMTBL(2,NAMPTR)
          ENDIF
          IF(NAMTBL(2,NAMPTR).GT.0) THEN
            NAMTBL(1,NAMTBL(2,NAMPTR))=NAMTBL(1,NAMPTR)
          ELSE
            HSHTBL(2,HSHPTR)=NAMTBL(1,NAMPTR)
          ENDIF
C
C Remove the name table block from the 'alpha' ordered list.
C
          IF(NAMTBL(7,NAMPTR).GT.0) THEN
            NAMTBL(8,NAMTBL(7,NAMPTR))=NAMTBL(8,NAMPTR)
          ELSE
            NAMAHD=NAMTBL(8,NAMPTR)
          ENDIF
          IF(NAMTBL(8,NAMPTR).GT.0) THEN
            NAMTBL(7,NAMTBL(8,NAMPTR))=NAMTBL(7,NAMPTR)
          ELSE
            NAMATL=NAMTBL(7,NAMPTR)
          ENDIF
C
C Finally add the name block to the free list.
C
          CALL SYMADL(NAMFHD,NAMFTL,NAMTBL,10,NAMPTR,NAMPTR)
C
C Indicate success.
C
          IFAIL=0
        ELSE
C
C The requested symbol does not exist.
C
          CALL LIBERR(ROUTN,74,4,0,0.0,SYMBOL,IFAIL)
        ENDIF
      ENDIF
      RETURN
C
C
C
C
      ENTRY SYMCLR
C
C This routine clears all symbols.
C
C All we need to do is ask for a table initialize the next time we try
C to anything.
C
      INITAB=.TRUE.
      RETURN
C
C
C
C
      ENTRY SYMGET(SYMBOL,PARS,IP,NPAR,MAXPAR,IFAIL)
C
C This routine returns the value of a symbol into the array pars.  If
C npar is greater than zero on entry parameters ip+1 to npar are shifted
C up in pars to make room for the new ones; otherwise they are
C overwritten.
C Input arguments:
C  SYMBOL   - The symbol name.
C  PARS     - An array to receive the symbol value (see above).
C  IP       - The starting location in pars to receive the value.
C  NPAR     - The number of parameters already in the parameters array.
C  MAXPAR   - The maximum size of the parameters array.
C Output arguments:
C  NPAR     - The new number of parameters.
C  IP       - Points the the last parameter loaded into pars.
C  IFAIL    - Zero if no errors were detected, -1 if the symbol does not
C             exist, else > 0.  no error is flagged if the symbol does
C             not exist.
C
C Have the tables been initialized yet?
C
      IF(INITAB) THEN
C
C No, so the symbol cannot exist.
C
        IFAIL=-1
      ELSE
C
C Yes, attempt to find the symbol.
C
        CALL SYMFSY(SYMBOL,SYMTBL,PTRTBL,NAMTBL,HSHTBL,BITSYM,MAXHSH,
     &              NAMPTR,HSHPTR)
C
C Does it exist?
C
        IF(NAMPTR.GT.0) THEN
C
C Yes, check that there is room for its value in the parameter array.
C
          NPAR=MAX(0,NPAR)
          IF(NPAR.EQ.0) THEN
            N=0
          ELSE
            N=NPAR-1
          ENDIF
          VENT=MAX(1,NAMTBL(12,NAMPTR))
          IF(N+VENT.LE.MAXPAR) THEN
C
C There is, check that the maximum parameter length stored in the
C table is not too long to fit into pars.
C
            IF(NAMTBL(11,NAMPTR).LE.LEN(PARS(1))) THEN
C
C It is not, first shift up the parameters in pars to make room for
C the new ones.
C
              DO 2 I=NPAR,IP+1,-1
                PARS(I+VENT-1)=PARS(I)
    2         CONTINUE
C
C Return the new number of parameters.
C
              NPAR=N+VENT
C
C Is there a symbol value stored?
C
              IF(NAMTBL(5,NAMPTR).GT.0) THEN
C
C Yes, load its value into pars.
C
                NEXTE1=-NAMTBL(5,NAMPTR)
                IP=IP-1
 3000           IF(NEXTE1.NE.0) THEN
C
C Is this the start of a new parameter?
C
                  IF(NEXTE1.LT.0) THEN
C
C Yes, increment the parameter counter and set the character counter.
C
                    IP=IP+1
                    IC=1
                    NEXTE1=ABS(NEXTE1)
                  ENDIF
C
C Load the current bit into the parameter array.
C
                  PARS(IP)(IC:)=SYMTBL(NEXTE1)
C
C Increment the character counter.
C
                  IC=IC+BITSYM
C
C Get the next bit of the value.
C
                  NEXTE1=PTRTBL(2,NEXTE1)
                  GOTO 3000
                ENDIF
              ELSE
C
C There is no symbol value stored, just load an empty parameter.
C
                PARS(IP)=' '
              ENDIF
C
C Finished, indicate success.
C
              IFAIL=0
            ELSE
C
C The length of a single element in pars is not enough to hold the
C longest parameter in the symbol value: flag an error.
C
              CALL LIBERR(ROUTN,75,5,NAMTBL(11,NAMPTR),0.0,' ',IFAIL)
            ENDIF
          ELSE
C
C There are too many parameters to fit into pars, flag an error.
C
            CALL LIBERR(ROUTN,3,6,MAXPAR,0.0,' ',IFAIL)
          ENDIF
        ELSE
C
C The symbol does not exist.
C
          IFAIL=-1
        ENDIF
      ENDIF
      RETURN
C
C
C
C
      ENTRY SYMEXI(SYMBOL,SEXI)
C
C This routine returns .true. If a symbol exists, otherwise it returns
C .false.
C Input argument:
C  SYMBOL   - The symbol name.
C Output argument:
C  SEXI     - .true. If the name exists, else .false..
C
C Have the tables been initiialized yet?
C
      IF(INITAB) THEN
C
C No, so the symbol cannot exist.
C
        SEXI=.FALSE.
      ELSE
C
C Yes, attempt to locate it.
C
        CALL SYMFSY(SYMBOL,SYMTBL,PTRTBL,NAMTBL,HSHTBL,BITSYM,MAXHSH,
     &              NAMPTR,HSHPTR)
C
C The symbol exists if namptr is  > 0.
C
        SEXI=NAMPTR.GT.0
      ENDIF
      RETURN
C
C
C
C
      ENTRY SYMSHO(SYMBOL,OUTBUF,IFAIL)
C
C This routine prints the value of a specified symbol.
C Input arguments:
C  SYMBOL   - The symbol name.
C  OUTBUF   - A character work space array.
C Output argument:
C  IFAIL    - Zero if no errors were detected, else > 0.
C     THE OUTPUT IS OF THE FORM    <name> = <value>; outbuf and the
C output line length must be at least long enough to hold the
C <name> = Section or the longest parameter in the symbol value plus
C 10 characters, whichever is the longer..
C
C Have the tables been initialized yet?
C
      IF(INITAB) THEN
C
C No, so the symbol cannot exist.
C
        CALL LIBERR(ROUTN,74,7,0,0.0,SYMBOL,IFAIL)
      ELSE
C
C Yes, attempt to locate the symbol.
C
        CALL SYMFSY(SYMBOL,SYMTBL,PTRTBL,NAMTBL,HSHTBL,BITSYM,MAXHSH,
     &              NAMPTR,HSHPTR)
C
C Does it exist?
C
        IF(NAMPTR.GT.0) THEN
C
C It does, start by loading its name into the output buffer.
C
          OUTBUF='   '//SYMBOL(1:LENGTH(SYMBOL))//' ='
C
C Now print the symbol value.
C
          CALL SYMPRT(SYMTBL,PTRTBL,NAMTBL,NAMPTR,BITSYM,OUTBUF)
C
C Indicate success.
C
          IFAIL=0
        ELSE
C
C The symbol does not exist.
C
          CALL LIBERR(ROUTN,74,8,0,0.0,SYMBOL,IFAIL)
        ENDIF
      ENDIF
      RETURN
C
C
C
C
      ENTRY SYMLST(OUTBUF)
C
C This routine prints the values of all symbols.
C Input argument:
C  OUTBUF   - A character work space array.
C     THE OUTPUT IS OF THE FORM       <name> = <value>; outbuf and the
C output line length must be at least long enough to hold the
C <name> = Section or the longest parameter in the symbol value plus
C 10 characters, whichever is the longer..
C
C Have the tables been initialized yet?
C
      IF(INITAB) THEN
C
C No, so there are no symbols.
C
        CALL OUTLIN('   No symbols are defined',1)
      ELSE
C
C Yes, but are there any symbols?
C
        IF(NAMAHD.LE.0) THEN
C
C There are not, say so.
C
          CALL OUTLIN('   No symbols are defined',1)
        ELSE
C
C There are some.  Form the heading.
C
          OUTBUF='   Symbol definition!:'
          IF(NAMTBL(8,NAMAHD).GT.0) THEN
            CALL SETIFM(2,OUTBUF)
          ELSE
            CALL SETIFM(1,OUTBUF)
          ENDIF
C
C Print it.
C
          CALL OUTLIN(OUTBUF,1)
C
C Now print the symbol names and values.
C
          NEXT=NAMAHD
 4000     IF(NEXT.GT.0) THEN
C
C Load the name of the current symbol into the output buffer.
C
            OUTBUF=' '
            IC=7
            NEXTE1=NAMTBL(3,NEXT)
 4001       IF(NEXTE1.GT.0) THEN
              OUTBUF(IC:)=SYMTBL(NEXTE1)
              NEXTE1=PTRTBL(2,NEXTE1)
              IC=IC+BITSYM
              GOTO 4001
            ENDIF
C
C Add on the '='.
C
            OUTBUF(MAX(9,LENGTH(OUTBUF)+2):)='='
C
C Print the symbol value.
C
            CALL SYMPRT(SYMTBL,PTRTBL,NAMTBL,NEXT,BITSYM,OUTBUF)
C
C Do the next symbol.
C
            NEXT=NAMTBL(8,NEXT)
            GOTO 4000
          ENDIF
        ENDIF
      ENDIF
      RETURN
C
C
C
C
      ENTRY SYMSAV(IUNIT,SFILE,IFAIL)
C
C This routine saves all symbols tables to a file.
C Input arguments:
C  IUNIT    - The unit number to use for the file.
C  SFILE    - The file name.
C Output argument:
C  IFAIL    - Zero if no errors were detected, else > 0.
C
C Have the tables been initialized yet?
C
      IF(INITAB) THEN
C
C No, there are no symbols to write, flag an error.
C
        CALL LIBERR(ROUTN,76,9,0,0.0,' ',IFAIL)
      ELSE
C
C Yes, check that there are some symbols.
C
        IF(NAMAHD.GT.0) THEN
C
C There are, attempt to open a new file for them.
C
          CALL SYMOPN(IUNIT,SFILE,3,IFAIL)
C
C Test if successful.
C
          IF(IFAIL.EQ.0) THEN
C
C Yes, write the values.  First the header information.
C
            CALL OLDATE(DATE)
            CALL OLTIME(TIME)
            WRITE(IUNIT,ERR=5777,IOSTAT=I) IDBPAT,SYFVER,DATE,TIME
C
C Now the table sizes.
C
            WRITE(IUNIT,ERR=5777,IOSTAT=I) MAXSYM,BITSYM,MAXNAM,MAXHSH
C
C Now the list heads and tails.
C
            WRITE(IUNIT,ERR=5777,IOSTAT=I) PTRFHD,PTRFTL,NAMFHD,NAMFTL,
     &                                     NAMAHD,NAMATL,NFENTP
C
C Finally the tables themselves.
C
            WRITE(IUNIT,ERR=5777,IOSTAT=I) SYMTBL
            WRITE(IUNIT,ERR=5777,IOSTAT=I) PTRTBL
            WRITE(IUNIT,ERR=5777,IOSTAT=I) NAMTBL
            WRITE(IUNIT,ERR=5777,IOSTAT=I) HSHTBL
C
C Close the file.
C
            CLOSE(IUNIT,IOSTAT=IFAIL)
C
C Test if successful.
C
            IF(IFAIL.NE.0) THEN
C
C No flag an error.
C
              CALL LIBERR(ROUTN,66,10,0,0.0,'Symbols''',IFAIL)
            ENDIF
          ELSE
C
C Failed to open the symbols file, flag an error.
C
            CALL LIBERR(ROUTN,30,11,0,0.0,SFILE,IFAIL)
          ENDIF
        ELSE
C
C There are no symbols to write.
C
          CALL LIBERR(ROUTN,76,12,0,0.0,' ',IFAIL)
        ENDIF
      ENDIF
      RETURN
C
C Write error, close the file and flag the error.
C
 5777 CLOSE(IUNIT,IOSTAT=IFAIL)
      CALL LIBERR(ROUTN,77,13,I,0.0,' ',IFAIL)
      RETURN
C
C
C
C
      ENTRY SYMLOD(IUNIT,SFILE,IFAIL)
C
C This routine loads the symbols tables from a file.
C Input arguments:
C  IUNIT    - The unit number to use for the file.
C  SFILE    - The file name.
C Output argument:
C  IFAIL    - Zero if no errors were detected, else > 0.
C
C Attempt to open the file.
C
      CALL SYMOPN(IUNIT,SFILE,-1,IFAIL)
C
C Test if successful.
C
      IF(IFAIL.EQ.0) THEN
C
C Yes, read in the file header.
C
        READ(IUNIT,ERR=6777,IOSTAT=I) BITPAT,VERSN,DATE,TIME
C
C Check that it is a symbols file.
C
        IF(BITPAT.EQ.IDBPAT) THEN
C
C Probably, read the array sizes.
C
          READ(IUNIT,ERR=6777,IOSTAT=I) MXSYM,BTSYM,MXNAM,MXHSH
C
C Check that these are the same as the current ones.
C
          IF(MXSYM.EQ.MAXSYM.AND.BTSYM.EQ.BITSYM.AND.MXNAM.EQ.MAXNAM
     &       .AND.MXHSH.EQ.MAXHSH) THEN
C
C They are, set the initialize flag in case the following reads fail.
C
            INITAB=.TRUE.
C
C Read the list pointers.
C
            READ(IUNIT,ERR=6777,IOSTAT=I) PTRFHD,PTRFTL,NAMFHD,NAMFTL,
     &                                    NAMAHD,NAMATL,NFENTP
C
C Finally the tables themselves.
C
            READ(IUNIT,ERR=6777,IOSTAT=I) SYMTBL
            READ(IUNIT,ERR=6777,IOSTAT=I) PTRTBL
            READ(IUNIT,ERR=6777,IOSTAT=I) NAMTBL
            READ(IUNIT,ERR=6777,IOSTAT=I) HSHTBL
C
C Cancel the initialize request flag.
C
            INITAB=.FALSE.
C
C Close the file.
C
            CLOSE(IUNIT,IOSTAT=IFAIL)
C
C Test if successful.
C
            IF(IFAIL.NE.0) THEN
C
C No, flag an error.
C
              CALL LIBERR(ROUTN,66,14,0,0.0,'Symbols''',IFAIL)
            ENDIF
          ELSE
C
C The table sizes are different, flag an error.
C
            CLOSE(IUNIT,IOSTAT=I)
            CALL LIBERR(ROUTN,79,15,0,0.0,' ',IFAIL)
          ENDIF
        ELSE
C
C The file is not a symbols file.
C
          CLOSE(IUNIT,IOSTAT=I)
          CALL LIBERR(ROUTN,80,16,0,0.0,SFILE,IFAIL)
        ENDIF
      ELSEIF(IFAIL.EQ.2) THEN
C
C The requested file does not exist.
C
        CALL LIBERR(ROUTN,31,17,0,0.0,SFILE,IFAIL)
      ELSE
C
C Unable to open the requested file.
C
        CALL LIBERR(ROUTN,30,18,0,0.0,SFILE,IFAIL)
      ENDIF
      RETURN
C
C Read error, close the file and flag the error.
C
 6777 CLOSE(IUNIT,IOSTAT=IFAIL)
      CALL LIBERR(ROUTN,78,19,I,0.0,' ',IFAIL)
      RETURN
C
C End of subroutine symcom.
C
      END
