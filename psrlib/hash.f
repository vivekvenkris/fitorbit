
*DECK HASH
C
C
C
      SUBROUTINE HASH (SYMBOL, LENTBL, OFFSET)
C
C This routine hashes a symbol and returns the corresponding offset
C to a location in a hash table.  It is used to speed up symbol searches
C in the basic command line processor (GETCMD).  The sequence of
C operations when searching for, or adding, a symbol is as follows:
C 1. The symbol name is hashed by this routine.
C 2. The resultant value is used as an offset into the hash table.
C 3. The contents of the hash table at the offset are used as a pointer
C    to the head of a list containing the symbol expansions.
C Input arguments:
C  SYMBOL   - The symbol name to be hashed (trailing blanks are
C             significant).
C  LENTBL   - The size of the hash table expressed as a power of two.
C Output argument:
C  OFFSET   - The offset into the hash table (0 is the first element).
C     In this generic routine, OFFSET is always returned as zero.
C     Version 1.2   8th September, 1986   Generic
C Paul Harrison  4-FEB-1991 - return the length of the string as the hash
C value, as some of the other routines seem to rely on this property. This
C is a hack that might break in the future!!!! 
C
C Declare the routine's arguments.
C
      CHARACTER SYMBOL*(*)
      INTEGER   LENTBL, OFFSET, LENGTH
C
C Return an offset of zero.
C
      OFFSET = LENGTH(SYMBOL)
C      OFFSET=0
C
C End of subroutine HASH.
C
      END
