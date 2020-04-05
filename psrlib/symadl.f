*DECK SYMADL
C
C
C
      SUBROUTINE SYMADL(LHEAD,LTAIL,TABLE,TABLED,FIRST,LAST)
C
C This routine adds a series of linked table entries to the head of a
C linked list.
C Input arguments:
C  LHEAD    - The current list head (zero if empty).
C  LTAIL    - The current list tail.
C  TABLE    - The complete table of entries of dimension
C             table(tabled, ):
C              TABLE(1, ) = Pointer to previous entry
C              TABLE(2, ) = Pointer to next entry
C  TABLED   - The size of the first dimension of table.
C  FIRST    - The first table entry to be added (zero if none).
C  LAST     - The last table entry to be added.
C Output arguments:
C  LHEAD    - The new list head.
C  LTAIL    - The new list tail.
C  TABLE    - Is adjusted accordingly.
C     Version 1.0   12May85
C
      IMPLICIT INTEGER (A-Z)
      DIMENSION TABLE(TABLED,*)
C
C Is there anything to add to the list?
C
      IF(FIRST.GT.0) THEN
C
C Make sure that all of the forward pointers are positive.
C
        NEXT=FIRST
 1000   IF(NEXT.NE.LAST) THEN
          TABLE(2,NEXT)=ABS(TABLE(2,NEXT))
          NEXT=TABLE(2,NEXT)
          GOTO 1000
        ENDIF
C
C There is, check if there are any entries in the list.
C
        IF(LHEAD.GT.0) THEN
C
C Yes, link in the entries at the list head.
C
          TABLE(1,LHEAD)=LAST
          TABLE(2,LAST)=LHEAD
          LHEAD=FIRST
          TABLE(1,LHEAD)=0
        ELSE
C
C No, set the list head and tail.
C
          LHEAD=FIRST
          LTAIL=LAST
C
C Clear the first and last pointers in the list.
C
          TABLE(1,LHEAD)=0
          TABLE(2,LTAIL)=0
        ENDIF
      ENDIF
      RETURN
C
C End of subroutine symadl.
C
      END
