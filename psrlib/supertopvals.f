       SUBROUTINE SUPERTOPVALS(DAT,NDAT,NTOP,NRES, IDX,
     &        NEXT,BOTTOM,STATUS)

c finds the NTOP highest values in DAT(NDAT) places pointers to these
c values in IDX(NTOP). The ordering of the NTOP values can be recovered
c from BOTTOM (which points via the corresponding element in IDX to the
c lowest value of the top NTOP) The next highest value pointer is pointed
c to by NEXT. The elements may thus be printed in ascending order by the
c following code fragment.
C modified version of topvals PAH 23-MAY-1991 - will only return
C 'peaks' if they are NRES points apart.
C
C paul harrison 

C
C      iptr=bottom      
C      do i=1,ntop
C       write(*,*) dat(idx(iptr))
C       iptr=next(iptr)
C      enddo       


         INTEGER NDAT,NTOP,NRES,STATUS
         INTEGER IDX(NTOP),NEXT(NTOP),BOTTOM,I,IPTR,ILAST
         INTEGER ITEMP, IDXPREV
         REAL DAT(NDAT),MINVAL

c     test if ndat is smaller than ntop panic!

         STATUS = 0
         IF (NDAT.LT.NTOP) THEN
            STATUS = -1
            RETURN
         END IF

Cc  Quicksort the first ntop points (these points do not have the
Cc setriction of being separated by nres - a minor bug)
C
C         CALL INDEXX(NTOP,DAT,IDX)
C
c  Fill next and initialise bottom and next(ntop)

         DO I=1,NTOP-1
            NEXT(I)=I+1 
            IDX(I)=1
         END DO
         NEXT(NTOP) = 0
         BOTTOM = 1
         IDXPREV = 1
         MINVAL = DAT(1)

c start main loop
         DO I=2, NDAT
            IF (DAT(I).GT.MINVAL) THEN

C first check that the point is further from previously found point
               IF(I-IDXPREV.GT.NRES) THEN

C put the previous point into the list
                   ILAST = BOTTOM
                   IPTR  = NEXT(BOTTOM)
                   DO WHILE(DAT(IDXPREV).GT.DAT(IDX(MAX(1,IPTR))).AND.
     :                                                (IPTR.NE.0))
                       ILAST = IPTR
                       IPTR = NEXT(IPTR)
                   END DO
                   IDX(BOTTOM)=IDXPREV
                   IF(ILAST.NE.BOTTOM)THEN
                     ITEMP = NEXT(BOTTOM)
                     NEXT(BOTTOM)=IPTR
                     NEXT(ILAST)=BOTTOM
                     BOTTOM=ITEMP
                   ENDIF
                   MINVAL = DAT(IDX(BOTTOM))
                   IDXPREV=I
               ELSEIF(DAT(I).GT.DAT(IDXPREV)) THEN

C make the current I then maximum found in this NRES group
                   IDXPREV=I
               ENDIF
            END IF
         END DO

C there is still 1 IDXPREV left to be put into the list
         ILAST = BOTTOM
         IPTR  = NEXT(BOTTOM)
         DO WHILE(DAT(IDXPREV).GT.DAT(IDX(MAX(1,IPTR))).AND.
     :                                            (IPTR.NE.0))
             ILAST = IPTR
             IPTR = NEXT(IPTR)
         END DO
         IDX(BOTTOM)=IDXPREV
         IF(ILAST.NE.BOTTOM)THEN
           ITEMP = NEXT(BOTTOM)
           NEXT(BOTTOM)=IPTR
           NEXT(ILAST)=BOTTOM
           BOTTOM=ITEMP
         ENDIF
         MINVAL = DAT(IDX(BOTTOM))

         RETURN
         END


