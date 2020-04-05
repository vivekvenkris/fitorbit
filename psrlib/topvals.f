         SUBROUTINE TOPVALS(DAT,NDAT,NTOP,IDX,NEXT,BOTTOM,STATUS)

c finds the NTOP highest values in DAT(NDAT) places pointers to these
c values in IDX(NTOP). The ordering of the NTOP values can be recovered
c from BOTTOM (which points via the corresponding element in IDX to the
c lowest value of the top NTOP) The next highest value pointer is pointed
c to by NEXT. The elements may thus be printed in ascending order by the
c following code fragment.

C
C      iptr=bottom      
C      do i=1,ntop
C       write(*,*) dat(idx(iptr))
C       iptr=next(iptr)
C      enddo       

C Paul Harrison, Matthew Bailes 21-SEP-1990

         INTEGER NDAT,NTOP,NRES,STATUS
         INTEGER IDX(NTOP),NEXT(NTOP),BOTTOM,I,IPTR,ILAST
         INTEGER ITEMP
         REAL DAT(NDAT),MINVAL

c     test if ndat is smaller than ntop panic!

         STATUS = 0
         IF (NDAT.LT.NTOP) THEN
            STATUS = -1
            RETURN
         END IF

c  Quiksort the first ntop points

         CALL INDEXX(NTOP,DAT,IDX)

c  Fill next and initialise bottom and next(ntop)

         DO I=1,NTOP-1
            NEXT(I)=I+1
         END DO
         NEXT(NTOP) = 0
         BOTTOM = 1

c start main loop

         MINVAL = DAT(IDX(BOTTOM))
         DO I=NTOP+1, NDAT
            IF (DAT(I).GT.MINVAL) THEN
               ILAST = BOTTOM
               IPTR  = NEXT(BOTTOM)
               DO WHILE(DAT(I).GT.DAT(IDX(MAX(1,IPTR))).AND.
     :                                                (IPTR.NE.0))
                   ILAST = IPTR
                   IPTR = NEXT(IPTR)
               END DO
               IDX(BOTTOM)=I
               IF(ILAST.NE.BOTTOM)THEN
                 ITEMP = NEXT(BOTTOM)
                 NEXT(BOTTOM)=IPTR
                 NEXT(ILAST)=BOTTOM
                 BOTTOM=ITEMP
               ENDIF
               MINVAL = DAT(IDX(BOTTOM))
            END IF
         END DO

         RETURN
         END


