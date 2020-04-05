*DECK WRDAT
C
C *************************************************************
      SUBROUTINE WRDAT ( IFAIL )
C *************************************************************
C
C WRITES DATA FROM THE INPUT ARRAY TO THE WORKING SPACE.
C THE FIRST DATA BIN IS TAKEN FROM THE ELEMENT POINTED TO BY INRD.
C INRD IS UNCHANGED AT THE END OF THE WRITE.
C IFAIL=0 IF NO ERROR HAS OCCURRED.
C
C THIS ROUTINE IS INSTALLATION DEPENDENT.
C
C VAX-11 FORTRAN VERSION.
C
c Mod 10/03/2006 Correct data type in call to pserr
c
      INCLUDE 'PSRLIB.DEF'
      INCLUDE 'PSRDAT.DEF'
      INTEGER INDAT(1024)
      EQUIVALENCE (WRK(39),INDAT)
C
      IFAIL = 0
C
      IF ( DATATYPE.EQ.0 ) THEN
C
C        DATATYPE 0 - SINGLE PULSE - 12-BIT INTEGER DATA BINS
C
         NW = NBIN/2
         DO 10 ICH=0,NCH-1
            IST = NW*ICH
            INST = LENCHAN*ICH
            DO 20 I=1,NW
               CALL MVBITS ( IN(INRD+I*2-2+INST),0,12
     &                      ,INDAT(I+IST),0 )
               CALL MVBITS ( IN(INRD+I*2-1+INST),0,12
     &                      ,INDAT(I+IST),12 )
   20       CONTINUE
   10    CONTINUE
      ELSEIF ( DATATYPE.EQ.1 ) THEN
C
C        DATATYPE 1 - INTEGRATED   - 24-BIT INTEGER
C
         DO 30 ICH=0,NCH-1
            IST = NBIN*ICH
            INST = LENCHAN*ICH
            DO 40 I=1,NDAT
               CALL MVBITS ( IN(INRD+I-1+INST),0,24
     &                      ,INDAT(I+IST),0 )
   40       CONTINUE
   30    CONTINUE
      ELSEIF ( DATATYPE.EQ.2 ) THEN
C
C        DATATYPE 2 - PROCESSED    - 24-BIT FLOATING POINT
C
         CALL PSRERR ( 'WRDAT',39,0,0.,'Writing of datatype 2 is' )
         IFAIL = 39
         NDAT = 0
      ELSEIF ( DATATYPE.EQ.3 ) THEN
C
C        DATATYPE 3 - SPECIAL      - 24-BIT FLOATING POINT
C
         CALL PSRERR ( 'WRDAT',39,0,0.,'Writing of datatype 3 is' )
         IFAIL = 39
         NDAT = 0
      ELSEIF ( DATATYPE.EQ.4 ) THEN
C
C        DATATYPE 4 - SEARCH       -  3-BIT INTEGER
C
         NW = NBIN/8
         DO 50 ICH=0,NCH-1
            IST = NW*ICH
            INST = LENCHAN*ICH
            DO 60 I=1,NW
               K = INRD+I*8
               DO 70 J=1,8
                  CALL MVBITS ( IN(K-J+INST),0,3
     &                         ,INDAT(I+IST),(J-1)*3 )
   70          CONTINUE
   60       CONTINUE
   50    CONTINUE
      ELSE
         CALL PSRERR ( 'WRDAT',50,DATATYPE,0.0,' ' )
         IFAIL = 50
      ENDIF
C
      RETURN
C
C END OF SUBROUTINE WRDAT
C
      END
