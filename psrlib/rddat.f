*DECK RDDAT
C
C *************************************************************
      SUBROUTINE RDDAT ( IFAIL )
C *************************************************************
C
C Reads data from the working space assuming that it contains
C a header followed by data from a previous call of RDHEAD
C giving four bytes of array in for each data bin.
C Call RDASPDAT if reading aspdata.
C The first data bin is loaded into the element pointed to by INWR
C and INWR is updated at the end of the read
C
C IFAIL=0 if no error has occurred
C
C THIS ROUTINE IS INSTALLATION DEPENDENT
C
C VAX-11 FORTRAN VERSION
C
c Mod 10/03/2006 Correct data type in call to pserr
c
      INCLUDE 'PSRDAT.DEF'
      INCLUDE 'PSRLIB.DEF'
c$$$      INTEGER MAXWORK
c$$$      PARAMETER (MAXWORK=64000)
      PARAMETER (LENIND=MAXWORK-39)
c      REAL WRK(MAXWORK)

      INTEGER INDAT(LENIND)
      EQUIVALENCE (WRK(39),INDAT)
C
      IFAIL = 0
C
C Check if we are dealing with aspdata
C
      IF (LASPDAT) THEN
        CALL RDASPDAT(IFAIL)
        RETURN
      ENDIF 
C
      IF ( NCH.GT.MAXCHAN ) THEN
         CALL OUTMON (
     &    ' ***** No. of input data channels exceeds array space')
         CALL OUTMON ( '       Number of channels truncated')
         NCH = MAXCHAN
      ENDIF
C
C Save original number of channels
C
      NCORIG = NCH
C
C Ascertain the number of channels to be read
C
      NCH = 0
      DO I=1,NCORIG
         IF ( ICCURR(I).GT.0 ) NCH = NCH+1
      ENDDO
C
C Check number of channels
C
      IF ( NCH.EQ.0 ) THEN
         IFAIL = 65
         CALL PSRERR ('RDDAT1',IFAIL,0,0.,' ')
         RETURN
      ENDIF
C
C Check that the length of the data array has been set
C
      IF ( LENDAT.EQ.0 ) LENDAT = MAXIN
C
C If the number of channels has changed, reset the data
C  storage control variables
C
      IF ( LENDAT/NCH.NE.LENCHAN ) THEN
         LENCHAN = LENDAT/NCH
         INRD = 0
         INWR = 0
      ENDIF
C
C Check for room in data array
C
      IF ( INWR+NBIN.GT.LENCHAN ) THEN
         IFAIL = 41
         CALL PSRERR ('RDDAT',IFAIL,INWR+NBIN,0.,'Data')
         RETURN
      ENDIF
C
      IF ( DATATYPE.EQ.0 ) THEN
C
C DATATYPE 0 - SINGLE PULSE - 12-BIT INTEGER DATA BINS
C
         NW = NBIN/2
         DO 10 ICH=0,NCH-1
            IST = NW*(ICORIG(ICH+1)-1)
            INST = LENCHAN*ICH
            DO 20 I=1,NW
               RIN(INWR+I*2-1+INST) = JIBITS(INDAT(I+IST),12,12)
               RIN(INWR+I*2  +INST) = JIBITS(INDAT(I+IST), 0,12)
   20       CONTINUE
   10    CONTINUE
      ELSEIF ( DATATYPE.EQ.1 ) THEN
C
C DATATYPE 1 - INTEGRATED   - 24-BIT INTEGER
C
C FIRST CALCULATE THE EFFECTIVE INTEGRATION TIME FOR NORMALISATION OF
C    DATA TO COUNTS IN 0.01 SEC..
         IF (NPULSES.NE.0) THEN
           BINTIME = 100 * TBIN * NPULSES
         ELSE
           BINTIME = 100 * TBIN
         ENDIF
         DO 30 ICH=0,NCH-1
            IST = NBIN*(ICORIG(ICH+1)-1)
            INST = LENCHAN*ICH
            DO 40 I=1,NBIN
               RIN(INWR+I+INST) = JIBITS(INDAT(I+IST),0,24)/BINTIME
   40       CONTINUE
   30    CONTINUE
      ELSEIF ( DATATYPE.EQ.2 ) THEN
C
C DATATYPE 2 - PROCESSED    - 24-BIT FLOATING POINT
C
C FIRST CALCULATE THE EFFECTIVE INTEGRATION TIME FOR NORMALISATION OF
C    DATA TO COUNTS IN 0.01 SEC.
         IF (NPULSES.NE.0) THEN
           BINTIME = 100 * TBIN * NPULSES
         ELSE
           BINTIME = 100 * TBIN
         ENDIF
C
         DO ICH=0,NCH-1
            IST = NBIN*(ICORIG(ICH+1)-1)
            INST = LENCHAN*ICH
            DO I=1,NBIN
C
C Fiddle here for starting point of Data array
C    as the routine has assumed that there are 38 words in the
C    header, but in this case there are 256 words.
C
               RIN(INWR+I+INST) = 
     &             SNGLCIRCEWRD(INDAT(I+IST+256-38))/BINTIME
            ENDDO
            DCLEV(ICH+1) = DCLEV(ICH+1)/BINTIME
            DCLON(ICH+1) = DCLON(ICH+1)/BINTIME
            DCLOFF(ICH+1)= DCLOFF(ICH+1)/BINTIME
            SIGMA(ICH+1) = SIGMA(ICH+1)/BINTIME
         ENDDO
C
      ELSEIF ( DATATYPE.EQ.3 ) THEN
C
C DATATYPE 3 - SPECIAL      - 24-BIT FLOATING POINT
C
         CALL PSRERR ( 'RDDAT',39,0,0.,'Reading of datatype 3 is' )
         IFAIL = 39
         NDAT = 0
      ELSEIF ( DATATYPE.EQ.4 ) THEN
C
C DATATYPE 4 - SEARCH       -  3-BIT INTEGER
C
         NW = NBIN/8
         DO 50 ICH=0,NCH-1
            IST = NW*(ICORIG(ICH+1)-1)
            ICHST = LENCHAN*ICH
            DO 60 I=1,NW
               K = INWR+I*8+ICHST
               DO 70 J=0,7
                  RIN(K-J) = JIBITS(INDAT(I+IST),J*3,3)
   70          CONTINUE
   60       CONTINUE
   50    CONTINUE
      ELSE
         CALL PSRERR ( 'RDDAT',50,DATATYPE,0.0,' ' )
         IFAIL = 50
         NBIN = 0
         RETURN
      ENDIF
C
C Update the data write pointer
C
      INWR = INWR+NBIN
      RETURN
C
C END OF SUBROUTINE RDDAT
C
      END
*DECK RDASPDAT
C
C *************************************************************
      SUBROUTINE RDASPDAT ( IFAIL )
C *************************************************************
C
C Reads data from the working space assuming that it contains
C     a header followed by data from a previous call of RDASPHEAD
C     giving four bytes of array in for each data bin.
C The first data bin is loaded into the element pointed to by INWR
C     and INWR is updated at the end of the read
C IFAIL=0 if no error has occurred.
C
C THIS ROUTINE IS INSTALLATION DEPENDENT.
C
C VAX-11 FORTRAN VERSION.
C
      INCLUDE 'PSRLIB.DEF'
      INCLUDE 'PSRDAT.DEF'
C

c$$$      INTEGER MAXWORK
c$$$      PARAMETER (MAXWORK=64000)
      PARAMETER (LENIND=MAXWORK-100)
      INTEGER INDAT(LENIND)
c      REAL WRK(MAXWORK)
      EQUIVALENCE (WRK(101),INDAT)
C
      IFAIL = 0
C
      IF ( NCH.GT.MAXCHAN ) THEN
         CALL OUTMON (
     &   ' ***** No. of input data channels exceeds array space' )
         CALL OUTMON ( '       Number of channels truncated' )
         NCH = MAXCHAN
      ENDIF
C
C Save original number of channels
C
      NCORIG = NCH
C
C Ascertain the number of channels to be read
C
      NCH = 0
      DO I=1,NCORIG
         IF ( ICCURR(I).GT.0 ) NCH = NCH+1
      ENDDO
C
C Check number of channels
C
      IF ( NCH.EQ.0 ) THEN
         IFAIL = 65
         CALL PSRERR ('RDASPDAT',IFAIL,0,0.0,' ')
         RETURN
      ENDIF
C
C Check that the length of the data array has been set
C
      IF ( LENDAT.EQ.0 ) LENDAT = MAXIN
C
C If the number of channels has changed, reset the data
C        storage control variables
C
      IF ( LENDAT/NCH.NE.LENCHAN ) THEN
         LENCHAN = LENDAT/NCH
         INRD = 0
         INWR = 0
      ENDIF
C
C Check for room in data array
C
      IF ( INWR+NBIN.GT.LENCHAN ) THEN
         IFAIL = 41
         CALL PSRERR ('RDASPDAT',IFAIL,INWR+NBIN,0.,'Data')
         RETURN
      ENDIF
C
C
C
         DO 10 ICH=0,NCH-1
            IST = ICORIG(ICH+1) - 1
            INST = LENCHAN*ICH
            DO 20 I=1,NBIN
               RIN(INWR+I+INST) = JIBITS(INDAT(I),IST,1)
   20       CONTINUE
   10    CONTINUE
C
C Update the data write pointer
C
      INWR = INWR+NBIN
      RETURN
C
C END OF SUBROUTINE RDASPDAT
C
      END
