C
C ***********************************************************
      LOGICAL FUNCTION RDHEAD ( LU,IFAIL )
C ***********************************************************
C
C Reads character data from logical unit LU into working
C     space and interprets it as pulsar data. If LARTHUR is true the
C     data is treated as if having come from ARTHUR else it is assumed 
C     to be in magnetic tape format.
C     returning the header of the next data block in the variables
C     declared in 'PSRDAT.DEF'.
C If unable to interpret the data it calls RDASPHEAD in order to
C     see if it is aspdata.
C The function is .TRUE. unless end-of-file is encountered,
C     or there is an error
C IFAIL is zero unless an error occurred.
C
C This routine is installation dependent.
C
C VAX-11 FORTRAN VERSION.
C
c Mod 10/03/2006 Correct data type in call to pserr
c
      INCLUDE 'PSRLIB.DEF'
      INCLUDE 'PSRDAT.DEF'

C
      INTEGER IBUF(MAXWORK),INDAT(MAXWORK),IC(3),ILOOP
      BYTE BBUF(MAXWORK*4),INBYTE(MAXWORK*4)
      REAL*8 ARTDAT
      EQUIVALENCE (WRK,INDAT,INBYTE),(IBUF,BBUF)
      CHARACTER OUTBUF*81,CJTIME*24
      CHARACTER*132 CTMP
      DIMENSION FRQCODE(8)
      REAL*8 DBLCIRCEWRD
      LOGICAL SYNCERR,RDASPHEAD,RDARTHURHD,L7_TRACK
      DATA FRQCODE  / 0.,240.,327.,408.,610.,928.,1420.,1667. /
      DATA IC / 64,4096,262144 /
C
C
C Clear error flags.
C
      IFAIL = 0
      RDHEAD = .FALSE.
      SYNCHERR= .FALSE.
      LASPDAT= .FALSE.
C
C Test if single pulse data and more data to read from this block.
C
      if(nwords.gt.nwdat) then
        NWORDS = NWORDS - NWDAT
        IF (NWORDS.GE.NWDAT.AND.NWORDS.NE.0) THEN
          DO I=1,NWORDS
            INDAT(I) = INDAT(I+NWDAT)
          ENDDO
          GOTO 47
        ENDIF
      ENDIF      
C
      ILOOP = 0
C
C Label 10 is the start of the synchronization search loop.
C
   10 CONTINUE
C Read a block into a buffer
C
      READ (LU,300,ERR=9999,END=9000,IOSTAT=ISTAT) 
     &      NCHARS,(BBUF(I),I=1,NCHARS)
      LUREC(LU) = LUREC(LU)+1
      ILOOP = ILOOP + 1
C
C Look for different possible synch sequences.
C
      IF (IBUF(2).EQ.'12552522525'O) THEN
C Asp data. transfer the data into INBYTE and call RDASPHEAD.
         DO I=1,NCHARS
            INBYTE(I) = BBUF(I)
         ENDDO
C
         IF (RDASPHEAD(NCHARS,IFAIL)) RDHEAD = .TRUE.
         LASPDAT = .TRUE.
         RETURN
C
      ELSEIF (BBUF(6)+256*BBUF(5)+65536*BBUF(4).EQ.-5659307) THEN
         L7_TRACK = .FALSE.
C 9-track data
C   Check that the number of characters is a multiple of three
C   i.e. that the data has an integral number of 24-bit words.
C
         ctmp=CJTIME(EPOCH+UTS/86400.0D0,3)
         IF ( MOD(NCHARS,3).NE.0 )
     &      CALL OUTMON (' 9-Track data block following '//
     &         ctmp(:length(ctmp))//
     &         ' has non-integral no of 3-byte words')
C
         NWORDS = NCHARS/3
C
C  Change the bytes from  CHAR1 , CHAR2 , CHAR3
C                     to  CHAR3 , CHAR2 , CHAR1 , 'ZERO'
C
         DO 40 I=1,NWORDS
            INBYTE(I*4-3) = BBUF(I*3)
            INBYTE(I*4-2) = BBUF(I*3-1)
            INBYTE(I*4-1) = BBUF(I*3-2)
            INBYTE(I*4) = 0
   40    CONTINUE
C
      ELSEIF (IBUF(2).EQ.353708586) THEN
         L7_TRACK = .TRUE.
C 7-track data
C
C Check that the number of characters is a multiple of four
C    i.e. that the data has an integral number of 24-bit words.
C
         ctmp=CJTIME(EPOCH+UTS/86400.0D0,3)
         IF ( MOD(NCHARS,4).NE.0 )
     &      CALL OUTMON (' 7-Track Data block following '//
     &         ctmp(:length(ctmp))//
     &         ' has non-integral no of 4-byte words')
         NWORDS = NCHARS/4
C
C Change the bytes from  CHAR1 , CHAR2 , CHAR3 , CHAR4
C                    to  CHAR4 , CHAR3 , CHAR2 , CHAR1
C
         DO 45 I=1,NWORDS
            INDAT(I) =       BBUF(I*4)   + IC(1)*BBUF(I*4-1)
     &               + IC(2)*BBUF(I*4-2) + IC(3)*BBUF(I*4-3)
   45    CONTINUE
C
      ELSE
C No synchronisation found
C Monitor an error before trying the next block.  
C   
         IF ( .NOT.SYNCERR ) THEN
            ctmp=CJTIME(EPOCH+UTS/86400.0D0,3)
            WRITE (OUTBUF,110)ctmp(:length(ctmp))   
     &          ,INDAT(1),INDAT(2)   
            CALL OUTMON(OUTBUF)  
            SYNCERR = .TRUE.
            L7_TRACK = .FALSE.
         ENDIF
C Too many tries at finding synchronisation
         IF (ILOOP.LT.6) THEN
           GOTO 10
         ELSE
           IFAIL = 51
           CALL PSRERR ('RDHEAD',IFAIL,0,0.0
     &   ,'Unable to synchronise - check file type')
           RETURN
         ENDIF
C         
      ENDIF
C
      SYNCERR = .FALSE.  
C
C Synch found. read header
C
      DATATYPE = INDAT(1)/2396745.
C
C Check if Datatype(2), 
C
   47 IF(DATATYPE.EQ.2)THEN
C
         UTS      = DBLCIRCEWRD (INDAT(5),INDAT(6))
         DEMRA    = DBLCIRCEWRD (INDAT(7),INDAT(8))
         DEMDEC   = DBLCIRCEWRD (INDAT(9),INDAT(10))
         PULSENO  = DBLCIRCEWRD (INDAT(11),INDAT(12))
         PB       = DBLCIRCEWRD (INDAT(13),INDAT(14))
         PBEPOCH  = DBLCIRCEWRD (INDAT(15),INDAT(16)) + 40000D0
         PBDOT    = 1D9/86400D0*DBLCIRCEWRD (INDAT(17),INDAT(18))
         DELAY    = DBLCIRCEWRD (INDAT(19),INDAT(20))
         TBIN     = DBLCIRCEWRD (INDAT(21),INDAT(22))
         DM       = DBLCIRCEWRD (INDAT(23),INDAT(24))
         SBS      = DBLCIRCEWRD (INDAT(25),INDAT(26))
         DDU      = MOD(SBS,1000)/100
         MEANANOM = DBLCIRCEWRD (INDAT(27),INDAT(28))
         POBS     = DBLCIRCEWRD (INDAT(29),INDAT(30))
         EPOCH    = DBLCIRCEWRD (INDAT(31),INDAT(32)) + 40000D0
         PDOT     = 1D9/86400D0*DBLCIRCEWRD (INDAT(33),INDAT(34))
         UTF      = DBLCIRCEWRD (INDAT(35),INDAT(36))
         PA       = DBLCIRCEWRD (INDAT(37),INDAT(38))
         DDUFREQ  = DBLCIRCEWRD (INDAT(39),INDAT(40))
         DO I=1,5
            CLOCK(I) = DBLCIRCEWRD (INDAT(39+2*I),INDAT(40+2*I))
            IF (I.NE.1) CLOCK(I) = 1D-6*CLOCK(I)
         ENDDO
         RA       = DBLCIRCEWRD (INDAT(51),INDAT(52))
         DEC      = DBLCIRCEWRD (INDAT(53),INDAT(54))
         LONG     = DBLCIRCEWRD (INDAT(55),INDAT(56))
         LAT      = DBLCIRCEWRD (INDAT(57),INDAT(58))
         AZ       = DBLCIRCEWRD (INDAT(59),INDAT(60))
         EL       = DBLCIRCEWRD (INDAT(61),INDAT(62))
         NBIN     = DBLCIRCEWRD (INDAT(63),INDAT(64))
         NCH      = DBLCIRCEWRD (INDAT(65),INDAT(66))
         ITELNO   = DBLCIRCEWRD (INDAT(67),INDAT(68))
         DO I=1,NCH
            TRES(I)   = DBLCIRCEWRD(INDAT(79+2*I),INDAT(80+2*I))
            DCLEV(I)  = DBLCIRCEWRD(INDAT(95+2*I),INDAT(96+2*I))
            DCLON(I)  = DBLCIRCEWRD(INDAT(111+2*I),INDAT(112+2*I))
            DCLOFF(I) = DBLCIRCEWRD(INDAT(127+2*I),INDAT(128+2*I))
            SIGMA(I)  = DBLCIRCEWRD(INDAT(143+2*I),INDAT(144+2*I))
            NP(I)     = DBLCIRCEWRD(INDAT(159+2*I),INDAT(160+2*I))
            NPON(I)   = DBLCIRCEWRD(INDAT(175+2*I),INDAT(176+2*I))
            NPOFF(I)  = DBLCIRCEWRD(INDAT(191+2*I),INDAT(192+2*I))
            ICCURR(I) = I
            CPOL(I)   = 'I'
            FOBS(I)   = DBLCIRCEWRD(INDAT(207+2*I),INDAT(208+2*I))
         ENDDO
C
         NPPH     = 1
         CAL      = SBS/10000.EQ.1
         NPULSES  = NP(1)
         OBSVERS  = 0.0
C
      ELSE
C
         PULSENO  = INDAT(3)
         UTS      = INDAT(4)/4. + INDAT(5)/8388608.0D0
         DEMRA    = INDAT(6)/46603.3778
         DEMDEC   = INDAT(7)/46603.3778
         IF ( DEMDEC.GT.180. ) DEMDEC = DEMDEC-360.
         PB       = (INDAT(8)+INDAT(9)/8388608.0D0)*1.D-6
         PBEPOCH  = INDAT(10)+40000.0D0
         PBDOT    = INDAT(11)/8388608./86400.E-9
         DELAY    = INDAT(12)*1.E-6
         TBIN     = INDAT(13)*0.25E-6
         DM       = INDAT(14)/4096.
         SBS      = INDAT(15)
         DDU      = MOD(SBS,1000)/100
         DDUFREQ  = FRQCODE(1+MOD(SBS,10000)/1000)
         CAL      = SBS/10000.EQ.1
         MEANANOM = INDAT(16)/46603.3778
         POBS     = (INDAT(17)+INDAT(18)/8388608.0D0)*1.D-6
         EPOCH    = INDAT(19)+40000.0D0
         DO WHILE ( UTS.GE.86400.0D0 )
            UTS   = UTS - 86400.0
            EPOCH = EPOCH + 1.0D0
         ENDDO
         PDOT     = INDAT(20)/8388608./86400.E-9
         UTF      = INDAT(21)/4.
         PA       = INDAT(22)/46603.3778
         RA       = INDAT(23)/46603.3778
         DEC      = INDAT(24)/46603.3778
         IF ( DEC.GT.180. ) DEC = DEC-360.
         LONG     = INDAT(25)/46603.3778
         LAT      = INDAT(26)/46603.3778
         IF ( LAT.GT.180. ) LAT = LAT-360.
         AZ       = INDAT(27)/46603.3778
         EL       = INDAT(28)/46603.3778
         IF ( EL.GT.180. ) EL = EL-360.
         CLOCK(1) = INDAT(29)/4.
         DO 50 I=2,5
            CLOCK(I) = INDAT(28+I)/1E6
   50    CONTINUE
         NPULSES  = INDAT(34)
         ITELNO   = MOD(INDAT(36),16)
         IPMULT   = MAX(1,INDAT(36)/16)
         OBSVERS  = INDAT(37)
         NBIN     = INDAT(38)/4096
         NPPH     = MAX(1,MOD(INDAT(38)/16,256))
         NCH      = MOD(INDAT(38),16)
C Set the period equal to the block length if zero (search data).
         IF (POBS.LT.1E-9) THEN
            POBS = NBIN*TBIN
            NPULSES = 1
         ENDIF
C
C Set some version dependent variables and other fixes
C
         IF (OBSVERS.GT.3.5) DDUFREQ = INDAT(35)
         IF (OBSVERS.EQ.0.AND.DATATYPE.NE.4) THEN
            TBIN = 4.*TBIN
            NPULSES = 1
         ENDIF
         IF (OBSVERS.LT.6.5) PMULT = 1
C
C Set the channel dependent parameters
C
         DO I=1,NCH
            ICCURR(I) = I
            TRES(I)   = TBIN
            DCLEV(I)  = 0.0
            SIGMA(I)  = 0.0
            FOBS(I)   = -1.0
            CPOL(I)   = 'I'
            NP(I)     = NPULSES
         ENDDO
C
C For search data 'NBIN' is half no. of actual data bins - double it
C
         IF ( DATATYPE.EQ.4 ) NBIN = NBIN*2
C
      ENDIF
C
C
C Compute the number of words expected for the data block
C and read continuation blocks if required
C
      IF ( DATATYPE.EQ.0 ) THEN
         NWDAT = 38+NBIN*NCH*NPPH/2
      ELSEIF ( DATATYPE.EQ.1 ) THEN
         NWDAT = 38+NBIN*NCH*NPPH
      ELSEIF ( DATATYPE.EQ.2.OR.DATATYPE.EQ.3 ) THEN
         NWDAT = 256+NBIN*NCH
      ELSEIF ( DATATYPE.EQ.4 ) THEN
         NWDAT = 38+NBIN*NCH/8
      ELSE
         IFAIL = 50
         CALL PSRERR ( 'RDHEAD',IFAIL,DATATYPE,0.0,' ' )
         RETURN
      ENDIF
C
C Label 20 is the start of the continuation block reading loop
C
   20 CONTINUE
      IF ( NWORDS.GE.NWDAT ) GOTO 30
C
C Check that the present block is the correct size
      IF ( MOD(NWORDS,1023).NE.0.AND.MOD(NWORDS,256).NE.0 ) THEN
        CALL OUTMON (' Data block '//
     &         CJTIME(EPOCH+UTS/86400.0D0,3)//
     &         ' has wrong number of words')
        IFAIL = 51
        RETURN
      ENDIF
C
      IF ( .NOT.L7_TRACK ) THEN
         READ (LU,100,ERR=9999,END=9000,IOSTAT=ISTAT) NCHARS
     &        ,(INDAT(NWORDS+I),I=1,NCHARS/3)
         LUREC(LU) = LUREC(LU)+1
         IF ( MOD(NCHARS,3).NE.0 ) THEN
           CALL OUTMON (' Continuation block '//
     &         CJTIME(EPOCH+UTS/86400.0D0,3)//
     &         ' has non-integral no of 3-byte words')
           IFAIL = 51
           RETURN
         ENDIF
C
C Check number of words against array space
C
         IF ( NWORDS+NCHARS/3.GT.MAXWORK ) THEN
            CALL OUTMON (' Data block '//
     &         CJTIME(EPOCH+UTS/86400.0D0,3)//
     &         ' is too large.')
            WRITE (OUTBUF,120) DATATYPE,NCH,NBIN,NWORDS
            CALL OUTMON(OUTBUF)
            IFAIL = 41
            CALL PSRERR ( 'RDHEAD',IFAIL,NWORDS+NCHARS/3,0.0,'Working' )
            RETURN
         ENDIF
C
C Juggle the bytes and update number of words
C
         DO 60 I=NWORDS+1,NWORDS+NCHARS/3
            INDAT(I) = JISHFTC(INDAT(I),16,32)
            INBYTE(I*4-2) = INBYTE(I*4)
            INBYTE(I*4) = 0
   60    CONTINUE
         NWORDS = NWORDS+NCHARS/3
C
      ELSE
C        7-TRACK DATA
         READ (LU,200,ERR=9999,END=9000,IOSTAT=ISTAT) NCHARS
     &        ,(INDAT(NWORDS+I),I=1,NCHARS/4)
         LUREC(LU) = LUREC(LU)+1
         IF ( MOD(NCHARS,4).NE.0 ) THEN
           CALL OUTMON (' Continuation block '//
     &         CJTIME(EPOCH+UTS/86400.0D0,3)//
     &         ' has non-integral no of 4-byte words')
           IFAIL = 51
           RETURN
         ENDIF
C
C Check number of words against array space
C
         IF ( NWORDS+NCHARS/4.GT.MAXWORK ) THEN
            CALL OUTMON (' Data block '//
     &         CJTIME(EPOCH+UTS/86400.0D0,3)//
     &         ' is too large.')
            WRITE (OUTBUF,120) DATATYPE,NCH,NBIN,NWORDS
            CALL OUTMON(OUTBUF)
            IFAIL = 41
            CALL PSRERR ( 'RDHEAD',IFAIL,NWORDS+NCHARS/4,0.0,'Working')
            RETURN
         ENDIF
C
C Juggle the bytes and update number of words
C
         DO 70 I=NWORDS+1,NWORDS+NCHARS/4
            INDAT(I) =       INBYTE(I*4)   + IC(1)*INBYTE(I*4-1)
     &               + IC(2)*INBYTE(I*4-2) + IC(3)*INBYTE(I*4-3)
   70    CONTINUE
         NWORDS = NWORDS+NCHARS/4
C
      ENDIF
C
      GOTO 20
C
C Label 30 is the exit point from the reading loop
C
   30 CONTINUE
C
C Reading successful, set function name and exit.
C
      RDHEAD = .TRUE.
      RETURN
C
C END-OF-FILE encountered.
C
 9000 CONTINUE
      RETURN
C
C Error in read.
C
 9999 CONTINUE
      IFAIL = 47
      CALL PSRERR ( 'RDHEAD',IFAIL,ISTAT,0.0,' ' )
      RETURN
C
  100 FORMAT ( Q,2731A3 )
  200 FORMAT ( Q,1024A4 )
  300 FORMAT ( Q,8192A1 )
  110 FORMAT ( ' *** Synchronization error at ',A,2Z10.8 )
  120 FORMAT ( ' Datatype ',I2,' , Nbins ',I4,' , Nchannels',I2
     &        ,' , Nwords ',I6 )
C
C End of logical function rdhead
C
      END
*DECK RDASPHEAD
C
C ***********************************************************
      LOGICAL FUNCTION RDASPHEAD ( NCHARS,IFAIL )
C ***********************************************************
C
C 
C Takes data in INDAT and interprets it as pulsar data in aspdata format
C     returning the header of the next data block in the variables
C     declared in 'PSRDAT.DEF'.
C The function is .TRUE. unless End-of-file is encountered,
C     or there is an error
C IFAIL is zero unless an error occurred.
C
C THIS ROUTINE IS INSTALLATION DEPENDENT.
C
C VAX-11 FORTRAN VERSION.
C
      INCLUDE 'PSRLIB.DEF'
      INCLUDE 'PSRDAT.DEF'
C
      INTEGER INDAT(MAXWORK)
      BYTE INBYTE(MAXWORK*4)
      EQUIVALENCE (WRK,INDAT,INBYTE)
      CHARACTER OUTBUF*81,CJTIME*24
      DIMENSION FRQCODE(8)
      LOGICAL BATCH   
      PARAMETER(DPI=3.1415926540897932D0)
      DATA FRQCODE  / 151.,240.,327.,408.,610.,960.,1420.,1666. /
C
C Clear error flags.
C
      IFAIL = 0
      RDASPHEAD = .FALSE.
C
C Data is already in INDAT, provided by RDHEAD.
C
C Check that the number of characters is a multiple of four
C  i.e. that the data has an integral number of 32-bit words.
C
      IF ( MOD(NCHARS,4).NE.0 )
     &      CALL OUTMON (' Data block following '//
     &         CJTIME(EPOCH+UTS/86400.0D0,3)//
     &         ' has non-integral no of 4-byte words')
      NWORDS = NCHARS/4
C
C Synch found. read header
C
      DATATYPE = IAND(INDAT(1),'77777777'O)/2396745.
      PULSENO  = INDAT(3)
      UTS      = INDAT(4) + INDAT(5)*1.0E-6
      DEMRA    = INDAT(6)/3600.
      DEMDEC   = INDAT(7)/3600.
      IF ( DEMDEC.GT.180. ) DEMDEC = DEMDEC-360.
      PB       = INDAT(8)*1.0E-6 + INDAT(9)*1.E-9
      PBEPOCH  = INDAT(10) - 1
      PBDOT    = INDAT(11)/8388608./86400.E-9
      DELAY    = INDAT(12)*1.E-6
      TBIN     = INDAT(13)*1.0E-6
      DM       = INDAT(14)*1.0E-3
      SBS      = INDAT(15)
      DDU      = MOD(SBS,1000)/100
      DDUFREQ  = FRQCODE(1+MOD(SBS,10000)/1000)
      CAL      = SBS/10000.EQ.1
      MEANANOM = INDAT(16)/46603.3778
      POBS     = INDAT(17)*1.0E-6 + INDAT(18)*1.E-9
      EPOCH    = INDAT(19) - 1
      IF ( UTS.GE.86400.0 ) THEN
         UTS = UTS-86400.0
         EPOCH = EPOCH+1
      ENDIF
      PDOT     = INDAT(20)/8388608./86400.E-9
      UTF      = INDAT(21)
      PA       = INDAT(22)/3600.
      RA       = INDAT(23)/3600.
      DEC      = INDAT(24)/3600.
      IF ( DEC.GT.180. ) DEC = DEC-360.
      LONG     = INDAT(25)/3600.
      LAT      = INDAT(26)/3600.
      IF ( LAT.GT.180. ) LAT = LAT-360.
      AZ       = INDAT(27)/3600.
      EL       = INDAT(28)/3600.
      IF ( EL.GT.180. ) EL = EL-360.
C
      CALL GETCOORD(DBLE(AZ),DBLE(EL),EPOCH,DBLE(UTS),RA,DEC)
C
      IF(AZ.LT.0.0)AZ=AZ+360.0
      DO 50 I=1,5
         CLOCK(I) = INDAT(28+I)
   50 CONTINUE
      NPULSES  = INDAT(34)
      ITELNO   = MOD(INDAT(36),16)
      IPMULT   = MAX(1,INDAT(36)/16)
      OBSVERS  = INDAT(37)
      NCH      = INDAT(38)
      NPPH     = 1
      NBIN     = INDAT(39)
C Set the period equal to the block length if zero (search data).
      IF (POBS.LT.1E-9) THEN
         POBS = NBIN*TBIN
         NPULSES = 1
      ENDIF
C
C Set the channel dependent parameters
C
      DO I=1,NCH
         ICCURR(I) = I
         TRES(I)   = TBIN
         DCLEV(I)  = 0.0
         SIGMA(I)  = 0.0
         FOBS(I)   = -1.0
         CPOL(I)   = 'I'
         NP(I)     = NPULSES
      ENDDO
C
C Check that the tape block is not too large for the working space
C
      IF ( NWORDS.GT.MAXWORK ) THEN
          CALL PSRERR ('RDASPHEAD',41,NWORDS,0.0,'WORKING')
          CALL OUTMON (
     &      '*** WARNING : PROGRAM MAY HAVE BEEN CORRUPTED ***' )
          IFAIL = 41
          RETURN
      ENDIF
C
C Reading successful, set function name and exit.
C
      RDASPHEAD = .TRUE.
      RETURN
C
C     END-OF-FILE ENCOUNTERED.
C
 9000 CONTINUE
      RETURN
C
C     ERROR IN READ.
C
 9999 CONTINUE
      IFAIL = 47
      CALL PSRERR ( 'RDASPHEAD',IFAIL,ISTAT,0.0,' ' )
      RETURN
C
  100 FORMAT ( Q,2048A4 )
  110 FORMAT ( ' *** Synchronization error at ',A,2Z10.8 )
  120 FORMAT ( ' Datatype ',I2,' , Nbins ',I4,' , Nchannels',I2
     &        ,' , Nwords ',I6 )
C
C END OF LOGICAL FUNCTION RDASPHEAD
C
      END
C
C
C ***********************************************************
      DOUBLE PRECISION FUNCTION DBLCIRCEWRD(II,JJ)
C ***********************************************************
C
C A routine to convert a CIRCE double precision word of 48 bits
C   to its VAX equivalent. The byte swopping has already been done.
C
      INTEGER II,JJ
      DOUBLE PRECISION DMAN
C
C Extract both parts of the mantissa
C
      DMAN = II*2D0**16 + JJ/256
C
C Extract the exponent
C
      IEXP = IAND (JJ, '377'O)
C
C Prevent overflow
C
      IF (DMAN.LE.1.0D-15)THEN
         DBLCIRCEWRD = 0.0D0
      ELSE
C
C Sign extend
C
         IF (II.GE.2**23)DMAN = DMAN-1.09951163D12
C
C Scale the exponent
C
         IF (IEXP.GE.'200'O)IEXP = IEXP-'400'O
C
C Prevent overflow
C
         IEXP = MAX(IEXP,-87)
C
C Scale the mantissa by the exponent
C
         DBLCIRCEWRD = DMAN*2.0D0**(IEXP-39)
      ENDIF 
C
      END
C

C
C ***********************************************************
      REAL FUNCTION SNGLCIRCEWRD(II)
C ***********************************************************
C
C A routine to convert a CIRCE single precision word of 24 bits
C   to its VAX equivalent. The byte swopping has already been done.
C
      INTEGER II
C
C Extract and scale the exponent
C
      IEXP = IAND(II,'377'O)
      IF(IEXP.GE.'200'O) IEXP=IEXP-'400'O
C
C Prevent overflow
C
      IEXP = MAX(IEXP,-111)
C
C Extract the mantissa
C
      RMAN = INT(II/256)
      IF(RMAN.EQ.0.0)THEN
         SNGLCIRCEWRD = 0.0
      ELSE
C
C Sign extend
C
         IF(RMAN.GE.2.0**15) RMAN=RMAN-2.0**16
C
C Scale the mantissa by the exponent
C
         SNGLCIRCEWRD = RMAN*2.0**(IEXP-15)
      ENDIF
C
      END
C

