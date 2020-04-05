*DECK SHOARR
C
C ***************************************************************
      SUBROUTINE SHOARRS ( LU,PARS,NPAR,IPAR,AS,ISTR,N,IFAIL )
C ***************************************************************
C
C WRITES A SECTION OF THE SHORT ARRAY AS(N) STRIDE ISTR
C     TO LOGICAL UNIT LU.
C THE RANGE IS OBTAINED FROM THE COMMAND PARAMETERS FROM PARS(IPAR)
C     TO PARS(NPAR).
C THE COMMANDS ARE INTERPRETED BY PARRNG AND MAY BE
C        FROM ... AND/OR TO ...
C    OR ABBREVIATIONS THEREOF.
C IF THE START(END) IS MISSING 1(N) IS ASSUMED.
C
C THE OUTPUT IS FORMATTED WITH NPLINE NUMBERS PER LINE
C    AND IN INTERACTIVE MODE PAUSES FOR USER RESPONSE AFTER EVERY PAGE.
C
C ENTRY POINTS SHOARR (REAL ARRAY) AND SHOIAR (INTEGER)
C    ARE FOR DIFFERENT ARRAY TYPES.
C
C THIS ROUTINE IS INSTALLATION DEPENDENT.
C
C VAX-11 FORTRAN VERSION.
C
      INCLUDE 'PSRLIB.DEF'
      CHARACTER*(*) PARS(*)
      BYTE AS(*)
      REAL AA(*)
      INTEGER IA(*)
C
      CHARACTER OUTST*40,OUTBUF*80,FMT*40,PAUBUF*60,rd*80
      DOUBLE PRECISION FROM,TO
      LOGICAL TERMINL
C
      ITYPE = 1
      NPLINE = 10
      GOTO 1000
C
C **************************************************************
      ENTRY SHOARR ( LU,PARS,NPAR,IPAR,AA,ISTR,N,IFAIL )
C
C     ENTRY POINT FOR REAL ARRAY.
C
      ITYPE = 2
      NPLINE = 5
      GOTO 1000
C
C **************************************************************
      ENTRY SHOIAR ( LU,PARS,NPAR,IPAR,IA,ISTR,N,IFAIL )
C
C     ENTRY POINT FOR INTEGER ARRAY.
C
      ITYPE = 3
      NPLINE = 10
C
C     LABEL 1000 IS THE COMMON STARTING POINT FOR ALL ENTRIES.
C
 1000 CONTINUE
C
C     PARRNG DECODES THE START AND FINISH ELEMENTS.
C
      CALL PARRNG ( PARS,NPAR,IPAR,FROM,TO,FDATE,TDATE
     &              ,.TRUE.,.FALSE.,-1.0,IFAIL )
      IF ( IFAIL.NE.0 ) RETURN
C
C     CHECK THEM AGAINST THE ARRAY BOUNDS AND ENSURE THE NUMBER OF
C     ELEMENTS IS POSITIVE.
C
      IST  = MIN(N,MAX(1,INT(MAX(DBLE(-MAXINT()),FROM))))
      IFIN = MIN(N,MAX(1,INT(MIN(DBLE(MAXINT()),TO))))
      NOUT = MAX(1,IFIN-IST+1)
      IFIN = IST+NOUT-1
C
C     SET THE OUTPUT FORMAT AND THE PAUSE LINE.
C
      FMT = '(1X,I?,'' : '',I?)'
      CALL SETIFM (IFIN,FMT)
      CALL SETIFM (IFIN,FMT)
      PAUBUF = 'Press return to continue, CNTRL D to exit :'
C
C     COMPUTE THE NUMBER OF LINES TO BE WRITTEN.
C
      NLINES = NOUT/NPLINE
      IF ( NLINES*NPLINE.NE.NOUT ) NLINES=NLINES+1
C
C     SET THE PAGE SIZE.
C
      NPPAGE = ABS(LUPAGL(LU)) - 1
C
C     WRITE OUT THE ARRAY.
C
      CALL OUTPUT (LU,' ')
      DO 10 I=1,NLINES
         IS = IST+(I-1)*NPLINE
         IF = IS+NPLINE-1
         IF ( IF.GT.IFIN ) IF = IFIN
         WRITE (OUTST,FMT) IS,IF
         IF ( ITYPE.EQ.1 ) THEN
            WRITE (OUTBUF,111,IOSTAT=IERR)
     &            (AS(J),J=(IS-1)*ISTR+1,(IF-1)*ISTR+1,ISTR)
         ELSEIF ( ITYPE.EQ.2 ) THEN
            WRITE (OUTBUF,112,IOSTAT=IERR)
     &            (AA(J),J=(IS-1)*ISTR+1,(IF-1)*ISTR+1,ISTR)
         ELSEIF ( ITYPE.EQ.3 ) THEN
            WRITE (OUTBUF,113,IOSTAT=IERR)
     &            (IA(J),J=(IS-1)*ISTR+1,(IF-1)*ISTR+1,ISTR)
         ENDIF
         CALL OUTPUT (LU,OUTST(1:LENGTH(OUTST))//' '//OUTBUF)
C
C        IF LOGICAL UNIT LU IS WRITING TO A TERMINAL,
C        PAUSE FOR USER INTERVENTION EVERY NPPAGE LINES.
C
         IF ( TERMINL(0,'SYS$OUTPUT',npagld,nwidd).AND.
     &                              MOD(I,NPPAGE).EQ.0 ) THEN
            CALL OUTPUT (LU,PAUBUF)
            READ (LUCMD,99,ERR=2000,END=2000) rd
         ENDIF
   10 CONTINUE
C
C     LABEL 2000 IS THE LOOP EXIT POINT FOR USER INTERVENTION.
C
 2000 CONTINUE
      CALL OUTPUT (LU,' ')
      RETURN
C
C     FORMATS.
C
   99 format (a80)
  111 FORMAT ( 10(1X,I5) )
  112 FORMAT ( 5(1X,F11.3) )
  113 FORMAT ( 10(1X,I5) )
C
C END OF SUBROUTINE SHOARR.
C
      END
