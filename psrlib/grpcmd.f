*DECK GRPCMD                    
      SUBROUTINE GRPCMD( CMDINP, NINP, JINP 
     &    ,PARLST ,PARDSC ,PARID
     &    ,IPAR ,RPAR ,DPAR ,LPAR ,CPAR
     &    ,IIUNITS ,IRUNITS ,IDUNITS ,IFAIL )


C Routine to parse the graphics command 

C expects to find the following parameters in the parameter list
C 'GRDEVICE'  = pgplot terminal device
C 'HARDDEV' = pgplot hardcopy device
C 'COL_IDX' = pgplot colour index
C 'LIN_WID' = pgplot line width
C 'LIN_STY' = pgplot line style
C 'FONT'    = pgplot font
C 'CHR_HT'  = pgplot character height
C 'PG_ASK'  = pgplot page prompting flag
C 'XPIC' = the number of x divisions of the plotting surface
C 'YPIC' = the number of y divisions of the plotting surface
c
c Mod 10/03/2006 Correct data type in call to pserr
c

      INTEGER NSTDPARS,NGRAFCMDS
      PARAMETER(NSTDPARS=10,NGRAFCMDS=15)
      CHARACTER*(*) CMDINP(*),PARLST(*),PARDSC(*),CPAR(*),PARID(*),
     &   STDPARS(NSTDPARS+1)*12,GRAFCMDS(NGRAFCMDS+1)*12,
     &   GRAFCMDDSC(NGRAFCMDS*2+2)*40,CHARDUMM(2)*1/' ',' '/,
     &   PGANS*80,OUTBUF*80,TEXT*132,SIDE*2,CH*1

      INTEGER IPAR(*),IIUNITS(*),IRUNITS(*),IDUNITS(*),XPIC,YPIC,
     &   ILENPGANS

      REAL RPAR(*),DUMMY(6),X,Y
      DOUBLE PRECISION DPAR(*)

      LOGICAL CALLED/.FALSE./

      data stdpars/'grdevice','harddev','col_idx','lin_wid','lin_sty',
     &   'font','chr_ht','pg_ask','xpic','ypic',' '/

      data grafcmds/'set','show','begin','end','ask','update',
     &   'curse','advance','paper','vport','vsize','hardcopy',
     &   'vstand','mtext','text',
     & ' '/

      DATA GRAFCMDDSC/
     &   'Set a graphics parameter',' ',
     &   'Show something',' ',
     &   'Open a plotting device',' ',
     &   'close a plotting device',' ',
     &   'Change the new page prompting',' ',
     &   'Flush PGPLOT buffers',' ',
     &   'Set up graphics Cursor',' ',
     &   'Clear a plotting page',' ',
     &   'Change the size of the Viewing',' Surface',
     &   'set up viewport NDC',' ',
     &   'set up viewport (inches)',' ',
     &   'Obtain a Hardcopy of any plot files',' ',
     &   'set up standard viewport',' ',
     &   'write text relative to viewport',' ',
     &   'write text anywhere',' ',
     &   ' ',' '/
     

C COMPOS  stores the position of the  listed parameters in the parid 
C    file

      INTEGER COMPOS(NSTDPARS,2)
      LOGICAL LPAR(*),COMSTR,PARINT,PAREAL,PARDBL,PARSWT,PARARG

C search for each of the parameters if this is the first call

      IF(.NOT.called) THEN
        CALLED=.TRUE.
        DO I=1,NSTDPARS
          COMPOS(I,1)=INTCMD(PARLST,STDPARS(I))
          IF(COMPOS(I,1).GT.0) THEN
            READ(PARID(COMPOS(I,1)),'(X,I2)') COMPOS(I,2)
          ELSE
            WRITE(*,'(X,A)') 'THE STANDARD GRAPHICS PARAMETERS ARE NOT'
     &         //' ALL PRESENT'
            WRITE(*,'(X,A)') STDPARS(I)
            CALL PSRERR('GRPCMD',7,0,0.,' ')
          ENDIF

        ENDDO
      ENDIF

      IF (JINP.LE.NINP) THEN
        ICMDGRP=INTCMD(GRAFCMDS,CMDINP(JINP))
        IF(ICMDGRP.GT.0) THEN
          JINP=JINP+1
          GOTO (1001,1002,1003,1004,1005,1006,1007,1008,1009,
     &          1010,1011,1012,1013,1014,1015),ICMDGRP

C ---SET----------------------------------------------------------------
 1001     CALL SETCMD ( CMDINP,NINP,JINP
     &              ,PARLST,PARDSC,PARID
     &              ,IPAR,RPAR,DPAR,LPAR,CPAR
     &              ,IIUNITS,IRUNITS,IDUNITS,IFAIL )
          CALL PGQINF('STATE',PGANS,ILENPGANS)
          IF(PGANS(:ILENPGANS).EQ.'OPEN') THEN
             CALL PGSCI(IPAR(COMPOS(3,2)))
             CALL PGSLW(IPAR(COMPOS(4,2)))
             CALL PGSLS(IPAR(COMPOS(5,2)))
             CALL PGSCF(IPAR(COMPOS(6,2)))
             CALL PGSCH(RPAR(COMPOS(7,2)))
             CAll PGASK(LPAR(COMPOS(8,2)))
          ENDIF  

          GOTO 2000

C---SHOW---------------------------------------------------------------
 1002     CALL SHWCMD ( CMDINP,NINP,JINP,'GRAPHICS'
     &             ,CHARDUMM,CHARDUMM,GRAFCMDS,GRAFCMDDSC
     &             ,PARLST,PARDSC,PARID
     &             ,IPAR,RPAR,DPAR,LPAR,CPAR
     &             ,IIUNITS,IRUNITS,IDUNITS,IFAIL )
          GOTO 2000

C---BEGIN--------------------------------------------------------------
 1003     IF(JINP.LE.NINP) THEN
             CPAR(COMPOS(1,2))=CMDINP(JINP)
             JINP=JINP+1
             IF(JINP.LE.NINP) THEN           
               IF(PARINT(CMDINP(JINP),IPAR(COMPOS(9,2)))) THEN
                 JINP=JINP+1
                 IF(JINP.LE.NINP) THEN
                   IF(PARINT(CMDINP(JINP),IPAR(COMPOS(10,2)))) THEN
                     JINP=JINP+1
                     IFAIL=0
                   ELSE                
                     IFAIL=1
                   ENDIF
                 ENDIF
               ELSE
                 IFAIL=1
               ENDIF
             ENDIF
           ENDIF
           IF(IFAIL.EQ.0) THEN
             CALL PGBEGIN(0,CPAR(COMPOS(1,2)),IPAR(COMPOS(9,2))
     &               ,IPAR(COMPOS(10,2)))
             CALL PGSCI(IPAR(COMPOS(3,2)))
             CALL PGSLW(IPAR(COMPOS(4,2)))
             CALL PGSLS(IPAR(COMPOS(5,2)))
             CALL PGSCF(IPAR(COMPOS(6,2)))
             CALL PGSCH(RPAR(COMPOS(7,2)))
             CAll PGASK(LPAR(COMPOS(8,2)))
             call pgvstand
           ENDIF
           GOTO 2000

C--- END ---------------------------------------------------------------
 1004      CALL PGEND
           GOTO 2000

C--- ASK ---------------------------------------------------------------
 1005      IF(JINP.LE.NINP) THEN
             IF(PARSWT(CMDINP(JINP),LPAR(COMPOS(8,2)))) THEN
               JINP=JINP+1
               CALL PGASK(LPAR(COMPOS(8,2)))
             ENDIF
           ENDIF
           GOTO 2000

C--- UPDATE ------------------------------------------------------------
 1006      IF(JINP.LE.NINP) THEN
             IF(PARINT(CMDINP(JINP),ITEMP)) THEN
               JINP=JINP+1
               CALL PGUPDT(ITEMP)
             ENDIF
           ENDIF
           GOTO 2000

C--- CURSE -------------------------------------------------------------
 1007      CALL PGCURSE(X,Y,CH)
           WRITE(OUTBUF,*)'X=',X,'Y=',Y
           CALL OUTMON(OUTBUF)
           GOTO 2000

C--- ADVANCE -----------------------------------------------------------
 1008      CALL PGADVANCE
           GOTO 2000

C--- PAPER -------------------------------------------------------------
 1009      IF(PARARG(JINP,NINP,CMDINP,2,DUMMY,IFAIL)) THEN
             CALL PGPAPER(DUMMY(1),DUMMY(2),DUMMY(3),DUMMY(4))
           ENDIF
           GOTO 2000

C--- VPORT -------------------------------------------------------------
 1010      IF(PARARG(JINP,NINP,CMDINP,4,DUMMY,IFAIL)) THEN
             CALL PGVPORT(DUMMY(1),DUMMY(2),DUMMY(3),DUMMY(4))
           ENDIF
           GOTO 2000

C--- VSIZE -------------------------------------------------------------
 1011      IF(PARARG(JINP,NINP,CMDINP,4,DUMMY,IFAIL)) THEN
             CALL PGVSIZE(DUMMY(1),DUMMY(2),DUMMY(3),DUMMY(4))
           ENDIF
           goto 2000


C--- HARDCOPY ----------------------------------------------------------
C N.B. dependent on the existence of PLOTALL psr utility

 1012        call pgend
             jinptemp=1
             ninptemp=1
             CALL OPSCMD('@PSRUTILDIR:PLOTALL',jinptemp,ninptemp)
c             CALL PGBEGIN(0,CPAR(COMPOS(1,2)),IPAR(COMPOS(9,2))
c     &               ,IPAR(COMPOS(10,2)))
c             CALL PGSCI(IPAR(COMPOS(3,2)))
c             CALL PGSLW(IPAR(COMPOS(4,2)))
c             CALL PGSLS(IPAR(COMPOS(5,2)))
c             CALL PGSCF(IPAR(COMPOS(6,2)))
c             CALL PGSCH(RPAR(COMPOS(7,2)))
c             CAll PGASK(LPAR(COMPOS(8,2)))
             goto 2000
     
 1013   call pgvstand
        GOTO 2000

 1014 IFAIL=1
      IF(JINP.LE.NINP) THEN
        SIDE=CMDINP(JINP)
        JINP=JINP+1
        IF(JINP.LE.NINP .AND. PAREAL(CMDINP(JINP),DISP)) THEN
          JINP=JINP+1
          IF(JINP.LE.NINP .AND. PAREAL(CMDINP(JINP),COORD)) THEN
            JINP=JINP+1
            IF(JINP.LE.NINP .AND. PAREAL(CMDINP(JINP),FJUST)) THEN
              JINP=JINP+1
              TEXT=CMDINP(JINP)
              JINP=JINP+1
              IFAIL=0
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      IF(IFAIL.EQ.0) CALL PGMTEXT(SIDE,DISP,COORD,FJUST,TEXT)
      GOTO 2000
   
 1015 IFAIL=1
      IF(JINP.LE.NINP.AND.PAREAL(CMDINP(JINP),X)) THEN
        JINP=JINP+1
        IF(JINP.LE.NINP .AND. PAREAL(CMDINP(JINP),Y)) THEN
          JINP=JINP+1
          TEXT=CMDINP(JINP)
          JINP=JINP+1
          IFAIL=0
        ENDIF
      ENDIF
      IF(IFAIL.EQ.0) CALL PGTEXT(X,Y,TEXT)

C a command that has been interpreted gets to here
 2000      CONTINUE
         ELSEIF(ICMDGRP.EQ.0) THEN
           IFAIL=5
           CALL PSRERR('GRPCMD',IFAIL,0,0.,CMDINP(JINP))
           JINP=JINP+1
         ELSEIF(ICMDGRP.LT.0) THEN
           IFAIL=6
           CALL PSRERR('GRPCMD',IFAIL,0,0,CMDINP(JINP))
           JINP=JINP+1
         ENDIF
       ENDIF           
      END


                                                     
