C*******************************************************************
      SUBROUTINE GRP_HANDLER(RETURNED_STRING)
C*******************************************************************
C
C Uses the graphics cursor to select a range of residuals. 
C  If residuals have not been calculated or a parameter has
C  changed, will then calculate new residuals.
C
      INCLUDE 'PSRTIMDIR:PSRTIME.DEF'
C
      CHARACTER*(*) RETURNED_STRING
      INTEGER PGCURSE,LENGTH,I,IDD,IDDD,IBAT,IBATD,NCHANGE
      REAL*4 REAMAX,D,DLAST,XMAX1,XMIN1,YMIN1,YMAX1,XX1,XX2,YY1,YY2
      REAL*4 DDLAST
      REAL*4 XD,YD,XDL,YDL
      REAL*4 XMIN,XMAX,YMIN,YMAX
      REAL*8 DX,DY,ESTEP,PER,V
      INTEGER ISTEP,IRES,JINP,NINP,IFAIL
      CHARACTER ANS*1,VALUE*6 ,DUMMY*80, STRING1*25
      LOGICAL INIT_GRP,GRP
      COMMON/GRP_HND/INIT_GRP,GRP
C
      NCHANGE = 0
C
C Check if any parameters have changed
C
      DO I=1,NDPARS
        IF (DPAR(I).NE.OLDDPAR(I)) NCHANGE = NCHANGE + 1
      ENDDO
C
C Check whether PGPLOT has been opened
C
      CALL PGQINF ('STATE',VALUE,LENGTH)
      IF (VALUE(:1).EQ.'C'.OR.NCHANGE.NE.0) THEN
        DUMMY=' '
        CALL PLTCMD (DUMMY,JINP,NINP,IFAIL)
      ENDIF
C
C Now get the cursor
C
   10 IF (PGCURSE (XD,YD,ANS).EQ.1) THEN
C Quit or Escape
        IF   (ANS.EQ.'Q'.OR.ANS.EQ.'q'
     &    .OR.ANS.EQ.'E'.OR.ANS.EQ.'e') THEN
          GRP = .FALSE.
          RETURNED_STRING=' '
          RETURN
        END IF
C REMEMBER LAST POINTS
          WRITE(STRING1,*)XDL+EPOCH
          CALL SYMADD('LXX',STRING1,1,IFAIL)
          WRITE(STRING1,*)YDL
          CALL SYMADD('LYY',STRING1,1,IFAIL)
          XDL=XD
          YDL=YD
          IF (IBAT.NE.0) THEN
            WRITE(STRING1,*)IBAT
            CALL SYMADD('LIBAT',STRING1,1,IFAIL)
            WRITE(STRING1,*)X(IDD)+EPOCH
            CALL SYMADD('LNX',STRING1,1,IFAIL)
            WRITE(STRING1,*)Y(IDD)*86400D3
            CALL SYMADD('LNY',STRING1,1,IFAIL)
          END IF
          IF (IBATD.NE.0) THEN
            WRITE(STRING1,*)IBATD
            CALL SYMADD('LIBATD',STRING1,1,IFAIL)
            WRITE(STRING1,*)X(IDDD)+EPOCH
            CALL SYMADD('LNXD',STRING1,1,IFAIL)
            WRITE(STRING1,*)Y(IDDD)*86400D3
            CALL SYMADD('LNYD',STRING1,1,IFAIL)
          END IF
C WORK OUT NEAREST BATS ETC
          CALL PGQWIN(XMIN,XMAX,YMIN,YMAX)
          DLAST = REAMAX()
          DO I=1,NFIT
            DX = (X(I)-XD)/(XMAX-XMIN)
            DY = (Y(I)*86400D3-YD)/(YMAX-YMIN)
            D  = SQRT(DX**2 + DY**2)
            IF ((D.LT.DLAST).AND.(SIGBAT(I).GT.0.0))THEN
              IDD = I
              DLAST = D
            ENDIF
            IF ((D.LT.DLAST).AND.(SIGBAT(I).LT.0.0))THEN
              IDDD = I
              DDLAST = D
            ENDIF
          ENDDO
          IF (IDD.NE.0) THEN
            IBAT = IBARES(IDD)
          ELSE
            IBAT = 0
          ENDIF
          IF (IDDD.NE.0) THEN
            IBATD = IBARES(IDDD)
          ELSE
            IBATD = 0
          ENDIF

          WRITE(STRING1,*)XD+EPOCH
          CALL SYMADD('XX',STRING1,1,IFAIL)
          WRITE(STRING1,*)YD
          CALL SYMADD('YY',STRING1,1,IFAIL)

          IF (IBAT.NE.0) THEN
            WRITE(STRING1,*)IBAT
            CALL SYMADD('IBAT',STRING1,1,IFAIL)
            WRITE(STRING1,*)X(IDD)+EPOCH
            CALL SYMADD('NX',STRING1,1,IFAIL)
            WRITE(STRING1,*)Y(IDD)*86400D3
            CALL SYMADD('NY',STRING1,1,IFAIL)
          END IF
          IF (IBATD.NE.0) THEN
            WRITE(STRING1,*)IBATD
            CALL SYMADD('IBATD',STRING1,1,IFAIL)
            WRITE(STRING1,*)X(IDDD)+EPOCH
            CALL SYMADD('NXD',STRING1,1,IFAIL)
            WRITE(STRING1,*)Y(IDDD)*86400D3
            CALL SYMADD('NYD',STRING1,1,IFAIL)
          END IF

C Plot again
        IF (ANS.EQ.'P'.OR.ANS.EQ.'p') THEN
          DUMMY = ' '
          CALL PLTCMD (DUMMY,JINP,NINP,IFAIL)
	  GOTO 10
C Execute Macro 'N'
        ELSEIF (ANS.EQ.'0') THEN
          RETURNED_STRING = 'MACRO0'
          RETURN
        ELSEIF (ANS.EQ.'1') THEN
          RETURNED_STRING = 'MACRO1'
          RETURN
        ELSEIF (ANS.EQ.'2') THEN
          RETURNED_STRING = 'MACRO2'
          RETURN
        ELSEIF (ANS.EQ.'3') THEN
          RETURNED_STRING = 'MACRO3'
          RETURN
        ELSEIF (ANS.EQ.'4') THEN
          RETURNED_STRING = 'MACRO4'
          RETURN
        ELSEIF (ANS.EQ.'5') THEN
          RETURNED_STRING = 'MACRO5'
          RETURN
        ELSEIF (ANS.EQ.'6') THEN
          RETURNED_STRING = 'MACRO6'
          RETURN
        ELSEIF (ANS.EQ.'7') THEN
          RETURNED_STRING = 'MACRO7'
          RETURN
        ELSEIF (ANS.EQ.'8') THEN
          RETURNED_STRING = 'MACRO8'
          RETURN
        ELSEIF (ANS.EQ.'9') THEN
          RETURNED_STRING = 'MACRO9'
          RETURN
C Delete
        ELSEIF (ANS.EQ.'D'.OR.ANS.EQ.'d') THEN
          IF (IBAT.NE.0) THEN
            SIGBAT(IBAT) = -SIGBAT(IBAT)
            CALL PGPOINT(1,X(IDD),Y(IDD)*86400000.,5)
          ENDIF
C Force new residuals by changing OLDDPAR(10
          OLDDPAR(1) = 0D0
          GOTO 10
C Start 
        ELSEIF (ANS.EQ.'S'.OR.ANS.EQ.'s') THEN
          START = XD + EPOCH
          GOTO 10
C Finish
        ELSEIF (ANS.EQ.'F'.OR.ANS.EQ.'f') THEN
          FINISH = XD + EPOCH
          GOTO 10
C ZOOM IN BY A FACTOR OF TWO
	ELSEIF (ANS.EQ.'Z'.OR.ANS.EQ.'z') THEN
	  CALL PGQWIN(XMIN1,XMAX1,YMIN1,YMAX1)
	  START =  EPOCH + XD - ABS(XMAX1-XMIN1)/4.0
	  FINISH = EPOCH + XD + ABS(XMAX1-XMIN1)/4.0
          CALL PLTCMD (DUMMY,JINP,NINP,IFAIL)
	  GOTO 10
	ELSEIF (ANS.EQ.'U'.OR.ANS.EQ.'u') THEN
C ZOOM OUT BY A FACTOR OF TWO
	   START = EPOCH + XD - ABS(FINISH-START)
	  FINISH = EPOCH + XD + ABS(FINISH-START)
          CALL PLTCMD (DUMMY,JINP,NINP,IFAIL)
	  GOTO 10
	ELSEIF (ANS.EQ.'+'.OR.ANS.EQ.'=') THEN
C ADD A TURN
	 ISTEP=1
	 ESTEP=EPOCH + XD
         CALL PERPARAM (PER,V,IFAIL)
         DO IRES=1,NFIT
          IF (DATEJD(IDTRES(IRES)) + TAIRES(IRES)/86400 .GT. ESTEP)
     &       Y(IRES) = Y(IRES) + ISTEP*PER/86400D0
         ENDDO
	 GOTO10
	ELSE IF (ANS.EQ.'-'.OR.ANS.EQ.'_') THEN
C SUBTRACT A TURN
	 ISTEP=-1
	 ESTEP=EPOCH + XD
         CALL PERPARAM (PER,V,IFAIL)
         DO IRES=1,NFIT
          IF (DATEJD(IDTRES(IRES)) + TAIRES(IRES)/86400 .GT. ESTEP)
     &       Y(IRES) = Y(IRES) + ISTEP*PER/86400D0
         ENDDO
 	 GOTO 10
C CREATE A RECTANGLE
	 ELSE IF (ANS.EQ.'R'.OR.ANS.EQ.'r') THEN
	   XX1=XD	     
	   YY1=YD
	   IF(PGCURSE(XD,YD,ANS).EQ.1)THEN
	    XX2=XD
	    YY2=YD
	   END IF
	   CALL PGSCI(6)
	   CALL PGMOVE(XX1,YY1)
	   CALL PGDRAW(XX2,YY1)
	   CALL PGDRAW(XX2,YY2)
	   CALL PGDRAW(XX1,YY2)
	   CALL PGDRAW(XX1,YY1)
	   IF (XX1.GT.XX2) THEN
	     XD=XX2
             XX2=XX1
	     XX1=XD
	   END IF
	   IF (YY1.GT.YY2) THEN
	     YD=YY2
             YY2=YY1
	     YY1=YD
	   END IF
          DO I=1,NFIT
	   IF(X(I).GT.XX1 .AND. X(I).LT.XX2 .AND.
     &        Y(I)*86400D3.GT.YY1 .AND. Y(I)*86400D3.LT.YY2) THEN
            IBAT = IBARES(I)
            IF (SIGBAT(IBAT).GT.0) SIGBAT(IBAT) = -SIGBAT(IBAT)
            CALL PGPOINT(1,X(I),Y(I)*86400000.,5)
           ENDIF
          ENDDO
	   CALL PGSCI(0)
	   CALL PGMOVE(XX1,YY1)
	   CALL PGDRAW(XX2,YY1)
	   CALL PGDRAW(XX2,YY2)
	   CALL PGDRAW(XX1,YY2)
	   CALL PGDRAW(XX1,YY1)
C Force new residuals by changing OLDDPAR(10
          OLDDPAR(1) = 0D0
          GOTO 10
        ELSE
           RETURNED_STRING = ' '
           GOTO 10
        END IF
      ELSE
          IFAIL = 51
          RETURNED_STRING=' '
          RETURN
      ENDIF
C
      RETURN
C
C END OF SUBROUTINE CURSOR
C
      END
C
C
C***********************************************************************

