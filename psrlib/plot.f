*DECK PLOT
C
C***********************************************************************
      SUBROUTINE PLTINIT(XMIN,YMIN,XMAX,YMAX,CHARH,CHARW,IWS)
C***********************************************************************
C
C  INITIALIZE PLOTTER
C
      IF (IWS.EQ.0) RETURN
      CALL GKS_OPKS(22)
      CALL GKS_OPWK(4,0,IWS)
      CALL GKS_ACWK(4)
      IF (IWS.EQ.5) CALL GKS_SWKVW(4,.26,.335)
      IF (IWS.EQ.7) CALL GKS_SWKVW(4,.26,.335)
      CALL GKS_SW(XMIN,YMIN,XMAX,YMAX)
      CALL GKS_STXSP(0.0,CHARH,CHARW,0.0,1.1*CHARW,0.0)
      CALL GKS_STXRP(4,1000,1,2)
C
      CALL GKS_CLRWK(4)
      CALL PLOT (0.0,0.0,2)
      CALL PLOT (0.0,0.0,4)
      CALL PLOT (0.0,0.0,3)
      END
C
C************************************************************************
      SUBROUTINE ENDPLOT (IWS,PLOTNAME)
C************************************************************************
C
      CHARACTER*(*) PLOTNAME
      CALL GKS_DAWK(4)
      CALL GKS_CLWK(4)
      CALL GKS_CLKS
C
      IF (IWS.EQ.5) THEN
         CALL PSPOOL('FOR000.DAT','GKS_RASTER',PLOTNAME,
     &               'VERSATEC',NOK)
      ELSEIF(IWS.EQ.7) THEN
         CALL PSPOOL('FOR000.DAT','GKS_RASTER',PLOTNAME,
     &               'PRINTRONIX',NOK)
      ENDIF
C
      END
C
C************************************************************************
      SUBROUTINE PLOT(X,Y,IPEN)
C************************************************************************
C
C  MOVES TO POINT (X,Y) ACCORDING TO IPEN:
C        IPEN=1   SET ORIGIN TO BE AT (X,Y) IN CURRENT COORDS.
C        IPEN=2   RESET ORIGIN TO BE AT (X,Y) IN WORLD COORDS.
C        IPEN=3   PEN UP
C        IPEN=4   PEN DOWN
      COMMON /ORIGIN/X0,Y0
      DIMENSION AX(200),AY(200)
      DATA NP/0/,X0/0/,Y0/0/
C
      GO TO (100,200,300,400) IPEN
  300 IF (NP.GE.2) CALL GKS_POLYL(NP,AX,AY)
      NP=0
  400 NP=NP+1
      AX(NP)=X0+X
      AY(NP)=Y0+Y
      RETURN
C
  100 X0=X0+0
      Y0=Y0+Y
      RETURN
  200 X0=X
      Y0=Y
      END
C
C************************************************************************
      SUBROUTINE TEXT(X,Y,CHARS,N,I)
C************************************************************************
C
      INTEGER CHARS(2)
      COMMON /ORIGIN/X0,Y0
      CALL GKS_TX(X0+X,Y0+Y,N+2,CHARS)
      END
C
C************************************************************************
      SUBROUTINE BLOB(X,Y,XR,YR,IOPEN)
C************************************************************************
C
      IF (IOPEN.EQ.0) THEN
         ICST=10
      ELSE
         ICST=1
      ENDIF
C
      DO IC=ICST,10
         FRR = 0.1*IC
         CALL PLOT(X+XR*FRR,Y,3)
         DO I=1,40
            XX=X+XR*FRR*COS(3.1416*I/20.)
            YY=Y+YR*FRR*SIN(3.1416*I/20.)
            CALL PLOT(XX,YY,4)
         ENDDO
      ENDDO
      END

