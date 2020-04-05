C*************************************************************************
        PROGRAM FITORBIT
C*************************************************************************
C
C A program to determine the orbital parameters of a binary pulsar from
C   period data.  The data, the current model and the residuals from the
C   model may be plotted, or the parameters may be fitted using LMM.
C The period data for pulsar PSRNAM are provided in a file PSRNAM.PER,
C   together with the initial estimate of the pulsar parameters.
C
C  BASED UPON THE ORIGINAL PROGRAM BY A.BRINKLOW & A.G.LYNE   OCT 1987
C
c Modified version written with the aid of the InterfaceBuilder
c       Matthew Bailes NRAL  JAN 1991
c Plot against binary phase added - AGL sept 1992
c Position fit facility added - AGL oct 1992
c Modified to use improved ephemeris file operations - FHJ & DLG, Nov '92
c Modified to deal with positions more correctly - AGL aug 1993
c Allow deletion of points (#) and symbol specification (*,+,o) by first
c     character on line in .per file  - AGL Jun 1994
c Set Epoch now works, changing by integral Porb - Jul 1998
c
c dialog variables -----------------------
      implicit none
      integer array_index
      character ans
c
c Arrays used to retrieve values from READEPH.
      DOUBLE PRECISION E_VALUES(63)
      DOUBLE PRECISION E_ERRORS(63)
c
c fitorbit variables -----------------------
c
      CHARACTER*20 RAFIT,DECFIT
      INTEGER NPAR,NPTMAX,NMOD,IW,MAXWORD,LENGTH,plotno,plotdi
      PARAMETER (NPAR=9,NPTMAX=500,NMOD=720,IW=NPAR*(NPAR+11)/2,
     &     MAXWORD=10)
      EXTERNAL BINARY
      LOGICAL OUTPUT,FULL,OPEN
      INTEGER NWORD,last_plot_type,len1,len2
      character*20
     &Crms*10,Cf*10,Cepperi*10,CTASCe,Cp0e,Cpdote,Cporbe,Casinie,
     &Comegae,cecce,Crae,Cdece
      CHARACTER*20 LINE*80,WORD(MAXWORD),PSRNAM*40,EPHFILE*80,
     & RESFIL*40,XTITLE*40,YTITLE*40,WEIGHT*3,THISDIR*80,grphdev*80
      CHARACTER GRDEV*30,LASTGRDEV*30,TEXT*7,PLOTTEXT*40,tmpchr*80
      REAL XX,YY,DIFF,FACTOR,DIFFX,DIFFY,DIST,CLOSEST
      integer aaa,bbb
      INTEGER NFIT,NPLOT,LPL,IFLAG(NPTMAX),I,ICLOSEST,NPARFIT,IER
      INTEGER II,NPTS,IPR,NITS,LOUT,IERR,ICON(NPAR),IWK(NPAR),IFAIL
      REAL TMOD(NMOD)
      REAL X1(NPTMAX),Y1(NPTMAX),X2(NMOD),Y2(NMOD),X3(NPTMAX),
     &    YERRT1(NPTMAX),YERRB1(NPTMAX),
     &    YERRT2(NMOD),YERRB2(NMOD),
     &    YRES(NPTMAX),XAXIS(2),ZERO(2),YMIN,YMAX,PMIN,PMAX,
     &    PLOTXMIN,PLOTXMAX,PLOTYMIN,PLOTYMAX,PHASE
      REAL*8 PAR(NPAR),GRAD(NPAR),ERR(NPAR),RES(NPTMAX),
     &         RPLANE(NPTMAX),WK(IW),TOL,EXPND,DECR,
     &         XFIT(NPTMAX),YFIT(NPTMAX),SIGFIT(NPTMAX)
      REAL*8 K1,K2,K3,P0,PORB,TASC,ASINI,OMEGA,ECC,EPPERI,EASC,RA0,DEC0,
     &         RA,DEC,MJDS,NEW_MJDS,MJDF,ORIGINAL_MJDS,ORIGINAL_MJDF,
     &         ECCANOM,MEANANOM,TRUEANOM,DP,DTASC,DPORB,DRA,DDEC,
     &         PMRA,PMDEC,RAS,DECS
      REAL*8 P0E,PORBE,PDOT,PDOTE,TASCE,ASINIE,OMEGAE,ECCE,RAE,DECE
      REAL*8 MJD(NPTMAX),PERIOD(NPTMAX),PERR(NPTMAX),PERDIFF(NPTMAX)
      REAL*8 PCALC(NPTMAX),PDOTCALC(NPTMAX),PDDCALC(NPTMAX),DT,DV,T,F
      REAL*8 PRANGMAX,PRANGMIN,SUMSQ,RMS,PMOD(NMOD),PI,C,VEL,RADDEG
      LOGICAL CURSE,REPLOT,REDISPLAY
      DATA CURSE,REPLOT,REDISPLAY/.FALSE.,.FALSE.,.FALSE./
      DATA PI,C/3.1415926537D0,2.99792458D8/
      DATA LASTGRDEV/'/xw'/
      DATA XAXIS,ZERO/46200.0,47700,0.0,0.0/
      DATA FACTOR /4.0/
      DATA ICON,WEIGHT,TOL/NPAR*-1,'OFF',0.0001/
      DATA PSRNAM/'1812-1737'/
      DATA Crms,Cf,Cepperi,CTASCe,Cp0e,Cpdote,Cporbe,Casinie,Comegae,
     &     cecce,Crae,Cdece/12*' '/
      LOGICAL LDELETE,LRECT,LSTART,LFINISH,LZOOM,LSETEPOCH,LPHASE,
     & LEPOCH,LUNZOOM,LIDENTIFY,
     & LFPERIOD,LFPDOT,LFEPOCH,LERRORBARS,
     & LFASINI,LFE,LFOMEGA,LFPBIN,LFRA,LFDEC,LWEIGHTS,LRESTORE,LDELETED,
     & LPERIOD,LRESIDUAL,LHARDCOPY,PARPOS

      EQUIVALENCE (PAR(1),DP),(PAR(2),PDOT),(PAR(3),DPORB),
     &       (PAR(4),DTASC),(PAR(5),ASINI),(PAR(6),OMEGA),
     &       (PAR(7),ECC),(PAR(8),DRA),(PAR(9),DDEC)
      EQUIVALENCE (ERR(1),P0E),(ERR(2),PDOTE),(ERR(3),PORBE),
     &       (ERR(4),TASCE),(ERR(5),ASINIE),(ERR(6),OMEGAE),
     &       (ERR(7),ECCE),(ERR(8),RAE),(ERR(9),DECE)
      COMMON P0,TASC,PORB,RA0,DEC0,XFIT,YFIT,SIGFIT,WEIGHT
C
      RADDEG = 180D0/PI
      PLOTDI=1
      OUTPUT=.FALSE.
      FULL=.TRUE.
      LPERIOD=.TRUE.
      LEPOCH=.TRUE.
      LERRORBARS=.TRUE.
      LRESIDUAL=.FALSE.
      LZOOM=.TRUE.

c get the current directory, if it's below timing then change the
c default pulsar name... DRL 93/07/21
      call getenv('PWD',THISDIR)
      len1=index(THISDIR,'timing/')
      len2=index(THISDIR,' ')-1
      if(len1.gt.0.and.len2.gt.len1) PSRNAM=THISDIR(len1+7:len2)
      len1 = index(psrnam,' ')-1
      psrnam = psrnam(1:len1)

c get graphics device.

      WRITE(6,'(x,a,$)') 'Enter Graphics Device (Default='//
     &          LASTGRDEV(1:INDEX(LASTGRDEV,' ')-1)//'):'
      READ(*,'(A)') GRDEV
      IF (GRDEV.EQ.' ') GRDEV=LASTGRDEV
      LPL = INDEX(GRDEV,' ')-1
      CALL PGBEGIN(0,GRDEV(1:LPL),1,1)
      LASTGRDEV=GRDEV

      DO WHILE (.TRUE.)
       IF (REPLOT) THEN
        MJDS=NEW_MJDS
C PLOT
        K1=2.0D0*PI*ASINI/(PORB*86400D0*SQRT(1.0D0-ECC**2))
        K2=4.0D0*PI**2*ASINI/(PORB*86400D0*(1-ECC**2))**2
        K3=8.0d0*PI**3*ASINI/(PORB*86400D0)**3/(1-ECC**2)**3.5
C
        EASC = 2D0*ATAND(SQRT((1-ECC)/(1+ECC))*TAND(-OMEGA/2))
        EPPERI = TASC - PORB*(EASC - 180/PI*ECC*SIND(EASC))/360D0
        DO II=1,NPTS
          T=MJD(II)
          X3(II)=MJD(II)-MJDS
          IF(LPHASE) THEN
            X1(II)=(T-TASC)/PORB
            X1(II)=MOD(X1(II),1.0D0)
            IF (X1(II).LT.0D0) X1(II) = X1(II) + 1.0D0
          ELSE
            X1(II)=MJD(II)-MJDS
          ENDIF
C
C  Calculate the Mean Anomaly, MEANANOM, and the binary phase, PHASE
C
          MEANANOM=360.0D0*(T-EPPERI)/PORB
          MEANANOM=MOD(MEANANOM,360.0D0)
          IF (MEANANOM.LT.0D0) MEANANOM = MEANANOM + 360.0D0
C
C  Calculate the Eccentric Anomaly, ECCANOM.
C
          CALL KEPLER(MEANANOM,ECC,ECCANOM,NITS)
C
C  Calculate the True Anomaly, TRUEANOM.
C
          TRUEANOM=2.0D0*ATAND(SQRT((1.0D0+ECC)/(1.0D0-ECC))
     &          *TAND(ECCANOM/2.0D0))
C
C Get the differential positional velocity,dv.
          CALL DELTAV(T,RA0,DEC0,RA-RA0,DEC-DEC0,2000.0D0,DV)

C  Calculate the expected period (Smart p359).
          PCALC(II)=(P0 + PDOT*(T-TASC)*86400D-15)*
     &       (1.0D0 + K1*DCOSD(TRUEANOM+OMEGA) + K1*ECC*DCOSD(OMEGA))*
     &       (1.0D0 - DV/C)
          PDOTCALC(II) = PDOT*1D-15 - (P0 + PDOT*(T-TASC)*86400D-15)
     &       *K2*(1+ECC*DCOSD(TRUEANOM))**2*DSIND(TRUEANOM+OMEGA)
          PDDCALC(II) = (P0 + PDOT*(T-TASC)*86400D-15)
     &       *K3*(1+ECC*DCOSD(TRUEANOM))**3
     &       *(2*ECC*DSIND(TRUEANOM)*DSIND(TRUEANOM+OMEGA)
     &       -(1+ECC*DCOSD(TRUEANOM))*DCOSD(TRUEANOM+OMEGA))
          PERDIFF(II)= PERIOD(II) - PCALC(II)
          YRES(II)=1E6*PERDIFF(II)
          YERRT2(II)=YRES(II)+1E6*PERR(II)
          YERRB2(II)=YRES(II)-1E6*PERR(II)
          Y1(II)=(PERIOD(II)-P0)*1E6
          YERRT1(II)=Y1(II)+1E6*PERR(II)
          YERRB1(II)=Y1(II)-1E6*PERR(II)
        END DO
C
C  Now calculate the model.
c  First, get the Mean Anomaly of any eclipse (code not checked).
c        ECCANOM = 2D0*ATAND(SQRT((1-ECC)/(1+ECC))*TAND((90.-OMEGA)/2))
c        MEANANOM = ECCANOM - 180/PI*ECC*SIND(ECCANOM)
c        WRITE(*,'(''MA of Eclipse ='',3f7.2)') MEANANOM

        IF(LPHASE) THEN
          DT=PORB/NMOD
        ELSE
          DT = (MJDF - MJDS)/NMOD
        ENDIF
        DO II=1,NMOD
          IF(LPHASE) THEN
            T = TASC + (II-1)*DT
            TMOD(II)=T
            X2(II)=(II-1)*DT/PORB
          ELSE
            T = MJDS + (II-1)*DT
            TMOD(II)=T
            X2(II)=T-MJDS
          ENDIF
C
C  Calculate the Mean Anomaly, MEANANOM.
C
          EASC = 2D0*ATAND(SQRT((1-ECC)/(1+ECC))*TAND(-OMEGA/2))
          EPPERI = TASC - PORB*(EASC - 180/PI*ECC*SIND(EASC))/360D0
          MEANANOM=360.0D0*(T-EPPERI)/PORB
          MEANANOM=MOD(MEANANOM,360.0D0)
          IF (MEANANOM.LT.0D0) MEANANOM = MEANANOM + 360.0D0
C
C  Calculate the Eccentric Anomaly, ECCANOM.
C
          CALL KEPLER(MEANANOM,ECC,ECCANOM,NITS)
C
C  Calculate the True Anomaly, TRUEANOM.
C
          TRUEANOM=2.0D0*ATAND(SQRT((1.0D0+ECC)/(1.0D0-ECC))
     &        *TAND(ECCANOM/2.0D0))
C
C Get the differential positional velocity,dv.
          CALL DELTAV(T,RA0,DEC0,RA-RA0,DEC-DEC0,2000.0D0,DV)

C  Calculate the expected period.
C
          PMOD(II)=(P0 + PDOT*(T-TASC)*86400D-15)*
     &             (1.0D0 + K1*DCOSD(TRUEANOM+OMEGA)
     &             + K1*ECC*DCOSD(OMEGA)) * (1.0D0 - DV/C)
          Y2(II)=1E6*(PMOD(II) - P0)
C            IF(OUTPUT)WRITE(1,*)V,PMOD(V)*1000.0D0 - P0*1D6
        END DO
C
C
C Get period range etc, depending upon IFLAG and date.
        NPLOT=0
        SUMSQ = 0D0
        YMIN = 1E30
        YMAX =-1E30
        PMIN = 1E30
        PMAX =-1E30
        DO II=1,NPTS
          IF(MJD(II).GT.MJDS.AND.MJD(II).LT.MJDF) THEN
            IF(IFLAG(II).LT.0) THEN
              NPLOT=NPLOT+1
              SUMSQ = SUMSQ + PERDIFF(II)**2
            ENDIF
            IF(LDELETED.OR.IFLAG(II).LT.0) THEN
              PMAX = MAX(PMAX,YERRT1(II))
              PMIN = MIN(PMIN,YERRB1(II))
              YMAX = MAX(YMAX,YERRT2(II))
              YMIN = MIN(YMIN,YERRB2(II))
            ENDIF
          ENDIF
        ENDDO

        PRANGMAX = ABS(PMAX) + 0.1*(ABS(PMAX) + ABS(PMIN))
        PRANGMIN = ABS(PMIN) + 0.1*(ABS(PMAX) + ABS(PMIN))
        IF(NPLOT.EQ.0) THEN
          REPLOT=.FALSE.
          GOTO 999
        ENDIF
        RMS=SQRT(SUMSQ/NPLOT)
        WRITE(CRMS,'(F10.6)')1D6*RMS
        XAXIS(1) = 0.0
        IF(LPHASE) THEN
          XAXIS(2) = 1.0
        ELSE
          XAXIS(2) = MJDF-MJDS
        ENDIF

        IF (LHARDCOPY) THEN
          CALL PGSCF(2)
          CALL PGSCH(1.3)
          CALL PGSLW(3)
        END IF
C Period plot.
        IF (LPERIOD) THEN
          last_plot_type = 1
          IF(.NOT. LHARDCOPY) THEN
            CALL XPLFITORBITDIALOG(1,0.07,0.01,0.2,0.0)
            CALL XPLFITORBITDIALOG(2,0.0,0.0,0.0,0.0)
            CALL SETPLFITORBITDIALOG(1,XAXIS(1),XAXIS(2),
     &         -real(PRANGMIN),real(PRANGMAX))
          ELSE
            CALL PGVPORT(0.1,0.9,0.2,0.9)
            CALL PGWINDOW(XAXIS(1),XAXIS(2),
     &         -real(PRANGMIN),real(PRANGMAX))
          END IF
          CALL PGBOX('BCNST',0.0,0,'BCNST',0.0,0)
          IF(LHARDCOPY) THEN
            CALL PGMTEXT('T',2.0,0.5,0.5,'PSR '//PSRNAM)
          ENDIF
          WRITE (YTITLE,
     &       '(''Period offset from'',f12.2,x,a1,
     &        ''gmsec'')') P0*1D6,char(92)
          IF(LPHASE) THEN
            WRITE (XTITLE,'(''Orbital Phase (periods)'')')
          ELSE
            WRITE (XTITLE,'(''Days after MJD '',f10.3)') MJDS
          ENDIF
          CALL PGMTEXT('B',2.5,0.5,0.5,XTITLE)
          CALL PGMTEXT('L',2.5,0.5,0.5,YTITLE)
          CALL PGSCH(1.0)
          CALL PGSLW(1)
          call pgsci(5)
          CALL PGLINE(NMOD,X2,Y2)
          DO II=1,NPTS
            IF(LDELETED.OR.IFLAG(II).LT.0) THEN
              CALL PGSCI(2)
              CALL PGPOINT(1,X1(II),Y1(II),-IFLAG(II))
              IF(LERRORBARS) THEN
                CALL PGSCI(1)
                CALL PGERRY(1,X1(II),YERRT1(II),YERRB1(II),0.0)
              ENDIF
              IF(LDELETED.AND.IFLAG(II).GT.0) THEN
                CALL PGSCI(7)
                CALL PGSCH(2.0)
                CALL PGPOINT(1,X1(II),Y1(II),5)
                CALL PGSCH(1.0)
              END IF
            ENDIF
          ENDDO
          CALL PGSCI(1)
C
C?          PRANGE=1.5*MAX(ABS(YMAX),ABS(YMIN))
        END IF

C Residual plot.
        IF (LRESIDUAL) THEN
          LAST_PLOT_TYPE=2
          IF(NPLOT.NE.0) THEN
            IF(.NOT. LHARDCOPY) THEN
              CALL XPLFITORBITDIALOG(1,0.07,0.01,0.2,0.0)
              CALL SETPLFITORBITDIALOG(1,
     &XAXIS(1),XAXIS(2),YMIN-0.15*(YMAX-YMIN),YMAX+0.15*(YMAX-YMIN))
            ELSE
              CALL PGVPORT(0.1,0.9,0.2,0.9)
              CALL PGWINDOW(XAXIS(1),XAXIS(2),
     &           YMIN-0.15*(YMAX-YMIN),YMAX+0.15*(YMAX-YMIN))
            ENDIF
          ENDIF

          CALL PGBOX('BCNST',0.0,0,'BCNST',0.0,0)
          call pgsci(1)
          IF(LHARDCOPY) THEN
            CALL PGMTEXT('T',2.0,0.5,0.5,'PSR '//PSRNAM)
          ENDIF
          CALL PGMTEXT('L',2.5,0.5,0.5,'Period Residual (microsec)')
          IF(LPHASE) THEN
            WRITE (XTITLE,'(''Orbital Phase (periods)'')')
          ELSE
            WRITE (XTITLE,'(''Days after MJD '',f10.3)') MJDS
          ENDIF
          CALL PGMTEXT('B',2.5,0.5,0.5,XTITLE)
          CALL PGSCH(1.0)
          CALL PGSLW(1)
          CALL PGSCI(3)
          CALL PGLINE(2,XAXIS,ZERO)
          CALL PGSCI(1)
          DO II=1,NPTS
            IF(LDELETED.OR.IFLAG(II).LT.0) THEN
              CALL PGSCI(2)
              CALL PGPOINT(1,X1(II),YRES(II),-IFLAG(II))
              IF(LERRORBARS) THEN
                CALL PGSCI(1)
                CALL PGERRY(1,X1(II),YERRT2(II),YERRB2(II),0.0)
              ENDIF
              IF(LDELETED.AND.IFLAG(II).GT.0) THEN
                CALL PGSCI(7)
                CALL PGSCH(2.0)
                CALL PGPOINT(1,X1(II),YRES(II),5)
                CALL PGSCH(1.0)
              END IF
            ENDIF
          ENDDO
          CALL PGSCI(1)
          CALL DIALOG_RESET
          REPLOT=.FALSE.
        END IF
        IF (LHARDCOPY) THEN
          CALL PGEND
          CALL PGBEGIN(0,LASTGRDEV(1:LPL),1,1)
          PLOTDI=1
        END IF
C DRAW LITTLE WINDOW
        IF (.NOT. LHARDCOPY) THEN
          CALL XPLFITORBITDIALOG(2,0.0,0.0,0.0,0.0)
          CALL SETPLFITORBITDIALOG(2,REAL(ORIGINAL_MJDS),
     &            REAL(ORIGINAL_MJDF),-1.0,1.0)
          CALL PGSCI(1)
          CALL PGBOX('BC',0.0,0,'BC',0.0,0)
          DO I=1,NPTS
            CALL PGPOINT(1,REAL(MJD(I)),0.0,ABS(IFLAG(I)))
          END DO
          CALL PGSCI(4)
          CALL PGSLW(3)
          CALL PGMOVE(REAL(MJDS),-1.0)
          CALL PGDRAW(REAL(MJDS),1.0)
          CALL PGMOVE(REAL(MJDF),-1.0)
          CALL PGDRAW(REAL(MJDF),1.0)
          CALL PGSLW(1)
          CALL PGSCI(1)
          CALL DIALOG_RESET
        END IF
      END IF
        RAS=RA*240.0D0
        DECS=DEC*3600.0D0
        CALL FITORBITDIALOG(PLOTDI,XX,YY,ANS,PLOTNO,ARRAY_INDEX,
     &CRMS,CF,CEPPERI,CTASCE,CP0E,CPDOTE,CPORBE,CASINIE,COMEGAE,CECCE,
     &CRAE,CDECE,
     &LDELETE,LRECT,LSTART,LFINISH,LZOOM,LSETEPOCH,LUNZOOM,LIDENTIFY,
     &LEPOCH,LPHASE,LRESTORE,
     &LFPERIOD,LFPDOT,LFEPOCH,LFASINI,LFE,LFOMEGA,LFPBIN,
     &LWEIGHTS,LERRORBARS,LDELETED,LHARDCOPY,LFRA,LFDEC,
     &FACTOR,
     &TASC,P0,PDOT,PORB,ASINI,OMEGA,ECC,RAS,DECS,
     &PSRNAM)
        RA=RAS/240.0D0
        DEC=DECS/3600.0D0
        PLOTDI=2
        REPLOT=.FALSE.


        IF (ARRAY_INDEX .NE. 8 .AND. ARRAY_INDEX .NE. 9) THEN
          LHARDCOPY = .FALSE.
        END IF


        IF (ARRAY_INDEX .EQ. 1) THEN
c  Fit + Plot
          DO I=1,9
            ICON(I)=1
          ENDDO
          IF (LFPERIOD)ICON(1)=-1
          IF (LFPDOT)  ICON(2)=-1
          IF (LFPBIN)  ICON(3)=-1
          IF (LFEPOCH) ICON(4)=-1
          IF (LFASINI) ICON(5)=-1
          IF (LFOMEGA) ICON(6)=-1
          IF (LFE)     ICON(7)=-1
          IF (LFRA)    ICON(8)=-1
          IF (LFDEC)   ICON(9)=-1
          NPARFIT=0
          DO I=1,9
            IF(ICON(I).LT.0) NPARFIT=NPARFIT+1
          ENDDO
          LOUT = 6
          NITS = 30
          EXPND = 2.0
          DECR = 0.4
          IPR=0
C Initialise variables which are offsets
          DP = 0D0
          DTASC = 0D0
          DPORB = 0D0
          DRA = RA - RA0
          DDEC = DEC - DEC0

C Form the data arrays for LMM, depending upon IFLAG and date.
          IF (LWEIGHTS) WEIGHT='ON'
          IF (.NOT. LWEIGHTS) WEIGHT='OFF'
          NFIT=0
          DO II=1,NPTS
            IF(MJD(II).GT.MJDS.AND.MJD(II).LT.MJDF.AND.IFLAG(II).LT.0)
     &          THEN
              NFIT=NFIT+1
              XFIT(NFIT)=MJD(II)
              YFIT(NFIT)=PERIOD(II)
              SIGFIT(NFIT)=PERR(II)
            ENDIF
          ENDDO
          IF(NFIT.LE.NPARFIT) THEN
            CALL PGSCI(0)
            CALL PGSFS(1)
            CALL PGRECT(39.0,62.0,9.0,13.0)
            CALL PGSCI(1)
            CALL PGSFS(2)
            CALL PGRECT(39.0,62.0,9.0,13.0)
            CALL PGTEXT(40.0,12.0,'Not enough points for fit')
            WRITE(PLOTTEXT,'(i4,a,i4,a)') NPARFIT,' : parameters',NFIT,
     &  ' : points.'
            CALL PGTEXT(40.0,10.0,PLOTTEXT(1:length(PLOTTEXT)))
            REPLOT=.FALSE.
          ELSE
           CALL LMM(PAR(1),GRAD(1),SUMSQ,NFIT,NPAR,TOL,EXPND,DECR,NITS,
     &      IPR,LOUT,ERR(1),RES(1),RPLANE(1),WK(1),IW,IWK(1),ICON(1),
     &      BINARY,XFIT(1),1,YFIT(1),IERR)
           IF(IERR.EQ.0) WRITE (*,*) ' CONVERGENCE OK'
           IF(IERR.NE.0) WRITE (*,*) ' CONVERGENCE FAILED:  IERR = ',IERR
           WRITE (*,'(A,I4,A,F15.6)') ' No of iterations=',NITS,
     &      '.   Residual sum of squares=',SUMSQ
C Increment variables for which offsets were fitted.
           P0 = P0 + DP
           TASC = TASC + DTASC
           PORB = PORB + DPORB
           RA   = RA0  + DRA
           DEC  = DEC0 + DDEC

           REPLOT=.TRUE.
           REDISPLAY=.TRUE.
C
C  Calculate the mass function (in solar masses):
C
           F=4.0D0*PI**2*(ASINI*C)**3/
     &         (PORB*24.0D0*3600.0D0)**2/1.99E30/6.67E-11
           WRITE(CF,'(F10.8)')F
           WRITE(CEPPERI,'(f10.4)')EPPERI
           WRITE(CTASCE,'(f10.8)')TASCE
           WRITE(CP0E,'(F10.8)')P0E
           WRITE(CPDOTE,'(F10.4)')PDOTE
           WRITE(CPORBE,'(F10.8)')PORBE
           WRITE(CASINIE,'(F10.8)')ASINIE
           WRITE(COMEGAE,'(F10.6)')OMEGAE
           WRITE(CECCE,'(F10.6)')ECCE
           WRITE(CRAE,'(F10.3)')240.0D0*RAE
           WRITE(CDECE,'(F10.3)')3600.0D0*DECE
          END IF


        ELSE IF(ARRAY_INDEX .EQ. 2)THEN
c restore defaults.
c Call subroutine to load ephemeris data
           EPHFILE = PSRNAM(1:index(psrnam,' ')-1)
c           EPHFILE = PSRNAM(1:index(psrnam,' ')-1)//'.eph'
           CALL READEPH(EPHFILE,E_VALUES,E_ERRORS,IFAIL)
c Pigeon-hole values
           P0=E_VALUES(8)
           PDOT=E_VALUES(9)
           EPPERI=E_VALUES(30)
           PORB=E_VALUES(31)
           ASINI=E_VALUES(32)
c If not a binary, use the period epoch and set a non-zero binary period.
           IF (ASINI .EQ. 0.D0) THEN
             EPPERI = E_VALUES(12)
             PORB = 1.0
           ENDIF
           OMEGA=E_VALUES(33)
           ECC=E_VALUES(35)
           EASC = 2D0*ATAND(SQRT((1-ECC)/(1+ECC))*TAND(-OMEGA/2))
           TASC = EPPERI + PORB*(EASC - 180/PI*ECC*SIND(EASC))/360D0
c RA and DEC are returned in degrees
           RA=E_VALUES(3)
           DEC=E_VALUES(4)
           DRA=RA-RA0
           DDEC=DEC-DEC0
c Need to call SEC2POSN to turn RA and DEC BACK into a string.
           CALL SEC2POSN((RA*240.0D0),RAFIT)
           CALL SEC2POSN((DEC*3600.0D0),DECFIT)

           CRMS=' '
           CF=' '
           CEPPERI=' '
           CTASCE=' '
           CP0E=' '
           CPDOTE=' '
           CPORBE=' '
           CASINIE=' '
           COMEGAE=' '
           CECCE=' '
           CRAE=' '
           CDECE=' '
           
           REDISPLAY=.TRUE.


        ELSE IF(ARRAY_INDEX .EQ. 3)THEN
C show fit.
C
 11     EASC = 2D0*ATAND(SQRT((1-ECC)/(1+ECC))*TAND(-OMEGA/2))
        EPPERI = TASC - PORB*(EASC - 180/PI*ECC*SIND(EASC))/360D0
        VEL=2.0D0*PI*ASINI*2.9979E8/PORB/24.0D0/3600.0D0/1000D0
C
C  Calculate the mass function (in solar masses):
C
        F=4.0D0*PI**2*(ASINI*C)**3/
     &         (PORB*24.0D0*3600.0D0)**2/1.99E30/6.67E-11
C
        WRITE(*,1245)P0,P0E,PDOT,PDOTE,PORB,PORBE,TASC,TASCE,EPPERI,
     &    ASINI,ASINIE,OMEGA,OMEGAE,ECC,ECCE,RA0+DRA,RAE,DEC0+DDEC,
     &    DECE,F,VEL,WEIGHT,TOL,ICON,MJDS,MJDF
 1245   FORMAT(/
     &  1X,'Mean Period =            ',2F15.12,' s'/
     &  1X,'Period Derivative=       ',2F15.6,' 10**-15s/s'/
     &  1X,'Orbital Period =         ',2F15.6,' days'/
     &  1X,'Epoch of Ascending Node =',2F15.6,' MJD'/
     &  1X,'Epoch of Periastron =    ',F15.6,15X,' MJD'/
     &  1X,'a sin(i) =               ',2F15.6,' light secs'/
     &  1X,'Longitude of Periastron =',2F15.6,' degrees'/
     &  1X,'Eccentricity =           ',2F15.6/
     &  1X,'Right Ascension =        ',2F15.6,' degrees'/
     &  1X,'Declination =            ',2F15.6,' degrees'/
     &  1X,'Mass function =          ',F15.6,15X,' solar masses'/
     &  1X,'Mean orbital velocity =  ',F10.1,20X,' km/sec'/
     &  1X,'Weights                  ',A3/
     &  1X,'Tolerance                ',F10.6/
     &  1X,'ICON =                   ',9I3/
     &  1X,'Start of analysis =      ',F15.6,15X,' MJD'/
     &  1X,'End of analysis =        ',F15.6,15X,' MJD')
c Write residuals
          INQUIRE(20,OPENED=OPEN)
          IF (.NOT.OPEN) THEN
            RESFIL=PSRNAM(1:length(psrnam))//'.res'
            OPEN(UNIT=20,FILE=RESFIL,STATUS='unknown')
          END IF

          WRITE(20,1245)
     &      P0,P0E,PDOT,PDOTE,PORB,PORBE,TASC,TASCE,EPPERI,
     &      ASINI,ASINIE,OMEGA,OMEGAE,ECC,ECCE,RA0+DRA,RAE,DEC0+DDEC,
     &      DECE,F,VEL,WEIGHT,TOL,ICON,MJDS,MJDF
          WRITE (20,'(/A,A,A)') '    MJD            Period     ',
     &      '    Model         Difference        Error       Pdot',
     &      '      Pddot   Phase'

          DO II=1,NPTS
            TEXT=' '
            IF(MJD(II).LT.MJDS.OR.MJD(II).GT.MJDF) THEN
               TEXT=' No fit'
            ELSEIF(IFLAG(II).GT.0) THEN
               TEXT=' Deleted'
            ENDIF
            PHASE=(MJD(II)-EPPERI)/PORB
            PHASE=MOD(PHASE,1.)
            IF(PHASE.LT.0.)PHASE=PHASE+1.0
            WRITE(20,30)MJD(II),
     &        1D6*PERIOD(II),1D6*PCALC(II),1D6*PERDIFF(II),
     &        1D6*PERR(II),
     &        1D15*PDOTCALC(II),1D24*PDDCALC(II),PHASE,TEXT
 30         FORMAT(1X,F10.3,2F17.8,2F14.8,F10.3,E10.3,F8.3,A)
          ENDDO
C.. LN modif
          CLOSE (20)

        ELSE IF(ARRAY_INDEX .EQ. 4)THEN
C wrfit
c Put values back into E_VALUES array
           E_VALUES(8)=P0
           E_VALUES(9)=PDOT
           E_VALUES(12)=EPPERI
           E_VALUES(31)=PORB
           E_VALUES(26)=TASC
           E_VALUES(30)=EPPERI
           E_VALUES(32)=ASINI
           E_VALUES(33)=OMEGA
           E_VALUES(35)=ECC
c RA and DEC are returned in degrees. First J2000.
           E_VALUES(3)=RA
           E_VALUES(4)=DEC
c Now calculate B1950.
           CALL SLA_FK54Z(RA/RADDEG,DEC/RADDEG,1950.0D0,E_VALUES(1),
     &       E_VALUES(2),PMRA,PMDEC)
           E_VALUES(1) = E_VALUES(1)*RADDEG
           E_VALUES(2) = E_VALUES(2)*RADDEG

c Call WRITEPH to write an updated ephemeris file
           EPHFILE = PSRNAM(1:length(psrnam))
           CALL WRITEPH(EPHFILE,E_VALUES,E_ERRORS)

        ELSE IF(ARRAY_INDEX .EQ. 5)THEN
c Quit.
          GOTO 888
        ELSE IF(ARRAY_INDEX .EQ. 6)THEN
c Load data
c Call subroutine to load ephemeris data
           EPHFILE = PSRNAM(1:index(psrnam,' ')-1)
c           EPHFILE = PSRNAM(1:index(psrnam,' ')-1)//'.eph'
           CALL READEPH(EPHFILE,E_VALUES,E_ERRORS,IFAIL)
c Pigeon-hole values
           P0=E_VALUES(8)
           PDOT=E_VALUES(9)
           EPPERI=E_VALUES(30)
           PORB=E_VALUES(31)
           ASINI=E_VALUES(32)
c If not a binary, set EPPERI to PEPOCH and a non-zero PORB.
           IF(ASINI .EQ. 0.D0) THEN
              EPPERI = E_VALUES(12)
              PORB = 1.0D0
           ENDIF
           OMEGA=E_VALUES(33)
           ECC=E_VALUES(35)
           EASC = 2D0*ATAND(SQRT((1-ECC)/(1+ECC))*TAND(-OMEGA/2))
           TASC = EPPERI + PORB*(EASC - 180/PI*ECC*SIND(EASC))/360D0
c RA and DEC are returned in degrees
           RA=E_VALUES(3)
           DEC=E_VALUES(4)
c Need to call SEC2POSN to turn RA and DEC BACK into a string.
           CALL SEC2POSN((RA*240.0D0),RAFIT)
           CALL SEC2POSN((DEC*3600.0D0),DECFIT)
c Call subroutine to read in period data
          CALL PSR_READ_PER(PSRNAM,NEW_MJDS,MJDF,NPTMAX,MJD,PERIOD,
     &        PERR,IFLAG,NPTS,LINE)

          CALL PARSE(LINE,WORD,NWORD,MAXWORD,' ')
          RA0=0D0
          DEC0=0D0
          IF(PARPOS(WORD(1),RA0)) RA0=RA0/240.0D0
          IF(PARPOS(WORD(2),DEC0)) DEC0=DEC0/3600.0D0
          DRA=RA-RA0
          DDEC=DEC-DEC0

          DT=MJDF-NEW_MJDS
          NEW_MJDS=NEW_MJDS - 0.05D0*DT
          MJDF    =    MJDF + 0.05D0*DT
          ORIGINAL_MJDS=NEW_MJDS
          ORIGINAL_MJDF=MJDF
          REDISPLAY=.TRUE.
          CRMS=' '
          CF=' '
          CEPPERI=' '
          CTASCE=' '
          CP0E=' '
          CPDOTE=' '
          CPORBE=' '
          CASINIE=' '
          COMEGAE=' '
          CECCE=' '
          CRAE=' '
          CDECE=' '
        else if (array_index.eq.7) then
c PSRTIME output
          continue
        else if (array_index.eq.8) then
          LRESIDUAL=.TRUE.
          LPERIOD=.FALSE.
          REPLOT=.TRUE.
c          IF (LHARDCOPY) THEN
c            CALL PGEND
c            CALL PGBEGIN(0,'?',1,1)
c          END IF
c
c     DRL added this feature 08/03/94 to specify row/col numbers in
c     the PGPLOT call-up (defaults to 1 1)
c
          IF (LHARDCOPY) THEN
            CALL PGEND
            write(*,'(''Enter PGPLOT graphics device > ''$)')
            read(*,'(a)') tmpchr
            grphdev=tmpchr(1:index(tmpchr,' ')-1)
            if (tmpchr(index(tmpchr,' '):).ne.' ') then
               read(tmpchr(index(tmpchr,' '):),*) aaa,bbb
            else
               aaa=1
               bbb=1
            endif
            CALL PGBEGIN(0,grphdev,aaa,bbb)
            call pgadvance
          END IF
        else if (array_index.eq.9) then
          LRESIDUAL=.FALSE.
          LPERIOD=.TRUE.
          REPLOT=.TRUE.
          IF (LHARDCOPY) THEN
            CALL PGEND
            write(*,'(''Enter PGPLOT graphics device > ''$)')
            read(*,'(a)') tmpchr
            grphdev=tmpchr(1:index(tmpchr,' ')-1)
            if (tmpchr(index(tmpchr,' '):).ne.' ') then
               read(tmpchr(index(tmpchr,' '):),*) aaa,bbb
            else
               aaa=1
               bbb=1
            endif
            CALL PGBEGIN(0,grphdev,aaa,bbb)
            call pgadvance
          END IF
        else if (array_index.eq.10) then
          NEW_MJDS=ORIGINAL_MJDS
          MJDF=ORIGINAL_MJDF
          REPLOT=.TRUE.
        else if (plotno .eq. 1) then
          IF (LSTART) THEN
            CALL SELPLFITORBITDIALOG(1)
            CALL QPLFITORBITDIALOG(1,
     &          PLOTXMIN,PLOTXMAX,PLOTYMAX,PLOTYMIN)
            NEW_MJDS = XX + mjds
            call pgsci(4)
            call pgmove(xx,PLOTYMIN)
            call pgdraw(xx,PLOTYMAX)
            call pgsci(1)
            CALL DIALOG_RESET
            LSTART=.FALSE.
            LFINISH=.TRUE.
          ELSE IF (LFINISH) THEN
            CALL SELPLFITORBITDIALOG(1)
            CALL QPLFITORBITDIALOG(1,
     &          PLOTXMIN,PLOTXMAX,PLOTYMAX,PLOTYMIN)
            MJDF = XX + mjds
            call pgsci(4)
            call pgmove(xx,PLOTYMIN)
            call pgdraw(xx,PLOTYMAX)
            call pgsci(1)
            CALL DIALOG_RESET
            LSTART=.TRUE.
            LFINISH=.FALSE.
            REPLOT=.TRUE.
          ELSE IF (LZOOM) THEN
            diff = (XAXIS(2)-XAXIS(1))
            mjdf = xx + diff/FACTOR/2.0 + mjds
            new_mjds = xx - diff/FACTOR/2.0 + mjds
            REPLOT=.TRUE.
          ELSE IF (LUNZOOM) THEN
            diff = (XAXIS(2)-XAXIS(1))
            mjdf=min(real(xx+diff*FACTOR/2.0+mjds),
     &              real(original_mjdf))
            new_mjds=max(real(xx-diff*FACTOR/2.0+mjds),
     &              real(original_mjds))
            REPLOT=.TRUE.
          ELSE IF (LSETEPOCH) THEN
            CALL SELPLFITORBITDIALOG(1)
            II = ((mjds+xx)-TASC)/porb
            TASC = TASC + II*porb
          ELSE IF (LDELETE) THEN
            diffx = abs(XAXIS(2)-XAXIS(1))
            CALL QPLFITORBITDIALOG(1,
     &          PLOTXMIN,PLOTXMAX,PLOTYMAX,PLOTYMIN)
            diffy = abs(PLOTYMAX-PLOTYMIN)
            closest=1.0e30
            iclosest = 1
            do i=1,npts
              if(iflag(i).lt.0) then
                if (last_plot_type.eq.1)then
                  dist = sqrt( ((xx-x1(I))/diffx)**2  +
     &                  ( (yy-y1(I))/diffy)**2)
                else
                  dist = sqrt( ((xx-x1(I))/diffx)**2  +
     &                  ( (yy-yres(I))/diffy)**2)
                end if
                if (dist .lt. closest) then
                  closest=dist
                  iclosest = i
                end if
              end if
            end do
            IFLAG(iclosest)=abs(iflag(iclosest))
            CALL SELPLFITORBITDIALOG(1)
            call pgsch(2.)
            call pgsci(7)
            if (last_plot_type.eq.1)then
              call pgpoint(1,x1(iclosest),y1(iclosest),5)
            else
              call pgpoint(1,x1(iclosest),yres(iclosest),5)
            end if
            call pgsch(1.)
            call pgsci(1)
            CALL DIALOG_RESET
          ELSE IF (LRESTORE) THEN
            diffx = abs(XAXIS(2)-XAXIS(1))
            CALL QPLFITORBITDIALOG(1,
     &          PLOTXMIN,PLOTXMAX,PLOTYMAX,PLOTYMIN)
            diffy = abs(PLOTYMAX-PLOTYMIN)
            closest=1.0e30
            iclosest = 1
            do i=1,npts
              if(iflag(i).gt.0) then
                if (last_plot_type.eq.1)then
                  dist = sqrt( ((xx-x1(I))/diffx)**2  +
     &                  ( (yy-y1(I))/diffy)**2)
                else
                  dist = sqrt( ((xx-x1(I))/diffx)**2  +
     &                  ( (yy-yres(I))/diffy)**2)
                end if
                if (dist .lt. closest) then
                  closest=dist
                  iclosest = i
                end if
              end if
            end do
            iflag(iclosest)=-abs(iflag(iclosest))
            CALL SELPLFITORBITDIALOG(1)
            call pgsch(2.)
            call pgsci(7)
            if (last_plot_type.eq.1)then
              call pgpoint
     &          (1,x1(iclosest),y1(iclosest),abs(iflag(iclosest)))
            else
              call pgpoint
     &          (1,x1(iclosest),yres(iclosest),abs(iflag(iclosest)))
            end if
            call pgsch(1.)
            call pgsci(1)
            CALL DIALOG_RESET
          ELSE IF (LIDENTIFY) THEN
            diffx = abs(XAXIS(2)-XAXIS(1))
            CALL QPLFITORBITDIALOG(1,
     &          PLOTXMIN,PLOTXMAX,PLOTYMAX,PLOTYMIN)
            diffy = abs(PLOTYMAX-PLOTYMIN)
            closest=1.0e30
            iclosest = 1
            do i=1,npts
              if(iflag(i).lt.0) then
                if (last_plot_type.eq.1)then
                  dist = sqrt( ((xx-x1(I))/diffx)**2  +
     &                  ( (yy-y1(I))/diffy)**2)
                else
                  dist = sqrt( ((xx-x1(I))/diffx)**2  +
     &                  ( (yy-yres(I))/diffy)**2)
                end if
                if (dist .lt. closest) then
                  closest=dist
                  iclosest = i
                end if
              end if
            end do
            IF(LEPOCH) WRITE (6,666) MJDS+x3(iclosest),
     &            y1(iclosest),yres(iclosest)
  666 FORMAT ('Epoch:',F12.5,'  Period Offset: ',F10.6,'  Res: ',F10.6)
            IF(LPHASE) WRITE (6,667) MJDS+x3(iclosest),x1(iclosest),
     &            y1(iclosest),yres(iclosest)
  667 FORMAT ('Epoch:',F12.5,' Phase: ',F5.3,
     &            ' Period Offset: ',F10.6,'  Res: ',F10.6)

c            IFLAG(iclosest)=0
          END IF
        ELSE IF (plotno.EQ.2) THEN
          IF (LSTART) THEN
            CALL SELPLFITORBITDIALOG(2)
            NEW_MJDS = XX
            call pgsci(4)
            call pgslw(3)
            call pgmove(xx,-1.0)
            call pgdraw(xx,1.0)
            call pgslw(1)
            call pgsci(1)
            CALL DIALOG_RESET
          ELSE IF (LFINISH) THEN
            CALL SELPLFITORBITDIALOG(2)
            MJDF = XX
            call pgsci(4)
            call pgslw(3)
            call pgmove(xx,-1.0)
            call pgdraw(xx,1.0)
            call pgslw(1)
            call pgsci(1)
            CALL DIALOG_RESET
          ELSE
            CALL XPLFITORBITDIALOG(2,0.0,0.0,0.0,0.0)
            CALL SELPLFITORBITDIALOG(2)
            Diff=(mjdf-mjds)/2.0
            call pgsci(4)
            call pgslw(3)
            new_mjds=max(real(xx-diff),real(original_mjds))
            mjdf=min(real(xx+diff),real(original_mjdf))
            CALL QPLFITORBITDIALOG(2,
     &          PLOTXMIN,PLOTXMAX,PLOTYMAX,PLOTYMIN)
            call pgmove(real(new_mjds),PLOTYMIN)
            call pgdraw(real(new_mjds),PLOTYMAX)
            call pgmove(real(mjdf),PLOTYMIN)
            call pgdraw(real(mjdf),PLOTYMAX)
            call pgslw(1)
            call pgsci(1)
            replot=.true.
            CALL DIALOG_RESET
          END IF
        end if

 999    continue
      end do
 888  continue
      call pgend
      end
C *************************************************************
      SUBROUTINE KEPLER(XMA,ECC,EA,NITS)
C *************************************************************
C
C  THIS PERFORMS AN ITERATIVE SOLUTION TO KEPLERS EQUATION FOR THE
C  ORBIT OF A BINARY SYSTEM.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (EACC=1D-10,RAD=57.29577951)
C
      EA=XMA + RAD*ECC*SIND(XMA)*(1+ECC*COSD(XMA))
C
      DO 10 I=1,10
      E=(XMA + RAD*ECC*SIND(EA) - EA*ECC*COSD(EA))/(1-ECC*COSD(EA))
      IF(ABS(EA-E).LT.EACC) GO TO 20
   10 EA=E
C
   20 EA=E
      EA=(XMA + RAD*ECC*SIND(EA) - EA*ECC*COSD(EA))/(1-ECC*COSD(EA))
      NITS=I
      END
C
C *************************************************************
      SUBROUTINE BINARY(I,X,NX,Y,W,PAR,NPAR,RES,GRAD,ICON,IFL)
C *************************************************************
C
C SUBROUTINE TO CALCULATE THE PERIOD OF A BINARY PULSAR FOR LMM.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NPTMAX=500)
      DATA PI/3.1415926537D0/
      REAL*8 K,OMEGA,MEANANOM,P,P0,T,TASC,PBIN,PORB,DRA,RA0,DDEC,DEC0,
     &   XX,YY,SIG,DV,C
      COMMON P0,TASC,PORB,RA0,DEC0,XX(NPTMAX),YY(NPTMAX),
     &   SIG(NPTMAX),WEIGHT
      CHARACTER WEIGHT*3
      DIMENSION X(*),Y(*),PAR(*),GRAD(*),ICON(*)
C
      C=2.99792458D8
      RAD=180/3.141592654D0
C
      P     = P0 + PAR(1)
      PDOT  = PAR(2)
      PBIN  = PORB + PAR(3)
      T     = TASC + PAR(4)
      ASINI = PAR(5)
      OMEGA = PAR(6)
      ECC   = PAR(7)
      DRA   = PAR(8)
      DDEC  = PAR(9)
C
C Calculate the differential velocity.
      CALL DELTAV(X(1),RA0,DEC0,DRA,DDEC,2000.0D0,DV)
      K=2.0D0*PI*ASINI/(PBIN*DBLE(24.0*3600.0)*SQRT(1.0D0-ECC**2))
C
C  Calculate the Mean Anomaly, MEANANOM.
C
      EASC = 2D0*ATAND(SQRT((1-ECC)/(1+ECC))*TAND(-OMEGA/2))
      EPPERI = T - PBIN*(EASC - 180/PI*ECC*SIND(EASC))/360D0
      MEANANOM=360.0D0*(X(1)-EPPERI)/PBIN
      MEANANOM=MOD(MEANANOM,360.0D0)
      IF (MEANANOM.LT.0D0) MEANANOM = MEANANOM + 360.0D0
C
C  Calculate the Eccentric Anomaly, ECCANOM.
C
      CALL KEPLER(MEANANOM,ECC,ECCANOM,NITS)
C
C  Calculate the True Anomaly, TRUEANOM.
C
      TRUEANOM=2.0D0*ATAND(SQRT((1.0D0+ECC)/(1.0D0-ECC))
     &          *TAND(ECCANOM/2.0D0))
C
C  Calculate the expected period.
C
      PER=(P + PDOT*(X(1)-T)*86400D-15)*
     &               (1.0D0 + K*DCOSD(TRUEANOM+OMEGA)
     &               + K*ECC*DCOSD(OMEGA))*(1.0D0-DV/C)
      RES=(Y(1)-PER)*1D6
C
C  If a weighted fit is required, calculate the weight.
C
      IF (WEIGHT(:2).EQ.'ON'.OR.WEIGHT(:2).EQ.'on') THEN
        W=(1D-6/SIG(I))**2
      ELSE
        W=1.0D0
      ENDIF
C
      END
C
C *************************************************************
      SUBROUTINE DELTAV(MJD,RA,DEC,DRA,DDEC,EQX,DV)
C *************************************************************
C
C SUBROUTINE TO CALCULATE DIFFERENCE DV(M/S) BETWEEN THE EARTH'S VELOCITY 
C   COMPONENTS IN TWO DIRECTIONS, (RA,DEC) AND (RA+DRA,DEC+DDEC) AT 
C   A GIVEN MJD.  COORDS ARE EPOCH OF DATE, GIVEN IN DEGREES.

      IMPLICIT NONE
      REAL*8 MJD,RA,DEC,DRA,DDEC,EQX,DV,DVB(3),DPB(3),DVH(3),DPH(3),
     &       POS0(3),POS1(3),sla_DVDV,RAD
C
      RAD=180.0D0/3.141592654D0
      CALL sla_DCS2C(RA/RAD,DEC/RAD,POS0)
      CALL sla_DCS2C((RA+DRA)/RAD,(DEC+DDEC)/RAD,POS1)
      CALL sla_EVP(MJD,EQX,DVB,DPB,DVH,DPH)
      DV = sla_DVDV(DVB,POS1) - sla_DVDV(DVB,POS0)
      DV = DV*149600.0D6
C
C     WRITE(*,*)MJD,RA,DEC,DRA,DDEC,POS0,POS1,DVB,DV

      END
C
C**********************************************************************
