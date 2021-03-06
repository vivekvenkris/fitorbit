*DECK FOLD7
C
C ***********************************************************
      SUBROUTINE FOLD7 ( DAT, ND, P, PTOL, SCALE, PROF, NP,
     &                   SNRM, NWM, BUFFER, MAXBUF, IFAIL )
C ***********************************************************
C
C FOLDS THE DATA IN DAT(ND) AT PERIOD P PRODUCING A PROFILE OF
C LENGTH 1.25 PERIODS ( IN CASE THE PULSE IS NEAR TO PHASE ZERO ).
C
C A PERIOD SEARCH IS ALSO PERFORMED.
C THE DATA IS INITIALLY FOLDED IN QUARTERS AND THE FOUR PROFILES
C     THEN COMBINED AT SEVEN DISTINCT PERIODS.
C THE RANGE OF THE PERIOD SEARCH IS DETERMINED BY PTOL, THE 
C     FRACTIONAL PERIOD TOLERANCE, SUCH THAT THE SEVEN PERIODS
C     COVER THE RANGE +/- PTOL*P.
C THE BASELINE IS THEN FOUND AND A WIDTH/SNR SEARCH CONDUCTED FOR
C     EACH OF THE SEVEN PROFILES.
C THE BEST PROFILE IS RETURNED IN PROF(NP), SCALED BY SCALE AND
C     THE SQUARE ROOT OF THE NUMBER OF PERIODS. ITS PERIOD, SNR
C     AND WIDTH ARE RETURNED IN P, SNRM AND NWM.
C NOTE THAT IN ORDER TO OBTAIN A TRUE SIGNAL-TO-NOISE RATIO, SNRM
C     SHOULD BE DIVIDED BY THE RMS OF THE DATA.
C BUFFER(MAXBUF,13) IS A BUFFER ARRAY WHICH IS USED TO STORE THE
C     INTERMEDIATE PROFILES AND MAXBUF MUST BE SUFFICIENT TO HOLD
C     1.25 TIMES THE REQUESTED PERIOD.
C IFAIL IS ZERO UNLESS AN ERROR OCCURRED.
C
c Mod 10/03/2006 Correct data type in call to pserr
c
      DIMENSION DAT(*),PROF(*),BUFFER(MAXBUF,13)
      DOUBLE PRECISION P,XK
C
C     CLEAR ERROR FLAG.
C
      IFAIL = 0
C
C     COMPUTE IW FROM THE PROVIDED PERIOD TOLERANCE.
C     IW IS THE SHIFT BETWEEN FIRST AND LAST DATA QUARTERS,
C        PER PERIOD INTERVAL.
C
      IW = MAX(1,MIN(INT(P/4),INT(PTOL*0.75*ND/3.5)))
C
C     NDQ IS THE NUMBER OF ELEMENTS IN A DATA QUARTER.
C
      NDQ = ND/4
      NDQ2 = 2*NDQ
      NDQ3 = 3*NDQ
C
C     NP IS THE NUMBER OF ELEMENTS IN THE PROFILE.
C
      NP = 1.25*P+0.5
      JP = P+0.99999
      IF (NP.LE.JP) NP = JP+1
C
C     CHECK NP AGAINST THE BUFFER SIZE.
C
      IF ( NP.GT.MAXBUF ) THEN
         IFAIL = 41
         CALL PSRERR ('FOLD7',IFAIL,NP,0.,'Working')
         RETURN
      ENDIF
C
C     NPRD IS THE NUMBER OF PERIODS IN A DATA QUARTER.
C
      NPRD = (NDQ-0.25*P)/P
C
C     CHECK NPRD.
C
      IF ( NPRD.LE.0 ) THEN
         IFAIL = 13
         CALL PSRERR ('FOLD7',IFAIL,0,SNGL(P),'folding period')
         RETURN
      ENDIF
C
C     FOLD THE FOUR QUARTERS OF THE DATA AT PERIOD P INTO BUFFER 1-4.
C
      DO 10 J=1,NP
         BUFFER(J,1) = DAT(J)
         BUFFER(J,2) = DAT(J+NDQ)
         BUFFER(J,3) = DAT(J+NDQ2)
         BUFFER(J,4) = DAT(J+NDQ3)
   10 CONTINUE
      XK = P
      DO 20 IPRD=2,NPRD
         K  = XK
         K1 = K+NDQ
         K2 = K+NDQ2
         K3 = K+NDQ3
         DO 30 J=1,NP
            BUFFER(J,1) = BUFFER(J,1)+DAT(K +J)
            BUFFER(J,2) = BUFFER(J,2)+DAT(K1+J)
            BUFFER(J,3) = BUFFER(J,3)+DAT(K2+J)
            BUFFER(J,4) = BUFFER(J,4)+DAT(K3+J)
   30    CONTINUE
         XK = XK+P
   20 CONTINUE
C
C     ADD THE FOUR QUARTER PROFILES AT SEVEN DIFFERENT PERIODS.
C     THE PERIOD INTERVAL IS CONTROLLED BY IW.
C
C     FIRST ADD THE PROFILES IN PAIRS INTO BUFFER 8-13,
C      8-10 ARE FOR THE FIRST HALF OF THE DATA
C                  AT SHIFTS OF -1,0,+1 TIMES IW,
C     11-13 ARE FOR THE SECOND HALF OF THE DATA .
C
      KP = P+0.5
      K1 = P-MOD(DBLE(NDQ),P)+0.5
      K2 = P-MOD(DBLE(NDQ2),P)+0.5
      K3 = P-MOD(DBLE(NDQ3),P)+0.5
      K4 = IW
      K5 = IW+K1
      K6 = IW+K2
      K7 = IW+K3
      DO 40 J=1,NP
         K1 = K1+1
         IF ( K1.GT.KP ) K1 = 1
         K2 = K2+1
         IF ( K2.GT.KP ) K2 = 1
         K3 = K3+1
         IF ( K3.GT.KP ) K3 = 1
         K4 = K4+1
         IF ( K4.GT.KP ) K4 = 1
         K5 = K5+1
         IF ( K5.GT.KP ) K5 = 1
         K6 = K6+1
         IF ( K6.GT.KP ) K6 = 1
         K7 = K7+1
         IF ( K7.GT.KP ) K7 = 1
         BUFFER(J, 8) = BUFFER(K4,1)+BUFFER(K1,2)
         BUFFER(J, 9) = BUFFER( J,1)+BUFFER(K1,2)
         BUFFER(J,10) = BUFFER( J,1)+BUFFER(K5,2)
         BUFFER(J,11) = BUFFER(K6,3)+BUFFER(K3,4)
         BUFFER(J,12) = BUFFER(K2,3)+BUFFER(K3,4)
         BUFFER(J,13) = BUFFER(K2,3)+BUFFER(K7,4)
   40 CONTINUE
C
C     THEN ADD THESE PROFILES TO GIVE THE PROFILES AT
C     THE SEVEN PERIODS.
C
      K1 = IW
      K2 = 2*IW
      DO 50 J=1,NP
         K1 = K1+1
         IF ( K1.GT.KP ) K1 = 1
         K2 = K2+1
         IF ( K2.GT.KP ) K2 = 1
         BUFFER(J,1) = BUFFER(K2, 8)+BUFFER( J,11)
         BUFFER(J,2) = BUFFER(K1, 8)+BUFFER( J,11)
         BUFFER(J,3) = BUFFER(K1, 9)+BUFFER( J,12)
         BUFFER(J,4) = BUFFER( J, 9)+BUFFER( J,12)
         BUFFER(J,5) = BUFFER( J, 9)+BUFFER(K1,12)
         BUFFER(J,6) = BUFFER( J,10)+BUFFER(K1,13)
         BUFFER(J,7) = BUFFER( J,10)+BUFFER(K2,13)
   50 CONTINUE
C
C     FOR EACH OF THE SEVEN PROFILES SCAN FOR THE PULSE WIDTH
C     WHICH GIVES THE MAXIMUM SIGNAL/NOISE.
C
      KH = MIN(P-2.,0.75*P)
      IF (P.GT.8.0) KH = MAX(1,INT((P+2.0)/2.0))
      WRITE(*,*)P,KH
C
C     SCALING FACTOR, FAC, IS THE RECIPROCAL OF THE NUMBER OF
C     BINS ADDED INTO EACH PROFILE BIN.
C
      FAC = 1.0/(4.0*NPRD)
      DFAC = 1.0/(4.0*NPRD*SCALE)
      SFAC= SQRT(FAC)
      SNRM = 0.0
      DO 60 IP=1,7
         JSTEP = MAX(1.0D0,0.75*P-KH)
         LMIN = MAXINT()
         NMAX = -LMIN
         DO 70 J=1,KP,JSTEP
            L = 0
            M = J-1
            DO 80 K=1,KH
               M = M+1
               IF (M.GT.NP) M = M-KP
               NMAX = MAX(NMAX,INT(BUFFER(M,IP)))
               L = L+BUFFER(M,IP)
   80       CONTINUE
            LMIN = MIN(L,LMIN)
   70    CONTINUE
         AVE = FLOAT(LMIN)/KH
         SNR = SFAC*(NMAX-AVE)/SQRT(1.+1./KH)
C
C        IF SNR IS GREATER THAN PREVIOUS BEST, SAVE SNR, WIDTH AND
C        PERIOD COUNTER.
C
         IF(SNR.GT.SNRM) THEN
            SNRM = SNR
            NWM = 1
            IM = IP
         ENDIF
         IF (JP.LT.4) GOTO 1000
C
C        FOR PERIODS GREATER THAN 4,
C        LOOK AT SNR FOR WIDTHS 2,3,4 AND 6.
C
         DO 90 NN=2,5
            KAVE = NN
            IF (KAVE.EQ.5) KAVE = 6
            JSTEP = (KAVE+1)/3
            NMAX = -1000000
            DO 100 J=1,JP,JSTEP
               L = 0
               DO 110 K=1,KAVE
                  L = L+BUFFER(J-1+K,IP)
  110          CONTINUE
               NMAX = MAX(NMAX,L)
  100       CONTINUE
            SNR = SFAC*(NMAX-KAVE*AVE)/SQRT(KAVE*(1.+FLOAT(KAVE)/KH))
            IF(SNR.GT.SNRM) THEN
               SNRM = SNR
               NWM = KAVE
               IM = IP
            ENDIF
            IF (KAVE.GT.JP/4) GOTO 1000
   90    CONTINUE
 1000    CONTINUE
   60 CONTINUE
C
C     SCAN COMPLETE.
C
C     COMPUTE MEAN.
C
      PMEAN = 0.0
      DO 120 J=1,NP
         PMEAN = PMEAN + BUFFER(J,IM)
  120 CONTINUE
      PMEAN = PMEAN/NP
C
C     PUT BEST PROFILE IN PROF SUBTRACTING THE MEAN AND
C     SCALING BY THE NUMBER OF PERIODS AND SCALE.
C
      DO 130 J=1,NP
         PROF(J) = (BUFFER(J,IM)-PMEAN)*DFAC
  130 CONTINUE
C
C     COMPUTE THE NEW PERIOD ESTIMATE.
C
      P = P*(1.0+DBLE((IM-4)*IW)/(0.75*ND))
C
      RETURN
C
C END OF SUBROUTINE FOLD7.
C
      END
