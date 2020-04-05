      SUBROUTINE acq_PSREPHJPL (EPOCH, ITELNO, RA50, DEC50, PBEPOCH, 
     &                   PB, PBDOT, PBDDOT, EPBIN, PBIN, ASINI, WBIN, 
     &                   WDOT, ECC, TOBS, POBS, XMA, BTDB)

      IMPLICIT NONE

      REAL*8 EPOCH, RA50, DEC50, PBEPOCH, PB, PBDOT, EPBIN, PBIN, 
     &       WDOT, ASINI, WBIN, ECC, TOBS, POBS, XMA, RA20, DEC20, 
     &       PBDDOT, BTDB
      INTEGER ITELNO

*+
* Subroutine to compute the arrival time and period at the telescope.
* All arguments are in double precision.
* Precision (by calculation of TEMPO residuals):
*           Routine found to track within  .014 us in any 1 sec
*                                          .045 us in any 1 min.
*                                         2.3   us in any hour
*                                        10.0   us in any day
*                                       180.0   us in any year
* Modified: Jan 1995. DMCONST changed to 2.41D-4  CAJ.
*           Aug 1995. RA/DEC2000 coords entry point added.
*                     PBDDOT added (in arrtim).
*                     Convert apparent coords from PVOBS to mean.
*                     rewrite BINARY - to use method from Manchester&Taylor 
*                     substitute DEVP - a DP version of sla_EVP
*           Mar 1996. replace DEVP with PLEPH (JPL epehemeris routine)
*           July 1998. return BTDB
* Given:
*    EPOCH       (DP sec)  MJD in secs.
*    ITELNO      (I)       Telescope number.
*    RA50,DEC50  (DP deg)  The B1950 RA and DEC of the pulsar in degrees.
*    RA20,DEC20  (DP deg)  The J2000 RA and DEC of the pulsar in degrees.
*    PBEPOCHS    (DP sec)  The fractional part of the barycentric MJD
*    PB          (DP sec)  The barycentric period at PBEPOCH. If negative,
*                            PBEPOCH, PB and PBDOT are already topocentric.
*    PBDOT       (DP s/s)  The barycentric period derivative.
*    PBDDOT      (DP s/s2) The period second derivative.
*    EPBIN       (DP secs) The barycentric MJD of binary periastron.
*    PBIN        (DP sec)  The binary barycentric period.
*    ASINI       (DP sec)  The binary major semi-axis.
*    WBIN        (DP deg)  The binary longitude of periastron.
*    ECC         (DP  - )  The binary orbital eccentricity.
*
* Returned:
*    TOBS        (DP sec)  arrival time of the next pulse - EPOCH
*    POBS        (DP sec)  The period of the pulses at the telescope.
*    XMA         (DP deg)  Mean anomaly.
*    BTDB        (DP sec)  Equivalent Barycentric TDB MJD of observation.
*-

* Include HSL telescope information - changed to 'TEL.DEF' for offline version
*      INCLUDE 'TELDIR:TEL(TELGPFS)'
      INCLUDE 'TEL.DEF'

* Define astronomical and mathematical constants.
      REAL*8 AUS, ETTAI, DMCONST, TWOPI, RTOD, STRAD, SARAD, REARTH,
     &       C, SOLSID
      PARAMETER ( AUS     = 499.0047837D0
     &           ,ETTAI   = 32.184D0
     &           ,DMCONST = 2.4100000D-4
     &           ,TWOPI=6.28318530717958648D0
     &           ,RTOD  = 360D0/TWOPI
     &           ,STRAD   = TWOPI/86400D0
     &           ,SARAD   = STRAD/15D0
     &           ,REARTH  = 6378.16D0
     &           ,C       = 299792.5D0
     &           ,SOLSID  = 1.00273790931D0 )

      REAL*8 sla_DTT, sla_GMST, sla_DVDV, sla_EQEQX, utctai

* Local variables.
      REAL*8 R1950, D1950, R2000, D2000, DVB(3), DPB(3), DVH(3), DPH(3),
     &       DPS(3), DPVE(6), OBSLONG, OBSLAT, OBSHT, EVEL, EDELAY,
     &       EACCN, BVEL, TDT, ST, BDELAY, BACCN, PTDB, PTIME, 
     &       TDB, DOPP, A, B, R, AD, BD, RD, AM, BM, ADM, BDM, DPVA(6),
     &       AMPRMS(21),DOPPB, DOPPE, V, VDOT, X, VDDOT, PHASE,E2, E,
     &       RH, F, S, G, Q, Y, CYCLES, OFFSET, PINST,OMEGA, TA,
     &       FEPOCHS, FPBEPOCHS, STM, R41950, D41950
      INTEGER I, NITS, IT, IEPOCH, IPBEPOCH

* JPL definitions
      DOUBLE PRECISION JD,PV(3,2), PVSUN(6)
      LOGICAL KM,BARY
C
      COMMON/STCOMM/KM,BARY,PVSUN
C
      
* Convert coordinates to radians.
      R1950 = RA50/RTOD
      D1950 = DEC50/RTOD

* Add e-terms
      CALL sla_ADDET(R1950, D1950, 1950.0D0, R41950, D41950)
 
* Convert source coords to J2000.0
      CALL sla_FK45Z (R41950, D41950, 1950.0D0, R2000, D2000)
      GOTO 10

* entry for J2000 coords
      ENTRY acq_PSREPHJJPL (EPOCH, ITELNO, RA20, DEC20, PBEPOCH, PB, 
     &               PBDOT, PBDDOT, EPBIN, PBIN, ASINI, WBIN, WDOT,ECC, 
     &               TOBS, POBS, XMA,BTDB)

* Convert coordinates to radians.
      R2000 = RA20/RTOD
      D2000 = DEC20/RTOD

* Retrieve Jodrell telescope coords. 
!10    OBSLONG = G_TEL(ITELNO).L
!      OBSLAT = G_TEL(ITELNO).B
!      OBSHT = G_TEL(ITELNO).H
10    OBSLONG = L(ITELNO)
      OBSLAT = B(ITELNO)
      OBSHT = H(ITELNO)
*
* Convert to DCS
      CALL sla_DCS2C (R2000, D2000, DPS)
      
* divide Epoch and binary epoch into integer and fractional part (in secs)
      IEPOCH = INT(EPOCH/86400d0)
      FEPOCHS = EPOCH - IEPOCH*86400d0

      IPBEPOCH = INT(PBEPOCH/86400d0)
      FPBEPOCHS = PBEPOCH - IPBEPOCH*86400d0

* Find the difference between IEPOCH and TDT. (sla routine not up to date for
* leap seconds)
!      TDT = sla_DTT (IEPOCH+FEPOCHS/86400d0) +FEPOCHS
      TDT = utctai (IEPOCH+FEPOCHS/86400.0D0) + ettai + FEPOCHS

* Convert UTC to TDB. (integer and fraction)
      G = (357.53 + 0.9856003 * (IEPOCH + FEPOCHS/86400.0 - 51544.5)) 
      TDB = TDT + 0.001658 * SIND (G) + 0.000014 * SIND (2 * G)

      IT = IEPOCH
      DOWHILE (TDB .LT. 0d0 .OR. TDB .GE. 86400d0)
        IF (TDB .GE. 86400d0) THEN
          IT = IT + 1
          TDB = TDB -86400d0
        ELSEIF (TDB .LT. 0d0) THEN
          IT = IT - 1
          TDB = TDB +86400d0
        ENDIF
      ENDDO

* Get the observatory position wrt the geocentre
      STM = sla_GMST (IEPOCH + FEPOCHS/86400.0 ) + OBSLONG
      ST = STM +sla_EQEQX(IT+TDB/86400D0)
      CALL sla_PVOBS (OBSLAT, OBSHT, ST, DPVE)
!      CALL sla_PVOBS (OBSLAT, OBSHT, ST, DPVA)

* DPVA in geocentric apparent - convert to spherical coords, then to 
* mean place
!      CALL sla_DCC2S(DPVA(1), A, B)
!      R = SQRT(DPVA(1)**2+DPVA(2)**2+DPVA(3)**2)
!      CALL sla_DCC2S(DPVA(4), AD, BD)
!      RD = SQRT(DPVA(4)**2+DPVA(5)**2+DPVA(6)**2)

!      CALL sla_MAPPA(2000D0, IT +TDB/86400D0, AMPRMS)
!      CALL sla_AMPQK(A, B, AMPRMS, AM, BM)
!      CALL sla_AMPQK(AD, BD, AMPRMS, ADM, BDM)

* then back again
!      CALL sla_DCS2C(AM, BM, DPVE(1))
!      CALL sla_DCS2C(ADM, BDM, DPVE(4))
!      DO I = 1,3
!        DPVE(I) = DPVE(I)*R 
!        DPVE(I+3) = DPVE(I+3)*RD
!      ENDDO

* Get the Earth's velocity and position using TDB - using special double 
* precision version of sla_EVP
!      CALL DEVP (IT+TDB/86400.0D0, 2000.0D0, DVB, DPB, DVH, DPH)

* Try out a JPL library version
      KM =0 
      BARY = 0
      JD = IT + TDB/86400d0 + 2400000.5
      CALL PLEPH(JD, 3,12, PV)

      DO I = 1,3
        DPB(I) = PV(I,1)
        DVB(I) = PV(I,2)/86400d0
      ENDDO

* Add the EC-observatory vectors to the barycentre-EC vectors.
      DO I=1,3
        DVB(I) = DVB(I) + DPVE(I+3)
        DPB(I) = DPB(I) + DPVE(I)
      ENDDO
*
* Calculate the components of velocity and position towards pulsar.
      EVEL    = AUS*sla_DVDV(DVB,DPS)         ! sec/sec
      EDELAY  = AUS*sla_DVDV(DPB,DPS)         ! sec
      BTDB    = TDB + EDELAY

      DO WHILE (BTDB .GE. 86400D0 .OR. BTDB .LT. 0d0)
        IF (BTDB .GE. 86400d0) THEN
          IT = IT+1
          BTDB = BTDB -86400d0
        ELSEIF (BTDB .LT. 0d0) THEN
          IT = IT-1
          BTDB = BTDB +86400d0
        ENDIF
      ENDDO

* Calculate the Doppler correction for Earth vel.
      DOPPE = SQRT((1-EVEL)/(1+EVEL))
      DOPPB =1D0

* Calc the pulse phase in periods
      X = (IT-IPBEPOCH)*86400d0 + BTDB - FPBEPOCHS
      OMEGA = WBIN + WDOT * (IT*86400d0 -EPBIN)+WDOT*BTDB

* Calculate pulsar freq. and derivatives from period derivatives
      V     = 1.0D0/PB
      VDOT  = - PBDOT / PB**2
      IF (PBDDOT .EQ. 0d0) THEN
        VDDOT = 0d0
      ELSE
        VDDOT = 2.0D0 * PBDOT**2 / PB**3 - PBDDOT / PB**2
      ENDIF

* Pulsar phase
      PHASE = V*X + VDOT*X**2/2D0 + VDDOT*X**3/6D0
*
* Calculate the binary delay,velocity if appropriate.
      IF (PBIN.NE.0D0) THEN

* Calculate the mean anomaly.
        XMA = (IT*86400d0 - EPBIN)/PBIN + BTDB/PBIN
        XMA = 360D0 * MOD(XMA,1D0)
        IF (XMA .LT. 0D0) XMA = XMA + 360D0
*
* Solve Kepler's equation to get the eccentric anomaly.
        CALL KEPLER (XMA,ECC,E,NITS)

* True anomaly, velocity due to binary motion, and its doppler
        E2 = SQRT(1D0-ECC**2)
        TA = 2D0 * ATAND(SQRT((1D0+ECC)/(1D0-ECC)) *TAND(E/2D0))
        BVEL = TWOPI/PBIN*ASINI*E2* 
     &              (COSD(TA+OMEGA) +ECC*COSD(OMEGA))
        DOPPB = SQRT((1+BVEL)/(1-BVEL))

* Binary phase from Manchester & Taylor p108
        G = ASINI * SIND(OMEGA)
        RH = ASINI * COSD(OMEGA) * E2
        F = COSD(E) - ECC
        S = 1D0/(1D0 - ECC*COSD(E))
        R = -G*SIND(E) + RH*COSD(E) 
        Q = F*G + RH*SIND(E)
        
        PHASE = PHASE + V*Q*(TWOPI*R*S/PBIN - 1D0)- VDOT*Q*X

      ENDIF

* Period
      PINST = 1D0/(V + VDOT*X +VDDOT*X**2/2D0)
      POBS = PINST * DOPPE * DOPPB
C
C The offset from EPOCH to the next arrival time is the fraction 
C of a cycle.
      OFFSET = MOD(PHASE,1.0D0)*PINST
      IF ( OFFSET.LT.0.0 ) OFFSET = OFFSET + PINST
C
C Calculate the arrival time.
C
      TOBS = (PINST-OFFSET)*DOPPE*DOPPB 

* and the total BTDB
      BTDB = IT*86400d0 + BTDB

* End of subroutine acq_PSREPHJPL.
      END


