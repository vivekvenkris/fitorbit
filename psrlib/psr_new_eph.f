C *************************************************************
      SUBROUTINE PSR_new_EPH (EPOCH, ITELNO, RA50, DEC50, PBEPOCH, 
     &    PB,  PBDOT, PBDDOT,VTRDOT,
     &    EPBIN, PBIN, ASINI, WBIN, wdot,ECC,
     &    TOBS, POBS, XMA)
C *************************************************************

      IMPLICIT NONE

      REAL*8 EPOCH, RA50, DEC50, PBEPOCH, PB, PBDOT, EPBIN, PBIN, 
     &       ASINI, WBIN, ECC, TOBS, POBS, XMA, PBDDOT,
     &       wdot,vtrdot
      INTEGER ITELNO

*+
* Subroutine to compute the arrival time and period at the telescope.
* All arguments are in double precision.
c uses the jplephemeris as per psrtime, but working in J2000. rsp 1.3.96
* The value of dmconst brought in line 11/1/95. rsp
* Pddot effect added 17.1.95 rsp
* vtrdot       added 8th aug 2006 caj
* Given:
*    EPOCH       (DP sec)  The MJD of the observation.
*    ITELNO      (I)       Telescope number.
*    RA50,DEC50  (DP deg)  The RA and DEC of the pulsar in degrees.
*    PBEPOCH     (DP sec)  The barycentric MJD of a pulse and epoch of PB.
*    PB          (DP sec)  The barycentric period at PBEPOCH. If negative,
*                            PBEPOCH, PB and PBDOT are already topocentric.
*    PBDOT       (DP s/s)  The barycentric period derivative.
*    PDDOT       (DP s/s**2) The second barycentric period derivative.
*    VTRDOT      (DP s/s**3) third frequency derivative
*    EPBIN       (DP sec)  The barycentric MJD of binary periastron.
*    PBIN        (DP sec)  The binary barycentric period.
*    ASINI       (DP sec)  The binary major semi-axis.
*    WBIN        (DP deg)  The binary longitude of periastron.
*    WDOT        (DP deg/year) The binary longitude first derivative.
*    ECC         (DP  - )  The binary orbital eccentricity.
*    
*
* Returned:
*    TOBS        (DP sec)  The MJD of the next pulse at the telescope.
*    POBS        (DP sec)  The period of the pulses at the telescope.
*    XMA         (DP deg)  Mean anomaly.
*-

* Include telescope information.
      INCLUDE 'TEL.DEF'

* Define astronomical and mathematical constants.
      REAL*8 AUS, ETTAI, DMCONST, TWOPI, RTOD, STRAD, SARAD, REARTH,
     &       C, SOLSID
      PARAMETER ( AUS     = 499.0047837D0
     &           ,ETTAI   = 32.184D0
     &           ,DMCONST = 2.4100000D-4
     &           ,TWOPI   = 6.2831853072D0
     &           ,RTOD  = 360D0/TWOPI
     &           ,STRAD   = TWOPI/86400D0
     &           ,SARAD   = STRAD/15D0
     &           ,REARTH  = 6378.16D0
     &           ,C       = 299792.5D0
     &           ,SOLSID  = 1.00273790931D0 )

      REAL*8 sla_GMST, sla_DVDV,sla_EQEQX,sla_rcc

* Local variables.
      REAL*8 R1950, D1950, R2000, D2000, DVB(3), DPB(3),
     &       DPS(3), DPVE(6), OBSLONG, OBSLAT, OBSHT, EVEL, EDELAY,
     &       BVEL, TDT, ST, BTDB,grvcor,
     &       TDB, DOPP,utctai,dumdet,vector(6),R41950,D41950,DR, DD,
     &       stm,doppe,doppb,x,omega,v,vdot,vddot,phase,e,
     &    e2,ta,f,s,g,r,q,pinst,offset,hh
      
      INTEGER I,aa,bb,nits
      
* Convert coordinates to radians.
      R1950 = RA50/RTOD
      D1950 = DEC50/RTOD

* Convert the time to TDT.
      TDT = EPOCH + utctai (EPOCH/86400.0D0) + ettai

* Get the observatory position wrt the geocentre
      OBSLONG = L(ITELNO)
      OBSLAT = B(ITELNO)
      OBSHT = H(ITELNO)

* Convert TDT to TDB.
c      Gg = (357.53 + 0.9856003 * (EPOCH / 86400.0 - 51544.5)) / RTOD
c      TDB = TDT + 0.001658 * SIN (Gg) + 0.000014 * SIN (2 * Gg)

      GRVCOR = SLA_RCC(TDT/86400d0,mod(epoch,86400d0)/86400d0
     &                  ,-obslong
     &                  ,REARTH*cos(obslat)
     &                  ,REARTH*sin(obslat))
C
      TDB = TDT + GRVCOR

      STm = sla_GMST (EPOCH/86400.0D0) + OBSLONG
      st  = stm + sla_EQEQX(TDB/86400.0d0)
      CALL sla_PVOBS (OBSLAT, OBSHT, ST, DPVE)
*
c Add E-terms
      call sla_addet(R1950,D1950,1950.0d0,R41950,D41950)
c
* Convert source coords to J2000.0
      CALL sla_FK45Z (R41950, D41950, 1950.0D0, R2000, D2000, DR, DD)
*
* Convert to DCS
      CALL sla_DCS2C (R2000, D2000, DPS)

*
* Get the Earth's velocity and position using TDB
c  copy of part of jplepv.f rsp.
      aa = 3
      bb = 12
      dumdet = tdb/86400d0 + 2400000.5d0
      call pleph (dumdet,aa,bb,vector)
c
* Add the EC-observatory vectors to the barycentre-EC vectors.
      DO I=1,3
        DVB(I) = vector(I+3)/86400d0 + DPVE(I+3)
        DPB(I) = vector(I) + DPVE(I)
      ENDDO
*
* Calculate the components of velocity and position towards pulsar.
      EVEL    = AUS*sla_DVDV(DVB,DPS)         ! sec/sec
      EDELAY  = AUS*sla_DVDV(DPB,DPS)         ! sec
      BTDB    = TDB + EDELAY
*
c Calculate the Doppler correction fot earth vel.
      doppe   = sqrt((1-evel)/(1+evel))
      doppb   = 1d0
c
c Calculate the pulse phase in periods
      x     = btdb - pbepoch
      omega = wbin + wdot*((btdb-epbin)/(86400d0*365.25))
c
c Calculate pulsar freq. and derivatives fro period derivatives
      v     = 1.0d0/pb
      vdot  = -pbdot/pb**2
      if(pbddot.eq.0d0) then
         vddot = 2.0d0*pbdot**2/pb**3
      else
         vddot = 2.0d0*pbdot**2/pb**3 - pbddot/pb**2
      endif
c
c Pulsar phase - vtrdot added aug2006
      phase = v*x + vdot*x**2/2d0 + vddot*x**3/6d0 + vtrdot*x**4/24d0
c
c Calculate the binary delay, velocity if appropriate
      if(pbin.ne.0d0) then
c
c Calculate the mean anomaly
         xma = (btdb-epbin)/pbin
         xma = 360d0*mod(xma,1d0)
         if (xma.lt.0d0 ) xma = xma + 360.0d0
c
c Solve Keplers equation to get the eccentric anomaly
         call kepler(xma,ecc,e,nits)
c
c True anomaly, velocity due to binary motion, and its doppler
         e2   = sqrt(1d0-ecc**2)
         ta   = 2d0*atand(sqrt((1d0+ecc)/(1d0-ecc))*tand(e/2d0))
         if (ta.lt.0d0)   ta = ta + 180d0
         if ( e.gt.180d0) ta = ta + 180d0
         bvel = twopi/pbin*asini*e2*
     &       (cosd(ta+omega)+ecc*cosd(omega))
         doppb  = sqrt((1+bvel)/(1-bvel))
*
c Binary phase from Manchester & Taylor p108
         g = asini*sind(omega)
         hh = asini*cosd(omega)*e2
         f = cosd(e)-ecc
         s = 1d0/(1d0-ecc*cosd(e))
         r = -g*sind(e) + hh*cosd(e)
         q = f*g + hh*sind(e)

         phase = phase + v*q*(twopi*r*s/pbin - 1d0) 
     &       - vdot*q*(x + (pbepoch-epbin))
      endif
c
c Period - mod use vtrdot
      pinst = 1d0/(v + vdot*x + vddot*x**2/2d0 + vtrdot*x**3/6d0)
      dopp = doppe*doppb
      pobs  = pinst*dopp
c
c The offset from epoch to the next arrival time is the fraction
c  of a cycle
      offset = mod(phase,1.0d0)*pinst
      if (offset.lt.0.0) offset = offset + pinst
c
c Calculate the arrival time
      tobs = (pinst-offset)*dopp

* End of subroutine PSREPH.
      END

C








