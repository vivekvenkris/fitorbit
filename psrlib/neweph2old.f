      SUBROUTINE NEWEPH2OLD(parmStatus, value_str, value_double,
     &             value_integer, error_double, OVALS, ERRS)

      IMPLICIT NONE
C
C ROUTINE TO convert the new array of eph values and errors to a new array
C         - mostly changing frequencies to periods, and rearranging
c         - zeros all vals and errors not found
c dec 1998 caj
c mod apr 2005 caj force tasc and pmepoch if not found in eph file.
 
c include new file key name definitions (/psr/lib) and arrays
      include 'keyinfo.com'
      REAL*8 OVALS(*), ERRS(*)

c and old file key name assignments and arrays
      include 'OEPH_KEY.COM'

c flags for whether an eph variable exists, and places to write them.
      INTEGER parmStatus(NUM_KEYS)
      CHARACTER*32 value_str(NUM_KEYS)
      REAL*8 value_double(NUM_KEYS)
      INTEGER value_integer(NUM_KEYS)
      REAL*8 error_double(NUM_KEYS)

c Declare variables:
      INTEGER I
      REAL*8 V,VD,P,PD,PDD,VDD,R41950,D41950,RAJ,DECJ, DR, DD, RAB, DECB
      REAL*8 ERRP, ERRPD, ERRPDD
      REAL*8 f0relerr, f1relerr, v0relerr, p1relerr, p0relerr, DBLMIN

c constants
      include 'PSRMATH.DEF;2'

c zero arrays 
      do I = 1, OEPH_MAX
        OVALS(I) = 0d0
        ERRS(I)=0d0
      enddo

c Pigeon-hole new eph data into old eph arrays
c
c RA,DEC in J2000 --> B1950
c RA needs to be in degrees (returned in turns)
c first get RA in radians
      if (parmStatus(EPH_RAJ) .ne. 0  .and.
     &      parmStatus(EPH_DECJ) .ne. 0) then
        RAJ = value_double(EPH_RAJ)*360./raddeg
        DECJ = value_double(EPH_DECJ)*360./raddeg

c convert J2000 to B1950
        CALL SLA_FK54Z(RAJ, DECJ, 1950.0D0, R41950,D41950, DR, DD)

c subtract earth terms
        CALL SLA_SUBET(R41950, D41950, 1950.0D0, RAB, DECB)

c change to degrees
        OVALS(OEPH_RAJ) = RAJ*raddeg
        OVALS(OEPH_DECJ) = DECJ*raddeg
        OVALS(OEPH_RAB) = RAB*raddeg
        OVALS(OEPH_DECB) = DECB*raddeg

c RA & DEC errors (secs/arcsecs  --> degrees)
        ERRS(OEPH_RAJ) = error_double(EPH_RAJ)/240d0
        ERRS(OEPH_DECJ) = error_double(EPH_DECJ)/3600d0
        ERRS(OEPH_RAB) = error_double(EPH_RAJ)/240d0
        ERRS(OEPH_DECB) = error_double(EPH_DECJ)/3600d0
      endif

c PBEPOCH as integer value and fractional part
      if (parmStatus(EPH_PEPOCH) .ne. 0) then
        OVALS(OEPH_PEPOCH) = dble(value_integer(EPH_PEPOCH)) +
     &              value_double(EPH_PEPOCH)
        ERRS(OEPH_PEPOCH) = error_double(EPH_PEPOCH)
      endif

* Period - Eph stores frequency in Hz, change, then tell program that units 
* are sec.
      V = value_double(EPH_F)
      P = 0d0
      ERRP = 0d0
      if (parmStatus(EPH_F) .ne. 0 .and. V .ne. 0d0) then
        P = 1.0D0/V
!        idunits(4)= 13

* need error in period not freq.
        f0relerr = error_double(EPH_F)/V
        ERRP = P * v0relerr
      endif

c now things get worser - vdot to pdot
      VD = value_double(EPH_F1)
      PD = 0d0
      ERRPD=0d0
      if (parmStatus(EPH_F1) .ne. 0 .and. VD .ne. 0d0) then
        PD = -P*P*VD
!        idunits(7) = 46

c aargh! vdot error to pdot err
        f1relerr =  error_double(EPH_F1)/VD
        p1relerr= sqrt(f1relerr*f1relerr - 4d0 * p0relerr*p0relerr)
        ERRPD = PD * p1relerr
      endif

c vddot to pddot
      VDD = value_double(EPH_F2)
      PDD = 0d0
      ERRPDD = 0d0
      if (parmStatus(EPH_F2) .ne. 0 .and. V .ne. 0d0 .and.
     &     VD .ne. 0d0 .and. VDD .ne. 0d0) then

C CHECK THAT VD IS NOT SO SMALL THAT IT WILL CAUSE AN UNDERFLOW WHEN SQUARED. 
        IF (ABS(VD).LT.SQRT(DBLMIN()) ) THEN

C NEGLECT THE VDOT TERM.
          PDD = - VDD/V**2
        ELSE

C FULL COMPUTATION.
          PDD = 2.0*VD**2/V**3 - VDD/V**2
        ENDIF
!        idunits(8) = 26

c double aargh! vddot error to pddot err
        ERRPDD = sqrt(error_double(EPH_F2) - 
     &           (VD * f1relerr * 4d0 * VD/V)**2 -
     &           (V * f0relerr * 2d0 * (VD*VD/(V*V) + PDD*V))**2)/(P*P) 
      endif

      OVALS(OEPH_P) = P
      OVALS(OEPH_PDOT) = PD*1d15
      OVALS(OEPH_PDDOT)= PDD*1d24
      ERRS(OEPH_P) = ERRP
      ERRS(OEPH_PDOT) = ERRPD*1d15
      ERRS(OEPH_PDDOT) = ERRPDD*1d24

c VTRDOT 
      if (parmStatus(EPH_F3) .ne. 0) 
     &      OVALS(OEPH_VTRDOT) = value_double(EPH_F3)*1d30

c PMRA PMDEC
      if (parmStatus(EPH_PMRA).ne.0)then
        OVALS(OEPH_PMRA) =value_double(EPH_PMRA)
        ERRS(OEPH_PMRA) = error_double(EPH_PMRA)
      endif

      if (parmStatus(EPH_PMDEC).ne.0)then
        OVALS(OEPH_PMDEC) =value_double(EPH_PMDEC)
        ERRS(OEPH_PMDEC) = error_double(EPH_PMDEC)
      endif

c Posepoch as integer and fraction aka PMEPO
      if (parmStatus(EPH_POSEPOCH) .ne. 0) 
     &     OVALS(OEPH_PMEPOCH) = DBLE(value_integer(EPH_POSEPOCH)) +
     &                          value_double(EPH_POSEPOCH) 

c DM
      if (parmStatus(EPH_DM) .ne. 0) then
        OVALS(OEPH_DM) = value_double(EPH_DM)
        ERRs(OEPH_DM) = error_double(EPH_DM)
      endif

c 35 = EPH_START 36 = EPH_FINISH (are these the same as FST, FFIN 
c Epoch of start of fit and end of fit)!!!!
      if (parmStatus(EPH_START) .ne. 0)
     &  OVALS(OEPH_START) = DBLE(value_integer(EPH_START)) + 
     &               value_double(EPH_START)
      if (parmStatus(EPH_FINISH) .ne. 0)
     &   OVALS(OEPH_FINISH) = DBLE(value_integer(EPH_FINISH)) + 
     &                       value_double(EPH_FINISH)
      if (OVALS(OEPH_START) .eq. 0d0) OVALS(OEPH_START) = -1d0
      if (OVALS(OEPH_FINISH) .eq. 0d0) OVALS(OEPH_FINISH) = -1d0

c SCAT not used in new .eph files
      OVALS(OEPH_SCATTER) = 0d0

c !!! 49 = T0 (epoch of periastron of first orbit) query = epbin
      if (parmStatus(EPH_T0) .ne. 0) then
        OVALS(OEPH_T0) = DBLE(value_integer(EPH_T0)) +
     &                                value_double(EPH_T0)
        ERRS(OEPH_T0)= error_double(EPH_T0)

c 34 = TASC - from BT ELL1. map to T0
      elseif (parmStatus(EPH_TASC) .ne. 0) then
        OVALS(OEPH_T0) = DBLE(value_integer(EPH_TASC)) +
     &                                value_double(EPH_TASC)
        ERRS(OEPH_T0)= error_double(EPH_TASC)
      endif

cc PBIN in days in .eph file.
      if (parmStatus(EPH_PB) .ne. 0 ) then
        OVALS(OEPH_PB) = value_double(EPH_PB)
        ERRS(OEPH_PB) =  error_double(EPH_PB)
      endif

c!!! ASINI - binary major semi-axis aka 43 = A1      
      if (parmStatus(EPH_A1) .ne. 0) then
        OVALS(OEPH_A1) = value_double(EPH_A1)
        ERRS(OEPH_A1) = error_double(EPH_A1)
      endif

c WBIN = longtitude of periastron = omega (deg) = OM = 55
      if ( parmStatus(EPH_OM) .ne. 0) then
        OVALS(OEPH_OM) = value_double(EPH_OM) 
        ERRS(OEPH_OM) = error_double(EPH_OM)

c BT ELL1 use EPS1 and 2 (113, 114) to calculate E and OM      
      elseif ( parmStatus(EPH_EPS_1) .ne. 0 .and. 
     &              parmStatus(EPH_EPS_2) .ne. 0 ) then

        OVALS(OEPH_OM) = 
     &     datan2(value_double(EPH_EPS_2),value_double(EPH_EPS_1))
c errors - buggerit, I'll do this properly later! 
        ERRS(OEPH_OM) = 
     &     sqrt((error_double(EPH_EPS_1) + error_double(EPH_EPS_2))*
     &          (error_double(EPH_EPS_1) + error_double(EPH_EPS_2)))/2.

c Now use this to correct T0
        OVALS(OEPH_T0) = OVALS(OEPH_T0) +  
     &          OVALS(OEPH_OM)*OVALS(OEPH_PB)/360d0
      endif
c WDOT deriv of WBIN degrees per year
      if (parmStatus(EPH_OMDOT) .ne. 0) then
        OVALS(OEPH_OMDOT) = value_double(EPH_OMDOT)
        ERRS(OEPH_OMDOT) = error_double(EPH_OMDOT)
      endif

c ECCBIN. Binary eccentricity aka E
      if (parmStatus(EPH_E) .ne. 0) then
        OVALS(OEPH_E) = value_double(EPH_E)
        ERRS(OEPH_E) = error_double(EPH_E)

c BT ELL1 use EPS1 and 2 (113, 114) to calculate E and OM      
      elseif ( parmStatus(EPH_EPS_1) .ne. 0 .and. 
     &              parmStatus(EPH_EPS_2) .ne. 0 ) then
        OVALS(OEPH_E) = 
     &     sqrt((value_double(EPH_EPS_1) * value_double(EPH_EPS_1)) +
     &          (value_double(EPH_EPS_2) * value_double(EPH_EPS_2)))

c guessing here!
        ERRS(OEPH_E) = 
     &     sqrt((error_double(EPH_EPS_1) + error_double(EPH_EPS_2))*
     &          (error_double(EPH_EPS_1) + error_double(EPH_EPS_2)))/2.


      endif

c!! INCBIN --> SINI =  sin of inclination angle = 66
      if ( parmStatus(EPH_SINI) .ne. 0) 
     &  OVALS(OEPH_INCBIN) = ASIN(value_double(EPH_SINI)/raddeg)

c MCBIN Companion Mass aka M2 =70
      if (parmStatus(EPH_M2) .ne. 0) 
     &      OVALS(OEPH_MCBIN) = value_double(EPH_M2) 

c!! PBINDOT old eph stored units of 10**-15. aka PBDOT stored units of 10**-12
      if (parmStatus(EPH_PBDOT) .ne. 0) 
     &    OVALS(OEPH_PBDOT) = value_double(EPH_PBDOT) * 1.0D3

c Second set of binary data
      if (parmStatus(EPH_T0_2) .ne. 0) OVALS(OEPH_T0_2) 
     &      = value_double(EPH_T0_2)+ DBLE(value_integer(EPH_T0_2))
      if (parmStatus(EPH_PB_2) .ne. 0) 
     &    OVALS(OEPH_PB_2) = value_double(EPH_PB_2)
      if (parmStatus(EPH_A1_2) .ne. 0)
     &    OVALS(OEPH_A1_2) = value_double(EPH_A1_2)
      if (parmStatus(EPH_OM_2).ne. 0)
     &    OVALS(OEPH_OM_2) =  value_double(EPH_OM_2)
      if (parmStatus(EPH_OMDOT_2).ne. 0)
     &    OVALS(OEPH_OMDOT_2) = value_double(EPH_OMDOT_2)
      if (parmStatus(EPH_E_2).ne. 0)
     &    OVALS(OEPH_E_2) = value_double(EPH_E_2)
      if (parmStatus(EPH_SINI_2).ne. 0)
     &    OVALS(OEPH_INCBIN_2) = ASIN(value_double(EPH_SINI_2)/raddeg)
      if (parmStatus(EPH_M2_2).ne. 0)
     &    OVALS(OEPH_MCBIN_2) = value_double(EPH_M2_2)
      if (parmStatus(EPH_PBDOT_2) .ne. 0)
     &    OVALS(OEPH_PBDOT_2) =  value_double(EPH_PBDOT_2)*1.0D3


c Third set of binary data
      if (parmStatus(EPH_T0_3) .ne. 0) 
     &   OVALS(OEPH_T0_3) = 
     &      value_double(EPH_T0_3)+ DBLE(value_integer(EPH_T0_3))
      if (parmStatus(EPH_PB_3) .ne. 0) 
     &   OVALS(OEPH_PB_3) = value_double(EPH_PB_3)
      if (parmStatus(EPH_A1_3) .ne. 0)
     &   OVALS(OEPH_A1_3) = value_double(EPH_A1_3)
      if (parmStatus(EPH_OM_3).ne. 0)
     &   OVALS(OEPH_OM_3) =  value_double(EPH_OM_3)
      if (parmStatus(EPH_OMDOT_3).ne. 0)
     &   OVALS(OEPH_OMDOT_3) = value_double(EPH_OMDOT_3)
      if (parmStatus(EPH_E_3).ne. 0)
     &   OVALS(OEPH_E_3) = value_double(EPH_E_3)
      if (parmStatus(EPH_SINI_3).ne. 0)
     &   OVALS(OEPH_INCBIN_3) = ASIN(value_double(EPH_SINI_3)/raddeg)
      if (parmStatus(EPH_M2_3).ne. 0)
     &   OVALS(OEPH_MCBIN_3) = value_double(EPH_M2_3)
      if (parmStatus(EPH_PBDOT_3) .ne. 0)
     &    OVALS(OEPH_PBDOT_3) =  value_double(EPH_PBDOT_3)*1.0D3
  
c EXPT is stored on the .eph in days. PSRTIME's default is
c   seconds:
      if (parmStatus(EPH_EXPA) .ne.0) then
        OVALS(OEPH_EXPA) = value_double(EPH_EXPA)
        ERRS(OEPH_EXPA) = error_double(EPH_EXPA)
      endif

      if (parmStatus(EPH_EXPT) .ne.0) then
        OVALS(OEPH_EXPT) = value_double(EPH_EXPT)
        ERRS(OEPH_EXPT) = error_double(EPH_EXPT)
      endif

      if (parmStatus(EPH_EXPA_2) .ne.0) then
        OVALS(OEPH_EXPA_2)=value_double(EPH_EXPA_2)
        ERRS(OEPH_EXPA_2) = error_double(EPH_EXPA_2)
      endif

      if (parmStatus(EPH_EXPT_2) .ne. 0)then
        OVALS(OEPH_EXPT_2) = value_double(EPH_EXPT_2)
        ERRS(OEPH_EXPT_2) = error_double(EPH_EXPT_2) 
      endif

      if (parmStatus(EPH_EXPA_3).ne. 0)
     &  OVALS(OEPH_EXPA_3) = value_double(EPH_EXPA_3)
      if (parmStatus(EPH_EXPT_3) .ne. 0)
     &  OVALS(OEPH_EXPT_3) = value_double(EPH_EXPT_3)

c!!!! ddot = dm-dot ??? =DM1
      if (parmStatus(EPH_DM1) .ne.0) then
        OVALS(OEPH_DDOT) = value_double(EPH_DM1)
        ERRS(OEPH_DDOT) = error_double(EPH_DM1) 
      endif

c!!!! OVALS(OEPH_FRQTOL) - no equivalent???? 

c if TASC = 0d0 then set it to PEPOCH
      IF(OVALS(OEPH_TASC) .eq. 0d0) 
     &        OVALS(OEPH_TASC) = OVALS(OEPH_PEPOCH)

c if POSEPOCH = 0d0 then set it to PEPOCH
      IF(OVALS(OEPH_PMEPOCH) .eq. 0d0) 
     &        OVALS(OEPH_PMEPOCH) = OVALS(OEPH_PEPOCH)

      RETURN

      END





