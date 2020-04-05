      SUBROUTINE OLDEPH2NEW(parmStatus, value_str, value_double,
     &             value_integer, error_double, OVALS, ERRS)

      IMPLICIT NONE
C
C ROUTINE TO convert the new array of eph values and errors to a new array
C         - mostly changing frequencies to periods, and rearranging
c         - zeros all vals and errors not found
c dec 1998 caj  - pruned for the mo. for fitorbit!
c july 2001 caj - pdoterr - use abs to force +ive.
c june 2002 caj - correct units for f1err & f2err
c may 2005 caj  - add fitflags where errors are non-zero
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
      REAL*8 p0relerr, p1relerr, f0relerr, f1relerr, p2relerr
      REAL*8  P,V,PD,VD,PDD,VDD
      INTEGER PARNUM

c constants
      include 'PSRMATH.DEF;2'

cc - new eph files - we have to load all the keyinfo arrays (aaargh!)
      DO PARNUM = 1, NUM_KEYS
          parmStatus(parnum) = 0
          error_double(parnum)=0.0d0
      ENDDO

c 1 = PSRJ name, 2 = PSRB name - do in readeph/writeph
c      value_str(EPH_PSRB) = 
c      parmStatus(EPH_PSRB) = 

c 3,4 RAJ/DEC in J2000- needs to be in turns
      value_double(EPH_RAJ) = OVALS(OEPH_RAJ)/360.
      parmStatus(EPH_RAJ) = 1
      value_double(EPH_DECJ) = OVALS(OEPH_DECJ)/360.
      parmStatus(EPH_DECJ) = 1

c RA error in degrees
      if (ERRS(OEPH_RAJ) .NE. 0d0) THEN
         error_double(EPH_RAJ) = ERRS(OEPH_RAJ)
         parmStatus(EPH_RAJ) = 2
      endif

c DEC errors in degrees
      if (ERRS(OEPH_DECJ) .NE. 0d0) THEN
         error_double(EPH_DECJ) = ERRS(OEPH_DECJ)
         parmStatus(EPH_DECJ) = 2
      endif

c PEPOCH as integer epoch + double fractional part ! no err in new eph?
      value_integer(EPH_PEPOCH) = INT(OVALS(OEPH_PEPOCH))
      value_double(EPH_PEPOCH) = OVALS(OEPH_PEPOCH) -
     &                                 DBLE(INT(OVALS(OEPH_PEPOCH))) 
      error_double(EPH_PEPOCH) = ERRS(OEPH_PEPOCH)
      parmStatus(EPH_PEPOCH) = 1

c F, F1, F2 ....... 18=FC (HEX) (doubles)
c store value as frequency in sec-1
      P = OVALS(OEPH_P)
      V = 1.0d0/P
      value_double(EPH_F) = V
      parmStatus(EPH_F) =1

c err in P
      If (P .ne. 0d0 .and. ERRS(OEPH_P) .ne. 0d0) then
        p0relerr = ERRS(OEPH_P)/P
        error_double(EPH_F)= V * p0relerr
        parmStatus(EPH_F) =2
      endif

c pdot - store value as Fdot in sec-2
      PD = OVALS(OEPH_PDOT)*1.0d-15
      IF (P .NE. 0d0 .AND. PD .NE. 0d0) then
        VD = - PD/(P*P)
        value_double(EPH_F1) =VD
        parmStatus(EPH_F1) =1

        if (ERRS(OEPH_PDOT) .gt. 0.0 .and. PD .ne. 0.0 .and. 
     &         p0relerr .gt.0) then 
          p1relerr= ERRS(OEPH_PDOT)*1.0d-15/PD
          f1relerr = sqrt(p1relerr*p1relerr + 4d0*p0relerr*p0relerr)  
          error_double(EPH_F1)= abs(VD) * f1relerr
          parmStatus(EPH_F1) = 2
        endif

c pddot - store fddot in sec-3
        PDD = OVALS(OEPH_PDDOT)*1.0d-24
        IF (PDD .NE. 0d0) THEN
          VDD = 2*VD*VD/V - PDD*V*V
          value_double(EPH_F2) = VDD
          parmStatus(EPH_F2) =1
          if (PDD .ne. 0d0 .and. V .gt. 0.0 .and. 
     &                       ERRS(OEPH_PDDOT).ne. 0d0) then
            p2relerr = ERRS(OEPH_PDDOT)*1.0d-24/PDD 
            error_double(EPH_F2)=sqrt(
     &             (VD * f1relerr * 4d0 * VD/V)**2 +
     &             (V * f0relerr * 2d0 * (VD*VD/(V*V) + PD*V))**2 +
     &             (PDD*p2relerr*V*V)**2)
            parmStatus(EPH_F2) =1
          endif
        endif
      endif

c VTRDOT !!!!!!!!!
      value_double(EPH_F3) = OVALS(OEPH_VTRDOT)
      parmStatus(EPH_F3) =1

c 19 = PMRA, 20 = PMDEC
      value_double(EPH_PMRA) = OVALS(OEPH_PMRA)
      value_double(EPH_PMDEC) = OVALS(OEPH_PMDEC)
      parmStatus(EPH_PMRA) = 1
      parmStatus(EPH_PMDEC) = 1
      if (errs(OEPH_PMRA) .ne. 0d0) then
         error_double(EPH_PMRA)= ERRS(OEPH_PMRA)
         parmStatus(EPH_PMRA) = 2
      endif
      if (errs(OEPH_PMDEC) .ne. 0d0) then
         error_double(EPH_PMDEC) = ERRS(OEPH_PMDEC)
         parmStatus(EPH_PMDEC) = 2
      endif

c 22 Posepoch as integer and fraction aka PMEPO!
      value_integer(EPH_POSEPOCH) = INT(OVALS(OEPH_PMEPOCH))
      value_double(EPH_POSEPOCH) = OVALS(OEPH_PMEPOCH) -
     &                                DBLE(INT(OVALS(OEPH_PMEPOCH)))
      parmStatus(EPH_POSEPOCH) = 1

c 24 = DM 
      value_double(EPH_DM) = OVALS(OEPH_DM)
      parmStatus(EPH_DM) = 1
      if (errs(OEPH_DM) .ne. 0d0) then
         error_double(EPH_DM) = ERRS(OEPH_DM)
         parmStatus(EPH_DM) = 2
      endif

c 26    TASC
      value_integer(EPH_TASC) = INT(OVALS(OEPH_TASC))
      value_double(EPH_TASC) = OVALS(OEPH_TASC) - 
     &                                    DBLE(INT(OVALS(OEPH_TASC)))
      parmStatus(EPH_TASC) = 1
      if (errs(OEPH_TASC) .ne. 0d0) then
         error_double(EPH_TASC) = ERRS(OEPH_TASC)
         parmStatus(EPH_TASC) = 2
      endif

c 35 = EPH_START 36 = EPH_FINISH (are these the same as FST, FFIN 
c Epoch of start of fit and end of fit)!!!!
      if (OVALS(OEPH_START).gt.0.0) then
        value_integer(EPH_START) = INT(OVALS(OEPH_START))
        value_double(EPH_START) = OVALS(OEPH_START)
     &                     - DBLE(INT(OVALS(OEPH_START))) 
        parmStatus(EPH_START) = 1
      endif

      if (OVALS(OEPH_FINISH).gt.0.0) then
        value_integer(EPH_FINISH) = INT(OVALS(OEPH_FINISH))
        value_double(EPH_FINISH) = OVALS(OEPH_FINISH) - 
     &                           DBLE(INT(OVALS(OEPH_FINISH))) 
        parmStatus(EPH_FINISH) = 1
      endif

c !!!Scatter removed according to ephio.txt - nearest thing might be TAU
c      RDVALS(29)=SCAT/1.0D-6

c BINARY parameter - set to 'BT'
      if ( OVALS(OEPH_A1) .ne. 0d0) THEN
         value_str(EPH_BINARY) = 'BT'
         parmStatus(EPH_BINARY) = 1
      endif

c !!! 49 = T0 (epoch of periastron of first orbit) query = epbin
      value_integer(EPH_T0) = INT(OVALS(OEPH_T0))
      value_double(EPH_T0) = OVALS(OEPH_T0) - 
     &                       DBLE(INT(OVALS(OEPH_T0)))
      parmStatus(EPH_T0) = 1
      if (errs(OEPH_T0) .ne. 0d0) then
         error_double(EPH_T0) = ERRS(OEPH_T0)
         parmStatus(EPH_T0) = 2
       endif

c 52 PB units !!!!! (days needed)
      value_double(EPH_PB) = OVALS(OEPH_PB)
      parmStatus(EPH_PB) = 1
      if (ERRS(OEPH_PB) .ne. 0d0) then
        error_double(EPH_PB) = ERRS(OEPH_PB)
        parmStatus(EPH_PB) =2
      endif

c!!! ASINI - binary major semi-axis aka 43 = A1      
      value_double(EPH_A1) = OVALS(OEPH_A1)
      parmStatus(EPH_A1)=1
      if (ERRS(OEPH_A1) .ne. 0d0) then
        error_double(EPH_A1)=ERRS(OEPH_A1)
        parmStatus(EPH_A1) =2
      endif

c WBIN = longtitude of periastron = omega (deg) = OM = 55
      value_double(EPH_OM) = OVALS(OEPH_OM)
      parmStatus(EPH_OM)=1
      if (ERRS(OEPH_OM) .ne. 0d0) then
        error_double(EPH_OM) = ERRS(OEPH_OM)
        parmStatus(EPH_OM) =2
      endif

c WDOT deriv of WBIN - units??
c      value_double(EPH_OMDOT) = WDOT*31557687.452663
c      error_double(EPH_OMDOT)= ERRWDOT*31557687.452663

      parmStatus(EPH_OMDOT)=1
      value_double(EPH_OMDOT) = OVALS(OEPH_OMDOT)
      if (ERRS(OEPH_OMDOT) .ne. 0d0) then
        error_double(EPH_OMDOT) = ERRS(OEPH_OMDOT)
        parmStatus(EPH_OMDOT) =2
      endif

c ECCBIN. Binary eccentricity aka E
      value_double(EPH_E) = OVALS(OEPH_E)
      parmStatus(EPH_E)=1
      if (ERRS(OEPH_E) .ne. 0d0) then
        error_double(EPH_E)=ERRS(OEPH_E)
        parmStatus(EPH_E) =2
      endif

c!! INCBIN --> SINI =  sin of inclination angle = 66
c      value_double(EPH_SINI) = SIND(OVALS(OEPH_INCBIN))
      value_double(EPH_SINI) = SIN(OVALS(OEPH_INCBIN)*DEGRAD)
      parmStatus(EPH_SINI)=1
      if (ERRS(OEPH_INCBIN) .ne. 0d0) then
c        error_double(EPH_SINI)= SIND(ERRS(OEPH_INCBIN))
        error_double(EPH_SINI)= SIN(ERRS(OEPH_INCBIN)*DEGRAD)
        parmStatus(EPH_SINI) =2
      endif

c MCBIN Companion Mass aka M2 =70
      value_double(EPH_M2) = OVALS(OEPH_MCBIN)
      parmStatus(EPH_M2) = 1
      if (ERRS(OEPH_MCBIN) .ne. 0d0) then
        error_double(EPH_M2)=ERRS(OEPH_MCBIN)
        parmStatus(EPH_M2) =2
      endif

c!! PBINDOT old eph stored units of 10**-15. 
c   aka PBDOT stored units of 10**-12
      value_double(EPH_PBDOT) = OVALS(OEPH_PBDOT)
      parmStatus(EPH_PBDOT)  =1
      if (ERRS(OEPH_PBDOT) .ne. 0d0) then
        error_double(EPH_PBDOT)=ERRS(OEPH_PBDOT)
        parmStatus(EPH_PBDOT) =2
      endif

c Second set of binary data
      value_integer(EPH_T0_2) = INT(OVALS(OEPH_T0_2))
      value_double(EPH_T0_2) = OVALS(OEPH_T0_2)- 
     &                         DBLE(INT(OVALS(OEPH_T0_2))) 
      parmStatus(EPH_T0_2) = 1
      if (ERRS(OEPH_T0_2) .ne. 0d0) then
        error_double(EPH_T0_2) = ERRS(OEPH_T0_2)
        parmStatus(EPH_T0_2) =2
      endif

      value_double(EPH_PB_2) = OVALS(OEPH_PB_2)
      parmStatus(EPH_PB_2) = 1
      if (ERRS(OEPH_PB_2) .ne. 0d0) then
        error_double(EPH_PB_2) = ERRS(OEPH_PB_2)
        parmStatus(EPH_PB_2) =2
      endif

      value_double(EPH_A1_2) = OVALS(OEPH_A1_2)
      parmStatus(EPH_A1_2)=1
      if (ERRS(OEPH_A1_2) .ne. 0d0) then
        error_double(EPH_A1_2)=ERRS(OEPH_A1_2)
        parmStatus(EPH_A1_2) =2
      endif

      value_double(EPH_OM_2) = OVALS(OEPH_OM_2)
      parmStatus(EPH_OM_2)=1
      if (ERRS(OEPH_OM_2) .ne. 0d0) then
        error_double(EPH_OM_2) = ERRS(OEPH_OM_2)
        parmStatus(EPH_OM_2) =2
      endif

      value_double(EPH_OMDOT_2) = OVALS(OEPH_OMDOT_2)
      parmStatus(EPH_OMDOT_2)=1
      if (ERRS(OEPH_OMDOT_2) .ne. 0d0) then
        error_double(EPH_OMDOT_2) = ERRS(OEPH_OMDOT_2)
        parmStatus(EPH_OMDOT_2) =2
      endif

      value_double(EPH_E_2) = OVALS(OEPH_E_2)
      parmStatus(EPH_E_2)=1
      if (ERRS(OEPH_E_2) .ne. 0d0) then
        error_double(EPH_E_2) = ERRS(OEPH_E_2)
        parmStatus(EPH_E_2) =2
      endif

      value_double(EPH_SINI_2) = SIN(OVALS(OEPH_INCBIN_2)*DEGRAD)
      parmStatus(EPH_SINI_2)=1
      if (ERRS(OEPH_INCBIN_2) .ne. 0d0) then
        error_double(EPH_SINI_2) = SIN(ERRS(OEPH_INCBIN_2)*DEGRAD)
        parmStatus(EPH_SINI) =2
      endif

      value_double(EPH_M2_2) = OVALS(OEPH_MCBIN_2)
      parmStatus(EPH_M2_2)=1
      if (ERRS(OEPH_MCBIN_2) .ne. 0d0) then
        error_double(EPH_M2_2)=ERRS(OEPH_MCBIN_2)
        parmStatus(EPH_M2_2) =2
      endif

      value_double(EPH_PBDOT_2) = OVALS(OEPH_PBDOT_2)
      parmStatus(EPH_PBDOT_2)  =1
      if (ERRS(OEPH_PBDOT_2) .ne. 0d0) then
        error_double(EPH_PBDOT_2) = ERRS(OEPH_PBDOT_2)
        parmStatus(EPH_PBDOT_2) =2
      endif

c Third set of binary data
      value_integer(EPH_T0_3) = INT(OVALS(OEPH_T0_3))
      value_double(EPH_T0_3) = OVALS(OEPH_T0_3)- 
     &                              DBLE(INT(OVALS(OEPH_T0_3)))
      parmStatus(EPH_T0_3) = 1
      error_double(EPH_T0_3) = ERRS(OEPH_T0_3)

      value_double(EPH_PB_3) = OVALS(OEPH_PB_3)
      parmStatus(EPH_PB_3) = 1
      error_double(EPH_PB_3) = ERRS(OEPH_PB_3)

      value_double(EPH_A1_3) = OVALS(OEPH_A1_3)
      parmStatus(EPH_A1_3)=1
      error_double(EPH_A1_3) = ERRS(OEPH_A1_3)

      value_double(EPH_OM_3) = OVALS(OEPH_OM_3)
      parmStatus(EPH_OM_3)=1
      error_double(EPH_OM_3) = ERRS(OEPH_OM_3)

      value_double(EPH_OMDOT_3) = OVALS(OEPH_OMDOT_3)
      parmStatus(EPH_OMDOT_3)=1
      error_double(EPH_OM_3) = ERRS(OEPH_OM_3)

      value_double(EPH_E_3) =  OVALS(OEPH_E_3)
      parmStatus(EPH_E_3)=1
      error_double(EPH_E_3) = ERRS(OEPH_E_3)


      value_double(EPH_SINI_3) = SIN(OVALS(OEPH_INCBIN_3)*DEGRAD)
      parmStatus(EPH_SINI_3)=1
      error_double(EPH_SINI_3) = SIN(ERRS(OEPH_INCBIN_3)*DEGRAD)

      value_double(EPH_M2_3) = OVALS(OEPH_MCBIN_3)
      parmStatus(EPH_M2_3)=1
      error_double(EPH_M2_3) = ERRS(OEPH_MCBIN_3)

      value_double(EPH_PBDOT_3) =  OVALS(OEPH_PBDOT_3)
      parmStatus(EPH_PBDOT_3)  =1
      error_double(EPH_PBDOT_3) = ERRS(OEPH_PBDOT_3)

c EXPA is stored on the .eph as milliseconds. PSRTIME wants it
c   in seconds so: 
      value_double(EPH_EXPA) = OVALS(OEPH_EXPA)/1.0D-3
      parmStatus(EPH_EXPA) = 1
      error_double(EPH_EXPA)=ERRS(OEPH_EXPA)/1.0D-3

c EXPT is stored on the .eph in days. PSRTIME's default is
c   seconds:
      value_double(EPH_EXPT)=OVALS(OEPH_EXPT)/86400.0D0
      parmStatus(EPH_EXPT) = 1
      error_double(EPH_EXPT)=ERRS(OEPH_EXPT)/86400.0D0

      value_double(EPH_EXPA_2) = OVALS(OEPH_EXPA_2)/1.0D-3
      parmStatus(EPH_EXPA_2) = 1
      error_double(EPH_EXPA_2)=ERRS(OEPH_EXPA_2)/1.0D-3

      value_double(EPH_EXPT_2)=OVALS(OEPH_EXPT_2)/86400.0D0
      parmStatus(EPH_EXPT_2) = 1
      error_double(EPH_EXPT_2) = ERRS(OEPH_EXPT_2)/86400.0D0

      value_double(EPH_EXPA_3) = OVALS(OEPH_EXPA_3)/1.0D-3
      parmStatus(EPH_EXPA_3) = 1
      error_double(EPH_EXPA_3)=ERRS(OEPH_EXPA_3)/1.0D-3

      value_double(EPH_EXPT_3)=OVALS(OEPH_EXPT_3)/86400.0D0
      parmStatus(EPH_EXPT_3) = 1
      error_double(EPH_EXPT_3)=ERRS(OEPH_EXPT_3)/86400.0D0

c!!!!      RDVALS(63)=FRQTOL  What's this !!!!

c!!!! ddot = dm-dot ??? =DM1
      value_double(EPH_DM1) = OVALS(OEPH_ddot)
      parmStatus(EPH_DM1) = 1
      error_double(EPH_DM1) = OVALS(OEPH_DDOT)

c Check and clear parmStatus for all zero items
      DO PARNUM = 1, NUM_KEYS
        if (parmTypes(parnum) .eq.0) then
          if (value_str(parnum) .eq. ' ') parmStatus(parnum)=0

        elseif (parmTypes(parnum) .eq.1 .or.
     &           parmTypes(parnum) .eq.2 .or.
     &            parmTypes(parnum) .eq.3 ) then

          if (value_double(parnum) .eq. 0d0) parmStatus(parnum)=0
        elseif (parmTypes(parnum) .eq.4) then
          if (value_integer(parnum) .eq. 0 .and. 
     &          value_double(parnum) .eq. 0d0) parmStatus(parnum)=0

        elseif (parmTypes(parnum) .eq.5) then
          if (value_integer(parnum) .eq. 0) parmStatus(parnum)=0
        endif 

      ENDDO

      RETURN

      END





