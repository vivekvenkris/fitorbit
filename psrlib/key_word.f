c **********************************************************************
      SUBROUTINE KEY_WORD(NO_KEYWORDS,ST_KEYWORD)
c **********************************************************************
c
c A subroutine to place the comments and values into the ST_KEYWORD
c   array. This was subroutinised because it is common to both READEPH
c   and WRITEPH, and doing so eill make it easier to alter in the future
c
c Note that you will still have to change the value of NO_KEYWORDS in
c   READEPH and WRITEPH (and oeph_key.com!)
c
c Written by Frederick H Jowett and Duncan Law-Green, November 14th 1992
c Mod: Include file written for maintenance   CAJ     Dec. 1998
c
      INCLUDE 'OEPH_KEY.COM'

c Declare variables
c The number of keywords
      INTEGER NO_KEYWORDS
c The ST_KEYWORDS character array
      CHARACTER*100 ST_KEYWORD(NO_KEYWORDS,2)
c
c Put the keywords and comments into the ST_KEYWORD array.
      ST_KEYWORD(1,1)='RAB'
      ST_KEYWORD(1,2)='   ! Right Ascension (B1950)'
      ST_KEYWORD(2,1)='DECB'
      ST_KEYWORD(2,2)='   ! Declination (B1950)'
      ST_KEYWORD(3,1)='RAJ'
      ST_KEYWORD(3,2)='   ! Right Ascension (J2000)'
      ST_KEYWORD(4,1)='DECJ'
      ST_KEYWORD(4,2)='   ! Declination (J2000)'
      ST_KEYWORD(5,1)='PMRA'
      ST_KEYWORD(5,2)='   ! Proper Motion in RA in marcsec per year'
      ST_KEYWORD(6,1)='PMDEC'
      ST_KEYWORD(6,2)='   ! Proper Motion in Dec in marcsec per year'
      ST_KEYWORD(7,1)='PMEPOCH'
      ST_KEYWORD(7,2)='   ! Epoch of PM measurement'
      ST_KEYWORD(8,1)='P'
      ST_KEYWORD(8,2)='   ! Barycentric Period of Pulsar in seconds'
      ST_KEYWORD(9,1)='Pdot'
      ST_KEYWORD(9,2)=
     &    '   ! Period Derivative in 10**-15 seconds per second'
      ST_KEYWORD(10,1)='Pddot'
      ST_KEYWORD(10,2)=
     &    '   ! Period Double-derivative in 10**-24 seconds per sec**2'
      ST_KEYWORD(11,1)='VTRDOT'
      ST_KEYWORD(11,2)=
     &    '   ! Frequency Third Derivative in 10**-30 s**-4'
      ST_KEYWORD(12,1)='PEPOCH'
      ST_KEYWORD(12,2)='   ! Epoch of Period Determination'
      ST_KEYWORD(13,1)='DM'
      ST_KEYWORD(13,2)='   ! Dispersion Measure in parsecs cm**-3'
      ST_KEYWORD(14,1)='RM'
      ST_KEYWORD(14,2)='   ! Rotation Measure'
      ST_KEYWORD(15,1)='We'
      ST_KEYWORD(15,2)='   ! Width of Pulse in milliseconds'
      ST_KEYWORD(16,1)='W50'
      ST_KEYWORD(16,2)='   ! Width of Pulse at 50% in milliseconds'
      ST_KEYWORD(17,1)='W10'
      ST_KEYWORD(17,2)='   ! Width of Pulse at 10% in milliseconds'
      ST_KEYWORD(18,1)='S400'
      ST_KEYWORD(18,2)='   ! Flux at 400MHz in mJy'
      ST_KEYWORD(19,1)='S600'
      ST_KEYWORD(19,2)='   ! Flux at 600MHz in mJy'
      ST_KEYWORD(20,1)='S925'
      ST_KEYWORD(20,2)='   ! Flux at 925MHz in mJy'
      ST_KEYWORD(21,1)='S1400'
      ST_KEYWORD(21,2)='   ! Flux at 1400MHz in mJy'
      ST_KEYWORD(22,1)='S1600'
      ST_KEYWORD(22,2)='   ! Flux at 1600MHz in mJy'
      ST_KEYWORD(23,1)='Dmin'
      ST_KEYWORD(23,2)='   ! Lower Limit for HI Distance in kpc'
      ST_KEYWORD(24,1)='DIST'
      ST_KEYWORD(24,2)='   ! Best Estimate for HI Distance in kpc'
      ST_KEYWORD(25,1)='Dmax'
      ST_KEYWORD(25,2)='   ! Upper Limit for HI Distance in kpc'
      ST_KEYWORD(26,1)='TASC'
      ST_KEYWORD(26,2)='   ! Time of ascending node'
      ST_KEYWORD(27,1)='START'
      ST_KEYWORD(27,2)='   ! Epoch of Start of Fit'
      ST_KEYWORD(28,1)='FINISH'
      ST_KEYWORD(28,2)='   ! Epoch of End of Fit'
      ST_KEYWORD(29,1)='SCATTER'
      ST_KEYWORD(29,2)='   ! Scatter at 408MHz in microseconds'
      ST_KEYWORD(30,1)='T0'
      ST_KEYWORD(30,2)='   ! Epoch of Periastron'
      ST_KEYWORD(31,1)='PB'
      ST_KEYWORD(31,2)='   ! Period of Binary in days'
      ST_KEYWORD(32,1)='A1'
      ST_KEYWORD(32,2)=
     &    '   ! Projected Semi-Major Axis in light-seconds'
      ST_KEYWORD(33,1)='OM'
      ST_KEYWORD(33,2)='   ! Longitude of Periastron in degrees'
      ST_KEYWORD(34,1)='OMdot'
      ST_KEYWORD(34,2)='   ! Periastron Advance in degrees per year'
      ST_KEYWORD(35,1)='E'
      ST_KEYWORD(35,2)='   ! Eccentricity'
      ST_KEYWORD(36,1)='INCBIN'
      ST_KEYWORD(36,2)='   ! Inclination of Binary Orbit'
      ST_KEYWORD(37,1)='MCBIN'
      ST_KEYWORD(37,2)='   ! Mass of Binary Companion'
      ST_KEYWORD(38,1)='PBDOT'
      ST_KEYWORD(38,2)='   ! Orbital Period Derivative'
      ST_KEYWORD(39,1)='T0-2'
      ST_KEYWORD(39,2)='   ! Epoch of Periastron [2]'
      ST_KEYWORD(40,1)='PB-2'
      ST_KEYWORD(40,2)='   ! Period of Binary in days [2]'
      ST_KEYWORD(41,1)='A1-2'
      ST_KEYWORD(41,2)=
     &    '   ! Projected Semi-Major Axis in light-seconds [2]'
      ST_KEYWORD(42,1)='OM-2'
      ST_KEYWORD(42,2)='   ! Longitude of Periastron in degrees [2]'
      ST_KEYWORD(43,1)='OMdot-2'
      ST_KEYWORD(43,2)='   ! Periastron Advance in degrees per year[2]'
      ST_KEYWORD(44,1)='E-2'
      ST_KEYWORD(44,2)='   ! Eccentricity [2]'
      ST_KEYWORD(45,1)='INCBIN-2'
      ST_KEYWORD(45,2)='   ! Inclination of Binary Orbit [2]'
      ST_KEYWORD(46,1)='MCBIN-2'
      ST_KEYWORD(46,2)='   ! Mass of Binary Companion [2]'
      ST_KEYWORD(47,1)='PBDOT-2'
      ST_KEYWORD(47,2)='   ! Orbital Period Derivative [2]'
      ST_KEYWORD(48,1)='T0-3'
      ST_KEYWORD(48,2)='   ! Epoch of Periastron [3]'
      ST_KEYWORD(49,1)='PB-3'
      ST_KEYWORD(49,2)='   ! Period of Binary in days [3]'
      ST_KEYWORD(50,1)='A1-3'
      ST_KEYWORD(50,2)=
     &    '   ! Projected Semi-Major Axis in light-seconds [3]'
      ST_KEYWORD(51,1)='OM-3'
      ST_KEYWORD(51,2)='   ! Longitude of Periastron in degrees [3]'
      ST_KEYWORD(52,1)='OMdot-3'
      ST_KEYWORD(52,2)='   ! Periastron Advance in degrees per year[3]'
      ST_KEYWORD(53,1)='E-3'
      ST_KEYWORD(53,2)='   ! Eccentricity [3]'
      ST_KEYWORD(54,1)='INCBIN-3'
      ST_KEYWORD(54,2)='   ! Inclination of Binary Orbit [3]'
      ST_KEYWORD(55,1)='MCBIN-3'
      ST_KEYWORD(55,2)='   ! Mass of Binary Companion [3]'
      ST_KEYWORD(56,1)='PBDOT-3'
      ST_KEYWORD(56,2)='   ! Orbital Period Derivative [3]'
      ST_KEYWORD(57,1)='EXPA'
      ST_KEYWORD(57,2)='   ! Phase Exponential Amplitude'
      ST_KEYWORD(58,1)='EXPT'
      ST_KEYWORD(58,2)='   ! Phase Exponential Timescale'
      ST_KEYWORD(59,1)='EXPA-2'
      ST_KEYWORD(59,2)='   ! Phase Exponential Amplitude [2]'
      ST_KEYWORD(60,1)='EXPT-2'
      ST_KEYWORD(60,2)='   ! Phase Exponential Timescale [2]'
      ST_KEYWORD(61,1)='EXPA-3'
      ST_KEYWORD(61,2)='   ! Phase Exponential Amplitude [3]'
      ST_KEYWORD(62,1)='EXPT-3'
      ST_KEYWORD(62,2)='   ! Phase Exponential Timescale [3]'
      ST_KEYWORD(63,1)='FRQTOL'
      ST_KEYWORD(63,2)='   ! Frequency Tolerance'
      ST_KEYWORD(64,1)='DDOT'
      ST_KEYWORD(64,2)='   ! 1st Derivative of DM'

c This following bit will be sorted out later.
c      DATA ST_KEYWORD(12)/'SURVEY'/   ! Surveys that saw Pulsar
c There is a reason for the above clumsiness: FORTRAN is RUBBISH!!
c
c
c Go back to READEPH or WRITEPH
c
      RETURN
      END







