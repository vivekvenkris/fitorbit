*DECK RCC
*
      REAL FUNCTION RCC (T, UT1, EL, U, V)
*
*-
*
*     - - - - - -
*        R C C
*     - - - - - -
*
*
*     RELATIVISTIC CLOCK CORRECTION
*
*
*     Required for the transformation from proper time on earth
*     to coordinate time in the solar system barycentric
*     space-time frame of reference.
*
*
*     Given:
*
*        T       coordinate time: effectively the ephemeris time
*        UT1     universal time: fraction of a day
*        EL      clock east longitude
*        U       clock distance from earth spin axis
*        V       clock distance north of earth equatorial plane
*
*
*     Returned:
*
*        RCC     the clock correction:  T - TAI - DTA, where T is the
*                coordinate time in the solar system barycentre frame
*                of reference, TAI is the International Atomic Time
*                from the clock on earth, and DTA is the constant term
*                in the expression for T - TAI (whose currently
*                accepted value is 32.184 seconds).
*
*                RCC has a main (annual) term of amplitude
*                approximately 1.7 milliseconds.
*
*
*     Units:
*
*        RCC     seconds
*
*        T       days: Modified Julian Date (JD - 2400000.5)
*        UT1     days
*        EL      radians
*        U,V     kilometres
*
*
*     Types:
*
*        T is DOUBLE PRECISION; everything else is REAL.
*
*
*     The algorithm is from Moyer, Cel. Mech., 23, 33, 1981, but
*     with the changing value of the earth orbit eccentricity
*     taken into account together with a more accurate value for
*     the coefficient of the main annual term, and with two extra
*     terms included to allow for the eccentricities of the orbits
*     of Jupiter and Saturn.  Note that Moyer's accuracy
*     objectives were for spacecraft tracking applications,
*     and the amplitudes of some of the short period terms are
*     below the accuracies of the slower terms.  The absolute
*     accuracy is better than 1 microsecond.
*
*
*  P.T.Wallace  Starlink  October 1981
*
*-

      DOUBLE PRECISION T
      REAL UT1,EL,U,V

      DOUBLE PRECISION TC



*  Convert T to seconds past 1950 Jan 1.0
      TC = (T-33282D0) * 86400D0

*  Convert UT1 to local solar time in radians
      TSOL = MOD(UT1,1.0)*6.28318531 + EL

*  Arguments

      E = 0.0167301 - 1.329E-14*REAL(TC)

      AL    = 4.888 339 + REAL( 1.991 063 83  D-7 * TC)
      AM    = 6.248 291 + REAL( 1.990 968 71  D-7 * TC)
      D     = 2.518 411 + REAL( 2.462 600 818 D-6 * TC)
      ALMLJ = 5.652 593 + REAL( 1.823 136 37  D-7 * TC)
      ALMLS = 2.125 474 + REAL( 1.923 399 23  D-7 * TC)
      EJ    = 5.286 877 + REAL( 1.678 506 3   D-8 * TC)
      ES    = 1.165 341 + REAL( 0.675 855 8   D-8 * TC)

      AJE = ALMLJ - EJ
      ASE = ALMLS - ES
      AL2 = 2.0*AL
      AM2 = 2.0*AM
      AE = AM + E*SIN(AM)

*  Correction from proper ET to coordinate ET
      W =     2.9E-14*U*SIN(TSOL+ALMLS)
      W = W + 1.33E-13*U*SIN(TSOL+ALMLJ)
      W = W + 0.26E-6*SIN(ASE)
      W = W + 1.00E-6*SIN(AJE)
      W = W + 4.58E-6*SIN(ALMLS)
      W = W + 20.73E-6*SIN(ALMLJ)
      W = W + 2.45E-6*SIN(ES)
      W = W + 5.21E-6*SIN(EJ)
      W = W - 1.3184E-10*V*COS(AL)
      W = W + 1.33E-13*U*SIN(TSOL-D)
      W = W - 2.29E-13*U*SIN(TSOL+AL2+AM)
      W = W - 1.3677E-11*U*SIN(TSOL+AL2)
      W = W + 1.00E-13*U*SIN(TSOL-AM2)
      W = W + 5.312E-12*U*SIN(TSOL-AM)
      W = W + 3.17679E-10*U*SIN(TSOL)
      W = W + 1.548E-6*SIN(D)
      W = W + 0.099153*E*SIN(AE)

      RCC = W

      END
