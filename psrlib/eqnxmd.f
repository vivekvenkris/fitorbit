*DECK EQNXMD
C
C
C
      DOUBLE PRECISION FUNCTION EQNXMD(IDATE)
C
C CALCULATE THE EQUATION OF THE EQUINOXES AT 0 UT ON SPECIFIED DATE.
C
C REFERENCE: EXPLANATORY SUPPLEMENT TO THE EPHEMERIS (HMSO,1961) 
C             (HENCEFORTH ESE)
C THE EQN OF THE EQUINOXES IS EQUAL TO: DELTA KSI * COS(EPSILON)
C
C IT RELATES MEAN SIDEREAL TIME AND APPARENT SIDEREAL TIME BY:
C      A.S.T. = M.S.T. + EQN OF EQUINOXES
C DELTA KSI IS THE NUTATION IN LONGITUDE OF THE EARTH.
C IT IS GIVEN BY A SERIES EXPANSION (P.44 OF ESE)
C EPSILON IS THE OBLIQUITY OF THE ECLIPTIC (P.38 OF ESE)
C     VERSION 1.0   5APR84
C
C NOW VARIABLE DEFINITIONS
C
      DOUBLE PRECISION T, TT
C
C NO. OF CENTURIES FROM 1900 TO DATE.
C T AND TT REFER TO JULIAN AND TROPICAL CENTURIES RESPECTIVELY
C
      DOUBLE PRECISION L,LDASH,F,D,OMEGA
C
C  FUNDAMENTAL ARGUMENTS OF NUMERICAL SERIES FOR NUTATION
C
      DOUBLE PRECISION DKSI, EPSLON
C 
C  DELTA KSI, EPSILON
C
      DOUBLE PRECISION TWOPI
      TWOPI = DBLE(8.)*DATAN(DBLE(1.))
C
C FIRST CALCULATE NO. OF JULIAN CENTURIES FROM 1900 TO DATE
C
      T=(DBLE(IDYDIF(500101,IDATE))+DBLE(18262.5))/DBLE(36525.0)
C
C CALCULATE FUNDAMENTAL ARGUMENTS IN DEGREES
C
C            TURNS      DEGREES   MINUTES      SECONDS
C
      L= (               296.0 +  6.0/60.0 + 16.59/3600.0)
     &  +(1325.0*360.0 + 198.0 + 50.0/60.0 + 56.79/3600.0)*T 
     &  +(                                   33.09/3600.0)*T*T
     &  +(                                    0.05/3600.0)*T*T*T
      LDASH=
     &   (               358.0 + 28.0/60.0 + 33.00/3600.0)
     &  +(  99.0*360.0 + 359.0 + 02.0/60.0 + 59.10/3600.0)*T
     &  -(                                    0.54/3600.0)*T*T
     &  -(                                    0.01/3600.0)*T*T*T
      F=
     &   (                11.0 + 15.0/60.0 + 03.20/3600.0)
     &  +(1342.0*360.0 +  82.0 + 01.0/60.0 + 30.54/3600.0)*T
     &  -(                                   11.56/3600.0)*T*T
      D=
     &   (               350.0 + 44.0/60.0 + 14.95/3600.0)
     &  +(1236.0*360.0 + 307.0 + 06.0/60.0 + 51.18/3600.0)*T
     &  -(                                    5.17/3600.0)*T*T
     &  +(                                    0.01/3600.0)*T*T*T
      OMEGA=
     &   (               259.0 + 10.0/60.0 + 59.79/3600.0)
     &  -(   5.0*360.0 + 134.0 +  8.0/60.0 + 31.23/3600.0)*T
     &  +(                                    7.48/3600.0)*T*T
     &  +(                                    0.01/3600.0)*T*T*T
C
C CONVERT TO RADIANS MODULO 2 PI
C
      L=TWOPI*L/360.0
      L=MOD(L,TWOPI)
      LDASH=TWOPI*LDASH/360.0
      LDASH=MOD(LDASH,TWOPI)
      F=TWOPI*F/360.0
      F=MOD(F,TWOPI)
      D=TWOPI*D/360.0
      D=MOD(D,TWOPI)
      OMEGA=TWOPI*OMEGA/360.0
      OMEGA=MOD(OMEGA,TWOPI)
C
C NOW USE THE SERIES TO CALCULATE DELTA KSI
C INITIALLY IN UNITS OF 0.0001 ARCSEC
C USE TERMS IN SERIES GIVEN IN ESE P.44 WITH MAGNITUDE
C GREATER THAN .001 ARCSEC
C
      DKSI= (-172327.0-173.7*T)*DSIN(    OMEGA)
     &     +(   2088.0 +  0.2*T)*DSIN(2.0*OMEGA)      
     &     +(     45.0         )*DSIN(-2.0*L+2.0*F+OMEGA)
     &     +(     10.0         )*DSIN(2.0*L-2.0*F)
     &     +( -12729.0 -  1.3*T)*DSIN(2.0*F-2.0*D+2.0*OMEGA)
     &     +(   1261.0 -  3.1*T)*DSIN(LDASH)
     &     +(   -497.0 +  1.2*T)*DSIN(LDASH+2.0*(F-D+OMEGA))
     &     +(    214.0 -  0.5*T)*DSIN(-LDASH+2.0*F-2.0*D+2.0*OMEGA)
     &     +(    124.0 +  0.1*T)*DSIN(2.*F-2.0*D+OMEGA)
     &     +(     45.0         )*DSIN(2.*L-2.0*D)
     &     +(    -21.0         )*DSIN(2.*F-2.0*D)
     &     +(     16.0 -  0.1*T)*DSIN(2.*LDASH)
     &     +(    -15.0         )*DSIN(LDASH+OMEGA)
     &     +(    -15.0 +  0.1*T)*DSIN(2.*(LDASH+F-D+OMEGA))
     &     +(    -10.0         )*DSIN(-LDASH+OMEGA)
      DKSI=DKSI+    
     &      (  -2037.0 -  0.2*T)*DSIN(2.0*F+2.0*OMEGA)
     &     +(    675.0 +  0.1*T)*DSIN(L)
     &     +(   -342.0 -  0.4*T)*DSIN(2.0*F+OMEGA)
     &     +(   -261.0         )*DSIN(L+2.0*F+2.0*OMEGA)
     &     +(   -149.0         )*DSIN(L-2.0*D)
     &     +(    114.0         )*DSIN(-L+2.0*F+2.0*OMEGA)
     &     +(     60.0         )*DSIN(2.0*D)
     &     +(     58.0         )*DSIN(L+OMEGA)
     &     +(    -57.0         )*DSIN(-L+OMEGA)
     &     +(    -52.0         )*DSIN(-L+2.0*(F+D+OMEGA))
     &     +(    -44.0         )*DSIN(L+2.0*F+OMEGA)
     &     +(    -32.0         )*DSIN(2.0*(F+D+OMEGA))
     &     +(     28.0         )*DSIN(2.0*L)
     &     +(    +26.0         )*DSIN(L+2.0*(F-D+OMEGA))
     &     +(    -26.0         )*DSIN(2.0*(L+F+OMEGA))
     &     +(     25.0         )*DSIN(2.0*F)
     &     +(     19.0         )*DSIN(-L+2.0*F+OMEGA)
     &     +(     14.0         )*DSIN(-L+2.0*D+OMEGA)
     &     +(    -13.0         )*DSIN(L-2.0*D+OMEGA)
C
C NOW CALCULATE EPSILON, THE OBLIQUITY OF THE ECLIPTIC
C SEE ESE P.38
C
C FIRST NEED TIME SINCE 1900 IN TROPICAL CENTURIES
C
      TT=T*36525.0/36524.21988
C
C GET EPSILON IN DEGREES
C
      EPSLON=(23.0 + 27./60.0 +  8.26 /3600.) 
     &       +(               - 46.845/3600.)*TT
     &       +(               -  0.006/3600.)*TT*TT
     &       +(               +  0.002/3600.)*TT*TT*TT
C
C CONVERT TO RADIANS MODULO 2 PI
C
      EPSLON=TWOPI*EPSLON/360.0
      EPSLON=MOD(EPSLON,TWOPI)
C
C ANSWER= DELTA KSI * COS(EPSILON)
C
      EQNXMD=DKSI*COS(EPSLON)*0.0001/15.0
      RETURN
C
C END OF DOUBLE PRECISION FUNCTION FUNCTION EQNXMD.
C
      END
