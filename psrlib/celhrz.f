*DECK CELHRZ
C
C
C *****************************************************************
        SUBROUTINE CELHRZ(RADEG,DECDEG,LSTSEC,AZDEG,ELDEG)
C *****************************************************************
C
C      THIS ROUTINE CONVERTS EQUATORIAL COORDINATES TO HORIZON
C  WITH DOUBLE PRECISION ACCURACY.
C  PARAMETERS :-
C   INPUT:
C          RADEG - RIGHT ASCENSION IN DECIMAL DEGREES.
C          DECDEG - DECLINATION IN DECIMAL DEGREES.
C          LSTSEC - LOCAL SIDEREAL TIME IN DECIMAL SECONDS.
C   OUTPUT:
C          AZDEG - AZIMUTH IN DECIMAL DEGREES.
C          ELDEG - ELEVATION IN DECIMAL DEGREES.
C
C N.B. ALL PARAMETERS ARE SINGLE PRECISION REAL NUMBERS. ALL INPUT
C      PARAMETERS ARE UNCHANGED ON EXIT.
C
*******************************************************************
C
         DOUBLE PRECISION LST,AZ,EL,COSCOS,COSSIN,LAT,HA,RA,DEC
         DOUBLE PRECISION PI,TWOPI,SINAZ,COSAZ
         REAL AZDEG,ELDEG,LSTSEC,RADEG,DECDEG
C
         PARAMETER( PI=3.141592654D0 , TWOPI=2.0D0*PI )
         PARAMETER( LAT=53.23666667D0*TWOPI/360.0D0 )
C
C CHANGE INPUT COORDINATES TO DOUBLE PRECISION RADIANS
C
         RA=DBLE(RADEG)*TWOPI/360.0D0
         DEC=DBLE(DECDEG)*TWOPI/360.0D0
C
C CONVERT LOCAL SIDEREAL TIME TO RADIANS
C
         LST=DBLE(LSTSEC)*TWOPI/86400.0D0
C
C NOW CALCULATE THE HOUR ANGLE (HA)
C
         HA=LST-RA
C
C FIND ELEVATION ...
C
         EL=DASIN(DSIN(LAT)*DSIN(DEC)+
     +          DCOS(LAT)*DCOS(DEC)*DCOS(HA))
C
C CALCULATE AZIMUTH ..
C
         AZ=DASIN(-DCOS(DEC)*DSIN(HA)/DCOS(EL))
C
C FIND THE SIN AND COS OF AZ TO UNIQUELY DEFINE IT.
C
         SINAZ=-DCOS(DEC)*DSIN(HA)/DCOS(EL)
         COSAZ=(DCOS(LAT)*DSIN(DEC)-DSIN(LAT)
     +            *DCOS(DEC)*DCOS(HA))/DCOS(EL)
C
C NOW CHOOSE CORRECT QUADRANT FOR AZIMUTH
C
         IF(SINAZ.LT.0.0D0)THEN
           IF(COSAZ.LT.0.0D0)THEN
              AZ=PI-AZ
           ELSE
              AZ=TWOPI+AZ
           ENDIF
         ELSE
           IF(COSAZ.LT.0.0D0)THEN
              AZ=PI-AZ
           ELSE
              AZ=AZ
           ENDIF
         ENDIF
C
C  CONVERT TO REAL DEGREES.
C
         AZDEG=REAL(AZ*360.0D0/TWOPI)
         ELDEG=REAL(EL*360.0D0/TWOPI)
C
         RETURN
C
C  END OF SUBROUTINE CELHRZ
C
         END
