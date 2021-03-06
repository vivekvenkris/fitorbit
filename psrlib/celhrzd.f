*DECK CELHRZD
C
C
C *****************************************************************
        SUBROUTINE CELHRZD(RA,DEC,LST,AZ,EL)
C *****************************************************************
C
C      THIS ROUTINE CONVERTS EQUATORIAL COORDINATES TO HORIZON
C  WITH DOUBLE PRECISION ACCURACY.

C  N.B. THIS ROUTINE DIFFERS FROM CELHRZ IN THAT ALL INPUT PARAMETERS
C       ARE DOUBLE PRECISION AND IN RADIANS

C  PARAMETERS :-
C   INPUT:
C          RA - RIGHT ASCENSION 
C          DECDEG - DECLINATION 
C          LSTSEC - LOCAL SIDEREAL TIME 
C   OUTPUT:
C          AZ - AZIMUTH 
C          EL - ELEVATION
C
C
*******************************************************************
C
         DOUBLE PRECISION LST,AZ,EL,COSCOS,COSSIN,LAT,HA,RA,DEC
         DOUBLE PRECISION PI,TWOPI,SINAZ,COSAZ
C
         PARAMETER( PI=3.141592654D0 , TWOPI=2.0D0*PI )
         PARAMETER( LAT=53.23666667D0*TWOPI/360.0D0 )
C
C
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
         RETURN
C
C  END OF SUBROUTINE CELHRZ
C
         END
