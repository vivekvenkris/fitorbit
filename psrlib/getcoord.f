*DECK GETCOORD
C
C **************************************************************
      SUBROUTINE GETCOORD ( AZDEG,ELDEG,JD,UTSEC,RADEG,DECDEG )
C **************************************************************
C
C     Converts the telescope azimuth & elevation to a 1950.0
C     RA & Dec using the Modified Julian Date (JD) and UT in
C     seconds (UTSEC).
C
      DOUBLE PRECISION UNITVEC(3),DPI,LSTSEC,AZ,EL
      DOUBLE PRECISION JD,UTSEC
      REAL RADEG,DECDEG,PI
      PARAMETER(DPI=3.1415926540897932D0,PI=3.141592654)
C
      AZ = DBLE(AZDEG)
      EL = DBLE(ELDEG)
C
C     Calculate the local sidereal time (allowing 9 mins
C     13.47 secs for Jodrell's westerly longitude)...
C
      LSTSEC = 43200.0D0/DPI
     &  *SLA_GMST(DBLE(JD+1.0D0) + DBLE(UTSEC)/86400.0D0) - 553.47D0  
     &  + SLA_EQEQX(DBLE(JD+1.0D0) + DBLE(UTSEC)/86400.0D0)
C
C     (LSTSEC is the local sidereal time in seconds)
C
C     Convert co-ords to RA & Dec(epoch).
C     RA & Dec will be returned in decimal degrees...
C
      CALL HRZCEL(AZ,EL,LSTSEC,RADEG,DECDEG)
C
C     Convert to RA & Dec (1950.0)...
C     Firstly, change RADEG & DECDEG to radians :
C
      RADEG=RADEG*PI/180.0
      DECDEG=DECDEG*PI/180.0
C
C     Then convert them to direction cosines...
C
      CALL SLA_DCS2C(RADEG,DECDEG,UNITVEC)
C
C     Precess direction cosines back to epoch 1950.0 (nominally 0.0)...
C
      CALL PRECES(UNITVEC(1),UNITVEC(2),UNITVEC(3),(JD+1.0D0),
     &        0.0,UNITVEC(1),UNITVEC(2),UNITVEC(3))
C
C     Convert 1950.0 direction cosines to 1950.0 celestial coords...
C
      CALL SLA_DCC2S(UNITVEC,RADEG,DECDEG)
C
      RADEG=RADEG*180.0D0/DPI
      DECDEG=DECDEG*180.0D0/DPI
C
      IF(RADEG.LT.0.0)RADEG=RADEG+360.0D0
      RETURN
C
C END OF SUBROUTINE GETCOORD
C
      END
