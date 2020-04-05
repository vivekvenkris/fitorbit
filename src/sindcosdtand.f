C*************************************************************************
      REAL*8 FUNCTION DSIND ( DDEG )
C **************************************************************
c Returns the sin of an angle supplied in degrees     
c SIND replacement caj jun2012

      IMPLICIT NONE
      REAL*8 D2R, DDEG
      PARAMETER (D2R = 3.1415926537/180.0)
     
      DSIND = DSIN(D2R * DDEG)
      RETURN
      END

C*************************************************************************
c      REAL*8 FUNCTION SIND ( DEG )
C **************************************************************
c Returns the sin of an angle supplied in degrees     
c SIND replacement caj jun2012

c      IMPLICIT NONE
c      REAL*4 D2R, DEG
c      PARAMETER (D2R = 2.0*3.1415926537/360.0)
c    
c      SIND = SIN(D2R * DEG)
c      RETURN
c      END

C*************************************************************************
      REAL*8 FUNCTION DCOSD ( DDEG )
C **************************************************************
c Returns the cos of an angle supplied in degrees     
c SIND replacement caj jun2012
      IMPLICIT NONE
      REAL*8 D2R, DDEG
      PARAMETER (D2R = 3.1415926537/180.0)
     
      DCOSD = DCOS(D2R * DDEG)
      RETURN
      END


C*************************************************************************
c      REAL*8 FUNCTION COSD ( DEG )
C **************************************************************
c Returns the cos of an angle supplied in degrees     
c SIND replacement caj jun2012
c      IMPLICIT NONE
c      REAL*4 D2R, DEG
c      PARAMETER (D2R = 3.1415926537/180.0)
c     
c      COSD = COS(D2R * DEG)
c      RETURN
c      END


C*************************************************************************
      REAL*8 FUNCTION DTAND ( DDEG )
C **************************************************************
c Returns the sin of an angle supplied in degrees     
c SIND replacement caj jun2012
      IMPLICIT NONE
      REAL*8 D2R, DDEG
      PARAMETER (D2R = 3.1415926537D0/180.0)
     
      DTAND = DTAN(D2R * DDEG)
      RETURN
      END

C*************************************************************************
      REAL*8 FUNCTION DATAND ( DATN )
C **************************************************************
c Returns the sin of an angle supplied in degrees     
c SIND replacement caj jun2012
      IMPLICIT NONE
      REAL*8 D2R, DATN
      PARAMETER (D2R = 3.1415926537d0/180d0)
     
      DATAND = ATAN(DATN)/D2R
      RETURN
      END

 
