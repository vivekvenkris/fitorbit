*DECK PARUNT

      BLOCK DATA PARUNTINIT

      include 'PARUNT.DEF'

      DATA (CUNITS(I),I=1,21)
     & / 'percent','integrations','k','transforms','periods'
     &  ,'degrees','radians','mas','arcsec'
     &  ,'nseconds','useconds','mseconds','seconds','minutes'
     &  ,'hours','days','years','myr'
     &  ,'pc','kpc','mpc'
     & /
      DATA (CUNITS(I),I=22,43)
     & / 'pc/cm**3'
     &  ,'rad/m**2'
     &  ,'us/day','ns/day'
     &  ,'e-24/s','e-15/s**2','e-24/s**3'
     &  ,'hz','khz','mhz','ghz'
     &  ,'ujy','mjy','jy'
     &  ,'mas/yr'
     &  ,'ms/p','deg/p'
     &  ,'ugauss','mgauss','gauss'
     &  ,'erg','erg/sec'
     & /
      DATA (CUNITS(I),I=44,NUNITS)
     & / 'e-21','e-18','e-15','e-12','e-9','e-6','e-3'
     &  ,'e+21','e+18','e+15','e+12','e+9','e+6','e+3'
     &  ,'deg/year','DM/year','null',' ' /
C
C     THE LIST OF UNIT SCALING FACTORS.
C
      DATA (SUNITS(I),I=1,21)
     & / 1.0,1.0,1024.0,1.0,1.0
     &  ,4*1.0
     &  ,1.0D-9,1.0D-6,1.0D-3,1.0,60.0
     &  ,3600.0,86400.0,1.0,1.0D6
     &  ,1.0,1.0D3,1.0D6
     & /
      DATA (SUNITS(I),I=22,43)
     & / 1.0
     &  ,1.0
     &  ,1.1574074D-11,1.1574074D-14
     &  ,1.0D-24,1.0D-15,1.0D-24
     &  ,1.0D-6,1.0D-3,1.0,1.0D3
     &  ,1.0D-6,1.0D-3,1.0
     &  ,1.0    
     &  ,1.0,1.0
     &  ,1.0D-6,1.0D-3,1.0
     &  ,1.0,1.0
     & /
      DATA (SUNITS(I),I=44,NUNITS)
     & / 1.0D-21,1.0D-18,1.0D-15,1.0D-12,1.0D-9,1.0D-6,1.0D-3
     &  ,1.0D+21,1.0D+18,1.0D+15,1.0D+12,1.0D+9,1.0D+6,1.0D+3
     &  ,3.1688D-8,1.0,1.0,1.0 /
      END
C
C **************************************************************
      LOGICAL FUNCTION PARUNT ( STRING, IUNIT )
C **************************************************************
C
C THIS ROUTINE TESTS STRING FOR CONTAINING A VALID UNIT SPECIFIER.
C IF FOUND, THE FUNCTION IS TRUE AND THE NUMBER OF THE UNIT
C     IS RETURNED IN IUNIT, OTHERWISE THE FUNCTION IS FALSE AND
C     IUNIT IS UNCHANGED.
C THE SCALING FACTOR FOR CONVERSION BETWEEN USER SUPPLIED VALUES
C     AND INTERNAL VALUES IS RETURNED BY DOUBLE PRECISION FUNCTION
C     SUNIT(IUNIT), AND THE NAME OF A UNIT IS RETURNED BY
C     CHARACTER*(*) FUNCTION CUNIT(IUNIT).
C
      CHARACTER*(*) STRING
C
C     COMMON BLOCK /CUNITS/ CONTAINS THE LIST OF UNITS,
C     AND THEIR SCALING FACTORS.
C
      INCLUDE 'PARUNT.DEF'
      CHARACTER*(LENUNT) UPPCASE,CSTRING
      external paruntinit
C
C     CLEAR PARUNT.
C
      PARUNT = .FALSE.
C
C     IF THE STRING IS EMPTY, THE DIMENSIONLESS UNIT IS ASSUMED.
C
      IF ( STRING.EQ.' ' ) THEN
         IUNIT = NUNITS
         PARUNT = .TRUE.
      ELSE
C
C        LOOK FOR THE UNIT IN THE UNIT LIST.
C
         I = (INTCMD(CUNITS,STRING))
         IF (I.LT.0) THEN
C
C        AMBIGUOUS MATCH :SEARCH FOR EXACT FIT
C
           CSTRING=STRING(:LENGTH(STRING))
           DO I=1,NUNITS
              IF (CUNITS(I)(:LENGTH(CUNITS(I))).EQ.CSTRING) GO TO 500
           ENDDO
         ELSEIF ( I.GT.0 ) THEN
            GO TO 500

         ENDIF
      ENDIF
C
      RETURN
C     
 500  CONTINUE
C Check for 'null' - leave unit unchanged but return true.
      IF (STRING(1:2) .EQ. 'nu' .OR. STRING(1:2) .EQ. 'NU') THEN 
        PARUNT = .TRUE.
        RETURN
      ENDIF

      IUNIT = I
      PARUNT = .TRUE.
      RETURN
C
C END OF LOGICAL FUNCTION PARUNT.
C
      END
C
C **************************************************************
      DOUBLE PRECISION FUNCTION SUNIT ( IUNIT )
C **************************************************************
C
C RETURNS THE SCALING FACTOR FOR UNIT IUNIT.
C THE FACTOR IS IN THE SENSE THAT VALUES SUPPLIED BY THE USER
C     ARE MULTIPLIED BY THE FACTOR BEFORE STORAGE, AND INTERNAL
C     VALUES ARE DIVIDED BY THE FACTOR BEFORE DISPLAY.
C
C     COMMON BLOCK /CUNITS/ CONTAINS THE LIST OF UNITS,
C     AND THEIR SCALING FACTORS.
C
      INCLUDE 'PARUNT.DEF'
C
C     IF IUNIT IS OUT OF RANGE, RETURN A FACTOR OF UNITY,
C     OTHERWISE THE FACTOR IS OBTAINED FROM SUNITS.
C     null UNIT leaves SUNIT unchanged
      IF ( IUNIT.LE.0.OR.IUNIT.GT.NUNITS ) THEN
         SUNIT = 1.0D0
      ELSEIF (CUNIT(MIN(NUNITS,IUNIT)) .NE. 'null') THEN
         SUNIT = SUNITS(MIN(NUNITS,IUNIT))
      ENDIF
C
      RETURN
C
C END OF DOUBLE PRECISION FUNCTION SUNIT.
C
      END
C
C **************************************************************
      CHARACTER*(*) FUNCTION CUNIT ( IUNIT )
C **************************************************************
C
C RETURNS THE NAME OF UNIT IUNIT.
C
C     COMMON BLOCK /CUNITS/ CONTAINS THE LIST OF UNITS,
C     AND THEIR SCALING FACTORS.
C
      INCLUDE 'PARUNT.DEF'
C
C     IF IUNIT IS OUT OF RANGE, RETURN AN EMPTY NAME,
C     OTHERWISE THE NAME IS OBTAINED FROM CUNITS.
C
      IF ( IUNIT.LE.0.OR.IUNIT.GT.NUNITS ) THEN
         CUNIT = ' '
      ELSE
         CUNIT = CUNITS(IUNIT)
      ENDIF
C
      RETURN
C
C END OF CHARACTER*(*) FUNCTION CUNIT.
C
      END
C
C **************************************************************
      SUBROUTINE SHOUNT ( )
C **************************************************************
C
C PRODUCES A MONITOR DISPLAY OF THE AVAILABLE UNITS.
C
C
C     COMMON BLOCK /CUNITS/ CONTAINS THE LIST OF UNITS,
C     AND THEIR SCALING FACTORS.
C
      INCLUDE 'PARUNT.DEF'
C
C     PRODUCE THE DISPLAY.
C
      CALL SHOLST (CUNITS,'List of units :')
C
      RETURN
C
C END OF SUBROUTINE SHOUNT.
C
      END


