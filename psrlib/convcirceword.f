      DOUBLE PRECISION FUNCTION DLWORD (CIRCEREAL)

      INTEGER II, JJ
      BYTE CIRCEREAL(6),DI(3),DJ(3)
      EQUIVALENCE(II,DI),(JJ,DJ)

*+
* Converts CIRCE double length floating form to double precision.
*
* Input arguments:
*          BYTE(6) ARRAY STRAIGHT FROM INPUT
* Value returned:
*          Double precision equivalent Circe floating point number.
*-

      DOUBLE PRECISION DMAN
      DO I=1,3
        DI(I)=CIRCEREAL(4-I)
        DJ(I)=CIRCEREAL(7-I)
      ENDDO

* Extract both parts of the mantissa.
      DMAN = II * 2.0D0**16 + JJ / 256

* Extract the exponent.
      IEXP = IAND (JJ, '377'O)

* Prevent overflow.
      IF (DMAN .LE. 1.0D-15) THEN

        DLWORD = 0.0D0

      ELSE

* Sign extend.
        IF (II .GE. 2**23) DMAN = DMAN - 1.09951163D12

* Scale the exponent.
        IF (IEXP .GE. '200'O) IEXP = IEXP - '400'O

* Prevent overflow.
        IEXP = MAX (IEXP, -87)

* Scale the mantissa by the exponent.
        DLWORD = DMAN * 2.0D0**(IEXP-39)

      ENDIF

* End of double precision function DLWORD.
      END

      REAL FUNCTION SLWORD (CIRCEREAL)

      BYTE CIRCEREAL(3),CIRCEREALW(3)
      INTEGER II
      EQUIVALENCE (CIRCEREALW,II)
*+
* Converts CIRCE single length to real.
* 
* Input arguments:
* BYTE(3) ARRAY CIRCEREAL
*   II   - 24-bit real number, made up of 16 bits mantissa, plus 8-bit 
*          biased exponent.
*
* Value returned:
*          Real equivalent of Circe 24-bit (short) real.
*-
      CIRCEREALW(1)=CIRCEREAL(3)
      CIRCEREALW(2)=CIRCEREAL(2)
      CIRCEREALW(3)=CIRCEREAL(1)

* Extract and scale the exponent.
      IEXP = IAND (II, '377'O)
      IF (IEXP .GE. '200'O) IEXP = IEXP - '400'O

* Prevent overflow.
      IEXP = MAX (IEXP, -111)

* Extract the mantissa.
      RMAN = INT(II / 256)

      IF (RMAN .EQ. 0.0) THEN

        SLWORD = 0.0

      ELSE

* Sign extend.
        IF (RMAN .GE. 2.0**15) RMAN = RMAN - 2.0**16

* Scale the mantissa by the exponent.
        SLWORD = RMAN * 2.0**(IEXP-15)

      ENDIF

* End of real function SLWORD.
      END
C***********************************************************************
      INTEGER FUNCTION INTWORD(CIRCEINT)
C***********************************************************************
C
C Converts a three byte CIRCE integer (which has MSByte first) to a
C VAX FORTRAN integer*4 which has LSByte first
C N.B. reads in 3 bytes 
C
C Paul Harrison  23-NOV-1986 
C 
      BYTE VAXBYTE(4),CIRCEINT(3)
      INTEGER*4 VAXINT
      EQUIVALENCE (VAXBYTE,VAXINT)
C
      VAXBYTE(3)=CIRCEINT(1)
      VAXBYTE(2)=CIRCEINT(2)
      VAXBYTE(1)=CIRCEINT(3)
      IF(VAXINT.GT.2**23) THEN
        VAXBYTE(4)='FF'X
      ELSE
        VAXBYTE(4)=0
      ENDIF
      INTWORD=VAXINT
      END
