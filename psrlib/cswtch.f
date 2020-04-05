*DECK CSWTCH
C
C ***********************************************************************
      CHARACTER*(*) FUNCTION CSWTCH ( LVAL )
C ***********************************************************************
C
C RETURNS A CHARACTER STRING REPRESENTING THE STATE OF SWITCH LVAL.
C
      LOGICAL LVAL
      CHARACTER*(*) ON,OFF
      PARAMETER ( ON='ON',OFF='OFF' )
C
C     RETURN THE APPROPRIATE STRING.
C
      IF ( LVAL ) THEN
         CSWTCH = ON
      ELSE
         CSWTCH = OFF
      ENDIF
C
      RETURN
C
C END OF CHARACTER*(*) FUNCTION CSWTCH.
C
      END
