*DECK RMS
C
C
C ******************************************************
      REAL FUNCTION RMS ( D, N )
C ******************************************************
C
C RETURNS THE RMS OF THE REAL DATA ARRAY D(N)
C
      DIMENSION D(*)
C
      IF ( N.LE.1 ) THEN
          RMS = 0
          RETURN
      ENDIF
      SUM=0.
      SUMSQ=0.
      DO 10 J=1,N
         S=D(J)
         SUM=SUM+S
         SUMSQ=SUMSQ+S*S
   10 CONTINUE
      RMS=SQRT(MAX(0.0,(SUMSQ-SUM*SUM/N)/(N-1.0)))
C
      RETURN
C
C END OF REAL FUNCTION RMS
C
      END
