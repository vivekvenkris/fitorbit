*DECK RMSSRT
C
C
C ******************************************************
      REAL FUNCTION RMSSRT ( D, N )
C ******************************************************
C
C RETURNS THE RMS OF THE SHORT DATA ARRAY D(N).
C
C THIS ROUTINE IS INSTALLATION DEPENDENT.
C
C VAX-11 FORTRAN VERSION.
C
      BYTE D(*)
C
C     TRAP FOR SMALL N.
C
      IF ( N.LE.1 ) THEN
          RMSSRT = 0
          RETURN
      ENDIF
C
C     COMPUTE THE RMS.
C
      SUM=0.
      SUMSQ=0.
      DO J=1,N
         S=D(J)
         SUM=SUM+S
         SUMSQ=SUMSQ+S*S
      ENDDO
      RMSSRT=SQRT(MAX(0.0,(SUMSQ-SUM*SUM/N)/(N-1.0)))
C
      RETURN
C
C END OF REAL FUNCTION RMSSRT.
C
      END
