*DECK CHARPL
C *********************************************************
      SUBROUTINE CHARPL ( CBUFF, NL,
     &                     A, N, NG, SCMIN, SCMAX )
C *********************************************************
C
C PLOTS THE DATA A(N) AVERAGED OVER NG POINTS IN THE TWO-DIMENSIONAL
C CHARACTER ARRAY CBUFF WHICH HAS NL LINES.
C THE SCALING IS CONTROLLED BY SCMIN AND SCMAX, SCMIN IS
C THE LEVEL OF THE FIRST LINE AND SCMAX IS THE LEVEL OF LINE
C NL
C
C IF SCMIN.EQ.SCMAX THEN THE SCALES ARE DETERMINED
C AUTOMATICALLY
C
      CHARACTER*(*) CBUFF(NL)
      DIMENSION A(*)
C
C NP IS THE NUMBER OF POINTS TO BE PLOTTED
C
      NP = MIN(LEN(CBUFF(1)),N/NG)
C
C IF SCALES ARE EQUAL, PERFORM AUTO-SCALING
C
      IF ( SCMIN.EQ.SCMAX ) THEN
         SUM = 0.0
         SUMSQ = 0.0
         DO 10 I=1,NP
            D = 0.0
            DO 20 J=1,NG
               D = D+A((I-1)*NG+J)
   20       CONTINUE
            SUM = SUM+D
            SUMSQ = SUMSQ+D*D
   10    CONTINUE
         RMS = SQRT((SUMSQ-SUM*SUM/NP)/(NP-1.0))
         IF ( RMS.LE.0.0 ) RMS = 1.0
         SC1 = RMS/5.
         SC2 = RMS*2.
      ELSE
         SC1 = SCMIN
         SC2 = SCMAX
      ENDIF
C
C DETERMINE THE BASELINE FROM THE OUTER TENTHS OF THE ARRAY
C
      N10 = MAX(1,N/10)
      D = 0.0
      DO 30 I=1,N10
         D = D+A(I)
         D = D+A(N-I+1)
   30 CONTINUE
      DC = D/(N10*2)*NG
C
C CLEAR THE CHARACTER BUFFER
C
      DO 40 J=1,NL
         CBUFF(J) = ' '
   40 CONTINUE
C
C PLOT THE ARRAY
C     NOTE THAT THE PLOT IS PRODUCED WITH THE TOP IN LINE 1
C        AND THE BOTTOM IN LINE NL, TO FACILITATE OUTPUT.
C
      DO 60 I=1,NP
         D = 0.0
         DO 70 J=1,NG
            D = D+A((I-1)*NG+J)
   70    CONTINUE
         J = NINT((D-DC-SC1)/(SC2-SC1)*(NL-1))+1
         IF ( J.GT.NL ) J=NL
         IF ( J.LT.1 ) J=1
         CBUFF(NL-J+1)(I:I) = '*'
   60 CONTINUE
C
      RETURN
C
C END OF SUBROUTINE CHARPL
C
      END
C
