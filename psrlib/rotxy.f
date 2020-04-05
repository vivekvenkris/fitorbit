*DECK ROTXY
C
C **************************************************************
      SUBROUTINE ROTXY ( XIN, YIN, XOUT, YOUT, ANGLE )
C **************************************************************
C
C ROTATES XIN,YIN ANTICLOCKWISE ABOUT 0,0 BY ANGLE DEGREES
C     TO GIVE XOUT,YOUT.
C
      IF ( ANGLE.EQ.0.0 ) THEN
C
C        FAST RETURN IF ANGLE IS ZERO
C
         XOUT = XIN
         YOUT = YIN
      ELSE
         S = SIND(ANGLE)
         C = COSD(ANGLE)
         XOUT = C*XIN - S*YIN
         YOUT = S*XIN + C*YIN
      ENDIF
C
      RETURN
C
C END OF SUBROUTINE ROTXY
C
      END
