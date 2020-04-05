*DECK PRECES
C
C
C **************************************************************
      SUBROUTINE PRECES (X,Y,Z,D,DP,XP,YP,ZP)
C **************************************************************
C
C              PRECESSES CARTESIAN COORDINATES FROM JULIAN
C                         DATE D TO JULIAN DATE DP
C                   ONE OF WHICH IS TO BE 1950.0
C                        EXPRESSED IN EPHEMERIS TIME
C NUTATION IS NOT INCLUDED
C               SET D OR DP = 0.0 TO SPECIFY EPOCH 1950.0
C
C              USE LENGTH OF TROPICAL YEAR AT 1969.0
C                       IN EPHEMERIS DAYS
C
C NOTE THAT ALL ARGUMENTS ARE DOUBLE PRECISION
C
      DOUBLE PRECISION X,Y,Z,D,DP,XP,YP,ZP
C
      YT = 365.24219455
      D50 = -33281.923
      TO = 0.0
      IF (D.EQ.0)  GO TO 2
    1 T  = 0.01 * (D+D50)/YT
      GO TO 3
    2 T  = 0.01 * (DP+D50)/YT
    3 T2 =  T*T
      T3 = T*T2
      E8 = 1.0E-08
      XX = 1-((29696+ 26*TO)* T2 + 13*T3)*E8
      YX = (-(2234941 + 1355* TO)* T -676*T2 + 221*T3)*E8
      ZX = (-(971690 - 414*TO) * T + 207* T2 + 96* T3 )*E8
      YY = 1- ((24975 + 30*TO) * T2 + 15* T3)*E8
      YZ = (-(10858 + 2*TO) * T2)*E8
      ZY = YZ
      ZZ = 1 -(4721 - 4*TO)*T2*E8
      XY = -YX
      XZ = -ZX
      IF (D.EQ.0) GO TO 6
C
C            EQUATIONS FOR DATE TO 1950.0
C
    5 XP =  XX* X  + XY* Y  + XZ* Z
      YP =  YX* X  + YY* Y  + YZ* Z
      ZP =  ZX* X  + ZY* Y  + ZZ* Z
       GO TO 7
C
C            USE TRANSPOSE OF MATRIX FOR 1950.0 TO DATE
C
    6 XP =  XX*X  + YX* Y  + ZX* Z
      YP =  XY*X  + YY* Y  + ZY* Z
      ZP =  XZ* X  + YZ* Y  + ZZ* Z
    7 RETURN
C
C END OF SUBROUTINE PRECES
C
      END
