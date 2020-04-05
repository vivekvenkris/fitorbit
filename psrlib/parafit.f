      LOGICAL FUNCTION PARAFIT3 (Y0,Y1,Y2,XM,YM)
C
C Subroutine to perform a parabolic fit to three equispaced points.
C XM returns the position of the peak or minimum relative to the position
C    of Y1 in units of the point spacing, and YM returns its amplitude.
C The function is .TRUE. if the fit is successful.
C
      PARAFIT3 = .FALSE.
      DENOM = (Y0 - 2*Y1 + Y2)
      IF (ABS(DENOM).LT.1E-20) RETURN
      XM = 0.5*(Y0 - Y2)/DENOM
      YM = Y1 - 0.25*(Y0 - Y2)*XM
      PARAFIT3 = .TRUE.
      END
C
      LOGICAL FUNCTION PARAFIT5 (Y0,Y1,Y2,Y3,Y4,XM,YM)
C
C Subroutine to perform a parabolic fit to five equispaced points.
C XM returns the position of the peak or minimum relative to the position
C    of Y2 in units of the point spacing, and YM returns its amplitude.
C The function is .TRUE. if the fit is successful.
C
      PARAFIT5 = .FALSE.
      DENOM = (2*(Y0-Y4) - (Y3-Y1) - 2*Y2)
      IF (ABS(DENOM).LT.1E-20) RETURN
      XM = 0.7*((Y1-Y3) + 2*(Y0-Y4))/DENOM
      YM = Y2 - 0.25*((Y1-Y3) + 2*(Y0-Y4))*XM
      PARAFIT5 = .TRUE.
      END
C
C
      LOGICAL FUNCTION DPARAFIT3 (Y0,Y1,Y2,XM,YM)
C
C Subroutine to perform a parabolic fit to three equispaced points.
C XM returns the position of the peak or minimum relative to the position
C    of Y1 in units of the point spacing, and YM returns its amplitude.
C The function is .TRUE. if the fit is successful.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DPARAFIT3 = .FALSE.
      DENOM = (Y0 - 2*Y1 + Y2)
      IF (ABS(DENOM).LT.1D-20) RETURN
      XM = 0.5*(Y0 - Y2)/DENOM
      YM = Y1 - 0.25*(Y0 - Y2)*XM
      DPARAFIT3 = .TRUE.
      END
C
      LOGICAL FUNCTION DPARAFIT5 (Y0,Y1,Y2,Y3,Y4,XM,YM)
C
C Subroutine to perform a parabolic fit to five equispaced points.
C XM returns the position of the peak or minimum relative to the position
C    of Y2 in units of the point spacing, and YM returns its amplitude.
C The function is .TRUE. if the fit is successful.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DPARAFIT5 = .FALSE.
      DENOM = (2*(Y0-Y4) - (Y3-Y1) - 2*Y2)
      IF (ABS(DENOM).LT.1D-20) RETURN
      XM = 0.7*((Y1-Y3) + 2*(Y0-Y4))/DENOM
      YM = Y2 - 0.25*((Y1-Y3) + 2*(Y0-Y4))*XM
      DPARAFIT5 = .TRUE.
      END
