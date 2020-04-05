*DECK VECCPU
C
C **************************************************************
      LOGICAL FUNCTION VECCPU ( )
C **************************************************************
C
C RETURNS THE VALUE .TRUE. IF A VECTOR PROCESSOR IS AVAILABLE.
C
C THIS ROUTINE IS INSTALLATION DEPENDENT.
C
C VAX-11 FORTRAN VERSION.
C
C     THERE IS NO VECTOR PROCESSOR.
C
      VECCPU = .FALSE.
      RETURN
C
C END OF LOGICAL FUNCTION VECCPU
C
      END
