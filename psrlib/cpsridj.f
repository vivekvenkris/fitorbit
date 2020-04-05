*DECK CPSRIDJ
C
C ************************************************************
      CHARACTER*12 FUNCTION CPSRIDJ ( RA, DEC )
C ************************************************************
C
C FORMS THE PULSAR NAME FROM THE RA AND DEC ( DEGREES )
C IN THE FORM HHMMsDDdd
C WHERE HH ARE THE HOURS OF RA
C       MM ARE THE MINUTES OF RA
C       s  IS THE SIGN OF DEC
C       DD ARE THE DEGREES OF DEC
c       dd is the minutes of dec
c
      WRITE (CPSRIDJ(1:2),100) INT(RA/15)
      WRITE (CPSRIDJ(3:4),100) INT((RA-INT(RA/15)*15)*4)
      IF ( DEC.LT.0.0 ) THEN
          CPSRIDJ(5:5) = '-'
      ELSE
          CPSRIDJ(5:5) = '+'
      ENDIF
      WRITE (CPSRIDJ(6:7),100) INT(ABS(DEC))
      write (cpsridj(8:9),100) int((abs(dec)-int(abs(dec)))*60)
C
      CPSRIDJ(10:12) = '   '
C
      RETURN
C
  100 FORMAT ( I2.2 )
C
C END OF CHARACTER FUNCTION CPSRID
C
      END
