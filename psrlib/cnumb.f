*DECK CNUMB
C
C **************************************************************
      CHARACTER*(*) FUNCTION CNUMB ( VAL, NSIG, LOG )
C **************************************************************
C
C RETURNS A CHARACTER STRING CONTAINING THE NUMBER VAL WRITTEN
C     TO NSIG SIGNIFICANT PLACES.
C LOG SPECIFIES THAT THE LOGARITHM OF THE VALUE IS TO BE TAKEN.
C
      LOGICAL LOG
      CHARACTER*10 FMT
C
      IF ( NSIG.LE.0 ) THEN
         CNUMB = ' '
         RETURN
      ENDIF
      IF ( VAL.EQ.0.0 ) THEN
         CNUMB = '0'
      ELSE
         ISIZE = ALOG10(ABS(VAL))+1
         NDEC = MAX(0,NSIG-ISIZE)
         IF ( VAL.LT.0.0 ) ISIZE = ISIZE+1
         IF ( VAL.GT.0.0.AND.VAL.LT.1.0 ) ISIZE = ISIZE+1
         IF ( ISIZE.GT.LEN(CNUMB) ) THEN
            NDEC = MAX(0,NDEC-(LEN(CNUMB)-ISIZE))
            ISIZE = LEN(CNUMB)
         ENDIF
         IF ( NDEC.LE.0 ) THEN
            WRITE (FMT,100) ISIZE
            WRITE (CNUMB,FMT) INT(SIGN(ABS(VAL)+0.5,VAL))
         ELSE
            WRITE (FMT,200) ISIZE+NDEC+1,NDEC
            WRITE (CNUMB,FMT) VAL
         ENDIF
      ENDIF
C
      RETURN
C
C     FORMATS
C
  100 FORMAT ( '(I',I3,')' )
  200 FORMAT ( '(F',I3,'.',I3,')' )
C
C END OF CHARACTER*(*) FUNCTION CNUMB
C
      END
