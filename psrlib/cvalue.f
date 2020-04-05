*DECK CVALUE
C
C
C **************************************************************
      CHARACTER*(*) FUNCTION CVALUE ( VAL, ERR, NDEC )
C **************************************************************
C
C FORMS A CHARACTER VALUE CONSISTING OF VALUE +/- ERROR
C     THE NUMBER OF DECIMAL PLACES IS DETERMINED BY THE ERROR
C        TO GIVE TWO SIGNIFICANT FIGURES FOR THE ERROR
C     IF THE ERROR IS .LT. 0 A BLANK STRING IS RETURNED
C     IF THE ERROR IS .EQ. 0 THEN THE ERROR IS NOT PRINTED
C                            AND NDEC DECIMAL PLACES ARE GIVEN
C
      DOUBLE PRECISION VAL,ERR
      CHARACTER BUF*40,FMT*40,RSIZE*2
C
C     NEGATIVE ERROR INDICATES VALUE NOT SET
C
      IF ( ERR.LT.0.0 ) THEN
         CVALUE = ' '
         RETURN
      ENDIF
C
      IF ( ERR.EQ.0 ) THEN
C
C        ZERO ERROR INDICATES ERROR NOT SET
C        PRECISION DEFAULTS TO NDEC DECIMAL PLACES
C
         BUF = RSIZE(VAL)
         READ (BUF,'(I2)') NF
         NF = NF+NDEC-3
         WRITE (FMT,995) NF,NDEC
         WRITE (CVALUE,FMT) VAL
      ELSE
C
C        DETERMINE PRECISION REQUIRED TO MAKE ERROR AN INTEGER
C           WITH TWO SIG FIGS
C
         WRITE (BUF,999) ERR
         READ  (BUF(6:9),998) ND
         ND = -ND+2
         READ  (BUF(3:4),997) IER
         IF ( ND.LT.0 ) THEN
            IER = NINT(ERR)
            ND = 0
         ENDIF
         IF ( ND.EQ.2 ) THEN
C
C           TWO DECIMAL PLACES FOR BOTH VALUE AND ERROR
C
            FMT = '(F??.2,1X,F??.2)'
            CALL SETRFM (VAL,FMT)
            CALL SETRFM (ERR,FMT)
            WRITE (CVALUE,FMT) VAL,ERR
         ELSEIF ( ND.EQ.1 ) THEN
C
C           ONE DECIMAL PLACE FOR BOTH VALUE AND ERROR
C
            FMT = '(F??.1,1X,F??.1)'
            CALL SETRFM (VAL,FMT)
            CALL SETRFM (ERR,FMT)
            WRITE (CVALUE,FMT) VAL,ERR
         ELSEIF ( ND.GT.0 ) THEN
C
C           ND DECIMAL PLACES FOR VALUE, INTEGER ERROR
C
            BUF = RSIZE(VAL)
            READ (BUF,'(I2)') NF
            NF = NF+ND-3
            WRITE (FMT,996) NF,ND
            WRITE (CVALUE,FMT) VAL,IER
         ELSE
C
C           OUTPUT BOTH AS INTEGERS
C
            FMT = '(I?,1X,I?)'
            CALL SETIFM (NINT(VAL),FMT)
            CALL SETIFM (IER,FMT)
            WRITE (CVALUE,FMT) NINT(VAL),IER
         ENDIF
      ENDIF
C
      RETURN
C
  999 FORMAT (D8.2)
  998 FORMAT (I3)
  997 FORMAT (I2)
  996 FORMAT ('(F',I3,'.',I3,',1X,I2)')
  995 FORMAT ('(F',I3,'.',I3,')')
C
C END OF CHARACTER FUNCTION CVALUE
C
      END
