*DECK FROMTO
C   
C   
C   
      SUBROUTINE FROMTO (CMDINP,NINP,JINP,NUMBOK,DATEOK,FRMCMD  
     &                   ,VAL,VDATE,DATE,TIMOPT,DEFJT,IFAIL)
C   
C THIS SUBROUTINE RETURNS THE VALUE IN A FROM/TO PHRASE.
C THE INPUT COMMANDS FROM CMDINP(JINP) TO CMDINP(NINP) ARE EXAMINED 
C     AND THE VALUE RETURNED IN VAL.
C NUMBOK SPECIFIES WHETHER THE VALUE MAY BE AN ORDINARY 
C     ( DOUBLE PRECISION ) NUMBER AND DATEOK SPECIFIES WHETHER IT   
C     MAY BE A DATE/TIME.   
C VDATE INDICATES WHETHER VAL IS A DATE/TIME.   
C DATE INDICATES WHETHER IN THE CASE OF A DATE/TIME THE DATE ONLY   
C     SPECIFIED.
C FRMCMD SHOULD CONTAIN THE PRECEDING FROM/TO COMMAND,  
C     I.E. 'FROM', 'TO' OR ' '. 
C DEFJT IS THE DEFAULT JULIAN TIME FOR PARSING DATE/TIME VALUES.
C ON EXIT JINP IS UPDATED TO POINT AT THE NEXT INPUT PARAMETER. 
C   
      CHARACTER*(*) CMDINP(*),FRMCMD,TIMOPT(*)  
      DOUBLE PRECISION VAL,DVAL,DBLMAX  
      LOGICAL PARDBL,DATEOK,NUMBOK,VDATE,DATE,TIME,CMDJT
C   
C     CLEAR THE ERROR FLAG AND DATE FLAGS.  
C   
      IFAIL = 0 
      VDATE = .FALSE.   
      DATE = .FALSE.
C   
C     SET THE DEFAULT DATE/TIME.
C   
      DVAL = DEFJT  
C   
C     CHECK THE NUMBER OF PARAMETERS.   
C   
      IF(JINP.LE.NINP) THEN 
C   
C       LOOK IN THE TIME OPTIONS LIST.  
C   
        II=INTCMD(TIMOPT,CMDINP(JINP))  
C   
C       IF NUMBERS ARE ALLOWED, TEST FOR A NUMBER.  
C   
        IF (NUMBOK.AND.PARDBL(CMDINP(JINP),DVAL)) THEN  
C   
C         RETURN THE NUMBER.
C   
          VAL = DVAL
          JINP = JINP+1 
C   
C       IF DATE/TIMES ARE ALLOWED, TEST FOR A DATE/TIME.
C   
        ELSEIF ( DATEOK.AND.
     &           CMDJT(CMDINP,NINP,JINP,DATE,TIME,DVAL) ) THEN  
C   
C          RETURN THE DATE/TIME VALUE.  
C   
           VAL = DVAL   
           VDATE = .TRUE.   
           DATE = DATE.AND..NOT.TIME
C   
C       IF THE COMMAND WAS 'FROM', CHECK FOR 'BEGINNING' OR 'START'.
C   
        ELSEIF ( FRMCMD.EQ.'FROM'.AND.(II.GT.0.AND.II.LE.2) ) THEN  
C   
C          RETURN THE MINIMUM.  
C          IT IS A DATE IF NUMBERS ARE NOT ALLOWED. 
C   
           VAL = -DBLMAX()  
           VDATE = DATEOK.AND..NOT.NUMBOK   
C   
C       IF THE COMMAND WAS 'TO', CHECK FOR 'FINISH' OR 'END'.   
C   
        ELSEIF ( FRMCMD.EQ.'TO'.AND.II.GT.2) THEN   
C   
C          RETURN THE MAXIMUM.  
C          IT IS A DATE IF NUMBERS ARE NOT ALLOWED. 
C   
           VAL = DBLMAX()   
           VDATE = DATEOK.AND..NOT.NUMBOK   
        ELSE
C   
C          THE CURRENT COMMAND HAS NOT BEEN RECOGNIZED. 
C          GIVE AN ERROR MESSAGE APPROPRIATE TO WHAT WAS EXPECTED.  
C   
           IF ( DATEOK ) THEN   
              IFAIL = 56
           ELSEIF ( NUMBOK ) THEN   
              IFAIL = 19
           ELSE 
              IFAIL=20  
           ENDIF
           CALL PSRERR('FROMTO',IFAIL,0,0.0,CMDINP(JINP))   
         ENDIF  
      ELSE  
C   
C        THERE ARE NO MORE PARAMETERS AFTER THE FROM/TO. FLAG AN ERROR. 
C   
         IFAIL=20   
         CALL PSRERR('FROMTO',IFAIL,0,0.0,CMDINP(NINP)) 
      ENDIF 
C   
      RETURN
C   
C END OF SUBROUTINE FROMTO. 
C   
      END   
