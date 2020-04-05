*DECK GETEDT
C
C **************************************************************
      SUBROUTINE GETEDT
     &    ( GBLLST,PROMPT,CMDINP,MAXINP,NINP,ICMD,PROGID,IFAIL )
C **************************************************************
C THIS SUBROUTINE OBTAINS THE NEXT LINE FROM THE EDIT ARRAY AND PUTS
C   IT IN CMDINP.  IT EMULATES GETCMD.
C
      INCLUDE 'PSRDAT.DEF'
C

      CHARACTER*(*) GBLLST(*),PROMPT,CMDINP(*),PROGID
      INTEGER MAXINP, NINP, ICMD, IFAIL
C
C     RETURN IF NO COMMANDS TO BE READ.
      IF (IEDT.GE.NEDT) THEN
         NINP=0
         IFAIL=1
         RETURN
      ENDIF
C
      IEDT=IEDT+1
      NINP = NPAREDT(IEDT)
      DO I=1,NINP
        CMDINP(I) = EDTLST(I,IEDT)
      ENDDO
C
      JINP = 1
      ICMD = INTCMD (GBLLST,CMDINP(JINP))
      IF (ICMD.EQ.0) THEN
         IFAIL = 5
      ELSEIF (ICMD.LT.0) THEN
         IFAIL = 6
      ENDIF
C
C END OF SUBROUTINE GETEDT
C
      END



