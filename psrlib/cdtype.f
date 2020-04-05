*DECK CDTYPE
C
C
C ****************************************************************
      CHARACTER*8 FUNCTION CDTYPE ( )
C ****************************************************************
C
C RETURNS A SINGLE WORD DESCRIBING THE DATA TYPE OF THE CURRENT
C INPUT DATA BLOCK
C THE WORDS ARE :
C     DATATYPE 0,1,2 'DATA'
C     DATATYPE 3     SUBTYPE  1 'PEH' ETC.
C     DATATYPE 4     'SEARCH'
C
      INCLUDE 'PSRDAT.DEF'
C
      PARAMETER ( NSTYPE=1 )
      CHARACTER*8 CSTYPE(NSTYPE)
     &   / 'PEH' /
C
      IF ( DATATYPE.LE.2 ) THEN
         CDTYPE = 'DATA'
      ELSEIF ( DATATYPE.EQ.3 ) THEN
         CDTYPE = CSTYPE(IN(INRD))
      ELSEIF ( DATATYPE.EQ.4 ) THEN
         CDTYPE = 'SEARCH'
      ENDIF
      RETURN
C
C END OF CHARACTER FUNCTION CDTYPE
C
      END
