*DECK FILTYP
C
C **************************************************************
      SUBROUTINE FILTYP ( FILNAM, TYP, ICON )
C **************************************************************
C
C GIVES THE FILENAME FILNAM FILETYPE TYP
C IF ICON=0 THE FILETYPE IS ONLY CHANGED IF IT WAS NOT SPECIFIED
C           IN FILNAM
C OTHERWISE THE FILETYPE IS REPLACED
C
C
C THIS ROUTINE IS INSTALLATION DEPENDENT
C
C VAX-11 FORTRAN VERSION
C
      CHARACTER*(*) FILNAM,TYP
C
C     IF FILENAME IS BLANK, RETURN
C
      IF ( FILNAM.EQ.' '.OR.FILNAM(1:1).EQ.'<'
     &     .OR.FILNAM.EQ.'VDU' ) RETURN
C
C     LOOK FOR A '.' THAT COMES AFTER A POSSIBLE ']'
C
      IUIC = INDEX(FILNAM,']')
      IDOT = INDEX(FILNAM(IUIC+1:),'.')
C
C     IF THERE IS NO SUCH '.' APPEND THE FILE TYPE
C     OTHERWISE REPLACE THE FILETYPE IF REQUESTED
C
      IF ( IDOT.LE.0 ) THEN
         FILNAM = FILNAM(1:LENGTH(FILNAM))//'.'//TYP(1:LENGTH(TYP))
      ELSEIF ( ICON.NE.0 ) THEN
         FILNAM = FILNAM(1:IDOT)//TYP(1:LENGTH(TYP))
      ENDIF
C
      RETURN
C
C END OF SUBROUTINE FILTYP
C
      END
