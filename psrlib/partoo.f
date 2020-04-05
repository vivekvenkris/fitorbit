
*DECK PARTOO
C***********************************************************************
      LOGICAL FUNCTION PARTOO(CMDINP,JINP,NINP,IFIRST,ISECN,ISTAT)
C***********************************************************************

C PARSES A TO PHRASE SETS BIT 2 OF ISTAT IF IFIRST=ISECN

C Paul Harrison  8-FEB-1987 
c Mod 10/03/2006 Correct data type in call to pserr
c

      CHARACTER CMDINP(*)*(*)
      LOGICAL PARINT,COMSTR

      PARTOO=.FALSE.

      IF(JINP.GT.NINP) THEN
        ISTAT=-1
        IFAIL=23
        CALL PSRERR('PARTOO',IFAIL,0,0.,' ')
        RETURN
      ENDIF
C test to see if the first argument is an integer
      IF(PARINT(CMDINP(JINP),IFIRST)) THEN
        JINP=JINP+1

C if no more arguments then assume no 'to'
        IF(JINP.GT.NINP) THEN
          ISTAT=ISTAT+2
          ISECN=IFIRST
          PARTOO=.TRUE.
          RETURN
        ENDIF
                                            
C see if 'to' is the next clause
        IF(COMSTR('TO',CMDINP(JINP))) THEN

C see if there is an integer next in the clause
          JINP=JINP+1
          IF(JINP.GT.NINP) THEN
            ISTAT=-1
            IFAIL=23
            CALL PSRERR('PARTOO',IFAIL,0,0.,' ')
            RETURN
          ENDIF
          IF(PARINT(CMDINP(JINP),ISECN)) THEN

C if there is a second integer then update the pointer to point at the next
C clause
            JINP=JINP+1
            PARTOO=.TRUE.
          ELSE
            ISTAT=-1
            RETURN
          ENDIF
        ELSE

C there was a valid  integer first but no 'to' leave it to another
C routine to decide if the next thing is valid or not.
          PARTOO=.TRUE.
          ISTAT=ISTAT+2
          ISECN=IFIRST
        ENDIF
      ELSE
        ISTAT=-1
        IFAIL=19
        CALL PSRERR('PARTOO',IFAIL,0,0.,CMDINP(JINP))
      ENDIF

      END
