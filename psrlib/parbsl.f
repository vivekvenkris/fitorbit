*DECK PARBSL
C***********************************************************************
      SUBROUTINE PARBSL(CMDINP,JINP,NINP,ISCEP1,ISCEP2,IBSLP1,IBSLP2,
     &            IBSLSTEP,ISTAT)
C***********************************************************************

C PARSES A STRING OF THE FORM
C  <iscep1> {TO <iscep2>} <ibslp1> {TO <ibslp2> {<ibslstep>}}
C where all values in angle brackets are integers
C used in PSRPPM plot commands to specify what is to be plotted
C returns ISTAT<0 if the routine could not parse the string
C         bit1 set if iscep1=iscep2
C         bit2 set if ibslp1=ibslp2
C         bit3 set if ibstep not specified

      CHARACTER CMDINP(*)*(*) 
      LOGICAL PARTOO,PARINT

      ISTAT=0

C parse the first 'to' clause
      IF(PARTOO(CMDINP,JINP,NINP,ISCEP1,ISCEP2,ISTAT)) THEN
        ISTAT=ISTAT/2

C if successful parse the second 'to' clause
        IF (PARTOO(CMDINP,JINP,NINP,IBSLP1,IBSLP2,ISTAT)) THEN
          IF(JINP.LE.NINP) THEN

C if successful and there are still parameters on the command line 
C then test for the presence of a 'step' clause
            IF (PARINT(CMDINP(JINP),IBSLSTEP)) THEN
              JINP=JINP+1
            ELSE
              ISTAT=-1
              RETURN
            ENDIF
          ELSE

C here if  there is no step clause but the rest is O.K.
            ISTAT=ISTAT+4
            IBSLSTEP=1
          ENDIF
        ELSE
          ISTAT=-1
          RETURN
        ENDIF
      ELSE
        ISTAT=-1
      ENDIF

      END   
