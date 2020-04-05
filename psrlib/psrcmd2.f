*DECK PSRCMD2
C
C **************************************************************
      SUBROUTINE PSRCMD2 ( PROGID,
     &     CMDLST, MAXCMD, CMDDSC, MAXDSC, ICMDGRP, ICMD,
     &     CMDINP, JINP, NINP, MAXINP, 
     &     PARLST, PARDSC, PARID, MAXPAR, IPARGRP,
     &     IPAR, RPAR, DPAR, LPAR, CPAR,
     &     IIUNITS, IRUNITS, IDUNITS, IFAIL )
C **************************************************************
C
C THE PULSAR COMMAND INTERPRETER.
C PROGID CONTAINS THE PROGRAM NAME.
C CMDLST CONTAINS THE LISTS OF COMMANDS WITH UP TO MAXCMD COMMANDS
C     IN EACH LIST. CMDLST(*,0) IS THE PRIMARY COMMAND LIST AND
C     CMDLST(*,I) IS THE LIST OF SUBSIDIARY COMMANDS FOR PRIMARY
C     COMMAND I.
C CMDDSC CONTAINS THE COMMAND DESCRIPTIONS WITH TWO DESCRIPTION
C     LINES FOR EACH COMMAND.
C ICMDGRP AND ICMD RETURN THE COMMAND GROUP AND COMMAND NUMBER THAT
C     HAVE BEEN REQUESTED BY THE USER.
C CMDINP RETURNS THE ACTUAL COMMANDS GIVEN BY THE USER, WHERE NINP
C     ARE THEIR NUMBER AND JINP POINTS TO THE NEXT COMMAND TO BE
C     INTERPRETED. MAXINP IS THE MAXIMUM NUMBER OF INPUT COMMANDS
C     ALLOWED.
C PARLST CONTAINS THE LIST OF PARAMETERS AND PARDSC THEIR DESCRIPTIONS,
C     WITH TWO DESCRIPTION LINES PER PARAMETER.
C PARID CONTAINS THE LISTS OF PARAMETER IDENTIFIERS AS REQUIRED BY THE
C     PARAMETER HANDLING ROUTINES AND MAXPAR IS THE MAXIMUM NUMBER OF
C     PARAMETERS. THERE MUST BE A PARAMETER IDENTIFIER FOR EACH 
C     PARAMETER IN THE LIST.
C IPARGRP CONTAINS THE NUMBER OF THE PARAMETER IDENTIFIER LIST WHICH IS
C     TO BE USED FOR EACH PRIMARY COMMAND, ENABLING THE PARAMETER LIST
C     TO BE CHANGED FOR DIFFERENT COMMANDS. THERE MUST BE AS MANY
C     ENTRIES IN IPARGRP AS THERE ARE PRIMARY COMMANDS, ALTHOUGH AN
C     ENTRY MAY BE ZERO INDICATING THAT NO PARAMETERS MAY BE ACCESSED
C     FROM THAT COMMAND.
C IPAR,RPAR,LPAR,DPAR,CPAR,IIUNITS,IRUNITS,IDUNITS
C     ARE THE PARAMETERS AND THEIR UNITS. THEIR USE IS DESCRIBED IN THE
C     PARAMETER HANDLING ROUTINES.
C IFAIL SHOULD BE ZERO IF COMMAND PROCESSING WAS SUCCESSFUL IN THE
C     CALLING PROGRAM, OTHERWISE IT SHOULD CONTAIN THE ERROR NUMBER.
C

c  Paul Harrison 24-SEP-1987  this is a new version of the PSR command
C interpreter that is to look essentially the same from the outside, but
C has a radically changed internal structure so that all commands are sent
C through the OLAF command interpreter.
c Mod 10/03/2006 Correct data type in call to pserr
c

      PARAMETER ( IEXCMD=5,IOPSCMD=6,NGLOBCMD=6 )
      INTEGER IFIRST
      CHARACTER*(*) PROGID,CMDLST(MAXCMD,0:*),CMDDSC(MAXDSC,0:*)
     &   ,CMDINP(*),PARLST(*),PARDSC(*),CPAR(*),PARID(MAXPAR,0:*)
      INTEGER IPARGRP(0:*),IPAR(*),IIUNITS(*),IRUNITS(*),IDUNITS(*)
      REAL RPAR(*)
      DOUBLE PRECISION DPAR(*)
      LOGICAL LPAR(*),COMSTR,LPRIMARY,lprimglob
C
      CHARACTER IDENT*81,PROMPT*20,CPRMPT*1,OPSCLI*10,OUTBUF*81
      CHARACTER*10 GBLLST(NGLOBCMD+1)
c The maximum length of commands set in the external program must be as
c set below, AND THE MAXIMUM NUMBER IMPORTANT
      character*20 allcmds(200),SUBCMDS(100)
      CHARACTER*60 GBLDSC(NGLOBCMD*2+2)
C
      DATA CPRMPT / '>' /
      DATA GBLLST
     &   / 'show','set','scan','graphics','exit','dcl',' ' /
      DATA GBLDSC
     &   / 'Display something',' '
     &    ,'Change one of the parameters',' '
     &    ,'Scan through the parameters',' '
     &    ,'Access the graphics commands',' '
     &    ,'Exit the current command level',' '
     &    ,'Obey simple DCL commands',' ',' ',' ' /
      SAVE
C
C     FIRST TIME, SET THE HELP DIRECTORY AND ERROR HANDLING STATUSES.
C
      IF (IFIRST.EQ.0) THEN
        IFIRST = 1
        LPRIMARY=.TRUE.
        CALL SETHLP ('PSRHELPDIR:PSR.HLB')         
        CALL SETLER (0,1)
        CALL SETLER (1,2)
        CALL SETPER (0,2)
        CALL SETPER (1,2)
C put all primary commands into the allcomand buffer
        I=1
        DO WHILE(I.LE.MAXCMD .AND. CMDLST(I,0).NE.' ')
          ALLCMDS(I)=CMDLST(I,0)                               
          I=I+1
        ENDDO
        NLOCCMD=I-1
        DO J=1,NGLOBCMD+1
          ALLCMDS(I)=GBLLST(J)
          I=I+1
        ENDDO
      ENDIF
C
C     SET THE COMMAND NAME AND DESCRIPTION FOR PASSING
C     COMMANDS TO THE OPERATING SYSTEM.
C       
      GBLLST(IOPSCMD) = OPSCLI()
      IF ( GBLLST(IOPSCMD).NE.' ' ) THEN
         GBLDSC(2*IOPSCMD-1) = 'Enter '
     &      //GBLLST(IOPSCMD)(1:LENGTH(GBLLST(IOPSCMD)))//' command'
      ENDIF
C     
C     
      IF(LPRIMARY) ICMDGRP=0
 10   IF (ICMDGRP.EQ.0) THEN
         IDENT = PROGID
         PROMPT = CPRMPT
      ELSE
C a command that has subcommands is still to have the subcommand specified
         IDENT = PROGID(1:MAX(1,LENGTH(PROGID)))//' '
     &            //CMDLST(ICMDGRP,0)
         PROMPT = CMDLST(ICMDGRP,0)(1:LENGTH(CMDLST(ICMDGRP,0)))
     &            //CPRMPT
      ENDIF

C set the ambiguous command errors to quiet non fatal
      CALL SETLER(6,2)
      CALL SETLER(5,2)
      CALL SETPER(6,1)
      IF ( IFAIL.EQ.0.AND.JINP.LE.NINP.AND.NINP.GT.0 ) THEN

C still some commands on the command line
         IF(.NOT.LPRIMARY) THEN
           I=1
           DO WHILE(I.LE.MAXCMD .AND. CMDLST(I,ICMDGRP).NE.' ')
             SUBCMDS(I)=CMDLST(I,ICMDGRP)
             I=I+1                                   
           ENDDO
           NSUBCMD=I-1
           DO J=1,NGLOBCMD+1
             SUBCMDS(I)=GBLLST(J)
             I=I+1
           ENDDO
           NPCMD=NSUBCMD
           ICMDT=INTCMD(SUBCMDS,CMDINP(JINP))
           JINP=JINP+1
         ELSE   
           ICMDT=INTCMD(ALLCMDS,CMDINP(JINP))
           NPCMD=NLOCCMD
           JINP=JINP+1
         ENDIF
         IF(ICMDT.LT.0) IFAIL=6
         IF(ICMDT.EQ.0) IFAIL=5
      ELSE
C a call must be made to getcmd with the full list if command list empty
         IF(.NOT.LPRIMARY) THEN
           I=1
           DO WHILE(I.LE.MAXCMD .AND. CMDLST(I,ICMDGRP).NE.' ')
             SUBCMDS(I)=CMDLST(I,ICMDGRP)
             I=I+1                                   
           ENDDO
           NSUBCMD=I-1
           DO J=1,NGLOBCMD+1
             SUBCMDS(I)=GBLLST(J)
             I=I+1
           ENDDO
           NPCMD=NSUBCMD
           CALL GETCMD
     &            (SUBCMDS,PROMPT,CMDINP,MAXINP,NINP,ICMDT,IDENT,IFAIL)
           JINP=2
         ELSE
           CALL GETCMD
     &          ( ALLCMDS,PROMPT,CMDINP,MAXINP,NINP,ICMDT,IDENT,IFAIL )
           NPCMD=NLOCCMD
           JINP=2
         ENDIF
      ENDIF
C deal with unrecognised command that could be an implicit SET
      IF(IFAIL.EQ.5) THEN
C check that there is a parameter list.
            JINP=JINP-1
            IF ( IPARGRP(ICMDGRP).GT.0 ) THEN
              CALL SETCMD ( CMDINP,NINP,JINP
     &              ,PARLST,PARDSC,PARID(1,IPARGRP(ICMDGRP))
     &              ,IPAR,RPAR,DPAR,LPAR,CPAR
     &              ,IIUNITS,IRUNITS,IDUNITS,IFAIL )
            ELSE
              IFAIL = 5
              CALL PSRERR ('PSRCMD',IFAIL,0,0.0,cmdinp(jinp))
              ninp=0
            ENDIF
            GOTO 10
       ENDIF   

         
C deal with ambiguous commands
      IF(IFAIL.EQ.6) THEN
        IF(LPRIMARY) THEN
          IAMBCMD=INTCMD(GBLLST,ALLCMDS(-ICMDT))
        ELSE
          IAMBCMD=INTCMD(GBLLST,SUBCMDS(-ICMDT))
        ENDIF
        IF(IAMBCMD.LE.0) THEN
C there was a true ambiguity in the command
          CALL SETPER(6,2)
          CALL PSRERR('PSRCMD',IFAIL,0,0.,CMDINP(JINP-1))
          JINP=0
          NINP=0
          GOTO 10
        ELSE  
C put the global command in the tempcommand and the local command
C into the ambiguous command pointer
          ITEMP=IAMBCMD
          IAMBCMD=ABS(ICMDT)
          IF(LPRIMARY) THEN
            ICMDT=NLOCCMD+ITEMP
          ELSE
            ICMDT=NSUBCMD+ITEMP
          ENDIF
          IFAIL=0
        ENDIF
      ELSE
        IAMBCMD=0
      ENDIF                 

C cancel an unrecognised error
      IF(IFAIL.EQ.26) IFAIL=0
      IF(IFAIL.EQ.0) THEN

C the command was recognised        
        CALL SETPER(6,2)
        CALL SETLER(6,1)

 20     IF(ICMDT-NPCMD.GT.0) THEN
C the command was a global command
          IF(IAMBCMD.NE.0) THEN 
            CALL SETPER(26,1)
            CALL SETLER(26,2)
          ENDIF

C 'SHOW' COMMAND -------------------------------------------------------
          IF ( ICMDT-NPCMD.EQ.1 ) THEN
C
C        IF THERE IS NO PARAMETER LIST,              
C        SUPPLY SHWCMD WITH AN EMPTY LIST.           
C
            IF ( IPARGRP(ICMDGRP).EQ.0 ) THEN
              CALL SHWCMD ( CMDINP,NINP,JINP,PROGID
     &             ,GBLLST,GBLDSC,CMDLST(1,ICMDGRP),CMDDSC(1,ICMDGRP)
     &             ,' ',' ',' '
     &             ,IPAR,RPAR,DPAR,LPAR,CPAR
     &             ,IIUNITS,IRUNITS,IDUNITS,IFAIL )
            ELSE
              CALL SHWCMD ( CMDINP,NINP,JINP,PROGID
     &             ,GBLLST,GBLDSC,CMDLST(1,ICMDGRP),CMDDSC(1,ICMDGRP)
     &             ,PARLST,PARDSC,PARID(1,IPARGRP(ICMDGRP))
     &             ,IPAR,RPAR,DPAR,LPAR,CPAR
     &             ,IIUNITS,IRUNITS,IDUNITS,IFAIL )
            ENDIF

C 'SET' COMMAND --------------------------------------------------------
          ELSEIF ( ICMDT-NPCMD.EQ.2 ) THEN

C check that there is a parameter list.

            IF ( IPARGRP(ICMDGRP).GT.0 ) THEN
              CALL SETCMD ( CMDINP,NINP,JINP
     &              ,PARLST,PARDSC,PARID(1,IPARGRP(ICMDGRP))
     &              ,IPAR,RPAR,DPAR,LPAR,CPAR
     &              ,IIUNITS,IRUNITS,IDUNITS,IFAIL )
            ELSE
              IFAIL = 42
              CALL PSRERR ('PSRCMD',IFAIL,0,0.0,' ')
            ENDIF

C 'SCAN' COMMAND -------------------------------------------------------
                                         
          ELSEIF ( ICMDT-NPCMD.EQ.3 ) THEN

C check that there is a parameter list.

            IF ( IPARGRP(ICMDGRP).GT.0 ) THEN

C perform the scan.

              CALL SCNPAR ( CMDINP,NINP,JINP
     &              ,IDENT,PARLST,PARDSC,PARID(1,IPARGRP(ICMDGRP))
     &              ,IPAR,RPAR,DPAR,LPAR,CPAR,IIUNITS,IRUNITS,IDUNITS )
            ELSE
              IFAIL = 42
              CALL PSRERR ('PSRCMD',IFAIL,0,0.0,' ')
            ENDIF
                             
C 'GRAPHICS' COMMAND ---------------------------------------------------
C     
          ELSEIF ( ICMDT-NPCMD.EQ.4 ) THEN
C 
              CALL GRPCMD ( CMDINP,NINP,JINP
     &              ,PARLST,PARDSC,PARID(1,0)
     &              ,IPAR,RPAR,DPAR,LPAR,CPAR
     &              ,IIUNITS,IRUNITS,IDUNITS,IFAIL )
                    
C 'EXIT' COMMAND -------------------------------------------------------

          ELSEIF ( ICMDT-NPCMD.EQ.5 ) THEN
            IF(ICMDGRP.GT.0) THEN
C if inside a primary command then just jump out
              ICMD=0
              ICMDGRP=0
              NINP=0
              IFAIL=0
              LPRIMARY=.TRUE.
C and go back to the top of the subroutine
              GOTO 10
            ELSE
C set parameters to indicate to outside that exit from program required
              ICMD=0   
              ICMDGRP=0
              RETURN
            ENDIF

C
C PASS COMMANDS TO OPERATING SYSTEM ------------------------------------
C
          ELSEIF ( ICMDT-NPCMD.EQ.6 ) THEN
            CALL OPSCMD (CMDINP,JINP,NINP)
          ENDIF
          CALL SETPER(26,2)
          CALL SETLER(26,1)
          CALL SETPER(6,2)

C if it did  fail 
          IF(IFAIL.NE.0) THEN 
            IF(IAMBCMD.NE.0)THEN
C it was an global command that was ambiguous, but then found something
C it did not recognise
              ICMDT=IAMBCMD
              IFAIL=0
            ELSE
C there was some other sort of failure which will already have been
C signaled, but still loose rest of input
              JINP=0
              NINP=0
              GOTO 10
            ENDIF
          ELSE
C global command execution OK
            IF(LPRIMARY) THEN
              ICMD=0
              ICMDGRP=0
            ENDIF
            GOTO 10
          ENDIF

        ENDIF


C the command was a local command

        IF(LPRIMARY) THEN
          ICMDGRP=ICMDT

C now see if there is a subcommand required
          IF(CMDLST(1,ICMDGRP).NE.' ') THEN
C yes there is
            IF(JINP.LE.NINP) THEN
C there is someting else on the command line
              ICMD=INTCMD(CMDLST(1,ICMDGRP),CMDINP(JINP))
              IF(ICMD.GT.0) THEN
C   subcommand has been recognised
                   JINP=JINP+1   
                   IFAIL=0

              ELSE
                  lprimary=.false.
C   subcommand not recognised or ambiguous
C   so try for a global command
                  ICMDT=INTCMD(GBLLST,CMDINP(JINP))
                  IF(ICMDT.GT.0) THEN
C   if GLOBAL back to start of subroutine indicating that this is a
C   global command following a primary
                    LPRIMARY=.TRUE.
                    GOTO 10
                  ENDIF
                  IFAIL=5
                  IF(ICMD.LT.0) IFAIL=IFAIL+1
                  CALL PSRERR('PSRCMD',IFAIL,0,0.,CMDINP(JINP))
                  JINP=0
                  NINP=0
                  GOTO 10
              ENDIF
            ELSE
C command needs a subcommand but there is no more input 
              LPRIMARY=.FALSE.    
              GOTO 10
            ENDIF
          ENDIF
        ELSE
C subcommand has been parsed successfully
          ICMD=ICMDT
        ENDIF
      ELSEIF(IFAIL.EQ.1) THEN
C end of file means exit            
        IF(ICMDGRP.GT.0) THEN
C if inside a primary command then just jump out
          ICMD=0
          ICMDGRP=0
          JINP=0
          NINP=0
          IFAIL=0
C and go back to the top of the subroutine
          LPRIMARY=.TRUE.
          GOTO 10
        ELSE
C set parameters to indicate to outside that exit from program required
          ICMD=0
          ICMDGRP=0
          IFAIL=0
        ENDIF
      ELSE
      
C some other error was found so go to top of subroutine loosing input
        CALL PSRERR('PSRCMD',IFAIL,0,0.0,CMDINP(JINP-1))
        jinp=0
        NINP=0
        GOTO 10
      ENDIF

C
      RETURN
      
C
C END OF SUBROUTINE PSRCMD
C
      END
