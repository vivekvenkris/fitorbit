*DECK SHOPRT
C
C
C **************************************************************
      SUBROUTINE SHOPRT ( FILE, PRTQUE, DELETE )
C **************************************************************
C
C DISPLAYS ON THE MONITOR CHANNEL THE CURRENT STATUS OF THE
C     OUTPUT FILE AND THE PRINT OPTIONS.
C FILE   IS THE OUTPUT FILE NAME.
C PRTQUE IS THE PRINT QUEUE NAME.
C DELETE SPECIFIES WHETHER THE FILE IS TO BE DELETED AFTER PRINTING.
C
      INCLUDE 'PSRLIB.DEF'
C
      LOGICAL DELETE
      CHARACTER*(*) FILE,PRTQUE
      CHARACTER*40 OUTBUF*81,FMT,UPPCASE
      CHARACTER VERB*5
C
C     START WITH A BLANK LINE.
C
      CALL OUTMON (' ')
C
C     REPORT ON THE OUTPUT FILE.
C
      IF ( UPPCASE(FILE).EQ.'<NONE>' ) THEN
C
C        NO OUTPUT FILE.
C
         CALL OUTMON (' There is no current output file')
C
C        SET THE VERB FOR THE PRINT QUEUE REPORT.
C
         VERB = 'would'
      ELSE
C
C        FILE NAME.
C
         CALL OUTMON (' The current output file is '//FILE)
C
C        FOR AN OPENED, PAGINATED FILE, 
C        REPORT ON THE NUMBER OF PAGES WRITTEN.
C
         IF ( LUOUT.GT.0 ) THEN
         IF ( LUPAGL(LUOUT).GT.0 ) THEN
            CALL OUTMON (' ')
C
C           TEST THE NUMBER OF PAGES WRITTEN.
C
            IF ( LUPAG(LUOUT).GT.0 ) THEN
C
C              REPORT THE NUMBER OF PAGES WRITTEN.
C
               FMT = '(6X,I?,'' page! written'')'
               CALL SETIFM (LUPAG(LUOUT),FMT)
               WRITE (OUTBUF,FMT) LUPAG(LUOUT)
               CALL OUTMON (OUTBUF)
            ELSE
C
C              NO PAGES WRITTEN.
C
               CALL OUTMON ('       The file is empty')
            ENDIF
         ENDIF
         ENDIF
C
C        SET THE VERB FOR THE PRINT QUEUE REPORT.
C
         VERB = 'will'
      ENDIF
      CALL OUTMON (' ')
C
C     IF THE FILE IS THE DEFAULT OUTPUT, NO MORE INFORMATION 
C     IS TO BE GIVEN.
C
      IF ( UPPCASE(FILE).EQ.'<DEFAULT>'.OR.UPPCASE(FILE).EQ.'<VDU>' ) 
     &   RETURN
C
C     REPORT ON THE PRINT QUEUE.
C
      IF ( UPPCASE(PRTQUE).EQ.'<NONE>' ) THEN
C
C        NO QUEUE SPECIFIED.
C
         CALL OUTMON (' The file '//VERB(1:LENGTH(VERB))
     &        //' not be printed')
      ELSE
C
C        REPORT ON THE PRINT QUEUE.
C
         CALL OUTMON (' The file '//VERB(1:LENGTH(VERB))
     &        //' be printed on queue '//PRTQUE)
C
C        REPORT ON THE DELETE OPTION.
C
         IF ( DELETE ) THEN
            CALL OUTMON (' It '//VERB(1:LENGTH(VERB))
     &        //' be deleted after printing')
         ELSE
            CALL OUTMON (' It '//VERB(1:LENGTH(VERB))
     &        //' not be deleted after printing')
         ENDIF
      ENDIF
C
      CALL OUTMON (' ')
C
      RETURN
C
C     END OF SUBROUTINE SHOPRT.
C
      END
