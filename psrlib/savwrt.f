*DECK SAVWRT
C
C
C
      SUBROUTINE SAVWRT(PARS,NP,ISAVE,IUPTR,IUNIT,DCHAR,CCHAR,OUTBUF,
     &                  MAXCL,IFAIL)
C                                            
C This routine writes the parameters in PARS to the save file open on
C unit IUNIT as a command line.  On entry NP is the number of
C parameters, if it is zero or less then PARS(1) is written out       
C verbatim; ISAVE is greater than zero if the save file is open; IUPTR
C is greater than one if the parameters were obtained from an obey file;
C DCHAR is the delimiter character; CCHAR is the continuation character;
C and MAXCL is the the maximum allowed command line length.
C     Version 1.3   15th July, 1988
C
      CHARACTER*(*) PARS(*),DCHAR,CCHAR*1,NUPDEL*1,OUTBUF,ROUTN
      LOGICAL SPECHR,PRMSEP,EOLCHR,DELMT,QUOTE
      PARAMETER (ROUTN='SAVWRT')
C
C Is a save file open and are we at the basic command level?
C
      IF(ISAVE.GT.0.AND.IUPTR.LE.1) THEN
C
C Yes, check the number of parameters.
C
        IF(NP.LE.0) THEN
C               
C Write PARS(1) verbatim.
C
          WRITE(IUNIT,200,IOSTAT=IE,ERR=7777) PARS(1)(:LENGTH(PARS(1)))
        ELSE
C
C Load the parameters into the output buffer.  First obtain the maximum
C command line length allowing room for two delimiters, a continuation,
C two quotes, and a space at its beginning.
C
          MAXC = MIN(LEN (OUTBUF), MAXCL) - 7
C
C Set the character counter.
C
          IC=1
C
C Load each parameter into the buffer.
C
          DO 1 I=1,NP
C
C Search the current parameter for any special characters, separators,
C lower case characters, or command separators.
C
            DELMT = .FALSE.
            QUOTE = .FALSE.
            L=MAX(1,LENGTH(PARS(I)))
            DO 2 J=1,L
              IF (SPECHR(PARS(I)(J:J)) .OR. PRMSEP(PARS(I)(J:J))
     &            .OR. EOLCHR(PARS(I)(J:J))) THEN
C
C Found one, note that delimiters are required.
C
                DELMT=.TRUE.
              END IF
              IF (PARS(I)(J:J).GE.'a' .AND. PARS(I)(J:J).LE.'z')
     &                THEN
C
C A lower case character has been found: note that we must quote the
C parameter.
C
                QUOTE = .TRUE.
              END IF
    2       CONTINUE
C
C Now start to load the parameter into the output buffer.  First note
C where it starts.
C
            ICS=IC
C
C Does the parameter have to be quoted?
C
            IF (QUOTE) THEN
C
C Yes, load one.
C                                 
              OUTBUF(IC:) = NUPDEL()
              IC = IC + 1
            END IF
C
C Is a delimiter required?
C
            IF (DELMT) THEN
C
C Yes, load one.
C
              OUTBUF(IC:) = DCHAR
              IC = IC + 1
            END IF
C
C Load the parameter proper into the output buffer.
C
            DO 3 J=1,L
C
C Test if there is room for the next character.
C
              IF(IC.GT.MAXC) THEN
C
C No, write out the previous line up to the last parameter, and add on a
C continuation.
C
                WRITE(IUNIT,200,IOSTAT=IE,ERR=7777) OUTBUF(:ICS-1)//
     &                                              CCHAR
C
C Copy the current parameter as far as it has been loaded into the
C output buffer.
C
                DO 4 K=ICS,IC-1
                  OUTBUF(K-ICS+1:K-ICS+1)=OUTBUF(K:K)
    4           CONTINUE
C
C Set the character position.
C
                IC=IC-ICS+1
                ICS=1
              ENDIF
C
C Is the current character the same as the delimiting character and is
C the parameter delimited?
C
              IF(DELMT.AND.PARS(I)(J:J).EQ.DCHAR) THEN
C
C Yes, is it followed by a separator?
C
                IF(J.LT.L) THEN
                  IF(PRMSEP(PARS(I)(J+1:J+1)).OR.SPECHR(PARS(I)(J+1:J+1)
     &               )) THEN
C
C It is, load another of it.
C
                    OUTBUF(IC:)=DCHAR
                    IC=IC+1
                  ENDIF
                ELSE
C
C It is the last character in the parameter, load another of it.
C
                  OUTBUF(IC:)=DCHAR
                  IC=IC+1
                ENDIF
              ENDIF
C
C Load the current character into the buffer.
C
              OUTBUF(IC:)=PARS(I)(J:J)
              IC=IC+1
    3       CONTINUE
C
C Is a delimiter required at the end of the current parameter?
C
            IF(DELMT) THEN
C
C Yes, load one.
C
              OUTBUF(IC:)=DCHAR
              IC=IC+1
            ENDIF
C
C Does the parameter have to be quoted?
C
            IF (QUOTE) THEN
C
C Yes, load one.
C                                 
              OUTBUF(IC:) = NUPDEL()
              IC = IC + 1
            END IF
C
C Do the next parameter.
C
            IC=IC+2
    1     CONTINUE
C
C Write out the last line.
C
          WRITE(IUNIT,200,IOSTAT=IE,ERR=7777) OUTBUF(:IC-2)
        ENDIF
      ENDIF
C
C Clear the failure indicator.
C
      IFAIL=0
      RETURN
C
C Write failure, flag an error.
C
 7777 CALL LIBERR(ROUTN,69,0,IE,0.0,' ',IFAIL)
      RETURN
C
C Format statement.
C
  200 FORMAT(1X,A)
C
C End of subroutine savwrt.
C
      END
