*DECK SHELLX
C
C
C **************************************************************
      SUBROUTINE SHELLX ( EXSWAP, EXCOMP, N )
C **************************************************************
C
C PERFORMS A USER DEFINED SHELL SORT ON N ELEMENTS.
C
C THE SORT IS DETERMINED BY THE EXTERNAL ROUTINES EXSWAP AND EXCOMP.
C
C     SUBROUTINE EXSWAP ( I,J ) PERFORMS A SWAP OF ELEMENTS I AND J
C
C     SUBROUTINE EXCOMP ( I,J,IFLAG ) RETURNS A POSITIVE INTEGER IN
C          IFLAG IF I AND J SHOULD BE SWAPPED OTHERWISE IT RETURNS ZERO.
C
C     FOR FURTHER DETAILS SEE SECTION 7.6 OF "USE OF FILES" BY
C        D.R. JUDD ( MACDONALD )
C
      EXTERNAL EXSWAP,EXCOMP
C
      LP = N
      M  = N
   10 CONTINUE
      LP = LP/2
      IF ( LP.EQ.0 ) THEN
         RETURN
      ENDIF
      IF ( M.LE.15 ) THEN
         M = LP/4*2+1
      ELSE
         M = LP/2*2+1
      ENDIF
      L = N-M
      DO 30 J=1,L
         I=J
   20    CONTINUE
            CALL EXCOMP (I,I+M,IFLAG)
            IF ( IFLAG.GT.0 ) THEN
               CALL EXSWAP (I,I+M)
               I = I-M
               IF ( I.GE.1 ) GOTO 20
            ENDIF
   30 CONTINUE
      GOTO 10
C
C END OF SUBROUTINE SHELLX.
C
      END
