*DECK SHELL
C
C
C **************************************************************
      SUBROUTINE SHELL ( COMP, INDEX, N, INCR )
C **************************************************************
C
C PERFORMS A SHELL SORT ON N ELEMENTS.
C
C N REAL ELEMENTS ARE PROVIDED IN COMP(N) AND A CORRESPONDING
C     ARRAY OF INTEGERS IN INDEX(N).
C THE ELEMENTS OF BOTH ARRAYS ARE SORTED SO AS TO GIVE
C     INCREASING(DECREASING) ORDER IN COMP IF INCR IS TRUE(FALSE).
C
C     FOR FURTHER DETAILS SEE SECTION 7.6 OF "USE OF FILES" BY
C        D.R. JUDD ( MACDONALD )
C
      DIMENSION INDEX(*),COMP(*)
      LOGICAL INCR
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
C
C           COMPARE ELEMENTS.
C
            IF ( (COMP(I) .GT. COMP(I+M)) .EQ. INCR ) THEN
C
C              SWAP INDEX ELEMENTS.
C
               IHOLD     = INDEX(I)
               INDEX(I) = INDEX(I+M)
               INDEX(I+M) = IHOLD
C
C              SWAP COMP ELEMENTS.
C
               CHOLD    = COMP(I)
               COMP(I) = COMP(I+M)
               COMP(I+M) = CHOLD
               I = I-M
               IF ( I.GE.1 ) GOTO 20
            ENDIF
   30 CONTINUE
      GOTO 10
C
C END OF SUBROUTINE SHELL.
C
      END
