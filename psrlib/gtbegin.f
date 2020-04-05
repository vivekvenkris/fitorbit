*DECK GTBEGIN
C
C **************************************************************
      SUBROUTINE GTBEGIN ( )
C **************************************************************
C
C TEMPORARY GRAPHICS INITIALIZATION ROUTINE
C
      TYPE 1000
 1000 FORMAT(' WORKSTATION TYPE? ( GOC=3 ) ',$)
C
      READ (*,*) IWS
      CALL HIGR_GZBGN (4,0,IWS,2)
      RETURN
C
C END OF SUBROUTINE GTBEGIN
C
      END
