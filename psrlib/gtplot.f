*DECK GTPLOT
C
C **************************************************************
      SUBROUTINE GTPLOT ( X, Y, N, TEXT )
C **************************************************************
C
C TEMPORARY PLOTTING ROUTINE
C
      DIMENSION X(*),Y(*)
      CHARACTER*(*) TEXT
      CHARACTER*80 BUF
C
      BUF = TEXT(1:LENGTH(TEXT))//'$'
      CALL HIGR_DRPLOT(X,Y,N,'ms$','$',%REF(BUF),'$' )
      RETURN
C
C END OF SUBROUTINE GTPLOT
C
      END
