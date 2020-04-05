*DECK RFFT
C
C **************************************************************
      SUBROUTINE RFFT ( X, N )
C **************************************************************
C
C REAL TO COMPLEX FFT OF X(N)
C
C THIS ROUTINE IS INSTALLATION DEPENDENT
C
C VAX-11 FORTRAN VERSION
C
C USE THE DISC BASED FFT SUPPLIED BY PKM
C LIBRARY IS [PKM.FAST]FFT.OLB
C
      DIMENSION X(*)
C
      MEXA=INT(LOG(N+1.)/LOG(2.))
      CALL FFT_RFFT(MEXA,1,1,-1,1.0,X,N,-1)
C
      RETURN
C
C END OF SUBROUTINE RFFT
C
      END
