cDECK IDSTS
c
c
c
c
c RETURNS THE TIME IN SECONDS FROM ISDATE (YYMMDD) TO IUT (SECONDS) ON
c DATE IDATE (YYMMDD).
c     VERSION 1.2   28FEB83
c
c EVALUATE THE NUMBER OF DAYS DIFFERENCE IN SECONDS AND ADD ON THE TIME.
c
      function idsts(isdate, idate, iut)
c
c CHECK THAT THE NUMBER IS NOT TOO LARGE TO FIT INTO AN INTEGER.
c
      rdsts = (real(idydif(isdate,idate)) * 86400.0) + iut
c
c IT IS NOT, RETURN IDSTS.
c
      if (abs(rdsts) .lt. real(maxint())) then
      idsts = nint(rdsts)
c
c IT IS TOO LARGE, RETURN IDSTS AS MAXINT.
c
      else
      idsts = maxint()
      end if
c
c END OF INTEGER FUNCTION IDSTS.
c
      return 
      end
