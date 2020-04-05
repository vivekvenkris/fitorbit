cDECK JDDATE
c
c ***************************************************************
c ***************************************************************
c
c RETURNS THE MODIFIED JULIAN DATE XJD CONVERTED INTO CALENDAR DATE
c    AS YYMMDD
c
      integer function jddate(xjd)
      double precision xjd
      dimension monthd(12)
c
      data monthd / 31, 28, 31, 30, 31, 30, 2*31, 30, 31, 30, 31 /
      nd = int(xjd) + 678957
      iyr = nd / 365.25
      if (iyr .lt. 0) then
      jddate = 0
      return 
      end if
      nd = nd - int((iyr * 365.25) + 0.9)
      if (nd .eq. 0) iyr = iyr - 1
      if (mod(iyr,4) .eq. 0) then
      monthd(2) = 29
      ndyr = 366
      else
      monthd(2) = 28
      ndyr = 365
      end if
      if (nd .eq. 0) nd = ndyr
      do 10 imn = 1, 12
      if (nd .le. monthd(imn)) goto 100
      nd = nd - monthd(imn)
   10 continue
  100 continue
      jddate = (((iyr - 1900) * 10000) + (imn * 100)) + nd
c
c END OF INTEGER FUNCTION JDDATE
c
      return 
      end
