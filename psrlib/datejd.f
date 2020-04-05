cDECK DATEJD
c  
c  ******************************************************************
c  ******************************************************************
c  
c  RETURNS THE DATE IDAT ( YYMMDD ) CONVERTED TO A TRUNCATED JULIAN
c  DATE
c  
      double precision function datejd(idat)
      integer monthd(12)
c  
      data monthd / 31, 28, 31, 30, 31, 30, 2*31, 30, 31, 30, 31 /
      iyr = int(idat / 10000) + 1900
      imn = mod(idat / 100,100)
      idy = mod(idat,100)
      nd = iyr * 365.25
      if (mod(iyr,4) .eq. 0) then
         monthd(2) = 29
         nd = nd - 1
      else
         monthd(2) = 28
      end if
      do 10 i = 1, imn - 1
         nd = nd + monthd(i)
 10   continue
      datejd = DBLE((nd + idy) - 678956)
c  
c  END OF REAL FUNCTION DATEJD
c  
      return 
      end
