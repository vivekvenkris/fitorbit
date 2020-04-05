cDECK IDYZ
c
c
c
c
c RETURNS THE NOTIONAL NUMBER OF DAYS SINCE YEAR 1000 FOR THE DATE
c SPECIFIED BY IY, IM AND ID.  IY IS IN YYYY.
c     VERSION 1.1   28FEB83
c
c EVALUATE THE NUMBER OF YEARS DIFFERENCE FROM 1000.
c
      function idyz(iy, im, id)
c
c CHECK THE MONTH.
c
      iy1 = iy - 1000
c
c JANUARY OR FEBRUARY.
c
      if (im .le. 2) then
      idyz = ((((365 * iy1) + id) + (31 * (im - 1))) + ((iy1 - 1) / 4))
     & - ((3 * (((iy1 - 1) / 100) + 1)) / 4)
c
c MARCH TO DECEMBER.
c
      else
      idyz = (((((365 * iy1) + id) + (31 * (im - 1))) - int((0.4 * real(
     &im)) + 2.3)) + (iy1 / 4)) - ((3 * ((iy1 / 100) + 1)) / 4)
      end if
c
c END OF INTEGER FUNCTION IDYZ.
c
      return 
      end
