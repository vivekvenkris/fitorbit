cDECK IUTSEC
c
c
c
c
c THIS ROUTINE RETURNS THE UT IN SECONDS SINCE THE STARTING DATE,
c IZDATE, TO IUT ON IDATE.
c     VERSION 1.1   28FEB83
c
      function iutsec(izdate, idate, iut, ifail)
c
c TEST THE CURRENT DATE.
c
      character cdate*8
c
c DEFAULT DATE, ALWAYS EQUAL TO THE STARTING DATE.
c
      if (idate .eq. 0) then
      iutsec = iut
      ifail = 0
c
c CHECK THE DATE.
c
      else
c
c THE CURRENT DATE IS LESS THAN THE STARTING DATE, FLAG AN ERROR.
c
      if (idate .lt. izdate) then
      call liberr('IUTSEC', 21, 0, 0, 0.0, cdate(izdate), ifail)
      iutsec = iut
c
c EVALUATE THE UT.
c
      else
c
c CHECK THAT THE UT IS VALID.
c
      iutsec = idsts(izdate,idate,iut)
c
c THE TIME DIFFERENCE WAS TOO LARGE, FLAG AN ERROR.
c
      if (iutsec .ge. maxint()) then
      call liberr('IUTSEC', 21, 1, 0, 0.0, ' ', ifail)
c
c THE TIME IS NOT TOO LARGE, CLEAR IFAIL.
c
      else
      ifail = 0
      end if
      end if
      end if
c
c END OF INTEGER FUNCTION IUTSEC.
c
      return 
      end
