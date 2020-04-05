cDECK CRTIMS
c
c
c **************************************************************
c **************************************************************
c
c RETURNS A CHARACTER STRING REPRESENTING A TIME IN SECONDS
c     IN THE FORM HH:MM:SS.SSS...SSS
c     IDEC SPECIFIES THE NUMBER OF DECIMAL PLACES.
c
      character *(*)function crtims(tim, idec)
      character fmt*20, ctimes*8, buf*40
c
c     TAKE MODULUS ONE DAY
c
      double precision t, tim
c
c     WRITE TO BUFFER AND READ BACK TO ENSURE CORRECT ROUNDING
c
      t = mod(tim,86400.0d0)
      write(unit=fmt, fmt=100) idec + 6, idec
      write(unit=buf, fmt=fmt) t
      if (idec .gt. 0) then
      write(unit=fmt, fmt=110) idec + 1, idec
      read(unit=buf, fmt=fmt) it, t
      else
      read(unit=buf, fmt=120) it
c
c     WRITE THE TWO PARTS SEPARATELY
c
      end if
      crtims = ctimes(it)
      if (idec .gt. 0) then
      write(unit=fmt, fmt=100) idec + 1, idec
      write(unit=crtims(9:), fmt=fmt) t
c
      end if
c
      return 
  100 format(2h(F,i3,1h.,i3,1h))
  110 format(5h(I5,F,i3,1h.,i3,1h))
c
c END OF CHARACTER FUNCTION CRTIMS
c
  120 format(i5)
      end
