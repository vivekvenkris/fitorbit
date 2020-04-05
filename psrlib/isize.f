cDECK ISIZE
c
c
c
c
c RETURNS THE NUMBER OF DIGITS IN AN INTEGER VALUE AS A CHARACTER,
c '1'.LE.ISIZE.LE.'9'.
c     VERSION 1.0   15JUL81
c
c EVALUATE NUMBER OF DIGITS.
c
      character *1function isize(intg)
      if (intg .lt. 0) then
      i = abs(intg) * 10
      else
      i = intg
      end if
      if (i .ne. 0) then
      i = max(1,min(9,int(log10(real(i) + 0.5)) + 1))
      else
      i = 1
c
c TURN INTO CHARACTER.
c
      end if
      write(unit=isize, fmt='(I1)') i
c
c END OF CHARACTER FUNCTION ISIZE.
c
      return 
      end
