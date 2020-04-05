cDECK RSIZEN
c
c
c
c
c THIS ROUTINE RETURNS THE NUMBER OF CHARACTER LOCATIONS REQUIRED TO
c PRINT A REAL VALUE TO A SPECIFIED NUMBER OF PLACES OF DECIMALS.  THE
c NUMBER OF CHARACTERS IS RETURNED AS A CHARACTER STRING IN THE RANGE
c ' 1' TO '99'.
c INPUT ARGUMENTS:
c  RL       - THE REAL VALUE
c  ND       - THE NUMBER OF DECIMAL PLACES TO WHICH IT IS REQUIRED.
c OUTPUT ARGUMENT:
c  RSIZEN   - THE TOTAL NUMBER OF CHARACTER LOCATIONS REQUIRED TO PRINT
c             THE REAL VALUE TO THE SPECIFIFIED NUMBER OF PLACES OF
c             DECIMALS.  THE VALUE IS RETURNED AS A CHARACTER STRING.
c     VERSION 1.0   22AUG84
c
      character *2function rsizen(rl, nd)
c
c ENSURE THAT THE NUMBER OF PLACES OF DECIMALS IN IN RANGE.
c
      character buffer*6
c
c OBTAIN THE ROUNDED VALUE.
c
      n = min(27,max(0,nd))
c
c OBTAIN THE INTEGRAL PART IF THE VALUE IS NOT TOO LARGE.
c
      temp = abs(roundn(rl,n))
c
c GET THE INTEGRAL PART.
c
      if (temp .lt. real(maxint())) then
      i = temp
c
c SET A NON-ZERO INTEGRAL PART.
c
      else
      i = 1
c
c TEST IF THERE IS AN INTEGRAL PART.
c
      end if
c
c THERE IS AN INTEGRAL PART, ADJUST TEMP IF THE REAL IS NEGATIVE.
c
      if (i .ne. 0) then
c
c NOW EVALUATE THE NUMBER OF DIGITS.
c
      if (rl .lt. 0.0) temp = max(10.0,temp * 10.0)
      j = max(1,min(99,(int(log10(temp)) + 2) + n))
c
c THERE IS NO INTEGRAL PART, FIND OUT WHETHER THIS SYSTEM PUTS A ZERO IN
c FRONT OF ITS DECIMAL POINTS.
c
      else
      write(unit=buffer, fmt='(F6.3)') rl
c
c IT DOES, ALLOW ONE EXTRA SPACE FOR IT.
c
      if (buffer(2:2) .eq. '0') then
      j = 2 + n
c
c IT DOES NOT.
c
      else
      j = 1 + n
c
c ALLOW ONE MORE CHARACTER IF THE REAL IS NEGATIVE.
c
      end if
      if (rl .lt. 0.0) j = j + 1
c
c ARE NO DECIMAL PLACES REQUIRED?
c
      end if
c
c YES, FIND OUT IF THIS SYSTEM PUTS A ZERO AFTER THE DECIMAL POINT IN
c THESE CIRCUMSTANCES.
c
      if (n .le. 0) then
      if (i .le. 0) then
      write(unit=buffer, fmt='(F6.0)') 0.0
      else
      write(unit=buffer, fmt='(F6.0)') sign(1.0,rl)
      end if
c
c IT DOES, ALLOW ONE MORE CHARACTER FOR IT.
c
      if (buffer(6:6) .eq. '0') then
      j = j + 1
      end if
c
c TURN INTO CHARACTER.
c
      end if
      write(unit=rsizen, fmt='(I2)') j
c
c END OF CHARACTER FUNCTION RSIZEN.
c
      return 
      end
