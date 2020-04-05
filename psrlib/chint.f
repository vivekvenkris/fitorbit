c
c Returns the positive integer I as a two character string including
c any leading zero.
c
c Argument:
c  I       Input  INTEGER  The integer to convert.
c
c     Version 1.0    11th November, 1981
c
c Declare the routine's argument.
c
      character *2function chint(i)
c
c Declare local variables.
c
      integer i
      integer i1, i2
      character nums(0:9)*1
      save nums
c
c Test I.
c
c
c The integer is out of range.
c
      data nums / '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' /
      if ((i .lt. 0) .or. (i .gt. 99)) then
      chint = '**'
c
c It is within range, split it into two digits.
c
      else
      i1 = i / 10
c
c Return as characters.
c
      i2 = i - (i1 * 10)
      chint(1:1) = nums(i1)
      chint(2:2) = nums(i2)
c
c End of character*2 function CHINT.
c
      end if
      end
