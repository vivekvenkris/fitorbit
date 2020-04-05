cDECK SETRFM
c
c
c
c
c THIS ROUTINE ADJUSTS THE FORMAT STATEMENT IN FMT READY FOR USE IN
c PRINTING THE REAL RVAL.  THE FOLLOWING OPERATIONS ARE CARRIED OUT:
c 1.   THE FIRST OCCURRENCE, IF ANY, OF THE STRING 'F??.' IS REPLACED
c      WITH 'FNN.', WHERE N IS THE EXACT FIELD WIDTH REQUIRED TO PRINT
c      RVAL;
c 2.   THE FIRST OCCURRENCE, IF ANY, OF THE STRING '!' IS REPLACED WITH
c      'S' IF RVAL IS GREATER THAN ONE, OR IS REMOVED ENTIRELY IF RVAL
c      IS LESS THAN OR EQUAL TO ONE.
c THIS ROUTINE HAS A SECOND ENTRY POINT (RSTRFM) WHICH CAN BE USED TO
c RESTORE THE FORMAT TO ITS ORIGINAL STATE IF IT IS CALLED BEFORE SETRFN
c IS USED AGAIN.
c INPUT ARGUMENTS:
c  RVAL       - THE VALUE THAT IS TO BE PRINTED.
c  FMT        - THE FORMAT STATEMENT AS DESCRIBED ABOVE.
c OUTPUT ARGUMENT:
c  FMT        - THE FORMAT STATEMENT AS DESCRIBED ABOVE.
c     VERSION 1.1   22AUG85
c
      subroutine setrfm(rval, fmt)
c
c WCHARS IS THE CHARACTERS MARKING THE 'VARIABLE' FIELD;
c SCHAR IS THE CHARACTER MARKING THE POSSIBLE 'S' LOCATION.
c
      character fmt*(*), wchars*(*), schar*(*), rsizen*2, buffer*2
c
c SAVE THE LOCATIONS OF WCHARS AND SCHAR IN THE LAST FORMAT PROCESSED SO
c THAT IT CAN BE RESTORED.
c
      parameter (wchars = '??', schar = '!')
      save locs, locw
c
c OBTAIN THE LOATION OF THE F FIELD DESCRIPTOR TO ADJUST.
c
c
c IS THERE ONE?
c
      data locw / 0 /
      data locs / 0 /
      locw = index(fmt,('F' // wchars) // '.')
c
c YES, POINT TO THE LOCATION OF THE NUMBER OF PLACES OF DECIMALS.
c
      if (locw .gt. 0) then
c
c FIND THE END OF THIS NUMBER.
c
      locn = index(fmt(locw:),'.') + locw
      do 1 i = locn, min(length(fmt),locn + 1)
c
c FOUND IT.
c
      if ((fmt(i:i) .lt. '0') .or. (fmt(i:i) .gt. '9')) then
      goto 1000
      end if
c
c OBTAIN THIS NUMBER AS AN INTEGER.
c
    1 continue
 1000 i = i - 1
      if (i .ge. locn) then
      buffer = fmt(locn:i)
      read(unit=buffer, fmt='(BN,I2)', iostat=i) n
      if (iostat .ne. 0) n = 0
      else
      n = 0
c
c NOW POINT TO THE CHARACTERS TO REPLACE.
c
      end if
c
c REPLACE THEM WITH THE CORRECT FIELD WIDTH.
c
      locw = locw + 1
      fmt(locw:locw + 1) = rsizen(rval,n)
c
c OBTAIN THE LOCATION OF THE SURROGATE 'S'.
c
      end if
c
c IS THERE ONE?
c
      locs = index(fmt,schar)
c
c YES, IS AN 'S' TO BE PRINTED?
c
      if (locs .gt. 0) then
c
c THERE IS, REPLACE THE CHARACTER WITH AN 'S'.
c
      if (abs(round(rval)) .gt. 1.0) then
      fmt(locs:locs) = 's'
c
c THERE IS NOT, SHIFT THE REMAINDER OF THE FORMAT DOWN BY ONE CHARACTER.
c
      else
      lf = length(fmt) - 1
      do 2 i = locs, lf
      fmt(i:i) = fmt(i + 1:i + 1)
c
c PUT A SPACE AT THE END.
c
    2 continue
c
c FINALLY NEGATE LOCS SO THAT WE KNOW THAT THE FORMAT HAS BEEN SHIFTED
c IF IT IS RESTORED.
c
      fmt(i:i) = ' '
      locs = - locs
      end if
      end if
c
c
c THE NEXT ENTRY POINT IS USED TO RESTORE THE LAST FORMAT PROCESSED.
c
c
      return 
c
c TEST IF THERE IS AN F FORMAT DESCRIPTOR FIELD WIDTH TO RESET.
c
      entry rstrfm(fmt)
c
c THERE IS, CHANGE IT BACK TO WCHARS.
c
      if (locw .gt. 0) then
c
c CLEAR ITS LOCATION POINTER SO THAT IT IS NOT DONE AGAIN.
c
      fmt(locw:locw + 1) = wchars
      locw = 0
c
c TEST IF THERE IS A SURROGATE 'S' TO REPLACE.
c
      end if
c
c THERE IS, CHECK IF THE FORMAT WAS SHIFTED DOWN.
c
      if (locs .ne. 0) then
c
c IT WAS, SHIFT IT UP AGAIN.
c
      if (locs .lt. 0) then
      lf = length(fmt)
      locs = abs(locs)
      do 3 i = lf, locs, -1
      fmt(i + 1:i + 1) = fmt(i:i)
    3 continue
c
c REPLACE SCHAR INTO ITS PREVIOUS LOCATION.
c
      end if
c
c CLEAR ITS LOCATION POINTER SO THAT IT IS NOT DONE AGAIN.
c
      fmt(locs:locs) = schar
      locs = 0
      end if
c
c END OF SUBROUTINE SETRFM.
c
      return 
      end
