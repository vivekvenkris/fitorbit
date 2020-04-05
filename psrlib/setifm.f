cDECK SETIFM
c
c
c
c
c THIS ROUTINE ADJUSTS THE FORMAT STATEMENT IN FMT READY FOR USE IN
c PRINTING THE INTEGER IVAL.  THE FOLLOWING OPERATIONS ARE CARRIED OUT:
c 1.   THE FIRST OCCURRENCE, IF ANY, OF THE STRING 'I?' IS REPLACED WITH
c      'IN', WHERE N IS THE EXACT FIELD WIDTH REQUIRED TO PRINT IVAL;
c 2.   THE FIRST OCCURRENCE, IF ANY, OF THE STRING '!' IS REPLACED WITH
c      'S' IF IVAL IS ZERO OR GREATER THAN ONE, OR IS REMOVED ENTIRELY
c      IF IVAL IS EQUAL TO ONE.
c THIS ROUTINE HAS A SECOND ENTRY POINT (RSTIFM) WHICH CAN BE USED TO
c RESTORE THE FORMAT TO ITS ORIGINAL STATE IF IT IS CALLED BEFORE SETIFM
c IS USED AGAIN.
c     VERSION 1.0   22AUG82
c
      subroutine setifm(ival, fmt)
c
c WCHAR IS THE CHARACTER MARKING THE 'VARIABLE' FIELD WIDTH;
c SCHAR IS THE CHARACTER MARKING THE POSSIBLE 'S' LOCATION.
c
      character fmt*(*), wchar*(*), schar*(*), isize*1
c
c SAVE THE LOCATIONS OF WCHAR AND SCHAR IN THE LAST FORMAT PROCESSED SO
c THAT IT CAN BE RESTORED.
c
      parameter (wchar = '?', schar = '!')
      save locs, locw
c
c OBTAIN THE LOCATION OF THE I FIELD DESCRIPTOR TO ADJUST.
c
c
c IS THERE ONE?
c
      data locw / 0 /
      data locs / 0 /
      locw = index(fmt,'I' // wchar)
c
c YES, POINT TO THE CHARACTER TO REPLACE.
c
      if (locw .gt. 0) then
c
c NOW REPLACE IT WITH THE CORRECT FIELD WIDTH.
c
      locw = locw + 1
      fmt(locw:locw) = isize(ival)
c
c OBTAIN THE LOATION OF THE SURROGATE 'S'.
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
      if (abs(ival) .ne. 1) then
      fmt(locs:locs) = 's'
c
c THERE IS NOT, SHIFT THE REMAINDER OF THE FORMAT DOWN BY ONE CHARACTER.
c
      else
      lf = length(fmt) - 1
      do 1 i = locs, lf
c
c PUT A SPACE AT THE END.
c
    1 fmt(i:i) = fmt(i + 1:i + 1)
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
c TEST IF THERE IS AN I FORMAT DESCRIPTOR FIELD WIDTH TO RESET.
c
      entry rstifm(fmt)
c
c THERE IS, CHANGE IT BACK TO WCHAR.
c
      if (locw .gt. 0) then
c
c CLEAR ITS LOCATION POINTER SO THAT IT IS NOT DONE AGAIN.
c
      fmt(locw:locw) = wchar
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
      do 2 i = lf, locs, -1
    2 fmt(i + 1:i + 1) = fmt(i:i)
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
c END OF SUBROUTINE SETIFM.
c
      return 
      end
