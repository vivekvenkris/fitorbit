c
c Writes out a new page header and resets the counters for OUTLIN.
c
c Argument:
c  IOPVAL  Input/Output  INTEGER  The channel status array (see OUTLIN).
c
c     Version 2.0   11th March, 1982
c
c Declare the routine's arguments.
c
      subroutine pagest(iopval)
      integer iopval(*)
c
c Declare external references.
c
      character headr1*(*), subhd1*(*)
      integer length
c
c Declare local variables.
c
      character ctimeh*8, cdate*8
      character ctmp*8, ctmp1*8
      character headr*50, subhd*70, fmt*38
      integer lh, is, ipnp, iphd, idate, itime
c
c Initialise the page heading and sub-heading.
c
      save subhd, headr
c
c Obtain the effective length of the 'HEADR' string.
c
      data headr / ' ' /
      data subhd / ' ' /
      lh = max(1,length(headr))
      do 1 is = 1, lh
      if (headr(is:is) .ne. ' ') goto 1000
    1 continue
c
c Calculate the position for the page number.
c
      is = 1
c
c Now evaluate the position for the title.
c
 1000 ipnp = iopval(4) - 8
c
c Obtain the date and time.
c
      iphd = ((iopval(4) - lh) + is) / 2
      call oldate(idate)
c
c Adjust the tab positions in the format statement.
c
      call oltime(itime)
c
c Increment the page counter.
c
      write(unit=fmt, fmt=800) iphd, ipnp
c
c Now print the page header.
c
      iopval(5) = mod(iopval(5) + 1,999)
c
c Write to the default unit.
c
         ctmp=cdate(idate)
         ctmp1=ctimeh(itime)
      if (iopval(2) .lt. 0) then
      write(unit=*, fmt=fmt) ctmp, ctmp1, headr(is:lh), 
     &iopval(5)
c
c Write to the specified unit.
c
      else
      write(unit=iopval(2), fmt=fmt) ctmp, ctmp1, headr(
     &is:lh), iopval(5)
c
c Is there a sub-title to print?
c
      end if
c
c Yes, print it and set the line count.
c
      if (subhd .ne. ' ') then
c
c Write to the default unit.
c
      if (iopval(2) .lt. 0) then
      write(unit=*, fmt=200) subhd
c
c Write to the specified unit.
c
      else
      write(unit=iopval(2), fmt=200) subhd
      end if
      iopval(6) = 4
c
c No, just print two blank lines.
c
      else
c
c Write to the default unit.
c
      if (iopval(2) .lt. 0) then
      write(unit=*, fmt=201) 
c
c Write to the specified unit.
c
      else
      write(unit=iopval(2), fmt=201) 
      end if
      iopval(6) = 3
      end if
c
c Format statements.
c
      return 
  200 format(1x,a//)
  201 format(/)
c
c End of subroutine PAGEST.
c
c The following entry point is used to set a title.
c
  800 format(ss,13h('1',A,2X,A,T,i3,4h,A,T,i3,15h,SS,'Page ',I3))
c
c Argument:
c  HEADR1  Input  CHARACTER*(*)  The new page title.
c
c Set the title.
c
      entry setttl(headr1)
      headr = headr1
c
c End of subroutine SETTTL.
c
c The following entry point is used to return the current title.
c
      return 
c
c Argument:
c  HEADR1  Output  CHARACTER*(*)  The current page title.
c
c Get the title.
c
      entry getttl(headr1)
      headr1 = headr
c
c End of subroutine GETTTL.
c
c The following entry point is used to set a sub-title.
c
      return 
c
c Argument:
c  SUBHD1  Input  CHARACTER*(*)  The new page sub-title.
c
c Set the sub-title.
c
      entry setstl(subhd1)
      subhd = subhd1
c
c End of subroutine SETSTL.
c
c The final entry point is used to return the current sub-title.
c
      return 
c
c Argument:
c  SUBHD1  Output  CHARACTER*(*)  The current page sub-title.
c
c Get the sub-title.
c
      entry getstl(subhd1)
      subhd1 = subhd
c
c End of subroutine GETSTL.
c
      return 
      end
