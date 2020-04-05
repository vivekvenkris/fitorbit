c
c Writes out NL elements of the buffer array LINE to all active output
c channels.  It deals with continuation lines which may be hyphenated
c if necessary and pagination.
c NOTE: The normal overprinting to the standard output has been replaced
c by a call to a special FX/Fortran routine, OVERPRINT, to avoid a bug.
c
c Arguments:
c  LINE    Input/Output  CHARACTER(*)*(*)  The array to print.  One line
c                                          per element.
c  NL      Input         INTEGER           The number of lines in LINE
c                                          to print.
c
c     Version 2.4   9th November, 1987
c
c Declare the routine's arguments.
c
      subroutine outlin(line, nl)
      character line(*)*(*)
c
c Declare external references.
c
      integer nl, ichan1, ichan2, ichan3, ichan4, ichan5, iunit, istat, 
     &lenpag, lenlin, numpag, numlin
c
c The following parameters set the maximum number of output channels,
c how close to the end of a page a new page is forced and how many
c spaces are allowed at the end of a line before a word is hyphenated.
c
      integer length
      integer maxchn, iendpg, iwordl
c
c The array IOPVAL contains the following values for each channel:-
c     IOPVAL(1,i)  status (-1 = not set up, 0 = disabled, +1 = enabled)
c     IOPVAL(2,i)  unit number (-1 for default unit)
c     IOPVAL(3,i)  maximum page length (-1 for no pagination)
c     IOPVAL(4,i)  maximum line length
c     IOPVAL(5,i)  page number
c     IOPVAL(6,i)  line number
c
c Declare local variables.
c
      parameter (maxchn = 3, iendpg = 5, iwordl = 10)
      character fmt*9
      logical hyphen, pad
      integer iopval(6, maxchn), nchan, i, nl1, isave, nsave, ichan, il
     &, nchr, ipado, n1, n2
c
c Set up default unit.
c
      save nchan, iopval
c
c If the number of lines is greater than zero, write to all active
c units.
c
C PAH commented out next line overlapping data initializations
c      data iopval(1, 1) / -1 /
      data iopval(1, 2) / -1 /
      data iopval(1, 3) / -1 /
      data iopval(1, 1) / 1 /
      data iopval(2, 1) / -1 /
      data iopval(3, 1) / -1 /
      data iopval(4, 1) / 81 /
      data iopval(5, 1) / 0 /
      data iopval(6, 1) / -1 /
      data nchan / 1 /
      if (nl .gt. 0) then
      nl1 = nl
c
c Otherwise activate the default unit.
c
      else
      isave = iopval(1,1)
      iopval(1,1) = 1
      if (nl .lt. 0) then
c
c If NL equals zero disable all other units and allow only one line.
c
      nl1 = abs(nl)
      else
      nsave = nchan
      nchan = 1
      nl1 = 1
      end if
c
c Output to all active channels.
c
      end if
      do 3 ichan = 1, nchan
c
c Force a new page if NL lines within IENDPG of end of page.
c
      if (iopval(1,ichan) .eq. 1) then
      if (((nl1 + iendpg) + iopval(6,ichan)) .gt. iopval(3,ichan)) then
      iopval(6,ichan) = iopval(3,ichan)
c
c Print out the NL lines.  First  evaluate the number of characters in
c a line.
c
      end if
      do 1 il = 1, nl1
c
c Set the format.
c
      nchr = max(1,length(line(il)))
c
c Is the line to be overprinted?
c
      fmt = '(150A)'
c
c Set the padding offset.
c
      pad = line(il)(1:1) .eq. '+'
c
c Force a new page if the carriage control character is a '1'.
c
      ipado = 0
      if (line(il)(1:1) .eq. '1') then
      iopval(6,ichan) = iopval(3,ichan)
c
c Update the line counter to reflect other carriage control character.
c
      line(il)(1:1) = ' '
      else if (pad) then
      iopval(6,ichan) = iopval(6,ichan) - 1
      else if (line(il)(1:1) .eq. '0') then
      iopval(6,ichan) = iopval(6,ichan) + 1
c
c Start and finish character positions.
c
      end if
      n2 = 0
 1000 hyphen = .false.
      n1 = n2 + 1
      n2 = n2 + iopval(4,ichan)
c
c No continuations needed.
c
      if (n2 .ge. nchr) then
      n2 = nchr
c
c Continuations required. Try to find a space near end of line.
c
      else
      do 2 i = n2, n2 - iwordl, -1
      if (line(il)(i:i) .eq. ' ') then
      goto 1001
      end if
c
c Must hyphenate.
c
    2 continue
      i = n2 - 2
      hyphen = .true.
 1001 n2 = i
c
c Increment the line count and get a new page if needed.
c
      end if
      iopval(6,ichan) = iopval(6,ichan) + 1
      if ((iopval(3,ichan) .ne. (-1)) .and. (iopval(6,ichan) .ge. iopval
     &(3,ichan))) then
      call pagest(iopval(1,ichan))
c
c Print the line.
c
      end if
      if (hyphen) then
c
c Write to the default unit.
c
      if (iopval(2,ichan) .eq. (-1)) then
c
c Pad out the line with trailing spaces.
c
c                 WRITE (*, FMT) LINE(IL)(N1:N2), ' -',
c    &                         (' ', I = 4 + IPADO,
c    &                                   IOPVAL(4,ICHAN) - N2 + N1)
      if (pad) then
      call overprint(line(il)(n1 + 1:n2), (((iopval(4,ichan) - n2) + n1)
     & - 3) - ipado)
c
c Do not pad out.
c
      else
      write(unit=*, fmt=fmt) line(il)(n1:n2), ' -'
      end if
c
c Write to the specified unit.
c
      else
c
c Pad out the line with trailing spaces.
c
      if (pad) then
      write(unit=iopval(2,ichan), fmt=fmt) line(il)(n1:n2), ' -', (' ',i
     & = 4 + ipado, (iopval(4,ichan) - n2) + n1)
c
c Do not pad out.
c
      else
      write(unit=iopval(2,ichan), fmt=fmt) line(il)(n1:n2), ' -'
      end if
      end if
      hyphen = .false.
      else
c
c Write to the default unit.
c
      if (iopval(2,ichan) .eq. (-1)) then
c
c Pad out the line with trailing spaces.
c
c                 WRITE(*,FMT) LINE(IL)(N1:N2),
c    &                         (' ', I = 2 + IPADO,
c    &                                   IOPVAL(4,ICHAN) - N2 + N1)
      if (pad) then
      call overprint(line(il)(n1 + 1:n2), (((iopval(4,ichan) - n2) + n1)
     & - 1) - ipado)
c
c Do not pad out the line.
c
      else
      write(unit=*, fmt=fmt) line(il)(n1:n2)
      end if
c
c Write to the specified unit.
c
      else
c
c Pad out the line with trailing spaces.
c
      if (pad) then
      write(unit=iopval(2,ichan), fmt=fmt) line(il)(n1:n2), (' ',i = 2
     & + ipado, (iopval(4,ichan) - n2) + n1)
c
c Do not pad out the line.
c
      else
      write(unit=iopval(2,ichan), fmt=fmt) line(il)(n1:n2)
      end if
      end if
c
c Carry on if there are any continuation lines.
c
      end if
      if (n2 .ne. nchr) then
      fmt = '(1X,150A)'
      ipado = 1
      goto 1000
      end if
    1 continue
      end if
c
c Restore status of all units.
c
    3 continue
      if (nl .le. 0) then
      iopval(1,1) = isave
      if (nl .eq. 0) then
      nchan = nsave
      end if
      end if
c
c End of subroutine OUTLIN.
c
c The following entry point is used to force a new page the next time
c OUTLIN or NEWLIN is called.
c
      return 
c
c Set the line counters.
c
      entry newpag()
      do 10 ichan = 1, nchan
      iopval(6,ichan) = iopval(3,ichan)
   10 continue
c
c End of subroutine NEWPAG.
c
c The following entry point is used to print nl new lines.
c
      return 
c
c If NL > 0 write to all active units.
c
      entry newlin(nl)
      if (nl .gt. 0) then
c
c Otherwise activate the default unit.
c
      nl1 = nl
      else
      isave = iopval(1,1)
      iopval(1,1) = 1
      if (nl .lt. 0) then
c
c If NL = 0 disable all other units and allow only one line.
c
      nl1 = abs(nl)
      else
      nsave = nchan
      nchan = 1
      nl1 = 1
      end if
c
c Output to all active channels.
c
      end if
      do 20 ichan = 1, nchan
c
c Start a new page if one has been previously demanded.
c
      if (iopval(1,ichan) .eq. 1) then
      if ((iopval(3,ichan) .ne. (-1)) .and. (iopval(6,ichan) .eq. iopval
     &(3,ichan))) then
      call pagest(iopval(1,ichan))
c
c Force a new page if NL lines within IENDPG of end of page.
c
      end if
      if ((iopval(3,ichan) .ne. (-1)) .and. (((nl1 + iendpg) + iopval(6,
     &ichan)) .gt. iopval(3,ichan))) then
c
c Otherwise output new lines.
c
      iopval(6,ichan) = iopval(3,ichan)
      else
c
c Write to the default unit.
c
      if (iopval(2,ichan) .eq. (-1)) then
      do 21 i = 1, nl1
   21 write(unit=*, fmt=100) 
c
c Write to the specified unit.
c
      else
      do 22 i = 1, nl1
      write(unit=iopval(2,ichan), fmt=100) 
   22 continue
c
c Increment the line counter.
c
      end if
      iopval(6,ichan) = iopval(6,ichan) + nl1
      end if
      end if
c
c Restore status of all units.
c
   20 continue
      if (nl .le. 0) then
      iopval(1,1) = isave
      if (nl .eq. 0) then
      nchan = nsave
      end if
      end if
c
c Format.
c
      return 
c
c End of subroutine NEWLIN.
c
c The following entry point searches for the specified unit.
c
  100 format()
c
c Arguments:
c  ICHAN1  Output  INTEGER  The output channel associated with the unit,
c                           zero if none found.
c  IUNIT   Input   INTEGER  The unit number.
c
c Trap for out of range unit number.
c
      entry outser(ichan1, iunit)
c
c Search all set up channels.
c
      iunit1 = max(iunit,-1)
      do 30 i = 1, nchan
      if ((iopval(1,i) .ne. (-1)) .and. (iopval(2,i) .eq. iunit1)) then
      ichan1 = i
      goto 7777
      end if
c
c Unit not set up.
c
   30 continue
      ichan1 = 0
c
c End of subroutine OUTSER.
c
c The following entry point searches for a free channel.
c
 7777 return 
c
c Argument:
c  ICHAN2  Output  INTEGER  A free channel number, -ve if none found.
c
c Search all channels.
c
      entry outfre(ichan2)
      do 40 i = 1, maxchn
      if (iopval(1,i) .eq. (-1)) then
      goto 7776
      end if
c
c No free channels.
c
   40 continue
      ichan2 = - maxchn
c
c Found a free channel.
c
      return 
 7776 ichan2 = i
      nchan = max(nchan,ichan2)
c
c End of subroutine OUTFRE.
c
c The following entry point sets the status of a particular channel.
c
      return 
c
c Arguments:
c  ICHAN3  Input  INTEGER  The channel number.
c  ISTAT   Input  INTEGER  The status for the channel:
c                            -1 = free
c                             0 = disabled
c                             1 = enabled
c
c Set the status, note that we cannot free the default unit.
c
      entry outsta(ichan3, istat)
      if (ichan3 .eq. 1) then
      iopval(1,ichan3) = min(1,max(0,istat))
      else
      iopval(1,ichan3) = min(1,max(-1,istat))
      end if
c
c End of subroutine OUTSTA.
c
c The following entry point sets the parameters for a channel.
c
      return 
c
c Arguments:
c  ICHAN4  Input  INTEGER  The channel number.
c  IUNIT   Input  INTEGER  The unit number for the channel (-1 for
c                          default)
c  LENPAG  Input  INTEGER  The page length for the channel (-1 for
c                          unpaginated)
c  LENLIN  Input  INTEGER  The line length for the channel.
c
c Set the parameters.
c
      entry outset(ichan4, iunit, lenpag, lenlin)
      iopval(2,ichan4) = max(iunit,-1)
      iopval(3,ichan4) = max(lenpag,-1)
      iopval(4,ichan4) = min(150,max(lenlin,80))
      iopval(5,ichan4) = 0
      iopval(6,ichan4) = iopval(3,ichan4)
c
c End of subroutine OUTSET.
c
c The final entry point returns the parameters for a channel.
c
      return 
c
c Arguments:
c  ICHAN1  Input   INTEGER  The channel number.
c  ISTAT   Output  INTEGER  The channel status:
c                             -1 = free
c                              0 = disabled
c                              1 = enabled
c                            
c  IUNIT   Output  INTEGER  The unit number (-1 = default).
c  LENPAG  Output  INTEGER  The page length (-1 = no pagination).
c  LENLIN  Output  INTEGER  The maximum line length.
c  NUMPAG  Output  INTEGER  The current page number.
c  NUMLIN  Output  INTEGER  The current line number.
c
c Return the parameters.
c
      entry outget(ichan5, istat, iunit, lenpag, lenlin, numpag, numlin)
      istat = iopval(1,ichan5)
      iunit = iopval(2,ichan5)
      lenpag = iopval(3,ichan5)
      lenlin = iopval(4,ichan5)
      numpag = iopval(5,ichan5)
      numlin = iopval(6,ichan5)
c
c End of subroutine OUTGET.
c
      return 
      end
