cDECK SETLIN
c
c
c
c
c SET UP AN EXISTING OUTPUT CHANNEL OR CREATE A NEW ONE.
c
c FIRST SEARCH FOR AN EXISTING CHANNEL.
c
      subroutine setlin(iunit, istat, lenpag, lenlin, ifail)
c
c IF NONE AVAILABLE SEARCH FOR A FREE CHANNEL.
c
      call outser(ichan, iunit)
      if (ichan .eq. 0) then
      call outfre(ichan)
c
c NO FREE CHANNELS.
c
      end if
      if (ichan .le. 0) then
      call liberr('SETLIN', 32, 0, abs(ichan), 0.0, ' ', ifail)
      return 
c
c SET STATUS AND PARAMETERS AND RETURN.
c
      end if
      call outsta(ichan, istat)
      call outset(ichan, iunit, lenpag, lenlin)
      ifail = 0
c
c END OF SUBROUTINE SETLIN.
c
      return 
      end
