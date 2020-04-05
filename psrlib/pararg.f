c***********************************************************************
c***********************************************************************
c returns the values of the arguments in dummy and has the value .true.
c if conversion fails then returns .false. with
c ifail=1 if a bad conversion took place 
c ifail=-1 if there are no arguments
      logical function pararg(jinp, ninp, cmdinp, narg, dummy, ifail)
      character cmdinp(*)*(*), sep
      parameter (sep = '%')
      real dummy(10)
c look for a command separator
      logical pareal
      if (cmdinp(jinp) .eq. sep) then
      ifail = -1
      jinp = jinp + 1
      else if ((ninp - jinp) .ge. (narg - 1)) then
      ifail = 0
      j = 1
      do while ((ifail .eq. 0) .and. (j .le. narg))
      if (pareal(cmdinp(jinp),dummy(j)) .and. (jinp .le. ninp)) then
      jinp = jinp + 1
      j = j + 1
      else
      ifail = 1
      end if
      end do
      else if (jinp .gt. ninp) then
      ifail = -1
      end if
      pararg = .true.
      if (ifail .ne. 0) pararg = .false.
      if (ifail .eq. 1) call outmon(' Bad Parameters')
      end
