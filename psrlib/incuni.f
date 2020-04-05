cDECK INCUNI
c
c
c
c
c This routine parses an obey command.  If a command file is requested
c then the input unit pointer for getcmd is incremented (if allowed).
c Input arguments:
c  PARS     - The parameters from the command line.
c  NPAR     - The number of these, if NPAR = -1 it is assumed that
c             there is no command line to parse and that a new obey
c             file name is in PARS(1).
c  INBUF    - A character work space array capable of holding a file
c             name.
c  IUPTR    - The current file units array pointer.
c  IUNIT    - The file units array.
c  NUNITS   - The number of units available for use in IUNIT.
c  NRPT     - An array which holds the requested repeat counts for
c             each active file unit.
c  IRPTR    - The requested repeat count (NPAR = -1 only).
c Output arguments:
c  IUPTR    - Is incremented if a new obey file was opened.
c  NRPT     - The repeat count for the new obey file is loaded into
c             NRPT(IUPTR).
c  IFAIL    - Zero if successful, else > 0.
c     Version 2.1   23rd October, 1986
c
      subroutine incuni(pars, npar, inbuf, iuptr, iunit, nunits, nrpt, 
     &irptr, ifail)
      character pars(*)*(*), inbuf*(*), olist(4)*12, filtxt(2)*8, fmt*7
     &, routn*(*)
      logical fexi, comstr, parint
      dimension iunit(*), nrpt(2:*), ipfil(2)
      parameter (routn = 'INCUNI')
      save fmt, filtxt, olist
c
c Test the number of parameters.
c
c
c There is a command line to parse, set the default requests.
c
      data olist / 'COMMAND_FILE', 'SYMBOLS_FILE', 'REPEAT_COUNT', ' ' /
      data filtxt / 'Command', 'Symbols''' /
      data fmt / '(SS,I?)' /
      if (npar .ge. 0) then
      ipfil(1) = -1
      ipfil(2) = -1
c
c Parse the command line.
c
      irpt = 0
      ifail = 0
      ip = 2
c
c Check the current parameter.
c
 1000 if ((ip .le. npar) .and. (ifail .eq. 0)) then
      ii = intcmd(olist,pars(ip))
c
c It is a recognized parameter, case it.
c
      if (ii .gt. 0) then
c
c Command_file or symbols_file, obtain any following value.
c
      goto (2000, 2000, 2001), ii
c
c Test if successful.
c
 2000 call getidc(pars, npar, ip, ifail)
c
c Yes, check the file name.
c
      if (ifail .eq. 0) then
c
c Test if successful.
c
      call chkfil(pars, ip, inbuf, i, ifail)
c
c Yes, but trap for default.
c
      if (ifail .eq. 0) then
c
c The default file name has been requested, flag an error.
c
      if (inbuf .eq. ' ') then
      call liberr(routn, 29, 0, 0, 0.0, filtxt(ii), ifail)
c
c The file name is valid, note its position.
c
      else
      ipfil(ii) = ip
      end if
      end if
c
c No file name was supplied, flag an error.
c
      else if (ifail .lt. 0) then
      call liberr(routn, 29, 1, 0, 0.0, filtxt(ii), ifail)
      end if
c
c Repeat_count, attempt to obtain any following value.
c
      goto 2777
c
c Test if successful.
c
 2001 call getidc(pars, npar, ip, ifail)
c
c Yes, check the value set.
c
      if (ifail .eq. 0) then
c
c It is, check that it is valid.
c
      if (parint(pars(ip),irpt)) then
c
c It is not, flag an error.
c
      if (irpt .le. 0) then
      call liberr(routn, 51, 2, 0, 0.0, pars(ip), ifail)
      pars(1) = pars(ip)
      end if
c
c It is not an integer, flag an error.
c
      else
      call liberr(routn, 51, 3, 0, 0.0, pars(ip), ifail)
      pars(1) = pars(ip)
      end if
c
c No repeat count was supplied, use a default value of 1.
c
      else if (ifail .le. 0) then
      irpt = 1
      ifail = 0
      end if
c
c Unrecognized parameter, attempt to treat it as a command file name.
c
      else if (ii .eq. 0) then
c
c Test if successful.
c
      call chkfil(pars, ip, inbuf, i, ifail)
c
c Yes, but trap for default.
c
      if (ifail .eq. 0) then
c
c The default file name has been requested, flag an error.
c
      if (inbuf .eq. ' ') then
      call liberr(routn, 29, 4, 0, 0.0, filtxt(1), ifail)
c
c The file name is valid, note its position.
c
      else
      ipfil(1) = ip
      end if
      end if
c
c Ambigous option (this cannot happen at the moment).
c
      else
      call liberr(routn, 37, 5, 0, 0.0, pars(ip), ifail)
      pars(1) = pars(ip)
c
c Continue with the parse.
c
      end if
 2777 ip = ip + 1
      goto 1000
      end if
c
c There is no command line to parse, just test the command file name in
c pars(1).
c
      else
c
c Test if successful.
c
      call chkfil(pars, 1, inbuf, i, ifail)
c
c Yes, check the supplied repeat count.
c
      if (ifail .eq. 0) then
c
c It is invalid, flag an error.
c
      if (irptr .le. 0) then
      call setifm(irptr, fmt)
      write(unit=inbuf, fmt=fmt) irptr
      call rstifm(fmt)
      call liberr(routn, 51, 6, 0, 0.0, inbuf, ifail)
c
c It is valid, set the file positions and repeat count.
c
      else
      ipfil(1) = 1
      ipfil(2) = -1
      irpt = irptr
      end if
      end if
c
c Was the parse successful?
c
      end if
c
c Yes, but check that a file name has been supplied.
c
      if (ifail .eq. 0) then
c
c One at least has, ensure that a repeat count has not been specified
c with a symbols' file only.
c
      if ((ipfil(1) .gt. 0) .or. (ipfil(2) .gt. 0)) then
c
c One has, flag an error.
c
      if ((irptr .gt. 0) .and. (ipfil(1) .le. 0)) then
      call liberr(routn, 81, 7, 0, 0.0, ' ', ifail)
c
c One has not, check that there is a free unit left in iunit.
c
      else
c
c No more units available, flag an error.
c
      if (iuptr .ge. nunits) then
      call liberr(routn, 28, 8, nunits - 1, 0.0, ' ', ifail)
c
c A unit is available, is a symbols' file to be loaded?
c
      else
c
c Yes, do so.
c
      if (ipfil(2) .gt. 0) then
      call symlod(iunit(iuptr + 1), pars(ipfil(2)), ifail)
c
c Test if successful so far.
c
      end if
c
c Yes, is a command file to be opened?
c
      if (ifail .eq. 0) then
c
c One is, check that it exists.
c
      if (ipfil(1) .gt. 0) then
      inquire(file=inbuf, exist=fexi, iostat=i) 
c
c It does, try to open it.
c
      if (fexi .and. (i .eq. 0)) then
c
c Test if successful.
c
      call readop(iunit(iuptr + 1), inbuf, 0, i)
c
c Yes, increment the unit pointer.
c
      if (i .eq. 0) then
c
c Rewind it to make sure that we start at the right place.
c
      iuptr = iuptr + 1
c
c Set the repeat count for the file.
c
      rewind(unit=iunit(iuptr)) 
      nrpt(iuptr) = max(1,irpt)
c
c Unable to open the file, flag an error.
c
      else
      call liberr(routn, 30, 9, i, 0.0, inbuf, ifail)
      if (npar .ge. 1) pars(1) = pars(ipfil(1))
      end if
c
c The specified file does not exist.
c
      else
      call liberr(routn, 31, 10, 0, 0.0, pars(ipfil(1)), ifail)
      if (npar .ge. 1) pars(1) = pars(ipfil(1))
      end if
      end if
      end if
      end if
      end if
c
c No file name has been supplied, flag an error.
c
      else
      call liberr(routn, 29, 11, 0, 0.0, filtxt(1), ifail)
      end if
      end if
c
c End of subroutine incuni.
c
      return 
      end
