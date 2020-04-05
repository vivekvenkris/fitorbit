cDECK SYMCMD
c
c
c
c
c This routine handles the symbols command.
c Input arguments:
c  PARS     - The parameters from the command line.
c  NPAR     - The number of these.
c Output arguments:
c  IFAIL    - Zero if successful, else > 0.
c     Version 1.2   8th July, 1988
c
      subroutine symcmd(pars, npar, ifail)
      character pars(*)*(*), delete*(*), all*(*), routn*(*), litdel*1, 
     &schar*1, lchar*1, supchr*1
      parameter (delete = 'DELETE', all = 'ALL', routn = 'SYMCMD')
c
c Obtain the suppress substitution and literal delimiter characters.
c
      logical delflg, comstr
      schar = supchr()
c
c Set the delete mode flag to off.
c
      lchar = litdel()
c
c Start to parse the command line.
c
      delflg = .false.
      ip = 2
      ifail = 0
c
c Check if the current parameter starts with the symbol suppression
c character (symbols are never substituted in this command).  If so
c set the starting character in the parameter to 2 in case it is a
c symbol name.
c
 1000 if ((ip .le. npar) .and. (ifail .eq. 0)) then
      if ((pars(ip)(1:1) .eq. schar) .and. (length(pars(ip)) .gt. 1)) 
     &then
      ifc = 2
      else
      ifc = 1
c
c Check if the current parameter is followed by an '='.
c
      end if
      ips = ip
      call getidc(pars, npar, ip, ifail)
c
c It is, treat it as a symbol name.  Is the next parameter the literal
c delimiter?
c
      if (ifail .eq. 0) then
c
c It is, parse this construct.
c
      if (pars(ip) .eq. lchar) then
      call litprs(pars, npar, ip, is, if, ifail)
c
c The next parameter is not a literal delimiter, treat it as a single
c parameter value.
c
      else
      is = ip
      if = ip
c
c Test if successful.
c
      end if
c
c Yes, is the symbol value blank?
c
      if (ifail .eq. 0) then
c
c It is, add it to the symbols' list.
c
      if (is .gt. if) then
      call symadd(pars(ips)(ifc:), ' ', 1, ifail)
c
c It is not blank, add it to the symbols' list.
c
      else
      call symadd(pars(ips)(ifc:), pars(is), (if - is) + 1, ifail)
c
c Cancel the delete mode flag in case it is set.
c
      end if
      delflg = .false.
      end if
c
c There is no '=' following the parameter, are we in delete mode?
c
      else if (ifail .lt. 0) then
      ifail = 0
c
c We are, attempt to delete the symbol defined by pars(ip).  First check
c if the 'symbol' might be 'all'.
c
      if (delflg) then
c
c It might be, this is handled as follows:  an attempt is made to delete
c the symbol as specified, if this fails because the symbol does not
c exist then all symbols are deleted.  First turn off error 74 (symbol
c not found) after noting its current value.
c
      if (comstr(all,pars(ip))) then
      call getler(74, i)
c
c Now attempt to delete the symbol.
c
      call setler(74, 2)
c
c Restore the previous status of error 74.
c
      call symdel(pars(ip), ifail)
c
c Did the delete fail with error 74?
c
      call setler(74, 3 - i)
c
c Yes, clear all symbols instead.
c
      if (ifail .eq. 74) then
      call symclr
      ifail = 0
      end if
c
c The symbol cannot be confused with 'all', just attempt to delete it.
c
      else
      call symdel(pars(ip)(ifc:), ifail)
      end if
c
c We are not in delete mode, is the current parameter an attempt to set
c this?
c
      else
      delflg = comstr(delete,pars(ip))
c
c No, flag unrecognized option.
c
      if (.not. delflg) then
      call liberr(routn, 26, 0, 0, 0.0, pars(ip), ifail)
      pars(1) = pars(ip)
      end if
      end if
c
c Continue with the parse.
c
      end if
      ip = ip + 1
      goto 1000
      end if
c
c End of subroutine symcmd.
c
      return 
      end
