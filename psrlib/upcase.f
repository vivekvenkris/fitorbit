      subroutine upcase(pars, is, if)
      integer is, if
c
c Declare external references.
c
      character pars(*)*(*)
c
c Declare local variables.
c
      integer length
c
c Work out the difference between 'a' and 'A'.
c
      integer diff, i, j
c$$$c
c$$$c Convert the parameters one at a time.
c$$$c
c$$$      diff = ichar('A') - ichar('a')
c$$$c
c$$$c Convert each character in turn.
c$$$c
c$$$      do 1 i = is, if
c$$$      do 2 j = 1, length(pars(i))
c$$$      if ((pars(i)(j:j) .ge. 'a') .and. (pars(i)(j:j) .le. 'z')) then
c$$$      pars(i)(j:j) = char(ichar(pars(i)(j:j)) + diff)
c$$$      end if
c$$$    2 continue
c$$$c
c End of subroutine UPCASE.
c
    1 continue
      end
