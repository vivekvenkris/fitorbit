      subroutine upcase_works(par)
      integer is, if
c
c Declare external references.
c
      character*(*) par
c
c Declare local variables.
c
      integer length
c
c Work out the difference between 'a' and 'A'.
c
      integer diff, i, j
c
c Convert the parameters one at a time.
c
      diff = ichar('A') - ichar('a')
c
c Convert each character in turn.
c
      do 2 j = 1, length(par)
      if ((par(j:j) .ge. 'a') .and. (par(j:j) .le. 'z')) then
      par(j:j) = char(ichar(par(j:j)) + diff)
      end if
    2 continue
c
c End of subroutine UPCASE_works.
c
      end
