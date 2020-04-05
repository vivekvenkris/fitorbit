cDECK LOCASE
c
c
c
c
c This routine converts the character array PARS into lower case.
c Input arguments:
c  PARS     - The array to be converted.
c  IS       - The first element to convert.
c  IF       - The last element to convert.
c Output argument:
c  PARS     - The converted array.
c     Version 1.0   13th February, 1987   Generic
c
c Declare the routine's arguments.
c
      subroutine locase(pars, is, if)
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
c
c Convert the parameters one at a time.
c
      diff = ichar('A') - ichar('a')
c
c Convert each character in turn.
c
      do 1 i = is, if
      do 2 j = 1, length(pars(i))
      if ((pars(i)(j:j) .ge. 'A') .and. (pars(i)(j:j) .le. 'Z')) then
      pars(i)(j:j) = char(ichar(pars(i)(j:j)) - diff)
      end if
    2 continue
c
c End of subroutine LOCASE.
c
    1 continue
      end
