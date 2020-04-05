cDECK READOP
c
c
c
c
c This routine opens the file 'FILE' on unit IUNIT for reading only.
c Input arguments:
c  IUNIT    - The unit number on which to open the file.
c  FILE     - The name of the file.
c  IOPT     - The sort of open required:
c                0 = formatted
c              <>0 = unformatted
c Output argument:
c  IFAIL    - The FORTRAN status return from the open.  Zero if
c             successful.
c
c     Version 1.3   20th November, 1986   Alliant FX-FORTRAN
c
c Declare the routine's arguments.
c
      subroutine readop(iunit, file, iopt, ifail)
      integer iunit, iopt, ifail
c
c Declare external references.
c
      character file*(*)
c
c Test the sort of open wanted.
c
      integer length
c
c Open the file for formatted read.
c
      if (iopt .eq. 0) then
      open(unit=iunit, file=file(1:max(1,length(file))), status='OLD', 
     &blank='NULL', iostat=ifail) 
c
c Open the file for unformatted read.
c
      else
      open(unit=iunit, file=file(1:max(1,length(file))), status='OLD', 
     &form='UNFORMATTED', iostat=ifail) 
c
c End of subroutine READOP.
c
      end if
      end
