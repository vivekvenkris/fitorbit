cDECK SYMOPN
c
c
c
c
c This routine opens a symbols file.
c Input arguments:
c  IUNIT    - The unit number to use for the file.
c  FNAME    - The file name.
c  IFLAG    - The type of open required:
c              -1 = Open read only
c               3 = Open a new file for writing.
c Output argument:
c  IFAIL    - Zero if the file was opened successfully, 1 if the file
c             does not exist (iflag=-1 only), 2 if the file could not
c             be opened.
c
c     Version 1.2   20th November, 1986   Alliant FX-FORTRAN
c
c Declare the routine's arguments.
c
      subroutine symopn(iunit, fname, iflag, ifail)
      integer iunit, iflag, ifail
c
c Declare local variables.
c
      character fname*(*)
c
c Check the type of open required.
c
      logical fexi
c
c Open for write.
c
      if (iflag .eq. 3) then
      open(unit=iunit, file=fname, status='NEW', form='UNFORMATTED', 
     &iostat=ifail) 
c
c Open for read only.
c
      else
      open(unit=iunit, file=fname, status='OLD', form='UNFORMATTED', 
     &iostat=ifail) 
c
c Test if successful.
c
      end if
c
c No, does the file exist?
c
      if (ifail .ne. 0) then
      fexi = .true.
      inquire(file=fname, exist=fexi, iostat=ifail) 
c
c File not found.
c
      if ((iflag .eq. (-1)) .and. ((ifail .ne. 0) .or. (.not. fexi))) 
     &then
      ifail = 2
c
c Some other open error.
c
      else
      ifail = 1
      end if
c
c End of subroutine SYMOPN.
c
      end if
      end
