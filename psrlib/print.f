cDECK PRINT
c
c
c
c
c This routine is used to print the contents of the file on IUNIT, and
c to delete and close the file.
c Input argument:
c  IUNIT   - The unit number on which the file is open.
c     Version 1.4   2nd June, 1987   Alliant FX/FORTRAN
c
c Declare the routine's argument.
c
      subroutine print(iunit)
c
c Declare local variables.
c
      integer iunit
      integer istat
c
c Obtain the file's name.
c
      character file*256
c
c Test if successful.
c
      inquire(unit=iunit, name=file, iostat=istat) 
c
c Yes, print the file remotely via a shell command.
c
      if (istat .eq. 0) then
      call giveos(('fpr < ' // file) // ' | rprint', istat)
c
c Close and delete the file.
c
      end if
c
c End of subroutine PRINT.
c
      close(unit=iunit, status='DELETE', iostat=istat) 
      end
