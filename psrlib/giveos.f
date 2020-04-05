c
c This routine passes the command string CMD to the operating system
c for execution.  If the character returned by the routine OSCHAR is a
c space or a comma then this routine is never called.
c
c Arguments:
c  CMD     Input   CHARACTER*(*)  A string containing the command line
c                                 to send.
c  IFAIL   Output  INTEGER        Zero if the command was executed
c                                 successfully, else > 0.
c
c     Version 1.2   24th November, 1986   Alliant FX-FORTRAN
c
c Declare the routine's arguments.
c
      subroutine giveos(cmd, ifail)
      integer ifail
c
c Declare external references.
c
      character cmd*(*)
c
c Declare local variables.
c
      integer system
c
c Pass the command to the shell.
c
      integer istat
c
c Test if successful.
c
      istat = system(cmd)
c
c Return success.
c
      if (istat .ge. 0) then
      ifail = 0
c
c Return failure.
c
      else
      ifail = 1
c
c End of subroutine GIVEOS.
c
      end if
      end
