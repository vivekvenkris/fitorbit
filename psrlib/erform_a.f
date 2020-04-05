c returns an error message MSSGE of the laas error that occured
c IOERR is included for compatibility with the VMS version
      subroutine erform(ioerr, messge)
      character messge*(*), gerror*(*)
      external gerror
      integer ioerr
      messge = gerror()
      end
