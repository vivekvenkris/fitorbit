c returns an error message MSSGE of the laas error that occured
c IOERR is included for compatibility with the VMS version
c may 2006 change mssge=gerror() for g77 compatibility
      subroutine erform(ioerr, messge)
      character messge*(*)
      character msg*100
      integer ioerr
      call gerror(msg)
      write(messge,'(a)')msg
      end

 
