cDECK MAXFIL
c
c
c
c
c This function returns the maximum allowable file name length,
c     In this generic version a maximum file size of 60 characters
c has been set.  This may be too large for some systems, but the value
c is used for checking file name lengths in command lines and similar
c places, so being too large is better than being too small.  The length
c specified also includes and directory or path information.  Programs
c should not fail if this parameter is incorrect, but some truncation or
c "file open" type errors may happen instead of "file name too long"
c errors.
c     Version 1.1   8th September, 1986   Generic
c
c Define the maximum size of file name.
c
      integer function maxfil()
      integer maxfnl
c
c Return the length.
c
      parameter (maxfnl = 60)
c
c End of INTEGER function MAXFIL.
c
      maxfil = maxfnl
      end
