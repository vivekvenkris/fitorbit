c
c This routine prints the string LINE, padded out with PAD trailing
c spaces, and with a single carriage return at its beginning.  The
c string is always sent to the standard output.
c
c Arguments:
c  LINE  Input  CHARACTER*(*) The string to print.
c  PAD   Input  INTEGER       The number of spaces to pad out the line.
c
c     Version 1.1   22nd March, 1987  Alliant FX/Fortran
c
c Declare the routine's arguments.
c
      subroutine overprint(line, pad)
      character line*(*)
c
c Declare local variables.
c
      integer pad
c
c First print the carriage return.
c
      integer i
c
c Now the string.
c
      call putc(char(13))
      do 1 i = 1, len(line)
      call putc(line(i:i))
c
c Finally the trailing spaces.
c
    1 continue
      do 2 i = 1, pad
      call putc(' ')
c
c Ensure that the output is flushed.
c
    2 continue
c
c End of subroutine OVERPRINT.
c
c      call flush(6)
      end
