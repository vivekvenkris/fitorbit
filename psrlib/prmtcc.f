c
c This routine returns the carriage control character to use when
c printing a command prompt.
c This routine is installation dependent.
c
c     Version 1.1   20th November, 1986   Alliant FX-FORTRAN.
c
c Define the character to use.
c
      character *1function prmtcc()
      character cc*(*)
c
c Return the character.
c
      parameter (cc = '$')
c
c End of CHARACTER*1 function PRMTCC.
c
      prmtcc = cc
      end
