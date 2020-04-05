c
c This routine returns the character that is used to prefix a command
c line with if the command is to be passed to the operating system for
c execution.  Characters which might start OLAF command lines must be
c avoided.  If this routine returns a space or comma then no commands
c will ever be passed to the operating system.
c     
c     Alliant version 1.0   26th January 1987.
c
c Return the character.
c
      character *1function oschar()
c
c End of CHARACTER*1 function OSCHAR.
c
      oschar = '$'
      end
