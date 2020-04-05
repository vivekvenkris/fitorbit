cDECK CSWITCH
c
c **************************************************************
c **************************************************************
c
c RETURNS A CHARACTER STRING REPRESENTING THE STATE OF SWITCH LVAL.
c
      character *(*)function cswitch(lval)
      logical lval
      character on*(*), off*(*)
c
c     RETURN THE APPROPRIATE STRING.
c
      parameter (on = 'ON', off = 'OFF')
      if (lval) then
      cswitch = on
      else
      cswitch = off
c
      end if
c
c END OF CHARACTER*(*) FUNCTION CSWITCH.
c
      return 
      end
