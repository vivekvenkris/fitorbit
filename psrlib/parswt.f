cDECK PARSWT
c
c
c *********************************************************************
c *********************************************************************
c
c ATTEMPTS TO INTERPRET THE STRING AS A SWITCH.
c     RETURNS THE VALUE .TRUE. IF SUCCESSFUL AND .FALSE. OTHERWISE.
c     ON EXIT LVAL CONTAINS THE SWITCH VALUE IF PARSWT IS .TRUE.
c     AND IS UNCHANGED OTHERWISE.
c
      logical function parswt(string, lval)
      character string*(*)
c
c     TLIST AND FLIST CONTAIN THE ALLOWED CHARACTER STRINGS FOR
c     TRUE AND FALSE STATES OF THE SWITCH.
c
      logical lval
      character tlist(5)*5, flist(5)*5
      save flist, tlist
c
c     LOOK FOR THE STRING IN EACH LIST.
c
      data tlist / 'on', 'true', 'yes', 'set', ' ' /
      data flist / 'off', 'false', 'no', 'reset', ' ' /
      itrue = intcmd(tlist,string)
c
c     THE STRING HAS ONLY BEEN UNAMBIGUOUSLY RECOGNIZED IF ONE OF
c     ITRUE AND IFALSE IS POSITIVE AND THE OTHER IS ZERO.
      ifalse = intcmd(flist,string)
c
c        SET THE FUNCTION AND THE RETURNED SWITCH VALUE.
c
      if (((itrue .gt. 0) .and. (ifalse .eq. 0)) .or. ((ifalse .gt. 0)
     & .and. (itrue .eq. 0))) then
      parswt = .true.
      lval = itrue .gt. 0
      else
      parswt = .false.
c
      end if
c
c END OF LOGICAL FUNCTION PARSWT.
c
      return 
      end







