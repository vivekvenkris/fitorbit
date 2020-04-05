c
c **********************************************************************
c **********************************************************************
c
c UNIX version- relies on Jodrell Bank rprint utility
c
c THIS ROUTINE IS USED TO PRINT THE CONTENTS OF THE FILE CURRENTLY
c     OPENED ON LOGICAL UNIT LUPRI.
c QUENAM CONTAINS THE NAME OF THE PRINT QUEUE ( DEFAULT: printer ).
c DELETE IS A LOGICAL DETERMINING WHETHER THE FILE IS DELETED AFTER
c     PRINTING.
c IFAIL IS ZERO IF THE PRINTING WAS SUCCESSFUL.
c
c THIS ROUTINE IS INSTALLATION DEPENDENT.
c
c FX/FORTRAN VERSION.
c
c Mod 10/03/2006 Correct data type in call to pserr
c
      subroutine prifil(lupri, quenam, delete, ifail)
c
      include 'PSRLIB.DEF'
c DEFINITIONS OF THE PULSAR LIBRARY GLOBALS
c
c     PROGRAM LOGICAL UNITS
c
      character quenam*(*), name*60, jobnam*40, quebuf*40, stabuf*40, 
     &logtrn*80, uppcase*80
c
c     CLEAR THE ERROR FLAG.
c
      logical delete, op
c
c     RETURN IF LOGICAL UNIT IS NOT ENABLED.
c
      ifail = 0
c
c     RETURN WITHOUT ERROR IF NO QUEUE IS SPECIFIED.
c
      if (lupri .le. 0) return 
c
c     OBTAIN THE NAME OF THE FILE.
c
      if (uppcase(quenam) .eq. '<NONE>') return 
c
c     ERROR EXIT IF THE FILE IS NOT OPENED.
c
      inquire(unit=lupri, name=name, opened=op, err=9999) 
c
c     SET THE QUEUE NAME. DEFAULT IS SYS$PRINT.
c
      if ((.not. op) .or. (name .eq. ' ')) goto 9999
      if (uppcase(quenam) .eq. '<DEFAULT>') then
      quebuf = 'printer'
      else
      quebuf = quenam
c
c
c     PRINT THE FILE.
c
      end if
c
c     SET THE STATUS OPTION FOR THE CLOSE STATEMENT.
c
      call giveos((('fpr < ' // name) // ' | rprint -d ') // quebuf, 
     &istat)
      if (delete) then
      close(unit=lupri, status='DELETE', err=9998, iostat=istat) 
      else
      close(unit=lupri, err=9998, iostat=istat) 
c     MONITOR.
c
      end if
c
      call outmon(' Job  entered on queue  ' // quebuf)
c
c     LABEL 9999 IS THE ERROR EXIT POINT FOR FILE NOT FOUND.
c
      return 
 9999 continue
      ifail = 43
      call psrerr('PRIFIL', ifail, lupri, 0., name)
c
c     LABEL 9998 IS THE EXIT POINT FOR ERROR IN CLOSE.
c
      return 
 9998 continue
      ifail = 63
      call psrerr('PRIFIL', ifail, istat, 0., name)
c
c END OF SUBROUTINE PRIFIL.
c
      return 
      end
