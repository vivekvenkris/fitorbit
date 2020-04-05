cDECK OPNFIL
c
c ***********************************************************
c ***********************************************************
c
c IF NOT ALREADY OPEN, OPENS FILE FILNAM RETURNING THE LOGICAL
c    UNIT NUMBER IN LU.
c ACCESS MAY BE ONE OF 'INPUT','OUTPUT','UNFORMATTED'
c    THIS DETERMINES SUCH THINGS AS THE RECORD LENGTH AND TYPE
c IFAIL IS ZERO UNLESS AN ERROR OCCURRED
c
c
c THIS ROUTINE IS INSTALLATION DEPENDENT
c
c Alliant FORTRAN VERSION
c
c Mod 10/03/2006 Correct data type in call to pserr
c
      subroutine opnfil(lu, filnam, access, ifail)
      character filnam*(*), access*(*)
      character fn*60, rectyp*60, type*60, form*60, cfail*60, org*60, 
     &acc*60
      logical op, exi, terminl
c
      integer ttwidth
c
c     CLEAR THE ERROR FLAG
c
      include 'PSRLIB.DEF'
c DEFINITIONS OF THE PULSAR LIBRARY GLOBALS
c
c     PROGRAM LOGICAL UNITS
c
      ifail = 0
      if (((((filnam .eq. '<default>') .or. (filnam .eq. '<vdu>')) .or. 
     &(filnam .eq. '<DEFAULT>')) .or. (filnam .eq. '<VDU>')) .or. (
     &filnam .eq. 'VDU')) then
      if (((access(1:5) .ne. 'DATA_') .and. (access .ne. 'UNFORMATTED'))
     & .and. (access .ne. 'INDEXED')) then
      if (access .eq. 'INPUT') then
      lu = 5
      lurecl(lu) = 80
      lupagl(lu) = -24
      if (terminl(lu,'SYS$INPUT',lupagl(lu),lurecl(lu))) then
      filnam = '<vdu>'
      else
      filnam = '<default>'
      end if
      else
c
c              SET RECORD AND PAGE SIZES ACCORDING TO WHETHER
c              A TERMINAL IS ATTACHED
c
      lu = 6
      if (terminl(lu,'SYS$OUTPUT',lupagl(lu),lurecl(lu))) then
      lupagl(lu) = - lupagl(lu)
      filnam = '<vdu>'
      else
      lurecl(lu) = 132
      lupagl(lu) = 59
      filnam = '<default>'
      end if
      end if
      return 
c
c     <none> INDICATES DISABLING LOGICAL UNIT
c
      end if
      else if (filnam(1:1) .eq. '<') then
      lu = -1
      filnam = '<none>'
c
c     IF LU = 5,6 OR <0 NEED TO FIND A SPARE LU
c
      return 
      else if (((lu .eq. 5) .or. (lu .eq. 6)) .or. (lu .le. 0)) then
      lu = lufree(ifail)
      if (ifail .ne. 0) return 
c
c     SET UP THE RECORD LENGTH, PAGE LENGTH, RECORD TYPE
c        AND FILE TYPE APPROPRIATE TO ACCESS.
c        DEFAULT IS FORMATTED, VARIABLE LENGTH.
c
      end if
      form = 'FORMATTED'
      rectyp = 'VARIABLE'
      org = 'SEQUENTIAL'
      acc = 'SEQUENTIAL'
      if (access .eq. 'INPUT') then
      lupagl(lu) = -1
      type = 'OLD'
      else if (access .eq. 'UNFORMATTED') then
      lupagl(lu) = -1
      lurecl(lu) = 47
      form = 'UNFORMATTED'
      type = 'UNKNOWN'
      else
      lurecl(lu) = 133
      lupagl(lu) = 59
      type = 'NEW'
c
c     SET THE CHARACTER ERROR MESSAGE TO FILNAM
c
      end if
c
c     CHECK FILENAME
c
      cfail = filnam
      if (filnam .eq. ' ') then
      ifail = 55
      cfail = 'File name'
      goto 2000
c
c     DETERMINE CURRENT STATUS OF LOGICAL UNIT LU
c
      end if
c
c     CHECK THAT THE ASSIGNED FILE IS DIFFERENT TO THE ONE REQUESTED
c
      inquire(unit=lu, name=fn, opened=op, err=9999, iostat=istat) 
c
c        IF THE LOGICAL UNIT IS ALREADY OPEN THEN CLOSE IT
c
      if (fn .ne. filnam) then
c
c        DETERMINE WHETHER THE FILE EXISTS
c
      if (op) close(unit=lu) 
c
c        IF TYPE IS UNKNOWN, SET TYPE
c
      inquire(file=filnam, exist=exi, iostat=istat, err=9999) 
      if (type .eq. 'UNKNOWN') then
      if (exi) then
      type = 'OLD'
      else
      type = 'NEW'
      end if
c
c        NOW OPEN THE FILE
c
      end if
c
c           FOR AN UNFORMATTED FILE THE CORRECT RECORDTYPE
c           MUST BE SPECIFIED
c
      if (type .eq. 'OLD') then
      if ((form .eq. 'UNFORMATTED') .and. (org .eq. 'SEQUENTIAL')) then
      open(unit=lu, file=filnam, form=form, status='OLD', iostat=istat, 
     &err=9998, recl=lurecl(lu)) 
c              OTHERWISE IT DEFAULTS TO THAT OF THE FILE
c
      else
      open(unit=lu, file=filnam, status='OLD', iostat=istat, err=9998, 
     &recl=lurecl(lu)) 
      end if
c
c           OPEN NEW FILE SPECIFYING EVERYTHING
c           FIRST, IF IT IS INDEXED
c
      else
      open(unit=lu, file=filnam, form=form, status='NEW', iostat=istat, 
     &err=9998, recl=lurecl(lu)) 
      end if
      lurec(lu) = 0
      lupag(lu) = 0
c
c     LABEL 2000 IS THE ROUTINE EXIT POINT
c
      end if
c
c     IF AN ERROR WAS FOUND CALL THE LIBRARY ERROR ROUTINE
c     OTHERWISE RETURN THE FULL FILE NAME
c
 2000 continue
      if (ifail .ne. 0) then
      call psrerr('OPNFIL', ifail, istat, 0., cfail)
      else
      inquire(unit=lu, name=filnam) 
      end if
c
      return 
c
c     TRAP FOR ERROR IN THE INQUIRE
c
 9999 continue
      ifail = 49
      goto 2000
c
c     TRAP FOR ERROR IN THE OPEN
c
 9998 continue
      ifail = 30
c
c END OF SUBROUTINE OPNFIL
c
      goto 2000
      end
