c *********************************************************************
      SUBROUTINE getwave(filin,date,wcorr,ifail)
c *********************************************************************
c
c Subroutine to get correction to be applied from wave parameters.
c  If a file 'fitwaves.par' exists and the parameters are zero, then
c  attempts to open the parameter file and read values from it.
c  Calculates the sum of all the waves at the given date(in days) 
c  and returns the value wcorr)in msec). 
c  Ifail is non-zero if a fault occurred.
c
c 10/04/01 GHobbs, modified to read in new version of fitwaves (and keep this
c          backwards compatable.  The new version defines individual sines/cosines
c          and frequencies.
c
      IMPLICIT NONE
c
      real*8 date,wcorr,wepoch,omega,dt
      integer nfit,maxfit,luin,ifail,length,i
      character filin*60,oldfile*60
      character version*4,line*120
      logical lexist
      parameter (maxfit=300)
      real*8 pars(maxfit)
      character wave*5(maxfit)
      real*8 freq(maxfit)
      save pars,oldfile,freq,wave,version
c
      ifail = 0
c
c  if a negative date supplied then clear the arrays
c
      if (date.lt.0.0) then
         wepoch = 0.0
         omega = 0.0
         do i=1,nfit
            pars(i) = 0.0
         enddo
         return
      endif
c
c First of all see if any parameters have already been loaded.
      if (wepoch.eq.0.0.or.filin.ne.oldfile) then
         INQUIRE(FILE=filin(1:length(filin)),EXIST=LEXIST)
         IF (.not.LEXIST) THEN
            WRITE(*,*)'Wave parameter file does not exist'
            ifail = 51
            return
         endif
c
         luin = 27
         open(luin,file=filin,status='old')
c
c GHobbs: For backwards compatability, check which version we are using
c
         read(luin,500,err=9000) line
 500     format((a))
         if (line(1:4).eq."Ver2") then
            read(line,600,err=9000) version,wepoch,omega,nfit
 600        format((a),x,f12.6,f15.9,i4)
            write(*,*)'Fitwaves, Version = ',version
            do i=1,nfit
               read(luin,*,err=9000) wave(i),pars(i),freq(i)
            enddo
         else if (line(1:4).ne."Ver2") then
            version = "Ver1"
            write(*,*)'Fitwaves, Version 1'
            read(line,*,err=9000) wepoch,omega,nfit
            do i=1,nfit
               read(luin,*,err=9000) pars(i)
            enddo
         endif
         close(luin)
         oldfile = filin
      endif
c     
c parameters now loaded
c calculate the sum of the waves
c first calculate the time from epoch
c
      dt = date - wepoch
      wcorr = 0.0
c
      if (version.eq."Ver2") then
         do i=1,nfit
            if (wave(i).eq.'cos') then 
               wcorr = wcorr + pars(i)*cos(freq(i)*dt)
            else if (wave(i).eq.'sin') then
               wcorr = wcorr + pars(i)*sin(freq(i)*dt)
            else
               write(*,*)'In getwave.f, unknown wave type'
            endif
         enddo
      else         
         write(*,*)'Process as version 1'
         do i=1,nfit/2
            wcorr = wcorr + pars(2*i-1)*cos(omega*(i-1)*dt)
     &           + pars(2*i)*sin(omega*(i-1)*dt)         
         enddo
      endif
c
      return
c
 9000 write(*,*)'Error in reading from parameter file'
      ifail = 51
      return
c
      END
c













