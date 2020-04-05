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
      IMPLICIT NONE
c
      real date,wcorr,wepoch,omega,dt
      integer nfit,maxfit,luin,ifail,length,i
      character directory*80,filin*60,oldfile*60
      logical lexist
      parameter (maxfit=100)
      real pars(maxfit)
      save pars,oldfile
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
         read(luin,*,err=9000) wepoch,omega,nfit
         do i=1,nfit
            read(luin,*,err=9000) pars(i)
         enddo
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
      do i=1,nfit/2
         wcorr = wcorr + pars(2*i-1)*cos(omega*(i-1)*dt)
     &       + pars(2*i)*sin(omega*(i-1)*dt)
      enddo
c
      return
c
 9000 write(*,*)'Error in reading from parameter file'
      ifail = 51
      return
c
      END
c













