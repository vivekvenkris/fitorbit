c *********************************************************************
      SUBROUTINE getwave(filin,date,wcorr,ifail)
c *********************************************************************
c
c Subroutine to get correction to be applied from wave parameters.
c  If a file filin'.wav' exists and the parameters are zero, then
c  attempts to open the parameter file and read values from it.
c  Calculates the sum of all the waves at the given date(in days) 
c  and returns the value wcorr)in msec). 
c  Ifail is non-zero if a fault occurred.
c
c 10/04/01 GHobbs, modified to read in new version of fitwaves (and keep this
c          backwards compatable.  The new version defines individual 
c          sines/cosines and frequencies.
c 06/11/01 Caj. Split off the open file/get pars into separate subroutine.
c          add wavopn and wrwav routines
c 22/01/02 Caj. Change to return only wcorr
c
      IMPLICIT NONE
      integer ifail,nfit,maxfit
      parameter (MAXFIT = 32)
      character*(*) filin
      real*8 epoch, omega
      real*8 freq(maxfit),pars(maxfit),werr(maxfit)

      real*8 date,wcorr,dt
      integer i,infit
      character oldfile*60
      save oldfile
c
      wcorr= 0d0
      ifail = 0

c  if a negative date supplied then clear the arrays

      if (date.lt.0.0) then
         epoch = 0.0
        omega = 0.0
         do i=1,maxfit,2
            pars(i) = 0.0
           pars(i+1) = 0.0
         enddo
         return
      endif

c First of all see if any parameters have already been loaded
      if (epoch.eq.0.0) then 

c .or.filin.ne.oldfile) then
        write(*,'(a)')' Loading wave parameters from '//filin

c check for .wav ending!
        CALL OPNWAV(ifail,filin,epoch,omega,nfit,pars,freq,werr,
     &              maxfit) 
        if (ifail .ne. 0)   RETURN
        oldfile = filin
      endif
c     
c check wthere a number of harmonics has been given, or use file
      if (infit .eq. 0 ) infit = nfit
      if (infit .gt. maxfit) infit = maxfit

c parameters now loaded
c calculate the sum of the waves
c first calculate the time from epoch
      dt = date - epoch
c
c      if (version.eq."Ver2") then
         do i=1,infit,2
               wcorr = wcorr + pars(i)*cos(freq(i)*dt)+
     &                     pars(i+1)*sin(freq(i+1)*dt)
        enddo
c
      return
      END
c


c *********************************************************************
      SUBROUTINE OPNWAV(ifail,filin,wepoch,omega,nfit,pars,freq,
     &                  werr,maxfit)
c *********************************************************************
c
c Subroutine to get wave parameters from filin.
c 10/04/01 GHobbs, modified to read in new version of fitwaves (and keep this
c          backwards compatable.  The new version defines individual sines/cosines
c          and frequencies.
c 22/01/02 Caj. Add wave errors column to file (ver3)
c 15/02/02 Caj. Read phase and amplitutudes (ver4&5) (Errors are in phase/amp not sin/cos)
      IMPLICIT NONE
c
      integer ifail,nfit,maxfit
      character*40 filin
      real*8 wepoch
      character*3 wave(maxfit)
      real*8 freq(maxfit),pars(maxfit),werr(maxfit), phase, amp,
     &       phaserr,amperr

      integer luin,length,i,getlun,flen
      character line*120, version*4
      logical lexist
      real*8 omega
      ifail = 0

C check for file - check for .wav and add it if necessary
      flen = length(filin)
      if (flen .gt.4 .and. filin(flen-3:flen) .eq. '.wav') flen=flen-4

      INQUIRE(FILE=filin(1:flen)//'.wav',EXIST=LEXIST)
      IF (.not.LEXIST) THEN
        ifail = 51
        return
      endif
c
      luin = getlun()
      open(luin,file=filin(1:flen)//'.wav',status='old')
c
c GHobbs: For backwards compatability, check which version we are using
c
      read(luin,500,err=9000) line
 500  format((a))
      version=line(1:4)
      if (version.eq.'Ver2') then
        read(line(5:),*,err=9000) wepoch,omega,nfit
 600    format(a4,f13.6,f16.9,i4)
        write(*,*)' Fitwaves, Version = '//version
            
        if (nfit .gt. maxfit) nfit =maxfit
        do i=1,nfit
          read(luin,*,err=9000) wave(i),pars(i),freq(i)
          werr(i)=0d0
        enddo
      
      elseif (version .eq. 'Ver4' .or. version .eq. 'Ver5') then
        read(line(5:),*,err=9000) wepoch,omega,nfit
        write(*,*)' Fitwaves, Version = '//version
            
        if (nfit .gt. maxfit) nfit =maxfit
        do i=1,nfit,2
          read(luin,*,err=9000) freq(i),amp,amperr,phase,phaserr 
          pars(i)= amp * sin(phase)
          pars(i+1) = amp * cos(phase)
        enddo
      
      elseif (version .eq. 'Ver3') then
        read(line(5:),*,err=9000) wepoch,omega,nfit
        write(*,*)' Fitwaves, Version = '//version
            
        if (nfit .gt. maxfit) nfit =maxfit
        do i=1,nfit
          read(luin,*,err=9000) wave(i),pars(i),freq(i),werr(i)
        enddo
      
      else if (line(1:4).ne.'Ver2') then
        version = 'Ver1'
        write(*,*) ' Fitwaves, Version 1'
        read(line,*,err=9000) wepoch,omega,nfit
        if (nfit .gt. maxfit) nfit =maxfit
        do i=1,nfit  
          read(luin,*,err=9000) pars(i)
          freq(i) = omega*(i-1)/2
          werr(i)=0d0
        enddo
      endif
      close(luin)
      call freelun(luin)
      return
c
 9000 ifail = 51
      return
c
      END
c


c *********************************************************************
      SUBROUTINE WRWAV(ifail,filin,wepoch,omega,nfit,pars,freq,
     &                 werr,maxfit)
c *********************************************************************
c
c Subroutine to write wave parameters to filin.wav
c 12/11/2001 CAJ. Writes a version 2 styled wav file
c 22/01/2002 CAJ. Added errors column - version 3 style file
c 15/02/2002 CAJ. Save phase and ampliitude - Version 4
c 18/03/2002 CAJ. Also write an interpolated data set 
c 10/05/2002 CAJ. Change errros in sin/cos terms to erros in phase/amp.
c 22/05/2002 CAJ. Ver 5 corrected the calculation of phase err!
      IMPLICIT NONE
      DOUBLE PRECISION PI,TWOPI, MINNUM
      PARAMETER ( PI      = 3.1415926536D0
     &           ,TWOPI   = 2.0*PI
     &           ,MINNUM  = 0.0000000001)

      integer ifail,nfit, maxfit,j,i, luin, getlun, length
      character*(*) filin
      real*8 wepoch, omega, dt, xspan, xmin,x,y,tmpomega
      real*8 freq(maxfit),pars(maxfit),werr(maxfit),phase,amp,
     &       amperr, phaserr
      character thisfile*40, newfile*40
      integer MAXPTS
      parameter (MAXPTS = 256)
      
      ifail = 0

C check for file
      thisfile = filin
      call CHECKB4WRITE(thisfile, '.wav', newfile)
      IF (NEWfile .NE. ' ' ) THEN
        thisfile = NEWfile
      ELSE
        RETURN
      ENDIF
      
c
      luin = getlun()
      open(luin,file=thisfile(1:length(thisfile))//'.wav',status='new',
     &        form='formatted')
c
c write version 5 style numbers
      write(luin,'(a,f13.6,f16.9,i4)',iostat = ifail) 
     &                        'Ver5 ',wepoch,omega,min(maxfit,nfit) 
      do i=1,min(maxfit,nfit),2
        amp = sqrt(pars(i)*pars(i) + pars(i+1)*pars(i+1))
        phase = atan2(pars(i), pars(i+1))

* Calculate errors in phase and amplitutde from errors in cos and sin terms
        Amperr = 0.0
        Phaserr = 0.0
        if (abs(amp) .gt.MINNUM )
     &         Amperr = 1.0/amp * sqrt(pars(i)*pars(i)*werr(i)*werr(i) + 
     &                          pars(i+1)*pars(i+1)*werr(i+1)*werr(i+1))
c ver 4 had this faulty calculation! changed May 22 2002
c        if ( abs(pars(i)).gt.MINNUM .and. abs(pars(i+1)) .gt. MINNUM )
c     &            Phaserr = sqrt(werr(i)*werr(i)/ (pars(i+1)*pars(i+1))+
c     &                 werr(i+1)*werr(i+1)/(pars(i)*pars(i)))
c ver 5 uses this one!
        if ( abs(pars(i+1)).gt.MINNUM )
     &            Phaserr = cos(Phase)*cos(Phase)/pars(i+1)*
     &                        sqrt(werr(i)*werr(i) + 
     &                             pars(i)*pars(i)/pars(i+1)/pars(i+1)
     &                             * werr(i+1)*werr(i+1))
        write(luin,'(5f14.8)',iostat = ifail)
     &                         freq(i),amp,amperr,phase,phaserr
      enddo

      close(luin)
c
c write out a file of interpolated data points
c  - don't bother checking file - use the answer given above!
      open(luin,file=thisfile(1:length(thisfile))//'.wavdat',
     &        status='new',form='formatted')

c calculate the data span, and start point  (years)
      xspan=TWOPI/omega/(1.+4./nfit)
c
c 256 data points
      xmin = -xspan/2.
      DO I=1,MAXPTS
        X = xmin + xspan*float(I-1)/MAXPTS
        Y = 0 .0
        TMPOMEGA = 0.0
        DT = X
        DO J = 1,nfit,2
          Y = Y + pars(J)*COS(+DT * TMPOMEGA)
     &                 + pars(J+1)*SIN(+DT * TMPOMEGA)
          TMPOMEGA = TMPOMEGA+OMEGA
        enddo
        write(luin,'(2f13.6)',iostat = ifail) x*365.25,y
      enddo

      close(luin)


      call freelun(luin)
      if (ifail .ne.0) goto 9000
      return
c
 9000 ifail = 51
      return
c
      END
c


