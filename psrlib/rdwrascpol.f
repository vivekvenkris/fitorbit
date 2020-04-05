      subroutine wrascpol (
     &  iounit,jname,name,pb,pdot,dm,rm,freq,chbw,nch,ctel,mjd,cdate,
     &  refkey,nstk,nbin,tbin,tres,lpa,ibase,rmsmjy,i,q,u,v,ifail)

c This subroutine writes pulsar Stokes parameter data in ascii form.
c Data given as r*4, converted to i*2 and written using wrascpoli.
c Data written by this subroutine can be read using rdascpol.

c Input parameters:
c  iounit   i4  unit for input or output
c  jname    c12 pulsar jname
c  name     c12 pulsar common name
c  pb       r8  barycentric period (msec)
c  pdot     r8  period derivative (10**-15)
c  dm       r8  dispersion measure (pc cm**-3)
c  rm       r8  rotation measure (rad m**2)
c  freq     r8  central observing frequency (MHz)
c  chbw     r8  channel bandwidth (MHz)
c  nch      i4  number of filterbank channels used
c  ctel     c8  telescope name
c  mjd      r8  mjd (days)
c  cdate    c8  gregorian date (yy/mm/dd)
c  refkey   c8  reference bibtex key
c  nstk     i4  number of channels (4 for full stokes or 1 for just i)
c  nbin     i4  number of bins
c  tbin     r4  bin size (msec)
c  tres     r4  net resolution (msec)
c  lpa      l4  1 if position angles are absolute, else 0.
c  ibase(4) i4  start and end bins for two areas used for base and rms calcs.
c  rmsmjy   i4  mean rms of the nstk channels (mJy)
c  i(nbin)  r4  Stokes parameter I (mJy)
c  q(nbin)  r4  Stokes parameter Q (mJy)
c  u(nbin)  r4  Stokes parameter U (mJy)
c  v(nbin)  r4  Stokes parameter V (mJy)
c Returned:
c  ifail    i4  0=OK, 1=Error on writing, 2=Bad no of channels

      implicit none
      integer*4 maxbin
      parameter(maxbin=1024)
      integer*4 iounit,nch,nstk,nbin,lpa,ibase(4),ifail,ibin
      integer*2 ii(maxbin),qq(maxbin),uu(maxbin),vv(maxbin)
      real*4 tbin,tres,rmsmjy,i(*),q(*),u(*),v(*),
     &         scale,offset,dmax,dmin
      real*8 pb,pdot,dm,rm,freq,chbw,mjd
      character jname*12,name*12,cdate*8,ctel*8,refkey*8

      ifail=0

c First determine the scale.
      dmax=-1e30
      dmin= 1e30
      if(nstk.eq.4)then
        do ibin=1,nbin
          dmax=max(dmax,i(ibin))
          dmax=max(dmax,q(ibin))
          dmax=max(dmax,u(ibin))
          dmax=max(dmax,v(ibin))
          dmin=min(dmin,i(ibin))
          dmin=min(dmin,q(ibin))
          dmin=min(dmin,u(ibin))
          dmin=min(dmin,v(ibin))
        enddo
      elseif(nstk.eq.1)then
        do ibin=1,nbin
          dmax=max(dmax,i(ibin))
          dmin=min(dmin,i(ibin))
        enddo
      else
        write(*,*) name,': Daft number of output channels requested !'
        ifail=2
      endif

      scale=9999./(dmax-dmin)
      offset=dmin

      do ibin=1,nbin
         ii(ibin)=nint((i(ibin)-offset)*scale)
      enddo
      if(nstk.eq.4)then
         do ibin=1,nbin
            qq(ibin)=nint((q(ibin)-offset)*scale)
            uu(ibin)=nint((u(ibin)-offset)*scale)
            vv(ibin)=nint((v(ibin)-offset)*scale)
         enddo
      endif

      call wrascpoli (
     &  iounit,jname,name,pb,pdot,dm,rm,freq,chbw,nch,ctel,mjd,cdate,
     &  refkey,nstk,nbin,tbin,tres,lpa,ibase,rmsmjy,ii,qq,uu,vv,scale,
     &  offset,ifail)

      return

      end

c***************************************************************************
      subroutine wrascpoli (
     &  iounit,jname,name,pb,pdot,dm,rm,freq,chbw,nch,ctel,mjd,cdate,
     &  refkey,nstk,nbin,tbin,tres,lpa,ibase,rmsmjy,ii,qq,uu,vv,scale,
     &  offset,ifail)

c This subroutine writes pulsar Stokes parameter data in ascii form.
c Data written by this subroutine can be read using rdascpol.

c Input parameters:
c  iounit   i4  unit for input or output
c  jname    c12 pulsar jname
c  name     c12 pulsar common name
c  pb       r8  barycentric period (msec)
c  pdot     r8  period derivative (10**-15)
c  dm       r8  dispersion measure (pc cm**-3)
c  rm       r8  rotation measure (rad m**2)
c  freq     r8  central observing frequency (MHz)
c  chbw     r8  channel bandwidth (MHz)
c  nch      i4  number of filterbank channels used
c  ctel     c8  telescope name
c  mjd      r8  mjd (days)
c  cdate    c8  gregorian date (yy/mm/dd)
c  refkey   c8  reference bibtex key
c  nstk     i4  number of channels (4 for full stokes or 1 for just i)
c  nbin     i4  number of bins
c  tbin     r4  bin size (msec)
c  tres     r4  net resolution (msec)
c  lpa      l4  1 if position angles are absolute, else 0.
c  ibase(4) i4  start and end bins for two areas used for base and rms calcs.
c  rmsmjy   i4  mean rms of the nstk channels (mJy)
c  ii(nbin) i2  Stokes parameter I (mJy)
c  qq(nbin) i2  Stokes parameter Q (mJy)
c  uu(nbin) i2  Stokes parameter U (mJy)
c  vv(nbin) i2  Stokes parameter V (mJy)
c  scale    r4  Scale factor for data used to convert stokes data to mJy.
c  offset   r4  Offset in mJy added to all stokes data before scaling.
c Returned:
c  ifail    i4  0=OK, 1=Error on writing, 2=Bad no of channels

      implicit none
      integer*4 iounit,nch,nstk,nbin,lpa,ibase(4),ifail,ibin
      integer*2 ii(*),qq(*),uu(*),vv(*)
      real*4 tbin,tres,rmsmjy,scale,offset
      real*8 pb,pdot,dm,rm,freq,chbw,mjd
      character jname*12,name*12,cdate*8,ctel*8,refkey*8

      ifail=0

      write(iounit,*,err=999)
      write(iounit,'(a)',err=999)
     &                   '@---------------------------------------'//
     &                   '----------------------------------------'
      write(iounit,'(2a12,f12.6,f12.6,f8.3,f10.3)',err=999) 
     &          jname,name,pb,pdot,dm,rm
      write(iounit,'(2f10.3,i4,x,a8,f10.3,x,a8,x,a8)',err=999) 
     &          freq,chbw,nch,ctel,mjd,cdate,refkey
      write(iounit,'(2i4,2f9.3,5i4,f8.3,2f12.6)',err=999) 
     &          nstk,nbin,tbin,tres,lpa,ibase,rmsmjy,scale,offset
      write(iounit,*,err=999)
      write(iounit,*,err=999)

      write(iounit,'(20i4)',err=999)   (ii(ibin),ibin=1,nbin)
      if(nstk.eq.4)then
        write(iounit,'(20i4)',err=999) (qq(ibin),ibin=1,nbin)
        write(iounit,'(20i4)',err=999) (uu(ibin),ibin=1,nbin)
        write(iounit,'(20i4)',err=999) (vv(ibin),ibin=1,nbin)
      endif

      return

 999  write(*,'(a)') ' Error writing poldata. Pulsar: '//jname
      ifail=1

      end

c***************************************************************************

      subroutine rdascpol (
     &  iounit,jname,name,pb,pdot,dm,rm,freq,chbw,nch,ctel,mjd,
     &  cdate,refkey,nstk,nbin,tbin,tres,lpa,ibase,rmsmjy,i,q,u,v,ifail)

c This subroutine reads pulsar Stokes parameter data in ascii form.
c Data written by wrascpol can be read using this subroutine.

c Returned parameters:
c  iounit   i4  unit for input or output
c  jname    c12 pulsar jname
c  name     c12 pulsar common name
c  pb       r8  barycentric period (msec)
c  pdot     r8  period derivative (10**-15)
c  dm       r8  dispersion measure (pc cm**-3)
c  rm       r8  rotation measure (rad m**2)
c  freq     r8  central observing frequency (MHz)
c  chbw     r8  channel bandwidth (MHz)
c  nch      i4  number of filterbank channels used
c  ctel     c8  telescope name
c  mjd      r8  mjd (days)
c  cdate    c8  gregorian date (yy/mm/dd)
c  refkey   c8  reference bibtex key
c  nstk     i4  number of channels (4 for full stokes or 1 for just i)
c  nbin     i4  number of bins
c  tbin     r4  bin size (msec)
c  tres     r4  net resolution (msec)
c  lpa      l4  1 if position angles are absolute, else 0.
c  ibase(4) i4  start and end bins for two areas used for base and rms calcs.
c  rmsmjy   i4  mean rms of the nstk channels (mJy)
c  i(nbin)  r4  Stokes parameter I (mJy)
c  q(nbin)  r4  Stokes parameter Q (mJy)
c  u(nbin)  r4  Stokes parameter U (mJy)
c  v(nbin)  r4  Stokes parameter V (mJy)
c  ifail    i4  0=OK, 1=error, 2=eof


      implicit none
      integer*4 maxbin
      parameter(maxbin=1024)
      integer*4 iounit,nch,nstk,nbin,lpa,ibase(4),ifail,ibin
      integer*2 ii(maxbin),qq(maxbin),uu(maxbin),vv(maxbin)
      real*4 tbin,tres,rmsmjy,i(*),q(*),u(*),v(*),
     &         scale,offset
      real*8 pb,pdot,dm,rm,freq,chbw,mjd
      character jname*12,name*12,cdate*8,ctel*8,refkey*8

      ifail=0

      call rdascpoli (
     &  iounit,jname,name,pb,pdot,dm,rm,freq,chbw,nch,ctel,mjd,
     &  cdate,refkey,nstk,nbin,tbin,tres,lpa,ibase,rmsmjy,ii,qq,uu,vv,
     &  scale,offset,ifail)
      if(ifail.ne.0) return

c Rescale.
      do ibin=1,nbin
         i(ibin) = ii(ibin)/scale + offset
      enddo

      if(nstk.eq.4)then
        do ibin=1,nbin
          q(ibin) = qq(ibin)/scale + offset
          u(ibin) = uu(ibin)/scale + offset
          v(ibin) = vv(ibin)/scale + offset
        enddo
      endif
      return

      end


c***************************************************************************

      subroutine rdascpoli (
     &  iounit,jname,name,pb,pdot,dm,rm,freq,chbw,nch,ctel,mjd,
     &  cdate,refkey,nstk,nbin,tbin,tres,lpa,ibase,rmsmjy,ii,qq,uu,vv,
     &  scale,offset,ifail)

c This subroutine reads pulsar Stokes parameter data in ascii form.
c Data returned in scaled i*2 form. Unpack using i = ii/scale + offset, etc.
c Data written by wrascpoli can be read using this subroutine.

c Returned parameters:
c  iounit   i4  unit for input or output
c  jname    c12 pulsar jname
c  name     c12 pulsar common name
c  pb       r8  barycentric period (msec)
c  pdot     r8  period derivative (10**-15)
c  dm       r8  dispersion measure (pc cm**-3)
c  rm       r8  rotation measure (rad m**2)
c  freq     r8  central observing frequency (MHz)
c  chbw     r8  channel bandwidth (MHz)
c  nch      i4  number of filterbank channels used
c  ctel     c8  telescope name
c  mjd      r8  mjd (days)
c  cdate    c8  gregorian date (yy/mm/dd)
c  refkey   c8  reference bibtex key
c  nstk     i4  number of channels (4 for full stokes or 1 for just i)
c  nbin     i4  number of bins
c  tbin     r4  bin size (msec)
c  tres     r4  net resolution (msec)
c  lpa      l4  1 if position angles are absolute, else 0.
c  ibase(4) i4  start and end bins for two areas used for base and rms calcs.
c  rmsmjy   i4  mean rms of the nstk channels (mJy)
c  i(nbin)  r4  Stokes parameter I (mJy)
c  q(nbin)  r4  Stokes parameter Q (mJy)
c  u(nbin)  r4  Stokes parameter U (mJy)
c  v(nbin)  r4  Stokes parameter V (mJy)
c  scale    r4  Scale factor for data used to convert stokes data to mJy.
c  offset   r4  Offset in mJy added to all stokes data before scaling.
c               Rescale from i2 to r4 using i = ii/scale + offset, etc.
c  ifail    i4  0=OK, 1=error, 2=eof


      implicit none
      integer*4 maxbin
      parameter(maxbin=1024)
      integer*4 iounit,nch,nstk,nbin,lpa,ibase(4),ifail,ibin
      integer*2 ii(maxbin),qq(maxbin),uu(maxbin),vv(maxbin)
      real*4 tbin,tres,rmsmjy,scale,offset
      real*8 pb,pdot,dm,rm,freq,chbw,mjd
      character jname*12,name*12,cdate*8,ctel*8,refkey*8,line*120

      ifail=0
      line=' '

      do while(line(1:2).ne.'@-')
         read(iounit,'(a)',err=998,end=999) line
      enddo

      read(iounit,'(2a12,f12.6,f12.6,f8.3,f10.3)',err=998,end=999) 
     &            jname,name,pb,pdot,dm,rm
      read(iounit,'(2f10.3,i4,x,a8,f10.3,x,a8,x,a8)',err=998,end=999) 
     &          freq,chbw,nch,ctel,mjd,cdate,refkey
      read(iounit,'(2i4,2f9.3,5i4,f8.3,2f12.6)',err=998,end=999) 
     &          nstk,nbin,tbin,tres,lpa,ibase,rmsmjy,scale,offset
      read(iounit,'(a)',err=998,end=999) line
      read(iounit,'(a)',err=998,end=999) line
      if(nbin.gt.maxbin)then
         write(*,'(a)') name//': Too many bins for rdascpol.'
         ifail=1
         return
      endif

      read(iounit,'(20i4)',err=998,end=999)   (ii(ibin),ibin=1,nbin)
      if(nstk.eq.4)then
        read(iounit,'(20i4)',err=998,end=999) (qq(ibin),ibin=1,nbin)
        read(iounit,'(20i4)',err=998,end=999) (uu(ibin),ibin=1,nbin)
        read(iounit,'(20i4)',err=998,end=999) (vv(ibin),ibin=1,nbin)
      endif
      return

 998  write(*,'(a,a)') ' Error reading ascpol data. Last pulsar: ',jname
      ifail=1
      return

 999  ifail=2
      return

      end




















