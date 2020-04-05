c-------------------------------------------------------------------
      subroutine fitplaw(nsamp, ivar, apar, bpar, alpha, a_err,
     &           beta, b_err, gamma, g_err)
c-------------------------------------------------------------------
c
c     Routine to fit an independent variable ivar to a power law
c     involving two parameters called apar and bpar of the following 
c     form
c            ivar ~ gamma * apar^alpha * bpar^beta
c     using the least squares lmm algorithm.
c
c     One example is the luminosity distribution of a sample of pulsars
c     or model pulsars to the familiar power law :
c                         L ~ P^alpha Pdot^beta
c     after Proszynski & Przbycien 1984. Uses least squares lmm
c     fitting algorithm to obtain convergence.
c
c     input parameters...
c       nsamp - integer number of pulsars in sample
c       ivar  - real*4 array containing the independent variable (nsamp)
c       apar  - real*4 array containing 1st "dependent" variable (nsamp)
c       bpar  - real*4 array containing 2nd "dependent" variable (nsamp)
c
c     output parameters... (all real*4)
c       alpha  - fitted apar index 
c       a_err  - associated error in alpha
c       beta   - fitted bpar index
c       b_err  - associated error in beta
c       gamma  - fitted constant term 
c       g_err  - associated error in gamma
c
c       mxb 91
c       drl 2/92
c
      external plaw
      integer nsamp, counter, nsamp2, npar, ndatmax, iw
      real ivar(nsamp),apar(nsamp),bpar(nsamp)
      real*8 expnd,decr
c
      parameter(npar=3,ndatmax=2500,iw=npar*(npar+11)/2)
      parameter(expnd=2.0,decr=0.4)
c
      double precision x(2,ndatmax),y(ndatmax),grad(npar),
     &   rplane(ndatmax),se(npar),res(ndatmax),wk(iw),par(npar)
      real*8 sumsq,tol
      integer icon(npar),iwk(ndatmax),i,its,iprint,lumon,ier
c
c     starting values for the fit
c
      par(1)= gamma
      par(2)= alpha
      par(3)= beta
      do i=1,3
        icon(i)=-1
      end do
c
c     vet the data for -999.0 values in pdots ls for
c     the real pulsar case
c
      counter = 1
      do i=1,nsamp
	if (bpar(i).ne.-999.0.and.ivar(i).ne.-999.0) then
          x(1,counter)=apar(i) 
          x(2,counter)=bpar(i) 
          y(counter)=ivar(i) 
	  counter=counter+1
	end if
      end do
      nsamp2 = counter-1
c
c     initialize lmm parameters before fitting...
c
      SUMSQ=0
      ITS=30
      tol=0.001
      iprint=-1
      lumon = 6
c
c     fit. see lmm.f for details
c
      call lmm(par,grad,sumsq,nsamp2,npar,tol,expnd,decr,its,iprint,
     &   lumon,se,res,rplane,wk,iw,iwk,icon,plaw,x,2,y,ier)
c
c     output fitted values with errors
c
      gamma = par(1)
      g_err = se(1)
      alpha = par(2)
      a_err = se(2)
      beta  = par(3)
      b_err = se(3)
      write(*,*) '-------------------------------------------'
      write(*,*) 'Results of least squares lmm fit...'
      write(*,'(x,a,f7.3,a,f7.3)') 'Gamma ',gamma,' +/- ',g_err
      write(*,'(x,a,f7.3,a,f7.3)') 'Alpha ',alpha,' +/- ',a_err
      write(*,'(x,a,f7.3,a,f7.3)') 'Beta  ',beta ,' +/- ',b_err
      write(*,*) '-------------------------------------------'
      end
c----------------------------------------------------------------
        subroutine plaw(i,x,nx,y,wt,p,np,e,grad,icon,ifl)
c----------------------------------------------------------------
c
c     Routine used by lmm to evaluate the power law we
c     are fitting to.
c
      implicit none
      integer  nx,i,np,ifl,icon(np)
      real*8   x(nx),y,p(np)
      real*8   wt,e,grad(np)

      e = log10(y) - log10( p(1)* (x(1)** p(2)) * (x(2)**p(3)) )
      wt=1d0

      end
