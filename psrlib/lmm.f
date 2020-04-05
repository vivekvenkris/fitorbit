*DECK LMM
c
c
      subroutine lmm(p,grad,sumsq,nobs,np,tol,expnd,decr,its,iprint,
     1 lout,se,res,rplane,wk,iw,iwk,icon,eandg,x,nx,y,ier)
c
c subroutine for unconstrained non-linear least squares.
c attempts to minimize sum(e(i)**2) where e(i) is usually a difference
c between an observed value and the fitted value.
c programmed derivatives are not required, but convergence is usually
c quicker and more likely if derivatives are provided.
c not suitable for constrained minimization when the minimum lies on a
c constraint boundary.
c
c algorithm: levenberg-marquardt using planar rotations (hammarling's
c            method).   the test of convergence must be satisfied in
c            two consecutive iterations with the full gauss-newton step
c            being taken in the second of the two.
c
c warning: there is no guaranty that the solution found is a global
c          rather than a local minimum; it may even be a saddle point.
c
c written by d.r.jackett and a.j.miller
c csiro division of mathematics and statistics
c latest revision -  07 aug 1980.
c
c notes added by mike ashworth, jodrell bank.
c
c calling arguments:-
c p     = input, vector of starting parameter values
c         output, final estimates of parameters
c grad  = vector used to store derivatives
c         must have dimension equal to at least the number of
c         parameters in the calling program
c sumsq = output, residual sum of squares
c nobs  = input, number of observations
c np    = input, total number of parameters, including
c                any to be held constant
c tol   = input, convergence tolerance on sq.root(sumsq).
c                tolerance is relative if sumsq.gt.1.0,
c                otherwise it is an absolute tolerance.
c               if the standard deviation of the residuals is
c               expected to be greater than 1.0, tol = 0.001 will
c               usually be adequate, otherwise set tol = 0.01 times
c               the anticipated residual standard deviation.
c expnd = input, expansion factor.
c                recommended value = 2.0
c decr  = input, reduction factor.
c                recommended value = 0.4
c its   = input, maximum number of iterations to be performed.
c                usually 30 will be plenty.
c         output, actual number of iterations.
c iprint= input, iprint = -1 for no printing except error messages
c                       =  0 for printing after convergence
c                       =  1 print results of each iteration
c                       =  2 as above plus a gradient check
c lout  = input, logical unit for output
c se    = output, approximate standard errors of parameters
c res   = output, vector of least-squares residuals
c rplane= output, vector of planar rotation (recursive) residuals
c wk    = workspace, must be dimensioned at least npst*(npst+11)/2
c                 in the calling program, where npst = np - nconst.
c                 after successful convergence, and provided that the
c                 number of observations exceeds the number of parameters,
c                 an approximate covariance matrix for the parameter
c                 estimates is returned in wk.   this is stored in
c                 upper-triangular form starting at location iu where
c                 iu = 5*npst + 1.
c iw    = input, dimension of wk in calling program
c iwk   = integer workspace, must be dimensioned at least npst
c         in the calling program
c icon  = input, for parameter i,
c                icon(i) = -2 if central differences are to be used
c                             to estimate derivatives
c                        = -1 if differences are to be used instead
c                             of derivatives.  lmm decides when to
c                             switch to central differences
c                        =  0 if user provides derivatives in the
c                             subroutine e and g
c                        =  1 if the parameter is to be held constant
c eandg = input, the name of the user's subroutine for calculating
c                residuals (e's) and gradients (g's)
c                the name of this subroutine, which does not need to
c                be called eandg, must be declared in an external
c                statement in the calling program.
c x     = input, nx*nobs array of observations
c               n.b.it is not necessary to use x, data can be passed
c               to your subroutine in common
c nx    = input, first dimension of x in the calling program,
c                if x is doubly dimensioned, otherwise put nx=1.
c                nx is usually the number of x-variables.
c y     = input, vector of observations
c ier   = output, error indicator
c               ier = 0 no error
c                   = 1 expnd not greater than 1.0
c                   = 2 decr not between 0.01 and 0.99
c                   = 8 matrix of first derivatives singular
c                   = 16 failed to converge
c                   = 32 insufficient work space provided
c                   = 64 zero or negative no. of parameters
c                   = 128 eps reached 1000
c                   = any sum of the above if more than one error
c
c
c specifications for user's subroutine e and g
c the call is:-
c call e and g(i,x,nx,y,wt,p,np,e,grad,icon,ifl)
c where the arguments are as for lmm plus:-
c i     = observation number
c x     = the i-th value of x.  n.b. the dimension of x is nx,
c         not nx*nobs as for lmm
c y     = the i-th value of the y-variable
c wt    = weight to be given to current term which must be set
c         set wt=1.0 for unweighted fitting
c e     = the residual for the i-th observation which is to be
c         calculated in this subroutine
c grad  = vector of derivatives of e with respect to the parameters,
c         i.e. grad(i) = de/dp(i)
c         n.b. the gradients are of e, not of the 'fitted' values
c ifl   = 1  if both e and the gradients grad are required.
c            grad(i) is to be calculated only if icon(i)=0
c       = 2,3 or 4  if only the residual e is to be calculated.
c            in this case the vector grad must not be changed.
c
c n.b.  the user's subroutine must not change any values other than
c       those in e and grad, except that a negative value may be
c       returned for ifl if the parameter values in p violate some
c       constraint.  in this case, lmm takes a smaller step to
c       attempt to satisfy your constraint but may not be able to.
c       you must ensure that your starting point satisfies any
c       constraints.   if the minimum is on a constraint boundary, the
c       algorithm used will not correctly find it in general.
c
c subroutines (other than the user's s/r) called by lmm:-
c baksub,covar,lmmerr,hammar,invert
c
c***************************************************************************
c
      implicit double precision (a-h,o-z)
      dimension p(np),grad(np),se(np),wk(iw),icon(np),x(nx,nobs),
     1y(nobs),iwk(np),res(nobs),rplane(nobs)
c      data plb/" 3h p( "/
        data wt/1.0d0/
c
c        initialise
c
      maxits=its
      eps=0.5d0
      its=0
      ic=0
      ier=0
c
c         some checks
c
      nconst=0
      do 1 i=1,np
        if(icon(i).gt.0) nconst=nconst+1
        se(i)=1.0d0
        if(icon(i).le.0.or.iprint.eq.2) se(i)=p(i)*0.001d0
        if(se(i).eq.0.0d0) se(i)=1.0d-3
    1 continue
c
      npst=np-nconst
      iel=npst+1
      irhs=iel+npst
      ixsq=irhs+npst
      ib=ixsq+npst
      iu=ib+npst
      il=iu+(npst*(npst+1))/2-1
      if(iw.lt.il) call lmmerr(1,il,lout)
      if(iw.lt.il) ier=32
      if(expnd.le.1.0) call lmmerr(2,ier,lout)
      if(ier.eq.1) expnd=2.0d0
      if(decr.le.0.01d0.or.decr.gt.0.99d0) call lmmerr(3,ier,lout)
      if(ier.eq.3.or.ier.eq.2) decr=0.4d0
      if(npst.le.0) call lmmerr(4,ier,lout)
      if(ier.gt.15) return
      do 2 i=1,npst
    2 iwk(i) = i
c
c         gradient check on programmed derivatives.
c
      if(iprint.ne.2) go to 5
      write(lout,33)
      do 4 i=1,np
        if(icon(i).ne.0) go to 4
        do 3 j=1,nobs
          ifl=1
          call eandg(j,x(1,j),nx,y(j),wt,p,np,e,grad,icon,ifl)
          res(j) = grad(i)
          wk(1)=p(i)
          p(i)=p(i)+se(i)
          ifl=2
          call eandg(j,x(1,j),nx,y(j),wt,p,np,e1,grad,icon,ifl)
          rplane(j)=(e1-e)/se(i)
          p(i)=wk(1)
    3   continue
        write(lout,34) i
        do j = 1, nobs
        write(unit=lout, fmt=35) res(j), rplane(j)
        end do
    4 continue
    5 if(iprint.gt.0) write(lout,36)
      if(iprint.gt.0) write(lout,37) (' pl(',i,')',p(i),i=1,np)
c
c         start main cycle
c
    6 its=its+1
      niter=1
      nrows=0
      ressq=0.0d0
      sumsq=0.0d0
      il=0
      do 7 i=1,np
        if(icon(i).gt.0) go to 7
        il=il+1
        wk(il)=p(i)
    7 continue
c
c         calculate the gradients
c
      do 11 line=1,nobs
        ifl=1
        call eandg(line,x(1,line),nx,y(line),wt,p,np,e,grad,icon,ifl)
        il=0
        do 9 ip=1,np
          if(icon(ip).le.0) il=il+1
          if(icon(ip).ge.0) go to 9
          ifl=2
          if(icon(ip).eq.-2) go to 8
          p(ip)=p(ip)+se(ip)
          call eandg(line,x(1,line),nx,y(line),wt,p,np,e1,grad,icon,ifl)
          grad(ip)=(e1-e)/se(ip)
          p(ip)=wk(il)
          go to 9
    8     p(ip)=p(ip)+se(ip)
          call eandg(line,x(1,line),nx,y(line),wt,p,np,e1,grad,icon,ifl)
          ifl=3
          p(ip)=p(ip)-2*se(ip)
          call eandg(line,x(1,line),nx,y(line),wt,p,np,e2,grad,icon,ifl)
          grad(ip)=(e1-e2)/(2*se(ip))
          p(ip)=wk(il)
    9 continue
c
c         squeeze grad
c
      i1=0
      do 10 i=1,np
        if(icon(i).gt.0) go to 10
        i1=i1+1
        grad(i1)=grad(i)
   10 continue
c debug
c      write(*,*) grad(1)
c
c         update cholesky factorization
c
      call hammar(0,npst,grad,e,wk(iu),iw,wk(iel),wk(irhs),nrows,
     1wt,wk(ixsq),sumsq,ressq,wk(ib),rplane(line))
   11 continue
      if(iprint.gt.0.and.its.eq.1) write(lout,38) sumsq
      if(iprint.gt.0) write(lout,39) its
c
c         augment the diagonal elements unless ic=1
c
      if(ic.eq.1) go to 15
   12 do 14 i=1,npst
        e=0.0d0
        do 13 j=1,npst
   13   grad(j)=0.0d0
        grad(i)=eps
        wt=wk(ixsq+i-1)
        call hammar(0,npst,grad,e,wk(iu),iw,wk(iel),wk(irhs),nrows,
     1   wt,wk(ixsq),sumsq,ressq,wk(ib),temp)
   14 continue
c
c         calculate parameter changes
c
   15 do 16 i=1,npst
        wk(ixsq+i-1)=wk(ixsq+i-1)*1.0e-15
   16 continue
      call baksub(wk(iu),iw,wk(iel),iwk,npst,wk(irhs),wk(ixsq),
     1wk(ib),npst,ndep)
      if(ndep.gt.0) then
c drop out if things go wrong pah         
          call lmmerr(7,ier,lout)
          return
       endif
      il=ib
      do 17 i=1,np
        if(icon(i).eq.1) go to 17
        p(i)=p(i)-wk(il)
        il=il+1
   17 continue
c
c         calculate new sum of squares
c
      snew=0.0d0
      ifl=4
      do 18 line=1,nobs
        if(ifl.lt.0) go to 18
c       write(6,6660) line,res(line),e,wt
6660    format(i10,3g10.3)
        call eandg(line,x(1,line),nx,y(line),wt,p,np,e,grad,icon,ifl)
c       write(6,6660) line,res(line),e,wt
        if(ic.ne.0) res(line)=e*dsqrt(wt)
        snew=snew+e**2*wt
   18 continue
      if(ifl.lt.0) go to 20
      diff=sumsq-snew
c
c         calculate pred = the predicted reduction in sum of squares
c
      pred=0.0d0
      do 19 i=1,npst
        pred=pred+wk(iel+i-1)*wk(irhs+i-1)*wk(irhs+i-1)
   19 continue
      if(iprint.gt.0) write(lout,40) niter,eps,snew,ressq
      if(iprint.gt.0) write(lout,37) (' pl(',i,')',p(i),i=1,np)
c
c         test for satisfactory reduction in sum of squares
c
      if(diff.ge.pred*1.0d-4) go to 24
c
c         reduction not o.k., repeat if eps less than 1000
c
   20 eps=eps*expnd
      ic=0
      niter=niter+1
      if(eps.gt.1.0d3) call lmmerr(5,ier,lout)
      if(ier.gt.15) return
c
c         reset parameter values to those at start of iteration.
c         switch to central differences if parameter changes small
c
      il=0
      do 21 i=1,np
        if(icon(i).eq.1) go to 21
        il=il+1
        p(i)=wk(il)
   21 continue
      il=ib-1
      do 22 i=1,np
        if(icon(i).le.0) il=il+1
        if(icon(i).ne.-1.or.dabs(wk(il)).ge.dabs(se(i))) go to 22
        icon(i)=-2
        go to 6
   22 continue
      do 23 i=1,npst
   23   wk(ixsq+i-1)=wk(ixsq+i-1)*1.0d15
      go to 12
c
c         end of iteration, test for convergence
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
   24 if(pred.le.2.0d0*tol*dsqrt(sumsq*(1.0d0+snew))) go to 25
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if(its.ge.maxits) call lmmerr(6,ier,lout)
      if(ier.gt.15) return
      if(niter.eq.1) eps=eps*decr
      ic=0
      go to 6
c
c         convergence test satisfied
c         ic = count of no. of times test is satisfied
c         test must be satisfied twice consecutively
c
   25 if(ic.eq.1) go to 26
      eps=eps*decr
      if(eps.gt.1.0d0.and.its.le.maxits) go to 6
      ic=1
      if(iprint.gt.0) write(lout,41)
      if(its.le.maxits) go to 6
      call lmmerr(6,ier,lout)
      return
   26 continue
      sumsq=snew
c
c         converged: get standard errors and correlations
c
      if(nobs.le.npst) return
      var=sumsq/float(nobs-npst)
      call covar(wk(iu),iu,wk(iel),npst,npst,npst,iwk,
     1wk(ixsq),var,wk(iu),iw,ndep)
      if(ndep.gt.0) call lmmerr(7,ier,lout)
      il=iu
      do 27 i=1,npst
        se(i)=dsqrt(wk(il))
        il=il+npst+1-i
   27 continue
      if(iprint.lt.0) go to 31
      write(lout,42)
      il=1
      do 28 i=1,np
        if(icon(i).gt.0) go to 28
        write(lout,43) i,p(i),se(il)
        iwk(il)=i
        il=il+1
   28 continue
      write(lout,44)
      do 30 i=1,npst
        temp=1.0d0/se(i)
        il=iu+i-1
        do 29 j=1,i
          if(i.eq.j) wk(j)=1.0d0
          if(i.ne.j) wk(j)=wk(il)*temp/se(j)
          il=il+npst-j
   29     continue
        write(lout,45) iwk(i),(wk(j),j=1,i)
   30 continue
c
c         now put the standard errors in their correct locations
c
   31 temp=dsqrt(var)
      if(iprint.ge.0) write(lout,46) temp
      if(nconst.eq.0) return
      il=nconst
      i=np
   32 se(i)=se(i-il)
      if(icon(i).gt.0) il=il-1
      i=i-1
      if(i.gt.1.and.i.gt.il) go to 32
      return
c
c         format statements
c
   33 format(5x,'gradients from eandg - gradients from differences
     1  in brackets')
   34 format(//2x,13hparameter no.,i8/)
   35 format(4(1x,g11.4,2h (,g11.4,1h),7x))
   36 format(/50h1  nonlinear least squares by levenberg algorithm
     1 30hlmm version dated  07 aug 1980/4x,16hstarting point:-)
   37 format(4(a3,i2,1h),g14.6)/)
   38 format(/22x,14hinitial sumsq=,g14.6)
   39 format(/7h   its=,i3)
   40 format(8h  niter=,i3,5h eps=,g14.6,7h sumsq=,g14.6,
     112h pred sumsq=,g14.6)
   41 format(/20x,'evidence of convergence')
   42 format(/15x,'parameter estimates(approx.s.e.)'/)
   43 format(i10,g22.6,3h  (g10.4,1h))
   44 format(/20x,'correlations of parameter estimates'/)
   45 format(/i6,(10f12.5))
   46 format(/34h standard deviation of residuals =,g12.5/)
      end
        subroutine baksub(u,iu,el,list,nvar,rhs,tol,b,k,ndep)
c
c                                 s/r to solve triangular set
c                                 of k linear equations using the
c                                 first k rows and columns of u
c
c                                 arguments as for s/r init plus:
c
c                                 b()    = output, the solutions
c                                          i.e. the regression
c                                          coefficients
c                                 k      = input, the no. of
c                                          variables or equations
c                                 ndep   = output, no. of zero
c                                          diagonal elements in
c                                          first k rows of u
c
c                                 n.b. b must have dimension at
c                                 least k in the calling program.
c                                 if ndep is not zero the solution
c                                 is not unique.
c
c     latest revision - 6 march 1979
c
c*****************************************************************
c
        implicit double precision (a-h,o-z)
        dimension u(iu),rhs(k),tol(nvar),b(k),list(nvar),el(k)
        if(k.le.0) return
        ndep=0
        ipos=((k-1)*(nvar+nvar-k+2))/2+1
c
c                                 calculate b(k)
c
        l=list(k)
        if(el(k)*u(ipos)*u(ipos).gt.tol(l)) go to 10
        ndep=1
        b(k)=0.0d0
        go to 20
10      b(k)=rhs(k)/u(ipos)
20      if(k.eq.1) return
c
c                                 calculate other solutions by
c                                 back-substitution
c
        irow=k-1
30      ipos=ipos-1-nvar+k
        l=list(irow)
        j=ipos-k+irow
        if(el(irow)*u(j)*u(j).gt.tol(l)) go to 40
        ndep=ndep+1
        b(irow)=0.0d0
        ipos=j
        go to 60
40      sum=rhs(irow)
        j=k
50      sum=sum-b(j)*u(ipos)
        ipos=ipos-1
        j=j-1
        if(j.gt.irow) go to 50
        b(irow)=sum/u(ipos)
60      irow=irow-1
        if(irow.gt.0) go to 30
        end
      subroutine covar(u,iu,el,nrows,nvar,klast,list,tol,var,wk,iw,ndep)
c
c                                 s/r to calculate a covariance
c                                 matrix from first klast rows of u
c
c                                 arguments as for s/r init plus:
c                                 var   = input, an estimate of
c                                         the residual variance
c                                 wk()  = output, the covariance
c                                         matrix stored in upper-
c                                         triangular form
c                                 iw    = input, dimension of wk.
c                                         must be at least
c                                         klast*(klast+1)/2
c                                 ndep  = output, the rank
c                                         deficiency of the
c                                         covariance matrix
c
c     latest revision - 28 june 1979
c
c******************************************************************
c
      implicit double precision (a-h,o-z)
      dimension u(iu),el(nrows),wk(iw),list(nvar),tol(nvar)
c
c                                 check that the dimension of
c                                 wk is adequate
c
      if(iw+iw.lt.klast*(klast+1)) return
c
c                                 replace small diagonal elements
c                                 with zeros
c
      ipos=1
      do 5 i=1,nrows
      diag=u(ipos)
      l=list(i)
      if(diag*el(i)*diag.lt.tol(l)) u(ipos)=0.0d0
      ipos=ipos+nvar+1-i
    5 continue
c
c                                 invert triangular matrix u
c
      call invert(u,iu,klast,nvar,wk,ndep)
c
c                                 form uinv*inv(el)*uinv-transpose
c                                 *var, overwriting uinv
c
      lij=1
      do 30 irow=1,klast
      ljk=lij
      do 20 jcol=irow,klast
      sum=0.0d0
      lik=lij
      do 10 k=jcol,klast
      scale=el(k)
      if(scale.ne.0.0d0) sum=sum+(wk(lik)/scale)*wk(ljk)
      lik=lik+1
      ljk=ljk+1
   10 continue
      wk(lij)=sum*var
      lij=lij+1
   20 continue
   30 continue
        end
        subroutine hammar(iconst,nvar,x,y,u,iu,el,rhs,nrows,wt,
     1      xsq,ysq,ressq,wk,rplane)
c
c                                 routine to update an upper-
c                                 triangular factorization using
c                                 hammarling's algorithm.
c                                 formula(3.7) is used from:-
c                                 hammarling,s. a note on
c                                 modifications to the givens
c                                 plane rotation, j.inst.maths.
c                                 applics.13,215-218,1974.
c
c                                 iconst = 1 input, if a constant
c                                          is to be added in
c                                        = anything else, no
c                                          constant will be added
c                                 nvar   = input, no. of variables
c                                          incl.constant if nec.
c                                 x()    = input, array containing
c                                          one line of data
c                                 y      = input, value of
c                                          dependant variable
c                                 u()    = output, the upper
c                                          triangular factor
c                                 iu     = input, dimension of u
c                                        = nvar*(nvar+1)/2
c                                 el()   = output, the multipliers
c                                          for each row of u
c                                          (hammarling's l's)
c                                 rhs()  = output, the projections
c                                          of the dependant
c                                          variable y
c                                 nrows  = output, no.of rows in
c                                          triangular matrix u
c                                 wt     = input, weight for
c                                          current line of data
c                                 xsq()  = output, sums of squares
c                                          of x-variables
c                                 ysq    = output, sum of squares
c                                          of y
c                                 ressq  = output, residual sum
c                                          of squares
c                                 wk()   = work space, must be
c                                          dimensioned at least
c                                          nvar in calling program
c                                 rplane = output, planar-rotation
c                                          residual (same as the
c                                          recursive residuals of
c                                          brown, durbin & evans)
c
c     latest revision - 23 june 1980
c
c***************************************************************
c
      implicit double precision (a-h,o-z)
      dimension x(nvar),u(iu),el(nvar),rhs(nvar),xsq(nvar),wk(nvar)
      double precision large
      data small/1.0d-36/,eps/1.0d-15/
c
c                                 initialize nrows,ressq,tssq,
c                                 xsq and ysq on first call
c
      if(nrows.ne.0) go to 40
      ressq=0.0d0
      large=1.0d0/small
      reduce=dsqrt(small)
      do 10 i=1,nvar
   10 xsq(i)=0.0d0
      ysq=0.0d0
      rplane=0.0d0
c
c                                 eps = smallest quantity such that
c                                 (1.0d0+eps) is calculated as different
c                                 from 1.0d0.   epsq=100.d0*eps**2.
c
   20 if(1.0d0+eps.gt.1.0d0) go to 30
      eps=eps+eps
      go to 20
   30 epsq=100.0d0*eps**2
c
c                                 copy x into work space,
c                                 insert a '1' if iconst=1
c
   40 j=0
      if(iconst.ne.1) go to 50
      xsq(1)=xsq(1)+wt
      j=1
      wk(1)=1.0d0
   50 ilast=nvar-j
      do 60 i=1,ilast
      j=j+1
      temp=x(i)
      wk(j)=temp
      xsq(j)=xsq(j)+temp**2*wt
   60 continue
      ysq=ysq+y**2*wt
      w=wt
      yy=y
      jj=1
      if(nrows.eq.0) go to 160
c
c                                 update rows 1,2,...,nrows
c                                 and rhs.
c                                 jj=location of current
c                                 element in matrix u
c
      do 150 irow=1,nrows
      v1=wk(irow)
      if(v1.ne.0.0d0) go to 70
      jj=jj+nvar+1-irow
      go to 150
   70 u1=u(jj)
c
c                                 if u1=0 swap rows of u and wk
c
      el1=el(irow)
c
c     note: following statement changed from el1*u1**2
c           to avoid floating overflow.
c           may also get floating underflow.
c
      if(el1*u1*u1.ge.epsq*xsq(irow)) go to 90
      u(jj)=0.0d0
      temp=w
      w=el1
      el(irow)=temp
      do 80 i=irow,nvar
      temp=wk(i)
      wk(i)=u(jj)
      u(jj)=temp
      jj=jj+1
   80 continue
      temp=rhs(irow)
      rhs(irow)=yy
      yy=temp
      go to 150
c
c                                 planar rotation calculations
c
c
c     note: may get divide by zero here caused by one of
c           the gradients being zero.
c
   90 v1u1=v1/u1
      elvelu=w*v1u1/el1
      temp=1.0d0+elvelu*v1u1
c
c                                 test for underflow
c
      if(el1.ge.small*temp) go to 110
      rhs(irow)=rhs(irow)*reduce
      jlast=jj+nvar-irow
      do 100 jjj=jj,jlast
  100 u(jjj)=u(jjj)*reduce
      el(irow)=el(irow)*large
      go to 70
  110 if(dabs(w).ge.small*temp.or.w.eq.0.0) go to 130
      w = w*large
      do 120 i=irow,nvar
  120 wk(i)=wk(i)*reduce
      yy=yy*reduce
      v1=wk(irow)
      go to 70
c
c                                 perform update
c
  130 el(irow)=el1/temp
      w=w/temp
      u(jj)=u1+elvelu*wk(irow)
      jj=jj+1
      temp=rhs(irow)
      rhs(irow)=temp+elvelu*yy
      yy=yy-v1u1*temp
      if(irow.eq.nvar) go to 150
      ir1=irow+1
      do 140 i=ir1,nvar
      temp=u(jj)
      u(jj)=temp+elvelu*wk(i)
      wk(i)=wk(i)-v1u1*temp
      jj=jj+1
  140 continue
  150 continue
c
c                                 end of update cycle.
c                                 if nrows.lt.nvar, copy
c                                 remainder of wk into u
c
  160 if(nrows.ge.nvar) go to 180
      nrows=nrows+1
      el(nrows)=w
      do 170 i=nrows,nvar
      u(jj)=wk(i)
      jj=jj+1
  170 continue
      rhs(nrows)=yy
      return
c
c                                 nrows.ge.nvar, increment ressq
c
  180 rplane=dsqrt(dabs(w))*yy
      if(w.gt.0.0d0) ressq=ressq+rplane**2
      if(w.lt.0.0d0) ressq=ressq-rplane**2
      return
      end
      subroutine invert(u,iu,nrows,ncols,uinv,ndep)
c
c                                 s/r to calculate a generalized
c                                 inverse of an upper-triangular
c                                 matrix u.   the elements of u
c                                 are stored by rows in symmetric
c                                 storage mode.   if the no. of
c                                 columns (ncols) exceeds the no.
c                                 of rows (nrows) to be used, the
c                                 last (ncols-nrows) elements in
c                                 each row are ignored.   uinv
c                                 and u may occupy the same
c                                 locations if nrows=ncols.
c
c                                 arguments as described above
c                                 plus:
c                                 iu    = input, dimension of u
c                                 uinv  = output, inverse of u
c                                 ndep  = output, rank deficiency
c                                         of u.   negative value
c                                         indicates either nrows.
c                                         gt.ncols or nrows.le.0
c
c     latest revision - 28 april 1979
c
c****************************************************************
c
        implicit double precision (a-h,o-z)
      dimension u(iu),uinv(iu)
c
c                                 test that nrows.le.ncols
c                                 and nrows.gt.0
c
      ndep=-1
      if(nrows.gt.ncols) return
      if(nrows.le.0) return
      ndep=0
c
c                                 set locu,loci = positions of
c                                 element in row and column nrows
c                                 in u and uinv respectively
c
      loci=(nrows*(nrows+1))/2
      locu=loci
      if(ncols.gt.nrows) locu=locu+(nrows-1)*(ncols-nrows)
c
c                                 invert, starting at the bottom
c                                 corner
c
      irow=nrows
   10 udiag=u(locu)
      if(udiag.ne.0.0d0) go to 30
      ndep=ndep+1
      l=loci
      do 20 icol=irow,nrows
      uinv(l)=0.0d0
      l=l+1
   20 continue
      go to 60
   30 udiag=1.0d0/udiag
      uinv(loci)=udiag
      if(irow.eq.nrows) go to 60
      ir1=irow+1
        icol=nrows
50      lu=locu
      li=loci+icol-irow
      l=li
      sum=0.0d0
      do 40 j=ir1,icol
      lu=lu+1
      li=li+nrows+1-j
      sum=sum-u(lu)*uinv(li)
   40 continue
      uinv(l)=sum*udiag
        icol=icol-1
        if(icol.gt.irow) go to 50
   60 irow=irow-1
      if(irow.le.0) return
      locu=locu-ncols-1+irow
      loci=loci-nrows-1+irow
      go to 10
      end
      subroutine rplot(res,nres,stdev,ncol,iwk,lout)
c
c     s/r to produce 9-line plots of standardized residuals
c
c     res   = array of residuals to be plotted
c     stdev = standard deviation of residuals - supplied by user
c     ncol  = no. of residuals to be plotted per line,
c             recommend 60 for vdu's and 110 for line printers.
c             n.b. an overlap of 10 points per line is given
c     iwk   = integer workspace array.   must be dimensioned at least
c             ncol in the calling program
c     lout  = logical unit no. of the output device
c
c     latest revision - 21 june 1980
c
c***********************************************************************
c
        implicit double precision (a-h,o-z)
      dimension res(nres),iwk(ncol)
c     data igap,iplus,istar/"1h" ,"1h+","1h*"/
      data igap,iplus,istar/32,43,42/
      character*1 cgap,cplus,cstar
      data cgap,cplus,cstar/" " ,"+","*"/
      ncol=10*((ncol+9)/10)
      if(ncol.le.0.or.nres.le.0) return
      if(ncol.gt.120) ncol=120
      write(lout,900) stdev
  900 format(/20x,39hplot of standardized residuals (stdev =,g12.4,1h))
      istart=1
      iend=nres
   10 if(iend-istart.ge.ncol) iend=istart+ncol-1
      n=iend-istart+1
      nu=999
      nl=3
      xu=9.9d+37
      xl=3.0*stdev
      write(lout,910)
  910 format(/18h st.devs.from zero)
      do 30 irow=1,8
      j=1
      do 20 i=istart,iend
      iwk(j)=igap
      if(res(i).gt.xl.and.res(i).lt.xu) iwk(j)=istar
      j=j+1
   20 continue
      if(irow.eq.1) write(lout,920)(iwk(j),j=1,n)
  920 format(16h more than 3   !,120a1)
      if(irow.gt.1.and.irow.lt.8) write(lout,930) nu,nl,(iwk(j),j=1,n)
  930 format(i4,3h to,i4,5h    !,120a1)
      if(irow.eq.8) write(lout,940)(iwk(j),j=1,n)
  940 format(16h less than -3  !,120a1)
      xu=xl
      xl=xl-stdev
      nu=nl
      nl=nl-1
      if(irow.eq.7) xl=-9.9d+37
   30 continue
c
c     put +'s under each fifth residual and number every tenth
c
      do 40 i=1,n
      iwk(i)=igap
      if(i.eq.5*(i/5)) iwk(i)=iplus
   40 continue
      write(lout,950)(iwk(i),i=1,n)
  950 format(16x,120a1)
      n1=10*(istart/10+1)
      n2=10*(iend/10)
      write(lout,960)(i,i=n1,n2,10)
  960 format(16x,12i10/)
c
c     end of one output block.
c     if there are remaining residuals, repeat last 10 in next block
c
      if(iend.ge.nres) return
      istart=iend-9
      iend=nres
      go to 10
      end

      subroutine lmmerr(ind,irr,lout)
      write(lout,1) ind
    1 format(' error no.',i3)
      go to (11,22,33,44,55,66,77) ind
   11 write(lout,12)
   12 format(' nconst is inconsistent with icon, nconst will be',
     1   ' reset ')
      return
   22 write(lout,23)
   23 format(' expnd must be greater than 1, reset to 1.6')
      irr=2
      return
   33 write(lout,34)
   34 format(' decr must be between 0.01 and 0.99, reset to 0.4')
      irr=3
      return
   44 write(lout,45)
   45 format(' zero or negative no. of parameters, check icon & np')
      irr=111
      return
   55 write(lout,56)
   56 format(' eps has reached a thousand, gradients may be wrong or'
     1  ' tol may be too small')
      irr=111
      return
   66 write(lout,67)
   67 format(' failed to converge in no. of iterations allowed')
      irr=111
      return
   77 if(irr.eq.7) return
      write(lout,78)
   78 format(' warning - your matrix of first derivatives is singular')
      irr=7
      return
      end

      double precision function lmmcov(ipar1, ipar2, np, icon,  wk)

C function to return the covariance of ipar1 and ipar2
C Paul Harrison 29-APR-1991 (might not be correct)

      integer ipar1,ipar2, np
      integer icon(np)
      double precision wk(*)

      integer i, ic, ic1, ic2, it

c first check that the parameters were fitted
      if(icon(ipar1).eq.1 .or. icon(ipar2).eq.1) then

C if not then return -999
         lmmcov=-999
         return
      endif

      ic=0
      ic1=0
      ic2=0
      i=1
      do while ((ic1.eq.0 .or. ic2.eq.0).and.i.le.np)
        if(icon(i).ne.1) ic=ic+1
        if(i.eq.ipar1) ic1=i
        if(i.eq.ipar2) ic2=i
      enddo

      if(ic1.lt.ic2) then
         it=ic2
         ic2=ic1
         ic1=it
      endif
        
      lmmcov=wk((ic1*(ic1-1))/2+ic2)


      end














