*DECK GRAN
C
C ******************************************************************
      REAL FUNCTION GRAN ( A, S, INIT )
C ******************************************************************
C
C GENERATES GAUSSIAN RANDOM NOISE MEAN A, RMS S
C INIT SHOULD BE A LARGE ODD INTEGER
C
C
C THIS ROUTINE IS INSTALLATION DEPENDENT
C
C VAX-11 FORTRAN VERSION
C
      B = 0
      DO 10 J=1,12
         B = B+RAN(INIT)
   10 CONTINUE
      GRAN = (B-6.0)*S+A
      RETURN
C
C END OF REAL FUNCTION GRAN
C
      END
c==============================================================================
      function ran(idum)
c==============================================================================
c
c     Donald Knuth's portable random number generator, nabbed from p199
c     of numerical recipes. Gives same random numbers on both Sun's &
c     Hp's given the same initial seed. Pass idum down as negative to 
c     reshuffle the random numbers.
c
c     Last Change 93/06/13 DRL @ JB.
c
c      function ran3(idum)
c         implicit real*4(m)
c         parameter (mbig=4000000.,mseed=1618033.,mz=0.,fac=2.5e-7)
      parameter (mbig=1000000000,mseed=161803398,mz=0,fac=1.e-9)
      dimension ma(55)
      data iff /0/
      if(idum.lt.0.or.iff.eq.0)then
        iff=1
        mj=mseed-iabs(idum)
        mj=mod(mj,mbig)
        ma(55)=mj
        mk=1
        do 11 i=1,54
          ii=mod(21*i,55)
          ma(ii)=mk
          mk=mj-mk
          if(mk.lt.mz)mk=mk+mbig
          mj=ma(ii)
11      continue
        do 13 k=1,4
          do 12 i=1,55
            ma(i)=ma(i)-ma(1+mod(i+30,55))
            if(ma(i).lt.mz)ma(i)=ma(i)+mbig
12        continue
13      continue
        inext=0
        inextp=31
        idum=1
      endif
      inext=inext+1
      if(inext.eq.56)inext=1
      inextp=inextp+1
      if(inextp.eq.56)inextp=1
      mj=ma(inext)-ma(inextp)
      if(mj.lt.mz)mj=mj+mbig
      ma(inext)=mj
      ran=mj*fac

      end
