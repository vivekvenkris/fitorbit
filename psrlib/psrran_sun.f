      real function psrran(seed)
c
c     random number generator for SUNS
c     DRL 29-May-92 pass down seed
c     for generator as an integer*4
c     returns random number between
c     0.0 and 1.0
c
      integer*4 seed
      logical first
      data first/.true./
c
c     seed doesn;t seem to
c     work on the suns, try
c     shuffling generator seed
c     times initially
c
      if (first) then
        seed=abs(seed)
        do while(seed.gt.0)
          psrran=rand(0)
          seed=seed-1
        enddo
        first=.false.
      endif

      psrran = rand(0)
      end
