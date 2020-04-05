      real function psrran(seed)
c
c     random number generator for HP's
c     DRL 29-May-92 pass down seed
c     for generator as an integer*4
c     returns random number between
c     0.0 and 1.0
c
      integer*4 seed
      psrran = ran(seed)
      end

