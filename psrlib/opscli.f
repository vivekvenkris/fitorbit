      character *(*)function opscli()
      character cli*(*)
c
c     RETURN THE NAME
c
      parameter (cli = 'UNIX')
      opscli = cli
c
c END OF CHARACTER*(*) FUNCTION OPSCLI
c
      return 
      end
