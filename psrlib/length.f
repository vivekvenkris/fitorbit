c     DECK LENGTH
c     
c     
c     
c     
c     RETURNS THE LENGTH OF 'STRING' EXCLUDING ANY TRAILING SPACES.
c     Mod to find last printable char 16th nov 2001 caj
      integer function length(string)
      character string*(*)
c     
c     OBTAIN THE LOCATION OF THE LAST NON-SPACE CHARACTER.
c     
      integer ilen, ipos
c     search for the first null
      ilen = len(string)
c     use the position of the first null
      do 1 i = ilen, 1, -1
c     
c     LENGTH FOUND.
c     
         if (string(i:i) .gt. char(32))then
c     &        .and.string(i:i).le.char(0)) then
            length = i
            return 
         end if
c     
c     STRING IS ALL SPACES OR ZERO LENGTH.
c     
    1 continue
      length = 0
c     
c     END OF INTEGER FUNCTION LENGTH.
c     
      return 
      end










