cDECK LENGTH
cDECK LENGTH
c
c
c
c
c
c
c
c
c RETURNS THE LENGTH OF 'STRING' EXCLUDING ANY TRAILING SPACES.
c RETURNS THE LENGTH OF 'STRING' EXCLUDING ANY TRAILING SPACES.
c
c
      function length(string)
      function length(string)
      character string*(*)
      character string*(*)
c
c
c OBTAIN THE LOCATION OF THE LAST NON-SPACE CHARACTER.
c OBTAIN THE LOCATION OF THE LAST NON-SPACE CHARACTER.
c     
c     
      integer ilen, ipos
      integer ilen, ipos
c search for the first null
c search for the first null
      ilen = len(string)
      ilen = len(string)
c use the position of the first null
c use the position of the first null
      ipos = index(string,char(0))
      ipos = index(string,char(0))
      if (ipos .ne. 0) ilen = min(ilen,max(ipos - 1,1))
      if (ipos .ne. 0) ilen = min(ilen,max(ipos - 1,1))
      do 1 i = ilen, 1, -1
      do 1 i = ilen, 1, -1
c
c
c LENGTH FOUND.
c LENGTH FOUND.
c
c
      if (string(i:i) .ne. ' ') then
      if (string(i:i) .ne. ' ') then
      length = i
      length = i
      return 
      return 
      end if
      end if
c
c
c STRING IS ALL SPACES OR ZERO LENGTH.
c STRING IS ALL SPACES OR ZERO LENGTH.
c
c
    1 continue
    1 continue
      length = 0
      length = 0
c
c
c END OF INTEGER FUNCTION LENGTH.
c END OF INTEGER FUNCTION LENGTH.
c
c
      return 
      return 
      end
      end
