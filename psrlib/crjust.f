cDECK CRJUST
c
c **************************************************************
c **************************************************************
c
c RETURNS A RIGHT-JUSTIFIED FORM OF STRING
c
      character *(*)function crjust(string)
c
      character string*(*)
      l = max(len(crjust) - length(string),0)
      do 10 i = 1, l
      crjust(i:i) = ' '
   10 continue
c
      crjust(l + 1:) = string
c
c END OF CHARACTER FUNCTION CRJUST
c
      return 
      end
