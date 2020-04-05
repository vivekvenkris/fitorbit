cDECK UPPCASE
c
c **************************************************************
c **************************************************************
c
c CONVERT STRING TO UPPERCASE CAHARACTERS
c
c THIS ROUTINE IS INSTALLATION DEPENDENT
c
c VAX-11 FORTRAN VERSION
c
      character *(*)function uppcase(string)
c
c     USE THE SYSTEM PROVIDED STRING HANDLING ROUTINE STR$UPCASE
c
      character string*(*)
      character*500 lstring
      integer length
c
      lstring=string
c      call upcase(lstring, 1, 1)
c this one works caj oct 2010
      call upcase_works(lstring)
      uppcase = lstring(:len(string))
c
c END OF CHARACTER*(*) FUNCTION UPPCASE
c
      return 
      end
