cDECK RTETXT 
c   
c   
c **************************************************************
c **************************************************************
c
c  RETURNS AN ERROR MESSAGE RELEVANT TO THE FORTRAN   
c  RUN-TIME ERROR NUMBER CONTAINED IN ISTAT  
c   
c
c THIS ROUTINE IS INSTALLATION DEPENDENT
c
c FX/FORTRAN VERSION 
c
      character *(*)function rtetxt(istat)
c
      character fmt*40, outbuf*50, gerror*63
c   
c  TEST THE VALUE OF ISTAT  
c   
      rtetxt = ' '
      if (istat .le. 0) then
      return 
      else
      rtetxt = gerror()
c   
      end if
c   
c  END OF CHARACTER FUNCTION RTETXT  
c   
      return 
      end
