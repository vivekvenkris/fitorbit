cDECK CURTIM
c   
c   
c   
c   
c RETURNS THE CURRENT TIME AS HHMMSS.  THIS ROUTINE IS INSTALLATION 
c DEPENDENT.
c     VERSION 1.0    PAUL HARRISON 4 -JAN 90 FX/FORTRAN 
c   
      subroutine curtim(retime)
c   
c FORM ITIME.   
c   
      integer*4 time(3), retime
      call itime(time)
      retime = ((time(1) * 10000) + (time(2) * 100)) + time(3)
c   
c FORMAT STATEMENT. 
c   
      return 
c   
c END OF SUBROUTINE CURTIM. 
c   
  800 format(i2,1x,i2,1x,i2)
      end
