C nicked from pgplot
C*GRGLUN -- get a Fortran logical unit number (Sun/Convex-UNIX)
C+
      INTEGER FUNCTION GETLUN()
      INTEGER LUN
C
C Get an unused Fortran logical unit number. 
C Returns a Logical Unit Number that is not currently opened.
C After GRGLUN is called, the unit should be opened to reserve
C the unit number for future calls.  Once a unit is closed, it
C becomes free and another call to GRGLUN could return the same
C number.  Also, GRGLUN will not return a number in the range 1-9
C as older software will often use these units without warning.
C
C Arguments:
C  LUN    : receives the logical unit number, or -1 on error.
C--
C 12-Feb-1989 [AFT/TJP].
c jan 2009 reduce highest lun to 20! caj
C-----------------------------------------------------------------------
      INTEGER I
      LOGICAL QOPEN
C---
      DO 10 I=20,10,-1
          INQUIRE (UNIT=I,  OPENED=QOPEN)
          IF (.NOT.QOPEN) THEN
              GETLUN = I
              RETURN
          END IF
   10 CONTINUE
C none left
      GETLUN = -1
      write(*,*)' GETLUN returns -1  - no LUNS left'
      END


      subroutine freelun(lun)
      integer lun
C should close maybe?
      end



