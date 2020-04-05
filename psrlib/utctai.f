*DECK UTCTAI
C
C
C **************************************************************
      double precision FUNCTION UTCTAI (XJD)
C **************************************************************
C
C RETURNS THE INCREMENT TO BE ADDED TO UTC TO GIVE TAI AT
C      XJD = MODIFIED JULIAN DATE + FRACTION OF DAY IN UTC
C THIS IS AN INTEGER NUMBER OF SECONDS COMPRISING A POSITIVE LEAP SECOND
C ON EACH OF THE DATES TABULATED IN XLEAPS
C
C THE TABLE IS COMPLETE FROM JAN 1ST 1972 TO 1st JAN 2006
C
C                                         M.ASHWORTH    JUN 1983
c                                         r.s.pritchard jan 1994
C                                         caj           jan 1999
C                                         caj           jan 2006
c
      double precision xjd
      integer nleaps
      PARAMETER ( NLEAPS=27 )
      double precision XLEAPS(NLEAPS)
      DATA XLEAPS /
C         1972         JAN       JUL
     &                41317d0    ,41499d0
C         1973
     &               ,41683d0
C         1974
     &               ,42048d0
C         1975
     &               ,42413d0
C         1976
     &               ,42778d0
C         1977
     &               ,43144d0
C         1978
     &               ,43509d0
C         1979
     &               ,43874d0
C         1980
     &               ,44239d0
C         1981
     &                         ,44786d0
C         1982
     &                         ,45151d0
C         1983
     &                         ,45516d0
C         1985
     &                         ,46247d0
C         1988
     &               ,47161d0
C         1990
     &               ,47892d0
C         1991
     &               ,48257d0
C         1992
     &                        ,48804d0
C         
     &                        ,49169d0,49534d0,50083d0
             
     &               ,50630d0 
C         1999
     &               ,51179d0
C         2006
     &               ,53736d0

C         2009
     &               ,54832d0 
c         Jul 2012
     &               ,56109d0
c         Jul 2015
     &               ,57204d0/
c
C
      IF(XJD.LT.41316.0d0) THEN
C        BEFORE 1972.0 CONTINUOUS SCALE (BIH ANNUAL REPORT 1978)
         UTCTAI = 7.515378 + (XJD - 40400.0)*0.002592
      ELSE
C        ADD A LEAP SECOND STARTING FROM 9 SECS FOR EVERY TIME THAT XJD
C        EXCEEDS THE DATE IN XLEAPS
C
         JSECS = 9
         DO 10 I=1,NLEAPS
            IF ( XJD.GE.XLEAPS(I) ) JSECS = JSECS+1
   10    CONTINUE
         UTCTAI = dble(JSECS)
C
      ENDIF
      RETURN
C
C END OF REAL FUNCTION UTCTAI
C
      END






























