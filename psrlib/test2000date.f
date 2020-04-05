      program test2000date
c
      real*8 mjd,mjd_new,dt,datejd
      integer imjd,tninp,tjinp
      character cdate*8,date*8,ctime*24,cjtime*24
      character crtims*24,tcmd(10)*30
      logical ldate,ltime,cmdjt
c
      write (*,*)
      do i=51543,51544
         mjd = i+0.55
c
         imjd = jddate(mjd)
         date = cdate(imjd)
         ctime = cjtime(mjd,6)
c     
         write (*,*)'JDDATE'
         write (*,*)'    MJD  ->  Idate'
         write (*,'(f10.2,i9)')mjd,imjd
         write (*,*)'CDATE'
         write (*,*)'  Idate  ->  Cdate'
         write (*,'(i9,a10)')imjd,date
         write (*,*)'CMDJT'

c         write (*,'(f10.2,i9,4x,a8,4x,a24)')mjd,imjd,date,ctime
c         write (*,*)'         Date                 ->'
c     &       //'           MJD'
c
         tcmd(1) = ctime(1:8)
         tcmd(2) = ctime(10:24)
c
         tninp = 2
         tjinp = 1
         if(.not.cmdjt(tcmd,tninp,tjinp,ldate,ltime,mjd_new))then
            write(*,*)'Error in date'
         endif
c
         write (*,'(a24,4x,f24.12)')ctime,mjd_new
c
         write(*,*)
      enddo
c
c      dt=datejd(imjd)
c      write(*,'(i10,4x,f8.2)')imjd,dt




 999  end
c
c
c ***************************************************************
c ***************************************************************
c
c RETURNS THE MODIFIED JULIAN DATE XJD CONVERTED INTO CALENDAR DATE
c    AS YYMMDD
c
      integer function jddate(xjd)
      double precision xjd
      dimension monthd(12)
c
      data monthd / 31, 28, 31, 30, 31, 30, 2*31, 30, 31, 30, 31 /
      nd = int(xjd) + 678957
      iyr = nd / 365.25
      if (iyr .lt. 0) then
      jddate = 0
      return 
      end if
      nd = nd - int((iyr * 365.25) + 0.9)
      if (nd .eq. 0) iyr = iyr - 1
      if (mod(iyr,4) .eq. 0) then
      monthd(2) = 29
      ndyr = 366
      else
      monthd(2) = 28
      ndyr = 365
      end if
      if (nd .eq. 0) nd = ndyr
      do 10 imn = 1, 12
      if (nd .le. monthd(imn)) goto 100
      nd = nd - monthd(imn)
   10 continue
  100 continue
c  The next line produces faulty output
      jddate = (((iyr - 1900) * 10000) + (imn * 100)) + nd
c
c END OF INTEGER FUNCTION JDDATE
c
      return 
      end
c
C
      CHARACTER*(*) FUNCTION CDATE(IDATE)
C
C RETURNS THE DATE IDATE AS AN INTEGER YYMMDD INTO THE CHARACTER STRING
C CDATE IN THE FORMAT YY/MM/DD.
C
      CHARACTER*8 BUFFER
C
C SPLIT IDATE INTO YY, MM AND DD.
C
c      I=MOD(ABS(IDATE),999999)  !this line is faulty
      I=MOD(ABS(IDATE),1000000)
      IYR=I/10000
      I=I-IYR*10000
      IMN=I/100
      IDY=I-IMN*100
      WRITE (BUFFER,100) IYR,IMN,IDY
      CDATE = BUFFER
      RETURN
C
C FORMAT.
C
  100 FORMAT (SS,I2.2,'/',I2.2,'/',I2.2)
C
C END OF CHARACTER*8 FUNCTION CDATE.
C
      END
C ****************************************************************
      CHARACTER*(*) FUNCTION CJTIME ( DJT, IDEC )
C ****************************************************************
C
C FORMS A CHARACTER STRING FROM THE MODIFIED JULIAN TIME
C     ( JULIAN DATE - 2400000.5 + UT AS FRACTION OF A DAY )
C     IN DJT WITH IDEC DECIMAL PLACES OF SECONDS
C     FORMAT IS 'YY/MM/DD HH:MM:SS.SS...SS'
C
      DOUBLE PRECISION DJT
      CHARACTER CDATE*8,CRTIMS*20
C
      CJTIME = ' '
C
C     IF NEGATIVE, THE JULIAN TIME IS REGARDED AS NOT SET
C
      IF ( DJT.LT.0.0 ) THEN
         CJTIME = '<none>'
         RETURN
      ENDIF
C
C     OBTAIN DATE FROM JULIAN TIME
C
      IDATE = JDDATE(DBLE(INT(DJT)))
C
C     ADD TO CHARACTER BUFFER
C
      IF ( IDATE.GT.0 ) THEN
         CJTIME = CDATE(IDATE)
         K = 10
      ELSE
         K = 1
      ENDIF
C
C     ADD TIME PART
C
      CJTIME(K:) = CRTIMS( MOD(DJT,1.D0)*86400.0,IDEC )
C
      RETURN
C
C END OF CHARACTER FUNCTION CJTIME
C
      END
c
c RETURNS A CHARACTER STRING REPRESENTING A TIME IN SECONDS
c     IN THE FORM HH:MM:SS.SSS...SSS
c     IDEC SPECIFIES THE NUMBER OF DECIMAL PLACES.
c
      character *(*)function crtims(tim, idec)
      character fmt*20, ctimes*8, buf*40
c
c     TAKE MODULUS ONE DAY
c
      double precision t, tim
c
c     WRITE TO BUFFER AND READ BACK TO ENSURE CORRECT ROUNDING
c
      t = mod(tim,86400.0d0)
      write(unit=fmt, fmt=100) idec + 6, idec
      write(unit=buf, fmt=fmt) t
      if (idec .gt. 0) then
      write(unit=fmt, fmt=110) idec + 1, idec
      read(unit=buf, fmt=fmt) it, t
      else
      read(unit=buf, fmt=120) it
c
c     WRITE THE TWO PARTS SEPARATELY
c
      end if
      crtims = ctimes(it)
      if (idec .gt. 0) then
      write(unit=fmt, fmt=100) idec + 1, idec
      write(unit=crtims(9:), fmt=fmt) t
c
      end if
c
      return 
  100 format(2h(F,i3,1h.,i3,1h))
  110 format(5h(I5,F,i3,1h.,i3,1h))
c
c END OF CHARACTER FUNCTION CRTIMS
c
  120 format(i5)
      end
C
      CHARACTER*8 FUNCTION CTIMES(ITIME)
C
C RETURNS THE TIME ITIME AS INTEGER SECONDS INTO THE CHARACTER STRING
C CTIMES IN THE FORMAT HH:MM:SS.
C
      CHARACTER*8 CTIMEH
C
C FORM STRING.
C
      CTIMES=CTIMEH(ISTHMS(ITIME))
      RETURN
C
C END OF CHARACTER*8 FUNCTION CTIMES.
C
      END
C
      CHARACTER*8 FUNCTION CTIMEH(ITIME)
C
C RETURNS THE TIME ITIME AS AN INTEGER HHMMSS INTO THE CHARACTER STRING
C CTIMEH HH:MM:SS.
C
      CHARACTER BUFFER*8
C
C SPLIT ITIME INTO HH, MM AND SS.
C
      I=MOD(ABS(ITIME),240000)
      IHR=I/10000
      I=I-IHR*10000
      IMN=I/100
      ISC=I-IMN*100
      WRITE (BUFFER,100) IHR,IMN,ISC
      CTIMEH = BUFFER
      RETURN
C
C FORMAT.
C
  100 FORMAT (SS,I2.2,':',I2.2,':',I2.2)
C
C END OF CHARACTER*8 FUNCTION CTIMEH.
C
      END
C
      FUNCTION ISTHMS(ITIME)
C
C TURNS THE INTEGER TIME ITIME FROM SECONDS INTO INTEGER HHMMSS.
C
C EVALUATE ISTHMS.
C
      I=MOD(ITIME,86400)
      J=I/3600
      K=(I-J*3600)/60
      ISTHMS=6400*J+40*K+I
      RETURN
C
C END OF INTEGER FUNCTION ISTHMS.
C
      END
c
      logical function cmdjt(cmd, ncmd, icmd, date, time, djt)
      character cmd(*)*(*)
      logical date, time
c
c     DECLARE EXTERNAL ROUTINES.
c
      double precision djt, dval, tim, dte, datejd
c
c     SET RETURN FLAG AND DATE/TIME FLAGS TO FALSE.
c
      logical partim, pardat, pardbl
      cmdjt = .false.
      date = .false.
c
c     RETURN IF NO PARAMETERS WERE PROVIDED.
c
      time = .false.
c
c     SPLIT DJT INTO DATE AND TIME.
c
      if (icmd .gt. ncmd) return 
      if (djt .gt. 0.0) then
      dte = int(djt)
      tim = mod(djt,1.d0) * 86400.0
      else
      dte = 0
      tim = 0
c
c     EXAMINE FIRST PARAMETER.
c
      end if
c
c        DATE FOUND.
c
      if (pardat(cmd(icmd),idte)) then
      dte = datejd(idte)
      date = .true.
c
c        TIME FOUND.
c
      else if (partim(cmd(icmd),tim)) then
      time = .true.
c
c        REAL JULIAN TIME FOUND.
c
      else if (pardbl(cmd(icmd),dval)) then
      dte = dval
      tim = 0
      time = .true.
      date = .true.
c
c        '<NONE>' INDICATES FLAGGED OFF TIME.
c
      else if (cmd(icmd)(1:1) .eq. '<') then
      djt = -1.0
c
c        NOT RECOGNIZED, RETURN.
c
      else
      return 
c
c     INCREMENT COMMAND POINTER.
c
      end if
c
c     IF A DATE OR A TIME WAS FOUND EXAMINE SECOND COMMAND, IF PRESENT,
c     FOR A TIME OR A DATE.
c
      icmd = icmd + 1
      if ((date .or. time) .and. (icmd .le. ncmd)) then
c
c           DATE FOUND, INCREMENT THE COMMAND POINTER.
c
      if (pardat(cmd(icmd),idte)) then
      dte = datejd(idte)
      date = .true.
      icmd = icmd + 1
c
c           TIME FOUND.
c
      else if (partim(cmd(icmd),tim)) then
      time = .true.
      icmd = icmd + 1
      end if
c
c     IF DATE OR TIME WAS FOUND, REFORM DJT.
c
      end if
      if (date .or. time) then
      djt = dte + (tim / 86400.0d0)
c
c     SUCCESS.
c
      end if
c
      cmdjt = .true.
c
c END OF LOGICAL FUNCTION CMDJT
c
      return 
      end
c
*DECK PARTIM
C
C
C
      LOGICAL FUNCTION PARTIM(PAR,TIME)
C
C TRIES TO INTERPRET THE PARAMETER STRING PAR AS A TIME.  RETURNS THE
C VALUE .TRUE. IF SUCCESSFUL AND .FALSE. OTHERWISE.  ON EXIT TIME CONTAINS
C THE TIME IN SECONDS IF PARTIM IS .TRUE. AND IS UNCHANGED OTHERWISE.
C
      CHARACTER PAR*(*),STR(2)*2
      LOGICAL PARDBL
      DOUBLE PRECISION TIME,SECS
C
C SET UP THE INITIAL VALUES.
C
      PARTIM=.FALSE.
      if (par(1:).eq.'<') return
      DO 1 I=1,2
    1 STR(I)=' '
      J=1
      IC=0
C
C SCAN THROUGH PAR AND ATTEMPT TO LOAD STR WITH ITS COMPONENT BITS.
C
      LP = LENGTH(PAR)
      DO 2 I=1,LP
        IF(PAR(I:I).EQ.':') THEN
C
C COLON, INCREMENT THE HMS POINTER.
C
          J=J+1
          IF(J.GT.2) THEN
C
C START OF THE SECONDS PART.
C
            GOTO 1001
          ENDIF
C
C CLEAR THE CHARACTER COUNTER.
C
          IC=0
        ELSEIF(PAR(I:I).NE.' ') THEN
C
C CHARACTER OTHER THAN SPACE, LOAD INTO STR.
C
          IC=IC+1
          IF(IC.GT.2) THEN
C
C STRING TOO LONG, NOT A TIME.
C
            RETURN
          ENDIF
C
C LOAD INTO STR.
C
          STR(J)(IC:IC)=PAR(I:I)
        ENDIF
    2 CONTINUE
C
C NOW ATTEMPT TO READ THE SECONDS PART AS A REAL NUMBER.
C
 1001 IF (I.GE.LP) THEN
C
C NO SECONDS SUPPLIED.
C
        SECS=0.0
      ELSEIF (.NOT.PARDBL(PAR(I+1:),SECS)) THEN
C
C NOT A REAL NUMBER.
C
        RETURN
      ENDIF
C
C CHECK THE SIGN AND MAGNITUDE OF SECS.
C
      IF ( SECS.LT.0.0.OR.SECS.GT.60.0 ) RETURN
C
C NOW ATTEMPT TO READ STR AS INTEGERS.  EXIT TO 1000 IF THE READ FAILS.
C
      IF(J.NE.1) THEN
        READ(STR,800,ERR=1000) I,J
C
C CHECK THE HM IN I AND J.
C
        IF(J.LT.0.OR.J.GT.59) RETURN
        IF(I.LT.0.OR.I.GT.24
     &     .OR.I.EQ.24.AND.(J.NE.0.OR.SECS.NE.0.0)) RETURN
C
C STRING IS A TIME, RETURN AS SECONDS.
C
        TIME=3600*I+60*J+SECS
        PARTIM=.TRUE.
      ENDIF
 1000 RETURN
C
C FORMAT STATEMENT.
C
  800 FORMAT(BN,2(I2:/))
C
C END OF LOGICAL FUNCTION PARTIM.
C
      END

*DECK PARDAT
C
C
C
      LOGICAL FUNCTION PARDAT(PAR,IDATE)
C
C TRYS TO INTERPRET THE PARAMETER STRING PAR AS A DATE.  RETURNS THE
C VALUE .TRUE. IF SUCCESSFUL AND .FALSE. OTHERWISE.  ON EXIT IDATE
C CONTAINS THE DATE AS YYMMDD IF PARDAT IS .TRUE. AND IS UNCHANGED OTHERWISE.
C
      CHARACTER PAR*(*),DIGITS*(*),MLIST(13)*9
      LOGICAL PARINT
      PARAMETER (DIGITS='0123456789+')
      SAVE MLIST
      DATA MLIST/'JANUARY','FEBRUARY','MARCH','APRIL','MAY','JUNE',
     &           'JULY','AUGUST','SEPTEMBER','OCTOBER','NOVEMBER',
     &           'DECEMBER',' '/
C
C SET THE INITIAL VALUES.
C
      PARDAT=.FALSE.
      if (par(1:1).eq.'<') return
C
C REMOVE ANY EMBEDDED SPACES IN THE PARAMETER.
C
      LP=LENGTH(PAR)
      I=1
 1000 IF(I.LT.LP) THEN
        IF(PAR(I:I).EQ.' ') THEN
C
C REMOVE THIS SPACE.
C
          LP=LP-1
          DO 1 J=I,LP
            PAR(J:J)=PAR(J+1:J+1)
    1     CONTINUE
        ELSE
C
C TEST THE NEXT CHARACTER.
C
          I=I+1
        ENDIF
        GOTO 1000
      ENDIF
C
C NOW ATTEMPT TO INTERPRET THE DATE IN DAY_MONTH_YEAR FORMAT.
C FIRST TRY TO LOCATE THE START OF THE MONTH.
C
      DO 2 I=2,LP
C
C IS THE CURRENT CHARACTER A DIGIT?
C
        IF(INDEX(DIGITS,PAR(I:I)).EQ.0) THEN
C
C NO, CHECK IF IT IS A '_'.
C
          IF(PAR(I:I).EQ.'_') THEN
C
C IT IS, SKIP IT.
C
            K=I+1
          ELSEIF(PAR(I:I).EQ.'/') THEN
C
C THE CHARACTER IS '/', TRY TO INTERPRET THE DATE AS THE
C FORMAT YY/MM/DD.
C
            CALL CHKYMD(PAR,LP,PARDAT,IDATE)
            if (idate.lt.600000) then
               idate = idate + 1000000
            endif
            RETURN
          ELSE
C
C IT IS SOMETHING ELSE, THE MONTH STARTS HERE.
C
            K=I
          ENDIF
C
C NOW TRY TO LOCATE THE START OF THE YEAR.
C
          GOTO 1001
        ENDIF
    2 CONTINUE
C
C NO MONTH WAS FOUND, THE PARAMETER IS NOT A DATE, EXIT
C
      RETURN
C
C FOUND THE MONTH, ATTEMPT TO LOCATE THE START OF THE YEAR.
C
 1001 DO 3 L=K+1,LP
C
C IS THE CURRENT PARAMETER A DIGIT?
C
        IF(INDEX(DIGITS,PAR(L:L)).NE.0) THEN
C
C YES, THE YEAR STARTS HERE.
C
          J=L
          GOTO 1002
        ELSEIF(PAR(L:L).EQ.'_') THEN
C
C IT IS A '_', THE YEAR STARTS AT THE NEXT CHARACTER.
C
          J=L+1
          GOTO 1002
        ENDIF
    3 CONTINUE
C
C NO YEAR COULD BE FOUND, THE PARAMETER IS NOT A DATE, EXIT.
C
      RETURN
C
C IS THE MONTH A REAL UNAMBIGUOUS ONE?
C
 1002 IM=INTCMD(MLIST,PAR(K:L-1))
      IF(IM.GT.0) THEN
C
C YES, IS THE DAY PART AN INTEGER?
C
        IF(PARINT(PAR(1:I-1),ID)) THEN
C
C IT IS (IT BETTER HAD BE); IS THE YEAR AN INTGER?
C
          IF(PARINT(PAR(J:LP),IY)) THEN
C
C IT IS, TEST THE RANGE OF THE YEAR.
C
            IF(IY.GE.1950.AND.IY.LE.2049) THEN
C
C FORGET THE MILLENIUM.
C
              IY=MOD(IY,100)
            ENDIF
C
C NOW WE HAVE THE DAY, MONTH, AND THE YEAR AS INTEGERS TEST IF
C THEY ARE CONSISTENT AND WITHIN RANGE.
C
            CALL CHKDMY(ID,IM,IY,PARDAT,IDATE)
          ENDIF
        ENDIF
      ENDIF
      RETURN
C
C END OF LOGICAL FUNCTION PARDAT.
C
      END
*DECK PARDBL
C
C
C
      LOGICAL FUNCTION PARDBL(PAR,VAL)
C
C ATTEMPTS TO INTERPRET THE PARAMETER STRING PAR AS A REAL VALUE.
C RETURNS THE VALUE .TRUE. IF SUCCESSFUL AND .FALSE. OTHERWISE.  ON EXIT
C VAL CONTAINS THE REAL VALUE IN DOUBLE PRECISION IF PARDBL IS .TRUE.
C AND IS UNCHANGED OTHERWISE.
C
      CHARACTER PAR*(*),FMT*10
      DOUBLE PRECISION VAL,V
C
C SET UP THE INITIAL VALUES.
C
      PARDBL=.FALSE.
      if (par(1:).eq.'<') return
C
C SCAN THROUGH PAR TO LOCATE THE FIRST NON-SPACE CHARACTER.
C
      VAL=0.0
      IF(PAR.EQ.'') RETURN
      LP=LENGTH(PAR)
      DO 1 I=1,LP
        IF(PAR(I:I).NE.' ') GOTO 1000
    1 CONTINUE
      RETURN
C
C EVALUATE THE NUMBER OF CHARACTERS IN THE PARAMETER.
C
 1000 NC=LP-I+1
C
C FRIG THE FORMAT STATEMENT TO GIVE THIS FIELD SIZE.
C
      WRITE(FMT,800) NC
C
C NOW ATTEMPT TO READ THE PARAMETER AS A REAL.
C
      READ(PAR(I:LP),FMT,ERR=100,IOSTAT=NC) V
C
C READ SUCCESSFUL.
C
      PARDBL=.TRUE.
      VAL=V
      RETURN
C
C READ UNSUCCESSFUL.
C
  100 CONTINUE
      RETURN
C
C FORMAT STATEMENT.
C
  800 FORMAT(SS,'(BN,E',I2,'.0)')
C
C END OF LOGICAL FUNCTION PARDBL.
C
      END


cDECK DATEJD
c  
c  ******************************************************************
c  ******************************************************************
c  
c  RETURNS THE DATE IDAT ( YYMMDD ) CONVERTED TO A TRUNCATED JULIAN
c  DATE
c  
      double precision function datejd(idat)
      integer monthd(12)
c  
      data monthd / 31, 28, 31, 30, 31, 30, 2*31, 30, 31, 30, 31 /
      iyr = int(idat / 10000) + 1900
c this mod needed
c      if (mod(idat/10000,100).le.49) iyr = iyr +100
c or to make it more general purpose
c     iyr = mod(idat / 10000,1000)+ 1900
c
      imn = mod(idat / 100,100)
      idy = mod(idat,100)
      nd = iyr * 365.25
      if (mod(iyr,4) .eq. 0) then
         monthd(2) = 29
         nd = nd - 1
      else
         monthd(2) = 28
      end if
      do 10 i = 1, imn - 1
         nd = nd + monthd(i)
 10   continue
      datejd = DBLE((nd + idy) - 678956)
c  
c  END OF REAL FUNCTION DATEJD
c  
      return 
      end
c     DECK LENGTH
c     
c     
c     
c     
c     RETURNS THE LENGTH OF 'STRING' EXCLUDING ANY TRAILING SPACES.
c     
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
         if (string(i:i) .ne. char(32) .and.
     &       string(i:i).ne.char(0)) then
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










*DECK CHKDMY
C
C
C
      SUBROUTINE CHKDMY(ID,IM,IY,DATE,IDATE)
C
C THIS ROUTINE CHECKS THAT THE DAY, MONTH, AND YEAR, ID, IM, AND
C IY ARE WITHIN RANGE AND CONSISTENT.  IF SO DATE IS SET TO
C .TRUE. AND IDATE TO THE DATE AS YYMMDD.  OTHERWISE THESE VALUES
C ARE NOT CHANGED.
C
      DIMENSION NDAYS(12)
      LOGICAL DATE
      SAVE NDAYS
      DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
C
C CHECK THE VALUES OF IY, IM, AND ID.
C
      IF(IY.GE.0.AND.IY.LE.99.AND.IM.GE.1.AND.IM.LE.12.AND.ID.GE.1) THEN
C
C THE BASIC VALUES ARE WITHIN RANGE, CHECK THE NUMBER OF DAYS.
C
        IF(ID.GT.NDAYS(IM)) THEN
C
C CHECK IF FEBRUARY AND A LEAP YEAR.
C
          IF(IM.EQ.2.AND.MOD(IY,4).EQ.0.AND.ID.EQ.29) THEN
C
C IT IS A VALID DATE, SET DATE.
C
            DATE=.TRUE.
            IDATE=IY*10000+IM*100+ID
          ENDIF
        ELSE
C
C IT IS A VALID DATE, SET DATE.
C
          DATE=.TRUE.
          IDATE=IY*10000+IM*100+ID
        ENDIF
      ENDIF
      RETURN
C
C END OF SUBROUTINE CHKDMY.
C
      END
*DECK INTCMD
C MADE FASTER by PAH 
C
C
C
      FUNCTION INTCMD(CLIST,STRING)
C
C THIS ROUTINE COMPARES THE CHARACTERS IN 'STRING' AGAINST THE LIST IN
C 'CLIST' AND RETURNS THE POSITION IN THE LIST WHICH AGREES WITH STRING
C UP TO AS MANY CHARACTERS AS ARE CONTAINED IN STRING.  IF NO MATCH CAN
C BE FOUND THEN INTCMD IS ZERO ON EXIT.  IF MORE THAN ONE MATCH IS FOUND
C THEN THE FOLLOWING ACTION IS TAKEN: IF THE VALUE IN CLIST IS ALSO
C AMBIGOUS THEN THE STRING IS ASSUMED AS MATCHING THE SHORTEST OF THE
C MATCHING CLIST ENTRIES; OTHERWISE IF THE VALUE IN CLIST IS NOT
C AMBIGUOUS OF IF THE AMBIGUOUS ENTRIES ARE IDENTICAL THEN INTCMD IS SET
C THE NEGATIVE OF THE FIRST ENTRY.
C INPUT ARGUMENTS:
C  CLIST    - THE LIST OF STRINGS TO COMPARE 'STRING' AGAINST TERMINATED
C             WITH A BLANK ENTRY.
C  STRING   - THE STRING TO COMPARE.
C OUTPUT ARGUMENT:
C  INTCMD   - THE POSITION OF THE MATCHING STRING IN CLIST, 0 IF NO
C             MATCH IS FOUND, -VE IF THE MATCH IS AMBIGUOUS (SEE ABOVE).
C     VERSION 2.0   10MAY85
C
      CHARACTER CLIST(*)*(*),STRING*(*)
      LOGICAL COMSTR2
      INTEGER LENS, LENF
C
C SET INTCMD.
C
      INTCMD=0
C
C SCAN THROUGH CLIST COMPARING THE ENTRIES WITH STRING UNTIL THE END OF
C THE LIST IS REACHED.
C
      I=1
      LENS=LENGTH(STRING)
 1000 IF(CLIST(I).NE.' ') THEN
         LENF=LENGTH(CLIST(I))
C
C COMPARE THE ENTRY AGAINST STRING.
C
        IF(COMSTR2(CLIST(I),LENF,STRING,LENS)) THEN
C
C THEY AGREE, TEST IF A PREVIOUS ENTRY ALSO AGREED.
C
          IF(INTCMD.NE.0) THEN
C
C YES, ARE BUT ARE THE ENTRIES IN CLIST AMBIGUOUS?  FIRST CHECK IF THEY
C ARE IDENTICAL.
C
            L1=LENGTH(CLIST(ABS(INTCMD)))
            L2=LENF
            IF(CLIST(ABS(INTCMD)).EQ.CLIST(I)) THEN
C
C THEY ARE IDENTICAL, NOTE THE POSSIBLE AMBIGUITY.
C
              INTCMD=-ABS(INTCMD)
            ELSEIF(L1.LT.L2) THEN
C
C THE PREVIOUS MATCH IS THE SHORTEST, ARE THE ENTRIES AMBIGUOUS?
C
              IF(COMSTR2(CLIST(I),LENF,CLIST(ABS(INTCMD)),L1)) THEN
C
C YES, STAY WITH THE PREVIOUS MATCH.
C
                INTCMD=ABS(INTCMD)
              ELSE
C
C NO, THE STRING IS AMBIGUOUS: EXIT.
C
                INTCMD=-ABS(INTCMD)
                GOTO 7777
              ENDIF
            ELSE
C
C THE PREVIOUS MATCH IS THE LONGER STRING OF THE TWO, ARE THE ENTRIES
C IN CLIST AMBIGUOUS?
C
              IF(COMSTR2(CLIST(ABS(INTCMD)),L1,CLIST(I),LENF)) THEN
C
C YES, USE THE MATCH TO THE LATEST ENTRY.
C
                INTCMD=I
              ELSE
C
C NO, THE STRING IS AMBIGUOUS: EXIT.
C
                INTCMD=-ABS(INTCMD)
                GOTO 7777
              ENDIF
            ENDIF
          ELSE
C
C THERE ARE NO PREVIOUS MATCHES, SET INTCMD TO THE LIST ENTRY NUMBER.
C
            INTCMD=I
          ENDIF
        ENDIF
C
C GET THE NEXT ENTRY.
C
        I=I+1
        GOTO 1000
      ENDIF
C
C THE END OF THE LIST HAS BEEN REACHED, EXIT.
C
 7777 continue
c$$$      write(*,'(''in intcmd i='',i,x,''intcmd='',i,x,
c$$$     &    ''string='',a)') i,intcmd,string
      RETURN
C
C END OF INTEGER FUNCTION INTCMD.
C
      END




*DECK PARINT
C
C
C
      LOGICAL FUNCTION PARINT(PAR,IVAL)
C
C ATTEMPTS TO INTERPRET THE PARAMETER STRING PAR AS AN INTEGER.  RETURNS
C THE VALUE .TRUE. IF SUCCESSFUL AND .FALSE. OTHERWISE.  ON EXIT IVAL
C CONTAINS THE INTEGER VALUE IF PARINT IS .TRUE. AND IS UNCHANGED OTHERWISE.
C
      CHARACTER PAR*(*),FMT*8
C
C SET UP THE INITIAL VALUES.
C
      PARINT=.FALSE.
C
C SCAN THROUGH PAR TO LOCATE THE FIRST NON-SPACE CHARACTER.
C
      LP=LENGTH(PAR)
      DO 1 I=1,LP
        IF(PAR(I:I).NE.' ') GOTO 1000
    1 CONTINUE
      RETURN
C
C EVALUATE THE NUMBER OF CHARACTERS IN THE PARAMETER.
C
 1000 NC=LP-I+1
C
C FRIG THE FORMAT STATEMENT TO GIVE THIS FIELD SIZE.
C
      WRITE(FMT,800) NC
C
C NOW ATTEMPT TO READ THE PARAMETER AS AN INTEGER.
C
      READ(PAR(I:LP),FMT,ERR=100,IOSTAT=NC) I
      IF(ABS(I).LE.MAXINT()) THEN
C
C READ SUCCESSFUL.
C
        PARINT=.TRUE.
        IVAL=I
      ENDIF
      RETURN
C
C READ UNSUCCESSFUL.
C
  100 CONTINUE
      RETURN
C
C FORMAT STATEMENT.
C
  800 FORMAT(SS,'(BN,I',I2,')')
C
C END OF LOGICAL FUNCTION PARINT.
C
      END
*DECK CHKYMD
C
C
C
      SUBROUTINE CHKYMD(PAR,LP,DATE,IDATE)
C
C THIS ROUTINE CHECKS IF THE DATE IN PAR IS A VALID DATE IN THE
C FORMAT YY/MM/DD.  IF IT IS DATE IS RETURNED AS .TRUE. AND IDATE
C CONTAINS THE DATE AS YYMMDD ON EXIT, OTHERWISE THESE VALUES ARE
C NOT CHANGED.  ON ENTRY LP SHOULD BE THE EFFECTIVE LENGTH OF PAR.
C
      CHARACTER PAR*(*),STR(3)*2
      LOGICAL DATE
C
C SET THE CONTENTS OF STR TO SOMETHING SILLY TO FORCE AN ERROR
C IF AN ELEMENT IS NOT SET.
C
      DO 1 I=1,3
    1 STR(I)='?'
      J=1
      IC=0
C
C SCAN THROUGH PAR AND ATTEMPT TO LOAD ITS COMPONENT BITS INTO STR.
C
      DO 2 I=1,LP
        IF(PAR(I:I).EQ.'/') THEN
C
C '/' ENCOUNTERED, INCREMENT THE YMD COUNTER.
C
          J=J+1
          IF(J.GT.3) THEN
C
C TOO MANY '/''S, THERFORE NOT A TIME.
C
            RETURN
          ENDIF
C
C CLEAR THE CHARACTER COUNTER.
C
          IC=0
        ELSEIF(PAR(I:I).NE.' '.AND.(IC.NE.0.OR.PAR(I:I).NE.'+')) THEN
C
C CHARACTER OTHER THAN A SPACE OR '+', LOAD IT INTO STR.
C
          IC=IC+1
          IF(IC.GT.2) THEN
C
C STRING IS TOO LONG, NOT A DATE.
C
            RETURN
          ENDIF
C
C LOAD INTO STR.
C
          STR(J)(IC:IC)=PAR(I:I)
        ENDIF
    2 CONTINUE
C
C NOW ATTEMPT TO READ STR AS INTEGERS.
C
      READ(STR,800,ERR=100,IOSTAT=IFAIL) I,J,K
C
C READ SUCCESSFUL, CHECK THE VALUES OF THE DAY, MONTH, AND YEAR.
C
      CALL CHKDMY(K,J,I,DATE,IDATE)
      if (idate.lt.600000) then
         idate = idate + 1000000
      endif
      RETURN
C
C READ UNSUCCESSFUL.
C
  100 CONTINUE
      RETURN
C
C FORMAT STATEMENT.
C
  800 FORMAT(BN,3(I2:/))
C
C END OF SUBROUTINE CHKYMD.
C
      END
c
c Returns the maximum possible usable positive integer value.
c     In this generic version, 32 bit integers are assumed.  It does
c not really matter if the maximum positive integer value is really
c greater than this implies.
c
c     Version 1.1    8th September, 1986   Generic
c
c Define the largest usable integer.
c
      integer function maxint()
      integer maxi
c
c Set MAXINT.
c
      parameter (maxi = 2147483647)
c
c End of INTEGER function MAXINT.
c
      maxint = maxi
      end
*DECK COMSTR
C faster version of COMSTR - needs the lenghs of the strings passed to
C      it -PAH

C
C
C **********************************************************************
      LOGICAL FUNCTION COMSTR2 ( FULSTR,LENF, STR, LENS )
C **********************************************************************
C
C THIS ROUTINE PERFORMS A COMPARISON BETEEN TWO STRINGS.
C IT RETURNS .TRUE. IF THE STRING STR IS A VALID SHORTENED FORM 
C     OF THE STRING FULSTR.
C THE STRINGS MAY BE MADE UP OF ONE OR MORE WORDS SEPARATED BY ANY OF 
C     THE CHARACTERS CONTAINED IN DELIM.
C     EACH WORD IN STR MAY BE SHORTENED INDIVIDUALLY.
C THE CASE OF ALPHABETIC CHARACTERS IN EITHER STRING IS IRRELEVENT.
C   ( NOTE: ONLY THE FIRST LENBUF CHARACTERS OF EACH WORD ARE COMPARED )
C
      CHARACTER*(*) FULSTR,STR,DELIM*4
      PARAMETER ( LENBUF=40 )
      CHARACTER*(LENBUF) UPPCASE,FULBUF,BUF
      integer NDEL
      DATA DELIM / ' _*/' /, NDEL/4/
C
C     OBTAIN THE EFFECTIVE LENGTHS OF THE TWO STRINGS.
C
C
C     CHECK IF IT IS VALID TO COMPARE THEM.
C
      IF ( LENS.LE.LENF.AND.LENS.GT.0.AND.LENF.GT.0 ) THEN
C
C        SET THE CHARACTER POINTERS AND COMSTR,
C        AND OBTAIN THE NUMBER OF DELIMITERS.
C
         IF = 1
         IS = 1
         COMSTR2 = .TRUE.
C
C        COMPARE EACH WORD IN TURN.
C        LABEL 1000 IS THE RETURN POINT FOR THE NEXT WORD.
C
 1000    CONTINUE
C
C        PROCEED IF COMPARISON STILL OK,
C        AND THERE IS SOME OF THE SHORT STRING LEFT.
C
         IF ( COMSTR2.AND.IS.LE.LENS ) THEN
C
C           LOCATE THE NEXT WORD BOUNDARY IN BOTH STRINGS.
C           CLEAR THE DELIMITER NUMBERS AND
C           SET THE WORD BOUNDARIES TO THE END OF THE STRINGS.
C
            IDELS= 0
            IDELF= 0
            LWBF = LENF
            LWBS = LENS
            DO 10 I=1,NDEL
C
C              LOOK FOR DELIMITER I IN THE FULL STRING.
C              L POINTS AT THE LAST CHARACTER OF THE WORD
C              TERMINATED BY THE DELIMITER.
C
               L = INDEX(FULSTR(IF:),DELIM(I:I))+IF-2
C
C              IF THE DELIMITER WAS FOUND AND IT IS BEFORE ANY
C              PREVIOUSLY FOUND DELIMITER,
C              THEN SET THE POSITION OF THE WORD BOUNDARY AND
C              REMEMBER WHICH DELIMITER IT WAS.
C
               IF ( L.GE.IF.AND.(LWBF.EQ.0.OR.L.LT.LWBF) ) THEN
                  LWBF = L
                  IDELF= I
               ENDIF
C
C              SAME FOR THE SHORT STRING.
C
               L = INDEX(STR(IS:),DELIM(I:I))+IS-2
               IF ( L.GE.IS.AND.(LWBS.EQ.0.OR.L.LT.LWBS) ) THEN
                  LWBS = L
                  IDELS= I
               ENDIF
   10       CONTINUE
C
C           IF THERE IS A WORD BOUNDARY IN THE SHORT STRING STR,
C           WHOSE DELIMITER DOES NOT MATCH THE THAT OF THE NEXT WORD
C           BOUNDARY IN THE FULL STRING, THEN THE STRINGS DO NOT MATCH.
C
            IF ( IDELS.GT.0.AND.IDELS.NE.IDELF ) THEN
               COMSTR2 = .FALSE.
            ELSE
C
C              COMPARE THE CURRENT WORDS IF THE REMAINING CHARACTERS IN
C              STR ARE FEWER THAN OR THE SAME NUMBER AS THOSE LEFT IN
C              FULSTR, AND THE WORD IN STR IS SHORTER THAN OR THE SAME
C              LENGTH AS THE WORD IN FULSTR.
C
               IF ( LENS-IS.LE.LENF-IF.AND.LWBS-IS.LE.LWBF-IF ) THEN
C
C                 COMPARE THE WORDS UP TO THE LENGTH OF THAT IN STR.
C                 IN ORDER THAT ALPHABETIC CASE IS IGNORED CONVERT TO 
C                 UPPERCASE IN BUFFERS FULBUF AND BUF.
C
                  FULBUF = UPPCASE(FULSTR(IF:IF+MIN(LWBS-IS,LENBUF-1)))
                     BUF = UPPCASE(   STR(IS:IS+MIN(LWBS-IS,LENBUF-1)))
                  COMSTR2 = FULBUF(IS:IS+MIN(LWBS-IS,LENBUF-1))
     &                   .EQ.BUF(IS:IS+MIN(LWBS-IS,LENBUF-1))
C
C                 SET THE POINTERS TO THE START OF THE NEXT WORD.
C
                  IF = LWBF+2
                  IS = LWBS+2
               ELSE
C
C                 THERE ARE MORE CHARACTERS LEFT IN STR THAN IN FULSTR,
C                 OR THE WORD IN STR IS LONGER THAN THAT IN FULSTR.
C                 SET COMSTR TO .FALSE.
C
                  COMSTR2 = .FALSE.
               ENDIF
            ENDIF
C
C           COMPARE THE NEXT WORD.
C
            GOTO 1000
         ENDIF
      ELSE
C
C        STR IS LONGER THAN FULSTR OR STR OR FULSTR ARE BLANK.
C        SET COMSTR IF BOTH ARE BLANK.
C
         COMSTR2 = LENF.EQ.LENS
      ENDIF
C
      RETURN
C
C END OF LOGICAL FUNCTION COMSTR2.
C
      END

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
      call upcase(lstring, 1, 1)
      uppcase = lstring(:len(string))
c
c END OF CHARACTER*(*) FUNCTION UPPCASE
c
      return 
      end
      subroutine upcase(pars, is, if)
      integer is, if
c
c Declare external references.
c
      character pars(*)*(*)
c
c Declare local variables.
c
      integer length
c
c Work out the difference between 'a' and 'A'.
c
      integer diff, i, j
c$$$c
c$$$c Convert the parameters one at a time.
c$$$c
c$$$      diff = ichar('A') - ichar('a')
c$$$c
c$$$c Convert each character in turn.
c$$$c
c$$$      do 1 i = is, if
c$$$      do 2 j = 1, length(pars(i))
c$$$      if ((pars(i)(j:j) .ge. 'a') .and. (pars(i)(j:j) .le. 'z')) then
c$$$      pars(i)(j:j) = char(ichar(pars(i)(j:j)) + diff)
c$$$      end if
c$$$    2 continue
c$$$c
c End of subroutine UPCASE.
c
    1 continue
      end
