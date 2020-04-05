cDECK PARRNG
c   
c   
c **********************************************************************
c **********************************************************************
c   
c THIS ROUTINE PARSES THE COMMAND STRING STARTING AT PARAMETER NUMBER   
c     JINP, WITH THE GENERAL FORMAT FROM <START> TO <FINISH>.   
c NUMBOK SPECIFIES WHETHER THE START AND FINISH VALUES MAY BE ORDINARY  
c     ( DOUBLE PRECISION NUMBERS ) AND DATEOK SPECIFIES WHETHER THEY
c     MAY BE DATE/TIMES.
c ON EXIT FROM AND TO CONTAIN THE STARTING AND FINISHING VALUES.
c FDATE AND TDATE INDICATE WHETHER FROM AND TO ARE DATE/TIMES.  
c FROM IS -DBLMAX IF THERE IS NO START, 
c     AND TO IS DBLMAX IF THERE IS NO FINISH.   
c DEFJT IS THE DEFAULT JULIAN TIME FOR PARSING DATE/TIME VALUES.
c ON EXIT JINP IS UPDATED TO POINT AT THE NEXT INPUT PARAMETER. 
c   
      subroutine parrng(cmdinp, ninp, jinp, from, to, fdate, tdate, 
     &numbok, dateok, defjt, ifail)
      character cmdinp(*)*(*), timopt(7)*9
      double precision from, to, dblmax, val
      logical fdate, tdate, numbok, dateok, partim, pardat, vdate, 
     &comstr, date, time
      save timopt
c   
c     SET DEFAULT RETURN VALUES.
c   
      data timopt / 'from', 'to', 'start', 'beginning', 'finish', 'end'
     &, ' ' /
      ifail = 0
      from = - dblmax()
c
c     LOCAL COPY OF DEFAULT JT.
c
      to = dblmax()
c
c     SAVE THE COMMAND POINTER.
c
      djt = defjt
c   
c     TEST THE NUMBER OF PARAMETERS.
c   
      jsave = jinp
c   
c        IF FIRST COMMAND IS 'ALL' THEN UPDATE COMMAND POINTER  
c        AND RETURN.
c   
      if (jinp .le. ninp) then
      if (comstr('all',cmdinp(jinp))) then
      jinp = jinp + 1
      return 
c   
c        OBTAIN THE OPTION. 
c   
      end if
c   
c        CANCEL AMBIGUOUS OPTIONS ( I.E. 'FROM' OVERRIDES 'FINISH' ).   
c   
      ii = intcmd(timopt,cmdinp(jinp))
      ii = abs(ii)
c   
c           THE PARAMETER IS 'FROM', OBTAIN THE VALUE FOLLOWING IT. 
c   
      if (ii .eq. 1) then
      jinp = jinp + 1
      call fromto(cmdinp, ninp, jinp, numbok, dateok, timopt(1), from, 
     &fdate, date, timopt(3), djt, ifail)
c
c           IF A DATE/TIME WAS FOUND, AND NO DEFAULT JT IS SET,
c           SET THE DEFAULT.
c
      if (ifail .ne. 0) return 
c   
c           IF THERE IS ANOTHER COMMAND, LOOK FOR A 'TO' CLAUSE.
c   
      if (fdate .and. (djt .le. 0.0)) djt = from
      if ((jinp .le. ninp) .and. comstr(timopt(2),cmdinp(jinp))) then
      jinp = jinp + 1
      call fromto(cmdinp, ninp, jinp, numbok, dateok, timopt(2), to, 
     &tdate, date, timopt(3), djt, ifail)
      if (ifail .ne. 0) return 
      end if
c   
c           THE CURRENT PARAMETER IS 'TO', OBTAIN THE VALUE FOLLOWING IT
c   
      else if (ii .eq. 2) then
      jinp = jinp + 1
      call fromto(cmdinp, ninp, jinp, numbok, dateok, timopt(2), to, 
     &tdate, date, timopt(3), defjt, ifail)
c
c           IF A DATE/TIME WAS FOUND, AND NO DEFAULT JT IS SET,
c           SET THE DEFAULT.
c
      if (ifail .ne. 0) return 
c   
c           IF THERE IS ANOTHER COMMAND, LOOK FOR A 'FROM' CLAUSE.  
c   
      if (tdate .and. (djt .le. 0.0)) djt = to
      if ((jinp .le. ninp) .and. comstr(timopt(2),cmdinp(jinp))) then
      jinp = jinp + 1
      call fromto(cmdinp, ninp, jinp, numbok, dateok, timopt(2), to, 
     &tdate, date, timopt(3), defjt, ifail)
      if (ifail .ne. 0) return 
      end if
c   
c           THE CURRENT PARAMETER IS SOMETHING ELSE,
c           ATTEMPT TO INTERPRET IT AS A VALID VALUE.   
c   
      else
      call fromto(cmdinp, ninp, jinp, numbok, dateok, ' ', val, vdate, 
     &date, timopt(3), defjt, ifail)
c   
c           SET THE DATE FLAGS. 
c   
      if (ifail .ne. 0) return 
      fdate = vdate
c   
c           CHECK FOR A LONE DATE.  
c   
      tdate = vdate
c   
c              SET THE STARTING AND FINISHING VALUES TO THE START AND   
c              END OF THE DAY.  
c   
      if (date) then
      from = val
      to = val + 1.0
c   
c             RETURN THE VALUE AS BOTH START AND FINISH.
c   
      else
      from = val
      to = val
      end if
c   
c        CHECK THE STARTING AND THE FINISHING VALUES.   
c   
      end if
c   
c           THE STARTING VALUE IS GREATER THAN THE FINISHING VALUE, 
c           FLAG AN ERROR.  
c   
      if (from .gt. to) then
      ifail = 24
      call psrerr('PARRNG', ifail, 0, 0.0, cmdinp(jsave))
      end if
c   
      end if
c   
c END OF SUBROUTINE PARRNG. 
c   
      return 
      end
