	program fitorbplt
	implicit none

	real*4 x(500), period(500), resid(500), err(500), modelx(1000), 
     &         modely(1000), minper, maxper, minres, maxres, xlo,xhi,
     &         ylo, yhi, yoff, minmjd, maxmjd, mjd0, psize, oldch
	character fname*80, title*80, 
     &            arg1*80, arg2*80, inlin*132, cuts(10)*20
	integer i, nmod, nper, llen, iargc, ia, lwidth, lcolour,
     &          pcolour, oldlw
        logical phaseflag, idenflag, headerflag
        integer pgbeg
        external pgbeg, iargc

* Init a few things
        phaseflag = .true.
        idenflag = .false.
        headerflag = .false.
        LWidth = 3
        lcolour =1
        pcolour =1
        PSize = 1.5
        fname = ' '
        llen = 0
        title = ' '

* tel user what they're running
        write(*,'(a,a)')' FITORBIT PLOT',
     &   '- measured period and residuals from fitorbit against phase'

* Check for command line arguments- last = filename prefix
        ia = iargc()
        call getarg(1,arg1)
        if(ia.eq.0 .or. arg1(1:2).eq.'-h')go to 50

        i=1
        do while(i.le.ia-1)
          call getarg(i,arg1)  

* help
          if(arg1(1:2).eq.'-h') then
            goto 50

* identify plot
          elseif(arg1(1:2).eq.'-i') then
            i=i+1
            idenflag = .true.

* time or phase plot
          elseif(arg1(1:2).eq.'-t') then
            i=i+1
            phaseflag = .false.

* Header
          elseif (arg1(1:2).eq.'-H') then
            i=i+1
            headerflag = .true.

* optional header string
            call getarg(i,arg2)  
            if(arg2(1:1).ne.'-' .and. i .lt. ia)then
              read(arg2,'(a)') title
              i=i+1
            endif
            
* Line colour
          elseif(arg1(1:3).eq.'-lc') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).ne.'-' .and. i .lt. ia)then
              read(arg2,*) lcolour
              i=i+1
            else
              write(*,*)'ERROR: Line colour needs an integer parameter'
              STOP
            endif

* Line width
          elseif(arg1(1:3).eq.'-lw') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).ne.'-' .and. i .lt. ia)then
              read(arg2,*) lwidth
              i=i+1
            else
              write(*,*)'ERROR: Line width needs an integer parameter'
              STOP
            endif

* Point colour
          elseif(arg1(1:3).eq.'-pc') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).ne.'-' .and. i .lt. ia)then
              read(arg2,*) pcolour
              i=i+1
            else
              write(*,*)'ERROR: point colour needs an integer parameter'
              STOP
            endif

* Point size
          elseif(arg1(1:3).eq.'-ps') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).ne.'-' .and. i .lt. ia)then
              read(arg2,*) psize
              i=i+1
            else
              write(*,*)'ERROR: point size needs a real parameter'
              STOP
            endif
          else
            write(*,*)'ERROR: Ignoring unknown argument '//arg1
            i=i+1
 
          endif


        enddo


* Check out last command line option
        if(i.gt.ia)then
          write(*,'(a)')
     &      ' ERROR: Must have residuals filename prefix as last option'
          STOP
        else
          call getarg(i,fname)
          if(fname(1:1).eq.'-')then
             write(*,'(a)')
     &      ' ERROR: Must have residuals filename prefix as last option'
            STOP
           endif
        endif

* cut off the spaces in the filename
        llen = 1
        dowhile (fname(llen:llen) .ne. ' ' .and. llen .le.80)
          llen = llen+1
        enddo
        llen = llen-1
        
* load up header
        if (headerflag .and. title .eq. ' ')then
          title = fname
        endif

* open model file
        open (10, file = fname(1:llen)//'.mod',
     &      form = 'formatted', status = 'old', readonly)

* read model, and find max/min of data
        minper = +99999999.
        maxper = -99999999.
        nmod = 0
        do while (nmod .lt. 1000)
          nmod= nmod+1
          read(10,*, end=9) modelx(nmod), modely(nmod)
          minper = min(minper, modely(nmod))
          maxper = max(maxper, modely(nmod))
        enddo
 9	nmod = nmod -1
        close (10)

* open residuals file
        open (10, file = fname(1:llen)//'.res',
     &      form = 'formatted', status = 'old', readonly)

* get the mean period to provide yoffset
 10     inlin = ' '
        do while (inlin(2:12) .ne. 'Mean Period')
          read(10,'(a)', end = 998)inlin
        enddo
        read(inlin(24:),*) yoff
        yoff = yoff*1000000.
 
* Remake model data.
        do i = 1, nmod
          modely(i) = modely(i)- yoff
        enddo
        minper = minper- yoff
        maxper = maxper -yoff

* get start of analysis (for time plot)
        if (.not. phaseflag) then
          do while (inlin(2:18) .ne. 'Start of analysis')
            read(10,'(a)', end = 998)inlin
          enddo
          read(inlin(26:),*) mjd0
        endif

* Now look for start of data
 20     inlin = ' '
        do while (inlin(5:7) .ne. 'MJD') 
          read(10,'(a)', end = 998)inlin
        enddo
       
* Start of data (I hope!) extract period(col 2) residual(col4) errs (col5) 
* and phase(col8) or MJD (col 1)
        nper = 0
        inlin = ' '
        minres = +99999999.
        maxres = -99999999.
        minmjd = 99999.0
        maxmjd = -99999.0
 
        do while (nper .lt. 500)
          read(10,'(a)', end = 30)inlin

* find 8 items as strings
          call  PARSE (inlin, cuts, i, 9, ' ' )

* throw away Nofit dat
          if (i .eq. 8 .or. (i .eq. 9 .and. cuts(9) .ne. 'No')) then

            nper = nper+1
            read(cuts(2),*)period(nper)
            period(nper) = period(nper) - yoff

            read(cuts(5),*)err(nper)

            maxper = max(period(nper)+err(nper), maxper)
            minper = min(period(nper)-err(nper), minper)

            read(cuts(4),*)resid(nper)
            maxres = max(resid(nper)+err(nper), maxres)
            minres = min(resid(nper)-err(nper), minres)

            If (phaseflag) then
              read(cuts(8),*)x(nper)
            else
              read(cuts(1),*)x(nper)
              x(nper) = x(nper) - mjd0
              minmjd = min(minmjd, x(nper))
              maxmjd = max(maxmjd, x(nper))
            endif

          endif
        enddo
 30	continue
        close (10)

* Plot it
 40     IF (PGBEG(0,'?',1,1) .LE. 0) STOP
        CALL PGASK(.FALSE.)

* set a viewport for first graph, draw axes, label graph.
        CALL PGSVP(0.15, 0.95, 0.30, 0.95)

* find a reasonable scale
        CALL PGRNGE (MINPER, MAXPER, YLO, YHI)

        IF (phaseflag) then
          XLO = 0.0
          XHI = 1.0
        else
          CALL PGRNGE (MINMJD, MAXMJD, XLO, XHI)
        endif

        CALL PGSWIN(XLO, XHI, YLO, YHI)
        CALL PGSCF(2)
        CALL PGBBUF
        CALL PGBOX('BCTS', 0.0, 0, 'BNCTS', 0.0, 0)

* find current PG settings
        CALL PGQCH(oldch)
        CALL PGQLW(oldlw)

        CALL PGSCH(1.5)
        CALL PGLABEL(' ','Period offset (\\gmsec)',title)

* plot model with line thickness 3 (store old line width and reset after
        CALL PGSLW(LWidth)
        CALL PGSCI(Lcolour)
        CALL PGLINE(nmod, modelx, modely)
        
* Plot period offset from mean against phase/time using point type 17
        CALL PGSCH(PSize)
        CALL PGSCI(PColour)
        CALL PGPT (Nper, x, period, 17)

* Plot error bars (up and down)
        CALL PGERRB(6,NPER,X, PERIOD, ERR,0.0)
        CALL PGSLW(oldlw)
        CALL PGSCI(1)

* Plot residuals against phase/time in lower box
        CALL PGSVP(0.15, 0.95, 0.15, .30)

* find a reasonable scale
        CALL PGRNGE (MINRES, MAXRES, YLO, YHI)

        CALL PGSWIN(XLO, XHI, YLO, YHI)
        CALL PGSCH(oldch)
        CALL PGSCF(2)
        CALL PGBOX('BNCTS', 0.0, 0, 'BNCTS', 0.0, 0)
        CALL PGSCH(1.5)

        if (phaseflag) then
          CALL PGLABEL('Orbital Phase','Residuals', title)
        else
          write (inlin,'(a,f9.3)')'Days from MJD ', mjd0
          CALL PGLABEL(inlin,'Residuals', title)
        endif

* put ID on graph if option set
        if (idenflag) CALL PGIDEN
        
* Plot residuals against phase/time using point type 17
        CALL PGSCH(PSize)
        CALL PGSCI(PColour)
        CALL PGSLW(LWidth)
        CALL PGPT (Nper, x, resid, 17)

* Plot error bars (up and down)
        CALL PGERRB(6,NPER,X, RESID, ERR,0.0)
        CALL PGEBUF

* allow user to repeat
        write(*,'(a,$)') ' Hardcopy y/<n> ? '
        read(*,'(a)')inlin

        CALL PGEND
        if (inlin(1:1) .eq. 'y' .or. inlin(1:1) .eq. 'Y') goto 40

        goto 999

 998	write(*,'('' Pulsar parameter not found in .res file'')')
        goto 999

* Give the user some help, if they look like they need it
 50     write(*,*)'Usage: plofitorbit [options] resfilename' 
        write(*,*)'Options:'
        write(*,*)'-t  Plot residuals against time model  (def = phase)'
        write(*,*)'-ps <f> point size                       (def = 1.5)'
        write(*,*)'-pc <n> point colour R=2 B=4 G=3         (def = W/B)'
        write(*,*)'-lw <n> line width                         (def = 3)'
        write(*,*)'-lc <n> line colour C=5 M=6 Y=7          (def = W/B)'
        write(*,*)'-H <optional string> Header        (def = no header)'
        write(*,*)'-i name/date in corner               (def = no date)'

 999	continue
        end

C *********************************************************************
      SUBROUTINE PARSE ( STRING, PARS, NPAR, MAXPAR, DELIM )
C *********************************************************************
C
C SPLITS THE CHARACTER ARRAY STRING INTO PARAMETERS SEPARATED BY
C    THE DELIMITERS DELIM
C THE NTH CHARACTER OF DELIM IS USED TO TERMINATE THE NTH
C PARAMETER, THE FINAL CHARACTER BEING REPEATED IF NECESSARY
C THE PARAMETERS ARE RETURNED IN PARS(MAXPAR) AND THEIR NUMBER IS NPAR
C
      CHARACTER*(*) STRING,PARS(MAXPAR),DELIM
C
C L HOLDS THE POSITION OF THE LAST DELIMITER FOUND
C IDEL POINTS TO THE CURRENT DELIMITER CHARACTER
C
      LENDEL = LEN(DELIM)
      IDEL = 1
      NPAR=0
      L=0
      DO 10 I=1,LEN(STRING)
         IF ( STRING(I:I).EQ.DELIM(IDEL:IDEL) ) THEN
            IF ( I-L.GT.1 ) THEN
C
C              NON-ZERO LENGTH STRING BETWEEN DELIMITERS FOUND
C              CHECK NUMBER OF PARAMETERS AGAINST MAXPAR
C              STORE CURRENT PARAMETER
C              UPDATE DELIMITER CHARACTER
C
               IF ( NPAR+1.GT.MAXPAR ) GOTO 100
               NPAR = NPAR+1
               PARS(NPAR) = STRING(L+1:I-1)
               IDEL = MIN(IDEL+1,LENDEL)
            ENDIF
            L=I
         ENDIF
   10 CONTINUE
C
C     IF A STRING IS REMAINING AT THE END, ADD IT TO PARS
C
      IF ( L.LT.LEN(STRING) ) THEN
         IF ( NPAR+1.GT.MAXPAR ) GOTO 100
         NPAR = NPAR+1
         PARS(NPAR) = STRING(L+1:LEN(STRING))
      ENDIF
C
  100 CONTINUE
      RETURN
C
C END OF SUBROUTINE PARSE
C
      END
 
