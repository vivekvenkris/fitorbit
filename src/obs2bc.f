      program obs2bc
      implicit none
*+
* Utility to take input (kb or file) of form
* RA/DEC (J2000) as 00:21:05 -72:20:00
* and then lines containing
* MJD P ..........
* Output to be 
* RA/DEC
* MJD (at barycentre) Pbc .........
*
* Created 7th July 1998 CAJ
* Uses PSREPHD and assumes that delay returned  by BTDB (Equiv. barycentric
* TDB MJD of observation)is 'symmetrical' ie the 
* delay from bary->earth is same as earth->bary (only backwards:-).
*-
      include 'tel.def'

      integer itelno, ntel, nword, status, length, parpos,i,ll
      real*8 ra2000, de2000
      real*8 mjd, mjdsin, mjdbc, btdbs, xma, doppe, arrtim
      real*8 pbc, pobs, edelay
      character place*10, site*10, buffer*80
      character*80 word(10)
      logical verbose /.false./
      data site/'PARKES'/

* Use command line arguments -v = verbose or -sJ or P for site (default ?)
* and -h for help
      call getarg(1,buffer)

* help  - give message and exit
      if (buffer(1:2) .eq. '-h') then

        type *, 'usage: obs2bc [-h | -v | -sX ]' 
        type *, '       given RA/DEC will convert site arrival time '
        type *, 
     &'       to equiv. BTDB, and correct Pobs for earth doppler'
        type *, ' '
        type *, '   -h: this message'
        type *, ' '
        type *, '   -v: verbose'
        type *, '       requests site (default Parkes)'
        type *, 
     &'       requests RA/DEC J2000 in hh:mm:ss dd:mm:ss format'
        type *, '       loops'
        type *, '         requesting MJD Pobs comment'
        type *, '            (where MJD = site arrival time (days)'
        type *, '              and Pobs = observed period (secs)'
        type *, '               comment = any additional data.)'
        type *, '       until blank line entered'
        type *, ' '
        type *, '   -sX:sets site to X (eg J = Jodrell, P=Parkes)'
        type *, ' '
        type *, '       In non-verbose mode, no prompts are given,'
        type *, 
     &'       Site is set from command line or default (P) is used.'
        type *, 
     &'       I/O is as for verbose, and can be redirected.'

        goto 999
      endif

* verbose
      verbose = buffer(1:2) .eq. '-v'

* site
      if (buffer(1:2) .eq. '-s') place = buffer(3:3) 

* set the telescope 
      if (verbose) then
        type '('' Enter site name (e.g. JODRELL or PARKES, etc.): '',$)'
        read (*,'(a)',end=5) place
 5      if (place.ne.' ') site=place
      endif

      do itelno=1,ntel
        if (site(1:length(site)).eq.
     &      ctel(itelno)(1:length(site))) go to 6
      enddo
      type *,' Telescope name not recognised'
      goto 999

 6    continue
      if (verbose) type *,ctel(itelno)

* get the ra/dec in degrees
      if (verbose) 
     &       write(*,'('' RA/DEC J2000 in hh:mm:ss dd:mm:ss : '',$)')
      read(*,'(a)', err=999, end = 999)buffer
      
* Echo input
      write(*,'(x,a)')buffer(1:length(buffer))

* sortout RA/DEC and rest of line
      CALL PARSE(buffer,WORD,NWORD,10,' ')
      RA2000=0D0
      DE2000=0D0
      IF(PARPOS(WORD(1),RA2000)) RA2000=RA2000/240.0D0
      IF(PARPOS(WORD(2),DE2000)) DE2000=DE2000/3600.0D0

      do while (.true.)

* No prompt - so file can be used?
        if (verbose) write(*,'('' MJD, Pobs...... '',$)')
        buffer = ' '
        read(*,'(a)', end =999, err =999)buffer
        if (length(buffer) .eq. 0) goto 999
        CALL PARSE(buffer,WORD,NWORD,10,' ')
        read(word(1), *, err=999, end = 999 ) mjd
        read(word(2), *, err =999, end =999 ) pobs
        buffer = ' ' 
        ll=1
        do i = 3,NWORD
          write(buffer(ll:),'(x,a)')word(i)(1:length(word(i)))
          ll=ll+length(word(i))+1
        enddo

* put mjd in seconds
        mjdsin = mjd*86400d0

* call psreph - setting all binary pars to 0 and period to 1.0 - returns doppe
        call PSREPHBJ (mjdsin, ITELNO, RA2000, de2000, 0d0, 1d0, 
     &                 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 
     &                 arrtim, doppe, XMA, BTDBs)
 
* change to days and calc Pbc using Doppler
        edelay = mjdsin - btdbs
        mjdbc = btdbs/86400d0
        Pbc = Pobs/doppe

* echo the line that was read in - dec places depending on period!
        write(*,'(x,f13.6, f12.9, a)', iostat =status) mjdbc, Pbc, 
     &          buffer(1:length(buffer))

* and do it all again
      enddo

 999  continue
      end







