c--------------------------------------------------
	subroutine get_setup
c
c	attempts to read the widget colours from
c	the .dialog file which is in the HOME
c	directory, if this can't be found then
c	the default colours are read in as set
c	up in dialogstructure DRL 23/12/92
c	could be extended to read in a whole
c	load of other things from the file.
c
	include 'dialogstructure'
	character*80 homedir
	integer lun, bcol, ccol, rcol, ocol
	logical first
	data first/.true./
	data butcol/9/,checol/8/,radcol/8/,outcol/8/

	if (first) then
	  call psrgetenv('HOME',homedir)
	  lun=78  ! the dreaded getlun is stuffed or seems to be??
	  open(unit=lun,file=homedir(1:index(homedir,' ')-1)//
     &         '/.dialog',status='old',err=999)
	  read(lun,*,err=999) ! read off header line 
	  read(lun,*,err=999) ! read off header line 
	  read(lun,*,err=999) bcol, ccol, rcol, ocol
	  close(unit=lun)
c
c	  read in values OK now copy over to common
c
	  butcol=bcol
	  checol=ccol
	  radcol=rcol
	  outcol=ocol	
	  first=.false.
	endif
	return
c
c	no .dialog file available or file reading
c       error just use defaults value already set
c	
 999    end

