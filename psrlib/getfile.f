c======================================================================
	subroutine getfile(directory, filename, files, nfiles, maxfile)
c======================================================================
c
c	This routine searches a specified directory for specific files
c	eg *.psrinfo and puts the nfiles files that it finds in the 
c       files array, allowing the user to get the desired file for 
c       their program. Uses UNIX commands to search directories and
c       is therefore not too portable.
c
c       Last change 12-05-93 DRL @ JB
c
	implicit none
c
c	passed down directory and filename
c	eg directory = '/psr/data/psrevolve/'
c       eg filename =  '*.psrs'
c	output files in the character array
c
	character*80 directory
        character*(*) files(*), filename
c
c	local variables
c
        character comm*120
        integer ldir, maxfile, nfiles, i, lfil, lun
        logical first
        data first/.true./
c
c	Work out lengths of directory 
c
	ldir = index(directory,' ')-1
	lfil = index(filename,' ')-1
c
c	Look for the file in the given directory and write to a file
c
        comm = 'ls -1 '//directory(1:ldir)//filename//' > getfile.tmp'
        call system(comm)
c
c       Open up getfile.tmp and read the filenames into output array
c
	nfiles = 1
	call glun(lun)
	open(unit=lun,file='getfile.tmp',status='old') 
        do while(.true.)
 	  if (nfiles.gt.maxfile) goto 999
	  read(lun,'(a)',end=999) files(nfiles)
          nfiles = nfiles + 1
	enddo
999     nfiles = nfiles - 1
c
c	Close the file and remove!
c	
	close(unit=lun,status='delete')
c
c	trim the array to contain just the file names
c
	do i=1,nfiles
          files(i)=files(i)(ldir+1:index(files(i),' ')-1)
	end do
	end

