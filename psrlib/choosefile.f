c----------------------------------------------------------------------
        subroutine choose_file(directory, files, arraysize, chosenfile, 
     &  maxfile, abort)
c----------------------------------------------------------------------
c
c       This routine conrols the dialog 'finder', supplying it with
c       the files obtained from getfile the user selects the file
c       in the dialog and chosenfile is passed back to the main program
c	New version has added feature, the abort button, if pressed
c	the routine returns abort=.true.  (DRL 08-10-91)
c
        implicit none
        external plfinder
        integer replot, plotno, button, start, arraysize, choice, i
        integer ldire, maxfile
        real x, y
	logical abort
c
c	filename and directory
c	eg directory = '/psr/data/psrevolve/'
c       eg filename ='*.psrs'
c
	character*(*) directory
        character*(*) files(*), ans*1, chosenfile, message*80

	abort = .false.
        ldire=index(directory,' ')-1
        directory=directory(1:ldire)
	message = 'Select loadfile'
        replot = 1
        start = 1
	do i = arraysize+1,maxfile
	  files(i)=' '
	end do
c
c	Main call to the dialog
c
 5      call finder(replot, x, y, ans, plotno, button, 
     &  files(start), files(start+1), files(start+2), files(start+3),
     &  files(start+4), files(start+5), files(start+6), message)
        replot = 2
        if (button.ge.1.and.button.le.7) then
c
c         select file and quit
c
           choice = start + (button - 1)
           if (choice.gt.arraysize) then
c
c            That file does not exist
c
             message = 'No file'
           else
c
c            Quit
c
             goto 10
           end if
        else if (button.eq.8) then
c
c         page up button pressed, check to see if the pointer start
c         is already at the top of the array
c
          if (start.eq.1) then
            message = 'Top of array'
          else
            start = start-7
            message = 'Paged up'
          end if
        else if (button.eq.9) then
c
c         page down button pressed, check to see if the pointer start
c         is already at the bottom of the array
c
          if (start+7.gt.arraysize) then
            message = 'End of array'
          else
            start = start+7
            message = 'Paged down'
          end if
        else if (button.eq.10) then
c
c	  Abort button pressed
c
	  abort = .true.
	  return
        end if
c
c       Update the page
c
        goto 5
c
c       Return the chosen file
c
10      chosenfile = directory(1:ldire)//files(choice)
     &      (1:index(files(choice),' ')-1)
        return
        end

