c ----------------------------------------------------
c Utilty to stop unsuspected file overwrite
      SUBROUTINE CHECKB4WRITE(oldfile, vext, newfile)

c Developed from inline code at wrneweph
c 16/11/2001 caj
c 10/03/2006 caj add a local variable 'ext' to deal with 
c            g77 compatability

      implicit none
      character*(*) oldfile, vext
      character*80 ext, newfile
      logical lexist
      integer file_error, flen, elen, choice, length,i,j
      character*80 thisfile, command, outline
      character*1 schoice
      character*3 sure
      LOGICAL PARINT

c move input into local variable - strip spaces
      write(ext,'(a)')vext
c
      thisfile = oldfile(1:length(oldfile))
      flen = length(thisfile)
      elen = length(ext)

 500  continue

      INQUIRE(FILE=thisfile(1:flen)//ext,EXIST=LEXIST,IOSTAT=FILE_ERROR)
      IF (LEXIST) THEN
        write(*,*)
     &  ' File called: '//thisfile(1:flen)//ext(1:elen)//' exists!'
c
c Eek. The file already exists. Print out a menu.
c
        WRITE(*,*)
 111    WRITE(*,*)' What do you wish to do? You can?'
        WRITE(*,*)'     1: Quit. Do not write anything!'
        WRITE(*,*)'     2: Change the name of the EXISTING file and '
        WRITE(*,*)'         write this data to: '
     &                   //thisfile(1:flen)//ext(1:elen)
        WRITE(*,*)'     3: Choose a new name for the data '//
     &       'to be saved.'
        WRITE(*,*)'     4: Write over the existing file.'
        WRITE(*,*)

c Get the menu choice from the user, and if it is not within the 
c   specified range, then get another.
        CHOICE=0
 112    schoice = ' '
        WRITE(*,'(x,a,$)')'Please input a choice (1-4) :'
        READ(*,'(a)')sCHOICE
        if (.not.parint(schoice,choice)) goto 112
        if (choice.lt.1.or.choice.gt.4) goto 112

c If the choice is: Quit
        IF (CHOICE.EQ.1) THEN
          newfile = ' '
          RETURN

c If the choice is to change the name of the existing file:
        ELSE IF (CHOICE.EQ.2) THEN
          WRITE(*,*)
          WRITE(*,*)' What shall '//thisfile(1:flen),' be renamed?'
          WRITE(*,'(a,$)')
     &    ' (Do not add the '//ext(1:elen)//' extension) :'
          READ(*,'(A)')newfile
          COMMAND=
     &     'mv '//thisfile(1:flen)//ext(1:elen)//
     &     ' '//NEWfile(1:LENGTH(NEWfile))//ext(1:elen)
          CALL SYSTEM(COMMAND)

c Print a helpful statement or two
          write(*,*)
          write(*,'(x,a)')
     &    thisfile(1:flen)//ext(1:elen)//
     &    ' renamed to: '//NEWfile(1:LENGTH(NEWfile))//ext(1:elen)
          goto 500

c If the choice is to choose a new file name:
        ELSE IF (CHOICE.EQ.3) THEN
          WRITE(*,*)
c Get the new name from the user
          write(*,*)' Please input a new name for '//thisfile(1:flen)
          WRITE(*,'(a,$)')
     &     ' (Do not add the '//ext(1:elen)//' extension) :'
          READ(*,'(A)')NEWfile
          WRITE(*,*)
c Some helpful comments.
          write(*,*)
     &    ' Writing new data to: '//newfile(1:length(newfile))
     &     //ext(1:elen)
          WRITE(*,*)
          WRITE(*,*)' NOTE: Pulsar Name has NOT changed. If you '//
     &          ' want to reload this data later, you will have '//
     &          ' to change the Pulsar Name to: '//
     &          NEwfile(1:LENGTH(NEWfile))
          WRITE(*,*)
c
          thisfile=  newfile
          flen = length(newfile)
          GO TO 500

c If the choice is to write over the existing file
        ELSE IF (CHOICE.EQ.4) THEN
c Make sure that this is what the user REALLY WANTS TO DO.
          WRITE(*,*)
          OUTLINE=' WARNING. This action will irrevocably delete'//
     &          ' the existing file '//thisfile(1:flen)
          WRITE(*,*)OUTLINE
          OUTLINE=' ARE YOU SURE YOU WANT TO DO THIS?'
          WRITE(*,*)OUTLINE
          WRITE(*,'('' Type "YES" or "yes": ''$)')
          READ(*,'(A)')SURE
c If user doesn't want to do this, go back to the menu and choose again
          IF ((SURE.NE.'YES').AND.(SURE.NE.'yes')) THEN
            GO TO 111
          ENDIF
c If he does. Use UNIX to erase the file, and then save the ephemeris
c   in memory.
          COMMAND='rm '//thisfile(1:flen)//ext(1:elen)
          CALL SYSTEM(COMMAND)
c if it's a wav file - also delete associated wavdat (dangerous!)
          if (ext(1:4) .eq. '.wav') then
            COMMAND='rm '//thisfile(1:flen)//'.wavdat'
            CALL SYSTEM(COMMAND)
          endif
c Some comments...
          OUTLINE=thisfile(1:flen)//ext(1:elen)//
     &     ' has been erased. If you have made a mistake. Tough!'
          WRITE(*,*)OUTLINE
          WRITE(*,*)
          OUTLINE=
     &   ' Writing new version of '//thisfile(1:flen)//ext(1:elen)
          WRITE(*,*)OUTLINE
c
          GO TO 500
c
        ENDIF
      endif
c
c Check if there is an IO error.
      IF (FILE_ERROR.GT.0) THEN
        WRITE(*,*)' There is an IO error. Number=,',FILE_ERROR
        newfile = ' ' 

c exit with acceptable unused file name
      else
        newfile = thisfile
      endif

      return
      END






