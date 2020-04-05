cDECK TITLE
c
c **************************************************************
c **************************************************************
c
c PRODUCES A PROGRAM TITLE ON LOGICAL UNIT LU.
c THE TITLE CONSISTS OF THE PROGRAM DESCRIPTION, DESC,
c     AND A VERSION, VERS, IF PRESENT.
c THE TITLE IS USED TO INITIALIZE ALL BLANK PAGE HEADERS.
c A PAGE IS THEN OUTPUT TO LU, WITH THE PROGRAM TITLE ( UNDERLINED AND
c     CENTRE JUSTIFIED ), AND NLINES OF TEXT.
c
      subroutine title(lu, desc, vers, text, nlines)
      character desc*(*), vers*(*), text(*)*(*), outbuf(3)*133, titbuf*
     &80, fmt*20, ulchar*1, chdef*1
      parameter (ulchar = '-')
c
c     MAKE THE TITLE
c
      include 'PSRLIB.DEF'
c DEFINITIONS OF THE PULSAR LIBRARY GLOBALS
c
c     PROGRAM LOGICAL UNITS
c
      titbuf = desc
c
c     SET THE TITLE AS THE DEFAULT PAGE HEADER FOR ALL UNITS,
c     IF THE TITLE IS BLANK OR IF IT IS EQUAL TO THE UNINITIALIZED CHDEF
c.
c
      if (vers .ne. ' ') titbuf = (titbuf(1:length(titbuf)) // 
     &' Version ') // vers(1:length(vers))
      do 10 i = 1, maxlu
      if ((luhead(i) .eq. ' ') .or. (luhead(i) .eq. chdef)) luhead(i) = 
     &titbuf
c
c     SEND TITLE TO JOB DAYFILE.
c
   10 continue
c
c     RETURN IF LU IS NOT ENABLED
c
      call mesage(titbuf)
c
c     NEW PAGE
c
      if (lu .le. 0) return 
c
c     CENTRALISE THE TITLE, AND THE UNDERLINING
c
      call page(lu, 0)
      ioff = max(1,(lurecl(lu) - length(titbuf)) / 2)
      outbuf(1) = ' '
      outbuf(1)(ioff + 1:) = titbuf
      do i=1,length(titbuf)
         outbuf(2)(ioff+i:ioff+i)=ulchar
      enddo
c
c     OUTPUT THE TITLE
c
      outbuf(3) = ' '
c
c     OUTPUT THE TEXT
c
      call outptn(lu, outbuf, 3)
c
      call outptn(lu, text, nlines)
c
c     FORMAT
c
      return 
c
c END OF SUBROUTINE TITLE
c
  100 format(ss,'(T',i3,',(A1))')
      end
