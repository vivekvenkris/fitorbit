cDECK OPSCMD
c
c **************************************************************
c **************************************************************
c
c PASSES THE COMMANDS CONTAINED IN CMDINP(JINP) THROUGH CMDINP(NINP)
c TO THE OPERATING SYSTEM COMMAND LINE INTERPRETER.
c
c THIS ROUTINE IS INSTALLATION DEPENDENT
c
c FX/FORTRAN VERSION
c
      subroutine opscmd(cmdinp, jinp, ninp)
      character cmdinp(*)*(*)
      character cmdbuf*255
      logical comstr
c
c     IF COMMAND WORDS HAVE BEEN SUPPLIED,
c     THEN SPAWN A SUBPROCESS TO PERFORM THE COMMAND
c
      integer istat, system
c
c        FORM THE WORDS INTO A SINGLE COMMAND LINE.
c
      if (jinp .le. ninp) then
      cmdbuf = cmdinp(jinp)
      do i = 1, ninp - jinp
      cmdbuf = (cmdbuf(1:length(cmdbuf)) // ' ') // cmdinp(jinp + i)
c
c        SPAWN THE COMMAND
c
      end do
      istat = system(cmdbuf)
c
c     SET THE COMMAND WORD POINTER
c
      end if
c
      jinp = ninp + 1
c
c END OF SUBROUTINE OPSCMD
c
      return 
      end
