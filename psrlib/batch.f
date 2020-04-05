c
c This routine returns whether or not the program that called it
c is running in a batch or interactive environment.  It returns
c .TRUE. if in batch, or .FALSE. if in interactive.
c 
c     Version 2.0   7th March, 1988   Alliant Concentrix
c
c In this case "batch" is determined by obtaining the terminal name
c associated with stdout.  If this is not a terminal, then the job
c is assumed to be running in batch.  Thus output redirection to a
c file results in BATCH returning .TRUE..
c
c Declare local variables.
c
      logical function batch()
      character term*32
C$pragma C(termnl)
c
c Obtain the terminal name (if any).
c
      integer lterm
c
c Is there one?
c
      call termnl(term, lterm)
c
c End of logical function BATCH.
c
      batch = term(1:lterm) .eq. ' '
      end














