cDECK OUTPTN
c     
c     **************************************************************
c     **************************************************************
c     
c     WRITES OUT NL LINES OF TEXT CONTAINED IN THE CHARACTER ARRAY LINE(NL)
c     TO LOGICAL UNIT LU, PROVIDED THAT IT IS ENABLED ( LU > 0 ).
c     LINES WHICH ARE LONGER THAN THE RECORD LENGTH OF THE UNIT ARE
c     CONTINUED BY HYPHENATING. IF THERE IS INSUFFICIENT SPACE ON
c     THE CURRENT PAGE FOR NL LINES, THEN A NEW PAGE IS GENERATED.
c     LOGICAL UNIT LU SHOULD PREVIOUSLY HAVE BEEN OPENED FOR OUTPUT 
c     USING THE LIBRARY ROUTINE OPNFIL.
c     IF OUTPUT IS TO LU=6, OLAFDIR:OUTLIN IS CALLED INSTEAD.
c     
      subroutine outptn(lu, line, nl)
      character line(*)*(*), fmt*8, prmtcc*1
      logical hyphen, overpr
c     
      parameter (iwordl = 10)
c     
      include 'PSRLIB.DEF'
c     DEFINITIONS OF THE PULSAR LIBRARY GLOBALS
c     
c     PROGRAM LOGICAL UNITS
c     
      if ((lu .le. 0) .or. (nl .le. 0)) return 
c     
c     EVALUATE THE NUMBER OF CHARACTERS IN
c     THE LINE AND SET THE FORMAT
c     
      do 10 il = 1, nl
         nchr = max(1,length(line(il)))
c     
c     CHECK THE CARRIAGE CONTROL CHARACTER.
c     
         fmt = '(150A)'
         overpr = line(il)(1:1) .eq. '+'
c     
c     FORCE A NEW PAGE IF IT IS A '1'
c     
         if (line(il)(1:1) .eq. '1') then
            lurec(lu) = lupagl(lu)
            line(il)(1:1) = ' '
c     
c     INSERT IT IN THE FORMAT IF IT IS A PROMPT.
c     
         else if (line(il)(1:1) .eq. prmtcc()) then
c     
c     UPDATE THE LINE COUNTER TO REFLECT OTHER CARRIAGE CONTROL 
c     CHARACTER.
c     
            fmt = ((fmt(1:1) // prmtcc()) // ',') // fmt(2:)
         else if (overpr) then
            lurec(lu) = lurec(lu) - 1
         else if (line(il)(1:1) .eq. '0') then
            lurec(lu) = lurec(lu) + 1
         end if
c     
c     LABEL 1000 IS THE START OF THE LINE CONTINUATION LOOP
c     
         n2 = 0
 1000    continue
c     
c     START AND FINISH CHARACTER POSITIONS
c     
         hyphen = .false.
         n1 = n2 + 1
         n2 = n2 + lurecl(lu)
c     
c     NO CONTINUATIONS NEEDED
c     
         if (n2 .ge. nchr) then
            n2 = nchr
c     
c     CONTINUATIONS REQUIRED
c     TRY TO FIND A SPACE NEAR END OF LINE
c     
         else
            do 20 i = n2, n2 - iwordl, -1
               if (line(il)(i:i) .eq. ' ') then
                  goto 1001
               end if
c     
c     MUST HYPHENATE
c     
 20         continue
            i = n2 - 2
            hyphen = .true.
 1001       continue
            n2 = i
c     
c     INCREMENT LINE COUNT AND GET NEW PAGE IF NEEDED
c     
         end if
         lurec(lu) = lurec(lu) + 1
         if ((lupagl(lu) .gt. 0) .and.
     &       ((lupagl(lu) - lurec(lu)) .lt. nl)) 
     &       then
c
c            call outpag(lu)
c     
c     PRINT LINE, HYPHENATING IF NECESSARY AND PADDING OUT
c     WITH SPACES IF OVERPRINTING.
c     
         end if
         if (hyphen) then
            if (overpr) then
               write(unit=lu, fmt=fmt) line(il)(n1:n2), ' -',
     &             (' ',i = 1, ((
     &             lurecl(lu) - n2) + n1) - 1)
            else
               write(unit=lu, fmt=fmt) line(il)(n1:n2), ' -'
            end if
            hyphen = .false.
         else
            if (overpr) then
               write(unit=lu, fmt=fmt) line(il)(n1:n2),
     &             (' ',i = 1, ((lurecl(lu)
     &             - n2) + n1) - 1)
            else
               write(unit=lu, fmt=fmt) line(il)(n1:n2)
            end if
c     
c     CHANGE FORMAT FOR CONTINUATION LINES
c     
         end if
         if (n2 .ne. nchr) then
            fmt = '(1X,A,A)'
            goto 1000
c     
c     LABEL 10 IS THE END OF THE LINE OUTPUT LOOP
c     
         end if
c     
 10   continue
c     
c     END OF SUBROUTINE OUTPTN
c     
c     THE FOLLOWING ENTRY POINT IS USED TO FORCE A NEW PAGE ON UNIT LU
c     THE NEXT TIME OUTPTN IS CALLED.
c     NCOND ALLOWS CONDITIONAL PAGING. IF IT IS NON-ZERO THEN A NEW PAGE IS
c     ONLY GENERATED IF THERE IS INSUFFICIENT ROOM ON THE CURRENT PAGE
c     FOR NCOND LINES.
c     
      return 
c     
c     CHECK UNIT IS ENABLED.
c     
      entry page(lu, ncond)
c     
c     CONDITIONAL PAGE THROW.
c     
      if (lu .gt. 0.) then
         if (ncond .gt. 0.) then
            if ((lupagl(lu) - lurec(lu)) .lt. ncond) then
               lurec(lu) = lupagl(lu)
            end if
c     
c     UNCONDITIONAL PAGE THROW.
c     
         else
            lurec(lu) = lupagl(lu)
         end if
c     
      end if
c     
c     END OF SUBROUTINE PAGE
c     
      return 
      end




