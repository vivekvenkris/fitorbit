c----------------------------------------------------------
	block data init_pgch
	include 'dialogstructure'
	data  actual_pgch/1.0/
	end
c-----------------------------------------------------------
        subroutine dialog_ste(str,abort)
c
c subroutine to accept a static text string
c
        character*(*) str
        logical place,abort,view

 10     call dialog931(str,str,place,abort,view)
        if (place) then
           goto 999
        else if (view) then
          call dialog931(str,str,place,abort,view)
        else if (abort) then
          goto 999
        end if
        goto 10
 999    continue
        return
        end
c-----------------------------------------------------------
        subroutine dialog_but(str,abort)
c
c subroutine to accept a static text string
c
        character*(*) str
        logical place,abort,view

 10     call dialog933(str,str,place,abort,view)
        if (place) then
           goto 999
        else if (view) then
          call dialog933(str,str,place,abort,view)
        else if (abort) then
          goto 999
        end if
        goto 10
 999    continue
        return
        end
c-----------------------------------------------------------
        subroutine dialog_rad(x,y,str,len)
c
c subroutine to accept a test string
c
        integer len
        real x,y,xx,yy
        character*1 ans
        character*(*) str
        integer curse, pgcurse
        external pgcurse

        ans = ' '
        len = 0
        call pgsfs(2)
        call dialog_circle(x,y)
        do while (len.lt.32 .and. ans.ne.'!')
          curse = pgcurse(xx,yy,ans)
          if (ans.ne.'!') then
             len=len+1
             str=str(1:len-1)//ans
          end if
          call write_letter(x,y,str(1:len),len)
        end do

        return
        end
c-----------------------------------------------------------
        subroutine dialog_che(x,y,str,len)
c
c subroutine to accept a test string
c
        integer len,pgcurse,curse
        real x,y,xx,yy
        character*1 ans
        character*(*) str
        external pgcurse

        ans = ' '
        len = 0
        call pgsfs(2)
        call pgrect(x-2.0,x-1.0,y,y+1.0)
        do while (len.lt.32 .and. ans.ne.'!')
          curse = pgcurse(xx,yy,ans)
          if (ans.ne.'!') then
             len=len+1
             str=str(1:len-1)//ans
          end if
          call write_letter(x,y,str(1:len),len)
        end do

        return
        end
c-----------------------------------------------------------
        subroutine dialog_dou(x,y,str,len,max)
c
c subroutine to accept a test string
c
        integer len,max,pgcurse,curse
        real x,y,xx,yy
        character*1 ans
        character*(*) str
        external pgcurse

        ans = ' '
        len = 0
        do while (len.lt.32 .and. ans.ne.'!')
          curse = pgcurse(xx,yy,ans)
          if (ans.ne.'!') then
             len=len+1
             str=str(1:len-1)//ans
          end if
          call write_letter(x,y,str(1:len),len)
        end do
        max = 24

        return
        end
c-----------------------------------------------------------
        subroutine dialog_out(x,y,x2,y2,ci,lw)
c
c subroutine to accept a test string
c
        integer ci,lw,pgcurse,curse
        real x,y,x2,y2,xx,yy
        character*1 ans
        external pgcurse

        ans = ' '
        curse = pgcurse(x,y,ans)
        curse = pgcurse(x2,y2,ans)
        call pgsfs(2)
        call pgsci(1)
        call pgrect(x,x2,y,y2)
c colour index
        xx=x2
        yy=y2
        curse = pgcurse(xx,yy,ans)
        read(ans,'(i1)')ci
c line width
        curse = pgcurse(xx,yy,ans)
        read(ans,'(i1)')lw
        return
        end

c-----------------------------------------------------------
        subroutine dialog_plo(x,y,x2,y2)
c
c subroutine to accept a test string
c
        real x,y,x2,y2

        call pgsfs(2)
        call pgsci(5)
        call pgrect(x,x2,y,y2)
        call pgtext(min(x,x2)+1.0,(y+y2)/2.0,'Plotting area')
        call pgsci(1)
        end

c-----------------------------------------------------------
        subroutine dialog_rea(x,y,str,len,max)
c
c subroutine to accept a test string
c
        integer len,max,pgcurse,curse
        real x,y,xx,yy
        character*1 ans
        character*(*) str
        external pgcurse

        ans = ' '
        len = 0
        do while (len.lt.32 .and. ans.ne.'!')
          curse = pgcurse(xx,yy,ans)
          if (ans.ne.'!') then
             len=len+1
             str=str(1:len-1)//ans
          end if
          call write_letter(x,y,str(1:len),len)
        end do
        max = 14

        return
        end
c-----------------------------------------------------------
        subroutine dialog_int(x,y,str,len,max)
c
c subroutine to accept a test string
c
        integer len,max,pgcurse,curse
        real x,y,xx,yy
        character*1 ans
        character*(*) str
        external pgcurse

        ans = ' '
        len = 0
        do while (len.lt.32 .and. ans.ne.'!')
          curse = pgcurse(xx,yy,ans)
          if (ans.ne.'!') then
             len=len+1
             str=str(1:len-1)//ans
          end if
          call write_letter(x,y,str(1:len),len)
        end do
        max=12

        return
        end
c------------------------------------------------------------
        subroutine dialog_edi(x,y,str,len,max)
c
c subroutine to accept a test string
c
        integer len,max,pgcurse,curse
        real x,y,xx,yy
        character*1 ans
        character*(*) str
        external pgcurse

        ans = ' '
        len = 0
        do while (len.lt.32 .and. ans.ne.'!')
          curse = pgcurse(xx,yy,ans)
          if (ans.ne.'!') then
             len=len+1
             str=str(1:len-1)//ans
          end if
          call write_letter(x,y,str(1:len),len)
        end do

        max = 25
        return
        end
c---------------------------------------------------------------
        Subroutine write_text(x,y,str,len)
c
c writes a string str of length len to the screen at posn x,y
c letters are evenly spaced and in predictable places so that
c a position can be located easily in the string with the cursor.
c
        real x,y
        integer len,i
        character*(*) str

        do i=1,len
          call write_letter(x,y,str,i)
        end do
        end
c---------------------------------------------------------------
        Subroutine pretty_text(x,y,str,len,fjust)
c
c writes a string str of length len to the screen at posn x,y
c letters are evenly spaced and in predictable places so that
c a position can be located easily in the string with the cursor.
c
        real x,y,fjust
        integer len
        character*(*) str
        call pgptext(x+fjust*len,y,0.0,fjust,str(1:len))
        end
c---------------------------------------------------------------
        Subroutine write_letter(x,y,str,pos)
c
c writes the pos'th letter of string str to the screen at
C posn x+pos-1,y
c
        real x,y
        integer pos
        character*(*) str

        call pgtext(x+pos,y,str(pos:pos))

        end
c---------------------------------------------------------------
        subroutine clear_text(x,y,istart,ifinish)
c
c clears a space where text will appear from the istart'th
c to the ifinish'th position in the string.
c
        real x,y
        integer istart,ifinish

        call pgsci(0)
        call pgsfs(1)
        call pgrect(x+istart,x+ifinish+1.0,y-0.3,y+1.1)
        call pgsci(1)

        end
c---------------------------------------------------------------------
        subroutine display_ste(static,clear,text)
c
c displays the pieces of text from istart to ifinish in the dialog.
c
        include 'dialogstructure'
        type(dstatictex) static
        logical clear,text

        call pgsch(actual_pgch)
          if (clear) then
            call clear_text(static%x,static%y,1,static%label_len)
          end if
          if (text) call
     & pretty_text(static%x,static%y,static%label,
     & static%label_len,0.0)

        end
c---------------------------------------------------------------------
        subroutine display_vte(variable,clear,text)
c
c displays the pieces of text from istart to ifinish in the dialog.
c
        include 'dialogstructure'
        type(dvartex) variable
        logical clear,text

        call pgsch(actual_pgch)
          if (clear) then
        call clear_text(variable%x,variable%y,0,variable%label_len)
          end if
          if (text) call
     & pretty_text(variable%x,variable%y,variable%label,
     & variable%label_len,0.0)

        end
c---------------------------------------------------------------------
        subroutine display_but(but,clear,text)
c
c displays a button
c
        include 'dialogstructure'
        type(dbutton) but
        real xl,yl
        logical clear,text

        call pglen(4,but%label(1:but%label_len),xl,yl)
        call pgsch(actual_pgch)
         if (clear) then
            call pgsci(0)
            call pgsfs(1)
            call pgrect(but%x-0.4,but%x+xl+0.4,
     &but%y-0.5,but%y+1.2)
         end if
         call pgsci(butcol)
         if (text)call
     & pretty_text(but%x,but%y,but%label,but%label_len,0.0)
         call pgsfs(2)
         call pgrect(but%x-0.2,but%x+xl+0.2,
     &but%y-0.3,but%y+1.0)
         call pgrect(but%x-0.4,but%x+xl+0.4,
     & but%y-0.5,but%y+1.2)
         call pgsci(1)
        end
c---------------------------------------------------------------------
        subroutine highlight_button(but)
c
c displays a button
c
        include 'dialogstructure'
        type(dbutton) but
        real xl,yl

        call pglen(4,but%label(1:but%label_len),xl,yl)
        call pgsch(actual_pgch)

         call pgsci(butcol)
         call pgsfs(1)
         call pgrect(but%x-0.2,but%x+xl+0.2,
     &but%y-0.3,but%y+1.0)
         call pgsci(0)
         call pretty_text(but%x,but%y,but%label,but%label_len,0.0)
         call pgsci(1)
        end
c---------------------------------------------------------------------
        subroutine display_out(out,clear,text)
c
c displays a box's outline
c
        include 'dialogstructure'
        type(doutline) out
        logical clear,text
c
c	Overide out%colour if .dialog file specified 1 for outline
c	DRL 93/06/21 so that boxes can be seen on monochrome sets.
c
	if (outcol.eq.1) then
	  call pgsci(outcol) 
	else
          call pgsci(out%colour)
	endif
        call pgslw(out%linewidth)
        call pgsfs(2)
        call pgrect(out%x1,out%x2,out%y1,out%y2)
        call pgsci(1)
        call pgslw(1)
        end
c---------------------------------------------------------------------
        subroutine display_plo(plot,clear,text)
c
c displays a plot
c
        include 'dialogstructure'
        type(dplot) plot
        logical clear,text

        call pgsfs(1)
        call pgsci(0)
        call pgrect(plot%x1,plot%x2,plot%y1,plot%y2)
        call pgsci(1)

        continue
        end
c---------------------------------------------------------------------
        subroutine display_che(check,clear,text)
c
c displays a check box
c
        include 'dialogstructure'
        type(dcheck) check
        logical clear,text

          call pgsch(actual_pgch)
          if (check%value) then
            call pgsci(checol)
            call pgsfs(1)
            call pgrect(check%x-2.0,check%x-1.0,
     & check%y,check%y+1.0)
          else
            call pgsci(0)
            call pgsfs(1)
            call pgrect(check%x-2.0,check%x-1.0,
     & check%y,check%y+1.0)
          end if
          call pgsfs(2)
          call pgsci(1)
          call pgrect(check%x-2.0,check%x-1.0,check%y,check%y+1.0)

          if (clear)
     &call clear_text(check%x,check%y,1,check%label_len)
          if (text) call
     & pretty_text(check%x,check%y,check%label,check%label_len,0.0)

        end
c--------------------------------------------------------------
        subroutine display_rad(radio,clear,text)
c
c displays a radio control
c
        include 'dialogstructure'
        type(dradio) radio
        logical clear,text
        call pgsch(actual_pgch)
        if (radio%value) then
            call pgsci(radcol)
            call pgsfs(1)
            call dialog_circle(radio%x,radio%y)
        else
            call pgsci(0)
            call pgsfs(1)
            call dialog_circle(radio%x,radio%y)
        end if
        call pgsfs(2)
        call pgsci(1)
        call dialog_circle(radio%x,radio%y)
        if (clear) call clear_text(radio%x,radio%y,1,radio%label_len)
        if (text) call  
     &pretty_text(radio%x,radio%y,radio%label,radio%label_len,0.0)
        end
c-----------------------------------------------------------------------
        subroutine display_edi(aaa,clear,text)
c
c displays editable text
c
        include 'dialogstructure'
        type(dedit) aaa
        integer length
        logical clear,text

          call pgsch(actual_pgch)
          if (clear) then
c            call clear_text(aaa%x,aaa%y,1,aaa%label_len)
            call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,
     &   1,aaa%maxlen,0)
          end if
          call pgsci(1)
          if (text) call
     &pretty_text(aaa%x,aaa%y,aaa%label,aaa%label_len,1.0)
          call draw_box(aaa%x,aaa%y,aaa%label_len,aaa%maxlen)
          aaa%str_len=length(aaa%value)
      call write_text_in_box(
     & aaa%x,aaa%y,aaa%label_len,aaa%value,
     & min(aaa%str_len,aaa%maxlen))

        end
c-----------------------------------------------------------------------
          subroutine draw_box(x,y,label_len,maxlen)
          real x,y
          integer label_len,maxlen
          call pgsfs(2)
          call pgsci(1)
          call pgrect(x+label_len+1.0,x+label_len+maxlen+1.0,
     & y-0.2,y+1.2)
          end
c-----------------------------------------------------------------------
        subroutine write_text_in_box(x,y,label_len,value,len)
        real x,y
        integer label_len,len,i,lll,length

        character*(*) value
        lll=length(value)
        do i = 1,min(len,lll)
          call write_letter_in_box(x,y,label_len,value,i)
        end do
        end
c-----------------------------------------------------------------------
        subroutine write_letter_in_box(x,y,label_len,value,pos)
        real x,y
        integer label_len,pos
        character*(*) value
        call pgtext(x+label_len+pos,y,value(pos:pos))
        end
c-----------------------------------------------------------------------
        subroutine highlight_letter_in_box(x,y,label_len,value,pos,max)
        real x,y
        integer label_len,pos,max
        character*(*) value
        if (pos.le.max) then
          call clear_text_in_box(x,y,label_len,pos,pos,0)
          call pgtext(x+label_len+pos,y,value(pos:pos))
        end if
        end
c-----------------------------------------------------------------------
        subroutine clear_text_in_box(x,y,label_len,pos1,pos2,ci)
        real x,y
        integer label_len,pos1,pos2,ci
        call pgsci(ci)
        call pgsfs(1)
        call pgrect(x+label_len+pos1,x+label_len+pos2+1.0,
     &y-0.2,y+1.2)
        end
c-----------------------------------------------------------------------
        subroutine clear_letter_in_box(x,y,label_len,pos)
        real x,y
        integer label_len,pos

        call pgsci(0)
        call pgsfs(1)
        call pgrect(x+label_len+pos-1.0,x+label_len+pos,
     &y-0.3,y+1.2)
        call pgsci(1)

        end
c-----------------------------------------------------------------------
        subroutine display_rea(aaa,clear,text)
c
c displays the reals from istart to ifinish in the dialog.
c
        include 'dialogstructure'
        integer length
        type(dreal) aaa
        logical clear,text

          call pgsch(actual_pgch)
          if (clear) then
c            call clear_text(aaa%x,aaa%y,1,aaa%label_len)
            call clear_text_in_box(
     & aaa%x,aaa%y,aaa%label_len,1,aaa%maxlen,0)
          end if
          call pgsci(1)
          if (text) call
     & pretty_text(aaa%x,aaa%y,aaa%label,aaa%label_len,1.0)
          call draw_box(aaa%x,aaa%y,aaa%label_len,aaa%maxlen)
          call convert_real_to_str(aaa%maxlen,aaa%value,aaa%str_val)
          aaa%str_len=min(aaa%maxlen,length(aaa%str_val))
      call
     &write_text_in_box(aaa%x,aaa%y,aaa%label_len,aaa%str_val,
     & aaa%str_len)
        end
c-----------------------------------------------------------------------
        subroutine display_dou(aaa,clear,text)
c
c displays the reals from istart to ifinish in the dialog.
c
        include 'dialogstructure'
        type(ddouble) aaa
        integer length
        logical clear,text

        character*(dia_max_vlab) cpostn,cjtime

          call pgsch(actual_pgch)
          if (clear)then
c             call clear_text(aaa%x,aaa%y,1,aaa%label_len)
             call clear_text_in_box(
     &aaa%x,aaa%y,aaa%label_len,1,aaa%maxlen,0)
          end if
          call pgsci(1)
          if (text) call
     &pretty_text(aaa%x,aaa%y,aaa%label,aaa%label_len,1.0)
          call draw_box(aaa%x,aaa%y,aaa%label_len,aaa%maxlen)
          if(aaa%type.eq.1) then 
             aaa%str_val=cpostn(aaa%value,5,0)
          elseif(aaa%type.eq.2) then
             aaa%str_val=cjtime(aaa%value,2)
          else
            call convert_real8_to_str(aaa%maxlen,aaa%value,aaa%str_val)
          endif
          aaa%str_len=min(aaa%maxlen,length(aaa%str_val))
        call
     &write_text_in_box(aaa%x,aaa%y,aaa%label_len,aaa%str_val,
     &aaa%str_len)

        end
c---------------------------------------------------------------------
        subroutine display_int(nnn,clear,text)
c
c displays the reals from istart to ifinish in the dialog.
c
        include 'dialogstructure'
        type(dinteger) nnn
        integer length
        logical clear,text

          call pgsch(actual_pgch)
         if (clear)then
c           call clear_text(nnn%x,nnn%y,1,nnn%label_len)
           call clear_text_in_box(nnn%x,nnn%y,nnn%label_len,
     &  1,nnn%maxlen,0)
         end if
          call pgsci(1)
          if (text)
     &call pretty_text(nnn%x,nnn%y,nnn%label,nnn%label_len,1.0)
          call draw_box(nnn%x,nnn%y,nnn%label_len,nnn%maxlen)
          call convert_int_to_str(nnn%maxlen,nnn%value,nnn%str_val)
          nnn%str_len=min(nnn%maxlen,length(nnn%str_val))
      call
     &  write_text_in_box(nnn%x,nnn%y,nnn%label_len,
     &   nnn%str_val,nnn%str_len)

        end
c---------------------------------------------------------------------
        subroutine dialog_circle(x,y)
        real x,y,xpts(12),ypts(12)
        integer i

        do i=1,12
          xpts(i)=x+0.7*cos((i*30.0+15.0)*3.141/180.)-1.3
          ypts(i)=y+0.5*sin((i*30.0+15.0)*3.141/180.)+0.4
        end do
        call pgpoly(12,xpts,ypts)
        return
        end

c--------------------------------------------------------------------
          subroutine convert_real_to_str(ilen,x,str)
          integer ilen
          real x
          character*(*) str
          character*20 FMT
          integer length,ilp,ill


          if(x.ne.0.0) then
             ill=log10(abs(x))
          else
             ill=0
          endif
          if(ill.lt.-4 .and. ilen.gt.7) then
             write(fmt,'(a,i2.2)') '(E',ilen
             ilp=ilen-6
          else
             write(fmt,'(a,i2.2)') '(F',ilen
             ilp=max(ilen-max(-1,ill)-2,0)
          endif
          if(x.lt.0.0)ilp=ilp-1

          if(ilp.ge.0) then
            write(fmt(length(fmt)+1:),'(a,i2.2,a)') '.',ilp,')'
            write(str,fmt)x
          else
             str='*****'
          endif
          call dialog_trim(str)
          end
c--------------------------------------------------------------------
          subroutine convert_real8_to_str(ilen,x,str)
          integer ilen
          real*8 x
          character*(*) str

          character*20 FMT
          integer length,ilp,ill

          if(x.ne.0.0) then
             ill=log10(abs(x))
          else
             ill=0
          endif
          if(ill.lt.-4 .and. ilen.gt.7) then
             write(fmt,'(a,i2.2)') '(E',ilen
             ilp=ilen-6
          else
             write(fmt,'(a,i2.2)') '(F',ilen
             ilp=max(ilen-max(-1,ill)-2,0)
          endif
          if(x.lt.0.0)ilp=ilp-1

          if(ilp.ge.0) then
            write(fmt(length(fmt)+1:),'(a,i2.2,a)') '.',ilp,')'
            write(str,fmt) x
          else
             str='*****'
          endif
          call dialog_trim(str)
          end

c---------------------------------------------------------------------
          subroutine convert_int_to_str(ilen,i,str)
          integer ilen
          integer i
          character*(*) str
          write(str,*)i
          call dialog_trim(str)
          end
c-------------------------------------------------------------------
          subroutine dialog_trim(str)
          character*(*) str
          integer count,len,length

          len=length(str)
          count=1
          do while (str(count:count).eq.' ' .and. count.lt.len)
            count=count+1
          end do
          str=str(count:max(len,1))
          end
c-------------------------------------------------------------------
         subroutine display_dialog(dialog,clear)
c
c  program displays the current dialog to pgplot_device
c
        logical clear
        include 'dialogstructure'
        real xmin,xmax,ymin,ymax
        type(dlg) dialog
        integer i
        if (dialog%dxmax.eq.0.) dialog%dxmax = 1.0
        if (dialog%dymax.eq.0.) dialog%dymax = 1.0

        xmin = 70.0*dialog%dxmin
        xmax = 70.0*dialog%dxmax
        ymin = 40.0*dialog%dymin
        ymax = 40.0*dialog%dymax

c        call pgvport(dialog%dxmin,dialog%dxmax,dialog%dymin,
c     &            dialog%dymax)
c        call pgwindow(xmin,xmax,ymin,ymax)
        call pgvport(0.0,1.0,0.0,1.0)
        call pgwindow(0.0,70.0,0.0,40.0)
        call pgsci(0)
        call pgsfs(1)
        call pgrect(xmin,xmax,ymin,ymax)
        call pgsci(outcol)
        call pgslw(3)
        call pgsfs(2)
        call pgrect(xmin,xmax,ymin,ymax)
        call pgslw(1)
        call pgsfs(1)
        call pgsci(1)
        call pgpoint(1,min(xmax-0.5,69.5),min(39.5,ymax-0.5),18)
        call pgpoint(1,max(xmin+0.5,0.5),min(ymax-0.5,39.5),28)
        call pgpoint(1,max(xmin+0.5,0.5),min(ymax-0.5,39.5),29)
        call pgbbuf
        do i=1,dialog%ndrad
          call display_rad(dialog%drad(i),clear,.true.)
        end do
        do i=1,dialog%ndche
          call display_che(dialog%dche(i),clear,.true.)
        end do
        do i=1,dialog%ndint
          call display_int(dialog%dint(i),clear,.true.)
        end do
        do i=1,dialog%ndrea
          call display_rea(dialog%drea(i),clear,.true.)
        end do
        do i=1,dialog%nddou
          call display_dou(dialog%ddou(i),clear,.true.)
        end do
        do i=1,dialog%ndedi
          call display_edi(dialog%dedi(i),clear,.true.)
        end do
        do i=1,dialog%ndbut
          call display_but(dialog%dbut(i),clear,.true.)
        end do
        do i=1,dialog%ndout
          call display_out(dialog%dout(i),clear,.true.)
        end do
        do i=1,dialog%ndplo
          call display_plo(dialog%dplo(i),clear,.true.)
        end do
        do i=1,dialog%ndste
          call display_ste(dialog%dste(i),clear,.true.)
        end do
        do i=1,dialog%ndvte
          call display_vte(dialog%dvte(i),clear,.true.)
        end do
        call pgebuf
        end
c-------------------------------------------------------------------
         subroutine patch_dialog(dialog,lxmin,lxmax,lymin,lymax)
c
c  program displays the current dialog to pgplot_device
c
        real lymin,lymax,lxmin,lxmax
        logical clear
        include 'dialogstructure'
        real xmin,xmax,ymin,ymax
        type(dlg) dialog
        integer i

        xmin = 70.0*lxmin
        xmax = 70.0*lxmax
        ymin = 40.0*lymin
        ymax = 40.0*lymax

        call pgvport(lxmin,lxmax,lymin,lymax)
        call pgwindow(xmin,xmax,ymin,ymax)
        call pgsci(0)
        call pgsfs(1)
        call pgrect(xmin,xmax,ymin,ymax)
        call pgsci(1)
c        call pgvport(dialog%dxmin,dialog%dxmax,dialog%dymin,
c     &     dialog%dymax)
c        call pgwindow(70.0*dialog%dxmin,70.0*dialog%dxmax,
c     &          40.0*dialog%dymin,40.0*dialog%dymax)
        do i=1,dialog%ndste
          if (dialog%dste(i)%y+1.0.gt.ymin) then
           if (dialog%dste(i)%y-1.lt. ymax)  then
            if (dialog%dste(i)%x-1 .lt. xmax) then
         if (dialog%dste(i)%x+dialog%dste(i)%label_len+2.gt.xmin) 
     &        call display_ste(dialog%dste(i),clear,.true.)
            end if
           end if
          end if
        end do
        do i=1,dialog%ndvte
          if (dialog%dvte(i)%y+1.0.gt.ymin) then
           if (dialog%dvte(i)%y-1.lt. ymax)  then
            if (dialog%dvte(i)%x-1 .lt. xmax) then
            if (dialog%dvte(i)%x+dialog%dvte(i)%maxlen+2.gt.xmin) 
     &        call display_vte(dialog%dvte(i),clear,.true.)
            end if
           end if
          end if
        end do
        do i=1,dialog%ndrad
          if (dialog%drad(i)%y+1.0.gt.ymin) then
           if (dialog%drad(i)%y-1 .lt. ymax) then
            if (dialog%drad(i)%x-3 .lt. xmax) then
         if (dialog%drad(i)%x+dialog%drad(i)%label_len+2.gt.xmin) 
     &          call display_rad(dialog%drad(i),clear,.true.)
            end if
           end if
          end if 
        end do
        do i=1,dialog%ndche
          if (dialog%dche(i)%y+1.0.gt.ymin)  then
           if (dialog%dche(i)%y-1.lt. ymax ) then
            if (dialog%dche(i)%x-3 .lt. xmax) then
             if(dialog%dche(i)%x+dialog%dche(i)%label_len+2
     &             .gt.xmin) 
     &           call display_che(dialog%dche(i),clear,.true.)
            end if
           end if
          end if
        end do
        do i=1,dialog%ndint
          call display_int(dialog%dint(i),clear,.true.)
        end do
        do i=1,dialog%ndrea
          call display_rea(dialog%drea(i),clear,.true.)
        end do
        do i=1,dialog%nddou
          call display_dou(dialog%ddou(i),clear,.true.)
        end do
        do i=1,dialog%ndedi
          call display_edi(dialog%dedi(i),clear,.true.)
        end do
        do i=1,dialog%ndbut
          if (dialog%dbut(i)%y+1.0.gt.ymin) then
           if (dialog%dbut(i)%y-1 .lt. ymax) then 
            if (dialog%dbut(i)%x-3 .lt. xmax) then
             if (dialog%dbut(i)%x+dialog%dbut(i)%label_len
     &            +2.gt.xmin) 
     &          call display_but(dialog%dbut(i),clear,.true.)
            end if
           end if
          end if
        end do
c always do outlines Is this just being lazy??
        do i=1,dialog%ndout
          call display_out(dialog%dout(i),clear,.true.)
        end do
cc when patching, don't erase plots.
c        do i=1,dialog%ndplo
c          call display_plo(dialog%dplo(i),clear,.true.)
c        end do
        call pgvport(0.0,1.0,0.0,1.0)
        call pgwindow(0.0,70.0,
     &          0.0,40.0)
        end
c---------------------------------------------------------------------
        subroutine delete_widget(dialog,w,a)
        include 'dialogstructure'
        type(dlg) dialog
        integer w,a,i

        if (w.eq.1) then
         do i=a,dialog%ndste-1
           dialog%dste(i)=dialog%dste(i+1)
         end do
         dialog%ndste=dialog%ndste-1
        else if (w.eq.2) then
         do i=a,dialog%ndvte-1
           dialog%dvte(i)=dialog%dvte(i+1)
         end do
         dialog%ndvte=dialog%ndvte-1
        else if (w.eq.3) then
         do i=a,dialog%ndrad-1
           dialog%drad(i)=dialog%drad(i+1)
         end do
         dialog%ndrad=dialog%ndrad-1
        else if (w.eq.4) then
         do i=a,dialog%ndche-1
           dialog%dche(i)=dialog%dche(i+1)
         end do
         dialog%ndche=dialog%ndche-1
        else if (w.eq.5) then
         do i=a,dialog%ndint-1
           dialog%dint(i)=dialog%dint(i+1)
         end do
         dialog%ndint=dialog%ndint-1
        else if (w.eq.6) then
         do i=a,dialog%ndrea-1
           dialog%drea(i)=dialog%drea(i+1)
         end do
         dialog%ndrea=dialog%ndrea-1
        else if (w.eq.7) then
         do i=a,dialog%nddou-1
           dialog%ddou(i)=dialog%ddou(i+1)
         end do
         dialog%nddou=dialog%nddou-1
        else if (w.eq.8) then
         do i=a,dialog%ndedi-1
           dialog%dedi(i)=dialog%dedi(i+1)
         end do
         dialog%ndedi=dialog%ndedi-1
        else if (w.eq.9) then
         do i=a,dialog%ndbut-1
           dialog%dbut(i)=dialog%dbut(i+1)
         end do
         dialog%ndbut=dialog%ndbut-1
        else if (w.eq.10) then
         do i=a,dialog%ndplo-1
           dialog%dplo(i)=dialog%dplo(i+1)
         end do
         dialog%ndplo=dialog%ndplo-1
        else if (w.eq.11) then
         do i=a,dialog%ndout-1
           dialog%dout(i)=dialog%dout(i+1)
         end do
         dialog%ndout=dialog%ndout-1
        end if
        end
c-------------------------------------------------------------------
        subroutine dialog_set_vport(dialog,n)
        include 'dialogstructure'
        type(dlg) dialog
        integer n

       call pgvport(min(dialog%dplo(n)%x1/70.0,dialog%dplo(n)%x2/70.0),
     &   max(dialog%dplo(n)%x1/70.0,dialog%dplo(n)%x2/70.0),
     &   min(dialog%dplo(n)%y1/40.0,dialog%dplo(n)%y2/40.0),
     &   max(dialog%dplo(n)%y1/40.0,dialog%dplo(n)%y2/40.0))
        return
        end
c-------------------------------------------------------------------
        subroutine dialog_erase_plot(dialog,n)
        include 'dialogstructure'
        type(dlg) dialog
        integer n
        real xx1,yy1,xx2,yy2

        call pgsfs(1)
        call pgsci(0)

        xx1=min(dialog%dplo(n)%x1,dialog%dplo(n)%x2)
        xx2=max(dialog%dplo(n)%x1,dialog%dplo(n)%x2)
        yy1=min(dialog%dplo(n)%y1,dialog%dplo(n)%y2)
        yy2=max(dialog%dplo(n)%y1,dialog%dplo(n)%y2)
        call pgrect(xx1-0.08*(xx2-xx1),xx2+0.08*(xx2-xx1),
     &             yy1-0.08*(yy2-yy1),yy2+0.08*(yy2-yy1))
        call pgsci(1)
        return
        end
c-------------------------------------------------------------------
c-------------------------------------------------------------------
        subroutine new_erase_plot(dialog,n,xmin,xmax,ymin,ymax)
        include 'dialogstructure'
        type(dlg) dialog
        integer n
        real xx1,yy1,xx2,yy2,xmin,xmax,ymin,ymax

        call dialog_reset
        call pgsfs(1)
        call pgsci(0)

        xx1=min(dialog%dplo(n)%x1,dialog%dplo(n)%x2)
        xx2=max(dialog%dplo(n)%x1,dialog%dplo(n)%x2)
        yy1=min(dialog%dplo(n)%y1,dialog%dplo(n)%y2)
        yy2=max(dialog%dplo(n)%y1,dialog%dplo(n)%y2)
        call pgrect(xx1-xmin*(xx2-xx1),xx2+xmax*(xx2-xx1),
     &             yy1-ymin*(yy2-yy1),yy2+ymax*(yy2-yy1))
        call pgsci(1)
        return
        end
c-------------------------------------------------------------------
        subroutine dialog_reset
        call pgvport(0.0,1.0,0.0,1.0)
        call pgwindow(0.0,70.0,0.0,40.0)
        return
        end
c------------------------------------------------------------------------
        subroutine dialog_set_window(dialog,n,x1,x2,y1,y2)
        include 'dialogstructure'
        integer n
        real x1,x2,y1,y2
        type(dlg) dialog

        dialog%dplo(n)%vx1=x1
        dialog%dplo(n)%vy1=y1
        dialog%dplo(n)%vx2=x2
        dialog%dplo(n)%vy2=y2
        end
C------------------------------------------------------------------------
        subroutine dialog_window(dialog,n,xmin,xmax,ymin,ymax)
        integer n
        real xmin,xmax,ymin,ymax
        include 'dialogstructure'
        type(dlg) dialog
        call dialog_erase_plot(dialog,n)
        call dialog_set_vport(dialog,n)
        call dialog_set_window(dialog,n,xmin,xmax,ymin,ymax)
        call pgwindow(dialog%dplo(n)%vx1,
     &                dialog%dplo(n)%vx2,
     &                dialog%dplo(n)%vy1,
     &                dialog%dplo(n)%vy2)
        return
        end
C------------------------------------------------------------------------
        subroutine dialog_window_set(dialog,n,xmin,xmax,ymin,ymax)
        integer n
        real xmin,xmax,ymin,ymax
        include 'dialogstructure'
        type(dlg) dialog
        integer norder(dial_max)
        common /order/norder

        call dialog_order(norder,dial_max,n)
        call dialog_set_vport(dialog,n)
        call dialog_set_window(dialog,n,xmin,xmax,ymin,ymax)
        call pgwindow(dialog%dplo(n)%vx1,
     &                dialog%dplo(n)%vx2,
     &                dialog%dplo(n)%vy1,
     &                dialog%dplo(n)%vy2)
        return
        end
cc------------------------------------------------------------------------
        subroutine dialog_select_plot(dialog,n)
        integer n
        include 'dialogstructure'
        type(dlg) dialog
        integer norder(dial_max)
        common /order/norder

        call dialog_order(norder,dial_max,n)
        call dialog_set_vport(dialog,n)
        call pgwindow(dialog%dplo(n)%vx1,
     &                dialog%dplo(n)%vx2,
     &                dialog%dplo(n)%vy1,
     &                dialog%dplo(n)%vy2)
        return
        end
c----------------------------------------------------------------------
        subroutine dialog_order(norder,nsize,n)
        integer nsize
        integer norder(nsize),n,i,marker
c
c routine to manage the array order which lists the order in which
c the pgplot windows have been plotted
c
        do i=1,nsize
           if (norder(i).eq.n) marker = i
        end do
        if (marker.ne.0) then
           do i=marker,2,-1
              norder(i)=norder(i-1)
           end do
           norder(1)=n
        else
           do i=nsize,2,-1
              norder(i)=norder(i-1)
           end do
           norder(1)=n
        end if
        return
        end
c--------------------------------------------------------------
        subroutine renormalise_dialog(size)
        real size
        include 'dialogstructure'
        actual_pgch=size
        end
c----------------------------------------------------------------------
      subroutine dialog_inq_vport(dialog,n,xmin,xmax,ymin,ymax)
      include 'dialogstructure'
      type(dlg) dialog
      real xmin,xmax,ymin,ymax
      integer n
      
      xmin= min(dialog%dplo(n)%x1/70.0,dialog%dplo(n)%x2/70.0)
      xmax= max(dialog%dplo(n)%x1/70.0,dialog%dplo(n)%x2/70.0)
      ymin= min(dialog%dplo(n)%y1/40.0,dialog%dplo(n)%y2/40.0)
      ymax= max(dialog%dplo(n)%y1/40.0,dialog%dplo(n)%y2/40.0)

      return
      end
