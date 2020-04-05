c------------------------------------------------------------------------
        subroutine dialogstd(plot_no,px,py,ans,button_no,replot,dialog)
c written with the InterfaceBuilder
c       Matthew Bailes NRAL
c
c  standard dialog handler.
c  Input:  replot,dialog
c  Output: button_no,plot_no,x,y,dialog
c-------------------------------------------------------------------------
        implicit none
        integer replot,button_no,plot_no
        real px,py
        integer widget_type,array_index,i,status
        logical hit,firstmacro,executing_macro,defining_macro,exit
        real x,y,x2,y2
        real last_dxmin,last_dxmax,last_dymin,last_dymax
        character ans, pgans*30
        integer lpgans,lastbuttons(100)
        integer macrolun, dialogcounter
        include 'dialogstructure'
        type(dlg) dialog,copy_of_dialog,macrodialog
        external init_pgch
c        save last_dxmin,last_dxmax,last_dymin,last_dymax
        data firstmacro/.true./
        data dialogcounter/1/
        data executing_macro/.false./
        data defining_macro/.false./
        logical bypass
        save
C work out if dxmin and dymin etc are set, if not set to -0.1,1.1,-0.1,1.1
C This is in for upward compatability

        if (dialog%number .eq. 0) then
             dialog%number = dialogcounter
             dialogcounter = dialogcounter + 1
        end if

c        if (replot .ne. 0) then
c          if (dialog%number .eq. lastdialognumber) then
c             replot = 2
c          else
c             replot = 1
c          end if
c        end if

        if (dialog%dxmin.eq.dialog%dxmax) then
           dialog%dxmin = -0.1
           dialog%dxmax = 1.1
           dialog%dymin = -0.1
           dialog%dymax = 1.1
        end if

CPaul Harrison 19-MAR-1991 exit with button_no set to -1 if no device open
        button_no=-1
        CALL PGQINF('STATE',PGANS,LPGANS)
        IF(PGANS(1:LPGANS).NE.'OPEN') RETURN
        CALL DIALOG_RESET
        call pgask(.false.)
        hit=.false.
        exit = .false.

        if (replot.le.1) then
           call display_dialog(dialog,.false.)
           if (replot.eq.0) exit = .true.
        else if(replot.eq.2) then
           call display_diffs(dialog,copy_of_dialog)
        else if (replot.eq.3) then
           call patch_dialog(dialog,last_dxmin-0.01,last_dxmax+0.01,
     &                       last_dymin-0.01,last_dymax+0.01)
        else if (replot.eq.4) then
c           call display_diffs(dialog,copy_of_dialog)
c
c     DRL Nadger to plot the variable texts
c
           do i=1,dialog%ndvte
              call display_vte(dialog%dvte(i),.true.,.true.)
           enddo
           call patch_dialog(dialog,last_dxmin-0.01,last_dxmax+0.01,
     &                       last_dymin-0.01,last_dymax+0.01)
        else if (replot.eq.5) then
           call display_diffs(dialog,copy_of_dialog)
           call patch_dialog(dialog,last_dxmin-0.01,last_dxmax+0.01,
     &                       last_dymin-0.01,last_dymax+0.01)
           exit = .true.
        end if

        if (lastbuttons(dialog%number).gt.0) then
          call display_but(dialog%dbut(lastbuttons(dialog%number)),
     &         .true.,.true.)
          lastbuttons(dialog%number)=0
        end if

        do while (.not. exit)
          call next_event(hit,dialog,x,y,ans,widget_type,
     &         array_index,x2,y2,defining_macro,
     &         executing_macro,macrolun)
          if (widget_type.eq.12) then
c get next click + change dialog's position, erase it and redraw it
c in the new position.
             call move_dialog(dialog)
             call display_dialog(dialog,.false.)
          end if
          if (widget_type.eq.13) then
c macro called. If first obtain resources.
             if (firstmacro) then
              call read_dialog('psrdiadir','macro',macrodialog,status)
              firstmacro=.false.
             end if
c
c     New feature (DRL 93/01/08) if the star is clicked on using the
c     mouse (ans='A') then follow old menu system, otherwise you can
c     bypass the sub-dialog with the following keyboard codes:
c
c     D or d ---> define macro (i.e. the 'learn' button)
c     E or e ---> end macro    (i.e. the 'end-macro' button)
c     M or m ---> start a macro(i.e. the 'execute' button)
c     S or s ---> save values  (i.e. the 'save values' button)
c     L or l ---> load values  (i.e. the 'load values' button)
c
c     Once bypassed to the desired option, simply click again as 
c     normal
c
             bypass=(ans.eq.'D'.or.ans.eq.'d'.or.ans.eq.'E'
     &               .or.ans.eq.'e'.or.ans.eq.'M'.or.ans.eq.'m'
     &               .or.ans.eq.'S'.or.ans.eq.
     &               's'.or.ans.eq.'L'.or.ans.eq.'l') 
             if (bypass) then
c
c              decode the array index
c
                if (ans.eq.'D'.or.ans.eq.'d') array_index=1
                if (ans.eq.'E'.or.ans.eq.'e') array_index=2
                if (ans.eq.'S'.or.ans.eq.'s') array_index=3
                if (ans.eq.'L'.or.ans.eq.'l') array_index=4
                if (ans.eq.'M'.or.ans.eq.'m') array_index=5
             else
c     
c              display the menu...
c
               call display_dialog(macrodialog,.false.)
             endif
c             
c              if they want to move it, let them.
c
             hit = .false.
             do while (.not. hit)
               if (bypass) then
                  ans=' '
                  do while(ans.eq.' ')
                     call pgcurse(x,y,ans)
                  enddo
               else
                  call pgcurse(x,y,ans)
               endif
               if (.not.bypass) call where_clicked
     &            (macrodialog,x,y,widget_type,array_index,x2,y2)
               if (widget_type .eq. 9. or. bypass) then
                 if (array_index .eq. 1) then
                    call begin_macro(macrolun,ans)
                    defining_macro = .true.
                 else if (array_index .eq. 2) then
                    call end_macro(macrolun)
                    defining_macro = .false.
                 else if (array_index .eq. 3) then
                    call save_values(dialog,ans)
                 else if (array_index .eq. 4) then
                    copy_of_dialog=dialog
                    call load_values(dialog,ans)
                 else if (array_index .eq. 5) then
                    call execute_macro(macrolun,ans,executing_macro)
                 end if
                 if (.not.bypass) then
                   call highlight_button(macrodialog%dbut(array_index))
                   call pgupdt
                   widget_type = 0
                   call patch_dialog(dialog,macrodialog%dxmin,
     &             macrodialog%dxmax,macrodialog%dymin,
     &             macrodialog%dymax)
                   call display_diffs(dialog,copy_of_dialog)
                 endif
                 hit = .true.
               else if (widget_type .eq. 12) then
                 call patch_dialog(dialog,macrodialog%dxmin,
     &  macrodialog%dxmax,macrodialog%dymin,macrodialog%dymax)
                 call move_dialog(macrodialog)
                 call display_dialog(macrodialog,.false.)
               end if
           end do
        end if

        hit = .false.
        if (widget_type.eq.3) then
         if (.not. dialog%drad(array_index)%value) then
           do i=1,dialog%ndrad
            if (dialog%drad(array_index)%group
     &  .eq.dialog%drad(i)%group .and. dialog%drad(i)%value) then
          dialog%drad(i)%value = .false.
           call display_rad(dialog%drad(i),.false.,.false.)
          end if
           end do
           dialog%drad(array_index)%value = .true.
         end if
         call display_rad (dialog%drad(array_index),.false.,.false.)
        else if (widget_type.eq.4) then
            if (dialog%dche(array_index)%value)then
            dialog%dche(array_index)%value=.false.
            else
            dialog%dche(array_index)%value=.true.
            end if
          call display_che(dialog%dche(array_index),.false.,.false.)
        else if (widget_type.eq.5) then
          if (executing_macro) then
             call display_int(dialog%dint(array_index),.true.,.true.)
             hit=.false.
          else
            if (ans.eq.'X' .or. ans .eq. 'x') then
             call enter_int_easily(dialog%dint(array_index))
             hit = .false.
            else
             call enter_int(dialog%dint(array_index),x,y)
             hit = .true.
            end if
          end if
          if (defining_macro) write(macrolun,*)
     &        dialog%dint(array_index)%value
        else if (widget_type.eq.6) then
          if (executing_macro) then
             call display_rea(dialog%drea(array_index),.true.,.true.)
             hit=.false.
          else
           if (ans.eq.'X' .or. ans .eq. 'x') then
            call enter_rea_easily(dialog%drea(array_index))
            hit = .false.
           else
            call enter_rea(dialog%drea(array_index),x,y)
            hit = .true.
           end if
          end if
          if (defining_macro) write(macrolun,*)
     &        dialog%drea(array_index)%value
        else if (widget_type.eq.7) then
          if (executing_macro) then
             call display_dou(dialog%ddou(array_index),.true.,.true.)
             hit=.false.
          else
             if (ans.eq.'X' .or. ans .eq. 'x') then
               call enter_dou_easily(dialog%ddou(array_index))
               hit = .false.
             else
               call enter_dou(dialog%ddou(array_index),x,y)
               hit = .true.
             end if
          end if
          if (defining_macro) write(macrolun,*)
     &        dialog%ddou(array_index)%value
        else if (widget_type.eq.8) then
          if (executing_macro) then
             call display_edi(dialog%dedi(array_index),.true.,.true.)
             hit=.false.
          else
             if (ans.eq.'X' .or. ans .eq. 'x') then
               call enter_edi_easily(dialog%dedi(array_index))
               hit = .false.
             else
               call enter_edi(dialog%dedi(array_index),x,y)
               hit = .true.
             end if
          end if
          if (defining_macro) write(macrolun,'(a)')
     &        dialog%dedi(array_index)%value
        else if (widget_type.eq.9) then
          button_no=array_index
          exit = .true.
        else if (widget_type.eq.10) then
          plot_no=array_index
          px=x2
          py=y2
          exit = .true.
        end if

        end do

c leave subroutine

        if (widget_type.eq.10) button_no = 0
        if (widget_type.eq.9) then 
            plot_no = 0
            call highlight_button(dialog%dbut(button_no))
        end if
        if (replot.eq.0) then
            button_no = 0
            plot_no = 0
        end if

        lastbuttons(dialog%number) = button_no
        copy_of_dialog=dialog
        last_dxmin = dialog%dxmin
        last_dxmax = dialog%dxmax
        last_dymin = dialog%dymin
        last_dymax = dialog%dymax
        call pgupdt
        end
c-------------------------------------------------------------
        subroutine display_diffs(dialog,copy)
        include 'dialogstructure'
        type(dlg) dialog,copy
c
c  program redisplays the differences between the two dialogs
c
        logical clear
        integer i

        clear=.false.
        call pgvport(0.0,1.0,0.0,1.0)
        call pgwindow(0.0,70.0,0.0,40.0)
        call pgsci(1)
        call pgsfs(1)
        call pgbbuf
c
c check the first button. If it is different plot the whole dialog
c otherwise just plot the differences.
c
        if (dialog%dbut(1)%label.ne.copy%dbut(1)%label) then
c redraw the whole dialog%..
          call display_dialog(dialog,.false.)
        else
c otherwise check for differences and plot those which are different.
         do i=1,dialog%ndvte
           if (dialog%dvte(i)%label.ne.copy%dvte(i)%label) then
              call display_vte(dialog%dvte(i),.true.,.true.)
           end if
         end do
         do i=1,dialog%ndrad
           if (dialog%drad(i)%value .neqv. copy%drad(i)%value) then
              call display_rad(dialog%drad(i),.false.,.false.)
           end if
         end do
         do i=1,dialog%ndche
           if (dialog%dche(i)%value .neqv. copy%dche(i)%value) then
              call display_che(dialog%dche(i),.false.,.false.)
           end if
         end do
         do i=1,dialog%ndint
           if (dialog%dint(i)%value .ne. copy%dint(i)%value) then
              call display_int(dialog%dint(i),.true.,.false.)
           end if
         end do
         do i=1,dialog%ndrea
           if (dialog%drea(i)%value .ne. copy%drea(i)%value) then
              call display_rea(dialog%drea(i),.true.,.false.)
           end if
         end do
         do i=1,dialog%nddou
           if (dialog%ddou(i)%value .ne. copy%ddou(i)%value) then
              call display_dou(dialog%ddou(i),.true.,.false.)
           end if
         end do
         do i=1,dialog%ndedi
           if (dialog%dedi(i)%value .ne. copy%dedi(i)%value) then
              call display_edi(dialog%dedi(i),.true.,.false.)
           end if
         end do
         call pgebuf
        end if
        end
c-----------------------------------------------------
        subroutine move_dialog(dialog)
        implicit none
        include 'dialogstructure'
        type(dlg) dialog
        real dx,dy,x,y
        real deltax,deltay
        integer i
        character ans*1

           call pgsci(0)
           call pgsfs(1)
           call pgrect(dialog%dxmin*70.0,dialog%dxmax*70.0,
     &         dialog%dymin*40.0,dialog%dymax*40.0)
           call pgsci(1)
           call pgcurse(x,y,ans)
           deltax = dialog%dxmin*70.0 + 0.5 - x
           deltay = dialog%dymax*40.0 - 0.5 - y

           dx = dialog%dxmax - dialog%dxmin
           dy = dialog%dymax - dialog%dymin
           dialog%dxmin = x/70.0 - 0.5/70.0
           dialog%dxmax = dialog%dxmin + dx
           dialog%dymax = y/40.0 + 0.5/40.0
           dialog%dymin = dialog%dymax - dy

           do i=1,dialog%ndste
              dialog%dste(i)%x = dialog%dste(i)%x - deltax
              dialog%dste(i)%y = dialog%dste(i)%y - deltay
           end do
           do i=1,dialog%ndvte
              dialog%dvte(i)%x = dialog%dvte(i)%x - deltax
              dialog%dvte(i)%y = dialog%dvte(i)%y - deltay
           end do
           do i=1,dialog%ndrad
              dialog%drad(i)%x = dialog%drad(i)%x - deltax
              dialog%drad(i)%y = dialog%drad(i)%y - deltay
           end do
           do i=1,dialog%ndche
              dialog%dche(i)%x = dialog%dche(i)%x - deltax
              dialog%dche(i)%y = dialog%dche(i)%y - deltay
           end do
           do i=1,dialog%ndint
              dialog%dint(i)%x = dialog%dint(i)%x - deltax
              dialog%dint(i)%y = dialog%dint(i)%y - deltay
           end do
           do i=1,dialog%ndrea
              dialog%drea(i)%x = dialog%drea(i)%x - deltax
              dialog%drea(i)%y = dialog%drea(i)%y - deltay
           end do
           do i=1,dialog%nddou
              dialog%ddou(i)%x = dialog%ddou(i)%x - deltax
              dialog%ddou(i)%y = dialog%ddou(i)%y - deltay
           end do
           do i=1,dialog%ndedi
              dialog%dedi(i)%x = dialog%dedi(i)%x - deltax
              dialog%dedi(i)%y = dialog%dedi(i)%y - deltay
           end do
           do i=1,dialog%ndbut
              dialog%dbut(i)%x = dialog%dbut(i)%x - deltax
              dialog%dbut(i)%y = dialog%dbut(i)%y - deltay
           end do
           do i=1,dialog%ndplo
              dialog%dplo(i)%x1 = dialog%dplo(i)%x1 - deltax
              dialog%dplo(i)%y1 = dialog%dplo(i)%y1 - deltay
              dialog%dplo(i)%x2 = dialog%dplo(i)%x2 - deltax
              dialog%dplo(i)%y2 = dialog%dplo(i)%y2 - deltay
           end do
           do i=1,dialog%ndout
              dialog%dout(i)%x1 = dialog%dout(i)%x1 - deltax
              dialog%dout(i)%y1 = dialog%dout(i)%y1 - deltay
              dialog%dout(i)%x2 = dialog%dout(i)%x2 - deltax
              dialog%dout(i)%y2 = dialog%dout(i)%y2 - deltay
           end do
           return
           end

c-----------------------------------------------------
          subroutine begin_macro(lun,ans)
c-----------------------------------------------------
          integer lun,getlun
          character ans*1
          lun = getlun()

          if (ans.ne.' ') then
          open (unit=lun, file='dialog'//ans//'.macro',form='formatted',
     &        status='unknown')
          end if
          return
          end

c--------------------------------------------------------
          subroutine end_macro(lun)
c--------------------------------------------------------
          integer lun
          close(lun)
          end

c-------------------------------------------------------------
          subroutine execute_macro(lun,ans,ok)
c-------------------------------------------------------------
          integer lun
          character*1 ans
          logical ok

          lun = getlun()
          open (unit=lun,file='dialog'//ans//'.macro',form='formatted',
     &        status='old',err=123)
          ok = .true.
          return
 123      continue
          write(*,*) ' No macro of that name!'
          ok = .false.
          end

c-------------------------------------------------------
          subroutine save_values(dialog,ans)
c-------------------------------------------------------
          include 'dialogstructure'
          type(dlg) dialog
          integer lun, getlun, i
          character*1 ans
          lun = getlun()

          open (unit=lun,file='dialog'//ans//'.values',form='formatted',
     &        status='unknown')
          do i=1,dialog%ndrad
             write(lun,*) dialog%drad(i)%value
          end do
          do i=1,dialog%ndche
             write(lun,*) dialog%dche(i)%value
          end do
          do i=1,dialog%ndint
             write(lun,*) dialog%dint(i)%value
          end do
          do i=1,dialog%ndrea
             write(lun,*) dialog%drea(i)%value
          end do
          do i=1,dialog%nddou
             write(lun,*) dialog%ddou(i)%value
          end do
          do i=1,dialog%ndedi
             write(lun,'(a)') dialog%dedi(i)%value
          end do
          close(lun)
          end

c---------------------------------------------------------
          subroutine load_values(dialog,ans)
c---------------------------------------------------------
          include 'dialogstructure'
          type(dlg) dialog
          character ans*1
          integer lun, getlun, i
          lun = getlun()
c
c     Modified by DRL 93/12/20 so that crash is avoided when no
c     file is found.
c
          open (unit=lun,file='dialog'//ans//'.values',form='formatted',
     &        status='old',err=879)
          do i=1,dialog%ndrad
             read(lun,*) dialog%drad(i)%value
          end do
          do i=1,dialog%ndche
             read(lun,*) dialog%dche(i)%value
          end do
          do i=1,dialog%ndint
             read(lun,*) dialog%dint(i)%value
          end do
          do i=1,dialog%ndrea
             read(lun,*) dialog%drea(i)%value
          end do
          do i=1,dialog%nddou
             read(lun,*) dialog%ddou(i)%value
          end do
          do i=1,dialog%ndedi
             read(lun,'(a)') dialog%dedi(i)%value
          end do
          close(lun)
          return
 879      write(*,*) 'No .values file found!'
          end
c---------------------------------------------------------------
        subroutine next_event(hit,dialog,x,y,ans,widget_type,
     &        array_index,x2,y2,
     &         defining_macro,executing_macro,macrolun)
c---------------------------------------------------------------
        include 'dialogstructure'
        logical hit
        real x,y
        character*1 ans
        integer widget_type,array_index,macrolun
C added missing defs - PAH
        logical executing_macro
        logical defining_macro
        real x2,y2
        type(dlg) dialog

        widget_type = 0
        do while (widget_type .le.2 .and. widget_type.ne.11)
          if (executing_macro) then
             read(macrolun,'(x,a1,x,i3,x,i3)') ans,
     &           widget_type, array_index
             hit = .true.
             if (widget_type.eq.4) then
                read(macrolun,*) dialog%dche(array_index)%value
             else if (widget_type.eq.5) then
                read(macrolun,*) dialog%dint(array_index)%value
             else if (widget_type.eq.6) then
                read(macrolun,*) dialog%drea(array_index)%value
             else if (widget_type.eq.7) then
                read(macrolun,*) dialog%ddou(array_index)%value
             else if (widget_type.eq.8) then
                read(macrolun,'(a)') dialog%dedi(array_index)%value
             else if (widget_type.eq.10) then
                read(macrolun,*) x2,y2
             else if (widget_type.eq.13.and.array_index.eq.0) then
                executing_macro = .false.
                widget_type = 0
                hit = .false.
                close(macrolun)
             end if
          else
              if (.not. hit) call pgcurse(x,y,ans)
              call where_clicked(dialog,x,y,widget_type,
     &            array_index,x2,y2)
              hit = .false.
          end if
        end do
        if (defining_macro) then
             write(macrolun,'(x,a1,x,i3,x,i3)') ans,widget_type,
     &         array_index
             if (widget_type.eq.4) then
                write(macrolun,*) dialog%dche(array_index)%value
             else if (widget_type.eq.10) then
                write(macrolun,*) x2,y2
             end if
        end if
        end















