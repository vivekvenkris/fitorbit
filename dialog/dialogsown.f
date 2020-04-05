c--------------
        subroutine dialog900(
     &  rad1,rad2,rad3,rad4,abort
     & )

c written with the InterfaceBuilder
c       Matthew Bailes NRAL

        logical            rad1,rad2,rad3,rad4,abort
        integer status,widget_type,array_index,i
        logical hit
        real x,y,x2,y2
        character ans
c----------------------
        include 'dialogstructure'
        type(dlg) dialog
        call pgask(.false.)
        hit=.false.
        abort = .false.
        call read_dialog('psrdiadir','dialog900',dialog,status)

        dialog%drad(1)%value=rad1
        dialog%drad(2)%value=rad2
        dialog%drad(3)%value=rad3
        dialog%drad(4)%value=rad4
        call display_dialog(dialog,.false.)
        do while (.true.)
        if (.not. hit) call pgcurse(x,y,ans)
        call where_clicked(dialog,x,y,widget_type,array_index,x2,y2)
        if (widget_type .le. 2 .or. widget_type .eq.11) then
           hit = .false.
           goto 999
        end if
 10     continue
        hit = .false.
        if (widget_type.eq.3) then
         if (.not. dialog%drad(array_index)%value) then
           do i=1,dialog%ndrad
            if (dialog%drad(array_index)%group
     &         .eq.dialog%drad(i)%group) then
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
         call enter_int(dialog%dint(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.6) then
         call enter_rea(dialog%drea(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.7) then
         call enter_dou(dialog%ddou(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.8) then
         call enter_edi(dialog%dedi(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.9) then
        if (array_index .eq. 1) then
          call dialog901
          call display_dialog(dialog,.false.)
        else if(array_index .eq. 2)then
          goto 888
        else if(array_index .eq. 3)then
          abort = .true.
          goto 888
        end if
        else if (widget_type.eq.10) then
        continue
        end if

 999      continue
        end do
 888      continue
        rad1=dialog%drad(1)%value
        rad2=dialog%drad(2)%value
        rad3=dialog%drad(3)%value
        rad4=dialog%drad(4)%value
        end
c--------------
        subroutine dialog901

c written with the InterfaceBuilder
c       Matthew Bailes NRAL

        integer status,widget_type,array_index,i
        logical hit
        real x,y,x2,y2
        character ans
c----------------------
        include 'dialogstructure'
        type(dlg) dialog
        call pgask(.false.)
        hit=.false.
        call read_dialog('psrdiadir','dialog901',dialog,status)

        call display_dialog(dialog,.false.)
        do while (.true.)
        if (.not. hit) call pgcurse(x,y,ans)
        call where_clicked(dialog,x,y,widget_type,array_index,x2,y2)
        if (widget_type .le. 2 .or. widget_type .eq.11) then
           hit = .false.
           goto 999
        end if
 10     continue
        hit = .false.
        if (widget_type.eq.3) then
         if (.not. dialog%drad(array_index)%value) then
           do i=1,dialog%ndrad
            if (dialog%drad(array_index)%group
     &         .eq.dialog%drad(i)%group) then
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
         call enter_int(dialog%dint(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.6) then
         call enter_rea(dialog%drea(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.7) then
         call enter_dou(dialog%ddou(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.8) then
         call enter_edi(dialog%dedi(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.9) then
        if (array_index .eq. 1) then
          goto 888
        end if
        else if (widget_type.eq.10) then
        continue
        end if

 999      continue
        end do
 888      continue
        end
c--------------
        subroutine dialog902(
     &  rad1,rad2,rad3,rad4,rad5,rad6,rad7,rad8,rad9
     & ,rad10,rad11
     & ,che1,help,view,add,finished,abort,move,align,delete
     & )

c written with the InterfaceBuilder
c       Matthew Bailes NRAL

        logical            rad1,rad2,rad3,rad4,rad5,rad6,rad7,rad8,rad9
     & ,rad10,rad11
        logical            che1
        integer status,widget_type,array_index,i
        logical hit
        logical help,view,add,finished,abort,move,align,delete
        real x,y,x2,y2
        character ans
c----------------------
        include 'dialogstructure'
        type(dlg) dialog
        call pgask(.false.)
        hit=.false.
        call read_dialog('psrdiadir','dialog902',dialog,status)

        dialog%drad(1)%value=rad1
        dialog%drad(2)%value=rad2
        dialog%drad(3)%value=rad3
        dialog%drad(4)%value=rad4
        dialog%drad(5)%value=rad5
        dialog%drad(6)%value=rad6
        dialog%drad(7)%value=rad7
        dialog%drad(8)%value=rad8
        dialog%drad(9)%value=rad9
        dialog%drad(10)%value=rad10
        dialog%drad(11)%value=rad11
        dialog%dche(1)%value=che1
        call display_dialog(dialog,.false.)
        do while (.true.)
        if (.not. hit) call pgcurse(x,y,ans)
        call where_clicked(dialog,x,y,widget_type,array_index,x2,y2)
        if (widget_type .le. 2 .or. widget_type .eq.11) then
           hit = .false.
           goto 999
        end if
 10     continue
        hit = .false.
        if (widget_type.eq.3) then
         if (.not. dialog%drad(array_index)%value) then
           do i=1,dialog%ndrad
            if (dialog%drad(array_index)%group
     &         .eq.dialog%drad(i)%group) then
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
         call enter_int(dialog%dint(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.6) then
         call enter_rea(dialog%drea(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.7) then
         call enter_dou(dialog%ddou(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.8) then
         call enter_edi(dialog%dedi(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.9) then
        help = .false.
        view=.false.
        add = .false.
        finished = .false.
        abort = .false.
        delete = .false.
        move = .false.
        align = .false.

        if (array_index .eq. 1) then
          help = .true.
          goto 888
        else if(array_index .eq. 2)then
          view = .true.
          goto 888
        else if(array_index .eq. 3)then
          add = .true.
          goto 888
        else if(array_index .eq. 4)then
          finished = .true.
          goto 888
        else if(array_index .eq. 5)then
          abort = .true.
          goto 888
        else if(array_index .eq. 6)then
          delete = .true.
          goto 888
        else if(array_index .eq. 7)then
          move = .true.
          goto 888
        else if(array_index .eq. 8)then
          align = .true.
          goto 888
        end if
        else if (widget_type.eq.10) then
        continue
        end if

 999      continue
        end do
 888      continue
        rad1=dialog%drad(1)%value
        rad2=dialog%drad(2)%value
        rad3=dialog%drad(3)%value
        rad4=dialog%drad(4)%value
        rad5=dialog%drad(5)%value
        rad6=dialog%drad(6)%value
        rad7=dialog%drad(7)%value
        rad8=dialog%drad(8)%value
        rad9=dialog%drad(9)%value
        rad10=dialog%drad(10)%value
        rad11=dialog%drad(11)%value
        che1=dialog%dche(1)%value
        end
c--------------
        subroutine dialog931(
     &  vte1
     & ,edi1,place,abort,view
     & )

c written with the InterfaceBuilder
c       Matthew Bailes NRAL

        character*(*)       vte1
        character*(*)       edi1
        integer status,widget_type,array_index,i
        logical hit,place,abort,view
        real x,y,x2,y2
        character ans
c----------------------
        include 'dialogstructure'
        type(dlg) dialog
        call pgask(.false.)
        hit=.false.
        call read_dialog('psrdiadir','dialog931',dialog,status)

        dialog%dvte(1)%label=vte1
        dialog%dedi(1)%value=edi1
        call display_dialog(dialog,.true.)
        do while (.true.)
        if (.not. hit) call pgcurse(x,y,ans)
        call where_clicked(dialog,x,y,widget_type,array_index,x2,y2)
        if (widget_type .le. 2 .or. widget_type .eq.11) then
           hit = .false.
           goto 999
        end if
 10     continue
        hit = .false.
        if (widget_type.eq.3) then
         if (.not. dialog%drad(array_index)%value) then
           do i=1,dialog%ndrad
            if (dialog%drad(array_index)%group
     &         .eq.dialog%drad(i)%group) then
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
         call enter_int(dialog%dint(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.6) then
         call enter_rea(dialog%drea(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.7) then
         call enter_dou(dialog%ddou(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.8) then
         call enter_edi(dialog%dedi(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.9) then
        place=.false.
        abort=.false.
        view=.false.
        if (array_index .eq. 1) then
          place=.true.
          goto 888
        else if(array_index .eq. 2)then
          abort=.true.
          goto 888
        else if(array_index .eq. 3)then
          view=.true.
          goto 888
        end if
        else if (widget_type.eq.10) then
        continue
        end if

 999      continue
        end do
 888      continue
        edi1=dialog%dedi(1)%value
        end
c--------------
        subroutine dialog932(
     &  int1,abort
     & )

c written with the InterfaceBuilder
c       Matthew Bailes NRAL

        integer            int1
        integer status,widget_type,array_index,i
        logical hit,abort
        real x,y,x2,y2
        character ans
c----------------------
        include 'dialogstructure'
        type(dlg) dialog
        call pgask(.false.)
        hit=.false.
        call read_dialog('psrdiadir','dialog932',dialog,status)

        dialog%dint(1)%value=int1
        call display_dialog(dialog,.false.)
        do while (.true.)
        if (.not. hit) call pgcurse(x,y,ans)
        call where_clicked(dialog,x,y,widget_type,array_index,x2,y2)
        if (widget_type .le. 2 .or. widget_type .eq.11) then
           hit = .false.
           goto 999
        end if
 10     continue
        hit = .false.
        if (widget_type.eq.3) then
         if (.not. dialog%drad(array_index)%value) then
           do i=1,dialog%ndrad
            if (dialog%drad(array_index)%group
     &         .eq.dialog%drad(i)%group) then
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
         call enter_int(dialog%dint(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.6) then
         call enter_rea(dialog%drea(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.7) then
         call enter_dou(dialog%ddou(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.8) then
         call enter_edi(dialog%dedi(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.9) then
        abort=.false.
        if (array_index .eq. 1) then
          goto 888
        else if(array_index .eq. 2)then
          abort = .true.
          goto 888
        end if
        else if (widget_type.eq.10) then
        continue
        end if

 999      continue
        end do
 888      continue
        int1=dialog%dint(1)%value
        end
c--------------
        subroutine dialog933(
     &  vte1
     & ,edi1,place,abort,view
     & )

c written with the InterfaceBuilder
c       Matthew Bailes NRAL

        character*(*)       vte1
        character*(*)       edi1
        integer status,widget_type,array_index,i
        logical hit,place,abort,view
        real x,y,x2,y2
        character ans
c----------------------
        include 'dialogstructure'
        type(dlg) dialog
        call pgask(.false.)
        hit=.false.
        call read_dialog('psrdiadir','dialog933',dialog,status)

        dialog%dvte(1)%label=vte1
        dialog%dedi(1)%value=edi1
        call display_dialog(dialog,.true.)
        do while (.true.)
        if (.not. hit) call pgcurse(x,y,ans)
        call where_clicked(dialog,x,y,widget_type,array_index,x2,y2)
        if (widget_type .le. 2 .or. widget_type .eq.11) then
           hit = .false.
           goto 999
        end if
 10     continue
        hit = .false.
        if (widget_type.eq.3) then
         if (.not. dialog%drad(array_index)%value) then
           do i=1,dialog%ndrad
            if (dialog%drad(array_index)%group
     &         .eq.dialog%drad(i)%group) then
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
         call enter_int(dialog%dint(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.6) then
         call enter_rea(dialog%drea(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.7) then
         call enter_dou(dialog%ddou(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.8) then
         call enter_edi(dialog%dedi(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.9) then
        place=.false.
        abort=.false.
        view=.false.
        if (array_index .eq. 1) then
          place=.true.
          goto 888
        else if(array_index .eq. 2)then
          abort=.true.
          goto 888
        else if(array_index .eq. 3)then
          view=.true.
          goto 888
        end if
        else if (widget_type.eq.10) then
        continue
        end if

 999      continue
        end do
 888      continue
        edi1=dialog%dedi(1)%value
        end
c--------------
        subroutine dialog934(
     &  vte1
     & ,edi1,abort
     & )

c written with the InterfaceBuilder
c       Matthew Bailes NRAL

        character*(*)       vte1
        character*(*)       edi1
        integer status,widget_type,array_index,i
        logical hit,abort
        real x,y,x2,y2
        character ans
c----------------------
        include 'dialogstructure'
        type(dlg) dialog
        call pgask(.false.)
        hit=.false.
        call read_dialog('psrdiadir','dialog934',dialog,status)

        dialog%dvte(1)%label=vte1
        dialog%dedi(1)%value=edi1
        call display_dialog(dialog,.false.)
        do while (.true.)
        if (.not. hit) call pgcurse(x,y,ans)
        call where_clicked(dialog,x,y,widget_type,array_index,x2,y2)
        if (widget_type .le. 2 .or. widget_type .eq.11) then
           hit = .false.
           goto 999
        end if
 10     continue
        hit = .false.
        if (widget_type.eq.3) then
         if (.not. dialog%drad(array_index)%value) then
           do i=1,dialog%ndrad
            if (dialog%drad(array_index)%group
     &         .eq.dialog%drad(i)%group) then
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
         call enter_int(dialog%dint(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.6) then
         call enter_rea(dialog%drea(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.7) then
         call enter_dou(dialog%ddou(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.8) then
         call enter_edi(dialog%dedi(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.9) then
         abort=.false.
        if (array_index .eq. 1) then
          abort=.true.
          goto 888
        else if(array_index .eq. 2)then
          goto 888
        end if
        else if (widget_type.eq.10) then
        continue
        end if

 999      continue
        end do
 888      continue
        edi1=dialog%dedi(1)%value
        end
c--------------
        subroutine dialog935(
     &  vte1
     & ,int1
     & ,edi1,abort
     & )

c written with the InterfaceBuilder
c       Matthew Bailes NRAL

        character*(*)       vte1
        integer             int1
        character*(*)       edi1
        integer status,widget_type,array_index,i
        logical hit,abort
        real x,y,x2,y2
        character ans
c----------------------
        include 'dialogstructure'
        type(dlg) dialog
        call pgask(.false.)
        hit=.false.
        call read_dialog('psrdiadir','dialog935',dialog,status)

        dialog%dvte(1)%label=vte1
        dialog%dint(1)%value=int1
        dialog%dedi(1)%value=edi1
        call display_dialog(dialog,.false.)
        do while (.true.)
        if (.not. hit) call pgcurse(x,y,ans)
        call where_clicked(dialog,x,y,widget_type,array_index,x2,y2)
        if (widget_type .le. 2 .or. widget_type .eq.11) then
           hit = .false.
           goto 999
        end if
 10     continue
        hit = .false.
        if (widget_type.eq.3) then
         if (.not. dialog%drad(array_index)%value) then
           do i=1,dialog%ndrad
            if (dialog%drad(array_index)%group
     &         .eq.dialog%drad(i)%group) then
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
         call enter_int(dialog%dint(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.6) then
         call enter_rea(dialog%drea(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.7) then
         call enter_dou(dialog%ddou(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.8) then
         call enter_edi(dialog%dedi(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.9) then
        abort=.false.
        if (array_index .eq. 1) then
          abort=.true.
          goto 888
        else if(array_index .eq. 2)then
          goto 888
        end if
        else if (widget_type.eq.10) then
        continue
        end if

 999      continue
        end do
 888      continue
        int1=dialog%dint(1)%value
        edi1=dialog%dedi(1)%value
        end
c--------------
        subroutine dialog936(abort)

c written with the InterfaceBuilder
c       Matthew Bailes NRAL

        integer status,widget_type,array_index,i
        logical hit,abort
        real x,y,x2,y2
        character ans
c----------------------
        include 'dialogstructure'
        type(dlg) dialog
        call pgask(.false.)
        hit=.false.
        call read_dialog('psrdiadir','dialog936',dialog,status)

        call display_dialog(dialog,.false.)
        do while (.true.)
        if (.not. hit) call pgcurse(x,y,ans)
        call where_clicked(dialog,x,y,widget_type,array_index,x2,y2)
        if (widget_type .le. 2 .or. widget_type .eq.11) then
           hit = .false.
           goto 999
        end if
 10     continue
        hit = .false.
        if (widget_type.eq.3) then
         if (.not. dialog%drad(array_index)%value) then
           do i=1,dialog%ndrad
            if (dialog%drad(array_index)%group
     &         .eq.dialog%drad(i)%group) then
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
         call enter_int(dialog%dint(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.6) then
         call enter_rea(dialog%drea(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.7) then
         call enter_dou(dialog%ddou(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.8) then
         call enter_edi(dialog%dedi(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.9) then
        abort=.false.
        if (array_index .eq. 1) then
          goto 888
        else if(array_index .eq. 2)then
          abort=.true.
          goto 888
        end if
        else if (widget_type.eq.10) then
        continue
        end if

 999      continue
        end do
 888      continue
        end
c--------------
        subroutine dialog937(
     &  int1,int2,abort
     & )

c written with the InterfaceBuilder
c       Matthew Bailes NRAL

        integer            int1,int2
        integer status,widget_type,array_index,i
        logical hit,abort
        real x,y,x2,y2
        character ans
c----------------------
        include 'dialogstructure'
        type(dlg) dialog
        call pgask(.false.)
        hit=.false.
        call read_dialog('psrdiadir','dialog937',dialog,status)

        dialog%dint(1)%value=int1
        dialog%dint(2)%value=int2
        call display_dialog(dialog,.false.)
        do while (.true.)
        if (.not. hit) call pgcurse(x,y,ans)
        call where_clicked(dialog,x,y,widget_type,array_index,x2,y2)
        if (widget_type .le. 2 .or. widget_type .eq.11) then
           hit = .false.
           goto 999
        end if
 10     continue
        hit = .false.
        if (widget_type.eq.3) then
         if (.not. dialog%drad(array_index)%value) then
           do i=1,dialog%ndrad
            if (dialog%drad(array_index)%group
     &         .eq.dialog%drad(i)%group) then
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
         call enter_int(dialog%dint(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.6) then
         call enter_rea(dialog%drea(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.7) then
         call enter_dou(dialog%ddou(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.8) then
         call enter_edi(dialog%dedi(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.9) then
         abort=.false.
        if (array_index .eq. 1) then
          abort=.true.
          goto 888
        else if(array_index .eq. 2)then
          goto 888
        end if
        else if (widget_type.eq.10) then
        continue
        end if

 999      continue
        end do
 888      continue
        int1=dialog%dint(1)%value
        int2=dialog%dint(2)%value
        end
c--------------
c--------------
        subroutine dialog961(
     & replot,x,y,ans,plotno,button,
     &  vte1        
     & )
 
c written with the dialog program
c       Matthew Bailes NRAL
 
c there are  1 buttons.
c the label for each is listed below
c number  1 ok                              
c there are  0 radio controls.
c the label for each is listed below
c there are  0 check boxes.
c the label for each is listed below
c there are  0 editable integers.
c the label for each is listed below
c there are  0 editable reals.
c the label for each is listed below
c there are  0 editable doubles.
c the label for each is listed below
c there are  0 editable strings.
c the label for each is listed below
 
        character*(*)      vte1                                                 
        integer vars,status,nnn,widget_type,array_index
        logical hit,first
        real x,y,x2,y2,xmin,xmax,ymin,ymax
        character ans
        integer replot,plotno,button,npl
        data first /.true./
        include 'dialogstructure'
        type(dlg) dialog
c-------------------------------------------
        if (first) then
        call read_dialog('psrdiadir',
     &'dialog961',dialog,status)
        first=.false. 
        end if 
 
        call dialog_reset
        dialog%dvte(1)%label=vte1
        call dialogstd(plotno,x,y,ans,button,replot,dialog)
        return
        Entry qpldialog961(npl,xmin,xmax,ymin,ymax)
        xmin=dialog%dplo(npl)%vx1
        xmax=dialog%dplo(npl)%vx2
        ymin=dialog%dplo(npl)%vy1
        ymax=dialog%dplo(npl)%vy2
        return
        Entry selpldialog961(npl)
        call dialog_select_plot(dialog,npl)
        return
        Entry setpldialog961(npl,xmin,xmax,ymin,ymax)
        call dialog_window_set(dialog,npl,xmin,xmax,ymin,ymax)
        return
        Entry xpldialog961(npl,xmin,xmax,ymin,ymax)
        call new_erase_plot(dialog,npl,xmin,xmax,ymin,ymax) 
        return 
        end
        
        Subroutine pldialog961(func,wn,xmin,xmax,ymin,ymax)
        character*1 func
        integer wn
        real xmin,xmax,ymin,ymax
        if ((func.eq.'q').or.(func.eq.'Q')) then
          call qpldialog961(wn,xmin,xmax,ymin,ymax)
        else if ((func.eq.'x').or.(func.eq.'X')) then
          call xpldialog961(wn,xmin,xmax,ymin,ymax)
        else if ((func.eq.'s').or.(func.eq.'S')) then
          call setpldialog961(wn,xmin,xmax,ymin,ymax)
        else if ((func.eq.'r').or.(func.eq.'R')) then
          call selpldialog961(wn)
        end if
        end
c--------------
        subroutine dialog962(
     &  rad1,rad2
     & ,rea1,rea2,but1,but2
     & )

c written with the InterfaceBuilder
c       Matthew Bailes NRAL

        logical            rad1,rad2,but1,but2
        real               rea1,rea2
        integer status,widget_type,array_index,i
        logical hit
        real x,y
        character ans
c----------------------
        include 'dialogstructure'
        type(dlg) dialog
        call pgask(.false.)
        hit=.false.
        call read_dialog('psrdiadir','dialog962',dialog,status)

        dialog%drad(1)%value=rad1
        dialog%drad(2)%value=rad2
        dialog%drea(1)%value=rea1
        dialog%drea(2)%value=rea2
        call display_dialog(dialog,.false.)
        do while (.true.)
        if (.not. hit) call pgcurse(x,y,ans)
        call where_clicked(dialog,x,y,widget_type,array_index)
        if (widget_type .le. 2 .or. widget_type .eq.11) then
           hit = .false.
           goto 999
        end if
 10     continue
        hit = .false.
        if (widget_type.eq.3) then
         if (.not. dialog%drad(array_index)%value) then
           do i=1,dialog%ndrad
            if (dialog%drad(array_index)%group
     &         .eq.dialog%drad(i)%group) then
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
         call enter_int(dialog%dint(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.6) then
         call enter_rea(dialog%drea(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.7) then
         call enter_dou(dialog%ddou(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.8) then
         call enter_edi(dialog%dedi(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.9) then
        but1=.false.
        but2=.false.
        if (array_index .eq. 1) then
          but1=.true.
          goto 888
        else if(array_index .eq. 2)then
          but2=.true.
          goto 888
        end if
        else if (widget_type.eq.10) then
        continue
        end if

 999      continue
        end do
 888      continue
        rad1=dialog%drad(1)%value
        rad2=dialog%drad(2)%value
        rea1=dialog%drea(1)%value
        rea2=dialog%drea(2)%value
        end
c--------------
        subroutine dialog960(
     &  edi1,lnew,lexist
     & )

c written with the dialog program
c       Matthew Bailes NRAL

        character*(*)      edi1
        integer status,widget_type,array_index,i
        logical hit,lnew,lexist
        real x,y,x2,y2
        character ans
c----------------------
        include 'dialogstructure'
        type(dlg) dialog
        call pgask(.false.)
        hit=.false.
        call read_dialog('psrdiadir',
     &   'dialog960',dialog,status)

        dialog%dedi(1)%value=edi1
        call display_dialog(dialog,.false.)
        do while (.true.)
        if (.not. hit) call pgcurse(x,y,ans)
        call where_clicked(dialog,x,y,widget_type,array_index,x2,y2)
        if (widget_type .le. 2 .or. widget_type .eq.11) then
           hit = .false.
           goto 999
        end if
 10     continue
        hit = .false.
        if (widget_type.eq.3) then
         if (.not. dialog%drad(array_index)%value) then
           do i=1,dialog%ndrad
            if (dialog%drad(array_index)%group
     &         .eq.dialog%drad(i)%group) then
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
         call enter_int(dialog%dint(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.6) then
         call enter_rea(dialog%drea(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.7) then
         call enter_dou(dialog%ddou(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.8) then
         call enter_edi(dialog%dedi(array_index),x,y)
         hit = .true.
        else if (widget_type.eq.9) then
        lnew=.false.
        lexist=.false.
        if (array_index .eq. 1) then
          lnew=.true.
          goto 888
        else if(array_index .eq. 2)then
          lexist=.true.
          goto 888
        end if
        else if (widget_type.eq.10) then
        continue
        end if

 999      continue
        end do
 888      continue
        edi1=dialog%dedi(1)%value
        end
c--------------
        subroutine dialogdou(
     & replot,x,y,ans,plotno,button,
     &  rad1,rad2,rad3                                                          
     & ,int1                                                                    
     & ,edi1                                                                    
     & )
 
c written with the dialog program
c       Matthew Bailes NRAL
 
c there are  2 buttons.
c the label for each is listed below
c number  1 Abort                           
c number  2 Place                           
c there are  3 radio controls.
c the label for each is listed below
c radio control number  1 Normal                          
c radio control number  2 HH:MM:SS.SSSS                   
c radio control number  3 YY/MM/DD HH:MM:SS.SSS           
c there are  0 check boxes.
c the label for each is listed below
c there are  1 editable integers.
c the label for each is listed below
c editable integer number  1 Field Length                    
c there are  0 editable reals.
c the label for each is listed below
c there are  0 editable doubles.
c the label for each is listed below
c there are  1 editable strings.
c the label for each is listed below
c editable text number  1 Labelble Text                   
 
        logical            rad1,rad2,rad3                                       
        integer            int1                                                 
        character*(*)      edi1                                                 
        integer vars,status,nnn,widget_type,array_index
        logical hit,first
        real x,y,x2,y2,xmin,xmax,ymin,ymax
        character ans
        integer replot,plotno,button,npl
        data first /.true./
        include 'dialogstructure'
        type(dlg) dialog
c-------------------------------------------
        if (first) then
        call read_dialog('psrdiadir',
     &'dialogdou',dialog,status)
        first=.false. 
        end if 
 
        dialog%drad(1)%value=rad1
        dialog%drad(2)%value=rad2
        dialog%drad(3)%value=rad3
        dialog%dint(1)%value=int1
        dialog%dedi(1)%value=edi1
        call dialogstd(plotno,x,y,ans,button,replot,dialog)
        rad1=dialog%drad(1)%value
        rad2=dialog%drad(2)%value
        rad3=dialog%drad(3)%value
        int1=dialog%dint(1)%value
        edi1=dialog%dedi(1)%value
        return
        Entry qpldialogdou(npl,xmin,xmax,ymin,ymax)
        xmin=dialog%dplo(npl)%vx1
        xmax=dialog%dplo(npl)%vx2
        ymin=dialog%dplo(npl)%vy1
        ymax=dialog%dplo(npl)%vy2
        return
        Entry selpldialogdou(npl)
        call dialog_select_plot(dialog,npl)
        return
        Entry setpldialogdou(npl,xmin,xmax,ymin,ymax)
        call dialog_window_set(dialog,npl,xmin,xmax,ymin,ymax)
        return
        Entry xpldialogdou(npl,xmin,xmax,ymin,ymax)
        call new_erase_plot(dialog,npl,xmin,xmax,ymin,ymax) 
        return 
        end
