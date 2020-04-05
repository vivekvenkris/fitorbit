c-------------------------------------------------------------------------
        subroutine where_clicked(dialog,x,y,
     &   widget_type,array_index,x2,y2)
c-------------------------------------------------------------------------
        real x,y,x2,y2
        integer widget_type,array_index,i
        logical in_rad,in_che,in_int,in_edi,in_dou,in_rea,in_but
        logical in_plo,in_out
        include 'dialogstructure'
        type(dlg) dialog
        integer norder(dial_max)
        common /order/norder

        widget_type = 0

c first of all check whether clicked on move icon.

        if (abs(max(dialog%dxmin*70.0+0.5,0.5)-x).lt.0.5) then
           if (abs(min(dialog%dymax*40.0-0.5,39.5)-y).lt.0.5)then
              widget_type=12
              array_index = 0
              goto 10
           end if
        end if

c first of all check whether clicked on move icon.

        if (abs(min(dialog%dxmax*70.0+0.5,69.5)-x).lt.0.5) then
           if (abs(min(dialog%dymax*40.0-0.5,39.5)-y).lt.0.5)then
              widget_type=13
              array_index = 0
              goto 10
           end if
        end if

        do i=1,dialog%ndrad
          if (in_rad(dialog%drad(i),x,y))then
                widget_type = 3
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndche
          if (in_che(dialog%dche(i),x,y))then
                widget_type = 4
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndint
          if (in_int(dialog%dint(i),x,y))then
                widget_type = 5
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndrea
          if (in_rea(dialog%drea(i),x,y))then
                widget_type = 6
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%nddou
          if (in_dou(dialog%ddou(i),x,y))then
                widget_type = 7
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndedi
          if (in_edi(dialog%dedi(i),x,y))then
                widget_type = 8
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndbut
          if (in_but(dialog%dbut(i),x,y))then
                widget_type = 9
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndplo
          norder(i)=max(1,norder(i))
          if (in_plo(dialog%dplo(norder(i)),x,y))then
                widget_type = 10
                array_index = norder(i)
                x2=(x-dialog%dplo(norder(i))%x1)/
     & abs(dialog%dplo(norder(i))%x2-dialog%dplo(norder(i))%x1)*
     & (dialog%dplo(norder(i))%vx2-dialog%dplo(norder(i))%vx1)
     & +dialog%dplo(norder(i))%vx1
                y2=(y-dialog%dplo(norder(i))%y1)/
     & abs(dialog%dplo(norder(i))%y2-dialog%dplo(norder(i))%y1)*
     & (dialog%dplo(norder(i))%vy2-dialog%dplo(norder(i))%vy1)
     & +dialog%dplo(norder(i))%vy1
                goto 10
          end if
        end do
        do i=1,dialog%ndout
          if (in_out(dialog%dout(i),x,y))then
                widget_type = 11
                array_index = i
                goto 10
          end if
        end do

 10     continue
        return
        end
c-------------------------------------------------------------------------
        subroutine nearest_widget(dialog,x,y,widget_type,array_index)
c-------------------------------------------------------------------------
        real x,y
        integer widget_type,array_index,i
        logical in_ste,in_vte,in_rea,in_int,in_dou,in_edi,in_plo
        logical in_out,in_but,in_rad,in_che
        include 'dialogstructure'
        type(dlg) dialog

        widget_type = 0

        do i=1,dialog%ndste
          if (in_ste(dialog%dste(i),x,y))then
                widget_type = 1
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndvte
          if (in_vte(dialog%dvte(i),x,y))then
                widget_type = 2
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndrad
          if (in_rad(dialog%drad(i),x,y))then
                widget_type = 3
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndche
          if (in_che(dialog%dche(i),x,y))then
                widget_type = 4
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndint
          if (in_int(dialog%dint(i),x,y))then
                widget_type = 5
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndrea
          if (in_rea(dialog%drea(i),x,y))then
                widget_type = 6
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%nddou
          if (in_dou(dialog%ddou(i),x,y))then
                widget_type = 7
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndedi
          if (in_edi(dialog%dedi(i),x,y))then
                widget_type = 8
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndbut
          if (in_but(dialog%dbut(i),x,y))then
                widget_type = 9
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndplo
          if (in_plo(dialog%dplo(i),x,y))then
                widget_type = 10
                array_index = i
                goto 10
          end if
        end do
        do i=1,dialog%ndout
          if (in_out(dialog%dout(i),x,y))then
                widget_type = 11
                array_index = i
                goto 10
          end if
        end do

 10     continue
        return
        end
c----------------------------------------------------------------
        logical function in_ste(aaa,x,y)
        real x,y
        include 'dialogstructure'
        logical inside_rect
        type(dstatictex) aaa

        in_ste = inside_rect(x,y,aaa%x,
     & aaa%x+aaa%label_len,aaa%y-0.2,aaa%y+1.2)
        end
c----------------------------------------------------------------
        logical function in_vte(aaa,x,y)
        real x,y
        logical inside_rect
        include 'dialogstructure'
        type(dvartex) aaa

        in_vte = inside_rect(x,y,aaa%x,
     & aaa%x+20.0,aaa%y-0.2,aaa%y+1.2)
        end
c----------------------------------------------------------------
        logical function in_var(aaa,x,y)
        real x,y
        logical inside_rect
        include 'dialogstructure'
        type(dvartex) aaa

        in_var = inside_rect(x,y,aaa%x+aaa%label_len+1.0,
     & aaa%x+aaa%label_len+aaa%maxlen+1.0,aaa%y-0.2,aaa%y+1.2)
        end
c----------------------------------------------------------------
        logical function in_rea(aaa,x,y)
        real x,y
        include 'dialogstructure'
        logical inside_rect
        type(dreal) aaa

        in_rea = inside_rect(x,y,aaa%x+aaa%label_len+1.0,
     & aaa%x+aaa%label_len+aaa%maxlen+1.0,aaa%y-0.2,aaa%y+1.2)
        end
c----------------------------------------------------------------
        logical function in_int(aaa,x,y)
        real x,y
        logical inside_rect
        include 'dialogstructure'
        type(dinteger) aaa

        in_int = inside_rect(x,y,aaa%x+aaa%label_len+1.0,
     & aaa%x+aaa%label_len+aaa%maxlen+1.0,aaa%y-0.2,aaa%y+1.2)
        end
c----------------------------------------------------------------
        logical function in_dou(aaa,x,y)
        real x,y
        logical inside_rect
        include 'dialogstructure'
        type(ddouble) aaa

        in_dou = inside_rect(x,y,aaa%x+aaa%label_len+1.0,
     & aaa%x+aaa%label_len+aaa%maxlen+1.0,aaa%y-0.2,aaa%y+1.2)
        end
c----------------------------------------------------------------
        logical function in_edi(aaa,x,y)
        real x,y
        logical inside_rect
        include 'dialogstructure'
        type(dedit) aaa

        in_edi = inside_rect(x,y,aaa%x+aaa%label_len+1.0,
     & aaa%x+aaa%label_len+aaa%maxlen+1.0,aaa%y-0.2,aaa%y+1.2)
        end
c----------------------------------------------------------------
        logical function in_but(aaa,x,y)
        real x,y
        logical inside_rect
        real xl,yl
        include 'dialogstructure'
        type(dbutton) aaa

        call pglen(4,aaa%label(1:aaa%label_len),xl,yl)
        in_but = inside_rect(x,y,aaa%x-0.4,aaa%x+xl+0.4,
     & aaa%y-0.5,aaa%y+1.2)
        end
c----------------------------------------------------------------
        logical function in_che(aaa,x,y)
        real x,y
        include 'dialogstructure'
        type(dcheck) aaa
        logical inside_rect

        in_che = inside_rect(x,y,aaa%x-2.0,aaa%x-1.0,aaa%y,aaa%y+1.0)
        end
c----------------------------------------------------------------
        logical function in_rad(aaa,x,y)
        real x,y,dist
        include 'dialogstructure'
        type(dradio) aaa

        dist=((x-aaa%x+1.3)**2+(y-aaa%y-0.4)**2)**0.5
        if (dist.lt.0.7) then
          in_rad = .true.
        else
          in_rad = .false.
        end if
        end
c----------------------------------------------------------------
        logical function in_plo(aaa,x,y)
        real x,y
        include 'dialogstructure'
        type(dplot) aaa
        logical inside_rect

        in_plo = inside_rect(x,y,aaa%x1,aaa%x2,aaa%y1,aaa%y2)
        end
c----------------------------------------------------------------
        logical function in_out(aaa,x,y)
        real x,y
        include 'dialogstructure'
        type(doutline) aaa
        logical inside_rect

        in_out = inside_rect(x,y,aaa%x1,aaa%x2,aaa%y1,aaa%y2)
        end
c-------------------------------------------------------------------
        logical function inside_rect(x,y,x1,x2,y1,y2)
        real x,y,x1,x2,y1,y2
        real minx,miny,maxx,maxy

        if (x1.gt.x2) then
           minx=x2
           maxx=x1
        else
           minx=x1
           maxx=x2
        end if
        if (y1.gt.y2) then
           miny=y2
           maxy=y1
        else
           miny=y1
           maxy=y2
        end if

        if (x.ge.minx.and.x.le.maxx.and.y.ge.miny.and.y.le.maxy)then
          inside_rect = .true.
        else
          inside_rect = .false.
        end if
        end
c--------------------------------------------------------------------------
        subroutine enter_edi(aaa,x,y)
        integer pos
        logical in_edi
        character ans*1
        real x,y
        include 'dialogstructure'
        type(dedit) aaa

c highlight box
        call highlight_edi(aaa)
c get another key
        call pgcurse(x,y,ans)
        if (in_edi(aaa,x,y)) then
c if next key is delete, wipe the whole thing and set position to 1
           if (ichar(ans).eq.127) then
              aaa%value=' '
              pos = 1
              call highlight_edi(aaa)
              call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,1,1,0)
           else
c determine position
         call loc_pos_in_edit(x,aaa%x,aaa%label_len,pos)
          if (pos+1.le.aaa%maxlen) then
       call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,pos,pos,0)
       call pgsci(1)
       call write_letter_in_box(aaa%x,aaa%y,
     &  aaa%label_len,aaa%value,pos)
          end if
          end if
         call pgcurse(x,y,ans)
         do while (in_edi(aaa,x,y))
          if (ichar(ans).eq.127) then
c delete previous character
            if (pos.gt.1) then
              call clear_text_in_box(aaa%x,aaa%y,
     &               aaa%label_len,pos,pos,1)
              call pgsci(0)
              call write_letter_in_box(aaa%x,aaa%y,aaa%label_len,
     &              aaa%value,pos)
              pos = pos-1        
              aaa%value(pos:pos)=' '
              call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,
     &              pos,pos,0)
              call pgsci(1)
              call write_letter_in_box(aaa%x,aaa%y,
     &             aaa%label_len,aaa%value,pos)
            end if           
          else if (pos.le.aaa%maxlen)then
           aaa%value(pos:pos)=ans
       call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,pos,pos,1)
       call pgsci(0)
       call write_letter_in_box(aaa%x,aaa%y,
     &  aaa%label_len,aaa%value,pos)
          if (pos+1.le.aaa%maxlen) then
       call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,pos+1,pos+1,0)
       call pgsci(1)
       call write_letter_in_box(aaa%x,aaa%y,
     &  aaa%label_len,aaa%value,pos+1)
          end if
           pos = pos + 1
          end if
           call pgcurse(x,y,ans)
         end do
        end if

c return box to normal
        call display_edi(aaa,.true.,.true.)
        end
c--------------------------------------------------------------------------
        subroutine enter_edi_easily(aaa)
        include 'dialogstructure'
        type(dedit) aaa

c highlight box
        call highlight_edi(aaa)
c get another key
        read(*,'(a)') aaa%value
        call clear_text_in_box(aaa%x,aaa%y,
     &            aaa%label_len,1,aaa%label_len,0)
        call pgsci(1)
c return box to normal
        call display_edi(aaa,.true.,.true.)
        end
c--------------------------------------------------------------------------
        subroutine enter_int(aaa,x,y)
        integer pos
        character ans*1
        logical parint,in_int
        real x,y
        include 'dialogstructure'
        type(dinteger) aaa

 100    continue
c highlight box
        call highlight_int(aaa)
c get another key
        call pgcurse(x,y,ans)
        if (in_int(aaa,x,y)) then
c if next key is delete, wipe the whole thing and set position to 1
           if (ichar(ans).eq.127) then
              aaa%str_val=' '
              pos = 1
              call highlight_int(aaa)
              call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,1,1,0)
           else
c determine position
             call loc_pos_in_edit(x,aaa%x,aaa%label_len,pos)
             if (pos.le.aaa%maxlen) then
                call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,
     &             pos,pos,0)
                call pgsci(1)
                call write_letter_in_box(aaa%x,aaa%y,
     &             aaa%label_len,aaa%str_val,pos)
             end if
           end if
         call pgcurse(x,y,ans)
         do while (in_int(aaa,x,y))
          if (ichar(ans).eq.127) then
c delete previous character
            if (pos.gt.1) then
              call clear_text_in_box(aaa%x,aaa%y,
     &               aaa%label_len,pos,pos,1)
              call pgsci(0)
              call write_letter_in_box(aaa%x,aaa%y,aaa%label_len,
     &              aaa%str_val,pos)
              pos = pos-1        
              aaa%str_val(pos:pos)=' '
              call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,
     &              pos,pos,0)
              call pgsci(1)
              call write_letter_in_box(aaa%x,aaa%y,
     &             aaa%label_len,aaa%str_val,pos)
            end if           
          else if (pos.le.aaa%maxlen)then
           aaa%str_val(pos:pos)=ans
       call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,pos,pos,1)
       call pgsci(0)
       call write_letter_in_box(aaa%x,aaa%y,
     &  aaa%label_len,aaa%str_val,pos)
          if (pos+1.le.aaa%maxlen) then
       call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,pos+1,pos+1,0)
       call pgsci(1)
       call write_letter_in_box(aaa%x,aaa%y,
     &  aaa%label_len,aaa%str_val,pos+1)
          end if
           pos = min(pos + 1,aaa%maxlen)
          end if
           call pgcurse(x,y,ans)
         end do
        end if
c
c turn string into an integer (if possible) otherwise

        if (.not. parint(aaa%str_val,aaa%value)) goto 100

c return box to normal
        call display_int(aaa,.true.,.true.)
        end
c--------------------------------------------------------------------------
        subroutine enter_int_easily(aaa)
        logical parint
        include 'dialogstructure'
        type(dinteger) aaa

 100    continue
c highlight box
        call highlight_int(aaa)
        read(*,'(a)') aaa%str_val
        call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,
     &    1,aaa%label_len,0)
        call pgsci(1)
c turn string into an integer (if possible) otherwise
        if (.not. parint(aaa%str_val,aaa%value)) goto 100
c return box to normal
        call display_int(aaa,.true.,.true.)
        end
c--------------------------------------------------------------------------
        subroutine enter_rea(aaa,x,y)
        integer pos
        character ans*1
        logical pareal,in_rea
        real x,y
        include 'dialogstructure'
        type(dinteger) aaa

 100    continue
c highlight box
        call highlight_rea(aaa)
c get another key
        call pgcurse(x,y,ans)
        if (in_rea(aaa,x,y)) then
c if next key is delete, wipe the whole thing and set position to 1
           if (ichar(ans).eq.127) then
              aaa%str_val=' '
              pos = 1
              call highlight_rea(aaa)
              call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,1,1,0)
           else
c determine position
            call loc_pos_in_edit(x,aaa%x,aaa%label_len,pos)
            if (pos+1.le.aaa%maxlen) then
              call clear_text_in_box(aaa%x,aaa%y,
     &            aaa%label_len,pos,pos,0)
              call pgsci(1)
              call write_letter_in_box(aaa%x,aaa%y,
     &            aaa%label_len,aaa%str_val,pos)
            end if
           end if
         call pgcurse(x,y,ans)
         do while (in_rea(aaa,x,y))
          if (ichar(ans).eq.127) then
c delete previous character
            if (pos.gt.1) then
              call clear_text_in_box(aaa%x,aaa%y,
     &               aaa%label_len,pos,pos,1)
              call pgsci(0)
              call write_letter_in_box(aaa%x,aaa%y,aaa%label_len,
     &              aaa%str_val,pos)
              pos = pos-1        
              aaa%str_val(pos:pos)=' '
              call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,
     &              pos,pos,0)
              call pgsci(1)
              call write_letter_in_box(aaa%x,aaa%y,
     &             aaa%label_len,aaa%str_val,pos)
            end if           
          else if (pos.le.aaa%maxlen)then
             aaa%str_val(pos:pos)=ans
             call clear_text_in_box(aaa%x,aaa%y,
     &            aaa%label_len,pos,pos,1)
             call pgsci(0)
             call write_letter_in_box(aaa%x,aaa%y,
     &            aaa%label_len,aaa%str_val,pos)
             if (pos+1.le.aaa%maxlen) then
                call clear_text_in_box(aaa%x,aaa%y,
     &             aaa%label_len,pos+1,pos+1,0)
                call pgsci(1)
                call write_letter_in_box(aaa%x,aaa%y,
     &             aaa%label_len,aaa%str_val,pos+1)
             end if
             pos = pos + 1
          end if
           call pgcurse(x,y,ans)
         end do
        end if
c
c turn string into a real (if possible) otherwise

        if (.not. pareal(aaa%str_val,aaa%value)) goto 100

c return box to normal
        call display_rea(aaa,.true.,.true.)
        end
c--------------------------------------------------------------------------
        subroutine enter_rea_easily(aaa)
        logical pareal
        include 'dialogstructure'
        type(dreal) aaa

 100    continue
c highlight box
        call highlight_rea(aaa)
        read(*,'(a)') aaa%str_val
        call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,
     &    1,aaa%label_len,0)
        call pgsci(1)
c turn string into an real (if possible) otherwise
        if (.not. pareal(aaa%str_val,aaa%value)) goto 100
c return box to normal
        call display_rea(aaa,.true.,.true.)
        end
c--------------------------------------------------------------------------
        subroutine enter_dou(aaa,x,y)
C PAH - added the position & MJD functions
        integer pos
        character ans*1
        logical pardbl,in_dou,parpos,parjt
        real x,y
        include 'dialogstructure'
        type(ddouble) aaa

 100    continue
c highlight box
        call highlight_dou(aaa)
c get another key
        call pgcurse(x,y,ans)
        if (in_dou(aaa,x,y)) then
c if next key is delete, wipe the whole thing and set position to 1
           if (ichar(ans).eq.127) then
              aaa%str_val=' '
              pos = 1
              call highlight_dou(aaa)
              call clear_text_in_box(aaa%x,aaa%y,
     &              aaa%label_len,1,1,0)
           else
c determine position
         call loc_pos_in_edit(x,aaa%x,aaa%label_len,pos)
            if (pos+1.le.aaa%maxlen) then
               call clear_text_in_box(aaa%x,aaa%y,
     &          aaa%label_len,pos,pos,0)
                call pgsci(1)
                call write_letter_in_box(aaa%x,aaa%y,
     &          aaa%label_len,aaa%str_val,pos)
            end if
          end if
         call pgcurse(x,y,ans)
         do while (in_dou(aaa,x,y))
          if (ichar(ans).eq.127) then
c delete previous character
            if (pos.gt.1) then
              call clear_text_in_box(aaa%x,aaa%y,
     &               aaa%label_len,pos,pos,1)
              call pgsci(0)
              call write_letter_in_box(aaa%x,aaa%y,aaa%label_len,
     &              aaa%str_val,pos)
              pos = pos-1        
              aaa%str_val(pos:pos)=' '
              call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,
     &              pos,pos,0)
              call pgsci(1)
              call write_letter_in_box(aaa%x,aaa%y,
     &             aaa%label_len,aaa%str_val,pos)
            end if           
          else if (pos.le.aaa%maxlen)then
           aaa%str_val(pos:pos)=ans
       call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,pos,pos,1)
       call pgsci(0)
       call write_letter_in_box(aaa%x,aaa%y,
     &  aaa%label_len,aaa%str_val,pos)
          if (pos+1.le.aaa%maxlen) then
       call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,pos+1,pos+1,0)
       call pgsci(1)
       call write_letter_in_box(aaa%x,aaa%y,
     &  aaa%label_len,aaa%str_val,pos+1)
          end if
           pos = pos + 1
          end if
           call pgcurse(x,y,ans)
         end do
        end if
c
c turn string into a double (if possible) otherwise

        if(aaa%type .eq. 1) then
           if (.not. parpos(aaa%str_val,aaa%value)) goto 100
        elseif(aaa%type .eq. 2) then
           if (.not. parjt(aaa%str_val,aaa%value)) goto 100
        else
           if (.not. pardbl(aaa%str_val,aaa%value)) goto 100
        endif

c return box to normal
        call display_dou(aaa,.true.,.true.)
        end
c--------------------------------------------------------------------------
        subroutine enter_dou_easily(aaa)
        logical pardbl,parpos,parjt
        include 'dialogstructure'
        type(ddouble) aaa

 100    continue
c highlight box
        call highlight_dou(aaa)
        read(*,'(a)') aaa%str_val
        call clear_text_in_box(aaa%x,aaa%y,aaa%label_len,
     &    1,aaa%label_len,0)
        call pgsci(1)
c turn string into a double (if possible) otherwise
        if(aaa%type .eq. 1) then
           if (.not. parpos(aaa%str_val,aaa%value)) goto 100
        elseif(aaa%type .eq. 2) then
           if (.not. parjt(aaa%str_val,aaa%value)) goto 100
        else
           if (.not. pardbl(aaa%str_val,aaa%value)) goto 100
        endif
c return box to normal
        call display_dou(aaa,.true.,.true.)
        end
c--------------------------------------------------------------------------
        subroutine loc_pos_in_edit(x,ex,len,pos)
        real x,ex
        integer len,pos

        pos = x - ex - len
        end

c-----------------------------------------------------------------------
        subroutine highlight_edi(aaa)
        include 'dialogstructure'
        type(dedit) aaa

        call pgsci(1)
        call pgsfs(1)
        call pgrect(aaa%x+aaa%label_len+1.0,
     &   aaa%x+aaa%label_len+1.0+aaa%maxlen,
     & aaa%y-0.2, aaa%y+1.2 )
        call pgsci(0)
        call pgsfs(2)
        call pgrect(aaa%x+aaa%label_len+1.0,
     &   aaa%x+aaa%label_len+1.0+aaa%maxlen,
     & aaa%y-0.2, aaa%y+1.2 )
c redraw string
        call write_text_in_box(aaa%x,aaa%y,aaa%label_len,
     &   aaa%value,aaa%maxlen)
        call pgupdt
        end
c-----------------------------------------------------------------------
        subroutine highlight_int(aaa)
        include 'dialogstructure'
        type(dinteger) aaa

        call pgsci(1)
        call pgsfs(1)
        call pgrect(aaa%x+aaa%label_len+1.0,
     &   aaa%x+aaa%label_len+1.0+aaa%maxlen,
     & aaa%y-0.2, aaa%y+1.2 )
        call pgsci(0)
        call pgsfs(2)
        call pgrect(aaa%x+aaa%label_len+1.0,
     &   aaa%x+aaa%label_len+1.0+aaa%maxlen,
     & aaa%y-0.2, aaa%y+1.2 )
c redraw string
        call write_text_in_box(aaa%x,aaa%y,aaa%label_len,
     &   aaa%str_val,aaa%maxlen)
        call pgupdt
        end
c-----------------------------------------------------------------------
        subroutine highlight_rea(aaa)
        include 'dialogstructure'
        type(dreal) aaa

        call pgsci(1)
        call pgsfs(1)
        call pgrect(aaa%x+aaa%label_len+1.0,
     &   aaa%x+aaa%label_len+1.0+aaa%maxlen,
     & aaa%y-0.2, aaa%y+1.2 )
        call pgsci(0)
        call pgsfs(2)
        call pgrect(aaa%x+aaa%label_len+1.0,
     &   aaa%x+aaa%label_len+1.0+aaa%maxlen,
     & aaa%y-0.2, aaa%y+1.2 )
c redraw string
        call write_text_in_box(aaa%x,aaa%y,aaa%label_len,
     &   aaa%str_val,aaa%maxlen)
        call pgupdt
        end
c-----------------------------------------------------------------------
        subroutine highlight_dou(aaa)
        include 'dialogstructure'
        type(ddouble) aaa

        call pgsci(1)
        call pgsfs(1)
        call pgrect(aaa%x+aaa%label_len+1.0,
     &   aaa%x+aaa%label_len+1.0+aaa%maxlen,
     & aaa%y-0.2, aaa%y+1.2 )
        call pgsci(0)
        call pgsfs(2)
        call pgrect(aaa%x+aaa%label_len+1.0,
     &   aaa%x+aaa%label_len+1.0+aaa%maxlen,
     & aaa%y-0.2, aaa%y+1.2 )
c redraw string
        call write_text_in_box(aaa%x,aaa%y,aaa%label_len,
     &   aaa%str_val,aaa%maxlen)
        call pgupdt
        end
c-----------------------------------------------------------------------
        subroutine move_widget(dialog,w,a,x1,y1,x2,y2)
        include 'dialogstructure'
        integer w,a
        real x1,y1,x2,y2
        type(dlg) dialog

        if (w.eq.1) then
          dialog%dste(a)%x=x1
          dialog%dste(a)%y=y1
        else if (w.eq.2) then
          dialog%dvte(a)%x=x1
          dialog%dvte(a)%y=y1
        else if (w.eq.3) then
          dialog%drad(a)%x=x1+1.35
          dialog%drad(a)%y=y1-0.4
        else if (w.eq.4) then
          dialog%dche(a)%x=x1+1.5
          dialog%dche(a)%y=y1-0.5
        else if (w.eq.5) then
          dialog%dint(a)%x=x1-dialog%dint(a)%label_len-1.0
          dialog%dint(a)%y=y1
        else if (w.eq.6) then
          dialog%drea(a)%x=x1-dialog%drea(a)%label_len-1.0
          dialog%drea(a)%y=y1
        else if (w.eq.7) then
          dialog%ddou(a)%x=x1-dialog%ddou(a)%label_len-1.0
          dialog%ddou(a)%y=y1
        else if (w.eq.8) then
          dialog%dedi(a)%x=x1-dialog%dedi(a)%label_len-1.0
          dialog%dedi(a)%y=y1
        else if (w.eq.9) then
          dialog%dbut(a)%x=x1
          dialog%dbut(a)%y=y1
        else if (w.eq.10) then
          dialog%dplo(a)%x1=min(x1,x2)
          dialog%dplo(a)%y1=min(y1,y2)
          dialog%dplo(a)%x2=max(x1,x2)
          dialog%dplo(a)%y2=max(y1,y2)
        else if (w.eq.11) then
          dialog%dout(a)%x1=min(x1,x2)
          dialog%dout(a)%y1=min(y1,y2)
          dialog%dout(a)%x2=max(x1,x2)
          dialog%dout(a)%y2=max(y1,y2)
        end if
        end
c-------------------------------------------------------------------
        subroutine display_widget(dialog,w,a)
        include 'dialogstructure'
        integer w,a
        type(dlg) dialog

        if (w.eq.1) then
           call display_ste(dialog%dste(w),.false.,.true.)
        else if (w.eq.2) then
           call display_vte(dialog%dvte(w),.false.,.true.)
        else if (w.eq.3) then
           call display_rad(dialog%drad(w),.false.,.true.)
        else if (w.eq.4) then
           call display_che(dialog%dche(w),.false.,.true.)
        else if (w.eq.5) then
           call display_int(dialog%dint(w),.false.,.true.)
        else if (w.eq.6) then
           call display_rea(dialog%drea(w),.false.,.true.)
        else if (w.eq.7) then
           call display_dou(dialog%ddou(w),.false.,.true.)
        else if (w.eq.8) then
           call display_edi(dialog%dedi(w),.false.,.true.)
        else if (w.eq.9) then
           call display_but(dialog%dbut(w),.false.,.true.)
        else if (w.eq.10) then
           call display_plo(dialog%dplo(w),.false.,.true.)
        else if (w.eq.11) then
           call display_out(dialog%dout(w),.false.,.true.)
        end if
        end
