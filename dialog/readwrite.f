c-------------------------------------------------------------------
        subroutine write_dialog(name,dialog,status)
c
c  routine writes out the dialog to a file dialognnn.rsz
c  where nnn is a number less than 1000.
c
        include 'dialogstructure'
        type(dlg) dialog
        integer i,status,d_number,length,ilength
        real temp
        character name*(*)

        d_number = dialog%number
        status = 0

        open(unit=32,file=name(1:length(name))//'.rsz',
     &   form='formatted',status='new',err=432)
        goto 433
 432    continue
        open(unit=32,file=name(1:length(name))//'.rsz',
     &   form='formatted',status='old')
 433    continue
        write(32,'(1x,i3,4(1x,f9.4))') 111,dialog%dxmin,dialog%dxmax,
     &     dialog%dymin,dialog%dymax
        write(32,'(1x,11(1x,i2))') dialog%ndste,dialog%ndvte,
     & dialog%ndrad,dialog%ndche,dialog%ndint,dialog%ndrea,dialog%nddou,
     & dialog%ndedi,dialog%ndbut,dialog%ndplo,dialog%ndout

        do i = 1,dialog%ndste
         ilength=dialog%dste(i)%label_len
         write(32,'(i3,1x,a)') dialog%dste(i)%label_len,
     &  dialog%dste(i)%label(1:ilength)
         write(32,'(f12.6,1x,f12.6)')dialog%dste(i)%x,dialog%dste(i)%y
        end do
        do i = 1,dialog%ndvte
         write(32,'(i3)') dialog%dvte(i)%label_len
         write(32,'(f12.6,1x,f12.6)')dialog%dvte(i)%x,dialog%dvte(i)%y
        end do
        do i = 1,dialog%ndrad
         ilength=dialog%drad(i)%label_len
         write(32,'(i3)') dialog%drad(i)%label_len
       write(32,'(a)') dialog%drad(i)%label(1:ilength)
         write(32,'(f12.6,1x,f12.6)')dialog%drad(i)%x,dialog%drad(i)%y
         write(32,'(i6)') dialog%drad(i)%group
        end do
        do i = 1,dialog%ndche
         ilength=dialog%dche(i)%label_len
         write(32,'(i3)') dialog%dche(i)%label_len
       write(32,'(a)') dialog%dche(i)%label(1:ilength)
         write(32,'(f12.6,1x,f12.6)')dialog%dche(i)%x,dialog%dche(i)%y
        end do
        do i = 1,dialog%ndint
         ilength=dialog%dint(i)%label_len
         write(32,'(i3)') dialog%dint(i)%label_len
       write(32,'(a)') dialog%dint(i)%label(1:ilength)
         write(32,'(f12.6,1x,f12.6)')dialog%dint(i)%x,dialog%dint(i)%y
         write(32,'(i6)') dialog%dint(i)%maxlen
        end do
        do i = 1,dialog%ndrea
         ilength=dialog%drea(i)%label_len
         write(32,'(i3)') dialog%drea(i)%label_len
       write(32,'(a)') dialog%drea(i)%label(1:ilength)
         write(32,'(f12.6,1x,f12.6)')dialog%drea(i)%x,dialog%drea(i)%y
         write(32,'(i6)') dialog%drea(i)%maxlen
        end do
        do i = 1,dialog%nddou
         ilength=dialog%ddou(i)%label_len
         write(32,'(i3)') dialog%ddou(i)%label_len
       write(32,'(a)') dialog%ddou(i)%label(1:ilength)
         write(32,'(f12.6,1x,f12.6)')dialog%ddou(i)%x,dialog%ddou(i)%y
         write(32,'(i6,x,i2)') dialog%ddou(i)%maxlen,
     :            dialog%ddou(i)%type
        end do
        do i = 1,dialog%ndedi
         ilength=dialog%dedi(i)%label_len
         write(32,'(i3)') dialog%dedi(i)%label_len
       write(32,'(a)') dialog%dedi(i)%label(1:ilength)
         write(32,'(f12.6,1x,f12.6)')dialog%dedi(i)%x,dialog%dedi(i)%y
         write(32,'(i6)') dialog%dedi(i)%maxlen
        end do
        do i = 1,dialog%ndbut
         ilength=dialog%dbut(i)%label_len
         write(32,'(i3)') dialog%dbut(i)%label_len
       write(32,'(a)') dialog%dbut(i)%label(1:ilength)
         write(32,'(f12.6,1x,f12.6)')dialog%dbut(i)%x,dialog%dbut(i)%y
        end do
        do i = 1,dialog%ndplo
        if (dialog%dplo(i)%x1.gt.dialog%dplo(i)%x2) then
          temp=dialog%dplo(i)%x2
          dialog%dplo(i)%x2=dialog%dplo(i)%x1
          dialog%dplo(i)%x1=temp
        end if
        if (dialog%dplo(i)%y1.gt.dialog%dplo(i)%y2) then
          temp=dialog%dplo(i)%y2
          dialog%dplo(i)%y2=dialog%dplo(i)%y1
          dialog%dplo(i)%y1=temp
        end if
        write(32,'(f12.6,1x,f12.6)')dialog%dplo(i)%x1,dialog%dplo(i)%y1
        write(32,'(f12.6,1x,f12.6)')dialog%dplo(i)%x2,dialog%dplo(i)%y2
        end do
        do i = 1,dialog%ndout
        write(32,'(f12.6,1x,f12.6)')dialog%dout(i)%x1,dialog%dout(i)%y1
        write(32,'(f12.6,1x,f12.6)')dialog%dout(i)%x2,dialog%dout(i)%y2
        write(32,'(i3,1x,i3)')dialog%dout(i)%colour,
     &    dialog%dout(i)%linewidth
        end do

        status = 1
        close(32)

        end
c-------------------------------------------------------------------
        subroutine read_dialog(logname,name,dialog,status)
c
c  routine writes out the dialog to a file dialognnn.rsz
c  where nnn is a number less than 1000.
c
        character*(*) logname,name

        integer i,length
        integer status
        include 'dialogstructure'
        type(dlg) dialog
        CHARACTER(len=255) :: dialog_home
        CHARACTER(len=255) :: rszfile
        real tempmin,tempmax
        status = 1
c        call psrgetenv(logname,file_name)
        call getenv(logname, dialog_home)
        write(*,*)' env ',logname,'  is ', dialog_home, ' and file is:',
     &       name
        rszfile=dialog_home(1:length(dialog_home))//name(1:length(name))
     & //'.rsz'
        write(*,*)' opening ', rszfile
        open(unit=32,file=rszfile,form='formatted',
     &        status='old',err=999)
c
c read in dialog number and borders - if error set borders to maximum
c
 90     continue

        read(32,'(1x,i3,4(1x,f9.4))',err=100)dialog%number,dialog%dxmin,
     & dialog%dxmax,dialog%dymin,dialog%dymax
        goto 101
 100    continue
        dialog%dxmin = 0.0
        dialog%dxmax = 1.0
        dialog%dymin = 0.0
        dialog%dymax = 1.0
 101    continue
        dialog%number = 0
        read(32,'(1x,11(1x,i2))') dialog%ndste,dialog%ndvte,
     & dialog%ndrad,dialog%ndche,dialog%ndint,dialog%ndrea,dialog%nddou,
     & dialog%ndedi,dialog%ndbut,dialog%ndplo,dialog%ndout

        do i=1,dialog%ndste
         read(32,'(i3,1x,a)') dialog%dste(i)%label_len,
     &  dialog%dste(i)%label
         read(32,'(f12.6,1x,f12.6)')dialog%dste(i)%x,dialog%dste(i)%y
        end do
        do i=1,dialog%ndvte
         read(32,'(i3)') dialog%dvte(i)%label_len
         read(32,'(f12.6,1x,f12.6)')dialog%dvte(i)%x,dialog%dvte(i)%y
        end do
        do i=1,dialog%ndrad
         read(32,'(i3)') dialog%drad(i)%label_len
         read(32,'(a)') dialog%drad(i)%label
         read(32,'(f12.6,1x,f12.6)')dialog%drad(i)%x,dialog%drad(i)%y
         read(32,'(i6)') dialog%drad(i)%group
        end do
        do i=1,dialog%ndche
         read(32,'(i3)') dialog%dche(i)%label_len
         read(32,'(a)') dialog%dche(i)%label
         read(32,'(f12.6,1x,f12.6)')dialog%dche(i)%x,dialog%dche(i)%y
        end do
        do i=1,dialog%ndint
         read(32,'(i3)') dialog%dint(i)%label_len
         read(32,'(a)') dialog%dint(i)%label
         read(32,'(f12.6,1x,f12.6)')dialog%dint(i)%x,dialog%dint(i)%y
         read(32,'(i6)') dialog%dint(i)%maxlen
        end do
        do i=1,dialog%ndrea
         read(32,'(i3)') dialog%drea(i)%label_len
         read(32,'(a)') dialog%drea(i)%label
         read(32,'(f12.6,1x,f12.6)')dialog%drea(i)%x,dialog%drea(i)%y
         read(32,'(i6)') dialog%drea(i)%maxlen
        end do
        do i=1,dialog%nddou
         read(32,'(i3)') dialog%ddou(i)%label_len
         read(32,'(a)') dialog%ddou(i)%label
         read(32,'(f12.6,1x,f12.6)')dialog%ddou(i)%x,dialog%ddou(i)%y
         read(32,'(i6,x,i2)') dialog%ddou(i)%maxlen,
     :         dialog%ddou(i)%type
        end do
        do i=1,dialog%ndedi
         read(32,'(i3)') dialog%dedi(i)%label_len
         read(32,'(a)') dialog%dedi(i)%label
         read(32,'(f12.6,1x,f12.6)')dialog%dedi(i)%x,dialog%dedi(i)%y
         read(32,'(i6)') dialog%dedi(i)%maxlen
        end do
        do i=1,dialog%ndbut
         read(32,'(i3)') dialog%dbut(i)%label_len
         read(32,'(a)') dialog%dbut(i)%label
         read(32,'(f12.6,1x,f12.6)')dialog%dbut(i)%x,dialog%dbut(i)%y
        end do
        do i=1,dialog%ndplo
         read(32,'(f12.6,1x,f12.6)')dialog%dplo(i)%x1,dialog%dplo(i)%y1
         read(32,'(f12.6,1x,f12.6)')dialog%dplo(i)%x2,dialog%dplo(i)%y2
C put in some default plot limits to stop nasties - Paul Harrison 19-MAR-1991
         dialog%dplo(i)%vx1=0.0
         dialog%dplo(i)%vx2=1.0
         dialog%dplo(i)%vy1=0.0
         dialog%dplo(i)%vy2=1.0
c make sure x1<x2 and same for y (can lead to awful bugs)
         tempmin=min(dialog%dplo(i)%x1,dialog%dplo(i)%x2)
         tempmax=max(dialog%dplo(i)%x1,dialog%dplo(i)%x2)
         dialog%dplo(i)%x1=tempmin
         dialog%dplo(i)%x2=tempmax
         tempmin=min(dialog%dplo(i)%y1,dialog%dplo(i)%y2)
         tempmax=max(dialog%dplo(i)%y1,dialog%dplo(i)%y2)
         dialog%dplo(i)%y1=tempmin
         dialog%dplo(i)%y2=tempmax
        end do
        do i=1,dialog%ndout
         read(32,'(f12.6,1x,f12.6)')dialog%dout(i)%x1,dialog%dout(i)%y1
         read(32,'(f12.6,1x,f12.6)')dialog%dout(i)%x2,dialog%dout(i)%y2
         read(32,'(i3,1x,i3)')dialog%dout(i)%colour,
     & dialog%dout(i)%linewidth
        end do

        status = 1
        close(32)
c
c	read in the set up from the .dialog file 
c	in HOME directory -- colours of widgets
c	are in this at the moment (DRL 23/12/92)
c
	call get_setup
        return
 999    status=0
        write(*,*)' *** failed to open dialog resource file ***'

        end

