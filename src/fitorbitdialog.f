c--------------
        subroutine fitorbitdialog(
     & replot,x,y,ans,plotno,button,
     &  vte1,vte2,vte3,vte4,vte5,vte6,vte7,vte8,vte9                            
     & ,vte10,vte11,vte12,vte13                                                 
     & ,rad1,rad2,rad3,rad4,rad5,rad6,rad7,rad8,rad9                            
     & ,rad10,rad11                                                             
     & ,che1,che2,che3,che4,che5,che6,che7,che8,che9                            
     & ,che10,che11,che12,che13                                                 
     & ,rea1                                                                    
     & ,dou1,dou2,dou3,dou4,dou5,dou6,dou7,dou8,dou9                            
     & ,edi1                                                                    
     & )
 
c written with the dialog program
c       Matthew Bailes 
 
c there are 10 buttons.
c the label for each is listed below
c number  1 Fit Uaf Plot                    
c number  2 Restore Defaults                
c number  3 Show fit                        
c number  4 WR Fit                          
c number  5 Quit                            
c number  6 Load                            
c number  7 Output for PSRTIME              
c number  8 Residual                        
c number  9 Period                          
c number 10 ALL                             
c there are 11 radio controls.
c the label for each is listed below
c radio control number  1 Delete                          
c radio control number  2 Rectangular Delete              
c radio control number  3 Start                           
c radio control number  4 Finish                          
c radio control number  5 Zoom                            
c radio control number  6 Set epoch                       
c radio control number  7 Unzoom                          
c radio control number  8 Identify                        
c radio control number  9 Epoch                           
c radio control number 10 Phase                           
c radio control number 11 Restore                         
c there are 13 check boxes.
c the label for each is listed below
c check box number  1 Period                          
c check box number  2 Pdot                            
c check box number  3 asini                           
c check box number  4 e                               
c check box number  5 Omega                           
c check box number  6 Bin Period                      
c check box number  7 Weights                         
c check box number  8 Errors                          
c check box number  9 Deleted                         
c check box number 10 Hardcopy                        
c check box number 11 RA                              
c check box number 12 DEC                             
c check box number 13 TASC                            
c there are  0 editable integers.
c the label for each is listed below
c there are  1 editable reals.
c the label for each is listed below
c editable real number  1 Factor                          
c there are  9 editable doubles.
c the label for each is listed below
c editable double number  1                                 
c editable double number  2                                 
c editable double number  3                                 
c editable double number  4                                 
c editable double number  5                                 
c editable double number  6                                 
c editable double number  7                                 
c editable double number  8                                 
c editable double number  9                                 
c there are  1 editable strings.
c the label for each is listed below
c editable text number  1 PSR                             
 
        character*(*)      vte1,vte2,vte3,vte4,vte5,vte6,vte7,vte8,vte9         
     & ,vte10,vte11,vte12,vte13                                                 
        logical            rad1,rad2,rad3,rad4,rad5,rad6,rad7,rad8,rad9         
     & ,rad10,rad11                                                             
        logical            che1,che2,che3,che4,che5,che6,che7,che8,che9         
     & ,che10,che11,che12,che13                                                 
        real               rea1                                                 
        double precision   dou1,dou2,dou3,dou4,dou5,dou6,dou7,dou8,dou9         
        character*(*)      edi1                                                 
        integer status
        logical first
        real x,y
        character ans
        integer replot,plotno,button
        integer npl
        real xmin,xmax,ymin,ymax
        data first /.true./
        include 'dialogstructure'
        type (dlg) dialog
c-------------------------------------------
        save
        if (first) then
        call read_dialog('fitorbitdir',
     &'fitorbitdialog',dialog,status)
        first=.false. 
        end if 
 
        call dialog_reset
        dialog%dvte(1)%label=vte1
        dialog%dvte(2)%label=vte2
        dialog%dvte(3)%label=vte3
        dialog%dvte(4)%label=vte4
        dialog%dvte(5)%label=vte5
        dialog%dvte(6)%label=vte6
        dialog%dvte(7)%label=vte7
        dialog%dvte(8)%label=vte8
        dialog%dvte(9)%label=vte9
        dialog%dvte(10)%label=vte10
        dialog%dvte(11)%label=vte11
        dialog%dvte(12)%label=vte12
        dialog%dvte(13)%label=vte13
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
        dialog%dche(2)%value=che2
        dialog%dche(3)%value=che3
        dialog%dche(4)%value=che4
        dialog%dche(5)%value=che5
        dialog%dche(6)%value=che6
        dialog%dche(7)%value=che7
        dialog%dche(8)%value=che8
        dialog%dche(9)%value=che9
        dialog%dche(10)%value=che10
        dialog%dche(11)%value=che11
        dialog%dche(12)%value=che12
        dialog%dche(13)%value=che13
        dialog%drea(1)%value=rea1
        dialog%ddou(1)%value=dou1
        dialog%ddou(2)%value=dou2
        dialog%ddou(3)%value=dou3
        dialog%ddou(4)%value=dou4
        dialog%ddou(5)%value=dou5
        dialog%ddou(6)%value=dou6
        dialog%ddou(7)%value=dou7
        dialog%ddou(8)%value=dou8
        dialog%ddou(9)%value=dou9
        dialog%dedi(1)%value=edi1
        call dialogstd(plotno,x,y,ans,button,replot,dialog)
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
        che2=dialog%dche(2)%value
        che3=dialog%dche(3)%value
        che4=dialog%dche(4)%value
        che5=dialog%dche(5)%value
        che6=dialog%dche(6)%value
        che7=dialog%dche(7)%value
        che8=dialog%dche(8)%value
        che9=dialog%dche(9)%value
        che10=dialog%dche(10)%value
        che11=dialog%dche(11)%value
        che12=dialog%dche(12)%value
        che13=dialog%dche(13)%value
        rea1=dialog%drea(1)%value
        dou1=dialog%ddou(1)%value
        dou2=dialog%ddou(2)%value
        dou3=dialog%ddou(3)%value
        dou4=dialog%ddou(4)%value
        dou5=dialog%ddou(5)%value
        dou6=dialog%ddou(6)%value
        dou7=dialog%ddou(7)%value
        dou8=dialog%ddou(8)%value
        dou9=dialog%ddou(9)%value
        edi1=dialog%dedi(1)%value
        return
        Entry qplfitorbitdialog(npl,xmin,xmax,ymin,ymax)
        xmin=dialog%dplo(npl)%vx1
        xmax=dialog%dplo(npl)%vx2
        ymin=dialog%dplo(npl)%vy1
        ymax=dialog%dplo(npl)%vy2
        return
        Entry qvplfitorbitdialog(npl,xmin,xmax,ymin,ymax)
        call dialog_inq_vport(dialog,npl,xmin,xmax,ymin,ymax)
        return
        Entry selplfitorbitdialog(npl)
        call dialog_select_plot(dialog,npl)
        return
        Entry setplfitorbitdialog(npl,xmin,xmax,ymin,ymax)
        call dialog_window_set(dialog,npl,xmin,xmax,ymin,ymax)
        return
        Entry xplfitorbitdialog(npl,xmin,xmax,ymin,ymax)
        call new_erase_plot(dialog,npl,xmin,xmax,ymin,ymax) 
        return 
        end
        
        Subroutine plfitorbitdialog(func,wn,xmin,xmax,ymin,ymax)
        character*1 func
        integer wn
        real xmin,xmax,ymin,ymax
        if ((func.eq.'q').or.(func.eq.'Q')) then
          call qplfitorbitdialog(wn,xmin,xmax,ymin,ymax)
        elseif ((func.eq.'V').or.(func.eq.'v')) then
          call qvplfitorbitdialog(wn,xmin,xmax,ymin,ymax)
        else if ((func.eq.'x').or.(func.eq.'X')) then
          call xplfitorbitdialog(wn,xmin,xmax,ymin,ymax)
        else if ((func.eq.'s').or.(func.eq.'S')) then
          call setplfitorbitdialog(wn,xmin,xmax,ymin,ymax)
        else if ((func.eq.'r').or.(func.eq.'R')) then
          call selplfitorbitdialog(wn)
        end if
        end
