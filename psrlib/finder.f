c--------------
        subroutine finder(
     & replot,x,y,ans,plotno,button,
     &  vte1,vte2,vte3,vte4,vte5,vte6,vte7,vte8                                 
     & )
 
c written with the dialog program
c       Matthew Bailes 
 
c there are 10 buttons.
c the label for each is listed below
c number  1 select                          
c number  2 select                          
c number  3 select                          
c number  4 select                          
c number  5 select                          
c number  6 select                          
c number  7 select                          
c number  8 Up                              
c number  9 Down                            
c number 10 Abort                           
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
 
        character*(*)      vte1,vte2,vte3,vte4,vte5,vte6,vte7,vte8              
        integer status
        logical first
        real x,y
        character ans
        integer replot,plotno,button
        data first /.true./
        include 'dialogstructure'
        record /dlg/ dialog
        save
c-------------------------------------------
        if (first) then
        call read_dialog('PSRLIBDIR',
     &'finder',dialog,status)
        first=.false. 
        end if 
 
        call dialog_reset
        dialog.dvte(1).label=vte1
        dialog.dvte(2).label=vte2
        dialog.dvte(3).label=vte3
        dialog.dvte(4).label=vte4
        dialog.dvte(5).label=vte5
        dialog.dvte(6).label=vte6
        dialog.dvte(7).label=vte7
        dialog.dvte(8).label=vte8
        call dialogstd(plotno,x,y,ans,button,replot,dialog)
        return
        end
