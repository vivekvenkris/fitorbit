# @(#)Makefile	1.5 
include ../Makefile_Custom

OBJECTS = fitorbit.o  fitorbitdialog.o psr_read_per.o sindcosdtand.o

fitorbit :  $(OBJECTS)
	$(FC) -o $(BINL)/fitorbit  $(OBJECTS) -L $(LIBDIR) -ldialog  -lpsr -lsla $(LPGPLOT) 

#fitorbit :  $(OBJECTS)
#	$(LINK.f) -o $(BINL)/fitorbit  $(OBJECTS) -L $(LIBDIR) -ldialog -lpsr -lsla $(LPGPLOT) 

conveph: conveph.o
	$(LINK.f) -o $@ conveph.o -lpsr -lsla -lutil

splitper: splitper.o
	$(LINK.f) -o $@ splitper.o -lpsr -lsla -lutil

obs2bc : obs2bc.o
	$(LINK.f) -o $(BINL)/obs2bc obs2bc.o  -lutil -lpsr -lsla 


plotfitorbit : plotfitorbit.o
	$(LINK.f) -o $(BINL)/plotfitorbit plotfitorbit.o $(LPGPLOT)


