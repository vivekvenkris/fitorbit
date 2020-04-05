# Makefile for dialog subs.
include ../Makefile_Custom

LIBDIALOG = libdialog.a

LIBDIALOGOBJECTS=\
$(LIBDIALOG)(dialsubs.o) $(LIBDIALOG)(readwrite.o) $(LIBDIALOG)(dialogsown.o)\
$(LIBDIALOG)(get_setup.o) $(LIBDIALOG)(activatesubs.o) \
$(LIBDIALOG)(dialogunix.o) $(LIBDIALOG)(dialogstd.o)

$(LIBDIALOG): $(LIBDIALOGOBJECTS) 
	cp $(LIBDIALOG)  $(LIBDIR)

.PRECIOUS : $(LIBDIALOG)

