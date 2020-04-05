# @(#)Makefile	1.5 
include ../Makefile_Custom
LIBPSR= libpsr.a
LIBDIR=${FTORBIT}/lib
#
# Here are the PSRlib library routines!!
#
LIBPSROBJECTS=\
$(LIBPSR)(curdat.o) $(LIBPSR)(curtim.o) $(LIBPSR)(cdate.o) $(LIBPSR)(erform.o)\
$(LIBPSR)(checkb4write.o) $(LIBPSR)(cjtime.o) $(LIBPSR)(clshift.o) \
$(LIBPSR)(cmdjt.o) $(LIBPSR)(cdnumb.o) $(LIBPSR)(comstr.o) \
$(LIBPSR)(cpostn.o) $(LIBPSR)(cpsrid.o) $(LIBPSR)(cpsridj.o) \
$(LIBPSR)(crjust.o) $(LIBPSR)(crtims.o) $(LIBPSR)(ctimeh.o) \
$(LIBPSR)(datejd.o) $(LIBPSR)(dselect.o) $(LIBPSR)(ephio.o) \
$(LIBPSR)(glun.o) \
$(LIBPSR)(intcmd.o) $(LIBPSR)(jddate.o) \
$(LIBPSR)(length.o) $(LIBPSR)(lmm.o) $(LIBPSR)(lufree.o) \
$(LIBPSR)(outmon.o) $(LIBPSR)(outpag.o) $(LIBPSR)(output.o) \
$(LIBPSR)(parint.o) $(LIBPSR)(parjt.o) $(LIBPSR)(parpos.o) \
$(LIBPSR)(parrng.o) $(LIBPSR)(parse.o) $(LIBPSR)(partim.o) \
$(LIBPSR)(parunt.o) $(LIBPSR)(psrerr.o) $(LIBPSR)(rmplus.o) \
$(LIBPSR)(plot.o) $(LIBPSR)(preces.o) $(LIBPSR)(psrcmd.o) \
$(LIBPSR)(setpar.o) $(LIBPSR)(shellx.o) $(LIBPSR)(shoprt.o) \
$(LIBPSR)(shousr.o) $(LIBPSR)(dsvdfit.o) $(LIBPSR)(timing.o)  \
$(LIBPSR)(uppcase.o) $(LIBPSR)(utctai.o)  $(LIBPSR)(shwcmd.o) $(LIBPSR)(rtetxt.o)\
$(LIBPSR)(roundn.o) $(LIBPSR)(neweph2old.o) $(LIBPSR)(psreph.o) \
$(LIBPSR)(usrler.o) $(LIBPSR)(giveos.o) $(LIBPSR)(ipagflt.o) \
$(LIBPSR)(pardat.o) $(LIBPSR)(outptn.o) $(LIBPSR)(sholst.o) $(LIBPSR)(scnpar.o)\
$(LIBPSR)(ctimes.o)  $(LIBPSR)(grpcmd.o) $(LIBPSR)(symopn.o) $(LIBPSR)(comstr2.o)\
$(LIBPSR)(pararg.o) $(LIBPSR)(fndpar.o) $(LIBPSR)(shoopt.o) $(LIBPSR)(fromto.o)\
$(LIBPSR)(pareal.o) $(LIBPSR)(cparam.o)  $(LIBPSR)(parswt.o) $(LIBPSR)(getedt.o) \
$(LIBPSR)(chkdmy.o) $(LIBPSR)(dround.o) $(LIBPSR)(pardbl.o) $(LIBPSR)(getwave.o)\
$(LIBPSR)(chkymd.o) $(LIBPSR)(shopar.o) $(LIBPSR)(shocmd.o) \
$(LIBPSR)(getcmd_sun.o) $(LIBPSR)(rdwlth.o) $(LIBPSR)(isthms.o) \
$(LIBPSR)(setcmd.o) $(LIBPSR)(cswitch.o) $(LIBPSR)(upcase.o)\
$(LIBPSR)(outlin.o) $(LIBPSR)(upcase_works.o)  $(LIBPSR)(olhelp.o) $(LIBPSR)(oltime.o) \
$(LIBPSR)(maxint.o) $(LIBPSR)(psrgetdir.o) $(LIBPSR)(psr_screen.o)  \
$(LIBPSR)(oceset.o) $(LIBPSR)(reamax.o) $(LIBPSR)(dblmax.o) $(LIBPSR)(termnl.o) \
$(LIBPSR)(dblmin.o) $(LIBPSR)(edit.o)  $(LIBPSR)(opscli.o) \
$(LIBPSR)(filtyp.o) $(LIBPSR)(terminl.o) $(LIBPSR)(hash.o) $(LIBPSR)(batch.o) \
$(LIBPSR)(title.o)  $(LIBPSR)(getlun.o) $(LIBPSR)(opnfil.o) $(LIBPSR)(user.o)\
$(LIBPSR)(overprint.o) $(LIBPSR)(prifil.o) $(LIBPSR)(prmtcc.o) \
$(LIBPSR)(cputime.o) $(LIBPSR)(incuni.o) $(LIBPSR)(oschar.o) $(LIBPSR)(print.o) \
$(LIBPSR)(elpsec.o) $(LIBPSR)(opscmd.o) $(LIBPSR)(cpusec.o) $(LIBPSR)(chkfil.o)\
$(LIBPSR)(unsafe.o)  $(LIBPSR)(oabort.o) $(LIBPSR)(oldate.o) $(LIBPSR)(cfiltyp.o) \
$(LIBPSR)(olaf_ctrlc_hr.o) $(LIBPSR)(olaf_read_c.o) $(LIBPSR)(olaf_write_c.o)  \
$(LIBPSR)(olaf_help_bck.o) $(LIBPSR)(olaf_help_fsu.o) $(LIBPSR)(olaf_help_prl.o) \
$(LIBPSR)(olaf_help_chk.o) $(LIBPSR)(olaf_help_get.o) $(LIBPSR)(olaf_help_pro.o) \
$(LIBPSR)(olaf_help_fin.o) $(LIBPSR)(olaf_help_no.o)  $(LIBPSR)(olaf_help_prt.o) \
$(LIBPSR)(olaf_help_opn.o) $(LIBPSR)(new_ol_rdlin.o) $(LIBPSR)(olaf_help_spl.o)\
$(LIBPSR)(key_word.o) $(LIBPSR)(readeph.o) $(LIBPSR)(writeph.o) \
$(LIBPSR)(mesage.o) $(LIBPSR)(maxfil.o)  $(LIBPSR)(readop.o) $(LIBPSR)(jplepv.o)

OLAFFIL=\
$(LIBPSR)(casprs.o) $(LIBPSR)(intdcm.o) $(LIBPSR)(pagest.o) $(LIBPSR)(shoobe.o) \
$(LIBPSR)(symfsy.o) $(LIBPSR)(olexpl.o) \
$(LIBPSR)(chint.o)  $(LIBPSR)(isize.o)  $(LIBPSR)(prmsep.o) $(LIBPSR)(shosav.o) \
$(LIBPSR)(symini.o) \
$(LIBPSR)(cmdsye.o) $(LIBPSR)(iutsec.o) $(LIBPSR)(round.o)  $(LIBPSR)(shosym.o) \
$(LIBPSR)(symprt.o) \
$(LIBPSR)(liberr.o) \
$(LIBPSR)(eolchr.o) $(LIBPSR)(litdel.o) $(LIBPSR)(rsizen.o) $(LIBPSR)(spechr.o) \
$(LIBPSR)(symsub.o) \
$(LIBPSR)(litprs.o) $(LIBPSR)(savfil.o) $(LIBPSR)(supchr.o) \
$(LIBPSR)(symtrn.o) \
$(LIBPSR)(getidc.o) $(LIBPSR)(locase.o) $(LIBPSR)(savwrt.o) $(LIBPSR)(symadl.o) \
$(LIBPSR)(getlin.o) $(LIBPSR)(lparse.o) $(LIBPSR)(setifm.o) $(LIBPSR)(symavl.o) \
$(LIBPSR)(idsts.o)  $(LIBPSR)(nupdel.o) $(LIBPSR)(setlin.o) $(LIBPSR)(symcmd.o) \
$(LIBPSR)(idyz.o)  $(LIBPSR)(opstat.o) $(LIBPSR)(setrfm.o) $(LIBPSR)(symcom.o) \
$(LIBPSR)(oldeph2new.o)  $(LIBPSR)(sec2posn.o) 

OLAFOBJ=$(OLAFFIL)

$(LIBPSR): $(LIBPSROBJECTS) $(OLAFOBJ) 
	cp $(LIBPSR) $(LIBDIR)
#	rm -f $(LIBDIR)/libpsr.a
#	ln -s $(PWD)/$(LIBPSR) $(LIBDIR)/libpsr.a

.PRECIOUS : $(LIBPSR)

