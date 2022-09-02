######################################################################
DEFINES= -DUNIX -DFLUSHFILE -DSLEEP
F90=gfortran
LD=gfortran
FFLAGS=-c -O3 -static
LDFLAGS=-static
######################################################################

######################################################################
# DON'T EDIT BELOW THIS LINE
######################################################################

all :	pest predvar1 predvar1a predvar1b predvar1c parvar1 predvar2 predvar3 \
        predvar4 predvar5 predunc1 predunc4 predunc5 predunc6 predunc7 infstat \
        infstat1 calmaintain rrfcalcpsi muljcosen wtsenout pnulpar identpar \
        supcalc ssstat simcase obscomp rrf2jco rrfcat rrfcull parobs2rrf rrf2parobs \
        rrfappend rrf2tab rrf2csv rrf2par rrfclean addcovmat pstclean jcoblank jcowt jcomix

.SUFFIXES: .F .for
.F.for :
	./cppp $(DEFINES) $< $*.for


cgsolve.o:	cgsolve.for
	$(F90) $(FFLAGS) cgsolve.for


common.o:	common.for
	$(F90) $(FFLAGS) common.for


compress.o:	compress.for
	$(F90) $(FFLAGS) compress.for


dercalc.o:	dercalc.for
	$(F90) $(FFLAGS) dercalc.for


drealrd.o:	drealrd.for
	$(F90) $(FFLAGS) drealrd.for


lapack1.o:	lapack1.for
	$(F90) $(FFLAGS) lapack1.for


linpos.o:	linpos.for
	$(F90) $(FFLAGS) linpos.for


modrun.o:	modrun.for
	$(F90) $(FFLAGS) modrun.for


ms_stubs.o:	ms_stubs.for
	$(F90) $(FFLAGS) ms_stubs.for


optwt.o:	optwt.for
	$(F90) $(FFLAGS) optwt.for


pardef.o:	pardef.for PESTDATA.mod
	$(F90) $(FFLAGS) pardef.for


pest.o:	pest.for PESTDATA.mod
	$(F90) $(FFLAGS) pest.for


PESTDATA.mod:	pestdata.for
	$(F90) $(FFLAGS) pestdata.for


pestsub1.o:	pestsub1.for
	$(F90) $(FFLAGS) pestsub1.for


pestsub2.o:	pestsub2.for  PESTDATA.mod
	$(F90) $(FFLAGS) pestsub2.for


pgetcl.o:	pgetcl.for
	$(F90) $(FFLAGS) pgetcl.for


readpest.o:	readpest.for PESTDATA.mod
	$(F90) $(FFLAGS) readpest.for


runpest.o:	runpest.for PESTDATA.mod
	$(F90) $(FFLAGS) runpest.for


space.o:	space.for
	$(F90) $(FFLAGS) space.for


pestwait.o:	pestwait.for
	$(F90) $(FFLAGS) pestwait.for


writall.o:	writall.for PESTDATA.mod
	$(F90) $(FFLAGS) writall.for


writint.o:	writint.for
	$(F90) $(FFLAGS) writint.for


writsig.o:	writsig.for
	$(F90) $(FFLAGS) writsig.for


predvar1.o:	predvar1.for PESTDATA.mod
	$(F90) $(FFLAGS) predvar1.for


predvar1a.o:	predvar1a.for PESTDATA.mod
	$(F90) $(FFLAGS) predvar1a.for


predvar1b.o:	predvar1b.for PESTDATA.mod
	$(F90) $(FFLAGS) predvar1b.for


predvar1c.o:	predvar1c.for PESTDATA.mod
	$(F90) $(FFLAGS) predvar1c.for


parvar1.o:	parvar1.for PESTDATA.mod
	$(F90) $(FFLAGS) parvar1.for


predvar2.o:	predvar2.for PESTDATA.mod
	$(F90) $(FFLAGS) predvar2.for


predvar3.o:	predvar3.for PESTDATA.mod
	$(F90) $(FFLAGS) predvar3.for


predvar4.o:	predvar4.for PESTDATA.mod
	$(F90) $(FFLAGS) predvar4.for


predvar5.o:	predvar5.for PESTDATA.mod
	$(F90) $(FFLAGS) predvar5.for


predunc1.o:	predunc1.for PESTDATA.mod
	$(F90) $(FFLAGS) predunc1.for


predunc4.o:	predunc4.for PESTDATA.mod
	$(F90) $(FFLAGS) predunc4.for


predunc5.o:	predunc5.for PESTDATA.mod
	$(F90) $(FFLAGS) predunc5.for


predunc6.o:	predunc6.for PESTDATA.mod
	$(F90) $(FFLAGS) predunc6.for


predunc7.o:	predunc7.for PESTDATA.mod
	$(F90) $(FFLAGS) predunc7.for


infstat.o:	infstat.for PESTDATA.mod
	$(F90) $(FFLAGS) infstat.for


infstat1.o:	infstat1.for PESTDATA.mod
	$(F90) $(FFLAGS) infstat1.for


calmaintain.o:	calmaintain.for PESTDATA.mod
	$(F90) $(FFLAGS) calmaintain.for


rrfcalcpsi.o:	rrfcalcpsi.for PESTDATA.mod
	$(F90) $(FFLAGS) rrfcalcpsi.for


wtsenout.o:	wtsenout.for PESTDATA.mod
	$(F90) $(FFLAGS) wtsenout.for


pnulpar.o:	pnulpar.for PESTDATA.mod
	$(F90) $(FFLAGS) pnulpar.for


muljcosen.o:	muljcosen.for PESTDATA.mod
	$(F90) $(FFLAGS) muljcosen.for


identpar.o:	identpar.for PESTDATA.mod
	$(F90) $(FFLAGS) identpar.for


supcalc.o:	supcalc.for PESTDATA.mod
	$(F90) $(FFLAGS) supcalc.for


ssstat.o:	ssstat.for PESTDATA.mod
	$(F90) $(FFLAGS) ssstat.for


simcase.o:	simcase.for PESTDATA.mod
	$(F90) $(FFLAGS) simcase.for


obscomp.o:	obscomp.for PESTDATA.mod
	$(F90) $(FFLAGS) obscomp.for


rrf2jco.o:	rrf2jco.for PESTDATA.mod
	$(F90) $(FFLAGS) rrf2jco.for


rrfcat.o:	rrfcat.for
	$(F90) $(FFLAGS) rrfcat.for


rrfcull.o:	rrfcull.for
	$(F90) $(FFLAGS) rrfcull.for


parobs2rrf.o:	parobs2rrf.for
	$(F90) $(FFLAGS) parobs2rrf.for


rrf2parobs.o:	rrf2parobs.for
	$(F90) $(FFLAGS) rrf2parobs.for


rrfappend.o:	rrfappend.for
	$(F90) $(FFLAGS) rrfappend.for


rrf2tab.o:	rrf2tab.for
	$(F90) $(FFLAGS) rrf2tab.for

rrf2csv.o:	rrf2csv.for
	$(F90) $(FFLAGS) rrf2csv.for

rrf2par.o:	rrf2par.for
	$(F90) $(FFLAGS) rrf2par.for


rrfclean.o:	rrfclean.for
	$(F90) $(FFLAGS) rrfclean.for


addcovmat.o:	addcovmat.for
	$(F90) $(FFLAGS) addcovmat.for


pstclean.o:	pstclean.for
	$(F90) $(FFLAGS) pstclean.for


jcoblank.o:	jcoblank.for PESTDATA.mod
	$(F90) $(FFLAGS) jcoblank.for


jcowt.o:	jcowt.for PESTDATA.mod
	$(F90) $(FFLAGS) jcowt.for


jcomix.o:	jcomix.for PESTDATA.mod
	$(F90) $(FFLAGS) jcomix.for


##########################################################################

openun.o:	openun.for
	$(F90) $(FFLAGS) openun.for


certfile.o :	certfile.for
	$(F90) $(FFLAGS) certfile.for


lsqr.o :	lsqr.for
	$(F90) $(FFLAGS) lsqr.for


lsqr_orig.o :	lsqr_orig.for
	$(F90) $(FFLAGS) lsqr_orig.for


orthog.o :	orthog.for
	$(F90) $(FFLAGS) orthog.for



##################################################################################

pest :	pest.o pestsub1.o pestsub2.o dercalc.o modrun.o writall.o \
		linpos.o lapack1.o writsig.o common.o \
		pgetcl.o pestwait.o writint.o pardef.o\
                drealrd.o space.o optwt.o cgsolve.o compress.o \
                readpest.o runpest.o lsqr.o orthog.o ms_stubs.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o pest \
                pest.o pestsub1.o pestsub2.o dercalc.o modrun.o writall.o \
		linpos.o lapack1.o writsig.o common.o \
		pgetcl.o pestwait.o writint.o pardef.o\
                drealrd.o space.o optwt.o cgsolve.o compress.o \
                readpest.o runpest.o lsqr.o orthog.o ms_stubs.o pestdata.o

predvar1 :	predvar1.o readpest.o lapack1.o openun.o space.o compress.o \
	        certfile.o linpos.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o predvar1 predvar1.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o pestdata.o linpos.o


predvar1a :	predvar1a.o readpest.o lapack1.o openun.o space.o compress.o \
	        certfile.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o predvar1a predvar1a.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o pestdata.o


predvar1b :	predvar1b.o readpest.o lapack1.o openun.o space.o compress.o \
	        certfile.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o predvar1b predvar1b.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o pestdata.o


predvar1c :	predvar1c.o readpest.o lapack1.o openun.o space.o compress.o \
	        certfile.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o predvar1c predvar1c.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o pestdata.o


parvar1 :	parvar1.o readpest.o lapack1.o openun.o space.o compress.o \
	        certfile.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o parvar1 parvar1.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o pestdata.o


predvar2 :	predvar2.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o linpos.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o predvar2 predvar2.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o pestdata.o linpos.o


predvar3 :	predvar3.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o linpos.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o predvar3 predvar3.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o pestdata.o linpos.o


predvar4 :	predvar4.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o linpos.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o predvar4 predvar4.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o pestdata.o linpos.o


predvar5 :	predvar5.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o linpos.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o predvar5 predvar5.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o pestdata.o linpos.o


predunc1 :	predunc1.o readpest.o \
                openun.o space.o compress.o certfile.o linpos.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o predunc1 predunc1.o readpest.o \
                openun.o space.o compress.o certfile.o pestdata.o linpos.o


predunc4 :	predunc4.o readpest.o \
                openun.o space.o compress.o certfile.o linpos.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o predunc4 predunc4.o readpest.o \
                openun.o space.o compress.o certfile.o pestdata.o linpos.o


predunc5 :	predunc5.o readpest.o \
                openun.o space.o compress.o certfile.o linpos.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o predunc5 predunc5.o readpest.o \
                openun.o space.o compress.o certfile.o pestdata.o linpos.o


predunc6 :	predunc6.o readpest.o \
                openun.o space.o compress.o certfile.o linpos.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o predunc6 predunc6.o readpest.o \
                openun.o space.o compress.o certfile.o pestdata.o linpos.o


predunc7 :	predunc7.o readpest.o \
                openun.o space.o compress.o certfile.o linpos.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o predunc7 predunc7.o readpest.o \
                openun.o space.o compress.o certfile.o pestdata.o linpos.o


infstat :	infstat.o readpest.o lapack1.o linpos.o \
	        openun.o space.o compress.o pgetcl.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o infstat infstat.o readpest.o lapack1.o linpos.o \
                openun.o space.o compress.o pgetcl.o pestdata.o


infstat1 :	infstat1.o readpest.o lapack1.o linpos.o \
	        openun.o space.o compress.o pgetcl.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o infstat1 infstat1.o readpest.o lapack1.o linpos.o \
                openun.o space.o compress.o pgetcl.o pestdata.o


calmaintain :	calmaintain.o readpest.o lapack1.o lsqr_orig.o \
	        openun.o space.o compress.o pgetcl.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o calmaintain calmaintain.o readpest.o lapack1.o lsqr_orig.o \
                openun.o space.o compress.o pgetcl.o pestdata.o


rrfcalcpsi :	rrfcalcpsi.o readpest.o lapack1.o pestsub2.o modrun.o \
	        openun.o space.o compress.o pgetcl.o pestwait.o \
	        writsig.o common.o drealrd.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o rrfcalcpsi rrfcalcpsi.o readpest.o lapack1.o pestsub2.o modrun.o \
                openun.o space.o compress.o pgetcl.o pestwait.o \
                writsig.o common.o drealrd.o pestdata.o


muljcosen :	muljcosen.o readpest.o lapack1.o linpos.o \
	        openun.o space.o compress.o pgetcl.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o muljcosen muljcosen.o readpest.o lapack1.o linpos.o \
                openun.o space.o compress.o pgetcl.o pestdata.o


wtsenout :	wtsenout.o readpest.o lapack1.o \
		openun.o space.o compress.o  pgetcl.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o wtsenout wtsenout.o readpest.o lapack1.o \
		openun.o space.o compress.o pgetcl.o pestdata.o


pnulpar :	pnulpar.o readpest.o lapack1.o \
                openun.o space.o compress.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o pnulpar pnulpar.o readpest.o lapack1.o \
                openun.o space.o compress.o pestdata.o


identpar :	identpar.o readpest.o lapack1.o openun.o space.o compress.o \
	        linpos.o pgetcl.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o identpar identpar.o readpest.o lapack1.o \
                openun.o space.o compress.o pgetcl.o pestdata.o linpos.o


supcalc :	supcalc.o readpest.o lapack1.o openun.o space.o compress.o \
	        linpos.o certfile.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o supcalc supcalc.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o pestdata.o linpos.o


ssstat :	ssstat.o readpest.o lapack1.o openun.o space.o compress.o \
	        linpos.o certfile.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o ssstat ssstat.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o pestdata.o linpos.o


simcase :	simcase.o readpest.o openun.o space.o compress.o pgetcl.o \
	        PESTDATA.mod
	$(LD) $(LDFLAGS) -o simcase simcase.o readpest.o pgetcl.o \
                openun.o space.o compress.o pestdata.o


obscomp :	obscomp.o readpest.o openun.o space.o compress.o pgetcl.o \
	        PESTDATA.mod
	$(LD) $(LDFLAGS) -o obscomp obscomp.o readpest.o pgetcl.o \
                openun.o space.o compress.o pestdata.o


rrf2jco :	rrf2jco.o readpest.o lapack1.o openun.o space.o compress.o \
	        certfile.o pgetcl.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o rrf2jco rrf2jco.o readpest.o lapack1.o \
                openun.o space.o compress.o certfile.o pgetcl.o pestdata.o


rrfcat :	rrfcat.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o rrfcat rrfcat.o space.o pgetcl.o


rrfcull :	rrfcull.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o rrfcull rrfcull.o space.o pgetcl.o


parobs2rrf :	parobs2rrf.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o parobs2rrf parobs2rrf.o space.o pgetcl.o


rrf2parobs :	rrf2parobs.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o rrf2parobs rrf2parobs.o space.o pgetcl.o


rrfappend :	rrfappend.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o rrfappend rrfappend.o space.o pgetcl.o


rrf2tab :	rrf2tab.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o rrf2tab rrf2tab.o space.o pgetcl.o


rrf2csv :	rrf2csv.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o rrf2csv rrf2csv.o space.o pgetcl.o


rrf2par :	rrf2par.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o rrf2par rrf2par.o space.o pgetcl.o


rrfclean :	rrfclean.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o rrfclean rrfclean.o space.o pgetcl.o


addcovmat :	addcovmat.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o addcovmat addcovmat.o space.o pgetcl.o


pstclean :	pstclean.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o pstclean pstclean.o space.o pgetcl.o


jcoblank :	jcoblank.o readpest.o openun.o space.o compress.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o jcoblank jcoblank.o readpest.o openun.o space.o compress.o pestdata.o


jcowt :	jcowt.o readpest.o openun.o space.o compress.o pgetcl.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o jcowt jcowt.o readpest.o openun.o space.o compress.o pgetcl.o pestdata.o


jcomix :	jcomix.o readpest.o openun.o space.o compress.o pgetcl.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o jcomix jcomix.o readpest.o openun.o space.o compress.o pgetcl.o pestdata.o
