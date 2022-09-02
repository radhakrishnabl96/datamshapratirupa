######################################################################
DEFINES= -DUNIX -DFLUSHFILE -DPARALLEL -DSLEEP
F90=gfortran
LD=gfortran
FFLAGS=-c -O3 -static
LDFLAGS=-static
######################################################################

######################################################################
# DON'T EDIT BELOW THIS LINE
######################################################################

all :	ppest

.SUFFIXES: .F .for
.F.for :
	./cppp $(DEFINES) $< $*.for



cgsolve.o :	cgsolve.for
	$(F90) $(FFLAGS) cgsolve.for


common.o :	common.for
	$(F90) $(FFLAGS) common.for


compress.o :	compress.for
	$(F90) $(FFLAGS) compress.for


dercalc.o :	dercalc.for
	$(F90) $(FFLAGS) dercalc.for


drealrd.o :	drealrd.for
	$(F90) $(FFLAGS) drealrd.for


lapack1.o :	lapack1.for
	$(F90) $(FFLAGS) lapack1.for


linpos.o :	linpos.for
	$(F90) $(FFLAGS) linpos.for


modrun.o :	modrun.for
	$(F90) $(FFLAGS) modrun.for


ms_stubs.o :	ms_stubs.for
	$(F90) $(FFLAGS) ms_stubs.for


numdays.o :	numdays.for
	$(F90) $(FFLAGS) numdays.for


optwt.o :	optwt.for
	$(F90) $(FFLAGS) optwt.for


pardef.o :	pardef.for PESTDATA.mod
	$(F90) $(FFLAGS) pardef.for


parpest.o :	parpest.for
	$(F90) $(FFLAGS) parpest.for


pest.o :	pest.for PESTDATA.mod
	$(F90) $(FFLAGS) pest.for


PESTDATA.mod :	pestdata.for
	$(F90) $(FFLAGS) pestdata.for


pestsub1.o :	pestsub1.for
	$(F90) $(FFLAGS) pestsub1.for


pestsub2.o :	pestsub2.for PESTDATA.mod
	$(F90) $(FFLAGS) pestsub2.for


pgetcl.o :	pgetcl.for
	$(F90) $(FFLAGS) pgetcl.for


readpest.o :	readpest.for PESTDATA.mod
	$(F90) $(FFLAGS) readpest.for


runpest.o :	runpest.for PESTDATA.mod
	$(F90) $(FFLAGS) runpest.for


space.o :	space.for
	$(F90) $(FFLAGS) space.for


pestwait.o :	pestwait.for
	$(F90) $(FFLAGS) pestwait.for


writall.o :	writall.for PESTDATA.mod
	$(F90) $(FFLAGS) writall.for


writint.o :	writint.for
	$(F90) $(FFLAGS) writint.for


writsig.o :	writsig.for
	$(F90) $(FFLAGS) writsig.for


lsqr.o :	lsqr.for
	$(F90) $(FFLAGS) lsqr.for


orthog.o :	orthog.for
	$(F90) $(FFLAGS) orthog.for




ppest :	pest.o pestsub1.o pestsub2.o dercalc.o modrun.o writall.o \
		linpos.o lapack1.o writsig.o common.o \
		pgetcl.o parpest.o numdays.o \
                writint.o drealrd.o pestwait.o \
                space.o optwt.o cgsolve.o compress.o pardef.o \
                readpest.o runpest.o lsqr.o orthog.o ms_stubs.o PESTDATA.mod
	$(LD) $(LDFLAGS) -o ppest  \
                pest.o pestsub1.o pestsub2.o dercalc.o modrun.o writall.o \
		linpos.o lapack1.o writsig.o common.o \
		pgetcl.o parpest.o numdays.o \
                writint.o drealrd.o pestwait.o \
                space.o optwt.o cgsolve.o compress.o pardef.o \
                readpest.o runpest.o lsqr.o orthog.o ms_stubs.o pestdata.o
