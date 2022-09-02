######################################################################
DEFINES= -DBEO -DUNIX -DFLUSHFILE -DPARALLEL -DSLEEP
C=gcc
CFLAGS=-c
F90=gfortran
LD=gfortran
FFLAGS=-c -O3 -static
LDFLAGS=-static
######################################################################


######################################################################
# DON'T EDIT BELOW THIS LINE
######################################################################

.SUFFIXES:
.SUFFIXES:	.o .for .F

all :	beopest


.F.for:
	./cppp $(DEFINES) $< $*.for


ip.o :	ip.c
	$(C) $(CFLAGS) ip.c


common_mod.o :	common_mod.for
	$(F90) $(FFLAGS) common_mod.for

beorun.o :	beorun.for pestdata.o common_mod.o
	$(F90) $(FFLAGS) beorun.for


beopest.o :	beopest.for pardef.o pestdata.o
	$(F90) $(FFLAGS) beopest.for



modrun.o:	modrun.for common_mod.o
	$(F90) $(FFLAGS) modrun.for


pardef.o:	pardef.for pestdata.o common_mod.o
	$(F90) $(FFLAGS) pardef.for


linpos.o:	linpos.for
	$(F90) $(FFLAGS) linpos.for


lsqr.o:	lsqr.for
	$(F90) $(FFLAGS) lsqr.for


orthog.o:	orthog.for common_mod.o
	$(F90) $(FFLAGS) orthog.for


lapack1.o:	lapack1.for
	$(F90) $(FFLAGS) lapack1.for


pest.o:	pest.for pestdata.o beopest.o pardef.o
	$(F90) $(FFLAGS) pest.for


runpest.o:	runpest.for pestdata.o common_mod.o
	$(F90) $(FFLAGS) runpest.for


pestsub1.o:	pestsub1.for
	$(F90) $(FFLAGS) pestsub1.for


pestsub2.o:	pestsub2.for pestdata.o common_mod.o
	$(F90) $(FFLAGS) pestsub2.for


readpest.o:	readpest.for pestdata.o
	$(F90) $(FFLAGS) readpest.for


dercalc.o:	dercalc.for common_mod.o
	$(F90) $(FFLAGS) dercalc.for


writall.o:	writall.for pestdata.o common_mod.o
	$(F90) $(FFLAGS) writall.for


writsig.o:	writsig.for
	$(F90) $(FFLAGS) writsig.for


common.o:	common.for
	$(F90) $(FFLAGS) common.for


pgetcl.o:	pgetcl.for
	$(F90) $(FFLAGS) pgetcl.for


nblnk.o:	nblnk.for
	$(F90) $(FFLAGS) nblnk.for


numdays.o:	numdays.for
	$(F90) $(FFLAGS) numdays.for


pestwait.o:	pestwait.for
	$(F90) $(FFLAGS) pestwait.for


space.o:	space.for
	$(F90) $(FFLAGS) space.for

writint.o:	writint.for
	$(F90) $(FFLAGS) writint.for


parpest.o:	parpest.for common_mod.o
	$(F90) $(FFLAGS) parpest.for


sstop.o:	sstop.for common_mod.o
	$(F90) $(FFLAGS) sstop.for


drealrd.o:	drealrd.for
	$(F90) $(FFLAGS) drealrd.for


optwt.o:	optwt.for common_mod.o
	$(F90) $(FFLAGS) optwt.for


compress.o:	compress.for
	$(F90) $(FFLAGS) compress.for


ms_stubs.o:	ms_stubs.for
	$(F90) $(FFLAGS) ms_stubs.for


pestdata.o:	pestdata.for
	$(F90) $(FFLAGS) pestdata.for


beopest:	pest.o pestsub1.o pestsub2.o dercalc.o modrun.o writall.o \
                linpos.o lapack1.o writsig.o common.o pgetcl.o parpest.o numdays.o \
                writint.o drealrd.o pestwait.o \
                space.o optwt.o compress.o pardef.o ms_stubs.o \
                readpest.o runpest.o lsqr.o orthog.o pestdata.o beopest.o beorun.o ip.o
	$(LD) $(LDFLAGS) -o beopest pest.o pestsub1.o pestsub2.o dercalc.o modrun.o writall.o \
                linpos.o lapack1.o writsig.o common.o pgetcl.o parpest.o numdays.o \
                writint.o drealrd.o pestwait.o \
                space.o optwt.o compress.o pardef.o ms_stubs.o \
                readpest.o runpest.o lsqr.o orthog.o pestdata.o beopest.o beorun.o ip.o


#EOF
