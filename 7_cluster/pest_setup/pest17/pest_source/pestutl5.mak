######################################################################
DEFINES= -DUNIX -DFLUSHFILE -DSLEEP
F90=gfortran
LD=gfortran
FFLAGS1=-c -O3 -static -ffree-form
FFLAGS=-c -O3 -static
LDFLAGS=-static
######################################################################

######################################################################
# DON'T EDIT BELOW THIS LINE
######################################################################

all :	cmaes_p sceua_p jactest rdmulres obs2obs

.SUFFIXES: .F .for
.F.for :
	./cppp $(DEFINES) $< $*.for



cmaes_p :	cmaes_p.o readpest.o pgetcl.o slapack.o space.o \
                common.o pestsub1.o writint.o linpos.o compress.o \
                certread.o mio.o prm.o cmaes.o ms_stubs.o pestdata.o
	$(LD) $(LDFLAGS) -o cmaes_p cmaes_p.o readpest.o pgetcl.o slapack.o space.o \
	                 common.o pestsub1.o writint.o linpos.o compress.o \
	                 certread.o mio.o prm.o cmaes.o ms_stubs.o pestdata.o

cmaes_p.o :	cmaes_p.for model_input_output_interface.mod  parallel_run_manager.mod \
                PESTDATA.mod cmaes_module.mod
	$(F90) $(FFLAGS) cmaes_p.for


sceua_p :	sceua_p.o readpest.o pgetcl.o space.o \
                common.o pestsub1.o writint.o linpos.o compress.o \
                mio.o prm.o sceua.o ms_stubs.o pestdata.o
	$(LD) $(LDFLAGS) -o sceua_p sceua_p.o readpest.o pgetcl.o space.o \
	                 common.o pestsub1.o writint.o linpos.o compress.o \
	                 mio.o prm.o sceua.o ms_stubs.o pestdata.o

sceua_p.o :	sceua_p.for model_input_output_interface.mod  parallel_run_manager.mod \
                PESTDATA.mod sceua_module.mod
	$(F90) $(FFLAGS) sceua_p.for


jactest	:	jactest.o readpest.o pgetcl.o space.o \
                common.o pestsub1.o writint.o compress.o
	$(LD) $(LDFLAGS) -o jactest jactest.o readpest.o pgetcl.o space.o common.o \
	                 pestsub1.o writint.o compress.o pestdata.o mio.o prm.o

jactest.o :	jactest.for model_input_output_interface.mod  parallel_run_manager.mod PESTDATA.mod
	$(F90) $(FFLAGS) jactest.for


rdmulres	:	rdmulres.o pgetcl.o space.o pestsub1.o writint.o
	$(LD) $(LDFLAGS) -o rdmulres rdmulres.o pgetcl.o space.o pestsub1.o writint.o mio.o

rdmulres.o :	rdmulres.for model_input_output_interface.mod
	$(F90) $(FFLAGS) rdmulres.for


obs2obs	:	obs2obs.o pgetcl.o space.o writint.o
	$(LD) $(LDFLAGS) -o obs2obs obs2obs.o pgetcl.o space.o writint.o mio.o eqn.o

obs2obs.o :	obs2obs.for model_input_output_interface.mod equation.mod
	$(F90) $(FFLAGS) obs2obs.for




certread.o:	certread.for
	$(F90) $(FFLAGS) certread.for


cmaes_module.mod :	cmaes.o


cmaes.o :	cmaes.for
	$(F90) $(FFLAGS1) cmaes.for

cmaes.for :	cmaes.F90
	./cppp $(DEFINES) cmaes.F90 cmaes.for


sceua_module.mod :	sceua.o


sceua.o :	sceua.for
	$(F90) $(FFLAGS) sceua.for


common.o:	common.for
	$(F90) $(FFLAGS) common.for


compress.o:	compress.for
	$(F90) $(FFLAGS) compress.for


linpos.o :	linpos.for
	$(F90) $(FFLAGS) linpos.for


model_input_output_interface.mod :	mio.o


mio.o :	mio.for
	$(F90) $(FFLAGS1) mio.for

mio.for :	mio.F90
	./cppp $(DEFINES) mio.F90 mio.for


equation.mod :	eqn.o


eqn.o :	eqn.for
	$(F90) $(FFLAGS1) eqn.for

eqn.for :	eqn.F90
	./cppp $(DEFINES) eqn.F90 eqn.for


parallel_run_manager.mod :	prm.o


prm.o :	prm.for
	$(F90) $(FFLAGS1) prm.for

prm.for :	prm.F90
	./cppp $(DEFINES) prm.F90 prm.for


PESTDATA.mod:	pestdata.for
	$(F90) $(FFLAGS) pestdata.for


pestsub1.o:	pestsub1.for
	$(F90) $(FFLAGS) pestsub1.for


space.o :	space.for
	$(F90) $(FFLAGS) space.for


pgetcl.o :	pgetcl.for
	$(F90) $(FFLAGS) pgetcl.for


readpest.o :	readpest.for PESTDATA.mod
	$(F90) $(FFLAGS) readpest.for


slapack.o :	slapack.for
	$(F90) $(FFLAGS) slapack.for


writint.o:	writint.for
	$(F90) $(FFLAGS) writint.for


ms_stubs.o:	ms_stubs.for
	$(F90) $(FFLAGS) ms_stubs.for
