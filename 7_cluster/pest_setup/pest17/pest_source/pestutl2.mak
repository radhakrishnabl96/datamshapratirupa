######################################################################
DEFINES= -DUNIX -DFLUSHFILE -DSLEEP
F90=gfortran
LD=gfortran
FFLAGS=-c -O3 -static -ffree-form
FFLAGS1=-c -O3 -static
LDFLAGS=-static
######################################################################


######################################################################
# DON'T EDIT BELOW THIS LINE
######################################################################

all :	parcalc obscalc


parcalc.for :	parcalc.F90
	./cppp $(DEFINES) parcalc.F90 parcalc.for


parcalc.o :	parcalc.for inter.inc utility.inc modio.inc
	$(F90) $(FFLAGS) parcalc.for


parcalc :	parcalc.o pgetcl.o
	$(LD) $(LDFLAGS) -o parcalc parcalc.o pgetcl.o



obscalc.for :	obscalc.F90
	./cppp $(DEFINES) obscalc.F90 obscalc.for


obscalc.o :	obscalc.for inter.inc utility.inc modio.inc
	$(F90) $(FFLAGS) obscalc.for


obscalc :	obscalc.o pgetcl.o
	$(LD) $(LDFLAGS) -o obscalc obscalc.o pgetcl.o



pgetcl.o :	pgetcl.for
	$(F90) $(FFLAGS1) pgetcl.for

pgetcl.for :	pgetcl.F
	./cppp $(DEFINES) pgetcl.F pgetcl.for
