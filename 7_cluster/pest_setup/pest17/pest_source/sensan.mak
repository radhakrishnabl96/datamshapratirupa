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


all :	sensan senschek


.SUFFIXES: .F .for
.F.for :
	./cppp $(DEFINES) $< $*.for

######################################################################


nblnk.o :	nblnk.for
	$(F90) $(FFLAGS) nblnk.for


pgetcl.o :	pgetcl.for
	$(F90) $(FFLAGS) pgetcl.for


scheksub.o :	scheksub.for
	$(F90) $(FFLAGS) scheksub.for


sensan.o :	sensan.for
	$(F90) $(FFLAGS) sensan.for


senschek.o :	senschek.for
	$(F90) $(FFLAGS) senschek.for


sensub.o :	sensub.for
	$(F90) $(FFLAGS) sensub.for


space.o :	space.for
	$(F90) $(FFLAGS) space.for


######################################################################


sensan :	sensan.o sensub.o nblnk.o pgetcl.o space.o
	$(LD) $(LDFLAGS) -o sensan sensan.o sensub.o nblnk.o \
              pgetcl.o space.o


senschek :	senschek.o nblnk.o pgetcl.o scheksub.o space.o
	$(LD) $(LDFLAGS) -o senschek senschek.o nblnk.o pgetcl.o \
	scheksub.o space.o

