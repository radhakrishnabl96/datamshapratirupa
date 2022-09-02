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

all :	csv2par csv2jcb


csv2par :	csv2par.o
	$(LD) $(LDFLAGS) -o csv2par csv2par.o

csv2par.o :	csv2par.f90
	$(F90) $(FFLAGS) csv2par.f90

csv2par.f90 :	csv2par.F
	./cppp $(DEFINES) csv2par.F csv2par.f90


csv2jcb :	csv2jcb.o
	$(LD) $(LDFLAGS) -o csv2jcb csv2jcb.o

csv2jcb.o :	csv2jcb.f90
	$(F90) $(FFLAGS) csv2jcb.f90

csv2jcb.f90 :	csv2jcb.F
	./cppp $(DEFINES) csv2jcb.F csv2jcb.f90

