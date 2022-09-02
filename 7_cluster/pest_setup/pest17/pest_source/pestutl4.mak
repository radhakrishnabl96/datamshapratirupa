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

all :	cov2cor covcond mat2jco mat2srf matadd matcolex matdiag matdiff \
        matinvp matjoinc matjoind matjoinr matorder matprod matquad matrow \
        matsmul matspec matsvd matsym mattrans matxtxi matxtxix

.SUFFIXES: .F .for
.F.for :
	./cppp $(DEFINES) $< $*.for

######################################################################

cov2cor.o :	cov2cor.for matman.for
	$(F90) $(FFLAGS) cov2cor.for

covcond.o :	covcond.for matman.for
	$(F90) $(FFLAGS) covcond.for

mat2jco.o :	mat2jco.for matman.for
	$(F90) $(FFLAGS) mat2jco.for

mat2srf.o :	mat2srf.for
	$(F90) $(FFLAGS) mat2srf.for

matadd.o :	matadd.for matman.for
	$(F90) $(FFLAGS) matadd.for

matcolex.o :	matcolex.for matman.for
	$(F90) $(FFLAGS) matcolex.for

matdiag.o :	matdiag.for matman.for
	$(F90) $(FFLAGS) matdiag.for

matdiff.o :	matdiff.for matman.for
	$(F90) $(FFLAGS) matdiff.for

matinvp.o :	matinvp.for matman.for
	$(F90) $(FFLAGS) matinvp.for

matjoinc.o :	matjoinc.for matman.for
	$(F90) $(FFLAGS) matjoinc.for

matjoind.o :	matjoind.for matman.for
	$(F90) $(FFLAGS) matjoind.for

matjoinr.o :	matjoinr.for matman.for
	$(F90) $(FFLAGS) matjoinr.for

matorder.o :	matorder.for matman.for
	$(F90) $(FFLAGS) matorder.for

matprod.o :	matprod.for matman.for
	$(F90) $(FFLAGS) matprod.for

matquad.o :	matquad.for matman.for
	$(F90) $(FFLAGS) matquad.for

matrow.o :	matrow.for matman.for
	$(F90) $(FFLAGS) matrow.for

matsmul.o :	matsmul.for matman.for
	$(F90) $(FFLAGS) matsmul.for

matspec.o :	matspec.for matman.for
	$(F90) $(FFLAGS) matspec.for

matsvd.o :	matsvd.for matman.for
	$(F90) $(FFLAGS) matsvd.for

matsym.o :	matsym.for matman.for
	$(F90) $(FFLAGS) matsym.for

mattrans.o :	mattrans.for matman.for
	$(F90) $(FFLAGS) mattrans.for

matxtxi.o :	matxtxi.for matman.for
	$(F90) $(FFLAGS) matxtxi.for

matxtxix.o :	matxtxix.for matman.for
	$(F90) $(FFLAGS) matxtxix.for

space.o :	space.for
	$(F90) $(FFLAGS) space.for

pgetcl.o :	pgetcl.for
	$(F90) $(FFLAGS) pgetcl.for

openun.o :	openun.for
	$(F90) $(FFLAGS) openun.for

lapack1.o :	lapack1.for
	$(F90) $(FFLAGS) lapack1.for

linpos.o :	linpos.for
	$(F90) $(FFLAGS) linpos.for


###############################################################################

cov2cor :	cov2cor.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o cov2cor cov2cor.o space.o  pgetcl.o

covcond :	covcond.o space.o pgetcl.o linpos.o
	$(LD) $(LDFLAGS) -o covcond covcond.o space.o pgetcl.o linpos.o

mat2jco :	mat2jco.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o mat2jco mat2jco.o space.o  pgetcl.o

mat2srf :	mat2srf.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o mat2srf mat2srf.o space.o pgetcl.o

matadd :	matadd.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o matadd matadd.o space.o  pgetcl.o

matcolex :	matcolex.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o matcolex matcolex.o space.o  pgetcl.o

matdiag :	matdiag.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o matdiag matdiag.o space.o  pgetcl.o

matdiff :	matdiff.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o matdiff matdiff.o space.o  pgetcl.o

matinvp :	matinvp.o space.o pgetcl.o linpos.o
	$(LD) $(LDFLAGS) -o matinvp matinvp.o space.o pgetcl.o linpos.o

matjoinc :	matjoinc.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o matjoinc matjoinc.o space.o  pgetcl.o

matjoind :	matjoind.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o matjoind matjoind.o space.o  pgetcl.o

matjoinr :	matjoinr.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o matjoinr matjoinr.o space.o  pgetcl.o

matorder :	matorder.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o matorder matorder.o space.o pgetcl.o

matprod :	matprod.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o matprod matprod.o space.o  pgetcl.o

matquad :	matquad.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o matquad matquad.o space.o  pgetcl.o

matrow :	matrow.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o matrow matrow.o space.o  pgetcl.o

matsmul :	matsmul.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o matsmul matsmul.o space.o  pgetcl.o

matspec :	matspec.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o matspec matspec.o space.o pgetcl.o

matsvd :	matsvd.o space.o pgetcl.o lapack1.o
	$(LD) $(LDFLAGS) -o matsvd matsvd.o space.o  pgetcl.o lapack1.o

matsym :	matsym.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o matsym matsym.o space.o  pgetcl.o

mattrans :	mattrans.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o mattrans mattrans.o space.o  pgetcl.o

matxtxi :	matxtxi.o space.o pgetcl.o linpos.o
	$(LD) $(LDFLAGS) -o matxtxi matxtxi.o space.o pgetcl.o linpos.o

matxtxix :	matxtxix.o space.o pgetcl.o linpos.o
	$(LD) $(LDFLAGS) -o matxtxix matxtxix.o space.o pgetcl.o linpos.o

