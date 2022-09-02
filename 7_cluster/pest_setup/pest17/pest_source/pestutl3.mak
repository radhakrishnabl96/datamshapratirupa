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

all :	addreg1 addreg2 addreg3 dercomb1 genlin jco2mat jcoaddz jcocomb jcodiff jcoorder jcosum jcozero jcopcat \
       jrow2mat jrow2vec jcol2vec obsrep paramerr pclc2mat pcov2mat pest2vec \
       pestlin prederr prederr1 prederr2 prederr3 pwtadj1 pwtadj2 regerr resproc \
       regpred reswrit scalepar subreg1 vec2pest veclog randpar randpar1 randpar2 randpar3 randpar4 \
       randobs mulpartab mulparobstab comfilnme comfilnme1 paramid postjactest genlinpred genlinpred_abbrev \
       phistats lhs2pest pest2lhs parreduce assesspar

.SUFFIXES: .F .for
.F.for :
	./cppp $(DEFINES) $< $*.for

random.for :	random.F90
	./cppp $(DEFINES) random.F90 random.for

######################################################################


addreg1.o :	addreg1.for
	$(F90) $(FFLAGS) addreg1.for

addreg2.o :	addreg2.for
	$(F90) $(FFLAGS) addreg2.for

addreg3.o :	addreg3.for
	$(F90) $(FFLAGS) addreg3.for

dercomb1.o :	dercomb1.for
	$(F90) $(FFLAGS) dercomb1.for

genlin.o :	genlin.for
	$(F90) $(FFLAGS) genlin.for

jco2mat.o :	jco2mat.for
	$(F90) $(FFLAGS) jco2mat.for

jcoaddz.o :	jcoaddz.for
	$(F90) $(FFLAGS) jcoaddz.for

jcocomb.o :	jcocomb.for
	$(F90) $(FFLAGS) jcocomb.for

jcodiff.o :	jcodiff.for
	$(F90) $(FFLAGS) jcodiff.for

jcoorder.o :	jcoorder.for
	$(F90) $(FFLAGS) jcoorder.for

jcosum.o :	jcosum.for
	$(F90) $(FFLAGS) jcosum.for

jcozero.o :	jcozero.for
	$(F90) $(FFLAGS) jcozero.for

jcopcat.o :	jcopcat.for
	$(F90) $(FFLAGS) jcopcat.for

jrow2mat.o :	jrow2mat.for
	$(F90) $(FFLAGS) jrow2mat.for

jrow2vec.o :	jrow2vec.for
	$(F90) $(FFLAGS) jrow2vec.for

jcol2vec.o :	jcol2vec.for
	$(F90) $(FFLAGS) jcol2vec.for

nblnk.o :	nblnk.for
	$(F90) $(FFLAGS) nblnk.for

obsrep.o :	obsrep.for
	$(F90) $(FFLAGS) obsrep.for

paramerr.o :	paramerr.for
	$(F90) $(FFLAGS) paramerr.for

pclc2mat.o :	pclc2mat.for matman.for
	$(F90) $(FFLAGS) pclc2mat.for

pcov2mat.o :	pcov2mat.for
	$(F90) $(FFLAGS) pcov2mat.for

pest2vec.o :	pest2vec.for
	$(F90) $(FFLAGS) pest2vec.for

pestlin.o :	pestlin.for
	$(F90) $(FFLAGS) pestlin.for

prederr.o :	prederr.for
	$(F90) $(FFLAGS) prederr.for

prederr1.o :	prederr1.for
	$(F90) $(FFLAGS) prederr1.for

prederr2.o :	prederr2.for
	$(F90) $(FFLAGS) prederr2.for

prederr3.o :	prederr3.for
	$(F90) $(FFLAGS) prederr3.for

pwtadj1.o :	pwtadj1.for
	$(F90) $(FFLAGS) pwtadj1.for

pwtadj2.o :	pwtadj2.for
	$(F90) $(FFLAGS) pwtadj2.for

regerr.o :	regerr.for
	$(F90) $(FFLAGS) regerr.for

resproc.o :	resproc.for
	$(F90) $(FFLAGS) resproc.for

regpred.o :	regpred.for
	$(F90) $(FFLAGS) regpred.for

reswrit.o :	reswrit.for
	$(F90) $(FFLAGS) reswrit.for

scalepar.o :	scalepar.for
	$(F90) $(FFLAGS) scalepar.for

subreg1.o :	subreg1.for
	$(F90) $(FFLAGS) subreg1.for

vec2pest.o :	vec2pest.for matman.for
	$(F90) $(FFLAGS) vec2pest.for

veclog.o :	veclog.for matman.for
	$(F90) $(FFLAGS) veclog.for

space.o :	space.for
	$(F90) $(FFLAGS) space.for

pgetcl.o :	pgetcl.for
	$(F90) $(FFLAGS) pgetcl.for

openun.o :	openun.for
	$(F90) $(FFLAGS) openun.for

certfile.o :	certfile.for
	$(F90) $(FFLAGS) certfile.for

lapack1.o :	lapack1.for
	$(F90) $(FFLAGS) lapack1.for

linpos.o :	linpos.for
	$(F90) $(FFLAGS) linpos.for

orthog.o :	orthog.for
	$(F90) $(FFLAGS) orthog.for

pdf.mod :	random.for
	$(F90) $(FFLAGS1) random.for

randgen.o :	randgen.for
	$(F90) $(FFLAGS) randgen.for

slinpos.o :	slinpos.for
	$(F90) $(FFLAGS) slinpos.for

randpar.o :	randpar.for pdf.mod
	$(F90) $(FFLAGS) randpar.for

randpar1.o :	randpar1.for
	$(F90) $(FFLAGS) randpar1.for

randpar2.o :	randpar2.for
	$(F90) $(FFLAGS) randpar2.for

randpar3.o :	randpar3.for
	$(F90) $(FFLAGS) randpar3.for

randpar4.o :	randpar4.for
	$(F90) $(FFLAGS) randpar4.for

randobs.o :	randobs.for pdf.mod
	$(F90) $(FFLAGS) randobs.for

comfilnme.o :	comfilnme.for
	$(F90) $(FFLAGS) comfilnme.for

comfilnme1.o :	comfilnme1.for
	$(F90) $(FFLAGS) comfilnme1.for

mulpartab.o :	mulpartab.for
	$(F90) $(FFLAGS) mulpartab.for

mulparobstab.o :	mulparobstab.for
	$(F90) $(FFLAGS) mulparobstab.for

paramid.o :	paramid.for
	$(F90) $(FFLAGS) paramid.for

postjactest.o :	postjactest.for
	$(F90) $(FFLAGS) postjactest.for

genlinpred.o :	genlinpred.for
	$(F90) $(FFLAGS) genlinpred.for

genlinpred_abbrev.o :	genlinpred_abbrev.for
	$(F90) $(FFLAGS) genlinpred_abbrev.for

phistats.o :	phistats.for
	$(F90) $(FFLAGS) phistats.for

lhs2pest.o :	lhs2pest.for
	$(F90) $(FFLAGS) lhs2pest.for

pest2lhs.o :	pest2lhs.for
	$(F90) $(FFLAGS) pest2lhs.for

parreduce.o :	parreduce.for
	$(F90) $(FFLAGS) parreduce.for

assesspar.o :	assesspar.for
	$(F90) $(FFLAGS) assesspar.for

drealrd.o:	drealrd.for
	$(F90) $(FFLAGS) drealrd.for

######################################################################

addreg1 :	addreg1.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o addreg1 addreg1.o space.o pgetcl.o

addreg2 :	addreg2.o space.o pgetcl.o drealrd.o
	$(LD) $(LDFLAGS) -o addreg2 addreg2.o space.o pgetcl.o drealrd.o

addreg3 :	addreg3.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o addreg3 addreg3.o space.o pgetcl.o

dercomb1 :	dercomb1.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o dercomb1 dercomb1.o space.o pgetcl.o

genlin :	genlin.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o genlin genlin.o space.o pgetcl.o openun.o

jco2mat :	jco2mat.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jco2mat jco2mat.o space.o pgetcl.o openun.o

jcoaddz :	jcoaddz.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcoaddz jcoaddz.o space.o pgetcl.o openun.o

jcocomb :	jcocomb.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcocomb jcocomb.o space.o pgetcl.o openun.o

jcodiff :	jcodiff.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcodiff jcodiff.o space.o pgetcl.o openun.o

jcoorder :	jcoorder.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcoorder jcoorder.o space.o pgetcl.o openun.o

jcosum :	jcosum.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcosum jcosum.o space.o pgetcl.o openun.o

jcozero :	jcozero.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcozero jcozero.o space.o pgetcl.o openun.o

jcopcat :	jcopcat.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcopcat jcopcat.o space.o pgetcl.o openun.o

jrow2mat :	jrow2mat.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jrow2mat jrow2mat.o space.o pgetcl.o openun.o

jrow2vec :	jrow2vec.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jrow2vec jrow2vec.o space.o pgetcl.o openun.o

jcol2vec :	jcol2vec.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcol2vec jcol2vec.o space.o pgetcl.o openun.o


obsrep :	obsrep.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o obsrep obsrep.o space.o  pgetcl.o

paramerr :	paramerr.o space.o openun.o certfile.o
	$(LD) $(LDFLAGS) -o paramerr paramerr.o space.o openun.o certfile.o

pclc2mat :	pclc2mat.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o pclc2mat pclc2mat.o space.o  pgetcl.o

pcov2mat :	pcov2mat.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o pcov2mat pcov2mat.o space.o  pgetcl.o

pest2vec :	pest2vec.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o pest2vec pest2vec.o space.o  pgetcl.o

pestlin :	pestlin.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o pestlin pestlin.o space.o pgetcl.o openun.o

prederr :	prederr.o space.o openun.o certfile.o
	$(LD) $(LDFLAGS) -o prederr prederr.o space.o openun.o certfile.o

prederr1 :	prederr1.o space.o openun.o certfile.o
	$(LD) $(LDFLAGS) -o prederr1 prederr1.o space.o openun.o certfile.o

prederr2 :	prederr2.o space.o openun.o certfile.o
	$(LD) $(LDFLAGS) -o prederr2 prederr2.o space.o openun.o certfile.o

prederr3 :	prederr3.o space.o openun.o certfile.o
	$(LD) $(LDFLAGS) -o prederr3 prederr3.o space.o openun.o certfile.o

pwtadj1 :	pwtadj1.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o pwtadj1 pwtadj1.o space.o pgetcl.o

pwtadj2 :	pwtadj2.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o pwtadj2 pwtadj2.o space.o pgetcl.o

regerr :	regerr.o space.o openun.o certfile.o
	$(LD) $(LDFLAGS) -o regerr regerr.o space.o openun.o certfile.o

resproc :	resproc.o space.o pgetcl.o lapack1.o linpos.o openun.o orthog.o
	$(LD) $(LDFLAGS) -o resproc resproc.o space.o pgetcl.o lapack1.o linpos.o \
	                    openun.o orthog.o

regpred :	regpred.o space.o lapack1.o certfile.o
	$(LD) $(LDFLAGS) -o regpred regpred.o space.o lapack1.o certfile.o

reswrit :	reswrit.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o reswrit reswrit.o space.o pgetcl.o openun.o

scalepar :	scalepar.o space.o openun.o certfile.o
	$(LD) $(LDFLAGS) -o scalepar scalepar.o space.o openun.o certfile.o

subreg1 :	subreg1.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o subreg1 subreg1.o space.o pgetcl.o

vec2pest :	vec2pest.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o vec2pest vec2pest.o space.o  pgetcl.o

veclog :	veclog.o space.o  pgetcl.o
	$(LD) $(LDFLAGS) -o veclog veclog.o space.o pgetcl.o

randpar :	randpar.o space.o certfile.o randgen.o slinpos.o random.o
	$(LD) $(LDFLAGS) -o randpar randpar.o space.o certfile.o randgen.o slinpos.o random.o

randpar1 :	randpar1.o space.o certfile.o lapack1.o
	$(LD) $(LDFLAGS) -o randpar1 randpar1.o space.o certfile.o lapack1.o

randpar2 :	randpar2.o space.o certfile.o lapack1.o
	$(LD) $(LDFLAGS) -o randpar2 randpar2.o space.o certfile.o lapack1.o

randpar3 :	randpar3.o space.o certfile.o lapack1.o openun.o
	$(LD) $(LDFLAGS) -o randpar3 randpar3.o space.o certfile.o lapack1.o openun.o

randpar4 :	randpar4.o space.o certfile.o lapack1.o openun.o
	$(LD) $(LDFLAGS) -o randpar4 randpar4.o space.o certfile.o lapack1.o openun.o

randobs :	randobs.o openun.o random.o randgen.o slinpos.o
	$(LD) $(LDFLAGS) -o randobs randobs.o openun.o random.o randgen.o slinpos.o

mulpartab :	mulpartab.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o mulpartab mulpartab.o space.o pgetcl.o

mulparobstab :	mulparobstab.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o mulparobstab mulparobstab.o space.o pgetcl.o

comfilnme :	comfilnme.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o comfilnme comfilnme.o space.o pgetcl.o

comfilnme1 :	comfilnme1.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o comfilnme1 comfilnme1.o space.o pgetcl.o

paramid :	paramid.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o paramid paramid.o space.o pgetcl.o

postjactest :	postjactest.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o postjactest postjactest.o space.o pgetcl.o

genlinpred :	genlinpred.o pgetcl.o openun.o space.o certfile.o
	$(LD) $(LDFLAGS) -o genlinpred genlinpred.o pgetcl.o openun.o certfile.o space.o

genlinpred_abbrev :	genlinpred_abbrev.o pgetcl.o openun.o space.o certfile.o
	$(LD) $(LDFLAGS) -o genlinpred_abbrev genlinpred_abbrev.o pgetcl.o openun.o certfile.o space.o

phistats :	phistats.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o phistats phistats.o space.o pgetcl.o

lhs2pest :	lhs2pest.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o lhs2pest lhs2pest.o space.o pgetcl.o

pest2lhs :	pest2lhs.o certfile.o space.o
	$(LD) $(LDFLAGS) -o pest2lhs pest2lhs.o certfile.o space.o

parreduce :	parreduce.o space.o nblnk.o pgetcl.o openun.o lapack1.o
	$(LD) $(LDFLAGS) -o parreduce parreduce.o space.o nblnk.o pgetcl.o openun.o lapack1.o

assesspar :	assesspar.o space.o nblnk.o pgetcl.o openun.o lapack1.o certfile.o
	$(LD) $(LDFLAGS) -o assesspar assesspar.o space.o nblnk.o pgetcl.o openun.o lapack1.o certfile.o



