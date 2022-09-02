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

all :	eigproc inschek jacwrit jco2jco jcosub jcochek \
        jcotrans par2par paramfix parrep parrep_rrf pestchek \
        pestgen picalc ppause pslave pagent pstop \
        pstopst punpause svdaprep tempchek wtfactor ppd2asc ppd2par \
        jcb2csv jcb2rrf jcb2rrf1 jcbcomb jcb2par jcb2avepar ejrowcol \
        ejcol2vec ejrow2vec tpl2pst

.SUFFIXES: .F .for
.F.for :
	./cppp $(DEFINES) $< $*.for

######################################################################


cheksub.o:	cheksub.for
	$(F90) $(FFLAGS) cheksub.for


common.o :	common.for
	$(F90) $(FFLAGS) common.for


compress.o :	compress.for
	$(F90) $(FFLAGS) compress.for


eigproc.o :	eigproc.for
	$(F90) $(FFLAGS) eigproc.for


inschek.o :	inschek.for
	$(F90) $(FFLAGS) inschek.for


jacwrit.o :	jacwrit.for
	$(F90) $(FFLAGS) jacwrit.for


jco2jco.o :	jco2jco.for
	$(F90) $(FFLAGS) jco2jco.for


jcosub.o :	jcosub.for
	$(F90) $(FFLAGS) jcosub.for


jcochek.o :	jcochek.for
	$(F90) $(FFLAGS) jcochek.for


jcotrans.o :	jcotrans.for
	$(F90) $(FFLAGS) jcotrans.for


ppd2par.o :	ppd2par.for
	$(F90) $(FFLAGS) ppd2par.for


ppd2asc.o :	ppd2asc.for
	$(F90) $(FFLAGS) ppd2asc.for


nblnk.o :	nblnk.for
	$(F90) $(FFLAGS) nblnk.for


openun.o:	openun.for
	$(F90) $(FFLAGS) openun.for


par2par.o :	par2par.for
	$(F90) $(FFLAGS) par2par.for


paramfix.o :	paramfix.for
	$(F90) $(FFLAGS) paramfix.for


parrep.o :	parrep.for
	$(F90) $(FFLAGS) parrep.for


parrep_rrf.o :	parrep_rrf.for
	$(F90) $(FFLAGS) parrep_rrf.for


PESTDATA.mod :   pestdata.for
	$(F90) $(FFLAGS) pestdata.for


pestchek.o :	pestchek.for
	$(F90) $(FFLAGS) pestchek.for


pestgen.o :	pestgen.for
	$(F90) $(FFLAGS) pestgen.for


pestsub1.o :	pestsub1.for
	$(F90) $(FFLAGS) pestsub1.for


pgetcl.o :	pgetcl.for
	$(F90) $(FFLAGS) pgetcl.for


picalc.o :	picalc.for
	$(F90) $(FFLAGS) picalc.for


pslave.o :	pslave.for
	$(F90) $(FFLAGS) pslave.for


pagent.o :	pagent.for
	$(F90) $(FFLAGS) pagent.for


pstop.o :	pstop.for
	$(F90) $(FFLAGS) pstop.for


pstopst.o :	pstopst.for
	$(F90) $(FFLAGS) pstopst.for


punpause.o:	punpause.for
	$(F90) $(FFLAGS) punpause.for


ppause.o:	ppause.for
	$(F90) $(FFLAGS) ppause.for


readpest.o :	readpest.for PESTDATA.mod
	$(F90) $(FFLAGS) readpest.for


space.o :	space.for
	$(F90) $(FFLAGS) space.for


svdaprep.o:	svdaprep.for
	$(F90) $(FFLAGS) svdaprep.for


sstop.o :	sstop.for
	$(F90) $(FFLAGS) sstop.for


tempchek.o :	tempchek.for
	$(F90) $(FFLAGS) tempchek.for


pestwait.o :	pestwait.for
	$(F90) $(FFLAGS) pestwait.for


writint.o :	writint.for
	$(F90) $(FFLAGS) writint.for


writsig.o :	writsig.for
	$(F90) $(FFLAGS) writsig.for


wtfactor.o :	wtfactor.for
	$(F90) $(FFLAGS) wtfactor.for


jcb2csv.o :	jcb2csv.for
	$(F90) $(FFLAGS) jcb2csv.for


jcb2par.o :	jcb2par.for
	$(F90) $(FFLAGS) jcb2par.for


jcb2avepar.o :	jcb2avepar.for
	$(F90) $(FFLAGS) jcb2avepar.for


jcb2rrf.o :	jcb2rrf.for
	$(F90) $(FFLAGS) jcb2rrf.for


jcb2rrf1.o :	jcb2rrf1.for
	$(F90) $(FFLAGS) jcb2rrf1.for


jcb2comb.o :	jcb2comb.for
	$(F90) $(FFLAGS) jcb2comb.for


ejrowcol.o :	ejrowcol.for
	$(F90) $(FFLAGS) ejrowcol.for


ejcol2vec.o :	ejcol2vec.for
	$(F90) $(FFLAGS) ejcol2vec.for


ejrow2vec.o :	ejrow2vec.for
	$(F90) $(FFLAGS) ejrow2vec.for


tpl2pst.o :	tpl2pst.for
	$(F90) $(FFLAGS) tpl2pst.for

######################################################################


eigproc :	eigproc.o space.o nblnk.o pgetcl.o
	$(LD) $(LDFLAGS) -o eigproc eigproc.o space.o nblnk.o pgetcl.o


inschek :	inschek.o pgetcl.o nblnk.o space.o
	$(LD) $(LDFLAGS) -o inschek inschek.o pgetcl.o nblnk.o space.o


jacwrit :	jacwrit.o pgetcl.o nblnk.o space.o
	$(LD) $(LDFLAGS) -o jacwrit jacwrit.o pgetcl.o nblnk.o space.o


jco2jco :	jco2jco.o space.o nblnk.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jco2jco jco2jco.o space.o nblnk.o pgetcl.o openun.o


jcosub :	jcosub.o space.o nblnk.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcosub jcosub.o space.o nblnk.o pgetcl.o openun.o


jcochek :	jcochek.o space.o nblnk.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcochek jcochek.o space.o nblnk.o pgetcl.o openun.o


jcotrans :	jcotrans.o space.o nblnk.o pgetcl.o
	$(LD) $(LDFLAGS) -o jcotrans jcotrans.o space.o nblnk.o pgetcl.o


par2par :	par2par.o pgetcl.o nblnk.o writsig.o space.o
	$(LD) $(LDFLAGS) -o par2par par2par.o pgetcl.o nblnk.o writsig.o space.o


paramfix :	paramfix.o pgetcl.o nblnk.o space.o
	$(LD) $(LDFLAGS) -o paramfix paramfix.o pgetcl.o nblnk.o space.o


parrep :	 parrep.o pgetcl.o nblnk.o space.o
	$(LD) $(LDFLAGS) -o parrep parrep.o pgetcl.o nblnk.o space.o


parrep_rrf :	 parrep_rrf.o pgetcl.o nblnk.o space.o
	$(LD) $(LDFLAGS) -o parrep_rrf parrep_rrf.o pgetcl.o nblnk.o space.o


pestchek :	pestchek.o common.o cheksub.o pgetcl.o nblnk.o space.o
	$(LD) $(LDFLAGS) -o pestchek pestchek.o common.o cheksub.o pgetcl.o nblnk.o space.o


pestgen :	pestgen.o pgetcl.o nblnk.o space.o
	$(LD) $(LDFLAGS) -o pestgen pestgen.o pgetcl.o nblnk.o space.o


picalc :	picalc.o pgetcl.o
	$(LD) $(LDFLAGS) -o picalc picalc.o pgetcl.o


ppause :	ppause.o
	$(LD) $(LDFLAGS) -o ppause ppause.o


pslave :	pslave.o pestwait.o pgetcl.o sstop.o
	$(LD) $(LDFLAGS) -o pslave pslave.o pestwait.o pgetcl.o sstop.o


pagent :	pagent.o pestwait.o pgetcl.o sstop.o
	$(LD) $(LDFLAGS) -o pagent pagent.o pestwait.o pgetcl.o sstop.o


pstop :	pstop.o
	$(LD) $(LDFLAGS) -o pstop pstop.o


pstopst :	pstopst.o
	$(LD) $(LDFLAGS) -o pstopst pstopst.o


punpause :	punpause.o
	$(LD) $(LDFLAGS) -o punpause punpause.o


svdaprep :	svdaprep.o
	$(LD) $(LDFLAGS) -o svdaprep svdaprep.o


tempchek :	tempchek.o pgetcl.o nblnk.o writsig.o space.o
	$(LD) $(LDFLAGS) -o tempchek tempchek.o pgetcl.o nblnk.o writsig.o space.o


wtfactor :	wtfactor.o pgetcl.o nblnk.o space.o
	$(LD) $(LDFLAGS) -o wtfactor wtfactor.o pgetcl.o nblnk.o space.o


ppd2asc :	ppd2asc.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o ppd2asc ppd2asc.o space.o pgetcl.o openun.o


ppd2par :	ppd2par.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o ppd2par ppd2par.o space.o pgetcl.o openun.o


jcb2csv :	jcb2csv.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcb2csv jcb2csv.o space.o pgetcl.o openun.o


jcb2par :	jcb2par.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcb2par jcb2par.o space.o pgetcl.o openun.o


jcb2avepar :	jcb2avepar.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcb2avepar jcb2avepar.o space.o pgetcl.o openun.o


jcb2rrf :	jcb2rrf.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcb2rrf jcb2rrf.o space.o pgetcl.o openun.o


jcb2rrf1 :	jcb2rrf1.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcb2rrf1 jcb2rrf1.o space.o pgetcl.o openun.o


jcbcomb :	jcbcomb.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o jcbcomb jcbcomb.o space.o pgetcl.o openun.o


ejrowcol :	ejrowcol.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o ejrowcol ejrowcol.o space.o pgetcl.o openun.o


ejcol2vec :	ejcol2vec.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o ejcol2vec ejcol2vec.o space.o pgetcl.o openun.o


ejrow2vec :	ejrow2vec.o space.o pgetcl.o openun.o
	$(LD) $(LDFLAGS) -o ejrow2vec ejrow2vec.o space.o pgetcl.o openun.o


tpl2pst :	tpl2pst.o space.o pgetcl.o
	$(LD) $(LDFLAGS) -o tpl2pst tpl2pst.o space.o pgetcl.o
