#
# Change the following if a different installation path is required.
#

INSTALLDIR=$(HOME)/bin

######################################################################
F90=gfortran
FFLAGS=
######################################################################


######################################################################
# DON'T EDIT BELOW THIS LINE
######################################################################


cppp :  cppp.F
	$(F90) $(FFLAGS) -o cppp cppp.F


d_test: d_test.f
	$(F90) -o d_test d_test.f
	./d_test

clean :
	rm -f *.o
	rm -f *.mod
	rm -f *.for
	rm -f string_utils.f90
	rm -f containers.f90
	rm -f array_btree.f90
	rm -f dsi.f90

clean_exe :
	rm -f addcovmat
	rm -f addreg1
	rm -f addreg2
	rm -f addreg3
	rm -f assesspar
	rm -f beopest
	rm -f calmaintain
	rm -f cmaes_p
	rm -f comfilnme
	rm -f comfilnme1
	rm -f cov2cor
	rm -f covcond
	rm -f cppp
	rm -f csv2par
	rm -f csv2jcb
	rm -f dercomb1
	rm -f dsi
	rm -f eigproc
	rm -f ejcol2vec
	rm -f ejrow2vec
	rm -f ejrowcol
	rm -f genlin
	rm -f genlinpred
	rm -f genlinpred_abbrev
	rm -f identpar
	rm -f infstat
	rm -f infstat1
	rm -f inschek
	rm -f jactest
	rm -f jacwrit
	rm -f jcb2csv
	rm -f jcb2par
	rm -f jcb2rrf
	rm -f jcb2rrf1
	rm -f jcb2avepar
	rm -f jco2jco
	rm -f jco2mat
	rm -f jcoaddz
	rm -f jcoblank
	rm -f jcochek
	rm -f jcocomb
	rm -f jcodiff
	rm -f jcol2vec
	rm -f jcomix
	rm -f jcoorder
	rm -f jcopcat
	rm -f jcosub
	rm -f jcosum
	rm -f jcotrans
	rm -f jcowt
	rm -f jcozero
	rm -f jrow2mat
	rm -f jrow2vec
	rm -f lhs2pest
	rm -f mat2jco
	rm -f mat2srf
	rm -f matadd
	rm -f matcolex
	rm -f matdiag
	rm -f matdiff
	rm -f matinvp
	rm -f matjoinc
	rm -f matjoind
	rm -f matjoinr
	rm -f matorder
	rm -f matprod
	rm -f matquad
	rm -f matrow
	rm -f matsmul
	rm -f matspec
	rm -f matsvd
	rm -f matsym
	rm -f mattrans
	rm -f matxtxi
	rm -f matxtxix
	rm -f muljcosen
	rm -f mulparobstab
	rm -f mulpartab
	rm -f obs2obs
	rm -f obscalc
	rm -f obscomp
	rm -f obsrep
	rm -f pagent
	rm -f par2par
	rm -f paramerr
	rm -f paramfix
	rm -f paramid
	rm -f parcalc
	rm -f parobs2rrf
	rm -f parreduce
	rm -f parrep
	rm -f parrep_rrf
	rm -f parvar1
	rm -f pclc2mat
	rm -f pcov2mat
	rm -f pest
	rm -f pest2lhs
	rm -f pest2vec
	rm -f pestchek
	rm -f pestgen
	rm -f pestlin
	rm -f phistats
	rm -f picalc
	rm -f pnulpar
	rm -f postjactest
	rm -f ppause
	rm -f ppd2asc
	rm -f ppd2par
	rm -f ppest
	rm -f prederr
	rm -f prederr1
	rm -f prederr2
	rm -f prederr3
	rm -f predunc1
	rm -f predunc4
	rm -f predunc5
	rm -f predunc6
	rm -f predunc7
	rm -f predvar1
	rm -f predvar1a
	rm -f predvar1b
	rm -f predvar1c
	rm -f predvar2
	rm -f predvar3
	rm -f predvar4
	rm -f predvar5
	rm -f pslave
	rm -f pstclean
	rm -f pstop
	rm -f pstopst
	rm -f punpause
	rm -f pwtadj1
	rm -f pwtadj2
	rm -f randobs
	rm -f randpar
	rm -f randpar1
	rm -f randpar2
	rm -f randpar3
	rm -f randpar4
	rm -f rdmulres
	rm -f regerr
	rm -f regpred
	rm -f resproc
	rm -f reswrit
	rm -f rrf2jco
	rm -f rrf2par
	rm -f rrf2parobs
	rm -f rrf2tab
	rm -f rrf2csv
	rm -f rrfappend
	rm -f rrfcalcpsi
	rm -f rrfcat
	rm -f rrfclean
	rm -f rrfcull
	rm -f scalepar
	rm -f sceua_p
	rm -f sensan
	rm -f senschek
	rm -f simcase
	rm -f ssstat
	rm -f subreg1
	rm -f supcalc
	rm -f supobspar
	rm -f supobspar1
	rm -f supobsprep
	rm -f svdaprep
	rm -f tempchek
	rm -f tpl2pst
	rm -f vec2pest
	rm -f veclog
	rm -f wtfactor
	rm -f wtsenout

install :
	install addcovmat $(installDIR)/addcovmat
	install addreg1 $(installDIR)/addreg1
	install addreg2 $(installDIR)/addreg2
	install addreg3 $(installDIR)/addreg3
	install assesspar $(installDIR)/assesspar
	install beopest $(installDIR)/beopest
	install calmaintain $(installDIR)/calmaintain
	install cmaes_p $(installDIR)/cmaes_p
	install comfilnme $(installDIR)/comfilnme
	install comfilnme1 $(installDIR)/comfilnme1
	install cov2cor $(installDIR)/cov2cor
	install covcond $(installDIR)/covcond
	install cppp $(installDIR)/cppp
	install csv2par $(installDIR)/csv2par
	install csv2jcb $(installDIR)/csv2jcb
	install dercomb1 $(installDIR)/dercomb1
	install dsi $(installDIR)/dsi
	install eigproc $(installDIR)/eigproc
	install ejcol2vec $(installDIR)/ejcol2vec
	install ejrow2vec $(installDIR)/ejrow2vec
	install ejrowcol $(installDIR)/ejrowcol
	install genlin $(installDIR)/genlin
	install genlinpred $(installDIR)/genlinpred
	install genlinpred_abbrev $(installDIR)/genlinpred_abbrev
	install identpar $(installDIR)/identpar
	install infstat $(installDIR)/infstat
	install infstat1 $(installDIR)/infstat1
	install inschek $(installDIR)/inschek
	install jactest $(installDIR)/jactest
	install jacwrit $(installDIR)/jacwrit
	install jcb2csv $(installDIR)/jcb2csv
	install jcb2par $(installDIR)/jcb2par
	install jcb2rrf $(installDIR)/jcb2rrf
	install jcb2rrf1 $(installDIR)/jcb2rrf1
	install jcb2avepar $(installDIR)/jcb2avepar
	install jco2jco $(installDIR)/jco2jco
	install jco2mat $(installDIR)/jco2mat
	install jcoaddz $(installDIR)/jcoaddz
	install jcoblank $(installDIR)/jcoblank
	install jcochek $(installDIR)/jcochek
	install jcocomb $(installDIR)/jcocomb
	install jcodiff $(installDIR)/jcodiff
	install jcol2vec $(installDIR)/jcol2vec
	install jcomix $(installDIR)/jcomix
	install jcoorder $(installDIR)/jcoorder
	install jcopcat $(installDIR)/jcopcat
	install jcosub $(installDIR)/jcosub
	install jcosum $(installDIR)/jcosum
	install jcotrans $(installDIR)/jcotrans
	install jcowt $(installDIR)/jcowt
	install jcozero $(installDIR)/jcozero
	install jrow2mat $(installDIR)/jrow2mat
	install jrow2vec $(installDIR)/jrow2vec
	install lhs2pest $(installDIR)/lhs2pest
	install mat2jco $(installDIR)/mat2jco
	install mat2srf $(installDIR)/mat2srf
	install matadd $(installDIR)/matadd
	install matcolex $(installDIR)/matcolex
	install matdiag $(installDIR)/matdiag
	install matdiff $(installDIR)/matdiff
	install matinvp $(installDIR)/matinvp
	install matjoinc $(installDIR)/matjoinc
	install matjoind $(installDIR)/matjoind
	install matjoinr $(installDIR)/matjoinr
	install matorder $(installDIR)/matorder
	install matprod $(installDIR)/matprod
	install matquad $(installDIR)/matquad
	install matrow $(installDIR)/matrow
	install matsmul $(installDIR)/matsmul
	install matspec $(installDIR)/matspec
	install matsvd $(installDIR)/matsvd
	install matsym $(installDIR)/matsym
	install mattrans $(installDIR)/mattrans
	install matxtxi $(installDIR)/matxtxi
	install matxtxix $(installDIR)/matxtxix
	install muljcosen $(installDIR)/muljcosen
	install mulparobstab $(installDIR)/mulparobstab
	install mulpartab $(installDIR)/mulpartab
	install obs2obs $(installDIR)/obs2obs
	install obscalc $(installDIR)/obscalc
	install obscomp $(installDIR)/obscomp
	install obsrep $(installDIR)/obsrep
	install pagent $(installDIR)/pagent
	install par2par $(installDIR)/par2par
	install paramerr $(installDIR)/paramerr
	install paramfix $(installDIR)/paramfix
	install paramid $(installDIR)/paramid
	install parcalc $(installDIR)/parcalc
	install parobs2rrf $(installDIR)/parobs2rrf
	install parreduce $(installDIR)/parreduce
	install parrep $(installDIR)/parrep
	install parrep_rrf $(installDIR)/parrep_rrf
	install parvar1 $(installDIR)/parvar1
	install pclc2mat $(installDIR)/pclc2mat
	install pcov2mat $(installDIR)/pcov2mat
	install pest $(installDIR)/pest
	install pest2lhs $(installDIR)/pest2lhs
	install pest2vec $(installDIR)/pest2vec
	install pestchek $(installDIR)/pestchek
	install pestgen $(installDIR)/pestgen
	install pestlin $(installDIR)/pestlin
	install phistats $(installDIR)/phistats
	install picalc $(installDIR)/picalc
	install pnulpar $(installDIR)/pnulpar
	install postjactest $(installDIR)/postjactest
	install ppause $(installDIR)/ppause
	install ppd2asc $(installDIR)/ppd2asc
	install ppd2par $(installDIR)/ppd2par
	install ppest $(installDIR)/ppest
	install prederr $(installDIR)/prederr
	install prederr1 $(installDIR)/prederr1
	install prederr2 $(installDIR)/prederr2
	install prederr3 $(installDIR)/prederr3
	install predunc1 $(installDIR)/predunc1
	install predunc4 $(installDIR)/predunc4
	install predunc5 $(installDIR)/predunc5
	install predunc6 $(installDIR)/predunc6
	install predunc7 $(installDIR)/predunc7
	install predvar1 $(installDIR)/predvar1
	install predvar1a $(installDIR)/predvar1a
	install predvar1b $(installDIR)/predvar1b
	install predvar1c $(installDIR)/predvar1c
	install predvar2 $(installDIR)/predvar2
	install predvar3 $(installDIR)/predvar3
	install predvar4 $(installDIR)/predvar4
	install predvar5 $(installDIR)/predvar5
	install pslave $(installDIR)/pslave
	install pstclean $(installDIR)/pstclean
	install pstop $(installDIR)/pstop
	install pstopst $(installDIR)/pstopst
	install punpause $(installDIR)/punpause
	install pwtadj1 $(installDIR)/pwtadj1
	install pwtadj2 $(installDIR)/pwtadj2
	install randobs $(installDIR)/randobs
	install randpar $(installDIR)/randpar
	install randpar1 $(installDIR)/randpar1
	install randpar2 $(installDIR)/randpar2
	install randpar3 $(installDIR)/randpar3
	install randpar4 $(installDIR)/randpar4
	install rdmulres $(installDIR)/rdmulres
	install regerr $(installDIR)/regerr
	install regpred $(installDIR)/regpred
	install resproc $(installDIR)/resproc
	install reswrit $(installDIR)/reswrit
	install rrf2jco $(installDIR)/rrf2jco
	install rrf2par $(installDIR)/rrf2par
	install rrf2parobs $(installDIR)/rrf2parobs
	install rrf2tab $(installDIR)/rrf2tab
	install rrf2csv $(installDIR)/rrf2csv
	install rrfappend $(installDIR)/rrfappend
	install rrfcalcpsi $(installDIR)/rrfcalcpsi
	install rrfcat $(installDIR)/rrfcat
	install rrfclean $(installDIR)/rrfclean
	install rrfcull $(installDIR)/rrfcull
	install scalepar $(installDIR)/scalepar
	install sceua_p $(installDIR)/sceua_p
	install sensan $(installDIR)/sensan
	install senschek $(installDIR)/senschek
	install simcase $(installDIR)/simcase
	install ssstat $(installDIR)/ssstat
	install subreg1 $(installDIR)/subreg1
	install supcalc $(installDIR)/supcalc
	install supobspar $(installDIR)/supobspar
	install supobspar1 $(installDIR)/supobspar1
	install supobsprep $(installDIR)/supobsprep
	install svdaprep $(installDIR)/svdaprep
	install tempchek $(installDIR)/tempchek
	install tpl2pst $(installDIR)/tpl2pst
	install vec2pest $(installDIR)/vec2pest
	install veclog $(installDIR)/veclog
	install wtfactor $(installDIR)/wtfactor
	install wtsenout $(installDIR)/wtsenout

uninstall :
	rm $(INSTALLDIR)/addcovmat
	rm $(INSTALLDIR)/addreg1
	rm $(INSTALLDIR)/addreg2
	rm $(INSTALLDIR)/addreg3
	rm $(INSTALLDIR)/assesspar
	rm $(INSTALLDIR)/beopest
	rm $(INSTALLDIR)/calmaintain
	rm $(INSTALLDIR)/cmaes_p
	rm $(INSTALLDIR)/comfilnme
	rm $(INSTALLDIR)/cov2cor
	rm $(INSTALLDIR)/covcond
	rm $(INSTALLDIR)/cppp
	rm $(INSTALLDIR)/csv2par
	rm $(INSTALLDIR)/csv2jcb
	rm $(INSTALLDIR)/dercomb1
	rm $(INSTALLDIR)/dsi
	rm $(INSTALLDIR)/eigproc
	rm $(INSTALLDIR)/ejcol2vec
	rm $(INSTALLDIR)/ejrow2vec
	rm $(INSTALLDIR)/ejrowcol
	rm $(INSTALLDIR)/genlin
	rm $(INSTALLDIR)/genlinpred
	rm $(INSTALLDIR)/genlinpred_abbrev
	rm $(INSTALLDIR)/identpar
	rm $(INSTALLDIR)/infstat
	rm $(INSTALLDIR)/infstat1
	rm $(INSTALLDIR)/inschek
	rm $(INSTALLDIR)/jactest
	rm $(INSTALLDIR)/jacwrit
	rm $(INSTALLDIR)/jcb2csv
	rm $(INSTALLDIR)/jcb2par
	rm $(INSTALLDIR)/jcb2rrf
	rm $(INSTALLDIR)/jcb2rrf1
	rm $(INSTALLDIR)/jcb2avepar
	rm $(INSTALLDIR)/jco2jco
	rm $(INSTALLDIR)/jco2mat
	rm $(INSTALLDIR)/jcoaddz
	rm $(INSTALLDIR)/jcoblank
	rm $(INSTALLDIR)/jcochek
	rm $(INSTALLDIR)/jcocomb
	rm $(INSTALLDIR)/jcodiff
	rm $(INSTALLDIR)/jcol2vec
	rm $(INSTALLDIR)/jcomix
	rm $(INSTALLDIR)/jcoorder
	rm $(INSTALLDIR)/jcopcat
	rm $(INSTALLDIR)/jcosub
	rm $(INSTALLDIR)/jcosum
	rm $(INSTALLDIR)/jcotrans
	rm $(INSTALLDIR)/jcowt
	rm $(INSTALLDIR)/jcozero
	rm $(INSTALLDIR)/jrow2mat
	rm $(INSTALLDIR)/jrow2vec
	rm $(INSTALLDIR)/lhs2pest
	rm $(INSTALLDIR)/mat2jco
	rm $(INSTALLDIR)/mat2srf
	rm $(INSTALLDIR)/matadd
	rm $(INSTALLDIR)/matcolex
	rm $(INSTALLDIR)/matdiag
	rm $(INSTALLDIR)/matdiff
	rm $(INSTALLDIR)/matinvp
	rm $(INSTALLDIR)/matjoinc
	rm $(INSTALLDIR)/matjoind
	rm $(INSTALLDIR)/matjoinr
	rm $(INSTALLDIR)/matorder
	rm $(INSTALLDIR)/matprod
	rm $(INSTALLDIR)/matquad
	rm $(INSTALLDIR)/matrow
	rm $(INSTALLDIR)/matsmul
	rm $(INSTALLDIR)/matspec
	rm $(INSTALLDIR)/matsvd
	rm $(INSTALLDIR)/matsym
	rm $(INSTALLDIR)/mattrans
	rm $(INSTALLDIR)/matxtxi
	rm $(INSTALLDIR)/matxtxix
	rm $(INSTALLDIR)/muljcosen
	rm $(INSTALLDIR)/mulparobstab
	rm $(INSTALLDIR)/mulpartab
	rm $(INSTALLDIR)/obs2obs
	rm $(INSTALLDIR)/obscalc
	rm $(INSTALLDIR)/obscomp
	rm $(INSTALLDIR)/obsrep
	rm $(INSTALLDIR)/par2par
	rm $(INSTALLDIR)/paramerr
	rm $(INSTALLDIR)/paramfix
	rm $(INSTALLDIR)/paramid
	rm $(INSTALLDIR)/parcalc
	rm $(INSTALLDIR)/parobs2rrf
	rm $(INSTALLDIR)/parreduce
	rm $(INSTALLDIR)/parrep
	rm $(INSTALLDIR)/parrep_rrf
	rm $(INSTALLDIR)/parvar1
	rm $(INSTALLDIR)/pclc2mat
	rm $(INSTALLDIR)/pcov2mat
	rm $(INSTALLDIR)/pest
	rm $(INSTALLDIR)/pest2lhs
	rm $(INSTALLDIR)/pest2vec
	rm $(INSTALLDIR)/pestchek
	rm $(INSTALLDIR)/pestgen
	rm $(INSTALLDIR)/pestlin
	rm $(INSTALLDIR)/phistats
	rm $(INSTALLDIR)/picalc
	rm $(INSTALLDIR)/pnulpar
	rm $(INSTALLDIR)/postjactest
	rm $(INSTALLDIR)/ppause
	rm $(INSTALLDIR)/ppd2asc
	rm $(INSTALLDIR)/ppd2par
	rm $(INSTALLDIR)/ppest
	rm $(INSTALLDIR)/prederr
	rm $(INSTALLDIR)/prederr1
	rm $(INSTALLDIR)/prederr2
	rm $(INSTALLDIR)/prederr3
	rm $(INSTALLDIR)/predunc1
	rm $(INSTALLDIR)/predunc4
	rm $(INSTALLDIR)/predunc5
	rm $(INSTALLDIR)/predunc6
	rm $(INSTALLDIR)/predunc7
	rm $(INSTALLDIR)/predvar1
	rm $(INSTALLDIR)/predvar1a
	rm $(INSTALLDIR)/predvar1b
	rm $(INSTALLDIR)/predvar1c
	rm $(INSTALLDIR)/predvar2
	rm $(INSTALLDIR)/predvar3
	rm $(INSTALLDIR)/predvar4
	rm $(INSTALLDIR)/predvar5
	rm $(INSTALLDIR)/pslave
	rm $(INSTALLDIR)/pstclean
	rm $(INSTALLDIR)/pstop
	rm $(INSTALLDIR)/pstopst
	rm $(INSTALLDIR)/punpause
	rm $(INSTALLDIR)/pwtadj1
	rm $(INSTALLDIR)/pwtadj2
	rm $(INSTALLDIR)/randobs
	rm $(INSTALLDIR)/randpar
	rm $(INSTALLDIR)/randpar1
	rm $(INSTALLDIR)/randpar2
	rm $(INSTALLDIR)/randpar3
	rm $(INSTALLDIR)/rdmulres
	rm $(INSTALLDIR)/regerr
	rm $(INSTALLDIR)/regpred
	rm $(INSTALLDIR)/resproc
	rm $(INSTALLDIR)/reswrit
	rm $(INSTALLDIR)/rrf2jco
	rm $(INSTALLDIR)/rrf2par
	rm $(INSTALLDIR)/rrf2parobs
	rm $(INSTALLDIR)/rrf2tab
	rm $(INSTALLDIR)/rrf2csv
	rm $(INSTALLDIR)/rrfappend
	rm $(INSTALLDIR)/rrfcalcpsi
	rm $(INSTALLDIR)/rrfcat
	rm $(INSTALLDIR)/rrfclean
	rm $(INSTALLDIR)/rrfcull
	rm $(INSTALLDIR)/scalepar
	rm $(INSTALLDIR)/sceua_p
	rm $(INSTALLDIR)/sensan
	rm $(INSTALLDIR)/senschek
	rm $(INSTALLDIR)/simcase
	rm $(INSTALLDIR)/ssstat
	rm $(INSTALLDIR)/subreg1
	rm $(INSTALLDIR)/supcalc
	rm $(INSTALLDIR)/supobspar
	rm $(INSTALLDIR)/supobspar1
	rm $(INSTALLDIR)/supobsprep
	rm $(INSTALLDIR)/svdaprep
	rm $(INSTALLDIR)/tempchek
	rm $(INSTALLDIR)/tpl2pst
	rm $(INSTALLDIR)/vec2pest
	rm $(INSTALLDIR)/veclog
	rm $(INSTALLDIR)/wtfactor
	rm $(INSTALLDIR)/wtsenout
