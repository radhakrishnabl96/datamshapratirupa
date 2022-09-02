C -- The following module stores data required for parallel operations.

       module ppestdata
          integer                       :: ipll
          integer                       :: itn
          integer                       :: irestart_pll
          integer                       :: nslave
          integer                       :: iwait
          integer                       :: repeatrun
          integer                       :: prm_mr = 30
          integer                       :: prm_mf = 31
          integer                       :: prm_wk = 32
          integer                       :: prm_nr = 33
          real, allocatable             :: runtime(:)
          double precision, allocatable :: parreg(:,:)
C          double precision, allocatable :: obsreg(:,:)
          character*200, allocatable    :: aslave(:)
          character*200, allocatable    :: asldir(:)
          character*200                 :: rmffile
          character*200                 :: rmrfile
          character*200                 :: rnrfile
          character*200                 :: restfile_pll
          character*1000                :: suppl_errmsg(2)
       end module ppestdata

       module modeldata
          character*200                 :: stopfile='pest.stp'
       end module modeldata

       program jactest

C -- Program JACTEST undertakes a number of model runs for an identified parameter
C    in order to test the integrity of derivatives wrt that parameter.

       use pestdata
       use ppestdata
       use modeldata
       use model_input_output_interface
       use parallel_run_manager

       implicit none

       logical lexist
       integer irestart,outunit
       integer ifail,nb_pst
       integer forgive_failure
       integer nruns,ipar,iipar,ipg,ict,i,ierr,irun,iobs,islave,
     +         itemp,j,nnn,istop,iflag,dontstopslaves,mstopslaves
       integer p1strt,pfile
       integer lw(10),rw(10)
       double precision fac,rinc,rmax
       double precision, allocatable :: pprun(:)
       character*4    aext,aswitch
       character*12   aapar,anum,aruns
       character*12   aprecis,decpoint
       character*12   aversion
       character*200  pestfile,afile,outfile
       character*300  instruction

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif
       call pgetcl (cline)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' JACTEST Version ',a,'. Watermark Numerical ',
     + 'Computing.')

       instruction=' '
       irestart=0
       ipll=0
       repeatrun=0
       itn=1
       outunit=20
       dontstopslaves=0
       mstopslaves=1
       forgive_failure=0

       call spacesub(cline)
       call linspl(ifail,6,lw,rw,cline)
       if(ifail.eq.0) go to 9400
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.eq.0) then
         aswitch=cline(lw(5):rw(5))
         call lowcas(aswitch)
         if((aswitch.ne.'/p ').and.(aswitch.ne.'/q ')) go to 9400
         ipll=1
         if(aswitch.eq.'/q ') mstopslaves=0
       end if
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0) go to 9400
       pestfile=cline(lw(1):rw(1))
       call remchar(pestfile,char(211))
#ifndef UNIX
       call lowcas(pestfile)
#endif
       nb_pst=len_trim(pestfile)
       nb_pst=nb_pst-3
       if(nb_pst.lt.1)nb_pst=1
       aext=pestfile(nb_pst:)
       call lowcas(aext)
       if(aext.ne.'.pst')then
         pestfile=trim(pestfile)//'.pst'
       end if
       inquire(file=pestfile,exist=lexist)
       if(.not.lexist)then
         call addquote(pestfile,afile)
         write(errmsg,50) trim(afile)
50       format('Cannot find PEST control file ',a,'.')
         go to 9890
       end if

       aapar=cline(lw(2):rw(2))
       call lowcas(aapar)

       anum=cline(lw(3):rw(3))
       call intread(ifail,anum,nruns)
       if(ifail.ne.0) then
         write(errmsg,60)
60       format('Cannot read number of increments from third term ',
     +   'of JACTEST command line.')
         go to 9890
       end if
       if(nruns.le.0)then
         write(errmsg,62)
62       format('Number of increments must be positive at third ',
     +   'term of JACTEST command line.')
         go to 9890
       end if

       outfile=cline(lw(4):rw(4))
       call remchar(outfile,char(211))
#ifndef UNIX
       call lowcas(outfile)
#endif

C -- All input data has now been acquired. The complete PEST input
C    dataset is now read.

       write(6,*)
       call addquote(pestfile,afile)
       write(6,235) trim(afile)
235    format(' - reading PEST control file ',a,'....')
       p1strt=0
       pfile=0
       call read_pest_data(ifail,irestart,p1strt,pfile,pestfile)
       if(ifail.ne.0) go to 9890
       if(nespar.eq.0)then
         write(errmsg,272) trim(afile)
272      format('There are no adjustable parameters cited in PEST ',
     +   'control file ',a,'.')
         go to 9890
       end if
       if(nobs.eq.0)then
         write(errmsg,273) trim(afile)
273      format('No observations are ',
     +   'cited in PEST control file ',a,'.')
         go to 9890
       end if
       if(nparfile.gt.0)then
         write(errmsg,2731)
2731     format('JACTEST cannot handle file parameters.')
         go to 9890
       end if
       if((nparsec.gt.0).or.(nequation.gt.0))then
         write(errmsg,2732)
2732     format('JACTEST cannot handle secondary parameters ',
     +   'and equations.')
         go to 9890
       end if
       write(6,275) trim(afile)
275    format(' - file ',a,' read ok.')

       nb_pst=len_trim(pestfile)
       nb_pst=nb_pst-3
       rmffile=pestfile(1:nb_pst)//'rmf'
       rmrfile=pestfile(1:nb_pst)//'rmr'
       rnrfile=pestfile(1:nb_pst)//'rcr'
       restfile_pll=pestfile(1:nb_pst)//'prf'

C -- If an existing stop file is present, it is deleted.

       inquire(file=stopfile,exist=lexist)
       if(lexist)then
         open(unit=outunit,file=stopfile,iostat=ierr)
         close(unit=outunit,status='delete',iostat=ierr)
       end if

C -- If Parallisation is activated an a run management record file is present, it is deleted.

       if(ipll.eq.1)then
         inquire(file=rmrfile,exist=lexist)
         if(lexist)then
           open(unit=outunit,file=rmrfile,iostat=ierr)
           close(unit=outunit,status='delete',iostat=ierr)
         end if
       end if

C -- If an existing output file is present, it is deleted.

       inquire(file=outfile,exist=lexist)
       if(lexist)then
         open(unit=outunit,file=outfile,iostat=ierr)
         close(unit=outunit,status='delete',iostat=ierr)
       end if

C -- Original parameter values are stored in the pval_o array for the later
C    use of tied parameters.

       do ipar=1,npar
         pval_o(ipar)=pval(ipar)
       end do

C -- We can delete some unused PEST memory.

       if(allocated(ixpr))       deallocate(ixpr,stat=ierr)
       if(allocated(ixpc))       deallocate(ixpc,stat=ierr)
       if(allocated(iestrans))   deallocate(iestrans,stat=ierr)

       if(allocated(parchglim))  deallocate(parchglim,stat=ierr)
       if(allocated(dercom))     deallocate(dercom,stat=ierr)

       if(allocated(obgnme))     deallocate(obgnme,stat=ierr)
       if(allocated(covind))     deallocate(covind,stat=ierr)
                                                                                              e
       if(allocated(covgp))      deallocate(covgp,stat=ierr)
       if(allocated(nobscov))    deallocate(nobscov,stat=ierr)
       if(allocated(covar))      deallocate(covar,stat=ierr)

       if(allocated(oval))       deallocate(oval,stat=ierr)
       if(allocated(owght))      deallocate(owght,stat=ierr)
       if(allocated(nobgnm))     deallocate(nobgnm,stat=ierr)
       if(allocated(modval_o))   deallocate(modval_o,stat=ierr)

       if(allocated(x))          deallocate(x,stat=ierr)
       if(allocated(ixc))        deallocate(ixc,stat=ierr)
       if(allocated(xc))         deallocate(xc,stat=ierr)

       if(allocated(di_par))     deallocate(di_par,stat=ierr)
       if(allocated(di_ies))     deallocate(di_ies,stat=ierr)
       if(allocated(di_obs))     deallocate(di_obs,stat=ierr)
       if(allocated(di_parent))  deallocate(di_parent,stat=ierr)

C -- The nominated parameter is identified in the PEST input dataset.

       do ipar=1,npar
         if(aapar.eq.apar(ipar)) go to 300
       end do
       write(errmsg,280) trim(aapar),trim(pestfile)
280    format('Parameter "',a,'" is not cited within PEST ',
     + 'control file ',a,'.')
       go to 9890
300    if(itrans(ipar).lt.0)then
         write(errmsg,310) trim(aapar),trim(pestfile)
310      format('Parameter "',a,'" is not adjustable in PEST ',
     +   'control file ',a,'.')
         go to 9890
       end if
       iipar=ipar

C -- The parallel PEST run management file is read.

       if(ipll.ne.0)then
         call addquote(rmffile,afile)
         write(6,276) trim(afile)
276      format(/,' - reading parallel run management file ',a,'...')
         call read_rmf(ifail)
         if(ifail.ne.0) go to 9890
         write(6,277) trim(afile)
277      format(' - file ',a,' read ok.')
       end if

C -- The parameter increment for the nominated parameter is now evaluated.

       ipg=ipargp(iipar)
       ict=inctyp(ipg)
       if(ict.eq.1)then
         if(pval(iipar).eq.0.0d0)then
           if(derinclb(ipg).eq.0.0d0)then
             write(errmsg,320) trim(aapar),trim(pestfile)
320          format('Zero derivative increment ',
     +       'computed for parameter "',a,'"; adjust derivative ',
     +       'increment settings in "parameter groups" section of ',
     +       'PEST control file ',a,'.')
             go to 9890
           end if
         end if
         fac=derinc(ipg)
         rinc=abs(fac*pval(iipar))
         if(rinc.lt.derinclb(ipg))rinc=derinclb(ipg)
       else if(ict.eq.2)then
         rinc=abs(derinc(ipg))
       else if(ict.eq.3)then
         rmax=0.0d0
         do ipar=1,npar
           if(ipargp(ipar).eq.ipg)then
             if(abs(pval(ipar)).gt.rmax)rmax=abs(pval(ipar))
           end if
           if(rmax.eq.0.0)then
             if(derinclb(ipg).eq.0.0d0)then
               write(errmsg,320) trim(aapar),trim(pestfile)
               go to 9890
             end if
           end if
           fac=derinc(ipg)
           rinc=fac*rmax
           if(rinc.lt.derinclb(ipg))rinc=derinclb(ipg)
         end do
       end if
       if(rinc.le.0.0)then
         write(errmsg,320) trim(aapar),trim(pestfile)
         go to 9890
       end if
       if((pval(iipar)+rinc*(nruns/2+1).gt.parubnd(iipar)).or.
     +    (pval(iipar)-rinc*(nruns/2+1).lt.parlbnd(iipar)))then
          write(errmsg,321) trim(aapar),trim(pestfile)
321       format('Parameter "',a,'" will move out of its bounds ',
     +    'in carrying out required computations. Adjust bounds ',
     +    'in PEST control file ',a,' or reduce number of ',
     +    'requested runs.')
          go to 9890
       end if

C -- Extra memory is allocated.

       nruns=nruns+1
       allocate(pprun(nruns),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- Files pertaining to model input/output communication are now read.

       if(precis.eq.0)then
         aprecis='single'
       else
         aprecis='double'
       end if
       if(nopnt.eq.0)then
         decpoint='point'
       else
         decpoint='nopoint'
       end if
       call mio_initialise(ifail,ntplfle,ninsfle,npar,nobs,
     + aprecis,decpoint)
       if(ifail.ne.0) go to 9000

       do i=1,ntplfle
         call mio_put_file(ifail,1,i,tempfle(i))
         if(ifail.ne.0) go to 9000
         call mio_put_file(ifail,2,i,infle(i))
         if(ifail.ne.0) go to 9000
       end do
       do i=1,ninsfle
         call mio_put_file(ifail,3,i,insfle(i))
         if(ifail.ne.0) go to 9000
         call mio_put_file(ifail,4,i,outfle(i))
         if(ifail.ne.0) go to 9000
       end do
       call mio_process_template_files(ifail,npar,apar)
       if(ifail.ne.0) go to 9000
       call mio_store_instruction_set(ifail)
       if(ifail.ne.0) go to 9000

C -- If necessary the parallelisation module is initialized.

       if(ipll.eq.1)then
         write(6,*)
         open(unit=prm_mr,file=rmrfile,action='write',iostat=ierr)
         if(ierr.ne.0)then
           call addquote(rmrfile,afile)
           write(errmsg,520) trim(afile)
520        format('Cannot open file ',a,' for output.')
           go to 9890
         end if
         open(unit=prm_nr,file=rnrfile,action='write',iostat=ierr)
         if(ierr.ne.0)then
           call addquote(rnrfile,afile)
           write(errmsg,521) trim(afile)
521        format('Cannot open file ',a,' for output.')
           go to 9890
         end if

         call prm_initialise(ifail,prm_mr,prm_mf,prm_wk,prm_nr,
     +   nslave,nruns,iwait,repeatrun)
         if(ifail.ne.0) go to 9895
         do islave=1,nslave
           itemp=ceiling(runtime(islave))
           call prm_slavedat(ifail,islave,itemp,aslave(islave),
     +     asldir(islave))
           if(ifail.ne.0) go to 9895
         end do

         call prm_slavetest(ifail)
         if(ifail.ne.0) go to 9895
C         allocate(parreg(npar,nruns),obsreg(nxrow,nruns),stat=ierr)
         allocate(parreg(npar,nruns),stat=ierr)
         if(ierr.ne.0) go to 9200
         do irun=1,nruns
           do ipar=1,npar
             parreg(ipar,irun)=pval(ipar)*scale(ipar)+offset(ipar)
           end do
         end do

       end if


C -- The JACTEST output file is opened.

       open(unit=outunit,file=outfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(outfile,afile)
         write(errmsg,130) trim(afile)
130      format('Cannot open file ',a,' for output.')
         go to 9890
       end if

C -- The X matrix is re-allocated to store model run data.

       allocate(x(nobs,nruns),stat=ierr)
       if(ierr.ne.0) go to 9200
       do irun=1,nruns
         x(1,irun)=1.1d301
       end do

C -- Model runs are now carried out.

       write(6,*)
       do ipar=1,npar
         if(ipar.ne.iipar)then
           if((scale(ipar).ne.1.0d0).or.(offset(ipar).ne.0.0d0))then
             pval(ipar)=pval(ipar)*scale(ipar)+offset(ipar)
           end if
         end if
       end do

       call writint(aruns,nruns)
       write(6,132) trim(aruns)
132    format(' - running model ',a,' times...')
       pval(iipar)=pval(iipar)-(nruns/2)*rinc
       do irun=1,nruns
         if(irun.gt.1)then
           pval(iipar)=pval(iipar)+rinc
         end if
         do ipar=1,npar
           if(-itrans(ipar).eq.iipar)then
             pval(ipar)=pval_o(ipar)/pval_o(iipar)*pval(iipar)
             if((scale(ipar).ne.1.0d0).or.(offset(ipar).ne.0.0d0))then
               pval(ipar)=pval(ipar)*scale(ipar)+offset(ipar)
             end if
           end if
         end do

         if(ipll.eq.0)then
           if((scale(iipar).ne.1.0d0).or.(offset(iipar).ne.0.0d0))then
             pval(iipar)=pval(iipar)*scale(iipar)+offset(iipar)
            end if
           call mio_delete_output_files(ifail)
           if(ifail.ne.0) go to 9000

           call mio_write_model_input_files(ifail,npar,apar,pval)
           if(ifail.ne.0) go to 9000
           if((scale(iipar).ne.1.0d0).or.(offset(iipar).ne.0.0d0))then
             pval(iipar)=(pval(iipar)-offset(iipar))/scale(iipar)
           end if
           pprun(irun)=pval(iipar)

#ifdef UNIX
#ifdef SYS_FUNCTION
           nnn=system(comlin(1))
#else
           call system(comlin(1))
#endif
#else
#ifdef LAHEY
#ifdef AT
           call system(comlin(1),.false.)
#else
           call system(comlin(1))
#endif
#else
           call system(comlin(1))
#endif
#endif

           call mio_read_model_output_files(ifail,nobs,aobs,x(:,irun),
     +     instruction)
           if(ifail.ne.0) go to 9000
           if(irun.eq.1)then
             write(6,256)
256          format(' - number of runs completed...')
             write(6,'(a)',advance='no') '   '
           end if
           write(6,257,advance='no') irun
257        format(i6)
           if((irun.eq.nruns).or.(mod(irun,12).eq.0))then
             write(6,*)
             write(6,'(a)',advance='no') '   '
           end if
           call stopress(istop)
           if(istop.ne.0) go to 424
         else
           parreg(iipar,irun)=pval(iipar)*scale(iipar)+offset(iipar)
           do ipar=1,npar
             if(-itrans(ipar).eq.iipar)then
               parreg(ipar,irun)=pval(ipar)
             end if
           end do
           pprun(irun)=pval(iipar)
         end if
       end do

       if(ipll.eq.1)then
         call prm_doruns(ifail,itn,npar,nobs,nruns,npar,nobs,parreg,
     +   x,apar,aobs,irestart_pll,restfile_pll,forgive_failure)
         if(ifail.lt.0) go to 424
         if(ifail.ne.0) go to 9000
       end if

       go to 425

424    write(6,423)
423    format(/,' User-initiated termination.')
       if(ipll.eq.1)then
         if(ifail.eq.-1)then
           dontstopslaves=1
         end if
       end if

425    continue
       call addquote(outfile,afile)
       write(6,430) trim(outfile)
430    format(/,' - writing file ',a,'....')
       iflag=0
       do irun=1,nruns
         if(x(1,irun).gt.1.0d300)then
           iflag=1
           go to 432
         end if
       end do
432    continue
       if(iflag.ne.0) nruns=irun-1
       write(outunit,440) 'param_value',(pprun(irun),irun=1,nruns)
       do iobs=1,nobs
         write(outunit,440) trim(aobs(iobs)),(x(iobs,irun),irun=1,nruns)
440      format(1x,a,t24,1000(1pg14.7,1x))
       end do
       close(unit=outunit)
       write(6,450) trim(outfile)
450    format(' - file ',a,' written ok.')

       if(ipll.eq.1)then
         if(dontstopslaves.eq.0)then
           if(mstopslaves.ne.0)then
             call prm_slavestop(ifail)
           end if
         end if
       end if

       go to 9900

9000   call mio_get_message_string(ifail,errmsg)
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
       if(instruction.ne.' ')then
         write(6,9010)
9010     format(' Instruction follows:-')
         write(6,9020) trim(instruction)
9020     format(1x,a)
       end if
       go to 9900

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9400   write(6,9410)
9410   format(/,' JACTEST is run using the command:',/)
       write(6,9420)
9420   format('     jactest pestfile parname n outfile [/p]',/)
       write(6,9430)
9430   format(' where',/)
       write(6,9450)
9450   format('     pestfile is the name of a PEST ',
     + 'control file,')
       write(6,9451)
9451   format('     parname  is the name of a parameter ',
     + 'featured in this file,')
       write(6,9452)
9452   format('     n        is the number of increments to ',
     + 'test,')
       write(6,9453)
9453   format('     outfile  is the name of the JACTEST output ',
     + 'file, and.')
       write(6,9454)
9454   format('     /p       is an optional parallelisation switch.')
       go to 9999

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)

9895   continue
       call prm_get_message_strings(ifail,i,errmsg,
     + suppl_errmsg)
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
       if(i.ne.1)then
         do j=1,i-1
           if(suppl_errmsg(j)(1:1).ne.' ')
     +        suppl_errmsg(j)=' '//trim(suppl_errmsg(j))
           write(6,'(a)') trim(suppl_errmsg(j))
         end do
       end if

9900   continue
       if(allocated(pprun))  deallocate(pprun,stat=ierr)
       if(ipll.eq.1)then
         if(allocated(parreg))  deallocate(parreg,stat=ierr)
         if(allocated(runtime)) deallocate(runtime,stat=ierr)
         if(allocated(aslave))  deallocate(aslave,stat=ierr)
         if(allocated(asldir))  deallocate(asldir,stat=ierr)
       end if
       call pest_data_deallocate(ifail)
       call mio_finalise(ifail)
       if(ipll.eq.1)then
         call prm_finalise(ifail)
       end if

9999   continue

       end


      subroutine writmess(iunit,amessage)

        implicit none

	integer iunit,jend,i,nblc,junit,leadblank,itake,j
        character*(*) amessage
	character (len=20) ablank

	ablank=' '
	itake=0
	j=0
	junit=iunit

        if(amessage.eq.' ')then
          write(junit,*)
          return
        end if
        write(junit,*)
	do i=1,min(20,len(amessage))
	  if(amessage(i:i).ne.' ')go to 21
20      end do
21	leadblank=i-1
	nblc=len_trim(amessage)
5       jend=j+78-itake
	if(jend.ge.nblc) go to 100
	do i=jend,j+1,-1
	if(amessage(i:i).eq.' ') then
	  if(itake.eq.0) then
	     write(junit,'(a)') amessage(j+1:i)
	     itake=2+leadblank
	  else
	     write(junit,'(a)') ablank(1:leadblank+2)//
     +       amessage(j+1:i)
	  end if
	  j=i
	  go to 5
	end if
	end do
	if(itake.eq.0)then
	  write(junit,'(a)') amessage(j+1:jend)
	  itake=2+leadblank
	else
	  write(junit,'(a)') ablank(1:leadblank+2)//
     +    amessage(j+1:jend)
	end if
	j=jend
	go to 5
100     jend=nblc
	if(itake.eq.0)then
	  write(junit,'(a)') amessage(j+1:jend)
	else
	  write(junit,'(a)') ablank(1:leadblank+2)//
     +    amessage(j+1:jend)
	end if
	return

      end



        subroutine intread(ifail,cline,itemp)

C -- Subroutine INTREAD reads a real number from a string.

        integer ifail
        integer itemp
        character*6 afmt
        character*(*) cline

        ifail=0
        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(cline)
        read(cline,afmt,err=100) itemp
        return

100     ifail=1
        return
        end



        subroutine read_rmf(ifail)

C -- Subroutine READ_RMF reads a parallel PEST run management file.

        use ppestdata
        use pestdata, only : errmsg,cline
        implicit none

        integer, intent(out)  :: ifail

        integer              :: ldosch,iunit,ierr,iline,ifltyp,
     +                          itemp,islave
        integer              :: nextunit
        integer              :: lw(5),rw(5)
        double precision     :: dtemp,dwait
        character*15         :: aline
        character*200        :: afile

        ifail=0
        ldosch=0
        repeatrun=0

        call addquote(rmffile,afile)
        iunit=nextunit()
        open(unit=iunit,file=rmffile,status='old',iostat=ierr)
        if(ierr.ne.0)then
          call addquote(rmffile,afile)
          write(errmsg,10) trim(afile)
10        format('Cannot open run management file ',a,'.')
          go to 9890
        end if
        iline=1
        read(iunit,'(a)',err=9050,end=9100) cline
        call lowcas(cline)
        cline=adjustl(cline)
        if(cline(1:3).ne.'prf')then
          write(errmsg,11) trim(afile)
11        format('First line of file ',a,' expected to be "prf".')
          go to 9890
        end if

        iline=2
        read(iunit,'(a)',err=9050,end=9100) cline
        call linspl(ifail,5,lw,rw,cline)
        if(ifail.eq.0)then
          call numrd(ifail,0,lw(5),rw(5),repeatrun,dtemp,cline)
          if(ifail.ne.0) go to 9050
          if(repeatrun.ne.0)repeatrun=1
        end if
        call linspl(ifail,4,lw,rw,cline)
        if(ifail.eq.0)then
          call numrd(ifail,0,lw(4),rw(4),ldosch,dtemp,cline)
          if(ifail.ne.0) go to 9050
        end if
        call linspl(ifail,3,lw,rw,cline)
        if(ifail.ne.0) go to 9050
        call numrd(ifail,0,lw(1),rw(1),nslave,dtemp,cline)
        if(ifail.ne.0) go to 9050
        if(nslave.le.0)then
          write(errmsg,18) trim(afile)
18        format('Number of agents not positive in parallel ',
     +    'run management file ',a,'.')
          go to 9890
        end if
        call  numrd(ifail,0,lw(2),rw(2),ifltyp,dtemp,cline)
        if(ifail.ne.0) go to 9050
        if(ifltyp.ne.0)then
          write(errmsg,20) trim(afile)
20        format('Second variable on line 2 of run management ',
     +    'file ',a,' (IFLTYPE) must be zero indicating short form ',
     +    'parallel run management file format.')
          go to 9890
        end if
        call numrd(ifail,1,lw(3),rw(3),itemp,dwait,cline)
        if(ifail.ne.0) go to 9050
        if(dwait.le.0.0d0) dwait=0.2d0
        iwait=nint(dwait*100.0d0)
        if(iwait.lt.1) iwait=1

C -- Memory is allocated.

        allocate(aslave(nslave),asldir(nslave),
     +  runtime(nslave),stat=ierr)
        if(ierr.ne.0) go to 9200

C -- The remainder of the file is read.

        do islave=1,nslave
          iline=iline+1
          read(iunit,'(a)',err=9050,end=9100) cline
          call spacesub(cline)
          call linspl(ifail,2,lw,rw,cline)
          if(ifail.ne.0) go to 9050
          aslave(islave)=cline(lw(1):rw(1))
          call remchar(aslave(islave),char(211))
          asldir(islave)=cline(lw(2):rw(2))
          call remchar(asldir(islave),char(211))
        end do

        iline=iline+1
        read(iunit,*,err=9050,end=9100)
     +  (runtime(islave),islave=1,nslave)
        do islave=1,nslave
          if(runtime(islave).le.0.0) then
            call writint(aline,iline)
            write(errmsg,50) trim(aline),trim(afile)
50          format('Non-positive run time provided on line ',a,' of ',
     +      'parallel run management file ',a,'.')
            go to 9890
          end if
        end do

        go to 9900

9050    call writint(aline,iline)
        write(errmsg,9060) trim(aline),trim(afile)
9060    format('Error reading line ',a,' of parallel run ',
     +  'management file ',a,'.')
        go to 9890
9100    write(errmsg,9110) trim(afile)
9110    format('Premature end encountered to parallel run ',
     +  'management file ',a,'.')
        go to 9890
9200    write(errmsg,9210)
9210    format('Cannot allocate sufficient memory to continue ',
     +  'execution.')
        go to 9890

9890    ifail=1

9900    continue
        close(unit=iunit,iostat=ierr)
        return

        end


       integer function nextunit()

! -- Function nextunit determines the lowest unit number available for
! -- opening.

       logical::lopen

       do nextunit=10,100
         inquire(unit=nextunit,opened=lopen)
         if(.not.lopen) return
       end do
       write(6,10)
10     format(' *** No more unit numbers to open files ***')
       stop

      end


        subroutine stopress(istop)

        use modeldata
        implicit none

        integer ipause,iunit,ierr,istop
        integer reason,nextunit

        ipause=0
10      continue

        iunit=nextunit()
        reason=0
        istop=0
        open(unit=iunit,file=stopfile,status='old',err=5)
        read(iunit,*,err=6,end=6) reason
6       continue
        if(reason.ne.3)then
          close(unit=iunit,status='delete',err=5)
        else
          close(unit=iunit,err=5)
        end if
5       continue

        if(reason.eq.3)then
          if(ipause.eq.0)then
            write(6,20)
20          format(/,' Program execution has been PAUSED...')
            ipause=1
          end if
          call pwait(100)
          go to 10
        else if((reason.eq.1).or.(reason.eq.2))then
          istop=reason
        else
          if(ipause.eq.1)then
            write(6,30)
30          format(' Program execution has been UNPAUSED.',/)
          end if
        end if
        return

        end

        subroutine pwait(nsec)

C -- Subroutine PWAIT hangs around for NSECS hundredths of a second.

        implicit none

        integer ddate(8),iticks,iticks1,nsec

        call date_and_time(values=ddate)
        iticks=ddate(5)*360000+ddate(6)*6000+ddate(7)*100+ddate(8)/10
10      call date_and_time(values=ddate)
        iticks1=ddate(5)*360000+ddate(6)*6000+ddate(7)*100+ddate(8)/10
        if(iticks1.lt.iticks) iticks1=iticks1+8640000
        if(iticks1.lt.iticks+nsec) go to 10

        return

        end


