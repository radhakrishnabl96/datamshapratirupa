C -- May get "file sharing not loaded". Not much I can do about it. Let me know.


       module subiter

         interface

           subroutine adj2all(xx)
             real, intent(in) :: xx(:)
           end subroutine adj2all

           subroutine priorcalc(outvals)
             double precision, intent(inout) :: outvals(:)
           end subroutine priorcalc

           subroutine objclc(outvals,phi)
             double precision, intent(in) :: outvals(:)
             real, intent(out)            :: phi
           end subroutine objclc

           end interface

       end module subiter


       module subiter1

         interface

           subroutine feval(ifail,xx,yy)
             integer, intent(out) :: ifail
             real, intent(in)     :: xx(:)
             real, intent(out)    :: yy
           end subroutine

           end interface

       end module subiter1


       module subiter2

         interface

           subroutine mul_feval(ifail,n,xx,yy)
             integer, intent(out)  :: ifail
             integer, intent(in)   :: n
             real, intent(inout)   :: xx(:,:)
             real, intent(inout)   :: yy(:)
           end subroutine mul_feval

           end interface

       end module subiter2


       module subiter3

         interface

           subroutine record_parameters(ifail,xx)
             integer, intent(out)  :: ifail
             real, intent(in)      :: xx(:)
           end subroutine record_parameters


           end interface

       end module subiter3




       module modeldata

          integer                       :: feval_call=0
          integer                       :: mul_feval_call=0
          integer                       :: mul_feval_model_call=0
          integer                       :: iopt
          character*12, allocatable     :: adjapar(:)
          double precision, allocatable :: res(:)
          character*500                 :: instruction
          character*200                 :: outfile_par
          character*200                 :: stopfile

          integer                       :: nipcoff
          integer, allocatable          :: iescoff(:)
          integer, allocatable          :: iobscoff(:)
          integer, allocatable          :: ies2ipar1(:)
          real, allocatable             :: sparlbnd(:)
          real, allocatable             :: sparubnd(:)
          real, allocatable             :: xx_lowest(:)
          double precision, allocatable :: rcoff(:)

          double precision, allocatable :: phid_contrib(:)
          real, allocatable             :: phi_contrib(:)

          real                          :: phi_lowest
          character*200                 :: restartfile_main

          integer                       :: slave_control
          character*200                 :: sce_slave_infile
          character*200                 :: sce_slave_tplfile
          character*200                 :: sce_slave_outfile
          character*200                 :: sce_slave_insfile

       end module modeldata

       module ppestdata
          integer                       :: ipll
          integer                       :: itn
          integer                       :: irestart_pll
          integer                       :: iswitch_pll=0
          integer                       :: nslave
          integer                       :: maxrun
          integer                       :: iwait
          integer                       :: repeatrun
          integer                       :: prm_mr = 30
          integer                       :: prm_mf = 31
          integer                       :: prm_wk = 32
          integer                       :: prm_nr = 33
          integer                       :: iunit_slave = 32
          integer                       :: npar_pll
          real, allocatable             :: runtime(:)
          double precision, allocatable :: parreg(:,:)
          double precision, allocatable :: obsreg(:,:)
          character*200, allocatable    :: aslave(:)
          character*200, allocatable    :: asldir(:)
          character*200                 :: rmffile
          character*200                 :: rmrfile
          character*200                 :: rnrfile
          character*200                 :: restfile_pll
          character*1000                :: suppl_errmsg(2)
          character*12,allocatable      :: apar_pll(:)
       end module ppestdata


       program sceua_p

C -- Program SCEUA_P is a global optimiser based on SCE.

       use subiter
       use subiter1
       use subiter2
       use subiter3
       use pestdata
       use ppestdata
       use modeldata
       use model_input_output_interface
       use parallel_run_manager
       use sceua_module
       implicit none

       logical lexist,ltemp

       integer ifail,jfail,jcov,itemp,info,ierr,iunit,itemp1
       integer i,j,iobs,ies,ipar,ibeg,iend
       integer iunit1,iunit2,itype,vardim,covdim
       integer nb_pst,jrestart,maxcovdim
       integer outunit1,outunit2
       integer islave,ipcoff
       integer irestart,resunit1,maxiter,userstop
       integer nextunit
       integer ngs,stop_reason,npt,npg
       integer mings,nspl,nps,nopt
       integer p1strt,pfile
       integer lw(5),rw(5)

       real rtemp
       real phi(1)

       real, allocatable :: sval(:,:)

       double precision dtemp
       double precision det(2)
       character*1 aa,ao,ayn
       character*3 acmd1,acmd2
       character*4 aext
       character*12 aprecis,decpoint
       character*12 aversion
       character*30 atemp,anum,atype
       character*200 pestfile,outfile1,afile,restartfile,
     +               scefile

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif
       call pgetcl (cline)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' SCEUA_P Version ',a,'. Watermark Numerical ',
     + 'Computing.')

       instruction=' '
       outunit1=50
       outunit2=51
       jrestart=0
       ipll=0
       itn=0
       irestart=1
       acmd1=' '
       acmd2=' '
       stopfile='pest.stp'
       maxiter=-999
       phi_lowest=huge(phi_lowest)
       sce_slave_infile='sce_slave.in'
       sce_slave_tplfile='sce_slave.tpl'
       sce_slave_outfile='sce_slave.out'
       sce_slave_insfile='sce_slave.ins'

       call spacesub(cline)
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 9400
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.eq.0)then
         acmd1=cline(lw(2):rw(2))
         call lowcas(acmd1)
         acmd2=cline(lw(3):rw(3))
         call lowcas(acmd2)
       else
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.eq.0)then
           acmd1=cline(lw(2):rw(2))
           call lowcas(acmd1)
         end if
       end if
       if(acmd1.eq.'/s') then
         if(acmd2.ne.' ') go to 9400
         ipll=-1
       else
         if(acmd1.ne.' ')then
           if((acmd1.ne.'/p ').and.(acmd1.ne.'/r ')) go to 9400
         end if
         if(acmd2.ne.' ')then
           if((acmd2.ne.'/p ').and.(acmd2.ne.'/r ')) go to 9400
         end if
         if((acmd1.eq.'/p ').or.(acmd2.eq.'/p '))then
           ipll=1
         end if
         if((acmd1.eq.'/r ').or.(acmd2.eq.'/r '))then
           irestart=2
         end if
         if(acmd1.ne.' ')then
           if(acmd1.eq.acmd2) go to 9400
         end if
       end if
       call linspl(ifail,1,lw,rw,cline)
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

C -- The names of other files are now formed.

       nb_pst=len_trim(pestfile)
       nb_pst=nb_pst-3
       outfile1=pestfile(1:nb_pst)//'rec'
       outfile_par=pestfile(1:nb_pst)//'par'
       rmffile=pestfile(1:nb_pst)//'rmf'
       rmrfile=pestfile(1:nb_pst)//'rmr'
       rnrfile=pestfile(1:nb_pst)//'rcr'
       restfile_pll=pestfile(1:nb_pst)//'prf'
       restartfile=pestfile(1:nb_pst)//'rst'
       restartfile_main=pestfile(1:nb_pst)//'jst'
       scefile='sceout.dat'

C -- All input data has now been acquired. The complete PEST input
C    dataset is now read.

       write(6,*)
       call addquote(pestfile,afile)
       write(6,235) trim(afile)
235    format(' - reading PEST control file ',a,'....')
       p1strt=0
       pfile=0
       call read_pest_data(ifail,jrestart,p1strt,pfile,pestfile)
       if(ifail.ne.0) go to 9890
       if(pestmode.ne.1)then
         write(errmsg,250) trim(afile)
250      format('SCEUA_P requires that PEST be run in ',
     +   'parameter estimation mode in PEST control file ',a,'.')
         go to 9890
       end if
       if(nespar.eq.0)then
         write(errmsg,272) trim(afile)
272      format('There are no adjustable parameters cited in PEST ',
     +   'control file ',a,'.')
         go to 9890
       end if
       if(nxrow.eq.0)then
         write(errmsg,273) trim(afile)
273      format('No observations or prior information items are ',
     +   'cited in PEST control file ',a,'.')
         go to 9890
       end if
       if(nparfile.gt.0)then
         write(errmsg,2731)
2731     format('SCEUA_P cannot handle file parameters.')
         go to 9890
       end if
       if((nparsec.gt.0).or.(nequation.gt.0))then
         write(errmsg,2732)
2732     format('SCEUA_P cannot handle secondary parameters ',
     +   'and equations.')
         go to 9890
       end if
       write(6,275) trim(afile)
275    format(' - file ',a,' read ok.')

C -- Original parameter values are stored in the pval_o array for the later
C    use of tied parameters.

       do ipar=1,npar
         pval_o(ipar)=pval(ipar)
       end do

C -- Special measures are taken to handle prior information.

       if(nprior.ne.0)then
         nipcoff=0
         do ies=1,nespar
           do iobs=nobs+1,nxrow
             if(maxcompdim.le.1)then
               if(x(iobs,ies).ne.0.0d0) nipcoff=nipcoff+1
             else
               call get_value(ncompdim,xc,ixc,dtemp,iobs,ies)
               if(dtemp.ne.0.0d0) nipcoff=nipcoff+1
             end if
           end do
         end do
         allocate(rcoff(nipcoff),iescoff(nipcoff),iobscoff(nipcoff),
     +   ies2ipar1(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         ipcoff=0
         do ies=1,nespar
           do iobs=nobs+1,nxrow
             if(maxcompdim.le.1)then
               dtemp=x(iobs,ies)
             else
               call get_value(ncompdim,xc,ixc,dtemp,iobs,ies)
             end if
             if(dtemp.ne.0.0d0)then
               ipcoff=ipcoff+1
               iescoff(ipcoff)=ies
               iobscoff(ipcoff)=iobs
               rcoff(ipcoff)=dtemp
             end if
           end do
         end do
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             ies=ies+1
             ies2ipar1(ies)=ipar
           end if
         end do
       end if

C -- Memory is allocated for the "contributions to phi" vector.

       allocate(phid_contrib(nobsgp),phi_contrib(nobsgp),stat=ierr)
       if(ierr.ne.0) go to 9200
       phid_contrib=0.0d0
       phi_contrib=0.0

C -- We can delete some unused PEST memory.

       if(allocated(ixpr))       deallocate(ixpr,stat=ierr)
       if(allocated(ixpc))       deallocate(ixpc,stat=ierr)
       if(allocated(iestrans))   deallocate(iestrans,stat=ierr)

       if(allocated(pargnme))    deallocate(pargnme,stat=ierr)
       if(allocated(inctyp))     deallocate(inctyp,stat=ierr)
       if(allocated(derinc))     deallocate(derinc,stat=ierr)
       if(allocated(derinclb))   deallocate(derinclb,stat=ierr)
       if(allocated(derincmul))  deallocate(derincmul,stat=ierr)
       if(allocated(forcen))     deallocate(forcen,stat=ierr)
       if(allocated(dermthd))    deallocate(dermthd,stat=ierr)

       if(allocated(parchglim))  deallocate(parchglim,stat=ierr)
       if(allocated(ipargp))     deallocate(ipargp,stat=ierr)
       if(allocated(dercom))     deallocate(dercom,stat=ierr)

       if(allocated(x))          deallocate(x,stat=ierr)
       if(allocated(ixc))        deallocate(ixc,stat=ierr)
       if(allocated(xc))         deallocate(xc,stat=ierr)

       if(allocated(di_par))     deallocate(di_par,stat=ierr)
       if(allocated(di_ies))     deallocate(di_ies,stat=ierr)
       if(allocated(di_obs))     deallocate(di_obs,stat=ierr)
       if(allocated(di_parent))  deallocate(di_parent,stat=ierr)

       if(allocated(phiabandon)) deallocate(phiabandon,stat=ierr)

C -- If appropriate, the parallel run management file is read.

       if(ipll.gt.0)then
         call addquote(rmffile,afile)
         write(6,276) trim(afile)
276      format(/,' - reading parallel run management file ',a,'...')
         call read_rmf(ifail)
         if(ifail.ne.0) go to 9890
         write(6,277) trim(afile)
277      format(' - file ',a,' read ok.')
       end if

C --   If any covariance matrices were in the original PEST dataset these
C      are now inverted.

       maxcovdim=0
       if(numcov.ne.0)then
         do jcov=1,numcov
           itemp=covar(jcov)%ndim
           if(itemp.gt.maxcovdim) maxcovdim=itemp
           call dpofa(covar(jcov)%dval,itemp,itemp,info)
           if(info.ne.0)then
             itemp=covgp(jcov)
             write(errmsg,310) trim(obgnme(itemp))
310          format('Cannot invert covariance matrix supplied for ',
     +       'observation group "',a,'".')
             go to 9890
           end if
           info=1
           call dpodi(covar(jcov)%dval,itemp,itemp,det,info)
           do i=1,itemp-1
             do j=i+1,itemp
               covar(jcov)%dval(j,i)=covar(jcov)%dval(i,j)
             end do
           end do
           itemp=covgp(jcov)
           do iobs=1,nxrow
             if(nobgnm(iobs).eq.itemp)owght(iobs)=0.0d0
           end do
         end do
         allocate(res(maxcovdim),stat=ierr)
         if(ierr.ne.0) go to 9200
       end if

C -- If this is the master of a parellised run, variables for model run
C    communication and parallelisation are now computed.


C -- The MIO module is prepared.

       if(ipll.ge.0)then
         write(6,311)
311      format(/,' - processing input data and preparing for ',
     +   'SCEUA run...')
       end if

C -- If an existing run record file is present, it is deleted unless a restart is being made.

       if(irestart.ne.2)then
         inquire(file=outfile1,exist=lexist)
         if(lexist)then
           open(unit=outunit1,file=outfile1,iostat=ierr)
           close(unit=outunit1,status='delete',iostat=ierr)
         end if
       end if

C -- If an existing SCE-generated file is present, it is deleted unless a restart is being made.

       if(irestart.ne.2)then
         inquire(file=scefile,exist=lexist)
         if(lexist)then
           open(unit=outunit1,file=scefile,iostat=ierr)
           close(unit=outunit1,status='delete',iostat=ierr)
         end if
       end if

C -- If an existing restart file is present, it it deleted unless a restart is being made.

       if(irestart.ne.2)then
         inquire(file=restartfile,exist=lexist)
         if(lexist)then
           open(unit=outunit1,file=restartfile,form='unformatted',
     +     iostat=ierr)
           close(unit=outunit1,status='delete',iostat=ierr)
         end if
         inquire(file=restartfile_main,exist=lexist)
         if(lexist)then
           open(unit=outunit1,file=restartfile_main,form='unformatted',
     +     iostat=ierr)
           close(unit=outunit1,status='delete',iostat=ierr)
         end if
       end if

C -- If an existing stop file is present, it is deleted.

       inquire(file=stopfile,exist=lexist)
       if(lexist)then
         open(unit=outunit1,file=stopfile,iostat=ierr)
         close(unit=outunit1,status='delete',iostat=ierr)
       end if

C -- If Parallisation is activated an a run management record file is present, it is deleted.

       if(ipll.eq.1)then
         inquire(file=rmrfile,exist=lexist)
         if(lexist)then
           open(unit=outunit1,file=rmrfile,iostat=ierr)
           close(unit=outunit1,status='delete',iostat=ierr)
         end if
         inquire(file=rnrfile,exist=lexist)
         if(lexist)then
           open(unit=outunit1,file=rnrfile,iostat=ierr)
           close(unit=outunit1,status='delete',iostat=ierr)
         end if
       end if

C -- SCEUA output files are opened.

       if(irestart.eq.2)then
         open(unit=outunit1,file=outfile1,action='write',
     +        position='append',iostat=ierr)
       else
         open(unit=outunit1,file=outfile1,action='write',iostat=ierr)
       end if
       if(ierr.ne.0)then
         call addquote(outfile1,afile)
         write(errmsg,130) trim(afile)
130      format('Cannot open file ',a,' for output.')
         go to 9890
       end if
       if(irestart.eq.2)then
         write(outunit1,131)
131      format(/,/,t35,'RESTART',/)
       end if

C -- The SCE module is initialised.

       call sceua_init(ifail,nespar,outunit1,outunit2)
       if(ifail.ne.0) go to 9100

C -- Memory is allocated.

       allocate(sval(nespar,1),sparlbnd(nespar),
     + sparubnd(nespar),xx_lowest(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           if(itrans(ipar).eq.0)then
             sval(ies,1)=pval(ipar)
             sparlbnd(ies)=parlbnd(ipar)
             sparubnd(ies)=parubnd(ipar)
           else
             if(pval(ipar).le.0.0d0)then
               write(errmsg,129)
129            format('Log transformed parameter supplied as zero ',
     +         'or negative in PEST control file.')
               go to 9890
             end if
             sval(ies,1)=log10(pval(ipar))
             sparlbnd(ies)=log10(parlbnd(ipar))
             sparubnd(ies)=log10(parubnd(ipar))
           end if
         end if
       end do
       call sceua_set_problem_definition(ifail,sval(:,1),
     + sparlbnd,sparubnd)
       if(ifail.ne.0) go to 9100

C -- SCEUA is informed of the number of observation groups.

       call sceua_set_integer_options(ifail,'ncontrib',nobsgp)

       if(irestart.eq.2)then
         call sceua_set_integer_options(ifail,'irestart',irestart)
         call sceua_set_character_options(ifail,'restartfile',
     +   restartfile)
         go to 512
       end if

C -- SCEUA is told to leave behind a restart file.

       call sceua_set_integer_options(ifail,'irestart',irestart)
       call sceua_set_character_options(ifail,'restartfile',restartfile)

       if(ipll.lt.0) go to 512

C -- If necessary, the mode of SCEUA operation is now established.

       iopt=0
       if((nobs.eq.1).and.(nprior.eq.0))then
         write(6,*)
         write(6,405)
405      format(' PEST control file has only one observation.',/)
404      write(6,406,advance='no')
406      format('   Minimize model equiv or match to observation? ',
     +   '[i/a] (<Enter> if "i"): ')
         read(5,'(a)') ao
         if((ao.eq.'I').or.(ao.eq.'i').or.(ao.eq.' '))then
           iopt=1
           call sceua_set_integer_options(ifail,'iopt',iopt)
         else if((ao.eq.'A').or.(ao.eq.'a'))then
           iopt=0
         else
           go to 404
         end if
       end if

C -- The user is given the option of using default options or of choosing
C    new ones.

       write(6,*)
       write(6,410)
410    format(' Enter values for following SCE control ',
     + 'variables:-',/)

413    write(6,414,advance='no')
414    format('   Initial number of complexes: ')
       read(5,'(a)') anum
       if(anum.eq.' ') go to 413
       call intread(ifail,anum,itemp)
       if(ifail.ne.0) go to 413
       if(itemp.le.0) go to 413
       call sceua_set_integer_options(ifail,'ngs',itemp)
       call sceua_set_integer_options(ifail,'mings',itemp)
       ngs=itemp

       call sceua_get_integer_options(ifail,'mings',itemp)
       call writint(atemp,itemp)
435    write(6,436,advance='no') trim(atemp)
436    format('   Minimum number of complexes (<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call intread(ifail,anum,itemp)
         if(ifail.ne.0) go to 435
         if(itemp.le.0) go to 435
         if(itemp.gt.ngs) go to 435
         call sceua_set_integer_options(ifail,'mings',itemp)
       end if

       call sceua_get_integer_options(ifail,'npg',itemp)
       call writint(atemp,itemp)
415    write(6,420,advance='no') trim(atemp)
420    format('   Parameter sets in each complex (<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call intread(ifail,anum,itemp)
         if(ifail.ne.0) go to 415
         if(itemp.le.1) go to 415
         call sceua_set_integer_options(ifail,'npg',itemp)
       end if
       npg=itemp
       npt=ngs*npg
       maxrun=npt

       call sceua_get_integer_options(ifail,'nps',itemp)
       call writint(atemp,itemp)
425    write(6,426,advance='no') trim(atemp)
426    format('   Parameter sets per sub-complex (<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call intread(ifail,anum,itemp)
         if(ifail.ne.0) go to 425
         if(itemp.le.1) go to 425
         if(itemp.gt.npg) go to 425
         call sceua_set_integer_options(ifail,'nps',itemp)
       end if

       call sceua_get_integer_options(ifail,'iniflg',itemp)
       if(itemp.eq.0)then
         aa='n'
       else
         aa='y'
       end if
431    write(6,432,advance='no') trim(aa)
432    format('   Include initial parameter set in population?  [y/n] ',
     + '(<Enter> if ',a,'): ')
       read(5,'(a)') ayn
       if(ayn.ne.' ')then
         if((ayn.eq.'Y').or.(ayn.eq.'y'))then
           itemp=1
         else if((ayn.eq.'N').or.(ayn.eq.'n'))then
           itemp=0
         else
           go to 431
         end if
         call sceua_set_integer_options(ifail,'iniflg',itemp)
       end if

       call sceua_get_integer_options(ifail,'nspl',itemp)
       call writint(atemp,itemp)
433    write(6,434,advance='no') trim(atemp)
434    format('   Evolution steps before shuffling ',
     + '(<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call intread(ifail,anum,itemp)
         if(ifail.ne.0) go to 433
         if(itemp.le.0) go to 433
         call sceua_set_integer_options(ifail,'nspl',itemp)
       end if

       call sceua_get_integer_options(ifail,'iseed',itemp)
       call writint(atemp,itemp)
422    write(6,423,advance='no') trim(atemp)
423    format('   Random number seed (<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call intread(ifail,anum,itemp)
         if(ifail.ne.0) go to 422
C         if(itemp.le.0) go to 422
         call sceua_set_integer_options(ifail,'iseed',itemp)
       end if

       call sceua_get_integer_options(ifail,'iprint',itemp)
       if(itemp.eq.0)then
         aa='n'
       else
         aa='v'
       end if
428    write(6,429,advance='no') trim(aa)
429    format('   Verbose or non-verbose SCEUA printout?  [v/n]: ',
     + '(<Enter> if "',a,'"): ')
       read(5,'(a)') ayn
       if(ayn.ne.' ') then
         if((ayn.eq.'v').or.(ayn.eq.'V'))then
           itemp=1
         else if((ayn.eq.'N').or.(ayn.eq.'n'))then
           itemp=0
         else
           go to 428
         end if
         call sceua_set_integer_options(ifail,'iprint',itemp)
       end if

       write(6,463)
463    format(/,' Termination Criteria:-')
       call sceua_get_real_options(ifail,'pcento',rtemp)
       write(atemp,'(1pg11.4)') rtemp
       atemp=adjustl(atemp)
465    write(6,470,advance='no') trim(atemp)
470    format('   Max relative obj fn change over N itns ',
     + '(<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call realread(ifail,anum,rtemp)
         if(ifail.ne.0) go to 465
         if(rtemp.le.0.0) go to 465
         call sceua_set_real_options(ifail,'pcento',rtemp)
       end if

       call sceua_get_integer_options(ifail,'kstop',itemp)
       call writint(atemp,itemp)
475    write(6,480,advance='no') trim(atemp)
480    format('   No of itns over which this applies ',
     + '(<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call intread(ifail,anum,itemp)
         if(ifail.ne.0) go to 475
         if(itemp.le.0) go to 475
         if(itemp.ge.20)then
           write(6,481)
481        format('   *** Must be greater than 20 - try again ***')
           go to 475
         end if
         call sceua_set_integer_options(ifail,'kstop',itemp)
       end if

       call sceua_get_integer_options(ifail,'maxn',itemp)
       maxiter=itemp
       call writint(atemp,itemp)
505    write(6,510,advance='no') trim(atemp)
510    format('   Maximum number of model runs ',
     + '(<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call intread(ifail,anum,itemp)
         if(ifail.ne.0) go to 505
         if(itemp.lt.0) go to 505
         call sceua_set_integer_options(ifail,'maxn',itemp)
         if(itemp.eq.0)then
           call sceua_set_integer_options(ifail,'iniflg',1)
         end if
         maxiter=itemp
       end if
       if((maxiter.eq.0).and.(ipll.ne.0))then
         write(errmsg,511)
511      format('SCEUA_P must not be run in parallel mode when only ',
     +   'one model run is requested.')
         go to 9890
       end if

512    continue

! -- If necessary the restart file is opened and read.

       if(irestart.eq.2)then
         resunit1=nextunit()
         itemp=max(8,nobsgp)*8
         if(itemp.lt.1000) itemp=1000
         open(unit=resunit1,file=restartfile_main,form='unformatted',
     +   recl=itemp,status='old',iostat=ierr)
         if(ierr.ne.0)then
           write(errmsg,513) trim(restartfile_main)
513        format('Cannot open file ',a,' to read restart data.')
           go to 9890
         end if
         read(resunit1,err=9350,end=9350) iopt
         read(resunit1,err=9350,end=9350)
     +   nopt,ngs,npg,nps,nspl,mings,npt
         read(resunit1,err=9350,end=9350) feval_call,mul_feval_call,
     +   mul_feval_model_call
         read(resunit1,err=9350,end=9350) phi_lowest
         read(resunit1,err=9350,end=9350) phi_contrib
         read(resunit1,err=9350,end=9350) phid_contrib
         close(unit=resunit1)
         maxrun=npt
         call sceua_set_integer_options(ifail,'ngs',ngs)
         call sceua_set_integer_options(ifail,'npg',npg)
         call sceua_set_integer_options(ifail,'nps',nps)
         call sceua_set_integer_options(ifail,'nspl',nspl)
         call sceua_set_integer_options(ifail,'mings',mings)
       end if

C -- Variables which control this SCEUA run are recorded to the run record file.

       if(irestart.ne.2)then
         if(ipll.ge.0)then
           call write_sceua_data_header(outunit1,pestfile,nespar)
         end if
       end if

       if(ipll.gt.0)then
         itemp=max(2,nespar)
         allocate(apar_pll(itemp),stat=ierr)
         if(ierr.ne.0) go to 9200
         ntplfle=1
         ninsfle=1
         npar_pll=nespar
         nobs=nobsgp+1
         nxrow=nobs
         nprior=0
         deallocate(oval,modval_o,aobs,stat=ierr)
         if(ierr.ne.0) go to 9200
         allocate(aobs(nobs),stat=ierr)
         if(ierr.ne.0) go to 9200
         tempfle(1)=sce_slave_tplfile
         infle(1)=sce_slave_infile
         insfle(1)=sce_slave_insfile
         outfle(1)=sce_slave_outfile
         iunit=nextunit()
         do i=1,nespar
           call writint(anum,i)
           anum=adjustl(anum)
           apar_pll(i)='x'//trim(anum)
         end do
         open(unit=iunit,file=tempfle(1))
         write(iunit,720)
720      format('ptf $')
         write(iunit,721) 1
721      format(i5)
         write(iunit,721) iopt
         do i=1,npar_pll
           write(iunit,730) trim(apar_pll(i))
730        format('$',a,t15,'$')
         end do
         close(unit=iunit)
         aobs(1)='objfn'
         do i=1,nobsgp
           call writint(anum,i)
           anum=adjustl(anum)
           aobs(i+1)='contrib_'//trim(anum)
         end do
         open(unit=iunit,file=insfle(1))
         write(iunit,740)
740      format('pif $')
         do i=1,nobs
           write(iunit,750) trim(aobs(i))
750        format('l1 [',a,']1:30')
         end do
         close(unit=iunit)
       end if

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
       if(ipll.eq.1)then
         itemp=npar_pll
       else
         itemp=npar
       end if
       call mio_initialise(ifail,ntplfle,ninsfle,itemp,nobs,
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
       if(ipll.eq.1)then
         call mio_process_template_files(ifail,npar_pll,apar_pll)
       else
         call mio_process_template_files(ifail,npar,apar)
       end if
       if(ifail.ne.0) go to 9000
       call mio_store_instruction_set(ifail)
       if(ifail.ne.0) go to 9000

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
     +   nslave,maxrun,iwait,repeatrun)
         if(ifail.ne.0) go to 9895
         do islave=1,nslave
           itemp=ceiling(runtime(islave))
           call prm_slavedat(ifail,islave,itemp,aslave(islave),
     +     asldir(islave))
           if(ifail.ne.0) go to 9895
         end do
         call prm_slavetest(ifail)
         if(ifail.ne.0) go to 9895
         allocate(parreg(npar_pll,maxrun),obsreg(nxrow,maxrun),
     +   stat=ierr)
         if(ierr.ne.0) go to 9200
       end if

C -- The slave situation is handled.

       if(ipll.eq.-1)then
         iunit=iunit_slave
         open(unit=iunit,file=sce_slave_infile,status='old',
     +   iostat=ierr)
         if(ierr.ne.0)then
           write(errmsg,760) trim(sce_slave_infile)
760        format('Cannot open file ',a,'.')
           go to 9890
         end if
         read(iunit,*,iostat=ierr) slave_control
         if(ierr.ne.0) go to 9500
         read(iunit,*,iostat=ierr) iopt
         if(slave_control.eq.1)then
           do i=1,nespar
             read(iunit,*,err=9500) xx_lowest(i)
           end do
           close(unit=iunit)
           write(6,770)
770        format(/,' - SCE agent running the model once...')
           call feval(ifail,xx_lowest,rtemp)
           if(ifail.ne.0) go to 9000
           open(unit=iunit,file=sce_slave_outfile)
           write(iunit,780) phi_lowest
780        format(1x,1pg14.7)
           do i=1,nobsgp
             write(iunit,780) phi_contrib(i)
           end do
           close(unit=iunit)
           write(6,790)
790        format(' - model run completed.')
           go to 9900
         else
           write(6,135)
135        format(/,' - evolving one parameter complex...')
           go to 136
         end if
       end if

C -- SCEUA is run.

       if(irestart.ne.2)then
         write(6,132)
132      format(' - input data processed ok.')
         write(6,133)
133      format(/,' - commencing SCE optimisation process ...')
       else
         write(6,134)
134      format(/,' - re-commencing SCE optimisation process ...')
       end if

C -- First inform SCEUA if it is running in parallel mode.

136    continue
       if(ipll.ne.0)then
         call sceua_set_integer_options(ifail,'ipll',ipll)
         call sceua_set_integer_options(ifail,'iopt',iopt)
         call sceua_set_integer_options(ifail,'iunit_slave',iunit_slave)
         call sceua_set_character_options(ifail,'sce_slave_tplfile',
     +   sce_slave_tplfile)
         call sceua_set_character_options(ifail,'sce_slave_outfile',
     +   sce_slave_outfile)
       end if

       call sceua_run(ifail)
       if(ifail.eq.11) go to 9000
       if(ifail.lt.0) go to 9300
       if(ifail.ne.0) go to 9100

C -- The reason for the stoppage is now acquired.

       if(ipll.lt.0) then
         write(6,182)
182      format(' - complex evolution complete.')
         go to 9900
       end if
       call sceua_get_stop_info(stop_reason)
       write(6,180)
180    format(/,' Completion of SCE process.')
       write(outunit1,181)
181    format(/,/,t25,'COMPLETION OF SCE PROCESS')

       write(6,185)
       write(outunit1,185)
185    format(/,' Reason for termination of SCE process:-')
       if(stop_reason.eq.1)then
         write(cline,186)
186      format('User-supplied maximum number of model runs has been ',
     +   'exceeded.')
       else if (stop_reason.eq.2)then
         write(cline,187)
187      format('Objective function change per shuffling loop is ',
     +   'less than user-supplied threshold.')
       else if (stop_reason.eq.3)then
         write(cline,188)
188      format('Parameter changes between shuffling loops too small.')
       else if (stop_reason.eq.4)then
         write(cline,191)
191      format('User requested only one model run using initial ',
     +   'parameters.')
       else if (stop_reason.eq.-2)then
         write(cline,189)
189      format('User-initiated termination of execution.')
       end if
       cline=' '//adjustl(cline)
       call writmess(6,cline)
       call writmess(outunit1,cline)

       call adj2all(xx_lowest)
       if(maxiter.eq.0)then
         write(outunit1,221)
221      format(/,' Initial parameter values ----->')
       else
         write(outunit1,220)
220      format(/,' Optimised parameter values ----->')
       end if
       write(outunit1,230)
230    format(/,t5,'Name',t25,'Value')
       do ipar=1,npar
         write(outunit1,240) trim(apar(ipar)),pval(ipar)
240      format(t5,a,t25,1pg13.6)
       end do

C -- One final run is now undertaken on the basis of optimised values.

        if(ipll.ge.0)then
          if(maxiter.ne.0)then
            write(6,242)
242         format(/,' Running model one last time using ',
     +         'optimised parameters...')
            if(ipll.ne.0)then
               write(6,238)
               write(outunit1,238)
238            format(/,' Final run cannot be undertaken when SCEUA_P ',
     +         'run in parallel mode.')
               write(6,237)
               write(outunit1,237)
237            format(' Use PARREP utility and then run SCEUA_P with ',
     +         'MAXITER equal to zero.')
               call prm_slavestop(ifail)
            else
              call feval(ifail,xx_lowest,rtemp)
              write(6,239)
239           format(' - model run completed.')
            end if
          end if
        end if

C -- If runs are parallelised the slaves are now stopped.

       if(maxiter.ne.0)then
         write(outunit1,*)
         write(6,*)
         write(6,690) trim(outfile1)
690      format(' See file ',a,' for run record.')
         write(outunit1,700) trim(outfile_par)
         write(6,700) trim(outfile_par)
700      format(' See file ',a,' for optimised parameter values in ',
     +   'parameter value ',/,'     file format.')
         write(6,710) trim(scefile)
         write(outunit1,710) trim(scefile)
710      format(' See file ',a,' for SCEUA-recorded optimisation ',
     +   'record.')
       end if

       go to 9900

9000   continue
       if(ipll.le.0)then
         call mio_get_message_string(ifail,errmsg)
         errmsg=' '//trim(errmsg)
         call writmess(6,errmsg)
         call writmess(outunit1,errmsg)
         if(instruction.ne.' ')then
           write(6,9010)
           write(outunit1,9010)
9010       format(' Instruction follows:-')
           write(6,9020) trim(instruction)
           write(outunit1,9020) trim(instruction)
9020       format(1x,a)
         end if
         go to 9900
       else
         go to 9895
       end if

9100   call sceua_get_message_string(ifail,errmsg)
       go to 9890

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9300   write(errmsg,9310)
9310   format('Stopping execution at user''s request.')
       go to 9890

9400    write(6,9410)
9410    format(/,' SCEUA_P is run using the command:',/)
        write(6,9420)
9420    format('     sceua_p pestfile [/p] [/r]',/)
        write(6,9421)
9421    format(' or',/)
        write(6,9422)
9422    format('     sceua_p pestfile /s',/)
        write(6,9430)
9430    format(' where',/)
        write(6,9450)
9450    format('     pestfile is the name of a PEST ',
     +  'control file,')
        write(6,9510)
9510    format('     /p       is an optional parallelisation switch,')
        write(6,9520)
9520    format('     /r       is an optional restart switch, and')
        write(6,9530)
9530    format('     /s       must be used if SCEUA_P is run by ',
     +  'PAGENT.')
        go to 9999

9350   write(errmsg,9360) trim(restartfile_main)
9360   format('Error encountered in reading restart file ',a,'.')
       go to 9890

9500   write(errmsg,9501) trim(sce_slave_infile)
9501   format('Error reading data from file ',a,'.')
       go to 9890

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
       go to 9900

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
       call pest_data_deallocate(ifail)
       call sceua_fin(ifail)
       call mio_finalise(ifail)
       if(ipll.gt.0)then
         call prm_finalise(ifail)
         if(allocated(parreg)) deallocate(parreg,stat=ierr)
         if(allocated(obsreg)) deallocate(obsreg,stat=ierr)
         if(allocated(apar_pll)) deallocate(apar_pll,stat=ierr)
       end if
       if(allocated(sval))         deallocate(sval,stat=ierr)
       if(allocated(sparlbnd))     deallocate(sparlbnd,stat=ierr)
       if(allocated(sparubnd))     deallocate(sparubnd,stat=ierr)
       if(allocated(adjapar))      deallocate(adjapar,stat=ierr)
       if(allocated(res))          deallocate(res,stat=ierr)
       if(allocated(iescoff))      deallocate(iescoff,stat=ierr)
       if(allocated(iobscoff))     deallocate(iobscoff,stat=ierr)
       if(allocated(rcoff))        deallocate(rcoff,stat=ierr)
       if(allocated(ies2ipar1))    deallocate(ies2ipar1,stat=ierr)
       if(allocated(phid_contrib)) deallocate(phid_contrib,stat=ierr)
       if(allocated(phi_contrib))  deallocate(phi_contrib,stat=ierr)
       if(allocated(xx_lowest))    deallocate(xx_lowest,stat=ierr)

       if(allocated(runtime))      deallocate(runtime,stat=ierr)
       if(allocated(aslave))       deallocate(aslave,stat=ierr)
       if(allocated(asldir))       deallocate(asldir,stat=ierr)

9999   continue

       end




       subroutine write_sceua_data_header(iunit,pestfile,ns)

C -- Subroutine WRITE_SCEUA_HEADER records SCEUA control variables at the start
C    of a SCEUA run record file.

       use sceua_module
       use pestdata
       use modeldata
       implicit none

       integer, intent(in)      :: iunit
       character(*), intent(in) :: pestfile
       integer, intent(in)      :: ns

       integer ifail,itemp,ipar,iflag,ies
       real rtemp
       character*1 ayn
       character*20 anum
       character*30 atemp,atrans
       character*200 afile

       write(iunit,10)
10     format(/,t30,'SCEUA_P RUN RECORD FILE')

       write(iunit,*)
       write(iunit,*)
       call addquote(pestfile,afile)
       write(iunit,20) trim(afile)
20     format(' Name of PEST control file : ',a)
       if(iopt.eq.1)then
         write(iunit,21)
21       format(' Single observation-equivalent minimized as ',
     +   'model-calculated objective function.')
       end if

       write(iunit,*)
       write(iunit,25)
25     format(' Problem dimensions ----->')
       write(iunit,*)
       call writint(anum,npar)
       write(iunit,30) trim(anum)
30     format(t5,'Number of parameters                               : '
     + ,a)
       call writint(anum,nespar)
       write(iunit,40) trim(anum)
40     format(t5,'Number of adjustable parameters                    : '
     + ,a)
       call writint(anum,nobs)
       write(iunit,50) trim(anum)
50     format(t5,'Number of observations                             : '
     + ,a)
       call writint(anum,nprior)
       write(iunit,60) trim(anum)
60     format(t5,'Number of prior information eqns                   : '
     + ,a)

       write(iunit,*)
       write(iunit,100)
100    format(' SCE control variables ----->')
       write(iunit,*)
       itemp=huge(itemp)
       call sceua_get_integer_options(ifail,'ngs',itemp)
       call writint(anum,itemp)
       write(iunit,110) trim(anum)
110    format(t5,'Number of complexes in initial population          : '
     + ,a)
       itemp=huge(itemp)
       call sceua_get_integer_options(ifail,'mings',itemp)
       call writint(anum,itemp)
       write(iunit,112) trim(anum)
112    format(t5,'Minumum number of complexes                        : '
     + ,a)
       itemp=huge(itemp)
       call sceua_get_integer_options(ifail,'npg',itemp)
       call writint(anum,itemp)
       write(iunit,114) trim(anum)
114    format(t5,'Number of parameter sets in each complex           : '
     + ,a)
       itemp=huge(itemp)
       call sceua_get_integer_options(ifail,'nps',itemp)
       call writint(anum,itemp)
       write(iunit,116) trim(anum)
116    format(t5,'Number of parameter sets in a sub-complex          : '
     + ,a)
       itemp=huge(itemp)
       call sceua_get_integer_options(ifail,'nspl',itemp)
       call writint(anum,itemp)
       write(iunit,118) trim(anum)
118    format(t5,'Number of evolution steps before complex shuffling : '
     + ,a)
       itemp=huge(itemp)
       call sceua_get_integer_options(ifail,'iseed',itemp)
       call writint(anum,itemp)
       write(iunit,120) trim(anum)
120    format(t5,'Initial random seed                                : '
     + ,a)

       write(iunit,*)
       itemp=huge(itemp)
       call sceua_get_integer_options(ifail,'iniflg',itemp)
       if(itemp.eq.1)then
         ayn='y'
       else if(itemp.eq.0)then
         ayn='n'
       else
         ayn='*'
       end if
       write(iunit,122) trim(ayn)
122    format(t5,'Include initial parameter set in population        : '
     + ,a)
       itemp=huge(itemp)
       call sceua_get_integer_options(ifail,'iprint',itemp)
       if(itemp.eq.1)then
         ayn='y'
       else if(itemp.eq.0)then
         ayn='n'
       else
         ayn='*'
       end if
       write(iunit,124) trim(ayn)
124    format(t5,'Print information on every point of population     : '
     + ,a)

       write(iunit,*)
       write(iunit,131)
131    format(' SCE termination criteria ----->')
       itemp=huge(itemp)
       call sceua_get_integer_options(ifail,'maxn',itemp)
       call writint(anum,itemp)
       write(iunit,135) trim(anum)
135    format(t5,'Maximum number of model runs                       : '
     + ,a)
       call sceua_get_real_options(ifail,'pcento',rtemp)
       write(anum,'(1pg11.4)') rtemp
       write(iunit,132) trim(anum)
132    format(t5,'Relative objective function improvement            : '
     + ,a)
       call sceua_get_integer_options(ifail,'kstop',itemp)
       call writint(anum,itemp)
       write(iunit,133) trim (anum)
133    format(t5,'Number of shuffling loops to which this applies    : '
     + ,a)

       write(iunit,*)
       write(iunit,200)
200    format(' Parameter data ----->')
       write(iunit,*)
       write(iunit,210)
210    format(t2,'Name',t15,'Transformation',t39,
     + 'Initial_value',t57,'Lower_bound',t75,'Upper_bound')
       do ipar=1,npar
         if(itrans(ipar).eq.0)then
           atrans='none'
         else if(itrans(ipar).eq.1)then
           atrans='log'
         else if(itrans(ipar).eq.-1000000)then
           atrans='fixed'
         else
           atemp=apar(-itrans(ipar))
           atrans='tied_to_'//trim(atemp)
         end if
         write(iunit,220) trim(apar(ipar)),trim(atrans),pval(ipar),
     +   parlbnd(ipar),parubnd(ipar)
220      format(t2,a,t18,a,t40,1pg13.6,t58,1pg13.6,t77,1pg13.6)
       end do
       write(iunit,*)
       write(iunit,230)
230    format(t2,'Name',t15,'Scale',t33,'Offset')
       do ipar=1,npar
         write(iunit,240) trim(apar(ipar)),scale(ipar),offset(ipar)
240      format(t2,a,t15,1pg13.6,t33,1pg13.6)
       end do

       write(iunit,*)
       write(iunit,340)
340    format(/,t28,'RECORD OF OPTIMISATION PROCESS')
       write(iunit,*)

#ifdef FLUSHFILE
       call flush(iunit)
#endif

       return

       end



       subroutine feval(ifail,xx,yy)

C -- Subroutine FEVAL undertakes a function evaluation. The output of this
C    is an objective function value.

       use subiter
       use subiter2
       use subiter3
       use pestdata
       use ppestdata
       use modeldata
       use model_input_output_interface
       implicit none

       integer, intent(out)  :: ifail
       real, intent(in)      :: xx(:)
       real, intent(out)     :: yy

       integer          :: jfail,ipar,i,istop

C -- Initialisation

       ifail=0

C -- Check for user-intervention

       call stopress(istop)
       if(istop.eq.1)then
         ifail=-1
         go to 9100
       else if(istop.eq.2)then
         ifail=-2
         go to 9100
       end if

C -- First the parameter values to employ on the current run are computed.

       call adj2all(xx)

C -- The scale and offset is applied.

       do ipar=1,npar
         pval(ipar)=pval(ipar)*scale(ipar)+offset(ipar)
       end do

C -- Now the run is actually undertaken.

       call mio_delete_output_files(jfail)
       if(jfail.ne.0) go to 9000

       call mio_write_model_input_files(jfail,npar,apar,pval)
       if(jfail.ne.0) go to 9000

       call system(comlin(1))

C -- As a precaution, a WAIT is inserted here to prevent operating
C    system messages getting crossed. Hopefully this avoids errors
C    in reading model output files.

       if(ipll.lt.0) call pwait(50)

       call mio_read_model_output_files(jfail,nobs,aobs,modval_o,
     + instruction)
       if(jfail.ne.0) go to 9000

C -- The following should not be necessary unless there is prior information,
C    but is done for safety.

       do ipar=1,npar
         pval(ipar)=(pval(ipar)-offset(ipar))/scale(ipar)
       end do

C -- Prior information is evaluated.

       if(nprior.ne.0) then
         call priorcalc(modval_o)
       end if

! -- The objective function is now evaluated.

       call objclc(modval_o,yy)

C -- Contributions to the objective function are placed into a real
C    (rather than double precision) array.

       if(yy.le.phi_lowest)then
         phi_lowest=yy
         do i=1,nobsgp
            phi_contrib(i)=phid_contrib(i)
         end do
         do i=1,nespar
           xx_lowest(i)=xx(i)
         end do
         if(ipll.ge.0)then
           call record_parameters(ifail,xx)
         end if
       end if

       feval_call=feval_call+1

       return

9000   continue
       yy=-1.1e35
       ifail=11
       return

9100   continue
       yy=-1.1e35
       return

       end



       subroutine write_restart_data_main(nopt,ngs,npg,nps,nspl,mings,
     + npt)

       use pestdata
       use modeldata
       implicit none

       integer, intent(in) :: nopt
       integer, intent(in) :: ngs
       integer, intent(in) :: npg
       integer, intent(in) :: nps
       integer, intent(in) :: nspl
       integer, intent(in) :: mings
       integer, intent(in) :: npt
       integer   :: resunit
       integer   :: nextunit
       integer   :: itemp

       resunit=nextunit()
       itemp=max(8,nobsgp)*8
       if(itemp.lt.1000) itemp=1000
       open(unit=resunit,file=restartfile_main,form='unformatted',
     + recl=itemp)

       write(resunit) iopt
       write(resunit) nopt,ngs,npg,nps,nspl,mings,npt
       write(resunit) feval_call,mul_feval_call,mul_feval_model_call
       write(resunit) phi_lowest
       write(resunit) phi_contrib
       write(resunit) phid_contrib

       close(unit=resunit)

       return
       end



       subroutine get_number_model_runs(icall)

       use ppestdata
       use modeldata
       implicit none
       integer, intent(out)   :: icall

       icall=feval_call+mul_feval_model_call

       return
       end



       subroutine priorcalc(outvals)

       use pestdata
       use modeldata
       implicit none

       double precision, intent(inout) :: outvals(:)

       integer iobs,ipcoff,ies,ipar

       do iobs=nobs+1,nxrow
         outvals(iobs)=0.0d0
       end do
       do ipcoff=1,nipcoff
         iobs=iobscoff(ipcoff)
         ies=iescoff(ipcoff)
         ipar=ies2ipar1(ies)
         if(itrans(ipar).eq.0)then
           outvals(iobs)=outvals(iobs)+rcoff(ipcoff)*pval(ipar)
         else if(itrans(ipar).eq.1)then
           outvals(iobs)=outvals(iobs)+rcoff(ipcoff)*log10(pval(ipar))
         end if
       end do

       end


       subroutine objclc(outvals,phi)

       use pestdata
       use modeldata
       implicit none

       double precision, intent(in)  :: outvals(:)
       real, intent(out)             :: phi

       integer                       :: iobs,jcov,j,i,igp,jj
       double precision              :: phid,rtemp

       phid=0.0d0
       do i=1,nobsgp
         phid_contrib(i)=0.0d0
       end do

       if(iopt.eq.1)then
         phi=outvals(1)
         igp=nobgnm(1)
         phid_contrib(igp)=phi
         return
       end if

       do iobs=1,nxrow
         rtemp=outvals(iobs)-oval(iobs)
         rtemp=rtemp*rtemp*owght(iobs)
         phid=phid+rtemp
         igp=nobgnm(iobs)
         phid_contrib(igp)=phid_contrib(igp)+rtemp
       end do
       if(numcov.ne.0)then
         do jcov=1,numcov
           j=covgp(jcov)
           jj=j
           i=0
           do iobs=1,nxrow
             if(nobgnm(iobs).eq.j)then
               i=i+1
               res(i)=outvals(iobs)-oval(iobs)
             end if
           end do
           do i=1,nobscov(jcov)
             do j=1,nobscov(jcov)
               rtemp=res(i)*res(j)*covar(jcov)%dval(i,j)
               phid=phid+rtemp
               phid_contrib(jj)=phid_contrib(jj)+rtemp
             end do
           end do
         end do
       end if

       phi=phid
       return
       end



       subroutine adj2all(xx)

C -- Subroutine ADJ2ALL populates PEST parameter values on the basis of
C    parameter values as employed by the optimiser.

       use pestdata
       implicit none

       real, intent(in)     :: xx(:)

       integer              :: ies,ipar,j

       ies=0
       do ipar=1,npar
         if(itrans(ipar).eq.0)then
           ies=ies+1
           pval(ipar)=xx(ies)
         else if(itrans(ipar).eq.1)then
           ies=ies+1
           pval(ipar)=10**xx(ies)
         else if(itrans(ipar).le.1000000)then
           pval(ipar)=pval_o(ipar)
         end if
       end do

       do ipar=1,npar
         if((itrans(ipar).lt.0).and.(itrans(ipar).gt.-1000000))then
           j=-itrans(ipar)
           pval(ipar)=pval(j)*pval_o(ipar)/pval_o(j)
         end if
       end do

       return

       end




       subroutine mul_feval(ifail,nrun,xx,yy)

C -- Subroutine MUL_FEVAL undertakes multiple function evaluations. The output of this
C    is a set of objective function values.

       use subiter
       use subiter1
       use subiter3
       use pestdata
       use ppestdata
       use modeldata
       use model_input_output_interface
       use parallel_run_manager
       implicit none

       integer, intent(out)  :: ifail
       integer, intent(in)   :: nrun
       real, intent(inout)   :: xx(:,:)
       real, intent(inout)   :: yy(:)

       integer          :: irun,ipar,i,j,istop,jfail,ii,ies,is
       integer          :: forgive_failure
       integer          :: nextunit
       character*10     :: arun

C -- Initialisation

       ifail=0
       mul_feval_call=mul_feval_call+1
       forgive_failure=0

       if(ipll.le.0)then
         write(6,*) ' Error - contact programmer.'
         stop
       else
         irestart_pll=0
         itn=itn+1
         is=size(xx,1)
         do irun=1,nrun
           do i=1,is
             parreg(i,irun)=xx(i,irun)
           end do
         end do
C         if(iswitch_pll.eq.0)then
C         call prm_doruns(ifail,itn,npar,nobs,nrun,npar,nxrow,parreg,
C     +                   obsreg,apar,aobs,irestart_pll,restfile_pll)
C         else
         call prm_doruns(ifail,itn,npar_pll,nobs,nrun,npar_pll,nxrow,
     +                   parreg,obsreg,apar_pll,aobs,irestart_pll,
     +                   restfile_pll,forgive_failure)
C         end if
         if(ifail.ne.0) go to 9000

         if(iswitch_pll.eq.0)then
           do irun=1,nrun
             yy(irun)=obsreg(1,irun)
             if(yy(irun).le.phi_lowest)then
               phi_lowest=yy(irun)
               do i=1,nobsgp
                  phi_contrib(i)=obsreg(i+1,irun)
               end do
               do i=1,nespar
                 xx_lowest(i)=xx(i,irun)
               end do
               call record_parameters(ifail,xx(:,irun))
             end if
           end do
         end if
       end if

       mul_feval_model_call=mul_feval_model_call+nrun

       return

9000   continue
       return

       end


       subroutine get_x_data(npt_in,nopt_in,ngs1_in,npg_in,x_main,
     + xf_main,jcall)

       use subiter
       use subiter1
       use subiter2
       use subiter3
       use modeldata
       use ppestdata
       use pestdata
       implicit none

       integer, intent(in)      :: npt_in,nopt_in,ngs1_in,npg_in
       integer, intent(out)     :: jcall
       double precision, intent(inout)  :: x_main(npt_in,nopt_in),
     +                                     xf_main(npt_in)

       integer                  :: igs,ii,k1,k2,j,jcalltot,i,ifail
       real                     :: rtemp

       jcalltot=0
       do igs=1,ngs1_in
         ii=0
         do k1 = 1, npg_in
           k2 = (k1-1) * ngs1_in + igs
           do j = 1, nopt_in
             ii=ii+1
             x_main(k2,j) = obsreg(ii,igs)
           end do
           ii=ii+1
           xf_main(k2) = obsreg(ii,igs)
         end do
         ii=ii+1
         rtemp=obsreg(ii,igs)
         if(rtemp.lt.phi_lowest)then
           phi_lowest=rtemp
           do i=1,nobsgp
             ii=ii+1
             phi_contrib(i)=obsreg(ii,igs)
             phid_contrib(i)=phi_contrib(i)
           end do
           do i=1,nopt_in
             ii=ii+1
             xx_lowest(i)=obsreg(ii,igs)
           end do
           call record_parameters(ifail,xx_lowest)
         else
           do i=1,nobsgp+nopt_in
             ii=ii+1
           end do
         end if
         ii=ii+1
         jcall=nint(obsreg(ii,igs))
         jcalltot=jcalltot+jcall
       end do

C -- An adjustment is made so that the model calls counter is correct.

       mul_feval_model_call=mul_feval_model_call-ngs1_in+jcalltot

       return

       end



       subroutine alter_mio_data(npg_in,nopt_in,ngs_in)

       use pestdata
       use modeldata
       use model_input_output_interface
       use parallel_run_manager
       use ppestdata
       implicit none

       integer, intent(in)  :: npg_in,nopt_in,ngs_in

       integer              :: ifail,iunit,i,ntot,ii,j,iruntime,islave
       integer              :: nextunit
       character*10         :: aprecis,decpoint,anum,anumi,anumj

C -- The previous setup is erased.

       call mio_finalise(ifail)
       deallocate(parreg,obsreg)
       deallocate(aobs)

C -- A new instruction file is written (the template file is written in SCEUA).

       ntot=npg_in*(nopt_in+1)
       nobs=ntot+1+nobsgp+nopt_in+1
       nxrow=nobs
       allocate(aobs(nobs))
       ii=0
       do i=1,npg_in
         do j=1,nopt_in
           ii=ii+1
           call writint(anumi,i)
           call writint(anumj,j)
           aobs(ii)='x_'//trim(anumi)//'_'//trim(anumj)
         end do
         ii=ii+1
         aobs(ii)='xf_'//trim(anumi)
       end do
       ii=ii+1
       aobs(ii)='objfn'
       do i=1,nobsgp
         call writint(anum,i)
         anum=adjustl(anum)
         ii=ii+1
         aobs(ii)='contrib_'//trim(anum)
       end do
       do i=1,nopt_in
         call writint(anum,i)
         anum=adjustl(anum)
         ii=ii+1
         aobs(ii)='xx_'//trim(anum)
       end do
       ii=ii+1
       aobs(ii)='calls'
       iunit=nextunit()
       open(unit=iunit,file=sce_slave_insfile)
       write(iunit,20)
20     format('pif $')
       do i=1,nobs
         write(iunit,30) trim(aobs(i))
30       format('l1 [',a,']1:30')
       end do
       close(unit=iunit)

C -- A new mio setup is now initialised.

       npar_pll=2
       apar_pll(1)='ctl1'
       apar_pll(2)='ctl2'
       aprecis='single'
       decpoint='point'
       call mio_initialise(ifail,ntplfle,ninsfle,npar_pll,nobs,
     + aprecis,decpoint)

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

       call mio_process_template_files(ifail,npar_pll,apar_pll)
       if(ifail.ne.0) go to 9000
       call mio_store_instruction_set(ifail)
       if(ifail.ne.0) go to 9000

       allocate(parreg(npar_pll,ngs_in),obsreg(nobs,ngs_in))
       iswitch_pll=1

C -- The parallel run manager is told to increase run times on account of the
C    fact that complexes are being evolved rather than individual runs undertaken.
C -- Also overdue run assessment is set more loosely because of the varying time
C    required for evolution of complexes.

       call prm_set_time_refresh_flag(ifail,.true.)
       call prm_set_fac_rerun(ifail,3.0d0)

       return

9000   continue
       write(6,9005)
9005   format(/,' Error in subroutine ALTER_MIO_DATA.')
       call mio_get_message_string(ifail,errmsg)
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
       if(instruction.ne.' ')then
         write(6,9010)
9010     format(' Instruction follows:-')
         write(6,9020) trim(instruction)
9020     format(1x,a)
       end if
       stop

       end


       subroutine get_lowest_x_vals(nopt_in,xx)

       use modeldata
       implicit none

       integer, intent(in)  :: nopt_in
       real, intent(out)    :: xx(nopt_in)
       integer            :: i

       do i=1,nopt_in
         xx(i)=xx_lowest(i)
       end do

       return
       end



       subroutine get_contribs(sphi,contrib,acontrib)

       use modeldata
       use pestdata
       implicit none

       real, intent(out)          :: sphi
       real, intent(out)          :: contrib(nobsgp)
       character*(*), intent(out) :: acontrib(nobsgp)
       integer             :: i

       sphi=phi_lowest
       do i=1,nobsgp
         contrib(i)=phi_contrib(i)
         acontrib(i)=obgnme(i)
       end do

       return

       end


       subroutine chkcst(nopt,x,bl,bu,ibound)
c
c      This subroutine checks if the SCE trial point satisfies all
c      constraints. Parts of this subroutine were extracted from FUNCTN08.FOR
c      supplied with SCE.

c      This subroutine is called by SCE code.
c
c      ibound - violation indicator
c             = -1 initial value
c             = 0  no violation
c             = 1  violation
c      nopt = number of optimizing variables
c      ii = the ii'th variable of the arrays x, bl, and bu
c
       implicit none

       integer, intent(in)           :: nopt
       double precision, intent(in)  :: x(nopt)
       double precision, intent(in)  :: bl(nopt)
       double precision, intent(in)  :: bu(nopt)
       integer, intent(out)          :: ibound

       integer       :: ii
c
       ibound = -1
c
c      Check if explicit constraints are violated
c
       do ii=1, nopt
         if (x(ii) .lt. bl(ii) .or. x(ii) .gt. bu(ii)) go to 10
       end do
       if (nopt .eq. 1) go to 9
c
c      Check if implicit constraints are violated
c      INSERT CODE HERE.
c      THIS CAN BE CODE THAT INVOLVES A FUNCTION OF THE PARAMETERS.
c
c      No constraints are violated
c
    9  ibound = 0
       return
   10  ibound = 1
       return
       end





       subroutine record_parameters(ifail,xx)

C -- Subroutine RECORD_PARAMETERS writes a parameter value file.

       use subiter
       use subiter1
       use subiter2
       use pestdata
       use modeldata

       implicit none

       integer, intent(out)  :: ifail
       real, intent(in)      :: xx(:)

       integer          :: ies,ipar,j,i,ierr,iunit
       integer          :: nextunit
       character*15     :: aprecis,apoint

C -- Initialisation

       ifail=0

C -- First parameter values are computed.

       call adj2all(xx)

C -- The file is opened.

       iunit=nextunit()
       open(unit=iunit,file=outfile_par,action='write',err=9890)
       if(precis.eq.0)then
         aprecis='single'
       else
         aprecis='double'
       end if
       if(nopnt.eq.0)then
         apoint='point'
       else
         apoint='nopoint'
       end if
       write(iunit,30) trim(aprecis),trim(apoint)
30     format(1x,a,2x,a)
       do ipar=1,npar
         write(iunit,50,err=9890) trim(apar(ipar)),pval(ipar),
     +   scale(ipar),offset(ipar)
50       format(1x,a,t20,1pg14.7,2x,1pg14.7,2x,1pg14.7)
       end do
       close(unit=iunit,err=9890)
       return

9890   continue
       close(unit=iunit,iostat=ierr)
       ifail=1
       return

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




        subroutine realread(ifail,cline,rtemp)

C -- Subroutine REALREAD reads a real number from a string.

        integer ifail
        real rtemp
        character*8 afmt
        character*(*) cline

        ifail=0
        afmt='(f   .0)'
        write(afmt(3:5),'(i3)') len_trim(cline)
        read(cline,afmt,err=100) rtemp
        return

100     ifail=1
        return
        end


        subroutine getfile(ifail,cline,filename,ibeg,iend)

C-- Subroutine getfile extracts a filename from a string.

C -- Arguments are as follows:-
C       ifail: returned as zero if filename successfully read
C       cline: a character string containing the file name
C       filename: the name of the file read from the string
C       ibeg: character position at which to begin search for filename
C       iend: on input  - character position at which to end search for filename
C             on output - character postion at which filename ends


        integer, intent(out)               :: ifail
        integer, intent(in)                :: ibeg
        integer, intent(inout)             :: iend
        character (len=*), intent(in)      :: cline
        character (len=*), intent(out)     :: filename

        integer                            :: i,j,k
        character (len=1)                  :: aa

        ifail=0
        do i=ibeg,iend
          aa=cline(i:i)
          if((aa.ne.' ').and.(aa.ne.',').and.(aa.ne.char(9)))go to 50
        end do
        ifail=1
        return

50      if((aa.eq.'"').or.(aa.eq.''''))then
          do j=i+1,iend
            if(cline(j:j).eq.aa) go to 60
          end do
          ifail=1
          return
60        iend=j
          if(i+1.gt.j-1)then
            ifail=1
            return
          else
            filename=cline(i+1:j-1)
          end if
        else
          do j=i+1,iend
            if((cline(j:j).eq.' ').or.(cline(j:j).eq.',').or.
     +         (cline(j:j).eq.char(9)))then
              k=j-1
              go to 100
            end if
          end do
          k=iend
100       filename=cline(i:k)
          if(cline(k:k).eq.'"')then
            ifail=1
            return
          else if(cline(k:k).eq.'''')then
            ifail=1
            return
          end if

          iend=k
        end if
        filename=adjustl(filename)
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


C -- Notes


csdfsd
