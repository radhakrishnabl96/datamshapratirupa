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

           subroutine record_history(phi,xval,obsval)
             real, intent(in)             :: phi
             real, intent(in)             :: xval(:)
             double precision, intent(in) :: obsval(:)
           end subroutine record_history

         end interface

       end module subiter1


       module subiter2

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

           subroutine feval(ifail,xx,yy,irecord)
             integer, intent(out) :: ifail
             real, intent(in)     :: xx(:)
             real, intent(out)    :: yy
             integer, intent(in)  :: irecord
           end subroutine

           subroutine record_history(phi,xval,obsval)
             real, intent(in)             :: phi
             real, intent(in)             :: xval(:)
             double precision, intent(in) :: obsval(:)
           end subroutine record_history

         end interface

       end module subiter2


       module subiter3

         interface

           subroutine mul_feval(ifail,n,xx,yy,adjust_flag,num_extra)
             integer, intent(out)  :: ifail
             integer, intent(in)   :: n
             real, intent(inout)   :: xx(:,:)
             real, intent(inout)   :: yy(:)
             integer, intent(out)  :: adjust_flag
             integer, intent(out)  :: num_extra
           end subroutine mul_feval

         end interface

       end module subiter3


       module modeldata

          integer                       :: mul_feval_call=0
          integer                       :: iopt
          double precision, allocatable :: cp(:,:)
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
          double precision, allocatable :: rcoff(:)

          double precision, allocatable :: phid_contrib(:)
          real, allocatable             :: phi_contrib(:)

          integer                       :: forgive_failure=1
          integer                       :: svd_app
          integer                       :: neig
          integer                       :: irecord_svd
          integer                       :: irecord_svd_last
          integer                       :: usehist
          integer                       :: usehist_test
          integer                       :: nhist
          integer                       :: lswork
          real                          :: phi_lowest
          real                          :: phi_fail=1.234e30
          integer, allocatable          :: nthresh(:)
          real, allocatable             :: seigthresh(:)
          real, allocatable             :: xxhist(:,:)
          real, allocatable             :: obshist(:,:)
          real, allocatable             :: phihist(:)
          real, allocatable             :: z(:,:)
          real, allocatable             :: rswork(:)
          real, allocatable             :: ztqr(:)
          real, allocatable             :: e(:)
          real, allocatable             :: f(:)
          real, allocatable             :: norm(:,:)
          real, allocatable             :: xx_svd(:,:)
          real, allocatable             :: yy_svd(:)
          character*200                 :: restartfile_main

       end module modeldata

       module ppestdata
          integer                       :: ipll
          integer                       :: itn
          integer                       :: irestart_pll
          integer                       :: nslave
          integer                       :: maxrun
          integer                       :: iwait
          integer                       :: repeatrun
          integer                       :: prm_mr = 30
          integer                       :: prm_mf = 31
          integer                       :: prm_wk = 32
          integer                       :: prm_nr = 33
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
       end module ppestdata


       program cmaes_p

C -- Program CMAES_P is a global optimiser based on CMA.

       use subiter
       use subiter3
       use pestdata
       use ppestdata
       use modeldata
       use model_input_output_interface
       use parallel_run_manager
       use cmaes_module
       implicit none

       logical lexist,ltemp

       integer ifail,jfail,jcov,itemp,info,ierr,iunit,itemp1
       integer i,j,iobs,ies,ipar,ibeg,iend
       integer iunit1,iunit2,itype,vardim,covdim,jpartype
       integer nb_pst,jrestart,maxcovdim
       integer outunit1,nstopflag
       integer islave,ipcoff,slaveflag
       integer irestart,resunit,resunit1,maxiter,userstop
       integer nextunit
       integer p1strt,pfile
       integer lw(5),rw(5)

       real rtemp
       real phi(1)
       double precision  :: parvar(1)

       real, allocatable :: sval(:,:),sigma(:)
       real, allocatable :: cps(:,:)

       double precision dtemp
       double precision det(2)
       character*1 aa,ao,ab,af
       character*3 acmd1,acmd2
       character*4 aext
       character*12 aprecis,decpoint
       character*12 aversion
       character*30 atemp,anum,atype
       character*200 pestfile,outfile1,afile,uncertfile,restartfile,
     +               uncfile,mttfile
       character*30 stopflag(10)

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif
       call pgetcl (cline)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' CMAES_P Version ',a,'. Watermark Numerical ',
     + 'Computing.')

       instruction=' '
       outunit1=20
       jrestart=0
       uncertfile=' '
       jpartype=0
       ipll=0
       itn=0
       irestart=1
       acmd1=' '
       acmd2=' '
       resunit=19
       stopfile='pest.stp'
       maxiter=-999
       usehist=0
       usehist_test=0
       nhist=0
       lswork=0
       irecord_svd=0
       irecord_svd_last=0
       phi_lowest=huge(phi_lowest)
       neig=0

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
       uncfile=pestfile(1:nb_pst)//'unc'
       mttfile=pestfile(1:nb_pst)//'cmf'

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
250      format('CMAES_P requires that PEST be run in ',
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
2731     format('CMAES_P cannot handle file parameters. Use CMAES_HP ',
     +   'instead.')
         go to 9890
       end if
       if(nparsec.gt.0)then
         write(errmsg,2732)
2732     format('CMAES_P cannot handle secondary parameters. ',
     +   'Use CMAES_HP instead.')
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

C -- Model-as-a-subroutine initialization is done.

       call modsub_flag(ifail,ms_status,errmsg)
       if(ifail.ne.0) go to 9890
       if(ms_status.ne.0)then
         if(ipll.ne.0)then
           write(errmsg,305)
305        format('MODSUB_FLAG must return a value of zero ',
     +     'if CMAES_P is to undertake model runs in parallel.')
           go to 9890
         end if
       end if
       if(ms_status.ne.0)then
         call modsub_init(ifail,npar,nobs,apar,itrans,aobs,errmsg)
         if(ifail.ne.0) go to 9890
       end if

C -- Memory is allocated for the "contributions to phi" vector.

       allocate(phid_contrib(nobsgp),phi_contrib(nobsgp),stat=ierr)
       if(ierr.ne.0) go to 9200

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

       if(ipll.ne.0)then
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

C -- The MIO module is prepared.

       write(6,311)
311    format(/,' - processing input data and preparing for ',
     + 'CMAES run...')

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

C -- If an existing run record file is present, it is deleted unless a restart is being made.

       if(irestart.ne.2)then
         inquire(file=outfile1,exist=lexist)
         if(lexist)then
           open(unit=outunit1,file=outfile1,iostat=ierr)
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

C -- CMAES output files are opened.

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

C -- The CMA module is initialised.

       call cmaes_init(ifail,nespar,outunit1)
       if(ifail.ne.0) go to 9100

C -- Memory is allocated.

       allocate(sigma(nespar),sval(nespar,1),sparlbnd(nespar),
     + sparubnd(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- Parameter standard deviations are set in accordance with their ranges.

       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           if(itrans(ipar).eq.0)then
             sigma(ies)=(parubnd(ipar)-parlbnd(ipar))*0.25
             sval(ies,1)=pval(ipar)
             sparlbnd(ies)=parlbnd(ipar)
             sparubnd(ies)=parubnd(ipar)
           else
             sval(ies,1)=log10(pval(ipar))
             sparlbnd(ies)=log10(parlbnd(ipar))
             sparubnd(ies)=log10(parubnd(ipar))
             sigma(ies)=(sparubnd(ies)-sparlbnd(ies))*0.25
           end if
         end if
       end do
       call cmaes_set_problem_definition(ifail,sval(:,1),
     + sparlbnd,sparubnd,sigma)
       if(ifail.ne.0) go to 9100

       if(irestart.eq.2)then
         call cmaes_set_integer_options(ifail,'irestart',irestart)
         call cmaes_set_character_options(ifail,'restartfile',
     +   restartfile)
         go to 512
       end if

C -- CMAES is informed of the number of observation groups.

       call cmaes_set_integer_options(ifail,'ncontrib',nobsgp)

C -- CMAES is told to leave behind a restart file.

       call cmaes_set_integer_options(ifail,'irestart',irestart)
       call cmaes_set_character_options(ifail,'restartfile',restartfile)

C -- If necessary, the mode of CMAES operation is now established.

       iopt=0
       if((nobs.eq.1).and.(nprior.eq.0))then
         write(6,*)
         write(6,405)
405      format(' PEST control file has only one observation.')
404      write(6,406,advance='no')
406      format('   Minimize model equiv or match to observation? ',
     +   '[i/a] (<Enter> if "i"): ')
         read(5,'(a)') ao
         if((ao.eq.'I').or.(ao.eq.'i').or.(ao.eq.' '))then
           iopt=1
           call cmaes_set_integer_options(ifail,'iopt',iopt)
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
410    format(' Enter values for following CMA control ',
     + 'variables:-',/)

       call cmaes_get_integer_options(ifail,'popsize',itemp)
       call writint(atemp,itemp)
415    write(6,420,advance='no') trim(atemp)
420    format('   Population size, lambda (<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call intread(ifail,anum,itemp)
         if(ifail.ne.0) go to 415
         if(itemp.le.0) go to 415
         call cmaes_set_integer_options(ifail,'popsize',itemp)
       end if
       maxrun=itemp

       call cmaes_get_integer_options(ifail,'parentnumber',itemp)
       call writint(atemp,itemp)
430    write(6,440,advance='no') trim(atemp)
440    format('   Number of parents, mu (<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call intread(ifail,anum,itemp)
         if(ifail.ne.0) go to 430
         if(itemp.le.0) go to 430
         call cmaes_set_integer_options(ifail,'parentnumber',itemp)
       end if

       call cmaes_get_character_options(ifail,'recombinationweights',
     + atemp)
       if(atemp(1:5).eq.'super') then
         anum='superlin'
       else if(atemp(1:5).eq.'linea')then
         anum='linear'
       else
         anum='equal'
       end if
450    write(6,460,advance='no') anum(1:1)
460    format('   Recombination wts - superlin, linear, equal '
     + '[s/l/e]  (<Enter> if "',a,'"): ')
       read(5,'(a)') aa
       if(aa.ne.' ')then
         if((aa.eq.'S').or.(aa.eq.'s'))then
           atemp='superlinear decrease'
         else if((aa.eq.'L').or.(aa.eq.'l'))then
           atemp='linear'
         else if((aa.eq.'E').or.(aa.eq.'e'))then
           atemp='equal'
         else
           go to 450
         end if
         call cmaes_set_character_options
     +   (ifail,'recombinationweights',atemp)
         if(ifail.ne.0) go to 450
       end if

       call cmaes_get_integer_options(ifail,'seed',itemp)
       call writint(atemp,itemp)
422    write(6,423,advance='no') trim(atemp)
423    format('   Random number seed (<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call intread(ifail,anum,itemp)
         if(ifail.ne.0) go to 422
C         if(itemp.le.0) go to 422
         call cmaes_set_integer_options(ifail,'seed',itemp)
       end if

451    write(6,452,advance='no')
452    format('   Read parameter covariance matrix from a file? ',
     + '[y/n] (<Enter> if "n"): ')
       read(5,'(a)') aa
       if(aa.eq.' ')then
         aa='n'
       else
         if((aa.eq.'Y').or.(aa.eq.'y'))then
           aa='y'
         else if((aa.eq.'N').or.(aa.eq.'n'))then
           aa='n'
         else
           go to 451
         end if
       end if
       if(aa.eq.'y')then
454      write(6,453,advance='no')
453      format('   Enter name of parameter uncertainty file: ')
         read(5,'(a)') afile
         if(afile.eq.' ') go to 454
         ibeg=1
         iend=len_trim(afile)
         call getfile(ifail,afile,uncertfile,ibeg,iend)
         if(ifail.ne.0) go to 454
         open(unit=15,file=uncertfile,status='old',err=454)
         iunit1=15
         iunit2=16
         itype=2
         vardim=1
         covdim=nespar
         atype='parameter'
         allocate(cp(nespar,nespar),adjapar(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             ies=ies+1
             adjapar(ies)=apar(ipar)
           end if
         end do
         call read_uncert_data_file(ifail,iunit1,iunit2,itype,
     +   jpartype,nespar,vardim,covdim,parvar,cp,atype,uncertfile,
     +   errmsg,cline,adjapar)
         if(ifail.ne.0) then
           errmsg=adjustl(errmsg)
           go to 9890
         end if
         write(6,455) trim(afile)
455      format('    - file ',a,' read ok.')
         if(jpartype.eq.1)then
           do i=1,nespar
             sigma(i)=sqrt(cp(i,i))
           end do
           call cmaes_set_real_array_options(ifail,'sigma',sigma)
         else
           call cmaes_set_logical_options(ifail,'SupplyInitialCov',
     +     .true.)
           allocate(cps(nespar,nespar),stat=ierr)
           if(ierr.ne.0) go to 9200
           do i=1,nespar
             do j=1,nespar
               cps(j,i)=cp(j,i)
             end do
           end do
           call cmaes_set_real_matrix_options(ifail,'c',cps)
         end if
       end if

       if(nespar.le.nxrow)then
         write(6,*)
456      write(6,457,advance='no')
457      format('   Employ SVD hybridisation?  [y/n]: ',
     +   '(<Enter> if "n"): ')
         read(5,'(a)') aa
         if(aa.eq.' ')then
           usehist=0
         else if((aa.eq.'y').or.(aa.eq.'Y'))then
           usehist=1
         else if((aa.eq.'n').or.(aa.eq.'n'))then
           usehist=0
         else
           go to 456
         end if
         if((numcov.ne.0).and.(usehist.eq.1))then
           write(errmsg,432)
432        format('SVD-hybridization cannot take place when one ',
     +     'or more observation covariance matrixes are cited in the ',
     +     'PEST input dataset.')
           go to 9890
         end if
         if(usehist.eq.1)then
442        write(6,441,advance='no')
441        format('   How many trial singular value thresholds? ',
     +     '(<Enter> if 3): ')
           read(5,'(a)') anum
           if(anum.ne.' ')then
             call intread(ifail,anum,neig)
             if(ifail.ne.0) go to 442
             if(neig.le.0) go to 442
           else
             neig=3
           end if
443        write(6,444,advance='no')
444        format('   Use "soft" or "hard" hybridization?  [s/h] ',
     +     '(<Enter> if "h"): ')
           read(5,'(a)') aa
           if(aa.eq.' ')then
             svd_app=2
           else
             if((aa.eq.'s').or.(aa.eq.'S'))then
               svd_app=1
             else if((aa.eq.'h').or.(aa.eq.'H'))then
               svd_app=2
             else
               go to 443
             end if
           end if
           allocate(xxhist(nespar,nespar+1),obshist(nxrow,nespar+1),
     +     xx_svd(nespar,neig),yy_svd(neig),phihist(nespar+1),
     +     seigthresh(neig),nthresh(neig),stat=ierr)
           if(ierr.ne.0) go to 9200
           xxhist=0.0
           obshist=0.0
           phihist=0.0
           allocate(z(nxrow,nespar),ztqr(nxrow),e(nespar),
     +     f(nespar),norm(nespar,nespar),stat=ierr)
           if(ierr.ne.0) go to 9200
           lswork=max(3*min(nespar,nxrow)+max(nespar,nxrow),
     +     5*min(nespar,nxrow))+nxrow
           allocate(rswork(lswork),stat=ierr)
           if(ierr.ne.0) go to 9200
           maxrun=maxrun+neig
         end if
       end if

       write(6,*)
4632   write(6,4631,advance='no')
4631   format('   Forgive model run failure? [y/n]: ',
     + '(<Enter> if "y"): ')
       read(5,'(a)') af
       if(af.eq.' ')then
        forgive_failure=1
       else if((af.eq.'Y').or.(af.eq.'y'))then
         forgive_failure=1
       else if((af.eq.'N').or.(af.eq.'n'))then
         forgive_failure=0
       else
         go to 4632
       end if

       write(6,463)
463    format(/,' Termination Criteria:-')
       call cmaes_get_real_options(ifail,'phiredstp',rtemp)
       write(atemp,'(1pg11.4)') rtemp
       atemp=adjustl(atemp)
465    write(6,470,advance='no') trim(atemp)
470    format('   Min rel obj fn change over N itns ',
     + '(<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call realread(ifail,anum,rtemp)
         if(ifail.ne.0) go to 465
         if(rtemp.le.0.0) go to 465
         call cmaes_set_real_options(ifail,'phiredstp',rtemp)
       end if

       call cmaes_get_integer_options(ifail,'nphistp',itemp)
       call writint(atemp,itemp)
475    write(6,480,advance='no') trim(atemp)
480    format('   No of itns (N) over which this applies ',
     + '(<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call intread(ifail,anum,itemp)
         if(ifail.ne.0) go to 475
         if(itemp.le.0) go to 475
         call cmaes_set_integer_options(ifail,'nphistp',itemp)
         call cmaes_set_integer_options(ifail,'nphinored',itemp)
       end if

       call cmaes_get_real_options(ifail,'relparstp',rtemp)
       write(atemp,'(1pg11.4)') rtemp
       atemp=adjustl(atemp)
485    write(6,490,advance='no') trim(atemp)
490    format('   Min rel param change over N itns ',
     + '(<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call realread(ifail,anum,rtemp)
         if(ifail.ne.0) go to 485
         if(rtemp.le.0.0) go to 485
         call cmaes_set_real_options(ifail,'relparstp',rtemp)
       end if

       call cmaes_get_integer_options(ifail,'nrelpar',itemp)
       call writint(atemp,itemp)
495    write(6,500,advance='no') trim(atemp)
500    format('   No of itns (N) over which this applies ',
     + '(<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call intread(ifail,anum,itemp)
         if(ifail.ne.0) go to 495
         if(itemp.le.0) go to 495
         call cmaes_set_integer_options(ifail,'nrelpar',itemp)
       end if

       call cmaes_get_real_options(ifail,'thresh_phimaxmin',rtemp)
       write(atemp,'(1pg11.4)') rtemp
       atemp=adjustl(atemp)
501    write(6,502,advance='no') trim(atemp)
502    format('   Rel high-low generated obj fn diff over N itns ',
     + '(<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call realread(ifail,anum,rtemp)
         if(ifail.ne.0) go to 501
         if(rtemp.le.0.0) go to 501
         call cmaes_set_real_options(ifail,'thresh_phimaxmin',rtemp)
       end if

       call cmaes_get_integer_options(ifail,'nphimaxmin',itemp)
       call writint(atemp,itemp)
503    write(6,504,advance='no') trim(atemp)
504    format('   No of itns (N) over which this applies ',
     + '(<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call intread(ifail,anum,itemp)
         if(ifail.ne.0) go to 503
         if(itemp.le.0) go to 503
         call cmaes_set_integer_options(ifail,'nphimaxmin',itemp)
       end if

       call cmaes_get_integer_options(ifail,'maxiter',itemp)
       maxiter=itemp
       call writint(atemp,itemp)
505    write(6,510,advance='no') trim(atemp)
510    format('   Maximum number of iterations ',
     + '(<Enter> if ',a,'): ')
       read(5,'(a)') anum
       if(anum.ne.' ')then
         call intread(ifail,anum,itemp)
         if(ifail.ne.0) go to 505
         if(itemp.lt.0) go to 505
         call cmaes_set_integer_options(ifail,'maxiter',itemp)
         maxiter=itemp
       end if

       if(maxiter.eq.0)then
         ltemp=.true.
         call cmaes_set_logical_options(ifail,'evalinitialx',ltemp)
       else
         write(6,*)
         call cmaes_get_logical_options(ifail,'evalinitialx',ltemp)
         if(ltemp)then
           ab='y'
         else
           ab='n'
         end if
506      write(6,507,advance='no') trim(ab)
507      format('   Run model with initial parameters? [y/n]  ',
     +   '(<Enter> if "',a,'"): ')
         read(5,'(a)') aa
         if(aa.ne.' ')then
           if((aa.eq.'Y').or.(aa.eq.'y'))then
             ltemp=.true.
           else if((aa.eq.'N').or.(aa.eq.'n'))then
             ltemp=.false.
           else
             go to 506
           end if
           call cmaes_set_logical_options(ifail,'evalinitialx',ltemp)
         end if
       end if

512    continue

! -- If necessary the restart file is opened and read. (We need to do it here rather
!    than in the cmaes module so that we know popsize and hence can assign maxrun
!    to the parallalisation module.)
! -- We also need the iopt setting.

       if(irestart.eq.2)then

! -- First data is read from the module restart file.

         call cmaes_set_integer_options(ifail,'resunit',resunit)
         open(unit=resunit,file=restartfile,form='unformatted',
     +   status='old',iostat=ierr)
         if(ierr.ne.0)then
           write(errmsg,513) trim(restartfile)
513        format('Cannot open file ',a,' to read restart data.')
           go to 9890
         end if
         call cmaes_read_restart_data_1(ifail)
         if(ifail.ne.0) then
           call cmaes_get_message_string(ifail,errmsg)
           go to 9890
         end if
         call cmaes_get_integer_options(ifail,'popsize',maxrun)
         call cmaes_get_integer_options(ifail,'iopt',iopt)

! -- Now it is read from the main program restart file.

         resunit1=nextunit()
         open(unit=resunit1,file=restartfile_main,form='unformatted',
     +   status='old',iostat=ierr)
         if(ierr.ne.0)then
           write(errmsg,513) trim(restartfile_main)
           go to 9890
         end if
         read(resunit1,err=9350,end=9350) usehist,forgive_failure
         if(usehist.ne.0)then
           read(resunit1,err=9350,end=9350) svd_app,neig,irecord_svd,
     +     irecord_svd_last,usehist_test,nhist,maxrun
           read(resunit1,err=9350,end=9350) phi_lowest
           allocate(xxhist(nespar,nespar+1),obshist(nxrow,nespar+1),
     +     xx_svd(nespar,neig),yy_svd(neig),phihist(nespar+1),
     +     seigthresh(neig),nthresh(neig),stat=ierr)
           if(ierr.ne.0) go to 9200
           allocate(z(nxrow,nespar),ztqr(nxrow),e(nespar),
     +     f(nespar),norm(nespar,nespar),stat=ierr)
           if(ierr.ne.0) go to 9200
           lswork=max(3*min(nespar,nxrow)+max(nespar,nxrow),
     +     5*min(nespar,nxrow))+nxrow
           allocate(rswork(lswork),stat=ierr)
           if(ierr.ne.0) go to 9200
           read(resunit1,err=9350,end=9350)
     +     ((xxhist(i,j),i=1,nespar),j=1,nespar+1)
           read(resunit1,err=9350,end=9350)
     +     ((obshist(i,j),i=1,nxrow),j=1,nespar+1)
           read(resunit1,err=9350,end=9350)(phihist(i),i=1,nespar+1)
         end if
         close(unit=resunit1)
       end if

C -- If necessary, the parallelisation module is initiated.

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
         allocate(parreg(npar,maxrun),obsreg(nxrow,maxrun),stat=ierr)
         if(ierr.ne.0) go to 9200
       end if

C -- Variables which control this CMAES run are recorded to the run record file.

       if(irestart.ne.2)then
         call write_cmaes_data_header(outunit1,pestfile,jpartype,
     +   nespar,sigma,uncertfile)
       end if

C       if(allocated(cps))deallocate(cps,stat=ierr)
       if(allocated(cp))deallocate(cp,stat=ierr)

C -- CMAES is run.

       if(irestart.ne.2)then
         write(6,132)
132      format(' - input data processed ok.')
         write(6,133)
133      format(/,' - commencing CMAES optimisation process ...')
       else
         write(6,134)
134      format(/,' - re-commencing CMAES optimisation process ...')
       end if

       call cmaes_run(ifail)
       if(ifail.eq.11) go to 9000
       if(ifail.lt.0) go to 9300
       if(ifail.ne.0) go to 9100

C -- The reason for the stoppage is now acquired.

       call cmaes_get_stop_info(ifail,nstopflag,stopflag)
       if(ifail.ne.0) go to 9100
       call cmaes_get_message_string(ifail,cline)
       write(6,180)
180    format(/,' Completion of CMA-ES process.')
       write(outunit1,181)
181    format(/,/,t25,'COMPLETION OF CMA-ES PROCESS')

       write(6,185)
       write(outunit1,185)
185    format(/,' Reason for termination of CMA-ES process:-')
       cline=' '//adjustl(cline)
       call writmess(6,cline)
       call writmess(outunit1,cline)
       slaveflag=0
       userstop=0
       if(index(cline,'User init').ne.0) then
         userstop=1
         if(index(cline,'final').eq.0)slaveflag=1
       end if

       call cmaes_get_best_parameters(ifail,sval(:,1))
       if(ifail.ne.0) go to 9890
       call adj2all(sval(:,1))
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

       if(maxiter.ne.0)then
         if(slaveflag.eq.0)then
           if((ipll.eq.0).or.
     +       ((ipll.eq.1).and.(userstop.eq.0)))then
             write(6,241)
241          format(/,' Running model one last time using ',
     +       'optimised parameters...')
             usehist_test=0
             call mul_feval(jfail,1,sval,phi,itemp,itemp1)
             if(jfail.gt.0)then
               ifail=11                ! indicates that error was in MIO/PRM
             else
               ifail=jfail
             end if
            if(ifail.eq.11) go to 9000
            if(ifail.ne.0) go to 9100
          else
            write(6,238)
            write(outunit1,238)
238         format(/,' Final run cannot be undertaken when CMAES_P ',
     +      'user-stopped in parallel mode.')
            write(6,237)
            write(outunit1,237)
237         format(' Use PARREP utility and then run CMAES_P with ',
     +      'MAXITER equal to zero.')
          end if
        end if
      end if

C -- If runs are parallelised the slaves are now stopped.

       if(slaveflag.eq.0)then
         if(ipll.ne.0)then
           call prm_slavestop(ifail)
         end if
       end if

C -- The covariance matrix is stored.

       if(maxiter.ne.0)then
         if(.not.allocated(cps))then
           allocate(cps(nespar,nespar),stat=ierr)
           if(ierr.ne.0) go to 9200
         end if
         call cmaes_get_real_matrix_options(ifail,'c',cps)
         if(ifail.ne.0) then
           write(errmsg,619)
619        format('Error retreiving covariance matrix from CMAES ',
     +     'module.')
           go to 9890
         end if
         call cmaes_get_real_options(ifail,'sigma',rtemp)
         if(ifail.ne.0) then
           write(errmsg,618)
618        format('Error retreiving SIGMA from CMAES module.')
           go to 9890
         end if
         iunit=15
         open(unit=iunit,file=uncfile)
         write(iunit,620)
620      format('START COVARIANCE_MATRIX')
         call addquote(mttfile,afile)
         write(iunit,630) trim(afile)
630      format(2x,'file ',a)
         write(iunit,635) rtemp*rtemp
635      format(2x,'Variance_multiplier ',1pg14.7)
         write(iunit,640)
640      format('END COVARIANCE MATRIX')
         close(unit=iunit)
         open(unit=iunit,file=mttfile)
         write(iunit,650) nespar,nespar,1
650      format(3i10)
         if(nespar.gt.1)then
           do i=1,nespar
             do j=i+1,nespar
               rtemp=(cps(i,j)+cps(j,i))*0.5
               cps(i,j)=rtemp
               cps(j,i)=rtemp
             end do
           end do
         end if
         do i=1,nespar
           write(iunit,660) (cps(i,j),j=1,nespar)
660        format(10(1x,1pg14.7))
         end do
         write(iunit,670)
670      format('* row and column names')
         do i=1,npar
           if(itrans(i).ge.0)then
             write(iunit,680) trim(apar(i))
680          format(1x,a)
           end if
         end do
         close(unit=iunit)
       end if

       write(outunit1,*)
       write(6,*)
       write(6,690) trim(outfile1)
690    format(' See file ',a,' for complete run record.')
       write(outunit1,700) trim(outfile_par)
       write(6,700) trim(outfile_par)
700    format(' See file ',a,' for optimised parameter values.')
       if(maxiter.ne.0)then
         write(outunit1,710) trim(uncfile)
         write(6,710) trim(uncfile)
710      format(' See file ',a,' for optimised covariance matrix.')
       end if

       go to 9900

9000   continue
       if(ipll.eq.0)then
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

9100   call cmaes_get_message_string(ifail,errmsg)
       go to 9890

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9300   write(errmsg,9310)
9310   format('Stopping execution at user''s request.')
       go to 9890

9350   write(errmsg,9360) trim(restartfile_main)
9360   format('Error encountered in reading restart file ',a,'.')
       go to 9890

9400    write(6,9410)
9410    format(/,' CMAES_P is run using the command:',/)
        write(6,9420)
9420    format('     cmaes_p pestfile [/p] [/r]',/)
        write(6,9430)
9430    format(' where',/)
        write(6,9450)
9450    format('     pestfile is the name of a PEST ',
     +  'control file,')
        write(6,9510)
9510    format('     /p       is an optional parallelisation switch, ',
     +  'and')
        write(6,9520)
9520    format('     /r       is an optional restart switch.')

        GO TO 9999

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
       call cmaes_fin(ifail)
       call mio_finalise(ifail)
       if(ipll.eq.1)then
         call prm_finalise(ifail)
         if(allocated(parreg)) deallocate(parreg,stat=ierr)
         if(allocated(obsreg)) deallocate(obsreg,stat=ierr)
       end if
       if(allocated(sigma))        deallocate(sigma,stat=ierr)
       if(allocated(sval))         deallocate(sval,stat=ierr)
       if(allocated(sparlbnd))     deallocate(sparlbnd,stat=ierr)
       if(allocated(sparubnd))     deallocate(sparubnd,stat=ierr)
       if(allocated(cp))           deallocate(cp,stat=ierr)
       if(allocated(cps))          deallocate(cps,stat=ierr)
       if(allocated(adjapar))      deallocate(adjapar,stat=ierr)
       if(allocated(res))          deallocate(res,stat=ierr)
       if(allocated(iescoff))      deallocate(iescoff,stat=ierr)
       if(allocated(iobscoff))     deallocate(iobscoff,stat=ierr)
       if(allocated(rcoff))        deallocate(rcoff,stat=ierr)
       if(allocated(ies2ipar1))    deallocate(ies2ipar1,stat=ierr)
       if(allocated(phid_contrib)) deallocate(phid_contrib,stat=ierr)
       if(allocated(phi_contrib))  deallocate(phi_contrib,stat=ierr)
       if(usehist.ne.0)then
         if(allocated(xxhist))     deallocate(xxhist)
         if(allocated(obshist))    deallocate(obshist)
         if(allocated(phihist))    deallocate(phihist)
         if(allocated(z))          deallocate(z)
         if(allocated(rswork))     deallocate(rswork)
         if(allocated(ztqr))       deallocate(ztqr)
         if(allocated(e))          deallocate(e)
         if(allocated(f))          deallocate(f)
         if(allocated(norm))       deallocate(norm)
         if(allocated(xx_svd))     deallocate(xx_svd)
         if(allocated(yy_svd))     deallocate(yy_svd)
         if(allocated(seigthresh)) deallocate(seigthresh)
         if(allocated(nthresh))    deallocate(nthresh)
       end if
       if(ms_status.ne.0)then
         call modsub_fin()
       end if

9999   continue

       end



       subroutine write_cmaes_data_header(iunit,pestfile,jpartype,
     + ns,sigma,uncertfile)

C -- Subroutine WRITE_CMAES_HEADER records CMAES control variables at the start
C    of a CMAES run record file.

       use cmaes_module
       use pestdata
       use modeldata
       implicit none


       integer, intent(in)      :: iunit
       character(*), intent(in) :: pestfile
       integer, intent(in)      :: jpartype
       integer, intent(in)      :: ns
       real, intent(in)         :: sigma(ns)
       character(*)             :: uncertfile

       integer ifail,itemp,ipar,iflag,ies
       real rtemp
       character*20 anum
       character*30 atemp,atrans
       character*200 afile

       write(iunit,10)
10     format(/,t30,'CMAES_P RUN RECORD FILE')

       write(iunit,*)
       write(iunit,*)
       call addquote(pestfile,afile)
       write(iunit,20) trim(afile)
20     format(' Name of PEST control file : ',a)
       if(iopt.eq.1)then
         write(iunit,21)
21       format(' Single observation-equivalent minimized as ',
     +   'model-calculated fitness.')
       end if

       write(iunit,*)
       write(iunit,25)
25     format(' Problem dimensions ----->')
       write(iunit,*)
       call writint(anum,npar)
       write(iunit,30) trim(anum)
30     format(t5,'Number of parameters                       : ',a)
       call writint(anum,nespar)
       write(iunit,40) trim(anum)
40     format(t5,'Number of adjustable parameters            : ',a)
       call writint(anum,nobs)
       write(iunit,50) trim(anum)
50     format(t5,'Number of observations                     : ',a)
       call writint(anum,nprior)
       write(iunit,60) trim(anum)
60     format(t5,'Number of prior information eqns           : ',a)

       write(iunit,*)
       write(iunit,100)
100    format(' CMA control variables ----->')
       write(iunit,*)
       itemp=huge(itemp)
       call cmaes_get_integer_options(ifail,'popsize',itemp)
       call writint(anum,itemp)
       write(iunit,110) trim(anum)
110    format(t5,'Population size (lambda)                   : ',a)
       itemp=huge(itemp)
       call cmaes_get_integer_options(ifail,'parentnumber',itemp)
       call writint(anum,itemp)
       write(iunit,120) trim(anum)
120    format(t5,'Number of parents (mu)                     : ',a)
       atemp='*****'
       call cmaes_get_character_options(ifail,'recombinationweights',
     + atemp)
       write(iunit,140) trim(atemp)
140    format(t5,'Recombination weights                      : "',a,'"')
       itemp=huge(itemp)
       call cmaes_get_integer_options(ifail,'seed',itemp)
       call writint(anum,itemp)
       write(iunit,130) trim(anum)
130    format(t5,'Random number seed                         : ',a)
       anum='no'
       if(usehist.ne.0) anum='yes'
       write(iunit,122) trim(anum)
122    format(t5,'Employ SVD hybridization                   : ',a)
       if(usehist.eq.1)then
         call writint(anum,neig)
         write(iunit,123) trim(anum)
123      format(t5,'Number of trial singular value thresholds  : ',a)
         if(svd_app.eq.1)then
           anum='soft'
         else
           anum='hard'
         end if
         write(iunit,124) trim(anum)
124      format(t5,'Employ soft or hard hybridization          : ',a)
       end if

       write(iunit,*)
       write(iunit,131)
131    format(' CMA termination criteria ----->')
       write(iunit,*)
       call cmaes_get_real_options(ifail,'phiredstp',rtemp)
       write(anum,'(1pg11.4)') rtemp
       write(iunit,132) trim(anum)
132    format(t5,'Relative objective fn improvement          : ',a)
       call cmaes_get_integer_options(ifail,'nphistp',itemp)
       call writint(anum,itemp)
       write(iunit,133) trim (anum)
133    format(t5,'Number of itns to which this applies       : ',a)
       call cmaes_get_real_options(ifail,'relparstp',rtemp)
       write(anum,'(1pg11.4)') rtemp
       write(iunit,134) trim(anum)
134    format(t5,'Maximum relative parameter change          : ',a)
       call cmaes_get_integer_options(ifail,'nrelpar',itemp)
       call writint(anum,itemp)
       write(iunit,133) trim (anum)
       call cmaes_get_real_options(ifail,'thresh_phimaxmin',rtemp)
       write(anum,'(1pg11.4)') rtemp
       write(iunit,136) trim(anum)
136    format(t5,'Relative generated obj fn difference       : ',a)
       call cmaes_get_integer_options(ifail,'nphimaxmin',itemp)
       call writint(anum,itemp)
       write(iunit,133) trim (anum)
       call cmaes_get_integer_options(ifail,'maxiter',itemp)
       call writint(anum,itemp)
       write(iunit,135) trim(anum)
135    format(t5,'Maximum number of iterations               : ',a)


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
       write(iunit,250)
250    format(' Initial parameter covariance data ----->')
       if(uncertfile.eq.' ')then
         write(iunit,260)
260      format(/,' Initial SIGMAs computed as 0.25 of ',
     +   '(log transformed) parameter bounds.')
         iflag=1
       else
         if(jpartype.eq.1)then
           write(iunit,270) trim(uncertfile)
270        format(/,' Initial SIGMAs computed as square root ',
     +     'of diagonals of diagonal',/,' covariance matrix ',
     +     'supplied in file ',a,'.')
           iflag=1
         else
           write(iunit,280) trim(uncertfile)
280        format(/,' Initial parameter covariance matrix read ',
     +     'from file ',a,'.')
           iflag=2
         end if
       end if
       if(iflag.eq.1)then
         write(iunit,300)
300      format(/,t2,'Name',t15,'Sigma')
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             ies=ies+1
             write(iunit,320) trim(apar(ipar)),sigma(ies)
320          format(t2,a,t15,1pg12.5)
           end if
         end do
       else
         write(iunit,330)
330      format(/,' Parameter covariance matrix follows:-',/)
         call writhed(ifail,iunit,8,nespar,adjapar)
         call writarr(ifail,nespar,iunit,8,nespar,cp,adjapar)
       end if

       write(iunit,*)
       write(iunit,340)
340    format(/,t28,'RECORD OF OPTIMISATION PROCESS')
       write(iunit,*)

#ifdef FLUSHFILE
       call flush(iunit)
#endif

       return

       end


        subroutine writhed(ifail,iunit,num,ndim,ahead)

        integer num,ndim,j1,j2,iunit,ifail,i,j
        character*(*) ahead(ndim)

        ifail=0
        j1=1
        j2=num

        if(ndim.ge.num)then
          do 30 i=1,ndim/num
            write(iunit,20,err=9000) (ahead(j),j=j1,j2)
20          format(1x,13x,8(a12,1x))
            j1=j1+8
            j2=j2+8
30        continue
        end if
        if(ndim.ge.j1)write(iunit,20,err=9000) (ahead(j),j=j1,ndim)
        if(ndim.gt.num) write(iunit,*,err=9000)
        return

9000    ifail=1
        return

        end




        subroutine writarr(ifail,lda,iunit,num,ndim,rarray,aname)

        integer num,ndim,j1,j2,iunit,ifail,i,j,ii,lda,iflag
        double precision rarray(lda,ndim)
        character*(*) aname(ndim)

        ifail=0

        do 50 ii=1,ndim
          j1=1
          j2=num
          iflag=1
          if(ndim.ge.num)then
            do 30 i=1,ndim/num
              if(iflag.eq.1)then
                write(iunit,20,err=9000)
     +          aname(ii),(rarray(ii,j),j=j1,j2)
20              format(1x,a12,1x,8(1x,1pg11.4,1x))
                iflag=0
              else
                write(iunit,21,err=9000) (rarray(ii,j),j=j1,j2)
21              format(1x,12x,1x,8(1x,1pg11.4,1x))
              end if
              j1=j1+8
              j2=j2+8
30          continue
          end if
          if(ndim.ge.j1)then
            if(iflag.eq.1)then
              write(iunit,20,err=9000)
     +        aname(ii),(rarray(ii,j),j=j1,ndim)
              iflag=0
            else
              write(iunit,21,err=9000) (rarray(ii,j),j=j1,ndim)
            end if
          end if
          if(ndim.gt.num) write(iunit,*,err=9000)
50      continue
        return

9000    ifail=1
        return

        end



       subroutine feval(ifail,xx,yy,irecord)

C -- Subroutine FEVAL undertakes a function evaluation. The output of this
C    is an objective function value.

       use subiter1
       use pestdata
       use modeldata
       use model_input_output_interface
       implicit none

       integer, intent(out)  :: ifail
       real, intent(in)      :: xx(:)
       real, intent(out)     :: yy
       integer, intent(in)   :: irecord

       integer          :: jfail,ipar,i

C -- Initialisation

       ifail=0

C -- First the parameter values to employ on the current run are computed.

       call adj2all(xx)

       if(ms_status.ne.0)then
         call modsub_mrun(ifail,pval,scale,offset,modval_o,errmsg)
         if(jfail.ne.0) go to 9000
         go to 100
       end if

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

       call mio_read_model_output_files(jfail,nobs,aobs,modval_o,
     + instruction)
       if(jfail.ne.0) go to 9000

C -- The following should not be necessary unless there is prior information,
C    but is done for safety.

       do ipar=1,npar
         pval(ipar)=(pval(ipar)-offset(ipar))/scale(ipar)
       end do

100    continue

C -- Prior information is evaluated.

       if(nprior.ne.0) then
         call priorcalc(modval_o)
       end if

! -- The objective function is now evaluated.

       call objclc(modval_o,yy)

       if((usehist.gt.0).and.(irecord.gt.0))then
         call record_history(yy,xx,modval_o)
       end if

C -- Contributions to the objective function are placed into a real
C    (rather than double precision) array.

       return

9000   continue
       if(forgive_failure.eq.0)then
         yy=-1.1e35
         ifail=11
       else
         yy=phi_fail
         modval_o=0.0  !an array
         do i=1,nobsgp
           phid_contrib(i)=phi_fail
         end do
       end if
       return

       end


       subroutine record_history(phi,xval,obsval)

       use pestdata
       use modeldata
       implicit none

       real, intent(in)             :: phi
       real, intent(in)             :: xval(:)
       double precision, intent(in) :: obsval(:)

       integer                      :: irow,i,ii,ies
       real                         :: rtemp

       irecord_svd=irecord_svd+1
       if(irecord_svd-irecord_svd_last.ge.nespar+1)then
         usehist_test=1
       end if
       if(nhist.lt.nespar+1)then
         nhist=nhist+1
       else
         nhist=1
       end if
       do irow=1,nxrow
         obshist(irow,nhist)=obsval(irow)
       end do
       phihist(nhist)=phi
       do ies=1,nespar
         xxhist(ies,nhist)=xval(ies)
       end do

       return

       end



       subroutine svd_upgrade()

       use pestdata
       use modeldata

       implicit none

       integer                :: i,ilow,icol,ihist,ies,irow,info,
     +                           jes,iflag,itemp,k,keig
       real                   :: rtemp,fac,rtemp1,rtemp2
       double precision       :: dtemp,dtemp1,dtemp2,delta

       integer ldvt,ldu
       real vt(1,1),u(1,1)

       z=0.0d0            ! an array
       ldvt=1
       ldu=1

! -- Eigenvalue threshold ratios are assigned.

       seigthresh(1)=1.0e-1
       if(neig.gt.1)then
         do keig=2,neig
           seigthresh(keig)=seigthresh(keig-1)/10.0
         end do
       end if

! -- The column with lowest objective function is found.

       rtemp=huge(rtemp)
       do i=1,nespar+1
         if(phihist(i).lt.rtemp) then
           ilow=i
           rtemp=phihist(i)
         end if
       end do

! -- The Jacobian matrix is formed.

       icol=0
       do ihist=1,nespar+1
         if(ihist.eq.ilow) cycle
         icol=icol+1
         dtemp=0.0d0
         do ies=1,nespar
           dtemp1=xxhist(ies,ihist)-xxhist(ies,ilow)
           dtemp=dtemp+dtemp1*dtemp1
         end do
         delta=sqrt(dtemp)
         if(delta.le.0.0d0)then
           do irow=1,nxrow
             z(irow,icol)=0.0d0
           end do
         else
           do irow=1,nxrow
             z(irow,icol)=(obshist(irow,ihist)-obshist(irow,ilow))/delta
           end do
         end if
       end do

! -- The weighted residuals matrix is formed.

       if(numcov.gt.0)then
         write(6,*) ' Not programmed yet.'
         stop
       end if
       do irow=1,nxrow
         rswork(irow)=(oval(irow)-obshist(irow,ilow))*owght(irow)
       end do

! -- The vector ZtQr is formed.

       do ies=1,nespar
         dtemp=0.0d0
         do irow=1,nxrow
           dtemp=dtemp+z(irow,ies)*rswork(irow)
         end do
         ztqr(ies)=dtemp
       end do

! -- SVD is undertaken of the Z matrix.

       call sgesvd('O','N',nxrow,nespar,z,nxrow,e,u,ldu,vt,ldvt,rswork,
     + lswork,info)
       if(info.ne.0)then
         do keig=1,neig
           xx_svd(1,keig)=-1.1e35
         end do
         return
       end if

! -- We square the eigenvalue matrix and find where the threshold is.


       do keig=1,neig
         nthresh(keig)=nespar
         rtemp=e(1)*e(1)
         do ies=1,nespar
           f(ies)=e(ies)*e(ies)
           if(f(ies)/rtemp.gt.seigthresh(keig))then
             f(ies)=1.0/f(ies)
           else
             nthresh(keig)=ies-1
             go to 30
           end if
         end do
30       continue
         if(keig.gt.1)then
           if(nthresh(keig).eq.nthresh(keig-1))then
             xx_svd(1,keig)=-1.1e35
             go to 200
           end if
         end if

! -- We now compute V1E-1Vt

         do ies=1,nespar
           do jes=1,ies
             dtemp=0.0
             do k=1,nthresh(keig)
               dtemp=dtemp+z(ies,k)*f(k)*z(jes,k)
             end do
             norm(jes,ies)=dtemp
           end do
         end do
         do ies=1,nespar-1
           do jes=ies+1,nespar
             norm(jes,ies)=norm(ies,jes)
           end do
         end do

! -- We now evaluate the parameter upgrade vector.

         do ies=1,nespar
           dtemp=0.0
           do jes=1,nespar
             dtemp=dtemp+norm(ies,jes)*ztqr(jes)
           end do
           rswork(ies)=dtemp
         end do

! -- Now the "parameters" here are actually the multipliers of unit vectors in parameter
!    space. The upgrade vector must be translated to real parameters.
!    We begin by putting into the z array the actual unit parameter vectors.

         icol=0
         do ies=1,nespar
           icol=icol+1
           if(ies.eq.ilow) icol=icol+1
           dtemp=0.0d0
           do jes=1,nespar
             dtemp1=xxhist(jes,icol)-xxhist(jes,ilow)
             norm(jes,ies)=dtemp1
             dtemp=dtemp+dtemp1*dtemp1
           end do
           dtemp=sqrt(dtemp)
           if(dtemp.gt.0.0d0)then
             do jes=1,nespar
               norm(jes,ies)=norm(jes,ies)/dtemp
             end do
           else
             do jes=1,nespar
               norm(jes,ies)=0.0d0
             end do
           end if
         end do

         do ies=1,nespar
           dtemp=0.0d0
           do jes=1,nespar
             dtemp=dtemp+norm(ies,jes)*rswork(jes)
           end do
           xx_svd(ies,keig)=dtemp+xxhist(ies,ilow)
         end do

! -- If the parameter vector is out of bounds it is shortened.

         fac=huge(rtemp)
         iflag=0
         do ies=1,nespar
           itemp=0
           if(xx_svd(ies,keig).gt.sparubnd(ies))then
             rtemp1=sparubnd(ies)-xxhist(ies,ilow)
             itemp=1
           else if(xx_svd(ies,keig).lt.sparlbnd(ies))then
             rtemp1=sparlbnd(ies)-xxhist(ies,ilow)
             itemp=1
           end if
           if(itemp.ne.0)then
             iflag=1
             rtemp2=xx_svd(ies,keig)-xxhist(ies,ilow)
             if(rtemp2.eq.0.0)then
               xx_svd(1,keig)=-1.1e35
               go to 200
             end if
             rtemp1=rtemp1/rtemp2
             if(rtemp1.lt.fac) fac=rtemp1
           end if
         end do
         if(iflag.ne.0)then
           if(fac.lt.1.0e-3)then
             xx_svd(1,keig)=-1.1e35
             go to 200
           else
             do ies=1,nespar
               xx_svd(ies,keig)=fac*(xx_svd(ies,keig)-xxhist(ies,ilow))
     +         +xxhist(ies,ilow)
             end do
           end if
         end if

200      continue
       end do

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




       subroutine mul_feval(ifail,nrun,xx,yy,adjust_flag,num_extra)

C -- Subroutine MUL_FEVAL undertakes multiple function evaluations. The output of this
C    is a set of objective function values.

       use subiter2
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
       integer, intent(out)  :: adjust_flag
       integer, intent(out)  :: num_extra

       integer          :: irun,ipar,i,j,istop,nrun1,jfail,ii,ies,
     +                     keig,jeig,svd_appflag,resunit1
       integer          :: jjflag
       integer          :: nextunit
       real             :: yy_temp,rtemp
       double precision :: minobjfn
       character*10     :: arun

C -- Initialisation

       ifail=0
       mul_feval_call=mul_feval_call+1

C -- Restart data is written.

       if((mul_feval_call.eq.1).or.(usehist.eq.1))then
         resunit1=nextunit()
         open(unit=resunit1,file=restartfile_main,form='unformatted')
         write(resunit1) usehist,forgive_failure
         if(usehist.ne.0)then
           write(resunit1) svd_app,neig,irecord_svd,
     +     irecord_svd_last,usehist_test,nhist,maxrun
           write(resunit1) phi_lowest
           write(resunit1)((xxhist(i,j),i=1,nespar),j=1,nespar+1)
           write(resunit1)
     +     ((obshist(i,j),i=1,nxrow),j=1,nespar+1)
           write(resunit1)(phihist(i),i=1,nespar+1)
         end if
         close(unit=resunit1)
       end if

       adjust_flag=0
       minobjfn=huge(minobjfn)
       nrun1=nrun
       if(usehist_test.eq.1)then
         usehist_test=0
         irecord_svd_last=irecord_svd
         call svd_upgrade()
         do keig=1,neig
           if(xx_svd(1,keig).gt.-1.0d35) nrun1=nrun1+1
         end do
       end if
       call writint(arun,nrun1)
       write(6,10) trim(arun)
10     format(/,' - running model ',a,' times...')

       if(ipll.eq.0)then
         keig=0
         do irun=1,nrun1
           if(irun.le.nrun)then
             call feval(ifail,xx(:,irun),yy_temp,1)
           else
11           keig=keig+1
             if(xx_svd(1,keig).lt.-1.0d35) go to 11
             call feval(ifail,xx_svd(:,keig),yy_temp,0)
           end if
           if(ifail.ne.0) go to 9000
C -- The following lines are placed here (and repeated below) before
C    another call to objclc (in feval) overwrites phid_contrib.
           if(yy_temp.lt.minobjfn)then
             minobjfn=yy_temp
             do i=1,nobsgp
               phi_contrib(i)=phid_contrib(i)
             end do
           end if
           if(irun.le.nrun)then
             yy(irun)=yy_temp
           else
             yy_svd(keig)=yy_temp
           end if
           call stopress(istop)
           if(istop.eq.1)then
             ifail=-1
             go to 9000
           else if(istop.eq.2)then
             ifail=-2
             go to 9000
           end if
           if(irun.eq.1)then
             write(6,256)
256          format(' - number of runs completed...')
             write(6,'(a)',advance='no') '   '
           end if
           write(6,257,advance='no') irun
257        format(i6)
           if((irun.eq.nrun1).or.(mod(irun,12).eq.0))then
             write(6,*)
             write(6,'(a)',advance='no') '   '
           end if
         end do
       else
         irestart_pll=0
         itn=itn+1
         keig=0
         do irun=1,nrun1
           if(irun.le.nrun)then
             call adj2all(xx(:,irun))
           else
258          keig=keig+1
             if(xx_svd(1,keig).lt.-1.0d35) go to 258
             call adj2all(xx_svd(:,keig))
           end if
           do ipar=1,npar
             pval(ipar)=pval(ipar)*scale(ipar)+offset(ipar)
           end do
           do ipar=1,npar
             parreg(ipar,irun)=pval(ipar)
           end do
         end do
         call prm_doruns(ifail,itn,npar,nobs,nrun1,npar,nxrow,parreg,
     +                        obsreg,apar,aobs,irestart_pll,
     +                        restfile_pll,forgive_failure)
         if(ifail.ne.0) then
           if(forgive_failure.eq.0)go to 9000
         end if

         keig=0
         do irun=1,nrun1
           if(irun.gt.nrun)then
259          keig=keig+1
             if(xx_svd(1,keig).lt.-1.0d35) go to 259
           end if

C -- The following should not be necessary unless there is prior information but
C    is done for safety.

           do ipar=1,npar
             pval(ipar)=(parreg(ipar,irun)-offset(ipar))/scale(ipar)
           end do

           if(nprior.ne.0)then
             call priorcalc(obsreg(:,irun))
           end if
           if((obsreg(1,irun).lt.1.235d300).and.
     +        (obsreg(1,irun).gt.1.233d300))then
             yy_temp=phi_fail
             do i=1,nxrow
               obsreg(i,irun)=0.0
             end do
             do i=1,nobsgp
               phid_contrib(i)=phi_fail
             end do
           else
             call objclc(obsreg(:,irun),yy_temp)
           end if
           if(yy_temp.lt.minobjfn)then
             minobjfn=yy_temp
             do i=1,nobsgp
               phi_contrib(i)=phid_contrib(i)
             end do
           end if
           if(irun.le.nrun)then
             yy(irun)=yy_temp
           else
             yy_svd(keig)=yy_temp
           end if
           if((usehist.gt.0).and.(irun.le.nrun))then
             call record_history(yy(irun),xx(:,irun),obsreg(:,irun))
           end if
         end do
       end if

C -- We keep the lowest ever objective function for posterity.

       do irun=1,nrun
         if(yy(irun).lt.phi_lowest) phi_lowest=yy(irun)
       end do
       if(nrun1.gt.nrun)then
         svd_appflag=0
         do keig=1,neig
           if(xx_svd(1,keig).gt.-1.0e35)then
             if(yy_svd(keig).lt.phi_lowest) then
               phi_lowest=yy_svd(keig)
               svd_appflag=1
             end if
           end if
         end do
       end if

C -- Do we need to replace a sampled parameter set with one determined by SVD?
C -- First we find the best of our SVD runs.

       if(nrun1.gt.nrun)then
         rtemp=huge(rtemp)
         do keig=1,neig
           if(xx_svd(1,keig).gt.-1.0e35)then
             if(yy_svd(keig).lt.rtemp)then
               rtemp=yy_svd(keig)
               jeig=keig
             end if
           end if
         end do

         jjflag=0
         rtemp=huge(rtemp)
         do irun=1,nrun
           if(yy(irun).lt.rtemp)then
             ii=irun
             rtemp=yy(irun)
           end if
         end do
         if(((svd_app.eq.1).and.(svd_appflag.eq.1)).or.
     +       (svd_app.eq.2))then
           if(yy_svd(jeig).lt.rtemp)then
             adjust_flag=ii
             jjflag=1
             do ies=1,nespar
               xx(ies,ii)=xx_svd(ies,jeig)
             end do
             yy(ii)=yy_svd(jeig)
           end if
         end if

         if((svd_app.eq.2).and.(jjflag.eq.0))then
           rtemp=0.0
           do irun=1,nrun
             if(yy(irun).gt.rtemp)then
               ii=irun
               rtemp=yy(irun)
             end if
           end do
           if(yy_svd(jeig).lt.rtemp)then
             do ies=1,nespar
               xx(ies,ii)=xx_svd(ies,jeig)
             end do
             yy(ii)=yy_svd(jeig)
           end if
         end if

       end if

       num_extra=nrun1-nrun

       return

9000   continue
       return

       end



       subroutine get_contribs(contrib,acontrib)

       use modeldata
       use pestdata
       implicit none

       real, intent(out)          :: contrib(nobsgp)
       character*(*), intent(out) :: acontrib(nobsgp)
       integer             :: i

       do i=1,nobsgp
         contrib(i)=phi_contrib(i)
         acontrib(i)=obgnme(i)
       end do

       return

       end


       subroutine record_parameters(ifail,xx)

C -- Subroutine RECORD_PARAMETERS writes a parameter value file.

       use subiter
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
21      leadblank=i-1
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

