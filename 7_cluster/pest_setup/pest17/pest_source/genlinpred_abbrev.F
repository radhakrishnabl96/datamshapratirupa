       program genlinpred

C -- Program GENLINPRED acts as a driver for PREDVAR*, PREDUNC* and other PEST
C    utilities in order to facilite a variety of error and uncertainty analyses
C    on a PEST input dataset.

       implicit none

       logical        lexist,skipline
       integer, parameter :: MAXSING=2000
       integer        ibeg,iend,nb_pst,iline,icurrent
       integer        pestmode,npar,nobs,npargp,nprior,nobsgp,
     +                nespar,numnonzero,numsup,nespar1
       integer        ipargp,ipar,iobsgp,iobs,ies,jes,jpar
       integer        predpar
       integer        ifail,ierr,nnn
       integer        n,itemp,i,ncol,j,nb
       integer        ncount,mcount,icount,istep,ilist,iicount,icountlo
       integer        isinglo,isingstart,isingfin,numsing,singmin,ising
       integer        ndim1,ndim2,ndim
       integer        ndimsol
       integer        userweight
       integer        irfile,nb_rsp
       integer        nn
       integer        lw(10),rw(10)
#ifdef I64
       integer        system
#endif


       double precision weightfac,refvar,phi,rtemp,var0,var
       double precision varmin
       double precision rtemp1,rtemp2
       double precision precalunc,precalvar,postcalunc,postcalvar,
     +                  precalerr,postcalerr
       double precision pretempl,pretempu,posttempl,posttempu,rtempm

       character*1    apg,aog,ab,aa
       character*1    ap,awe,awu,ace,acu,ag,as,arpur,arper,asne,aid,
     +                aun,ai
       character*1    avchoice
       character*4    aext
       character*12   aversion,aapar
       character*15   aline
       character*20   tonull,atemp,anum,apred
       character*15   preatempl,preatempu,postatempl,postatempu,atempm
       character*25   aprogram,aprogram_lo
       character*25   heading(4)
       character*30   atemp30
       character*40   asection
       character*200  def_uncertfile,scaled_uncertfile,scaled_pestfile,
     +                scaled_jcofile,vecbase,varbase,
     +                tinfile,toutfile,tdatfile,tsenfile,tplistfile,
     +                tslistfile,tslistfile0,
     +                tolistfile_sub,tolistfile_add,
     +                scaled_covfile
       character*200  tempfile
       character*200  outfile
       character*200  uncertfile,pestfile,jcofile,senfile
       character*200  afile,afile1,afile2,bfile
       character*200  rfile,arfile,trailtext
       character*200  astring
       character*300  cline
       character*500  amessage

       integer                       :: sing(2,MAXSING),ss(MAXSING)
       double precision              :: nulterm(2,MAXSING),nt(MAXSING),
     +                                  solterm(2,MAXSING),st(MAXSING),
     +                                  total(2,MAXSING),tt(MAXSING)

       integer,            allocatable :: icovmat(:)
       integer,            allocatable :: nobgnm(:),npargnm(:),itrans(:)
       character (len=12), allocatable :: apar(:),apargp(:),aobsgp(:)
       character (len=20), allocatable :: acontrib(:)
       character (len=20), allocatable :: aobs(:)
       double precision,   allocatable :: lbound(:),ubound(:),pval(:)
       double precision,   allocatable :: ident(:),relerr(:),relunc(:)
       double precision,   allocatable :: senscale(:),predvec(:)
       double precision,   allocatable :: tempvec(:)
       double precision,   allocatable :: contrib(:,:),ocontrib(:)

       integer             :: itype,jpartype,iunit,junit,vardim,covdim
       integer             :: nxrow,nxrow1,ntplfle,ninsfle,irow,kes
       integer             :: itemp1,itemp2
       integer             :: running_supcalc
       double precision    :: sd,dtemp
       double precision    :: parvar(1)
       character (len=12)  :: anumsup
       character (len=12)  :: apar1
       character (len=20)  :: atype,aobs1
       character (len=200) :: scaled_post_uncertfile,scaled_post_covfile
       double precision,   allocatable :: x(:,:)
       double precision,   allocatable :: parcov(:,:)
       character (len=12), allocatable :: aaapar(:)
       character (len=14), allocatable :: aastring(:)

C -- Initialization

       def_uncertfile         = 'p###.unc'
       scaled_uncertfile      = 's###.unc'
       scaled_post_uncertfile = 'sp##.unc'
       scaled_covfile         = 's###.cov'
       scaled_post_covfile    = 'sp##.cov'
       scaled_pestfile        = 'p###.pst'
       scaled_jcofile         = 'p###.jco'
       vecbase                = 'v###'
       varbase                = 's###'

       tinfile                = 't###.in'
       toutfile               = 't###.out'
       tdatfile               = 't###.dat'
       tsenfile               = 't###.vec'
       tplistfile             = 'p###.lst'
       tslistfile             = 's###.lst'
       tslistfile0            = 's##0.lst'
       tolistfile_sub         = 'o##s.lst'
       tolistfile_add         = 'o##a.lst'
       uncertfile=' '

       apg=' '
       aog=' '
       ab=' '
       aa=' '
       ap=' '
       awe=' '
       awu=' '
       ace=' '
       acu=' '
       ag=' '
       as=' '
       arpur=' '
       arper=' '
       asne=' '
       aid=' '
       aun=' '
       ai=' '

       running_supcalc=0

#ifdef UNIX
       tonull=' > /dev/null'
#else
       tonull=' > nul'
#endif

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' GENLINPRED_ABBREV Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

       irfile=0
       rfile=' '
       arfile=' '
       ai='f'

20     write(6,30,advance='no')
30     format(' Enter name of PEST control file: ')
       read(5,'(a)') afile
       call endstrip(afile)
       if(afile.eq.' ') go to 20
       afile=adjustl(afile)
       if((afile(1:2).eq.'e ').or.(afile(1:2).eq.'E '))then
         write(6,*)
         go to 9895
       end if
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,pestfile,ibeg,iend)
       if(ifail.ne.0) go to 20
#ifndef UNIX
       call lowcas(pestfile)
#endif
       nb_pst=len_trim(pestfile)
       nb_pst=nb_pst-3
       if(nb_pst.lt.1)nb_pst=1
       aext=pestfile(nb_pst:)
       call lowcas(aext)
       if(aext.ne.'.pst')then
         write(6,40)
40       format(/,' PEST control file must have an extension of ',
     +   '".pst" - try again.',/)
         go to 20
       end if
       open(unit=10,file=pestfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(pestfile,afile)
         write(6,41) trim(afile)
41       format(/,' Cannot open file ',a,' - try again.',/)
         go to 20
       end if
       call addquote(pestfile,afile)

C -- A check is made that the corresponding Jacobian matrix file is present.

       jcofile=pestfile(1:nb_pst)//'jco'
       inquire(file=jcofile,exist=lexist)
       if(.not.lexist)then
         call addquote(pestfile,afile1)
         call addquote(jcofile,afile2)
         write(amessage,60) trim(afile2),trim(afile1)
60       format('Cannot find JCO file ',a,' corresponding ',
     +   'to PEST control file ',a,'.')
         go to 9890
       end if

C -- Some data is acquired from the PEST control file.

       write(6,*)
       call addquote(pestfile,afile)
       write(6,100) trim(afile)
100    format(' - reading PEST control file ',a,'....')
       iline=1
       read(10,'(a)',err=9000,end=9050) cline
       call lowcas(cline)
       if(cline(1:3).ne.'pcf')then
         write(amessage,110) trim(afile)
110      format('"pcf" header expected at first line of file ',a,'.')
         go to 9890
       end if
1009   iline=iline+1
       read(10,'(a)',err=9000,end=9050) cline
       if(skipline(cline)) go to 1009
1102   iline=iline+1
       read(10,'(a)',err=9000,end=9050) cline
       if(skipline(cline)) go to 1102
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9150
       atemp=cline(lw(2):rw(2))
       call lowcas(atemp)
       if(atemp.eq.'estimation')then
         pestmode=1
       else if(atemp.eq.'prediction')then
         pestmode=2
       else if(atemp(1:5).eq.'regul')then
         pestmode=3
       else if(atemp(1:5).eq.'paret')then
         pestmode=4
       else
         call writint(aline,iline)
         write(amessage,109) trim(aline),trim(afile)
109      format('Unknown mode at line ',a,' of file ',a,'.')
         go to 9890
       end if
       if(pestmode.eq.3)then
         write(amessage,111) trim(afile)
111      format('PEST control file ',a,' instructs PEST to run ',
     +   'in regularisation mode. GENLINPRED will not allow this. ',
     +   'Copy to new PEST control file, alter to "estimation" ',
     +   'mode, remove regularisation ',
     +   'observations and/or prior information equations, and ',
     +   'then run JCO2JCO to build corresponding new JCO file.')
         go to 9890
       else if (pestmode.eq.4)then
         write(amessage,108) trim(afile)
108      format('PEST control file ',a,' instructs PEST to run ',
     +   'in pareto mode. GENLINPRED does not allow this.')
         go to 9890
       end if
1081   iline=iline+1
       read(10,'(a)',err=9000,end=9050) cline
       if(skipline(cline)) go to 1081
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(1):rw(1)),npar)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(2):rw(2)),nobs)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(3):rw(3)),npargp)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(4):rw(4)),nprior)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(5):rw(5)),nobsgp)
       if(ifail.ne.0) go to 9000
       if(npar.le.0) then
         write(amessage,1082)
1082     format('NPAR is non-positive in PEST control file.')
         go to 9890
       end if
       if(nprior.ne.0)then
         write(amessage,112) trim(afile)
112      format('PEST control file ',a,' includes prior information. ',
     +   'GENLINPRED will not allow this. Copy to new PEST control ',
     +   'file, remove prior information, ',
     +   'and then run JCO2JCO to build corresponding new JCO file.')
         go to 9890
       end if
       nxrow=nobs
       allocate(apar(npar),apargp(npargp),aobs(nobs),aobsgp(nobsgp),
     + nobgnm(nobs),npargnm(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(itrans(npar),lbound(npar),ubound(npar),ident(npar),
     + relerr(npar),relunc(npar),pval(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(icovmat(nobsgp),stat=ierr)
       if(ierr.ne.0) go to 9200

1121   iline=iline+1
       read(10,'(a)',err=9000,end=9050) cline
       if(skipline(cline)) go to 1121
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(1):rw(1)),ntplfle)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(2):rw(2)),ninsfle)
       if(ifail.ne.0) go to 9000

       asection='parameter groups'
       do
         iline=iline+1
         read(10,'(a)',err=9000,end=9250) cline
         cline=adjustl(cline)
         call lowcas(cline)
         if(cline(1:1).eq.'*')then
           if(index(cline,'parameter groups').ne.0) exit
         end if
       end do
       do ipargp=1,npargp
1122     iline=iline+1
         read(10,'(a)',err=9000,end=9050) cline
         if(skipline(cline)) go to 1122
         call linspl(ifail,1,lw,rw,cline)
         if(ifail.ne.0) go to 9150
         apargp(ipargp)=cline(lw(1):rw(1))
         call lowcas(apargp(ipargp))
       end do

       asection='parameter data'
       do
         iline=iline+1
         read(10,'(a)',err=9000,end=9250) cline
         cline=adjustl(cline)
         call lowcas(cline)
         if(cline(1:1).eq.'*')then
           if(index(cline,'parameter data').ne.0) exit
         end if
       end do
       icurrent=1
       nespar=0
       do ipar=1,npar
114      continue
         iline=iline+1
         read(10,'(a)',err=9000,end=9050) cline
         if(skipline(cline)) go to 114
         call lowcas(cline)
         nn=index(cline,'=')
         if(nn.eq.1) go to 9000
         if(nn.ne.0) go to 114
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9150
         if(cline(lw(2):rw(2)).eq.'file_parameter') go to 114
         call linspl(ifail,7,lw,rw,cline)
         if(ifail.ne.0) go to 9150
         apar(ipar)=cline(lw(1):rw(1))
         atemp=cline(lw(2):rw(2))
         if(atemp.eq.'log')then
           itrans(ipar)=1
           nespar=nespar+1
         else if(atemp.eq.'none')then
           itrans(ipar)=0
           nespar=nespar+1
         else if((atemp.eq.'fixed').or.(atemp.eq.'tied'))then
           itrans(ipar)=-1
         else
           call writint(aline,iline)
           write(amessage,115) trim(aline),trim(afile)
115        format('Transformation type must be "none", "fixed", ',
     +     '"tied" or "log" at line ',a,' of file ',a,'.')
           go to 9890
         end if
         call drealread(ifail,cline(lw(4):rw(4)),pval(ipar))
         if(ifail.ne.0) go to 9000
         call drealread(ifail,cline(lw(5):rw(5)),lbound(ipar))
         if(ifail.ne.0) go to 9000
         call drealread(ifail,cline(lw(6):rw(6)),ubound(ipar))
         if(ifail.ne.0) go to 9000
         if(itrans(ipar).ge.0)then
           if(ubound(ipar)-lbound(ipar).le.0.0)then
             call writint(aline,iline)
             write(amessage,120) trim(aline),trim(afile)
120          format('Parameter upper bound does not exceed lower ',
     +       'bound at line ',a,' of file ',a,'.')
             go to 9890
           end if
         end if
         if(itrans(ipar).eq.1)then
           if(lbound(ipar).le.0.0)then
             call writint(aline,iline)
             write(amessage,121) trim(aline),trim(afile)
121          format('Parameter is log transformed, yet lower bound is ',
     +       'not positive at line ',a,' of file ',a,'.')
             go to 9890
           end if
         end if
         atemp=cline(lw(7):rw(7))
         call lowcas(atemp)
         call which1(ifail,npargp,icurrent,apargp,atemp)
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,150) trim(atemp),trim(aline),trim(afile)
150        format('Parameter group "',a,'" cited at line ',a,
     +     ' of PEST control file ',a,' is not cited in ',
     +     '"parameter groups" section of this file.')
           go to 9890
         end if
         npargnm(ipar)=icurrent
       end do

       asection='observation groups'
       do
         iline=iline+1
         read(10,'(a)',err=9000,end=9250) cline
         cline=adjustl(cline)
         call lowcas(cline)
         if(cline(1:1).eq.'*')then
           if(index(cline,'observation group').ne.0) exit
         end if
       end do
       do iobsgp=1,nobsgp
1511     iline=iline+1
         read(10,'(a)',err=9000,end=9050) cline
         if(skipline(cline)) go to 1511
         call linspl(ifail,1,lw,rw,cline)
         if(ifail.ne.0) go to 9150
         aobsgp(iobsgp)=cline(lw(1):rw(1))
         call lowcas(aobsgp(iobsgp))
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0)then
           icovmat(iobsgp)=0
         else
           icovmat(iobsgp)=1
           afile1=cline(lw(2):)
           ibeg=1
           iend=len_trim(afile1)
           call getfile(ifail,afile1,tempfile,ibeg,iend)
           if(ifail.ne.0)then
             call writint(aline,iline)
             write(amessage,151) trim(aline),trim(afile)
151          format('Cannot read name of covariance matrix file at ',
     +       'line ',a,' of file 'a,'.')
             go to 9890
           end if
           inquire(file=tempfile,exist=lexist)
           if(.not.lexist)then
             call writint(aline,iline)
             call addquote(tempfile,afile1)
             write(amessage,152) trim(afile1),trim(aline),trim(afile)
152          format('Cannot find covariance matrix file ',a,
     +       ' cited at line ',a,' of file ',a,'.')
             go to 9890
           end if
         end if
       end do

       asection='observation data'
       numnonzero=0
       do
         iline=iline+1
         read(10,'(a)',err=9000,end=9250) cline
         cline=adjustl(cline)
         call lowcas(cline)
         if(cline(1:1).eq.'*')then
           if(index(cline,'observation data').ne.0) exit
         end if
       end do
       icurrent=1
       do iobs=1,nobs
1521     iline=iline+1
         read(10,'(a)',err=9000,end=9050) cline
         if(skipline(cline)) go to 1521
         call linspl(ifail,4,lw,rw,cline)
         if(ifail.ne.0) go to 9150
         aobs(iobs)=cline(lw(1):rw(1))
         call lowcas(aobs(iobs))
         atemp=cline(lw(4):rw(4))
         call lowcas(atemp)
         call which1(ifail,nobsgp,icurrent,aobsgp,atemp)
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,160) trim(atemp),trim(aline),trim(afile)
160        format('Observation group "',a,'" cited at line ',a,
     +     ' of PEST control file ',a,' is not cited in ',
     +     '"observation groups" section of this file.')
           go to 9890
         end if
         nobgnm(iobs)=icurrent
         if(icovmat(icurrent).eq.0)then
           call drealread(ifail,cline(lw(3):rw(3)),rtemp)
           if(ifail.ne.0) go to 9000
           if(rtemp.gt.0.0d0) numnonzero=numnonzero+1
         end if
       end do

C -- The number of non-zero-weighted observations is corrected for covariance matrices.

       do iobsgp=1,nobsgp
         if(icovmat(iobsgp).ne.0)then
           do iobs=1,nobs
             if(nobgnm(iobs).eq.iobsgp) numnonzero=numnonzero+1
           end do
         end if
       end do

       close(unit=10)
       write(6,190) trim(afile)
190    format(' - file ',a,' read ok.')

C -- It is now ascertained whether an uncertainty file needs to be written, or whether
C    the user has supplied one.

       write(6,*)
250    write(6,260,advance='no')
260    format(' Use bounds or uncert file for param ',
     + 'uncertainties  [b/u] <Enter> if "b": ')
       read(5,'(a)') ab
       call endstrip(ab)
       if(ab.eq.' ')ab='b'
       if((ab.eq.'e').or.(ab.eq.'e'))then
         deallocate(apar,apargp,aobs,aobsgp,nobgnm,npargnm)
         deallocate(itrans,lbound,ubound,ident,relerr,relunc,pval)
         deallocate(icovmat)
         write(6,*)
         go to 20
       else if((ab.eq.'B').or.(ab.eq.'b'))then
         ab='b'
       else if((ab.eq.'U').or.(ab.eq.'u'))then
         ab='u'
       else
         go to 250
       end if

C -- The name of the uncertainty file is aquired if appropropriate.

279    continue
       if(ab.eq.'u')then
280      write(6,290,advance='no')
290      format(' Enter name of parameter uncertainty file: ')
         read(5,'(a)') afile
         call endstrip(afile)
         if(afile.eq.' ') go to 280
         afile=adjustl(afile)
         if((afile(1:2).eq.'e ').or.(afile(1:2).eq.'E '))then
           write(6,*)
           go to 250
         end if
         ibeg=1
         iend=len_trim(afile)
         call getfile(ifail,afile,uncertfile,ibeg,iend)
         if(ifail.ne.0) go to 280
#ifndef UNIX
         call lowcas(uncertfile)
#endif
         inquire(file=uncertfile,exist=lexist)
         if(.not.lexist)then
           call addquote(uncertfile,afile)
           write(6,300) trim(afile)
300        format(/,' File ',a,' does not exist - try again.',/)
           go to 280
         end if
         call addquote(uncertfile,afile)
       end if

C -- The relationship between observation weights and measurement uncertainty is now determined.

       write(6,*)
350    continue
       userweight=0
       write(6,360,advance='no')
360    format(' Are weights the inverse of measurement ',
     + 'uncertainty?  [y/n] <Enter> if "y": ')
       read(5,'(a)') aa
       call endstrip(aa)
       if(aa.eq.' ')aa='y'
3601   continue
       if((aa.eq.'e').or.(aa.eq.'E'))then
         write(6,*)
         if(ab.eq.'u')then
           go to 279
         else
           go to 250
         end if
       else if((aa.eq.'Y').or.(aa.eq.'y'))then
         weightfac=1.0d0
       else if((aa.eq.'N').or.(aa.eq.'n'))then
       else
         go to 350
       end if
3801   continue
       if((aa.eq.'N').or.(aa.eq.'n'))then
380      write(6,390,advance='no')
390      format(' Enter factor for weights to make this so: ')
         read(5,'(a)') atemp30
         call endstrip(atemp30)
         if(atemp30.eq.' ') go to 380
         atemp30=adjustl(atemp30)
         call lowcas(atemp30)
         if(atemp30(1:2).eq.'e ')then
           write(6,*)
           go to 350
         end if
         call drealread(ifail,atemp30,weightfac)
         if(ifail.ne.0) go to 380
         if(weightfac.le.0.0d0) go to 380
         userweight=1
         atemp30=adjustl(atemp30)
       end if
       refvar=1.0d0/weightfac/weightfac

C -- The name of the output file is ascertained.

       write(6,*)
630    write(6,640,advance='no')
640    format(' Enter name for output file ',
     + '<Enter> if genlinpred_abbrev.out: ')
       read(5,'(a)') afile
       call endstrip(afile)
       if(afile.eq.' ') afile='genlinpred_abbrev.out'
       afile=adjustl(afile)
       if((afile(1:2).eq.'E ').or.(afile(1:2).eq.'e '))then
         write(6,*)
         if(userweight.eq.1)then
           aa='n'
           go to 3801
         else
           go to 350
         end if
       end if
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,outfile,ibeg,iend)
       if(ifail.ne.0) go to 630
#ifndef UNIX
       call lowcas(outfile)
#endif
       call addquote(outfile,afile)

C -- The user is asked for details of parameter identifiability calculations (including whether
C    these calculations are required at all).

       ag='y'
       aid='y'
3761   continue
       arper='n'
       as='y'
3781   continue
       arpur='y'

C -- The user is asked for details of predictive/parameter error/uncertainty analysis (including
C    if such analysis is required at all).

3941   continue
       write(6,*)
395    write(6,396,advance='no')
396    format(' Perform comprehensive analysis of a prediction/',
     + 'param? [y/n] <Enter> if "y": ')
       read(5,'(a)') ap
       call endstrip(ap)
       if(ap.eq.' ')ap='y'
       if((ap.eq.'e').or.(ap.eq.'E'))then
         write(6,*)
         go to 630
       else if((ap.eq.'Y').or.(ap.eq.'y'))then
         ap='y'
       else if((ap.eq.'N').or.(ap.eq.'n'))then
         ap='n'
       else
         go to 395
       end if

4501   continue
       if(ap.eq.'y')then
450      write(6,460,advance='no')
460      format('     Enter name of prediction/parameter to analyze: ')
         read(5,'(a)') apred
         call endstrip(apred)
         if(apred.eq.' ') go to 450
         call lowcas(apred)
         if(apred(1:2).eq.'e ')then
           write(6,*)
           go to 395
         end if
         apred=adjustl(apred)
       end if
4801   continue
       if(ap.eq.'y')then
480      write(6,490,advance='no')
490      format('     Enter file to read its sensitivities ',
     +   '["p" if a parameter]: ')
         read(5,'(a)') afile
         call endstrip(afile)
         afile=adjustl(afile)
         if(afile.eq.' ') go to 480
         afile=adjustl(afile)
         if((afile(1:2).eq.'E ').or.(afile(1:2).eq.'e '))then
           write(6,*)
           go to 4501
         end if
         ibeg=1
         iend=len_trim(afile)
         call getfile(ifail,afile,senfile,ibeg,iend)
         if(ifail.ne.0) go to 480
#ifndef UNIX
         call lowcas(senfile)
#endif
         if(senfile.eq.'P')senfile='p'
         if(senfile.ne.'p')then
           inquire(file=senfile,exist=lexist)
           if(.not.lexist)then
             call addquote(senfile,afile)
             write(6,489) trim(afile)
489          format(/,'     File ',a,' does not exist - try again.',/)
             go to 480
           end if
           call addquote(senfile,afile)
         else
           do ipar=1,npar
             if(apred.eq.apar(ipar)) go to 492
           end do
           call addquote(pestfile,afile)
           write(6,491) trim(apred),trim(afile)
491        format(/,' Parameter "',a,'" is not featured in PEST ',
     +     'control file ',a,'.',/)
           go to 4501
492        continue
           if(itrans(ipar).lt.0)then
             call addquote(pestfile,afile)
             write(6,493) trim(apred),trim(afile)
493          format(/,' Parameter "',a,'" is not ',
     +       'adjustable in PEST control file ',a,'.',/)
             go to 4501
           end if
           predpar=ipar
         end if
       else
         apred=' '
       end if

4821   continue
       asne='n'
       if(apred.ne.' ')then
         asne='y'
         aun='y'
         ace='n'
         acu='y'
         apg='g'
         awe='n'
         awu='y'
         aog='g'
       end if

C -- All input data has now been acquired.

C -- The formulation used by PREDUNC-suite programs is now set.

       if(nobs.lt.nespar)then
         avchoice='2'
       else
         avchoice='1'
       end if

C -- Data is recorded to the output file.

#ifdef LAHEY
#ifdef LF90
       open(unit=20,file=outfile,action='readwrite,denynone')
#else
       open(unit=20,file=outfile,action='write')
#endif
#else
       open(unit=20,file=outfile,action='write')
#endif
       write(20,*)
       write(20,*)
       write(20,700)
700    format(t20,51('#'))
       write(20,710)
710    format(t20,'#',49(' '),'#')
       write(20,730)
730    format(t20,'#      GENLINPRED UNCERTAINTY/ERROR ANALYSIS',
     + t70,'#')
       write(20,710)
       write(20,700)
       do i=1,3
         write(20,*)
       end do
       write(20,740)
740    format(t20,'ANALYSIS DETAILS')
       write(20,750)
750    format(t20,'----------------')

       write(20,*)
       call addquote(pestfile,afile)
       write(20,760) trim(afile)
760    format(t5,'Name of PEST control file',t65,': ',a)
       write(atemp,'(1pg12.5)') weightfac
       atemp=adjustl(atemp)
       write(20,765) trim(atemp)
765    format(t5,'Factor by which to multiply observation ',
     + 'weights',t65,': ',a)
       if(ab.eq.'b')then
         astring='parameter bounds'
       else
         astring='uncertainty file'
       end if
       write(20,770) trim(astring)
770    format(t5,'Source of parameter uncertainty data',t65,': ',a)
       if(ab.eq.'u')then
         call addquote(uncertfile,afile)
         write(20,780) trim(afile)
780      format(t5,'Name of uncertainty file',t65,': ',a)
       end if

       write(20,*)
       if(ag.eq.'n')then
         atemp='no'
       else
         atemp='yes'
       end if
       write(20,789) trim(atemp)
789    format(t5,'Perform global parameter estimability analysis',
     + t65,': ',a)
       if(ag.eq.'y')then
         if(aid.eq.'n')then
           atemp='no'
         else
           atemp='yes'
         end if
         write(20,788) trim(atemp)
788      format(t10,'Parameter identifiability',t65,': ',a)
         if(arper.eq.'n')then
           atemp='no'
         else
           atemp='yes'
         end if
         write(20,787) trim(atemp)
787      format(t10,'Relative parameter error reduction',t65,': ',a)
         if(arpur.eq.'n')then
           atemp='no'
         else
           atemp='yes'
         end if
         write(20,786) trim(atemp)
786      format(t10,'Relative parameter uncertainty reduction',
     +   t65,': ',a)
       end if

       write(20,*)
       if(ap.eq.'y')then
         atemp='yes'
       else
         atemp='no'
       end if
       write(20,790) trim(atemp)
790    format(t5,'Analyse error/uncertainty of an entity',t65,': ',a)
       if(ap.eq.'y')then
         write(20,800) trim(apred)
800      format(t10,'Name of entity',t65,': ',a)
         if(senfile.eq.'p')then
           atemp='parameter'
         else
           atemp='prediction'
         end if
         write(20,810) trim(atemp)
810      format(t10,'Type of entity',t65,': ',a)
         if(senfile.ne.'p')then
           call addquote(senfile,afile)
           write(20,820) trim(afile)
820        format(t10,'File containing prediction sensitivities',t65,
     +     ': ',a)
         end if

         write(20,*)
         if(asne.eq.'y')then
           atemp='yes'
         else
           atemp='no'
         end if
         write(20,821) trim(atemp)
821      format(t10,'Total error variance',t65,': ',a)
         if(aun.eq.'y')then
           atemp='yes'
         else
           atemp='no'
         end if
         write(20,819) trim(atemp)
819      format(t10,'Total uncertainty',t65,': ',a)

         if(ace.eq.'y')then
           atemp='yes'
         else
           atemp='no'
         end if
         write(20,*)
         write(20,822) trim(atemp)
822      format(t10,'Parameter contributions to error ',
     +   'variance',t65,': ',a)
         if(acu.eq.'y')then
           atemp='yes'
         else
           atemp='no'
         end if
         write(20,823) trim(atemp)
823      format(t10,'Parameter contributions to uncertainty',
     +   t65,': ',a)
         if((ace.eq.'y').or.(acu.eq.'y'))then
           if(apg.eq.'g')then
             atemp='by group'
           else
             atemp='individual'
           end if
           write(20,830) trim(atemp)
830        format(t10,'Parameter contributions',t65,': ',a)
         end if

         write(20,*)
         if(awe.eq.'y')then
           atemp='yes'
         else
           atemp='no'
         end if
         write(20,832) trim(atemp)
832      format(t10,'Observation worth in reducing error variance',
     +   t65,': ',a)
         if(awu.eq.'y')then
           atemp='yes'
         else
           atemp='no'
         end if
         write(20,833) trim(atemp)
833      format(t10,'Observation worth in reducing uncertainty ',
     +   'variance',t65,': ',a)
         if((awe.eq.'y').or.(awu.eq.'y'))then
           if(aog.eq.'g')then
             atemp='by group'
           else
             atemp='individual'
           end if
           write(20,840) trim(atemp)
840        format(t10,'Observation worth',t65,': ',a)
         end if
       end if

#ifdef FLUSHFILE
       call flush(20)
#endif

C -- More memory is allocated.

       allocate(senscale(nespar),predvec(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

       if(apg.eq.'g')then
         ndim1=npargp+1
       else
         ndim1=nespar+1
       end if
       allocate(contrib(2,ndim1),stat=ierr)
       if(ierr.ne.0) go to 9200

       if(aog.eq.'g')then
         ndim2=nobsgp+1
       else
         ndim2=1
       end if
       ndim=max(ndim1,ndim2)
       allocate(acontrib(ndim),stat=ierr)
       if(ierr.ne.0) go to 9200

       if(aog.eq.'g')then
         ndim2=nobsgp+1
       else if(aog.eq.'i')then
         ndim2=nobs+1
       else
         ndim2=1
       end if
       allocate(ocontrib(ndim2),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- Now the work begins.

C -- First we build a covariance matrix for existing parameters.

       allocate(aaapar(nespar),parcov(nespar,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       parcov=0.0d0          ! an array
       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           aaapar(ies)=apar(ipar)
         end if
       end do

       if(ab.eq.'b')then
         jpartype=1
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             ies=ies+1
             if(itrans(ipar).eq.1)then
               rtemp=log10(ubound(ipar))-log10(lbound(ipar))
             else
               rtemp=ubound(ipar)-lbound(ipar)
             end if
             parcov(ies,ies)=(rtemp*0.25)*(rtemp*0.25)
           end if
         end do
       else
         call addquote(uncertfile,afile)
         write(6,4001) trim(afile)
4001     format(/,' - reading parameter uncertainty file ',a,'...')
         iunit=31
         open(unit=iunit,file=uncertfile,status='old',iostat=ierr)
         if(ierr.ne.0)then
           write(amessage,4002) trim(afile)
4002       format(' Cannot open uncertainty file ',a,'.')
           go to 9890
         end if
         junit=32
         itype=2
         vardim=1
         covdim=nespar
         atype='parameter'
         call read_uncert_data_file(ifail,iunit,junit,itype,jpartype,
     +   nespar,vardim,covdim,parvar,parcov,atype,uncertfile,amessage,
     +   cline,aaapar)
         if(ifail.ne.0) go to 9890
         write(6,4101) trim(afile)
4101     format(' - parameter uncertainty file ',a,' read ok.')
         jpartype=1
         do ies=1,nespar
           do jes=1,nespar
             if(ies.ne.jes)then
               if(parcov(ies,jes).ne.0.0)then
                 jpartype=2
                 go to 4102
               end if
             end if
           end do
         end do
4102     continue
       end if

C -- Scaling factors are now evaluated if this is warranted.

       if(apred.ne.' ')then
         do ies=1,nespar
           senscale(ies)=sqrt(parcov(ies,ies))
         end do
       end if

C -- The new PEST control file is written. But first the old PEST file is re-opened.

       call addquote(pestfile,afile)
       write(6,568)
568    format(' - writing scaled PEST control file...')
       open(unit=10,file=pestfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,569) trim(afile)
569      format(' Cannot re-open PEST control file ',a,'.')
         go to 9890
       end if
       open(unit=31,file=scaled_pestfile)
       do i=1,3
5691     continue
         read(10,'(a)',err=9225,end=9225) cline
         if(skipline(cline)) go to 5691
         write(31,'(a)') trim(cline)
       end do
5692   continue
       read(10,'(a)',err=9225,end=9225) cline
       if(skipline(cline)) go to 5692
       write(31,570) nespar,nobs,1,nprior,nobsgp
570    format(5i7)
5701   continue
       read(10,'(a)',err=9225,end=9225) cline
       if(skipline(cline)) go to 5701
       write(31,575) ntplfle,ninsfle,'single   point'
575    format(i4,i4,2x,a)
5751   continue
       read(10,'(a)',err=9225,end=600) cline
       if(skipline(cline)) go to 5751
       write(31,'(a)') ' 10.0  2.0  0.1  0.03  10'
5752   continue
       read(10,'(a)',err=9225,end=600) cline
       if(skipline(cline)) go to 5752
       write(31,'(a)') ' 10.0 10.0  0.01'
5753   continue
       read(10,'(a)',err=9225,end=600) cline
       if(skipline(cline)) go to 5753
       write(31,'(a)') '0.1'
       do
5754     continue
         read(10,'(a)',err=9225,end=600) cline
         if(skipline(cline)) go to 5754
         write(31,'(a)') trim(cline)
         if(index(cline,'*').ne.0)then
           if(cline(1:1).eq.'*')then
             call lowcas(cline)
             if(index(cline,'parameter gr').ne.0) go to 620
           end if
         end if
       end do
600    call addquote(pestfile,afile)
       write(amessage,610) trim(afile)
610    format(' Cannot find "parameter groups" section of PEST ',
     + 'control file ',a,'.')
       go to 9890
620    continue
       write(31,631)
631    format(' pargroup absolute 0.01 0.0 switch 2.0 parabolic')
       write(31,641)
641    format('* parameter data')
       do ies=1,nespar
         write(31,650) trim(aaapar(ies))
650      format(1x,a,t15,'none relative 10.0 7.0 13.0 pargroup ',
     +   '1.0 -10.0')
       end do
       do
         read(10,'(a)',err=9225,end=600) cline
         if(index(cline,'*').ne.0)then
           cline=adjustl(cline)
           if(cline(1:1).eq.'*')then
             call lowcas(cline)
             if(index(cline,'observation gr').ne.0) go to 651
           end if
         end if
       end do
651    write(31,'(a)') trim(cline)
       do
6511     continue
         read(10,'(a)',end=850) cline
         if(skipline(cline)) go to 6511
         write(31,'(a)') trim(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'* model command').ne.0) go to 652
         end if
       end do
652    continue
       read(10,'(a)',end=850) cline
       if(skipline(cline)) go to 652
       write(31,'(a)') trim(cline)
       do
         read(10,'(a)',end=850) cline
         call lowcas(cline)
         if(index(cline,'* model input').ne.0) go to 653
       end do
653    write(31,6531)
6531   format('* model input/output')
       do
6532     continue
         read(10,'(a)',end=850) cline
         if(skipline(cline)) go to 6532
         if(cline.ne.'* model output')then
           write(31,'(a)') trim(cline)
         end if
       end do
850    continue
       close(unit=10)
       close(unit=31)
       call addquote(scaled_pestfile,afile)
       write(6,779) trim(afile)
779    format(' - file ',a,' written ok.')

C -- The Jacobian matrix file is read.

       call addquote(jcofile,afile)
       write(6,915) trim(afile)
915    format(' - reading Jacobian matrix file ',a,'...')
       call open_unformatted_file(ifail,10,'read',jcofile,amessage)
       if(ifail.ne.0) go to 9890
       read(10,err=9370,end=9370)nespar1,nxrow1
       if(nespar1.ge.0)then
         write(amessage,916) trim(afile)
916      format(' Jacobian matrix file ',a,' uses old format; ',
     +   'use JCOTRANS utility to translate it to new format.')
         go to 9890
       end if
       nespar1=abs(nespar1)
       nxrow1=abs(nxrow1)
       if((nespar1.ne.nespar).or.(nxrow1.ne.nobs)) go to 9520
       allocate(x(nxrow,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       do i=1,nespar
         do j=1,nxrow
           x(j,i)=0.0d0
         end do
       end do
       read(10,err=9370,end=9370)icount
       do i=1,icount
         read(10,err=9370,end=9370) j,dtemp
         ies=(j-1)/nxrow+1
         irow=j-(ies-1)*nxrow
         x(irow,ies)=dtemp
       end do
       do ies=1,nespar
         read(10,err=9370,end=9370) apar1
         call lowcas(apar1)
         if(apar1.ne.aaapar(ies)) go to 9520
       end do
       do irow=1,nxrow
         read(10,err=9370,end=9370) aobs1
         call lowcas(aobs1)
         if(aobs1.ne.aobs(irow)) go to 9520
       end do
       close(unit=10)
       write(6,929) trim(afile)
929    format(' - Jacobian matrix file ',a,' read ok.')

C -- The Jacobian matrix file is now scaled.

       do ies=1,nespar
         sd=parcov(ies,ies)
         sd=sqrt(sd)
         do iobs=1,nobs
           x(iobs,ies)=x(iobs,ies)*sd
         end do
       end do

C -- The new Jacobian matrix file is now written.

       call addquote(scaled_jcofile,afile)
       write(6,935) trim(afile)
935    format(' - writing scaled Jacobian matrix file ',a,'...')
       call open_unformatted_file(ifail,30,'write',scaled_jcofile,
     + amessage)
       if(ifail.ne.0) go to 9890
       write(30,err=9600) -nespar,-nobs
       icount=0
       do i=1,nespar
          do j=1,nobs
            if(x(j,i).ne.0.0d0) icount=icount+1
          end do
       end do
       write(30,err=9600) icount
       do i=1,nespar
         do j=1,nobs
           if(x(j,i).ne.0.0d0)then
             icount=(i-1)*nobs+j
             write(30,err=9600)icount,x(j,i)
           end if
         end do
       end do
       do ies=1,nespar
         write(30,err=9600) aaapar(ies)
       end do
       do iobs=1,nobs
         write(30,err=9600) aobs(iobs)
       end do
       close(unit=30)
       write(6,939) trim(afile)
939    format(' - scaled Jacobian matrix file ',a,' written ok.')

C -- The scaled uncertainty file is written

       open(unit=30,file=scaled_uncertfile)
       call addquote(scaled_uncertfile,afile)
       write(6,1039) trim(afile)
1039   format(' - writing scaled uncertainty file ',a,'...')
       if(jpartype.eq.1)then
         write(30,1040)
1040     format('START STANDARD_DEVIATION')
         do ies=1,nespar
           write(30,1050) trim(aaapar(ies))
1050       format(4x,a,t20,'1.000')
         end do
         write(30,1052)
1052     format('END STANDARD_DEVIATION')
         close(unit=30)
       else
         write(30,1070)
1070     format(' START COVARIANCE_MATRIX')
         call addquote(scaled_covfile,bfile)
         write(30,1080) trim(bfile)
1080     format(3x,'file ',a)
         write(30,1090)
1090     format(3x,'variance_multiplier 1')
         write(30,1101)
1101     format(' END COVARIANCE_MATRIX')
         close(unit=30)
         do ies=1,nespar
           if(parcov(ies,ies).ne.1.0d0)then
             if(parcov(ies,ies).gt.0.0d0)then
               rtemp=sqrt(1.0d0/parcov(ies,ies))
               do jes=1,nespar
                 parcov(ies,jes)=parcov(ies,jes)*rtemp
               end do
               do kes=1,nespar
                 parcov(kes,ies)=parcov(kes,ies)*rtemp
               end do
             end if
           end if
         end do
         open(unit=30,file=scaled_covfile)
         write(6,1105) trim(bfile)
1105     format(' - writing scaled covariance matrix file ',a,'...')
         write(30,*) nespar,nespar,1
         allocate(aastring(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         do ies=1,nespar
           do jes=1,nespar
             if(parcov(ies,jes).eq.0.0)then
               aastring(jes)='0             '
             else
               write(aastring(jes),'(1pg14.7)') parcov(ies,jes)
             end if
           end do
           write(30,1129) (trim(aastring(jes)),jes=1,nespar)
1129       format(10(1x,a))
         end do
         write(30,1139)
1139     format('* row and column names')
         do ies=1,nespar
           write(30,1150) trim(aaapar(ies))
1150       format(1x,a)
         end do
         close(unit=30)
         write(6,1151) trim(bfile)
1151     format(' - scaled covariance matrix file ',a,' written ok.')
         deallocate(aastring,stat=ierr)
       end if
       write(6,1053) trim(afile)
1053   format(' - scaled uncertainty file ',a,' written ok.')

C -- Memory is deallocated.

       deallocate(x,parcov,stat=ierr)

C -- Next SUPCALC is run in order to compute number of super parameters.

       if(ag.eq.'n') go to 1279

       write(20,920)
920    format(/,/,t20,'ANALYSIS OF PARAMETERS')
       write(20,921)
921    format(    t20,'----------------------',/)


       if((aid.eq.'y').or.(arper.eq.'y'))then
         if(as.eq.'y')then
           running_supcalc=1
           aprogram='SUPCALC'
           aprogram_lo='supcalc'
           write(6,*)
           write(6,905)
905        format(' - running SUPCALC to compute dimensions of ',
     +     'solution space...')
           open(unit=30,file=tinfile)
           write(30,'(a)') trim(scaled_pestfile)
           phi=numnonzero/weightfac/weightfac
           write(30,*) phi
           write(30,'(a)') '1'
           write(30,'(a)') 'u'
           write(30,'(a)') trim(scaled_uncertfile)
           write(30,'(a)') trim(tdatfile)
           close(unit=30)
           call delfile(toutfile)
#ifdef I64
           nnn=system
     +     ('i64supcalc < '//trim(tinfile)//' > '//trim(toutfile))
#else
#ifdef SYS_FUNCTION
           nnn=system
     +     ('supcalc < '//trim(tinfile)//' > '//trim(toutfile))
#else
           call system
     +     ('supcalc < '//trim(tinfile)//' > '//trim(toutfile))
#endif
#endif
           open(unit=11,file=toutfile,status='old',iostat=ierr)
           if(ierr.ne.0) go to 9300
           do
             read(11,'(a)',end=9300) cline
             if(index(cline,'Minimum number of super').ne.0) exit
           end do
           n=index(cline,'=')
           atemp=cline(n+1:)
           atemp=adjustl(atemp)
           call intread(ifail,atemp,numsup)
           if(ifail.ne.0) go to 9300
C           close(unit=11,status='delete')
           close(unit=11)
           call delfile(tdatfile)
           write(6,910) trim(aprogram)
910        format(' - program ',a,' run ok.')

           write(20,930) trim(aprogram)
930        format(/,' Outcomes of running program ',a,' ----->')
           write(20,*)
           call writint(anum,numsup)
           write(20,940) trim(anum)
940        format(t5,'Number of dimensions of calibration solution ',
     +     'space = ',a)
           itemp=nespar-numsup
           call writint(anum,itemp)
           write(20,950) trim(anum)
950        format(t5,'Number of dimensions of calibration null ',
     +     'space     = ',a)

#ifdef FLUSHFILE
           call flush(20)
#endif

C -- The user is notified of SUPCALC's conclusions and asked if he/she wishes to over-ride this.

           write(6,*)
           call writint(anum,numsup)
960        write(6,965) trim(anum)
965        format(' SUPCALC has recommended the use of ',a,
     +     ' solution ',
     +     'space dimensions ',/,' for computation of parameter ',
     +     'identifiability and relative ',/,
     +     ' error reduction.')
           if(numsup.gt.0) go to 9911
           write(6,*)
970        write(6,980,advance='no')
980        format(' Do you wish to over-ride this? [y/n] ',
     +     '<Enter> if "n": ')
           read(5,'(a)') aa
           call endstrip(aa)
           if(aa.eq.' ')aa='n'
           if((aa.eq.'E').or.(aa.eq.'e')) go to 970
           if((aa.eq.'Y').or.(aa.eq.'y'))then
             aa='y'
           else if((aa.eq.'N').or.(aa.eq.'n'))then
             aa='n'
           else
             go to 970
           end if
           if(aa.eq.'n')then
             if(numsup.eq.0)then
               write(6,985)
985            format(/,' A value of zero MUST be overridden - ',
     +         'try again.',/)
               go to 970
             end if
           end if
           if(aa.eq.'y')then
             call writint(anum,nespar)
988          write(6,990,advance='no') trim(anum)
990          format(' Enter new solution space dimensions ',
     +       '[1 - ',a,']: ')
             read(5,'(a)') atemp30
             call endstrip(atemp30)
             if(atemp30.eq.' ') go to 988
             atemp30=adjustl(atemp30)
             call lowcas(atemp30)
             if(atemp30.eq.'e ')then
               write(6,*)
               go to 970
             end if
             call intread(ifail,atemp30,numsup)
             if(ifail.ne.0) go to 988
             if((numsup.lt.1).or.(numsup.gt.nespar)) go to 988
             write(20,*)
             write(20,991)
991          format(' User over-ride of SUPCALC recommendations ----->')
             write(20,*)
             call writint(anum,numsup)
             write(20,940) trim(anum)
             itemp=nespar-numsup
             call writint(anum,itemp)
             write(20,950) trim(anum)
           end if
9911       continue
         else
           numsup=ndimsol
           if(numsup.gt.nespar)numsup=nespar
           write(20,992)
992        format(t5,'Dimensions of calibration solution space were ',
     +     'provided by user.')
           if(ndimsol.gt.nespar)then
             write(20,993)
993          format(t5,'(These were reduced to comply with number of ',
     +       'adjustable parameters.)')
           end if
           write(20,*)
           call writint(anum,numsup)
           write(20,940) trim(anum)
           itemp=nespar-numsup
           call writint(anum,itemp)
           write(20,950) trim(anum)
         end if
       end if
       running_supcalc=0

C -- IDENTPAR is now run in order to compute parameter identifiabilities.

       if(aid.eq.'y')then
         aprogram='IDENTPAR'
         aprogram_lo='identpar'
         write(6,*)
         write(6,1000)
1000     format(' - running IDENTPAR to compute parameter ',
     +   'identifiabilities...')
         call writint(anum,numsup)
         call delfile(toutfile)
#ifdef I64
         nnn=system('i64identpar '//trim(scaled_pestfile)//' '//
     +   trim(anum)//
     +   ' null null '//trim(toutfile)//trim(tonull))
#else
#ifdef SYS_FUNCTION
         nnn=system('identpar '//trim(scaled_pestfile)//' '//
     +   trim(anum)//
     +   ' null null '//trim(toutfile)//trim(tonull))
#else
         call system('identpar '//trim(scaled_pestfile)//' '
     +   //trim(anum)//
     +   ' null null '//trim(toutfile)//trim(tonull))
#endif
#endif

         open(unit=11,file=toutfile,status='old',iostat=ierr)
         if(ierr.ne.0) go to 9300
         read(11,*,err=9300,end=9300)
         do ipar=1,npar
           if(itrans(ipar).lt.0) cycle
           read(11,*,err=9300,end=9300) aapar,(rtemp,i=1,numsup),
     +     ident(ipar)
         end do
C         close(unit=11,status='delete')
         close(unit=11)
         write(6,910) trim(aprogram)
       end if

C -- The following sets of files are needed by both PREDVAR and PREDUNC in computing
C    relative error reduction.

       if((arper.eq.'y').or.(arpur.eq.'y')) then
         ies=0
         do ipar=1,npar
           if(itrans(ipar).lt.0) cycle
           ies=ies+1
           call writint(anum,ies)
           afile=trim(vecbase)//trim(anum)//'.vec'
           open(unit=30,file=afile)
           write(30,1120) nespar,1,2
1120       format(3i8)
           do jes=1,nespar
             if(jes.eq.ies) then
               rtemp=1.0
             else
               rtemp=0.0
             end if
             write(30,1130) rtemp
1130         format(f10.2)
           end do
           write(30,1140)
1140       format('* row names')
           jes=0
           do jpar=1,npar
             if(itrans(jpar).lt.0) cycle
             write(30,'(a)') trim(adjustl(apar(jpar)))
           end do
           write(30,1160)
1160       format('* column names')
           write(30,'(a)') trim(adjustl(apar(ipar)))
           close(unit=30)
         end do
       end if

C -- Parameter error variance reduction is now calculated. Note that we do not need to scale parameter
C    sensitivities here.

       if(arper.eq.'y')then
         aprogram='PARVAR1'
         aprogram_lo='parvar1'
         write(6,*)
         write(6,1100)
1100     format(' - running PARVAR1 to compute rel param ',
     +   'error variance reductions...')
         call delfile(toutfile)
         open(unit=30,file=tinfile)
         write(30,'(a)') trim(scaled_pestfile)
         write(30,*) refvar
         write(30,'(a)') trim(scaled_uncertfile)
         call writint(anumsup,numsup)
         write(30,'(a)') trim(anumsup)
         write(30,'(a)') trim(toutfile)
         close(unit=30)

#ifdef I64
         nnn=system('i64parvar1 < '//trim(tinfile)//trim(tonull))
#else
#ifdef SYS_FUNCTION
         nnn=system('parvar1 < '//trim(tinfile)//trim(tonull))
#else
         call system('parvar1 < '//trim(tinfile)//trim(tonull))
#endif
#endif

         open(unit=11,file=toutfile,status='old',err=9300)
         do i=1,3
           read(11,*,err=9300,end=9300)
         end do
         ies=0
         do ipar=1,npar
           if(itrans(ipar).lt.0) cycle
           ies=ies+1
           read(11,'(a)',err=9300,end=9300) cline
           call linspl(ifail,4,lw,rw,cline)
           call drealread(ifail,cline(lw(4):rw(4)),var)
           if(ifail.ne.0) go to 9300
           relerr(ipar)=1.0d0-var
         end do
         close(unit=11,status='delete')
         call delfile(tinfile)
         write(6,910) trim(aprogram)

       end if

C -- Parameter uncertainty variance reduction is now calculated. Note that we do not need to scale parameter
C    sensitivities here.

       if(arpur.eq.'y')then
         aprogram='PREDUNC7'
         aprogram_lo='predunc7'
         write(6,*)
         write(6,1200)
1200     format(' - running PREDUNC7 to compute rel param ',
     +   'uncert variance reductions...')

         call delfile(toutfile)
         open(unit=30,file=tinfile)
         write(30,'(a)') scaled_pestfile
         write(30,*) refvar
         write(30,'(a)') trim(scaled_uncertfile)
         write(30,'(a)') trim(scaled_post_covfile)
         write(30,'(a)') trim(scaled_post_uncertfile)
         write(30,'(a)') trim(avchoice)
         close(unit=30)
#ifdef I64
         nnn=system('i64predunc7 < '//trim(tinfile)//trim(tonull))
#else
#ifdef SYS_FUNCTION
         nnn=system('predunc7 < '//trim(tinfile)//trim(tonull))
#else
         call system('predunc7 < '//trim(tinfile)//trim(tonull))
#endif
#endif
         open(unit=11,file=scaled_post_covfile,status='old',err=9300)
         read(11,*,err=9300) itemp1,itemp2
         if((itemp1.ne.nespar).or.(itemp2.ne.nespar)) go to 9300
         allocate(tempvec(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             ies=ies+1
             read(11,*,err=9300,end=9300) (tempvec(jes),jes=1,nespar)
             var=tempvec(ies)
             relunc(ipar)=1.0d0-var
           end if
         end do
         close(unit=11,status='delete')
         call delfile(tinfile)
         call delfile(scaled_post_uncertfile)
         write(6,910) trim(aprogram)
       end if

C -- Files needed by both the above PREDVAR1A and PREDVAR runs are deleted.

       ies=0
       do ipar=1,npar
         if(itrans(ipar).lt.0) cycle
         ies=ies+1
         call writint(anum,ies)
         afile=trim(vecbase)//trim(anum)//'.vec'
         call delfile(afile)
       end do

C -- Results are written to the GENLINPRED output file.

       write(20,1250)
1250   format(/,' Outcomes of parameter identifiability',
     + ' and error/uncertainty variance reduction analysis ----->')
       write(20,*)
       heading(1)='Parameter'
       ncol=1
       if(aid.eq.'y')then
         ncol=ncol+1
         heading(ncol)='Identifiability'
       end if
       if(arper.eq.'y')then
         ncol=ncol+1
         heading(ncol)='Rel_err_var_redn'
       end if
       if(arpur.eq.'y')then
         ncol=ncol+1
         heading(ncol)='Rel_uncert_var_redn'
       end if
       write(20,1260) (trim(heading(i)),i=1,ncol)
1260   format(t5,a,t28,a,t51,a,t74,a)
       do i=1,ncol
         do j=1,len_trim(heading(i))
           heading(i)(j:j)='-'
         end do
       end do
       write(20,1260) (trim(heading(i)),i=1,ncol)
       ies=0
       do ipar=1,npar
         if(itrans(ipar).lt.0) cycle
         heading(1)=trim(apar(ipar))
         ncol=1
         if(aid.eq.'y')then
           ncol=ncol+1
           write(heading(ncol),1170) ident(ipar)
         end if
         if(arper.eq.'y')then
           ncol=ncol+1
           write(heading(ncol),1170) relerr(ipar)
1170       format(1pg14.7)
         end if
         if(arpur.eq.'y')then
           ncol=ncol+1
           write(heading(ncol),1170) relunc(ipar)
         end if
         write(20,1260) (trim(heading(i)),i=1,ncol)
        end do
        if(aid.eq.'y')then
          write(20,*)
          write(20,1280)
1280      format(t5,'Note: run IDENTPAR yourself to see identiability ',
     +    'contributions by different solution ',/,t5,'space ',
     +    'eigencomponents.')
        end if

#ifdef FLUSHFILE
        call flush(20)
#endif

C -- If predictive uncertainty is not to be examined, the program is terminated.

        if(apred.eq.' ') go to 9895

1279    continue
        write(20,1281)
1281    format(/,/,t20,'ANALYSIS OF PREDICTION')
        write(20,1282)
1282    format(    t20,'----------------------',/)

C -- The predictive sensitivity is formulated or read.

        if(senfile.eq.'p')then
          ies=0
          do ipar=1,npar
            if(itrans(ipar).lt.0) cycle
            ies=ies+1
            if(ipar.eq.predpar)then
              predvec(ies)=1.0d0
            else
              predvec(ies)=0.0d0
            end if
          end do
        else
          nb=len_trim(senfile)
          i=nb-3
          if(i.lt.1)i=1
          aext=senfile(i:nb)
          call lowcas(aext)
          if(aext.eq.'.jco')then
            write(6,1310)
1310        format(/,' - running JROW2VEC to extract ',
     +      'predictive sensitivities...')
            aprogram='JROW2VEC'
            aprogram_lo='jrow2vec'
            call delfile(toutfile)
            call addquote(senfile,afile1)
#ifdef I64
            nnn=system('i64jrow2vec '//trim(afile1)//' '//
     +      trim(apred)//' '//trim(toutfile)//trim(tonull))
#else
#ifdef SYS_FUNCTION
            nnn=system('jrow2vec '//trim(afile1)//' '//
     +      trim(apred)//' '//trim(toutfile)//trim(tonull))
#else
            call system('jrow2vec '//trim(afile1)//' '//
     +      trim(apred)//' '//trim(toutfile)//trim(tonull))
#endif
#endif
            open(unit=11,file=toutfile,status='old',iostat=ierr)
            if(ierr.ne.0) go to 9300
            read(11,*,err=9300,end=9300) nespar1
            if(nespar1.lt.nespar)then
              call addquote(senfile,afile1)
              call addquote(pestfile,afile2)
              write(amessage,1320) trim(afile1),trim(afile2)
1320          format('JCO file ',a,' cites less adjustable parameters ',
     +        'than PEST control file ',a,'.')
              go to 9890
            end if
            afile=toutfile
          else
            call addquote(senfile,afile)
            write(6,1375) trim(afile)
1375        format(/,' - reading predictive sensitivities from ',
     +      'file ',a,'...')
            open(unit=11,file=senfile,status='old',iostat=ierr)
            if(ierr.ne.0)then
              call addquote(senfile,afile)
              write(amessage,1380) trim(afile)
1380          format('Cannot open file ',a,' to read ',
     +        'predictive sensitivities.')
              go to 9890
            end if
            read(11,*,err=9400,end=9400) nespar1
            if(nespar1.lt.nespar)then
              call addquote(senfile,afile1)
              call addquote(pestfile,afile2)
              write(amessage,1390) trim(afile1),trim(afile2)
1390          format('File ',a,' cites less adjustable parameters ',
     +        'than PEST control file ',a,'.')
              go to 9890
            end if
            call addquote(senfile,afile)
          end if
          if(allocated(tempvec))then
            if(size(tempvec).lt.nespar1)then
              deallocate(tempvec,stat=ierr)
              allocate(tempvec(nespar1),stat=ierr)
              if(ierr.ne.0) go to 9200
            end if
          else
            allocate(tempvec(nespar1),stat=ierr)
            if(ierr.ne.0) go to 9200
          end if
          do i=1,nespar1
            read(11,*,err=9400,end=9400) tempvec(i)
          end do
          read(11,'(a)',err=9400,end=9400) cline
          call lowcas(cline)
          if(index(cline,'name').eq.0)then
            call writint(aline,nespar+2)
            write(amessage,1410) trim(aline),trim(afile)
1410        format('"* row names" header expected to follow ',
     +      'predictive sensitivities and precede parameter names ',
     +      'at line ',a,' of file ',a,'.')
            go to 9890
          end if
          predvec=-1.1d35          ! an array
          do i=1,nespar1
            read(11,'(a)') aapar
            call lowcas(aapar)
            ies=0
            do ipar=1,npar
              if(itrans(ipar).lt.0) cycle
              ies=ies+1
              if(aapar.eq.apar(ipar))then
                predvec(ies)=tempvec(i)
                go to 1350
              end if
            end do
1350        continue
          end do
          close(unit=11)
          deallocate(tempvec,stat=ierr)
          ies=0
          do ipar=1,npar
            if(itrans(ipar).lt.0) cycle
            ies=ies+1
            if(predvec(ies).lt.-1.0d35)then
              call addquote(senfile,afile1)
              call addquote(pestfile,afile2)
              if(aext.eq.'.jco')then
                atemp='JCO'
              else
                atemp='sensitivity'
              end if
              write(amessage,1360) trim(apar(ipar)),trim(afile2),
     +        trim(atemp),trim(afile1)
1360          format('Parameter "',a,'" cited in PEST control file ',
     +        a,' is not cited in ',a,' file ',a,'.')
              go to 9890
            end if
          end do
          if(aext.eq.'.jco')then
            write(6,1315)
1315        format(' - program JROW2VEC run ok.')
          else
            call addquote(senfile,afile)
            write(6,1361) trim(afile)
1361        format(' - file ',a,' read ok.')
          end if

        end if

C -- The sensitivities of the prediction must now be scaled.

        do ies=1,nespar
          predvec(ies)=predvec(ies)*senscale(ies)   ! check that non divide by zero
        end do

C -- The scaled predictive sensitivity vector is now written to a sensitivity file.

        open(unit=30,file=tsenfile,action='write')
        write(30,1120) nespar,1,2
        do ies=1,nespar
          write(30,1430) predvec(ies)
1430      format(1x,1pg14.7)
        end do
        write(30,1140)
        do ipar=1,npar
          if(itrans(ipar).lt.0) cycle
          write(30,'(a)') trim(adjustl(apar(ipar)))
        end do
        write(30,1440)
1440    format('* column names')
        write(30,'(1x,a)') trim(apred)
        close(unit=30)

C -- PREDVAR1A is now run in order to obtain plots of error variance vs singular number.

        if(asne.eq.'y')then
          aprogram='PREDVAR1A'
          aprogram_lo='predvar1a'
          write(6,1445)
1445      format(/,' - running PREDVAR1A to obtain pred. error ',
     +    'variance sing. val. dependence...')
          open(unit=30,file=tdatfile,action='write')
          write(30,1450) trim(tsenfile),trim(toutfile)
1450      format(1x,a,2x,a)
          close(unit=30)

C -- It is run a first time to put us in the ballpark

          ncount=0
          if(nespar.gt.50) then
            call delfile(toutfile)
            open(unit=30,file=tinfile,action='write')
            write(30,1455) trim(scaled_pestfile)
1455        format(1x,a)
            write(30,1460) refvar
1460        format(1x,1pg14.7)
            write(30,1455) trim(scaled_uncertfile)
            write(30,1455) trim(tdatfile)
            istep=nespar/20
            ncount=0
            do i=0,nespar,istep
              ncount=ncount+1
              if(ncount.gt.MAXSING)then
                write(amessage,1475)
                go to 9890
              end if
              write(30,1470) i
1470          format(1x,i5)
              sing(1,ncount)=i
            end do
            if(i.ne.nespar) then
              write(30,1470) nespar
              ncount=ncount+1
              if(ncount.gt.MAXSING)then
                write(amessage,1475)
1475            format('Increase MAXSING and re-compile program.')
                go to 9890
              end if
              sing(1,ncount)=nespar
            end if
            write(30,'(a)') ' '
            close(unit=30)
#ifdef I64
            nnn=system('i64predvar1a < '//trim(tinfile)//' '//
     +      trim(tonull))
#else
#ifdef SYS_FUNCTION
            nnn=system('predvar1a < '//trim(tinfile)//' '//
     +      trim(tonull))
#else
            call system('predvar1a < '//trim(tinfile)//' '//
     +      trim(tonull))
#endif
#endif
            open(unit=11,file=toutfile,status='old',err=9300)
            read(11,*,err=9300,end=9300)
            read(11,*,err=9300,end=9300)
            read(11,*,err=9300,end=9300)
            varmin=1.0d300
            do icount=1,ncount
              read(11,*,err=9300,end=9300) itemp,
     +        nulterm(1,icount),solterm(1,icount),total(1,icount)
              if(total(1,icount).lt.varmin)then
                varmin=total(1,icount)
                icountlo=icount
              end if
            end do
C            close(unit=11,status='delete')
            close(unit=11)
            if(icountlo.eq.1)then
              isingstart=sing(1,1)
              isingfin=sing(1,4)
            else if(icountlo.eq.ncount)then
              isingstart=sing(1,ncount-3)
              isingfin=sing(1,ncount)
            else
              i=icountlo-2
              j=icountlo+2
              if(i.lt.1)then
                i=1
                j=j+1
              else if(j.gt.ncount)then
                j=ncount
                i=i-1
              end if
              if(i.lt.1)i=1
              if(j.gt.ncount)j=ncount
              isingstart=sing(1,i)
              isingfin=sing(1,j)
            end if
          else
            isingstart=0
            isingfin=nespar
          end if

C -- Now we run PREDVAR1A again.

          call delfile(toutfile)
          open(unit=30,file=tinfile,action='write')
          write(30,1455) trim(scaled_pestfile)
          write(30,1460) refvar
          write(30,1455) trim(scaled_uncertfile)
          write(30,1455) trim(tdatfile)
          mcount=0
          do i=isingstart,isingfin
            mcount=mcount+1
            write(30,1470) i
            sing(2,mcount)=i
          end do
          write(30,'(a)') ' '
          close(unit=30)
#ifdef I64
          nnn=system('i64predvar1a < '//trim(tinfile)//' '
     +     //trim(tonull))
#else
#ifdef SYS_FUNCTION
          nnn=system('predvar1a < '//trim(tinfile)//' '//trim(tonull))
#else
          call system('predvar1a < '//trim(tinfile)//' '//trim(tonull))
#endif
#endif
          open(unit=11,file=toutfile,status='old',err=9300)
          read(11,*,err=9300,end=9300)
          read(11,*,err=9300,end=9300)
          read(11,*,err=9300,end=9300)
          do icount=1,mcount
            read(11,*,err=9300,end=9300) itemp,
     +      nulterm(2,icount),solterm(2,icount),total(2,icount)
          end do
          close(unit=11)

C -- The results from the two runs are combined.

          if(ncount.eq.0)then
            do icount=1,mcount
              ss(icount)=sing(2,icount)
              nt(icount)=nulterm(2,icount)
              st(icount)=solterm(2,icount)
              tt(icount)=total(2,icount)
            end do
            numsing=mcount
          else
            numsing=0
            do
              ilist=0
              singmin=999999
              do icount=1,ncount
                if(sing(1,icount).lt.singmin)then
                  singmin=sing(1,icount)
                  ilist=1
                  iicount=icount
                end if
              end do
              do icount=1,mcount
                if(sing(2,icount).lt.singmin)then
                  singmin=sing(2,icount)
                  ilist=2
                  iicount=icount
                end if
              end do
              if(ilist.ne.0)then
                if(numsing.ne.0)then
                  do j=1,numsing
                    if(singmin.eq.ss(j)) then
                      sing(ilist,iicount)=9999999
                      go to 1499
                    end if
                  end do
                end if
                numsing=numsing+1
                if(numsing.gt.MAXSING)then
                  write(amessage,1475)
                  go to 9890
                end if
                ss(numsing)=singmin
                nt(numsing)=nulterm(ilist,iicount)
                st(numsing)=solterm(ilist,iicount)
                tt(numsing)=total(ilist,iicount)
                sing(ilist,iicount)=9999999
              else
                go to 1500
              end if
1499          continue
            end do
1500        continue
          end if

C -- The results are now written to the GENLINPRED output file.

          write(20,1520) trim(apred)
1520      format(' PREDVAR1 analysis for prediction "',a,'" ----->',/)
          write(20,1540)
1540      format(t5,'Sing_val',t20,'Null-space',t37,'Soln-space',t52,
     +    'Total',  t70,'Total')
          write(20,1550)
1550      format(t5,'index',   t20,'term',      t37,'term',      t52,
     +    'variance',t70,'std_dev')
          write(20,1551)
1551      format(t5,'--------',t20,'----------',t37,'----------',t52,
     +    '--------',t70,'-------')
          do ising=1,numsing
            write(20,1560) ss(ising),nt(ising),st(ising),
     +      tt(ising),sqrt(tt(ising))
1560        format(i8,t20,1pg14.7,t37,1pg14.7,t52,
     +      1pg14.7,t70,1pg14.7)
          end do
          varmin=1.0d300
          do ising=1,numsing
            if(tt(ising).lt.varmin)then
              varmin=tt(ising)
              isinglo=ising
            end if
          end do
          write(20,*)
          write(20,1570)
1570      format(t5,'Pre-calibration:- ')
          write(20,1571) tt(1)
1571      format(t5,'Total error variance                 = ',1pg14.7)
          write(20,1572) sqrt(tt(1))
1572      format(t5,'Total error standard deviation       = ',1pg14.7)
          write(20,*)
          write(20,1580)
1580      format(t5,'Post-calibration:-')
          write(20,1571) varmin
          write(20,1572) sqrt(varmin)
          call writint(anum,ss(isinglo))
          write(20,1590) trim(anum)
1590      format(t5,'Minimum error variance occurs at singular value ',
     +    'number ',a,'.')
          precalerr=tt(1)
          postcalerr=varmin

          write(6,910) trim(aprogram)

        end if

C -- PREDUNC1 is now run in order to obtain pre- and post-calibration predictive uncertainty.

        if(aun.eq.'y')then
          aprogram='PREDUNC1'
          aprogram_lo='predunc1'
          write(6,1600)
1600      format(/,' - running PREDUNC1 to compute predictive ',
     +    'uncertainty...')
          call delfile(toutfile)
          open(unit=30,file=tinfile,action='write')
          write(30,1610) trim(scaled_pestfile)
1610      format(1x,a)
          write(30,1620) refvar
1620      format(1x,1pg14.7)
          write(30,1610) trim(scaled_uncertfile)
          write(30,1610) trim(tsenfile)
          write(30,'(a)') trim(avchoice)
          close(unit=30)
#ifdef I64
          nnn=system('i64predunc1 < '//trim(tinfile)//' > '
     +    //trim(toutfile))
#else
#ifdef SYS_FUNCTION
          nnn=system('predunc1 < '//trim(tinfile)//' > '
     +    //trim(toutfile))
#else
          call system('predunc1 < '//trim(tinfile)//' > '
     +    //trim(toutfile))
#endif
#endif
          open(unit=11,file=toutfile,status='old',err=9300)
          do
            read(11,'(a)',end=9300) cline
            if(index(cline,'Pre-cal').ne.0) exit
          end do
          n=index(cline,'=')
          atemp=cline(n+1:)
          call drealread(ifail,atemp,precalunc)
          if(ifail.ne.0) go to 9300
          read(11,'(a)',end=9300) cline
          n=index(cline,'=')
          atemp=cline(n+1:)
          call drealread(ifail,atemp,postcalunc)
          if(ifail.ne.0) go to 9300
          close(unit=11)
          precalvar=precalunc*precalunc
          postcalvar=postcalunc*postcalunc

          if(asne.eq.'y')then
            write(20,*)
            write(20,*)
          end if
          if(senfile.ne.'p')then
            write(20,1650) trim(apred)
1650        format(' PREDUNC1 analysis for prediction "',a,'" ----->',/)
          else
            write(20,1649) trim(apred)
1649        format(' PREDUNC1 analysis for parameter "',a,'" ----->',/)
          end if
          write(20,1570)
          write(20,1651) precalvar
1651      format(t5,'Total uncertainty variance           = ',1pg14.7)
          write(20,1652) precalunc
1652      format(t5,'Total uncertainty standard deviation = ',1pg14.7)
          write(20,*)
          write(20,1580)
          write(20,1651) postcalvar
          write(20,1652) postcalunc

          if(senfile.eq.'p')then
            if(itrans(predpar).eq.1)then
              rtempm=log10(pval(predpar))
            else
              rtempm=pval(predpar)
            end if
            pretempu=rtempm+1.96*precalunc
            pretempl=rtempm-1.96*precalunc
            posttempu=rtempm+1.96*postcalunc
            posttempl=rtempm-1.96*postcalunc
            if(itrans(predpar).eq.1)then
              rtempm=pval(predpar)
              pretempu=10**pretempu
              pretempl=10**pretempl
              posttempu=10**posttempu
              posttempl=10**posttempl
            end if
            write(atempm,1654) rtempm
            atempm=adjustl(atempm)
            write(20,1653)
1653        format(/,t5,'95% confidence interval based on parameter ',
     +      'value in PEST control file ----->')
            write(20,1657) trim(apred),trim(atempm)
1657        format(t5,'Value for parameter "',a,
     +      '" in PEST control file = ',a)
1654        format(1pg13.6)
            write(preatempu,1654) pretempu
            write(preatempl,1654) pretempl
            write(postatempu,1654) posttempu
            write(postatempl,1654) posttempl
            preatempu=adjustl(preatempu)
            preatempl=adjustl(preatempl)
            postatempl=adjustl(postatempl)
            postatempu=adjustl(postatempu)
            write(20,1655) trim(preatempl),trim(preatempu)
1655        format(t5,'Pre-calibration :  lower limit = ',a,t53,
     +      'upper limit = ',a)
            write(20,1656) trim(postatempl),trim(postatempu)
1656        format(t5,'Post-calibration:  lower limit = ',a,t53,
     +      'upper limit = ',a)
          end if
          write(6,910) trim(aprogram)

        end if

#ifdef FLUSHFILE
        call flush(20)
#endif

C -- Parameter contributions to predictive error variance and uncertainty are now computed.

        if((ace.eq.'n').and.(acu.eq.'n')) go to 1866

C -- First a parameter list file is made.

        open(unit=30,file=tplistfile,action='write')
        write(30,1700)
1700    format('* parameter type none')
        mcount=1
        acontrib(mcount)='none'
        if(apg.eq.'i')then
          do ipar=1,npar
            if(itrans(ipar).lt.0) cycle
            write(30,1710) trim(apar(ipar))
1710        format('* parameter type ',a)
            mcount=mcount+1
            acontrib(mcount)=apar(ipar)
            write(30,1712) trim(apar(ipar))
1712        format(1x,a)
          end do
        else
          do ipargp=1,npargp
            icount=0
            do ipar=1,npar
              if(itrans(ipar).ge.0)then
                if(npargnm(ipar).eq.ipargp) then
                  if(icount.eq.0)then
                    mcount=mcount+1
                    acontrib(mcount)=apargp(ipargp)
                    write(30,1710) trim(apargp(ipargp))
                  end if
                  icount=icount+1
                  write(30,1712) trim(apar(ipar))
                end if
              end if
            end do
1715        continue
          end do
        end if
        close(unit=30)

C -- PREDVAR4 is run.

        if(ace.eq.'y')then
          aprogram='PREDVAR4'
          aprogram_lo='predvar4'
          write(6,1720)
1720      format(/,' - running PREDVAR4 to compute param contribs ',
     +    'to pred error variance...')
          open(unit=30,file=tslistfile,action='write')
          do ies=0,nespar
            write(30,1730) ies
1730        format(1x,i5)
          end do
          close(unit=30)
          open(unit=30,file=tslistfile0,action='write')
          write(30,1730) 0
          close(unit=30)
          if(mcount.eq.2) go to 1759
          call delfile(toutfile)
          open(unit=30,file=tinfile,action='write')
          write(30,1740) trim(scaled_pestfile)
1740      format(1x,a)
          write(30,1750) refvar
1750      format(1x,1pg14.7)
          write(30,1740) trim(scaled_uncertfile)
          write(30,1740) trim(tsenfile)
          write(30,1740) trim(tslistfile)
          write(30,1740) trim(tplistfile)
          write(30,1740) trim(toutfile)
          close(unit=30)
#ifdef I64
          nnn=system('i64predvar4 < '//trim(tinfile)//trim(tonull))
#else
#ifdef SYS_FUNCTION
          nnn=system('predvar4 < '//trim(tinfile)//trim(tonull))
#else
          call system('predvar4 < '//trim(tinfile)//trim(tonull))
#endif
#endif
          open(unit=11,file=toutfile,status='old',err=9300)
          do i=1,3
            read(11,*,err=9300,end=9300)
          end do
          do i=1,mcount
            read(11,'(a)',err=9300,end=9300) cline
            call linspl(ifail,4,lw,rw,cline)
            call drealread(ifail,cline(lw(4):rw(4)),contrib(2,i))
            if(ifail.ne.0) go to 9300
          end do
          close(unit=11)

          call delfile(toutfile)
          open(unit=30,file=tinfile,action='write')
          write(30,1740) trim(scaled_pestfile)
          write(30,1750) refvar
          write(30,1740) trim(scaled_uncertfile)
          write(30,1740) trim(tsenfile)
          write(30,1740) trim(tslistfile0)
          write(30,1740) trim(tplistfile)
          write(30,1740) trim(toutfile)
          close(unit=30)
#ifdef I64
          nnn=system('i64predvar4 < '//trim(tinfile)//trim(tonull))
#else
#ifdef SYS_FUNCTION
          nnn=system('predvar4 < '//trim(tinfile)//trim(tonull))
#else
          call system('predvar4 < '//trim(tinfile)//trim(tonull))
#endif
#endif
          open(unit=11,file=toutfile,status='old',err=9300)
          do i=1,3
            read(11,*,err=9300,end=9300)
          end do
          do i=1,mcount
            read(11,'(a)',err=9300,end=9300) cline
            call linspl(ifail,4,lw,rw,cline)
            call drealread(ifail,cline(lw(4):rw(4)),contrib(1,i))
            if(ifail.ne.0) go to 9300
          end do
          close(unit=11)

1759      continue
          if((asne.eq.'y').or.(aun.eq.'y'))then
            write(20,*)
            write(20,*)
          end if
          write(20,1760) trim(apred)
1760      format(' PREDVAR4 analysis for prediction "',a,'" ----->',/)
          write(20,1765)
1765      format(t5,'Contributions to predictive error variance',/)
          write(20,1780)
1780      format(t5,'Parameter',t20,'Pre-cal',t40,'Post-cal')
          if(apg.eq.'g')then
            atemp='group'
          else
            atemp=' '
          end if
          write(20,1790) trim(atemp)
1790      format(t5,a,t20,'contribution',t40,'contribution')
          write(20,1800)
1800      format(t5,'---------',t20,'------------',t40,'------------')
          if(mcount.eq.2)then
            write(20,1810) trim(acontrib(2)),precalerr,postcalerr
          else
            do i=2,mcount
              rtemp1=contrib(1,1)-contrib(1,i)
              rtemp2=contrib(2,1)-contrib(2,i)
              write(20,1810) trim(acontrib(i)),rtemp1,rtemp2
1810          format(t5,a,t20,1pg14.7,t40,1pg14.7)
            end do
          end if

          call delfile(tslistfile0)
          write(6,910) trim(aprogram)
        end if

#ifdef FLUSHFILE
        call flush(20)
#endif

C -- PREDUNC4 is run.

        if(acu.eq.'y')then
          aprogram='PREDUNC4'
          aprogram_lo='predunc4'
          write(6,1820)
1820      format(/,' - running PREDUNC4 to compute param contribs ',
     +    'to pred uncert variance...')
          if(mcount.eq.2) go to 1859

          call delfile(toutfile)
          open(unit=30,file=tinfile,action='write')
          write(30,1740) trim(scaled_pestfile)
          write(30,1750) refvar
          write(30,1740) trim(scaled_uncertfile)
          write(30,1740) trim(tsenfile)
          write(30,1740) trim(tplistfile)
          write(30,1740) trim(toutfile)
          write(30,'(a)') trim(avchoice)
          close(unit=30)
#ifdef I64
          nnn=system('i64predunc4 < '//trim(tinfile)//trim(tonull))
#else
#ifdef SYS_FUNCTION
          nnn=system('predunc4 < '//trim(tinfile)//trim(tonull))
#else
          call system('predunc4 < '//trim(tinfile)//trim(tonull))
#endif
#endif
          open(unit=11,file=toutfile,status='old',err=9300)
          do i=1,3
            read(11,*,err=9300,end=9300)
          end do
          do i=1,mcount
            read(11,'(a)',err=9300,end=9300) cline
            call linspl(ifail,3,lw,rw,cline)
            call drealread(ifail,cline(lw(2):rw(2)),contrib(1,i))
            call drealread(ifail,cline(lw(3):rw(3)),contrib(2,i))
            if(ifail.ne.0) go to 9300
          end do
          close(unit=11)

1859      continue
          if((asne.eq.'y').or.(aun.eq.'y').or.
     +       (ace.eq.'y'))then
             write(20,*)
             write(20,*)
          end if
          write(20,1860) trim(apred)
1860      format(' PREDUNC4 analysis for prediction "',a,'" ----->',/)
          write(20,1865)
1865      format(t5,'Contributions to predictive uncertainty ',
     +    'variance',/)
          write(20,1780)
          if(apg.eq.'g')then
            atemp='group'
          else
            atemp=' '
          end if
          write(20,1790) trim(atemp)
          write(20,1800)
          if(mcount.eq.2)then
            write(20,1810) trim(acontrib(2)),precalvar,postcalvar
          else
            do i=2,mcount
              rtemp1=contrib(1,1)-contrib(1,i)
              rtemp2=contrib(2,1)-contrib(2,i)
              write(20,1810) trim(acontrib(i)),rtemp1,rtemp2
            end do
          end if

          call delfile(tplistfile)
          write(6,910) trim(aprogram)
        end if

#ifdef FLUSHFILE
        call flush(20)
#endif

C -- Observation worth is now computed. First through observation subtraction.

1866    continue
        if((awe.eq.'n').and.(awu.eq.'n')) go to 2400

        open(unit=30,file=tolistfile_sub,action='write')
        open(unit=31,file=tolistfile_add,action='write')
        write(30,1900)
1900    format('* observation type none')
        write(31,1901)
1901    format('* observation type base')
        mcount=1
        acontrib(mcount)='first'
        if(aog.eq.'i')then
          do iobs=1,nobs
            write(30,1910) trim(aobs(iobs))
            write(31,1910) trim(aobs(iobs))
1910        format('* observation type ',a)
            write(30,1912) trim(aobs(iobs))
            write(31,1912) trim(aobs(iobs))
1912        format(1x,a)
            mcount=mcount+1
          end do
        else
          do iobsgp=1,nobsgp
            icount=0
            do iobs=1,nobs
              if(nobgnm(iobs).eq.iobsgp) then
                if(icount.eq.0)then
                  mcount=mcount+1
                  acontrib(mcount)=aobsgp(iobsgp)
                  write(30,1910) trim(aobsgp(iobsgp))
                  write(31,1910) trim(aobsgp(iobsgp))
                  acontrib(mcount)=aobsgp(iobsgp)
                end if
                icount=icount+1
                write(30,1912) trim(aobs(iobs))
                write(31,1912) trim(aobs(iobs))
              end if
            end do
          end do
        end if
        close(unit=30)
        close(unit=31)

C -- PREDVAR5 is run.

        if(awe.eq.'y')then
          aprogram='PREDVAR5'
          aprogram_lo='predvar5'
          write(6,1920)
1920      format(/,' - running PREDVAR5 to compute observ worth ',
     +    'through subtraction...')
          open(unit=30,file=tslistfile,action='write')
          do ies=0,nespar
            write(30,1730) ies
          end do
          close(unit=30)
          if(mcount.eq.2) go to 1959

          call delfile(toutfile)
          open(unit=30,file=tinfile,action='write')
          write(30,1740) trim(scaled_pestfile)
          write(30,1750) refvar
          write(30,1740) trim(scaled_uncertfile)
          write(30,1740) trim(tsenfile)
          write(30,1740) trim(tslistfile)
          write(30,1740) trim(tolistfile_sub)
          write(30,'(a)') 's'
          write(30,1740) trim(toutfile)
          close(unit=30)
#ifdef I64
          nnn=system('i64predvar5 < '//trim(tinfile)//trim(tonull))
#else
#ifdef SYS_FUNCTION
          nnn=system('predvar5 < '//trim(tinfile)//trim(tonull))
#else
          call system('predvar5 < '//trim(tinfile)//trim(tonull))
#endif
#endif
          open(unit=11,file=toutfile,status='old',err=9300)
          do i=1,3
            read(11,*,err=9300,end=9300)
          end do
          do i=1,mcount
            read(11,'(a)',err=9300,end=9300) cline
            call linspl(ifail,4,lw,rw,cline)
            call drealread(ifail,cline(lw(4):rw(4)),ocontrib(i))
            if(ifail.ne.0) go to 9300
          end do
          close(unit=11)

1959      continue
          if((asne.eq.'y').or.(aun.eq.'y').or.
     +       (ace.eq.'y').or.(acu.eq.'y'))then
             write(20,*)
             write(20,*)
          end if
          write(20,1960) trim(apred)
1960      format(' PREDVAR5 observation subtraction analysis for ',
     +    'prediction "',a,'" ----->',/)
          write(20,1965)
1965      format(t5,'Increases in predictive error variance incurred ',
     +    'through loss of observations',/)
          if(aog.eq.'g')then
            write(20,1980)
1980        format(t5,'Observation_group',t30,'Variance_increase')
          else
            write(20,1981)
1981        format(t5,'Observation',t30,'Variance_increase')
          end if
          if(aog.eq.'g')then
            write(20,1990)
1990        format(t5,'-----------------',t30,'-----------------')
          else
            write(20,1991)
1991        format(t5,'-----------',t30,'-----------------')
          end if
          if(mcount.eq.2)then
            if(aog.eq.'i')then
              write(20,2010) trim(aobs(1)),postcalerr
            else
              write(20,2010) trim(acontrib(2)),postcalerr
            end if
          else
            do i=2,mcount
              rtemp1=ocontrib(i)-ocontrib(1)
              if(aog.eq.'i')then
                write(20,2010) trim(aobs(i-1)),rtemp1
              else
                write(20,2010) trim(acontrib(i)),rtemp1
2010            format(t5,a,t30,1pg14.7)
              end if
            end do
          end if
          write(6,910) trim(aprogram)
        end if

#ifdef FLUSHFILE
        call flush(20)
#endif

C -- PREDUNC5 is run.

        if(awu.eq.'y')then
          aprogram='PREDUNC5'
          aprogram_lo='predunc5'
          write(6,2020)
2020      format(/,' - running PREDUNC5 to compute observ worth ',
     +    'through subtraction...')
          if(mcount.eq.2) go to 2059

          call delfile(toutfile)
          open(unit=30,file=tinfile,action='write')
          write(30,1740) trim(scaled_pestfile)
          write(30,1750) refvar
          write(30,1740) trim(scaled_uncertfile)
          write(30,1740) trim(tsenfile)
          write(30,1740) trim(tolistfile_sub)
          write(30,'(a)') 's'
          write(30,1740) trim(toutfile)
          write(30,'(a)') trim(avchoice)
          close(unit=30)
#ifdef I64
          nnn=system('i64predunc5 < '//trim(tinfile)//trim(tonull))
#else
#ifdef SYS_FUNCTION
          nnn=system('predunc5 < '//trim(tinfile)//trim(tonull))
#else
          call system('predunc5 < '//trim(tinfile)//trim(tonull))
#endif
#endif
          open(unit=11,file=toutfile,status='old',err=9300)
          do i=1,3
            read(11,*,err=9300,end=9300)
          end do
          do i=1,mcount
            read(11,'(a)',err=9300,end=9300) cline
            call linspl(ifail,3,lw,rw,cline)
            call drealread(ifail,cline(lw(3):rw(3)),ocontrib(i))
            if(ifail.ne.0) go to 9300
          end do
          close(unit=11)

2059      continue
          if((asne.eq.'y').or.(aun.eq.'y').or.
     +       (ace.eq.'y').or.(acu.eq.'y').or.
     +       (awe.eq.'y'))then
             write(20,*)
             write(20,*)
          end if
          write(20,2060) trim(apred)
2060      format(' PREDUNC5 observation subtraction analysis for ',
     +    'prediction "',a,'" ----->',/)
          write(20,2065)
2065      format(t5,'Increases in predictive uncertainty variance ',
     +    'incurred through loss of observations',/)
          if(aog.eq.'g')then
            write(20,1980)
          else
            write(20,1981)
          end if
          if(aog.eq.'g')then
            write(20,1990)
          else
            write(20,1991)
          end if
          if(mcount.eq.2)then
            if(aog.eq.'i')then
              write(20,2010) trim(aobs(1)),postcalvar
            else
              write(20,2010) trim(acontrib(2)),postcalvar
            end if
          else
            do i=2,mcount
              rtemp1=ocontrib(i)-ocontrib(1)
              if(aog.eq.'i')then
                write(20,2010) trim(aobs(i-1)),rtemp1
              else
                write(20,2010) trim(acontrib(i)),rtemp1
              end if
            end do
          end if
          write(6,910) trim(aprogram)
        end if

        call delfile(tolistfile_sub)

#ifdef FLUSHFILE
        call flush(20)
#endif

C -- Observation worth is now computed. First through observation addition.

C -- PREDVAR5 is run.

        if(awe.eq.'y')then
          aprogram='PREDVAR5'
          aprogram_lo='predvar5'
          write(6,2120)
2120      format(/,' - running PREDVAR5 to compute observ worth ',
     +    'through addition...')
          if(mcount.eq.2) go to 2159

          call delfile(toutfile)
          open(unit=30,file=tinfile,action='write')
          write(30,1740) trim(scaled_pestfile)
          write(30,1750) refvar
          write(30,1740) trim(scaled_uncertfile)
          write(30,1740) trim(tsenfile)
          write(30,1740) trim(tslistfile)
          write(30,1740) trim(tolistfile_add)
          write(30,'(a)') 'a'
          write(30,1740) trim(toutfile)
          close(unit=30)
#ifdef I64
          nnn=system('i64predvar5 < '//trim(tinfile)//trim(tonull))
#else
#ifdef SYS_FUNCTION
          nnn=system('predvar5 < '//trim(tinfile)//trim(tonull))
#else
          call system('predvar5 < '//trim(tinfile)//trim(tonull))
#endif
#endif
          open(unit=11,file=toutfile,status='old',err=9300)
          do i=1,3
            read(11,*,err=9300,end=9300)
          end do
          do i=1,mcount
            read(11,'(a)',err=9300,end=9300) cline
            call linspl(ifail,4,lw,rw,cline)
            call drealread(ifail,cline(lw(4):rw(4)),ocontrib(i))
            if(ifail.ne.0) go to 9300
          end do
          close(unit=11)

2159      continue
          write(20,*)
          write(20,*)
          write(20,2160) trim(apred)
2160      format(' PREDVAR5 observation addition analysis for ',
     +    'prediction "',a,'" ----->',/)
          write(20,2165)
2165      format(t5,'Decreases in pre-calibration predictive error ',
     +    'variance incurred through addition of observations',/)
          if(aog.eq.'g')then
            write(20,2180)
2180        format(t5,'Observation_group',t30,'Variance_decrease')
          else
            write(20,2181)
2181        format(t5,'Observation',t30,'Variance_decrease')
          end if
          if(aog.eq.'g')then
            write(20,2190)
2190        format(t5,'-----------------',t30,'-----------------')
          else
            write(20,2191)
2191        format(t5,'-----------',t30,'-----------------')
          end if
          if(mcount.eq.2)then
            if(aog.eq.'i')then
              write(20,2010) trim(aobs(1)),postcalerr
            else
              write(20,2010) trim(acontrib(2)),postcalerr
            end if
          else
            do i=2,mcount
              rtemp1=ocontrib(1)-ocontrib(i)
              if(aog.eq.'i')then
                write(20,2010) trim(aobs(i-1)),rtemp1
              else
                write(20,2010) trim(acontrib(i)),rtemp1
              end if
            end do
          end if
          write(6,910) trim(aprogram)
          call delfile(tslistfile)
        end if

#ifdef FLUSHFILE
        call flush(20)
#endif

C -- PREDUNC5 is run.

        if(awu.eq.'y')then
          aprogram='PREDUNC5'
          aprogram_lo='predunc5'
          write(6,2220)
2220      format(/,' - running PREDUNC5 to compute observ worth ',
     +    'through addition...')
          if(mcount.eq.2) go to 2259

          call delfile(toutfile)
          open(unit=30,file=tinfile,action='write')
          write(30,1740) trim(scaled_pestfile)
          write(30,1750) refvar
          write(30,1740) trim(scaled_uncertfile)
          write(30,1740) trim(tsenfile)
          write(30,1740) trim(tolistfile_add)
          write(30,'(a)') 'a'
          write(30,1740) trim(toutfile)
          write(30,'(a)')trim(avchoice)
          close(unit=30)
#ifdef I64
          nnn=system('i64predunc5 < '//trim(tinfile)//trim(tonull))
#else
#ifdef SYS_FUNCTION
          nnn=system('predunc5 < '//trim(tinfile)//trim(tonull))
#else
          call system('predunc5 < '//trim(tinfile)//trim(tonull))
#endif
#endif
          open(unit=11,file=toutfile,status='old',err=9300)
          do i=1,3
            read(11,*,err=9300,end=9300)
          end do
          do i=1,mcount
            read(11,'(a)',err=9300,end=9300) cline
            call linspl(ifail,3,lw,rw,cline)
            call drealread(ifail,cline(lw(3):rw(3)),ocontrib(i))
            if(ifail.ne.0) go to 9300
          end do
          close(unit=11)

2259      continue
          write(20,*)
          write(20,*)
          write(20,2260) trim(apred)
2260      format(' PREDUNC5 observation addition analysis for ',
     +    'prediction "',a,'" ----->',/)
          write(20,2265)
2265      format(t5,'Decreases in pre-calibration predictive ',
     +    'uncertainty variance incurred through addition of ',
     +    'observations',/)
          if(aog.eq.'g')then
            write(20,2180)
          else
            write(20,2181)
          end if
          if(aog.eq.'g')then
            write(20,2190)
          else
            write(20,2191)
          end if
          if(mcount.eq.2)then
            if(aog.eq.'i')then
              write(20,2010) trim(aobs(1)),postcalvar
            else
              write(20,2010) trim(acontrib(2)),postcalvar
            end if
          else
            do i=2,mcount
              rtemp1=ocontrib(1)-ocontrib(i)
              if(aog.eq.'i')then
                write(20,2010) trim(aobs(i-1)),rtemp1
              else
                write(20,2010) trim(acontrib(i)),rtemp1
              end if
            end do
          end if
          write(6,910) trim(aprogram)
        end if

        call delfile(tolistfile_sub)

2400    continue
        close(unit=20,iostat=ierr)
        call addquote(outfile,afile)
        write(6,2300) trim(afile)
2300    format(/,' - see file ',a,' for complete GENLINPRED output.')

        go to 9895

9000   call writint(aline,iline)
       write(amessage,9010) trim(aline),trim(afile)
9010   format('Error encountered when reading line ',a,' of PEST ',
     + 'control file ',a,'.')
       go to 9890
9050   write(amessage,9060) trim(afile)
9060   format('Premature end encountered to PEST control file ',a,'.')
       go to 9890
9150   call writint(aline,iline)
       write(amessage,9160) trim(aline),trim(afile)
9160   format('Insufficient entries on line ',a,' of file ',a,'.')
       go to 9890
9200   write(amessage,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890
9225   write(amessage,9226) trim(afile)
9226   format(' Error re-reading PEST control file ',a,'.')
       go to 9890
9250   write(amessage,9260) trim(asection),trim(afile)
9260   format('Cannot find "',a,'" section of PEST control file ',a,'.')
       go to 9890
9300   continue
       call program_error(aprogram,aprogram_lo,tinfile,numsup,toutfile,
     + scaled_pestfile,senfile,apred)
       go to 9900
9370   write(amessage,9380) trim(afile)
9380   format(' Error reading Jacobian matrix from file ',a,'.')
       go to 9890
9400   write(amessage,9410) trim(afile)
9410   format('Error reading predictive sensitivities from file ',a,'.')
       go to 9890
9500   continue
       write(amessage,9510) trim(arfile)
9510   format('Cannot write to response file ',a,'.')
       go to 9890
9520   call addquote(pestfile,bfile)
       write(amessage,9530) trim(afile),trim(bfile)
9530   format(' The Jacobian matrix contained in file ',a,
     + ' is not compatible with that of the inversion problem ',
     + 'supplied in PEST control file ',a,'.')
       go to 9890
9600   write(amessage,9610) trim(afile)
9610   format(' Error writing to Jacobian matrix file ',a,'.')
       go to 9890

9890   continue
       amessage=' '//trim(amessage)
       call writmess(6,amessage)
       close(unit=20,iostat=ierr)
       close(unit=90,iostat=ierr)

C -- Tidying up.

9895   continue
       call delfile(def_uncertfile)
       call delfile(scaled_uncertfile)
       call delfile(scaled_covfile)
       call delfile(scaled_pestfile)
       call delfile(scaled_jcofile)
       call delfile(tinfile)
       call delfile(toutfile)
       call delfile(tdatfile)
       call delfile(tsenfile)
       call delfile(tplistfile)
       call delfile(tslistfile)
       call delfile(tslistfile0)
       call delfile(tolistfile_sub)
       call delfile(tolistfile_add)
       call delfile(scaled_post_uncertfile)
       call delfile(scaled_post_covfile)

9900   deallocate(icovmat,nobgnm,npargnm,itrans,apar,
     + apargp,aobsgp,aobs,lbound,ubound,ident,
     + relerr,relunc,senscale,predvec,pval,stat=ierr)
       deallocate(acontrib,contrib,ocontrib,stat=ierr)

       deallocate(x,parcov,aaapar,aastring,stat=ierr)
       deallocate(tempvec,stat=ierr)

       end



       subroutine delfile(afile)

       implicit none
       character*(*), intent(in) :: afile
       integer                   :: ierr
       logical lexist

       if(afile.ne.' ')then
         inquire(file=afile,exist=lexist)
         if(lexist)then
#ifdef INTEL
           open(unit=19,file=afile,dispose='delete',iostat=ierr)
           close(unit=19,iostat=ierr)
#else
           open(unit=19,file=afile,status='old',iostat=ierr)
           if(ierr.eq.0)then
             close(unit=19,status='delete',iostat=ierr)
           end if
#endif
         end if
       end if

       end



       subroutine program_error(aprogram,aprogram_lo,tinfile,numsup,
     + toutfile,scaled_pestfile,senfile,apred)

C -- Subroutine PROGRAM_ERROR reports an error message arising from failure
C    of a program to run.

       implicit none

       character*(*), intent(in) :: aprogram,aprogram_lo,tinfile,
     +                              scaled_pestfile,senfile,apred

       integer,       intent(in) :: numsup
       character*(*), intent(in) :: toutfile

       character*10              :: anum
       character*200             :: afile

       write(6,*)
       write(6,5)
5      format(78('*'))
       write(6,*)
       write(6,10) trim(aprogram)
10     format(' An error was detected in running program ',a,'.')
       write(6,*)
       write(6,20)
20     format(' Run this program yourself using the command:-',/)
       if(aprogram_lo.eq.'identpar')then
         call writint(anum,numsup)
         write(6,25) trim(scaled_pestfile),trim(anum),trim(toutfile)
#ifdef I64
25       format('      i64identpar ',a,' ',a,' null null ',a)
#else
25       format('      identpar ',a,' ',a,' null null ',a)
#endif
       else if(aprogram_lo.eq.'jrow2vec')then
         call addquote(senfile,afile)
         write(6,26) trim(afile),trim(apred),trim(toutfile)
#ifdef I64
26       format('      i64jrow2vec ',a,' ',a,' ',a)
#else
26       format('      jrow2vec ',a,' ',a,' ',a)
#endif
       else
         write(6,30) trim(aprogram_lo),trim(tinfile)
#ifdef I64
30       format('       i64',a,' < ',a)
#else
30       format('     ',a,' < ',a)
#endif
       end if
       write(6,*)
       write(6,40)
40     format(' and monitor screen output to see the problem.')
       write(6,*)
       write(6,5)
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



        SUBROUTINE WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)

C -- SUBROUTINE WHICH1 LOCATES A STRING IN AN ARRAY

        INTEGER NPAR,IPAR,I
        INTEGER IFAIL
        CHARACTER*(*) TPAR
        CHARACTER*(*) APAR(NPAR)

        IFAIL=0
        IF((IPAR.LT.1).OR.(IPAR.GT.NPAR)) IPAR=1
        CALL LOWCAS(TPAR)
        IF(TPAR.EQ.APAR(IPAR)) RETURN
        IF(IPAR.NE.NPAR)THEN
          DO 20 I=IPAR+1,NPAR
          IF(TPAR.EQ.APAR(I))THEN
            IPAR=I
            RETURN
          END IF
20        CONTINUE
        END IF
        IF(IPAR.NE.1)THEN
          DO 40 I=IPAR-1,1,-1
          IF(TPAR.EQ.APAR(I)) THEN
            IPAR=I
            RETURN
          END IF
40        CONTINUE
        END IF
        IFAIL=1
        RETURN

        END



        SUBROUTINE DREALREAD(IFAIL,CLINE,RTEMP)

C -- Subroutine DREALREAD reads a real number from a string.

        INTEGER IFAIL
        DOUBLE PRECISION RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
        READ(CLINE,AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END


        subroutine endstrip(astring)

C -- Subroutine ENDSTRIP removes all characters following "!" in a text string.

        implicit none

        character*(*) ,intent(inout)   :: astring

        integer                        :: n

        n=index(astring,'!')
        if(n.eq.0) then
          continue
        else if(n.eq.1)then
          astring=' '
        else
          astring=astring(1:n-1)
        end if

        return

        end subroutine endstrip



       logical function skipline(cline)

       implicit none
       character*(*) cline
       integer nn,ll,icount,jcount,i

       skipline=.FALSE.
       cline=adjustl(cline)
       if((cline.eq.' ').or.(cline(1:2).eq.'++')) then
         skipline=.TRUE.
         go to 200
       end if
       nn=index(cline,'#')
       if(nn.eq.0)then
         go to 200
       else if(nn.eq.1)then
         skipline=.TRUE.
         go to 200
       end if
       ll=len_trim(cline)
       icount=0
       jcount=0
       do i=1,ll
         if(cline(i:i).eq.'''') then
           icount=1-icount
         else if(cline(i:i).eq.'"') then
           jcount=1-jcount
         else if(cline(i:i).eq.'#') then
           if((cline(i-1:i-1).eq.' ').or.                 ! Notice that we require the space before #
     +        (cline(i-1:i-1).eq.char(9))) then           ! This allows a filename to have a # in it (mostly)
             if((icount.eq.0).and.(jcount.eq.0)) then
               cline(i:)=' '
               if(cline.eq.' ')then
                 skipline=.TRUE.
               else
                 skipline=.FALSE.
               end if
               go to 200
             end if
           end if
         end if
       end do

200    continue
       return
       end





C -- Notes

C -- In manual point out that if we select a parameter for which to determine
C    sensitivities then it is really the log of the parameter.
C -- Also covariance matrix must pertain to parameter logs of course.

