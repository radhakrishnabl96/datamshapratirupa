       program REGPRED

C -- Program REGPRED starts with a PEST control dataset which was used for regularised
C    inversion. It then builds a file ready for regularised predictive uncertainty analysis.

       implicit none

       integer          i,j,ierr,nsdim,ifail,iline,ipar,iobsgp,jobsgp,
     +                  icount,nn,iobs,iprior,ne,iregulcount,irow,ii,
     +                  itemp1,itemp2,icode,ies,jes,icut,jnoreg,
     +                  regflag,k,jcut,nb,iicut,svditn,jline,itn
       integer          npredmaxmin,ipestmode,npar,nobs,npargp,nobsgp,
     +                  nprior,ntplfle,ninsfle,nxrow,nespar,bnxrow,
     +                  ncut,numcom,jacfile
       integer          ipredgroup,ipredcount,ipredobs,iprednoise
       integer          iunit,junit,itype,jpartype,vardim,covdim,
     +                  jobstype,iregwarn
       integer          ldu,ldvt,info,lwork
       integer          n_npar,n_nobs,n_npargp,n_nprior,n_nobsgp,
     +                  n_ntplfle,n_ninsfle
       integer          lw(10),rw(10)

       double precision pd0,owght,predweight,sum1,sum2,rtemp
       double precision vt(1,1)

       character*1      aa,asn
       character*4      aext
       character*10     aline,atrans,acut
       character*12     aversion
       character*12     aaobsgp,aapar
       character*15     aprecis,apoint,nulstring
       character*20     atype,aaobs,aweight,ctemp
       character*30     atemp,atemp1
       character*200    pestinfile,apestinfile,resfile,aresfile,
     +                  parcovfile,obscovfile,modbatch,afile,
     +                  pestoutfile,apestoutfile
       character*200    partplfile,parfile,parlogfile,veclogfile,
     +                  refparfile,pardiffile,projfile,projdiffile,
     +                  projcovfile,projinsfile,newmodbat,
     +                  svdfile,asvdfile
       character*400    cline
       character*600    dline,eline
       character*600    amessage

C -- Allocatable arrays

       integer,          allocatable :: iregobsgp(:),itrans(:),
     +                                  nobgnm(:),iwt(:)
       integer,          allocatable :: nnb(:),nne(:)
       double precision, allocatable :: pval(:),oval(:)
       double precision, allocatable :: parcov(:,:),parvar(:),
     +                                  obscov(:,:),obsvar(:),
     +                                  res(:,:),u(:,:),covarproj(:,:)
       double precision, allocatable :: s(:),work(:)
       character*12,     allocatable :: aobsgp(:),apar(:),bpar(:)
       character*20,     allocatable :: aobs(:),bobs(:)

       partplfile  = 'par###.tpl'
       parfile     = 'par###.mat'
       parlogfile  = 'parlog###.mat'
       veclogfile  = 'partran###.mat'
       refparfile  = 'refpar###.mat'
       pardiffile  = 'pardiff###.mat'
       projfile    = 'proj###.mat'
       projdiffile = 'projdiff###.mat'
       projcovfile = 'projcov###.mat'
       projinsfile = 'proj###.ins'
       newmodbat   = 'regpredbat.bat'


#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,50) trim(aversion)
50     format(/,' REGPRED Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

100    write(6,110,advance='no')
110    format(' Enter name of existing PEST control file: ')
       read(5,'(a)') pestinfile
       if(pestinfile.eq.' ') go to 100
       call remchar(pestinfile,'"')
       call remchar(pestinfile,'''')
       pestinfile=adjustl(pestinfile)
#ifndef UNIX
       call lowcase(pestinfile)
#endif
       i=len_trim(pestinfile)
       j=i-4
       if(j.lt.1)j=1
       atemp=pestinfile(j:i)
       if(index(atemp,'.').eq.0) then
         pestinfile=trim(pestinfile)//'.pst'
       end if
       i=len_trim(pestinfile)
       atemp=pestinfile(i-3:i)
       call lowcase(atemp)
       if(atemp(1:4).ne.'.pst')then
         write(6,109)
109      format(/,' PEST control file must have an extension of ',
     +   '".pst" - try again.',/)
         go to 100
       end if
       call addquote(pestinfile,apestinfile)
       open(unit=10,file=pestinfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(6,115) trim(apestinfile)
115      format(/,' Cannot open file ',a,' - try again.',/)
         go to 100
       end if

130    write(6,140,advance='no')
140    format(' Does it contain optimised parameter and observation ',
     + 'values?  [y/n]: ')
       read(5,'(a)') aa
       if(aa.eq.'Y')aa='y'
       if(aa.eq.'N')aa='n'
       if((aa.ne.'y').and.(aa.ne.'n')) go to 130
       if(aa.eq.'n')then
         write(amessage,150)
150      format(' Then use PARREP and OBSREP to build such a PEST ',
     +   'control file.')
         go to 9800
       end if

       write(6,*)
1501   write(6,1511,advance='no')
1511   format(' Use resolution or SVD matrix for null space ',
     + 'projection? [r/s]: ')
       read(5,'(a)') asn
       if(asn.eq.' ') go to 1501
       if(asn.eq.'R') asn='r'
       if(asn.eq.'S') asn='s'
       if((asn.ne.'r').and.(asn.ne.'s')) go to 1501

       if(asn.eq.'r')then
200      write(6,*)
210      write(6,220,advance='no')
220      format(' Enter name of resolution matrix file: ')
         read(5,'(a)') resfile
         if(resfile.eq.' ') go to 210
         call remchar(resfile,'"')
         call remchar(resfile,'''')
         resfile=adjustl(resfile)
#ifndef UNIX
         call lowcase(resfile)
#endif
         call addquote(resfile,aresfile)
         open(unit=11,file=resfile,status='old',err=210)
       else
1521     write(6,1531,advance='no')
1531     format(' Enter name of of SVD matrix file: ')
         read(5,'(a)') svdfile
         if(svdfile.eq.' ') go to 1521
         call remchar(svdfile,'"')
         call remchar(svdfile,'''')
         svdfile=adjustl(svdfile)
#ifndef UNIX
         call lowcase(svdfile)
#endif
         call addquote(svdfile,asvdfile)
         open(unit=11,file=svdfile,status='old',err=1521)
1540     write(6,1550,advance='no')
1550     format(' Get SVD for which iteration number? <Enter if 1>: ')
         read(5,'(a)') atemp
         if(atemp.eq.' ') then
           svditn=1
         else
           call intread(ifail,atemp,svditn)
           if(ifail.ne.0) go to 1540
           if(svditn.le.0) go to 1540
         end if
       end if

240    write(6,250,advance='no')
250    format(' Enter dimensions of solution space: ')
       read(5,*,err=240) nsdim
       if(nsdim.le.0)then
         write(6,260)
260      format(' *** This must be greater than zero - ',
     +   'try again ***')
         go to 240
       end if

       write(6,*)
350    write(6,360,advance='no')
360    format(' Enter name of parameter uncertainty file: ')
       read(5,'(a)')parcovfile
       if(parcovfile.eq.' ') go to 350
       call remchar(parcovfile,'''')
       call remchar(parcovfile,'"')
       parcovfile=adjustl(parcovfile)
#ifndef UNIX
       call lowcase(parcovfile)
#endif
       open(unit=12,file=parcovfile,status='old',err=350)

380    write(6,390,advance='no')
390    format(' Enter name of observation uncertainty file: ')
       read(5,'(a)')obscovfile
       if(obscovfile.eq.' ') go to 380
       call remchar(obscovfile,'''')
       call remchar(obscovfile,'"')
       obscovfile=adjustl(obscovfile)
#ifndef UNIX
       call lowcase(obscovfile)
#endif
       open(unit=13,file=obscovfile,status='old',err=380)

       write(6,*)
400    write(6,410,advance='no')
410    format(' Enter name for new predictive analysis PEST control ',
     + 'file: ')
       read(5,'(a)') pestoutfile
       if(pestoutfile.eq.' ') go to 400
       call remchar(pestoutfile,'"')
       call remchar(pestoutfile,'''')
       pestoutfile=adjustl(pestoutfile)
#ifndef UNIX
       call lowcase(pestoutfile)
#endif
       i=len_trim(pestoutfile)
       j=i-4
       if(j.lt.1)j=1
       atemp=pestoutfile(j:i)
       if(index(atemp,'.').eq.0) then
         pestoutfile=trim(pestoutfile)//'.pst'
       end if
       i=len_trim(pestoutfile)
       atemp=pestoutfile(i-3:i)
       call lowcase(atemp)
       if(atemp(1:4).ne.'.pst')then
         write(6,109)
         go to 400
       end if
       call addquote(pestoutfile,apestoutfile)
       open(unit=20,file=pestoutfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(6,420) trim(apestoutfile)
420      format(/,' Cannot open file ',a,' for output - try again.',/)
         go to 400
       end if

423    write(6,422,advance='no')
422    format(' Maximise or minimise prediction? [a/i]: ')
       read(5,'(a)') aa
       if(aa.eq.'A')aa='a'
       if(aa.eq.'I')aa='i'
       if((aa.ne.'a').and.(aa.ne.'i')) go to 423
       if(aa.eq.'a')then
         npredmaxmin=1
       else
         npredmaxmin=-1
       end if
424    write(6,425,advance='no')
425    format(' Enter value for PDO (<Enter> if 9.0): ')
       read(5,'(a)') atemp
       if(atemp.eq.' ')then
         pd0=9.0d0
       else
         call drealread(ifail,atemp,pd0)
         if(ifail.ne.0) go to 424
         if(pd0.le.0.0d0) go to 424
       end if

C -- There is one final piece of information to gather. But first we need to read the PEST
C    input dataset.

       write(6,421) trim(apestinfile)
421    format(/,' Reading PEST control file ',a,' for first time...')

       iline=1
       read(10,'(a)',err=9000,end=9000) cline
       call lowcase(cline)
       if(cline(1:3).ne.'pcf') go to 9000
       iline=iline+1
       read(10,'(a)',err=9000,end=9000) cline
       call lowcase(cline)
       if((index(cline,'*').eq.0).or.(index(cline,'control').eq.0))
     + go to 9000
       iline=iline+1
       read(10,'(a)',err=9000,end=9000) cline
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       atemp=cline(lw(2):rw(2))
       call lowcase(atemp)
       if(atemp(1:5).eq.'estim')then
         ipestmode=1
       else if(atemp(1:7).eq.'predict')then
         ipestmode=2
       else if(atemp(1:5).eq.'regul')then
         ipestmode=3
       else
         go to 9000
       end if
       iline=iline+1
       read(10,'(a)',err=9000,end=9000) cline
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.ne.0)then
         call writint(aline,iline)
         write(amessage,440) trim(aline),trim(apestinfile)
440      format(' Five entries are expected on line ',a,' of file ',
     +   a,'.')
         go to 9800
       end if
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
       if(nobsgp.eq.0)then
         call writint(aline,iline)
         write(amessage,450) trim(aline),trim(apestinfile)
450      format(' NOBSGP supplied as zero at line ',a,' of PEST ',
     +   'control file ',a,'.')
         go to 9800
       end if
       if(nobsgp.lt.0)then
         call writint(aline,iline)
         write(amessage,460) trim(aline),trim(apestinfile)
460      format(' NOBSGP supplied as negative at line ',a,' of PEST ',
     +   'control file ',a,'.')
         go to 9800
       end if

       iline=iline+1
       call writint(aline,iline)
       read(10,'(a)',err=9000,end=9000) cline
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0)then
         write(amessage,441) trim(aline),trim(apestinfile)
441      format(' Four entries are expected on line ',a,' of file ',
     +   a,'.')
         go to 9800
       end if
       call intread(ifail,cline(lw(1):rw(1)),ntplfle)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(2):rw(2)),ninsfle)
       if(ifail.ne.0) go to 9000
       aprecis=cline(lw(3):rw(3))
       call lowcase(aprecis)
       apoint=cline(lw(4):rw(4))
       call lowcase(apoint)
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.eq.0)then
         call intread(ifail,cline(lw(5):rw(5)),numcom)
         if(ifail.ne.0)then
           write(amessage,446) trim(aline),trim(apestinfile)
446        format(' Cannot read value for NUMCOM from line ',a,
     +     ' of PEST control file ',a,'.')
           go to 9800
         else if(numcom.ne.1)then
           write(amessage,442) trim(aline),trim(apestinfile)
442        format(' If supplied, NUMCOM must be "1" at line ',a,
     +     ' of PEST control file ',a,'.')
           go to 9800
         end if
         call linspl(ifail,6,lw,rw,cline)
         if(ifail.eq.0)then
           call intread(ifail,cline(lw(6):rw(6)),jacfile)
           if(ifail.ne.0)then
             write(amessage,443) trim(aline),trim(apestinfile)
443          format(' Cannot read value for JACFILE from line ',a,
     +       ' of PEST control file ',a,'.')
             go to 9800
           else if(jacfile.ne.0)then
             write(amessage,444) trim(aline),trim(apestinfile)
444          format(' If supplied, JACFILE must be "0" at line ',a,
     +       ' of PEST control file ',a,'.')
             go to 9800
           end if
         end if
       end if

C -- Some memory is allocated.

       nxrow=nobs+nprior
       allocate(aobsgp(nobsgp),iregobsgp(nobsgp),
     + stat=ierr)
       if(ierr.ne.0) go to 9200

       allocate(apar(npar),pval(npar),itrans(npar),stat=ierr)
       if(ierr.ne.0) go to 9200

       allocate(aobs(nxrow),nobgnm(nxrow),oval(nxrow),iwt(nxrow),
     + stat=ierr)
       if(ierr.ne.0) go to 9200

C -- The "parameter data" section is found and read.

       nespar=0
       do
         iline=iline+1
         read(10,'(a)',err=9000,end=9050) cline
         cline=adjustl(cline)
         if(index(cline,'*').ne.0) call lowcase(cline)
         if(index(cline,'* parameter dat').ne.0) go to 481
         if(index(cline,'* svd assist').ne.0) then
           write(amessage,484) trim(apestinfile)
484        format(' PEST control file ',a,' must not instruct PEST ',
     +     'to use SVD-assist. Only a base PEST control file ',
     +     'can be used with REGPRED.')
           go to 9800
         end if
       end do
481    continue
       do ipar=1,npar
         iline=iline+1
         read(10,*,err=9000,end=9000) apar(ipar),atrans,atemp,pval(ipar)
         call lowcase(apar(ipar))
         if(apar(ipar)(1:3).eq.'iw_')then
           write(amessage,482)
482        format(' REGPRED cannot accomodate adaptive ',
     +     'regularisation parameters; the names of these parameters ',
     +     'begin with "iw_". If these are not adaptive ',
     +     'regularisation parameters alter their names. If they are ',
     +     'adaptive regularisation parameters, then get rid of them.')
           go to 9800
         end if
         call lowcase(atrans)
         if(atrans.eq.'log')then
           itrans(ipar)=1
           nespar=nespar+1
         else if(atrans.eq.'none')then
           itrans(ipar)=0
           nespar=nespar+1
         else if(atrans.eq.'fixed')then
           itrans(ipar)=-1000000
         else if(atrans.eq.'tied')then
           itrans(ipar)=-1
         else
           go to 9000
         end if
         if((pval(ipar).le.0.0d0).and.(itrans(ipar).eq.1))then
           write(amessage,483) trim(apar(ipar)),trim(apestinfile)
483        format(' Parameter "',a,'" is log transformed in file ',a,
     +     '; however it does not have a positive initial value.')
           go to 9800
         end if
       end do

       if(nsdim.gt.nespar-1)then
         call writint(atemp,nsdim)
         call writint(atemp1,nespar)
         write(amessage,489) trim(apestinfile),trim(atemp1),trim(atemp)
489      format(' PEST control file ',a,' cites ',a,' adjustable ',
     +   'parameters. You have said above that the dimensions of the ',
     +   'solution space are ',a,'. This is not allowed. The ',
     +   'dimensions of the solution space must be less than the ',
     +   'number of adjustable parameters for REGPRED to be of use.')
         go to 9800
       end if

C -- The "observation groups" section is found and observation groups read.

       do
         iline=iline+1
         read(10,'(a)',err=9000,end=490) cline
         if(index(cline,'*').ne.0) then
           call lowcase(cline)
           if(index(cline,'observation gr').ne.0) go to 500
         end if
       end do
490    write(amessage,491) trim(apestinfile)
491    format(' Cannot find "observation groups" section of PEST ',
     + 'control file ',a,'.')
       go to 9800
500    do iobsgp=1,nobsgp
         iline=iline+1
         read(10,'(a)',err=9000,end=9000) cline
         call linspl(ifail,1,lw,rw,cline)
         aobsgp(iobsgp)=cline(lw(1):rw(1))
         call lowcase(aobsgp(iobsgp))
         if(aobsgp(iobsgp)(1:1).eq.'*') go to 9000
         if(iobsgp.gt.1)then
           do jobsgp=1,iobsgp-1
             if(aobsgp(jobsgp).eq.aobsgp(iobsgp))then
               call writint(aline,iline)
               write(amessage,510) trim(aline),trim(apestinfile)
510            format(' Duplicate observation group name found at ',
     +         'line ',a,' of PEST control file ',a,'.')
               go to 9800
             end if
           end do
         end if
       end do
       if(ipestmode.eq.3)then
         icount=0
         do iobsgp=1,nobsgp
           if(aobsgp(iobsgp)(1:5).eq.'regul') icount=icount+1
         end do
         if(icount.eq.0)then
           write(amessage,520) trim(apestinfile)
520        format(' PEST control file ',a,' instructs PEST to run ',
     +     'in "regularisation" mode. However there are no ',
     +     'observation groups whose names begin with "regul" ',
     +     'cited in this file.')
           go to 9800
         end if
       end if
       if(ipestmode.ne.3)then
         do iobsgp=1,nobsgp
           if(aobsgp(iobsgp)(1:5).eq.'regul')then
             write(amessage,522) trim(apestinfile)
522          format(' To avoid confusion in the present instance, ',
     +       'PEST control file ',a,' should not contain an ',
     +       'observation group whose name begins with "regul" as ',
     +       'it does not instruct PEST to run in "regularisation" ',
     +       'mode. Please rename this group and re-run REGPRED.')
             go to 9800
           end if
         end do
       end if

C -- The names and observation groups of observations are read.

       do
         iline=iline+1
         read(10,'(a)',err=9000,end=525) cline
         call lowcase(cline)
         cline=adjustl(cline)
         if(index(cline,'*').ne.0) call lowcase(cline)
         if(index(cline,'* observation data').ne.0) go to 523
       end do
525    write(amessage,526) trim(apestinfile)
526    format(' Cannot find "observation data" section of PEST ',
     + 'control file ',a,'.')
       go to 9800
523    continue
       iobsgp=1
       do iobs=1,nobs
         iline=iline+1
         read(10,*,err=9000,end=9000) aobs(iobs),oval(iobs),owght,atemp
         call lowcase(aobs(iobs))
         call lowcase(atemp)
         call whichone(ifail,nobsgp,iobsgp,aobsgp,atemp)
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,550) trim(atemp),trim(aline),trim(apestinfile)
550        format(' Observation group "',a,'" cited at line ',a,
     +     ' of PEST control file ',a,' does not feature in ',
     +     '"observation groups" section of this file.')
           go to 9800
         end if
         nobgnm(iobs)=iobsgp
         if(owght.lt.0.0d0)then
           call writint(aline,iline)
           write(amessage,552) trim(aline),trim(apestinfile)
552        format(' Negative weight provided for observation at line ',
     +     a,' of PEST control file ',a,'.')
           go to 9800
         else if(owght.eq.0.0d0)then
           iwt(iobs)=0
         else
           iwt(iobs)=1
         end if
       end do

C -- We now read the "model command line" section.

       do
         iline=iline+1
         read(10,'(a)',err=9000,end=540) cline
         call lowcase(cline)
         if(index(cline,'* model comman').ne.0) go to 531
         if(index(cline,'* derivatives command').ne.0)then
           write(amessage,530) trim(apestinfile)
530        format(' PEST control file ',a,' has a "derivatives ',
     +     'command line" section; as presently programmed REGPRED ',
     +     'cannot accomodate this.')
           go to 9800
         end if
       end do
540    write(amessage,541) trim(apestinfile)
541    format(' Cannot find "model command line" section in file ',
     + a,'.')
       go to 9800
531    continue
       iline=iline+1
       read(10,'(a)',err=9000,end=9000) modbatch
       call remchar(modbatch,'"')
       call remchar(modbatch,'''')
       modbatch=adjustl(modbatch)
#ifndef UNIX
       call lowcase(modbatch)
#endif
       afile=modbatch
       call lowcase(afile)
       nn=len_trim(afile)
       i=nn-3
       if(i.lt.1)i=1
       aext=modbatch(i:)
       if(aext.ne.'.bat')then
         call writint(aline,iline)
         write(amessage,560) trim(aline),trim(apestinfile)
560      format(' Model command line cited at line ',a,' of PEST ',
     +   'control file ',a,' must end in ".bat" and be ',
     +   'a batch/script file.')
         go to 9800
       end if

C -- The names and observation groups of prior information equations are obtained.

       if(nprior.eq.0) go to 700
       do
         iline=iline+1
         read(10,'(a)',err=9000,end=580) cline
         if(index(cline,'*').ne.0) then
           call lowcase(cline)
           cline=adjustl(cline)
           if(index(cline,'prior info').ne.0) go to 591
         end if
       end do
580    write(amessage,590) trim(apestinfile)
590    format(' Cannot find "prior information" section of ',
     + 'PEST control file ',a,'.')
       go to 9800
591    continue
       do iprior=1,nprior
         dline=' '
         if(iprior.eq.1)then
600        iline=iline+1
           read(10,'(a)',err=9300,end=9300) cline
           cline=adjustl(cline)
         end if
         dline=cline
610      iline=iline+1
         if(iprior.ne.nprior)then
           read(10,'(a)',err=9000,end=9000) cline
         else
           read(10,'(a)',iostat=ierr)cline
           if(ierr.ne.0) cline=' '
         end if
         cline=adjustl(cline)
         if(cline(1:1).eq.'&')then
           cline(1:1)=' '
           cline=adjustl(cline)
           dline=trim(dline)//' '//trim(cline)
           go to 610
         end if
         ne=index(dline,'=')
         if(ne.eq.0) then
           iline=iline-1
           go to 9300
         end if
         eline=dline
         call linesplit(ifail,1,lw,rw,eline)
         aobs(iprior+nobs)=eline(lw(1):rw(1))
         call lowcase(aobs(iprior+nobs))
         dline=dline(ne+1:)
         call linspl(ifail,3,lw,rw,dline)
         if(ifail.ne.0) then
           call linspl(ifail,2,lw,rw,dline)
           if(ifail.eq.0)then
             call writint(aline,iline-1)
             write(amessage,620) trim(aline),trim(apestinfile)
620          format(' Observation group name missing from prior ',
     +       'information equation at or about line ',a,' of PEST ',
     +       'control file ',a,'.')
             go to 9800
           else
             iline=iline-1
             go to 9300
           end if
         end if
         aaobsgp=dline(lw(3):rw(3))
         call lowcase(aaobsgp)
         call whichone(ifail,nobsgp,iobsgp,aobsgp,aaobsgp)
         if(ifail.ne.0)then
           call writint(aline,iline-1)
           write(amessage,630) trim(aaobsgp),trim(aline),
     +     trim(apestinfile)
630        format(' Unknown observation group "',a,'" in prior ',
     +     'information equation at or about line ',a,' of PEST ',
     +     'control file ',a,'.')
           go to 9800
         end if
         nobgnm(iprior+nobs)=iobsgp
         atemp=dline(lw(1):rw(1))
         call drealread(ifail,atemp,oval(iprior+nobs))
         if(ifail.ne.0)then
           call writint(aline,iline-1)
           write(amessage,650) trim(aline),trim(apestinfile)
650        format(' Cannot read prior information value on or about ',
     +     'line ',a,' of PEST control file ',a,'.')
           go to 9800
         end if
         atemp=dline(lw(2):rw(2))
         call drealread(ifail,atemp,owght)
         if(ifail.ne.0)then
           call writint(aline,iline-1)
           write(amessage,651) trim(aline),trim(apestinfile)
651        format(' Cannot read prior information weight on or ',
     +     'about line ',a,' of PEST control file ',a,'.')
           go to 9800
         end if
         if(owght.lt.0.0d0)then
           call writint(aline,iline-1)
           write(amessage,652) trim(aline),trim(apestinfile)
652        format(' Negative prior information weight at or about line ',
     +     a,' of PEST control file ',a,'.')
           go to 9800
         else if(owght.eq.0.0d0)then
           iwt(nobs+iprior)=0
         else
           iwt(nobs+iprior)=1
         end if
       end do

700    continue

C -- We now count the number of observations that belong to the group "predict"
C    and, if necessary, ask the user a final question about whether predictive
C    noise is required in new PEST control file.

       ipredgroup=0
       ipredcount=0
       ipredobs=0
       iprednoise=0
       do i=1,nobsgp
         if(aobsgp(i).eq.'predict') then
           ipredcount=ipredcount+1
           ipredgroup=i
         end if
       end do
       if(ipredcount.ne.0) go to 730
       if(ipestmode.eq.2)then
         write(amessage,710) trim(apestinfile)
710      format(' PEST control file ',a,' instructs PEST to run in ',
     +   '"predictive analysis" mode. However there is no ',
     +   'observation group named "predict".')
         go to 9800
       end if
       go to 751
730    continue
       ipredcount=0
       do i=1,nxrow
         if(nobgnm(i).eq.ipredgroup)then
           ipredcount=ipredcount+1
           ipredobs=i
         end if
       end do
       if(ipredcount.eq.0)then
         if(ipestmode.eq.2)then
           write(amessage,735) trim(apestinfile)
735        format(' PEST control file ',a,' instructs PEST to run ',
     +     'in "predictive analysis" mode. However no observations ',
     +     'or prior information equations belong to the observation ',
     +     'group "predict".')
           go to 9800
         end if
       else if(ipredcount.gt.1)then
         if(ipestmode.eq.2)then
           write(amessage,736) trim(apestinfile)
736        format(' PEST control file ',a,' instructs PEST to run ',
     +     'in "predictive analysis" mode. However there must only ',
     +     'be one member of the observation group "predict".')
           go to 9800
         else
           write(amessage,737) trim(apestinfile)
737        format(' There must not be more than one member of the ',
     +     'observation group "predict" in PEST control file ',a,'.')
           go to 9800
         end if
       else if (ipredcount.eq.1)then
749      write(6,*)
         write(6,750,advance='no') trim(aobs(ipredobs))
750      format(' Treat single member "',a,'" of observation group ',
     +   '"predict" ',/,'   as "the prediction" in new PEST control ',
     +   'file?  [y/n]: ')
         read(5,'(a)') aa
         if(aa.eq.'Y')aa='y'
         if(aa.eq.'N')aa='n'
         if((aa.ne.'y').and.(aa.ne.'n')) go to 749
         if(aa.eq.'n')then
           write(amessage,760) trim(apestinfile)
760        format(' Then re-assign this to a new observation group ',
     +     'in PEST control file ',a,'.')
           go to 9800
         end if
       end if
       if(ipredcount.eq.1)then
770      write(6,780,advance='no')
780      format(' Incorporate predictive noise in pred. anal. ',
     +   'process  [y/n]: ')
         read(5,'(a)') aa
         if(aa.eq.'Y')aa='y'
         if(aa.eq.'N')aa='n'
         if((aa.ne.'y').and.(aa.ne.'n')) go to 770
         if(aa.eq.'y')then
           iprednoise=1
790        write(6,800,advance='no')
800        format(' Enter value for prediction weight: ')
           read(5,*,err=790) predweight
           if(predweight.le.0.0d0)then
             write(6,810)
810          format(' *** Weight must be positive - try again ***')
             go to 790
           end if
         end if
         write(6,*)
       end if
751    continue

C -- We test for members of observation group regul if not running in
C    regularisation mode.

       iregulcount=0
       icount=0
       bnxrow=nxrow
       do i=1,nobsgp
         if(aobsgp(i)(1:5).eq.'regul')then
           iregobsgp(i)=1
           icount=icount+1
         else
           iregobsgp(i)=0
         end if
       end do
       if(icount.ne.0)then
         bnxrow=0
         do irow=1,nxrow
           ii=nobgnm(irow)
           if(iregobsgp(ii).ne.0) then
             iregulcount=iregulcount+1
           else
             bnxrow=bnxrow+1
           end if
         end do
       end if
       allocate(bobs(bnxrow),stat=ierr)
       if(ierr.ne.0) go to 9200
       if(ipestmode.ne.3)then
         bobs=aobs
       else
         i=0
         do irow=1,nxrow
           ii=nobgnm(irow)
           if(iregobsgp(ii).eq.0)then
             i=i+1
             bobs(i)=aobs(irow)
           end if
         end do
       end if

       write(6,830) trim(apestinfile)
830    format(' - PEST control file ',a,' read ok.')

C -- The parameter uncertainty file is read.

       allocate(parcov(nespar,nespar),parvar(1),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(bpar(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           bpar(ies)=apar(ipar)
         end if
       end do

       write(6,850)
850    format(/,' Reading parameter uncertainty data...')
       iunit=12
       junit=16
       itype=2
       vardim=1
       covdim=nespar
       atype='parameter'
       call read_uncert_data_file(ifail,iunit,junit,itype,jpartype,
     + nespar,vardim,covdim,parvar,parcov,atype,parcovfile,amessage,
     + cline,bpar)
       if(ifail.ne.0) go to 9800
       write(6,860)
860    format(' - parameter uncertainty data read ok.')

       deallocate(bpar,stat=ierr)

C -- The observation uncertainty file is read.

       allocate(obscov(1,1),obsvar(bnxrow),stat=ierr)
       if(ierr.ne.0) go to 9200

       write(6,870)
870    format(/,' Reading observation uncertainty data...')
       iunit=13
       junit=16
       itype=1                ! Thus no covariance data is allowed.
       vardim=bnxrow
       covdim=1
       atype='observation'
       call read_uncert_data_file(ifail,iunit,junit,itype,jobstype,
     + bnxrow,vardim,covdim,obsvar,obscov,atype,obscovfile,amessage,
     + cline,bobs)
       if(ifail.ne.0) go to 9800
       write(6,875)
875    format(' - observation uncertainty data read ok.')

C -- The resolution matrix is now read.

       allocate(res(nespar,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       ncut=nespar-nsdim

       if(asn.eq.'r')then
         write(6,900) trim(aresfile)
900      format(/,' Reading resolution matrix file ',a,'...')
         read(11,'(a)',err=9350,end=9350) cline
         call linesplit(ifail,3,lw,rw,cline)
         if(ifail.ne.0) go to 9350
         atemp=cline(lw(1):rw(1))
         call intread(ifail,atemp,itemp1)
         if(ifail.ne.0) go to 9350
         if(itemp1.ne.nespar) go to 9400
         atemp=cline(lw(2):rw(2))
         call intread(ifail,atemp,itemp2)
         if(ifail.ne.0) go to 9350
         if(itemp2.ne.nespar) go to 9400
         atemp=cline(lw(3):rw(3))
         call intread(ifail,atemp,icode)
         if(ifail.ne.0) go to 9350
         if((icode.ne.-1).and.(icode.ne.1).and.(icode.ne.2))then
           write(amessage,910) trim(aresfile)
910        format(' ICODE must be "-1", "1" or "2" at first line ',
     +     'of resolution matrix file ',a,'.')
           go to 9800
         end if
         if(icode.gt.0)then
           do ies=1,nespar
             read(11,*,iostat=ierr) (res(ies,jes),jes=1,nespar)
             if(ierr.ne.0)then
               call writint(atemp,ies)
               write(amessage,920) trim(atemp),trim(aresfile)
920            format(' Error reading matrix row number ',a,' from ',
     +         'resolution matrix file ',a,'.')
               go to 9800
             end if
           end do
         else
           res=0.0d0  ! an array
           do ies=1,nespar
             read(11,*,iostat=ierr) res(ies,ies)
             if(ierr.ne.0)then
               call writint(atemp,ies)
               write(amessage,920) trim(atemp),trim(aresfile)
               go to 9800
             end if
           end do
         end if
         read(11,'(a)',iostat=ierr) cline
         if(ierr.ne.0)then
           write(amessage,925) trim(aresfile)
925        format(' Error reading row/column names from matrix file ',
     +     a,'.')
           go to 9800
         end if
         call lowcase(cline)
         if(index(cline,'name').eq.0)then
           if(abs(icode).eq.1)then
             write(amessage,930) trim(aresfile)
930          format('"* row and column names" section header not ',
     +       'found in resolution matrix file ',a,'.')
           else
             write(amessage,940) trim(aresfile)
940          format('"* row names" section header not found in ',
     +       'resolution matrix file ',a,'.')
           end if
           go to 9800
         end if
         ipar=0
         do ies=1,nespar
           read(11,'(a)',iostat=ierr) aapar
           if(ierr.ne.0)then
             call writint(atemp,ies)
             write(amessage,950) trim(atemp),trim(aresfile)
950          format(' Error reading name of parameter number ',a,
     +       ' from resolution matrix file ',a,'.')
             go to 9800
           end if
           call lowcase(aapar)
960        ipar=ipar+1
           if(itrans(ipar).lt.0) go to 960
           if(aapar.ne.apar(ipar)) go to 9400
         end do
         close(unit=11)
         write(6,970) trim(aresfile)
970      format(' - file ',a,' read ok.')
       else
         jline=0
1564     continue
         do
           jline=jline+1
           read(11,'(a)',end=1565) cline
           if(index(cline,'ITERATION').ne.0) go to 1570
         end do
1565     call writint(atemp,svditn)
         write(amessage,1566) trim(asvdfile),trim(atemp)
1566     format(' Unexpected end encountered to SVD file ',a,' while ',
     +   'looking for data pertaining to iteration number ',a,'.')
         go to 9800
1570     continue
         cline=cline(37:)
         cline=adjustl(cline)
         atemp=cline(1:12)
         call intread(ifail,atemp,itn)
         if(ifail.ne.0)then
           call writint(aline,jline)
           write(amessage,1580) trim(aline),trim(asvdfile)
1580       format(' Cannot read iteration number from line ',
     +     a,' of file ',a,'.')
           go to 9800
         end if
         if(itn.ne.svditn) go to 1564
         do
           read(11,'(a)',end=1601) cline
           if(index(cline,'EIGENVECTOR').ne.0) go to 1622
         end do
1601     call writint(aline,svditn)
         write(amessage,1611) trim(aline),trim(asvdfile)
1611     format(' Cannot find eigenvectors for iteration ',a,
     +   ' in SVD file ',a,'.')
         go to 9800
1622     continue
         do ies=1,nespar
           read(11,*,iostat=ierr) (res(ies,jes),jes=1,nespar)
           if(ierr.ne.0)then
             call writint(aline,svditn)
             write(amessage,1631) trim(aline),trim(asvdfile)
1631         format(' Error reading matrix of eigenvectors ',
     +       'for iteration ',a,' from SVD file ',a,'.')
             go to 9800
           end if
         end do
         close(unit=11)
         write(6,970) trim(asvdfile)
       end if

C -- Singular value decomposition is now undertaken on resolution matrix.

       if(asn.eq.'r')then
         write(6,980)
980      format(/,' Undertaking singular value decomposition of ',
     +   'resolution matrix...')
       end if

       lwork=8*nespar
       allocate(s(nespar),work(lwork),u(nespar,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(covarproj(nespar,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       covarproj=res
       if(asn.eq.'s') go to 1700
       ldu=nespar
       ldvt=1
       call dgesvd('A','N',nespar,nespar,res,nespar,s,u,ldu,vt,
     + ldvt,work,lwork,info)

       if(info.ne.0)then
         write(amessage,990)
990      format(' SVD was not successful.')
         go to 9800
       end if

       write(6,1000)
1000   format(' - singular value decomposition process successful.')

C -- We now truncate the decomposed resolution matrix and record some data.

       call writint(atemp,ncut)
       write(6,1010) trim(atemp)
1010   format(/,' For last ',a,' eigenvalues of resolution matrix...')

       write(atemp,'(1pg12.5)') abs(s(nsdim+1))
       atemp=adjustl(atemp)
       write(6,1020) trim(atemp)
1020   format('    highest singular value = ',a)
       write(atemp,'(1pg12.5)') abs(s(nespar))
       atemp=adjustl(atemp)
       write(6,1030) trim(atemp)
1030   format('    lowest singular value  = ',a)

C -- The projection matrix (last NCUT columns of U) is now written to a file.

1700   continue
       call addquote(projfile,afile)
       write(6,1060) trim(afile)
1060   format(/,' Writing projection matrix to file ',a,'...')
       open(unit=21,file=projfile,action='write',err=9450)
       write(21,1065,err=9450) ncut,nespar,2
1065   format(3i5)
       do icut=1,ncut
         iicut=icut+nsdim
         if(asn.eq.'r')then
           write(21,1070,err=9450) (u(ies,iicut),ies=1,nespar)
1070       format(8(1x,1pg14.7))
         else
           write(21,1070,err=9450) (res(ies,iicut),ies=1,nespar)
         end if
       end do
       write(21,1075)
1075   format('* row names')
       do icut=1,ncut
         call writint(atemp,icut)
         write(21,1079) trim(atemp)
1079     format('parproj',a)
       end do
       write(21,1080)
1080   format('* column names')
       do ipar=1,npar
         if(itrans(ipar).lt.0) cycle
         write(21,1090) trim(apar(ipar))
1090     format(a)
       end do
       close(unit=21)
       write(6,1100) trim(afile)
1100   format(' - file ',a,' written ok.')

C -- The covariance matrix of projected parameters is calculated. But first we look at the
C    beginning and the end of non-zero elements in each row of the pre-calibration parameter
C    covariance matrix.

       write(6,1110)
1110   format(/,' Computing covariance matrix of projected ',
     + 'parameters...')

       allocate(nne(nespar),nnb(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- First we calculate (I-R) and then U'(I-R)

       if(asn.eq.'r')then
         do ies=1,nespar
           do jes=1,nespar
             if(jes.eq.ies)then
               res(jes,ies)=1.0d0-covarproj(jes,ies)
             else
               res(jes,ies)=-covarproj(jes,ies)
             end if
           end do
         end do

         do icut=1,ncut
           iicut=icut+nsdim
           do ies=1,nespar
             sum1=0.0d0
             do k=1,nespar
               sum1=sum1+u(k,iicut)*res(k,ies)
             end do
             covarproj(icut,ies)=sum1
           end do
         end do
         do ies=1,nespar
           do icut=1,ncut
             res(icut,ies)=covarproj(icut,ies)
           end do
         end do
       else
         covarproj=res
         do ies=1,nespar
           do icut=1,ncut
             res(icut,ies)=covarproj(ies,icut+nsdim)
           end do
         end do
       end if
       do ies=1,nespar
         do jes=1,ies
           if(parcov(jes,ies).ne.0.0d0)then
             nnb(ies)=jes
             go to 1120
           end if
         end do
         nnb(ies)=ies
1120     continue
         do jes=nespar,ies,-1
           if(parcov(jes,ies).ne.0.0d0)then
             nne(ies)=jes
             go to 1130
           end if
         end do
         nne(ies)=ies
1130     continue
       end do
       do icut=1,ncut
         do jcut=1,icut
           sum1=0.0d0
           do ies=1,nespar
             sum2=0.0d0
             do jes=nnb(ies),nne(ies)
               sum2=sum2+res(jcut,jes)*parcov(jes,ies)
             end do
             sum1=sum1+sum2*res(icut,ies)
           end do
           covarproj(icut,jcut)=sum1
         end do
       end do
       do icut=1,ncut-1
         do jcut=icut+1,ncut
           covarproj(icut,jcut)=covarproj(jcut,icut)
         end do
       end do
       write(6,1140)
1140   format(' - covariance matrix computed ok.')

C -- The covariance matrix of projected parameters is written to a file.

       call addquote(projcovfile,afile)
       write(6,1160) trim(afile)
1160   format(/,' Writing covariance matrix to file ',a,'...')
       open(unit=21,file=projcovfile,action='write',err=9450)
       write(21,1065,err=9450) ncut,ncut,1
       do icut=1,ncut
         write(21,1070,err=9450) (covarproj(icut,jcut),jcut=1,ncut)
       end do
       write(21,1076)
1076   format('* row and column names')
       do icut=1,ncut
         call writint(atemp,icut)
         write(21,1079) trim(atemp)
       end do
       close(unit=21)
       write(6,1100) trim(afile)

C -- The new parameter template file is written.

       call addquote(partplfile,afile)
       write(6,1150) trim(afile)
1150   format(/,' Writing parameter template file ',a,'...')
       open(unit=21,file=partplfile,action='write',err=9450)
       write(21,1155)
1155   format('ptf $')
       write(21,1065,err=9450) nespar,1,2
       do ipar=1,npar
         if(itrans(ipar).lt.0) cycle
         write(21,1157,err=9450) trim(apar(ipar))
1157     format('$',a,t15,'$')
       end do
       write(21,1161,err=9450)
1161   format('* row names')
       do ipar=1,npar
         if(itrans(ipar).lt.0) cycle
         write(21,1170,err=9450) trim(apar(ipar))
1170     format(a)
       end do
       write(21,1080,err=9450)
       write(21,1190,err=9450)
1190   format('col1')
       close(unit=21)
       write(6,1100) trim(afile)

C -- The VECLOG parameter transformation file is now written.

       call addquote(veclogfile,afile)
       write(6,1200) trim(afile)
1200   format(/,' Writing VECLOG parameter transformation file ',
     + a,'...')
       open(unit=21,file=veclogfile,action='write',err=9450)
       write(21,1065,err=9450) nespar,1,2
       do ipar=1,npar
         if(itrans(ipar).lt.0) cycle
         write(21,1210,err=9450) itrans(ipar)
1210     format(i5)
       end do
       write(21,1161,err=9450)
       do ipar=1,npar
         if(itrans(ipar).lt.0) cycle
         write(21,1170,err=9450) trim(apar(ipar))
       end do
       write(21,1080,err=9450)
       write(21,1190,err=9450)
       close(unit=21)
       write(6,1100) trim(afile)

C -- A vector of reference parameter values is now written.

       call addquote(refparfile,afile)
       write(6,1240) trim(afile)
1240   format(/,' Writing reference parameter file ',a,'...')
       open(unit=21,file=refparfile,action='write',err=9450)
       write(21,1065,err=9450) nespar,1,2
       do ipar=1,npar
         if(itrans(ipar).lt.0) then
           cycle
         else if(itrans(ipar).eq.0)then
           write(21,1250) pval(ipar)
1250       format(1x,1pg14.7)
         else if(itrans(ipar).eq.1)then
           write(21,1250) log10(pval(ipar))
         end if
       end do
       write(21,1161,err=9450)
       do ipar=1,npar
         if(itrans(ipar).lt.0) cycle
         write(21,1170,err=9450) trim(apar(ipar))
       end do
       write(21,1080,err=9450)
       write(21,1190,err=9450)
       close(unit=21)
       write(6,1100) trim(afile)

C -- An instruction file to read the parameter projections is now read.

       call addquote(projinsfile,afile)
       write(6,1270) trim(afile)
1270   format(/,' Writing instruction file ',a,'...')
       open(unit=21,file=projinsfile,action='write',err=9450)
       write(21,1280)
1280   format('pif $')
       do icut=1,ncut
         call writint(atemp,icut)
         if(icut.eq.1)then
           write(21,1290) trim(atemp)
1290       format('l2  [projdiff',a,']1:25')
         else
           write(21,1300) trim(atemp)
1300       format('l1  [projdiff',a,']1:25')
         end if
       end do
       close(unit=21)
       write(6,1100) trim(afile)

C -- The new model batch file is written.

       call addquote(newmodbat,afile)
       write(6,1310) trim(afile)
1310   format(/,' Writing new model batch file ',a,'...')
       open(unit=15,file=modbatch,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(modbatch,afile)
         write(amessage,1320) trim(afile)
1320     format(' Cannot open existing model batch file ',a,'.')
         go to 9800
       end if
       open(unit=21,file=newmodbat,action='write',err=9450)
#ifndef UNIX
       write(21,1319,err=9450)
1319   format('@echo off')
#endif
       write(21,1329,err=9450)
#ifdef UNIX
1329   format('###############################################',
     + '#############')
#else
1329   format('REM ###########################################',
     + '#############')
#endif
       write(21,1330,err=9450)
#ifdef UNIX
1330   format('### The following part of this file was written',
     + ' by REGPRED.')
#else
1330   format('REM The following part of this file was written',
     + ' by REGPRED.')
#endif
       write(21,1329,err=9450)
       write(21,1340,err=9450)
#ifdef UNIX
1340   format(/,'### First some files are deleted.'/)
#else
1340   format(/,'REM First some files are deleted.'/)
#endif
       write(21,1350,err=9450) trim(parlogfile)
#ifdef UNIX
1350   format('/bin/rm ',a,' > /dev/null')
#else
1350   format('del ',a,' > nul')
#endif
       write(21,1350,err=9450) trim(pardiffile)
C       write(21,1350,err=9450) trim(projdiffile)
       write(21,1360,err=9450)
#ifdef UNIX
1360   format(/,'### Projected parameter differences are evaluated.',/)
#else
1360   format(/,'REM Projected parameter differences are evaluated.',/)
#endif
#ifdef UNIX
       nulstring=' > /dev/null'
#else
       nulstring=' > nul'
#endif
       write(21,1370,err=9450) trim(parfile),trim(veclogfile),
     + trim(parlogfile),trim(nulstring)
1370   format('veclog ',a,' ',a,' ',a,a)
       write(21,1380,err=9450) trim(parlogfile),trim(refparfile),
     + trim(pardiffile),trim(nulstring)
1380   format('matdiff ',a,' ',a,' ',a,a)
       write(21,1390,err=9450) trim(projfile),trim(pardiffile),
     + trim(projdiffile),trim(nulstring)
1390   format('matprod ',a,' ',a,' ',a,a)
       write(21,*,err=9450)
       write(21,1395,err=9450)
#ifdef UNIX
1395   format('#######################################################',
     + '##################')
#else
1395   format('REM ###################################################',
     + '##################')
#endif
       write(21,1400,err=9450) trim(modbatch)
#ifdef UNIX
1400   format('### The remainder of this file is copied directly ',
     + 'from the original model '/,'### batch file ',a,'.')
#else
1400   format('REM The remainder of this file is copied directly ',
     + 'from the original model '/,'REM batch file ',a,'.')
#endif
       write(21,1395,err=9450)
       write(21,*,err=9450)
       do
         read(15,'(a)',end=1335) cline
#ifndef UNIX
         ctemp=cline(1:20)
         call lowcase(ctemp)
         if(ctemp.eq.'@echo off') cycle
#endif
         write(21,'(a)',err=9450) trim(cline)
       end do
1335   close(unit=15)
       close(unit=21)
       write(6,1365) trim(afile)
1365   format(' - file ',a,' written ok.')

C -- The new PEST input file is written (after the other one is re-wound).

       write(6,1366) trim(apestoutfile)
1366   format(/,' Writing PEST control file ',a,'...')
       rewind(unit=10)
       iline=0
       do i=1,3
         iline=iline+1
         read(10,*,err=9600,end=9600)
       end do
       write(20,1382,err=9500)
1382   format('pcf')
       write(20,1383,err=9500)
1383   format('* control data')
       write(20,1384,err=9500)
1384   format('restart prediction')

       iline=iline+1
       read(10,*,err=9600)
       n_npar=npar
       n_nobs=0
       iregwarn=0
       do i=1,nobs
         ii=nobgnm(i)
         if(iregobsgp(ii).ne.1)then
           n_nobs=n_nobs+1
         else
           iregwarn=1
         end if
       end do
       n_nobs=n_nobs+ncut
       n_npargp=npargp
       n_nprior=nprior
       if(nprior.ne.0)then
         n_nprior=0
         do i=nobs+1,nxrow
           ii=nobgnm(i)
           if(iregobsgp(ii).ne.1)n_nprior=n_nprior+1
         end do
       end if
       n_nobsgp=0
       do i=1,nobsgp
         if(aobsgp(i)(1:5).eq.'regul') cycle
         n_nobsgp=n_nobsgp+1
       end do
       n_nobsgp=n_nobsgp+1           ! for the new observation group
       if(ipredgroup.eq.0) n_nobsgp=n_nobsgp+1
       write(20,1391,err=9500) n_npar,n_nobs,n_npargp,n_nprior,n_nobsgp
1391   format(5i5)

       n_ntplfle=ntplfle+1
       n_ninsfle=ninsfle+1
       read(10,'(a)',err=9600) cline
       call linesplit(ifail,5,lw,rw,cline)
       if(ifail.ne.0)then
         write(20,1392) n_ntplfle,n_ninsfle,trim(aprecis),trim(apoint)
1392     format(i5,i5,2x,a,2x,a)
       else
         write(20,1393) n_ntplfle,n_ninsfle,trim(aprecis),trim(apoint),
     +   trim(cline(lw(5):))
1393     format(i5,i5,2x,a,2x,a,2x,a)
       end if
       do i=1,3
         iline=iline+1
         read(10,'(a)',err=9600,end=9600) cline
         call lowcase(cline)
         write(20,'(a)',err=9500) trim(cline)
       end do
       iline=iline+1
       read(10,'(a)',err=9600,end=9600) cline
       call linesplit(ifail,2,lw,rw,cline)
       write(20,1394,err=9500) '50',trim(cline(lw(2):))
1394   format(1x,a,2x,a)
       do
         iline=iline+1
         read(10,'(a)',err=9600,end=1401) cline
         call lowcase(cline)
         if(index(cline,'* singular').ne.0)then
           do i=1,3
             iline=iline+1
             read(10,'(a)',err=9600,end=9600) cline
           end do
         else if(index(cline,'* observation group').ne.0)then
           write(20,'(a)',err=9500) trim(cline)
           go to 1430
         else
           write(20,'(a)',err=9500) trim(cline)
         end if
       end do
1401   write(amessage,1410) trim(apestinfile)
1410   format(' Cannot find "observation groups" section when ',
     + 're-reading PEST control file ',a,'.')
       go to 9800
1430   do i=1,nobsgp
         iline=iline+1
         read(10,*,err=9600,end=9600)
         if(aobsgp(i)(1:5).eq.'regul') cycle
         if(aobsgp(i).ne.'predict')write(20,1440,err=9500)
     +   trim(aobsgp(i))
1440     format(1x,a)
       end do
       call addquote(projcovfile,afile)
       write(20,1445,err=9500) 'projdiff',trim(afile)
1445   format(1x,a,2x,a)
       write(20,'(a)',err=9500) ' predict'

       iline=iline+1
       read(10,'(a)',err=9600) cline
       if(index(cline,'* observation d').eq.0)then
         write(amessage,1450) trim(apestinfile)
1450     format(' Cannot locate "observation data" section on ',
     +   're-reading PEST control file ',a,'.')
         go to 9800
       end if
       write(20,'(a)',err=9500) trim(cline)
       do i=1,nobs
         iline=iline+1
         read(10,*,err=9600,end=9600)
       end do
       jnoreg=0
       do i=1,nobs
         ii=nobgnm(i)
         if(iregobsgp(ii).eq.1) cycle
         jnoreg=jnoreg+1
         if(ipredobs.eq.i)then
           cycle
         else
           if(iwt(i).eq.0)then
             write(20,1470,err=9500) trim(aobs(i)),oval(i),0.0,
     +       trim(aobsgp(ii))
           else
             rtemp=1.0d0/sqrt(obsvar(jnoreg))
             write(20,1470,err=9500) trim(aobs(i)),oval(i),rtemp,
     +       trim(aobsgp(ii))
1470         format(1x,a,t22,1pg14.7,2x,1pg14.7,2x,a)
           end if
         end if
       end do
       do icut=1,ncut
         call writint(acut,icut)
         aaobs='projdiff'//trim(acut)
         write(20,1470,err=9500) trim(aaobs),0.0,1.0,'projdiff'
       end do
       if(ipredobs.ne.0)then
         if(iprednoise.eq.0)then
           rtemp=0.0d0
         else
           rtemp=predweight
         end if
         write(20,1470,err=9500) trim(aobs(ipredobs)),oval(ipredobs),
     +   rtemp,'predict'
       end if

       iline=iline+1
       read(10,*,err=9600,end=9600)
       iline=iline+1
       read(10,*,err=9600,end=9600)

       write(20,1480,err=9500)
1480   format('* model command line')
       call addquote(newmodbat,afile)
       write(20,1490,err=9500) trim(afile)
1490   format(1x,a)


       iline=iline+1
       read(10,*,err=9600,end=9600)
       write(20,1500,err=9500)
1500   format('* model input/output')
       do i=1,ntplfle
         iline=iline+1
         read(10,'(a)',err=9600,end=9600) cline
         write(20,'(a)',err=9500) trim(cline)
       end do
       write(20,1520,err=9500) trim(partplfile),trim(parfile)
1520   format(1x,a,2x,a)
       do i=1,ninsfle
         iline=iline+1
         read(10,'(a)',err=9600,end=9600) cline
         write(20,'(a)',err=9500) trim(cline)
       end do
       write(20,1530,err=9500) trim(projinsfile),trim(projdiffile)
1530   format(1x,a,1x,a)

C -- Prior information is now handled if necessary.

       if(n_nprior.eq.0) go to 1800
       do
         iline=iline+1
         read(10,'(a)',err=9600,end=1600) cline
         if(index(cline,'*').ne.0)then
           call lowcase(cline)
           if(index(cline,'prior info').ne.0) go to 1620
         end if
       end do
1600   write(amessage,1610) trim(afile)
1610   format(' Cannot find "prior information" section of ',
     + 'PEST control file ',a,' when re-reading this file.')
       go to 9800
1620   continue
       write(20,1621,err=9500)
1621   format('* prior information')
       do iprior=1,nprior
         dline=' '
         if(iprior.eq.1)then
1630       iline=iline+1
           read(10,'(a)',err=9600,end=9600) cline
           cline=adjustl(cline)
         end if
         dline=cline
1650     iline=iline+1
         if(iprior.ne.nprior)then
           read(10,'(a)',err=9600,end=9600) cline
         else
           read(10,'(a)',iostat=ierr)cline
           if(ierr.ne.0) cline=' '
         end if
         cline=adjustl(cline)
         if(cline(1:1).eq.'&')then
           cline(1:1)=' '
           cline=adjustl(cline)
           dline=trim(dline)//' '//trim(cline)
           go to 1650
         end if
         ne=index(dline,'=')
         if(ne.eq.0) go to 9600
         eline=dline
         dline=dline(ne+1:)
         call linspl(ifail,3,lw,rw,dline)
         aaobsgp=dline(lw(3):rw(3))
         call lowcase(aaobsgp)
         if(aaobsgp(1:5).ne.'regul') then
           jnoreg=jnoreg+1
           regflag=0
         else
           regflag=1
         end if
1670     continue
         if(regflag.eq.0)then
           if(iwt(iprior+nobs).eq.0)then
             owght=0.0d0
           else
             owght=1.0d0/sqrt(obsvar(jnoreg))
           end if
           write(aweight,'(1pg13.6)') owght
           dline=dline(1:lw(2)-1)//trim(aweight)//dline(rw(2)+1:)
           eline=eline(1:ne)//' '//trim(dline)
1685       continue
           nb=len_trim(eline)
           if(nb.lt.200)then
             write(20,'(a)',err=9500) trim(eline)
           else
             do i=200,1,-1
               if(eline(i:i).eq.' ')then
                 write(20,'(a)',err=9500) eline(1:i)
                 eline(1:i)=' '
                 eline='& '//adjustl(eline)
                 go to 1685
               end if
             end do
           end if
         end if
       end do

C -- The predictive analysis of the PEST control file is now written.

1800   continue
       write(20,1810,err=9500)
1810   format('* predictive analysis')
       write(20,1820,err=9500) npredmaxmin,iprednoise
1820   format(2i5)
       write(20,1830,err=9500) pd0,1.01*pd0,2.0*pd0
1830   format(3(2x,1pg12.5))
       write(20,1840,err=9500)
1840   format(' 0.00  0.0005  0.3 1.3  15')
       write(20,1850,err=9500)
1850   format(' 0.00  0.005')
       write(20,1860,err=9500)
1860   format(' 6  0.00 0.0000005  6')

       close(unit=20)
       write(6,1865) trim(apestoutfile)
1865   format(' - PEST control file ',a,' written ok.')

       if(ipredobs.eq.0)then
         write(amessage,1870) trim(apestinfile)
1870     format('Warning: there is no prediction cited in PEST ',
     +   'control file ',a,'. Nevertheless REGPRED built ',
     +   'a PEST input dataset for regularised predictive ',
     +   'analysis. You must now ',
     +   'add a prediction to this dataset.')
         call writmess(6,amessage)
       end if

       if(iregwarn.ne.0)then
         write(amessage,1880) trim(apestinfile),trim(apestoutfile)
1880     format('Warning: regularisation observations have been ',
     +   'removed from PEST control file ',a,' in writing the new ',
     +   'PEST control file ',a,'. It is the user''s responsibility ',
     +   'to remove pertinent instruction files and model ',
     +   'components.')
         call writmess(6,amessage)
       end if


       go to 9900


9000   call writint(aline,iline)
       write(amessage,9010) trim(aline),trim(apestinfile)
9010   format(' Error reading line ',a,' of PEST control file ',a,
     + ': check this file with PESTCHEK.')
       go to 9800
9050   write(amessage,9060) trim(apestinfile)
9060   format(' Cannot find "parameter data" section in PEST ',
     + 'control file ',a,'.')
       go to 9800
9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800
9300   call writint(aline,iline)
       write(amessage,9310) trim(aline),trim(apestinfile)
9310   format(' Error reading prior information on or about line ',a,
     + ' of PEST control file ',a,'.')
       go to 9800
9350   write(amessage,9360) trim(aresfile)
9360   format(' Error reading first line of matrix file ',a,'.')
       go to 9800
9400   write(amessage,9410) trim(aresfile),trim(apestinfile)
9410   format(' Resolution matrix contained in file ',a,
     + ' must cite the same adjustable parameters in the same order ',
     + 'as PEST control file ',a,'. If necessary use MATORDER ',
     + 'to rectify this.')
       go to 9800
9450   write(amessage,9460) trim(afile)
9460   format(' Error encountered opening or writing to file ',a,'.')
       go to 9800
9500   write(amessage,9510) trim(apestoutfile)
9510   format(' Error encountered opening or writing to file ',a,'.')
       go to 9800
9600   call writint(aline,iline)
       write(amessage,9610) trim(aline),trim(apestinfile)
9610   format(' Error re-reading line ',a,' of PEST input control ',
     + 'file ',a,'.')
       go to 9800


9800   continue
       call writmess(6,amessage)


9900   deallocate (iregobsgp,itrans,nobgnm,iwt,stat=ierr)
       deallocate (nnb,nne,stat=ierr)
       deallocate (pval,oval,stat=ierr)
       deallocate (parcov,parvar,obscov,obsvar,res,u,covarproj,
     +            stat=ierr)
       deallocate (s,work,stat=ierr)
       deallocate (aobsgp,apar,stat=ierr)
       deallocate (aobs,bobs,stat=ierr)


       end


        SUBROUTINE LOWCASE(ASTRNG)

C -- Subroutine LOWCASE converts a string to lower case.

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,LEN_TRIM(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN
        END



        SUBROUTINE DREALREAD(IFAIL,CLINE,RTEMP)

C -- Subroutine REALREAD reads a real number from a string.

        INTEGER IFAIL
        double precision RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') LEN_TRIM(CLINE)
        READ(CLINE,AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END


         SUBROUTINE LINESPLIT(IFAIL,NUM,LW,RW,CLINE)

C -- Subroutine LINESPLIT splits a string into blank-delimited fragments.

        INTEGER IFAIL,NW,NBLC,J,I
        INTEGER NUM
        INTEGER LW(NUM),RW(NUM)
        CHARACTER*(*) CLINE

        IFAIL=0
        NW=0
        NBLC=LEN_TRIM(CLINE)
        IF((NBLC.NE.0).AND.(INDEX(CLINE,CHAR(9)).NE.0)) THEN
          CALL TABREM(CLINE)
          NBLC=LEN_TRIM(CLINE)
        ENDIF
        IF(NBLC.EQ.0) THEN
          IFAIL=-1
          RETURN
        END IF
        J=0
5       IF(NW.EQ.NUM) RETURN
        DO 10 I=J+1,NBLC
          IF((CLINE(I:I).NE.' ').AND.(CLINE(I:I).NE.',').AND.
     +    (ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10      CONTINUE
        IFAIL=1
        RETURN
20      NW=NW+1
        LW(NW)=I
        DO 30 I=LW(NW)+1,NBLC
          IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',').OR.
     +    (ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30      CONTINUE
        RW(NW)=NBLC
        IF(NW.LT.NUM) IFAIL=1
        RETURN
40      RW(NW)=I-1
        J=RW(NW)
        GO TO 5

        END


        SUBROUTINE TABREM(CLINE)

C -- Subroutine TABREM removes tabs from a string.

        INTEGER I
        CHARACTER*(*) CLINE

        DO 10 I=1,LEN(CLINE)
10      IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '

        RETURN
        END






