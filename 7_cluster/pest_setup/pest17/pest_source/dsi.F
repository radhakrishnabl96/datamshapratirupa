       program dsi

! -- Program DSI establishes the expected posterior value of a prediction, as well as its
!    uncertainty, by analyzing the outcomes of an ensemble of history matching and predictive model runs.

       implicit none

       integer seed_size,iseed
       integer,allocatable                :: seed(:)

       logical :: dequals
       integer :: iline,jline,ierr,ifail,ldvt,ldu,i,nb,ibeg,iend
       integer :: nobs,iobs,nobswtnonzero,jobs,mobs
       integer :: npar,nreal,ireal,jreal
       integer :: itemp,ptrans
       integer :: lwork,info,ncol,icol,irow
       integer :: ichoice,itemp1,itemp2
       integer :: nsamp,icount1,icount2
       integer :: mid_prior_lim,up68_prior_lim,up95_prior_lim,low68_prior_lim,    &
                  low95_prior_lim,mid_post_lim,up68_post_lim,up95_post_lim,       &
                  low68_post_lim,low95_post_lim

       integer :: lw(10),rw(10)

       real             :: tol,rtemp
       double precision :: random_normal
       double precision :: dtemp,dtemp1,poffset,pscale,sum,sum1,dtemp2
       double precision :: vt(1,1),u(1,1)
       double precision :: mu1,var1,var2
       double precision :: pbaseval
       double precision :: mu1_prior,mid_prior,mid_post,std_dev_prior,std_dev_post
       double precision :: up68_prior,up95_prior,low68_prior,low95_prior
       double precision :: up68_post,up95_post,low68_post,low95_post
       double precision :: mid_prior_t,mid_post_t
       double precision :: minpval,maxpval
       double precision :: s1,thresh
       character (len=1)   :: aa,ab,jobu,jobvt,ans
       character (len=5)   :: aext
       character (len=12)  :: aversion
       character (len=15)  :: aline,anum,anum1
       character (len=20)  :: bobs,apred
       character (len=20)  :: atemp1,atemp2,atemp3,atemp4,atemp5,atemp20
       character (len=256) :: infile,afile,transfile,rrffile,storefile,prrffile,outfile
       character (len=500) :: cline
       character (len=1500) :: amessage

       integer, allocatable            :: otrans(:),obs2comp(:),comp2obs(:)
       integer, allocatable            :: iwork1(:)
       real, allocatable               :: oval_rank(:),orig_oval(:)
       real, allocatable               :: baseval(:)
       real, allocatable               :: nstval(:),wwork1(:),wwork2(:),orig_pval_sorted(:)
       double precision, allocatable   :: rval(:,:),pval(:)
       double precision, allocatable   :: mu2(:),oval(:),weight(:),offset(:),scale(:),s(:),c12(:)
       double precision, allocatable   :: ztz(:,:),work1(:),work(:),usneg1(:,:)
       character (len=20), allocatable :: aobs(:)

! -- Initialization

       include 'version.inc'
       write(6,5) trim(aversion)
5      format(/,' DIS Version ',a,'. Watermark Numerical Computing.',/)
       infile=' '
       transfile=' '

! -- We obtain the seed for the random number generator.

10     write(6,20,advance='no')
20     format(' Enter seed for random number generator (<Enter> if 1111): ')
       read(5,'(a)') anum
       if(anum.eq.' ')then
         iseed=1111
       else
         call intread(ifail,anum,iseed)
         if(ifail.ne.0) go to 10
       end if
       call random_seed(size=seed_size) ! find out size of seed
       allocate(seed(seed_size))
       seed=iseed                       ! an array
       call random_seed(put=seed)       ! set current seed

! -- Now we read a PEST control file and obtain the names and weights of observations.
! -- Note that it does not need to be a PEST control file. It just needs to be the "observation data"
!    section of a PEST control file.

100    write(6,110,advance='no')
110    format(' Enter name of PEST control file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 100
       ibeg=1
       iend=len(afile)
       call getfile(ifail,afile,infile,ibeg,iend)
       if(ifail.ne.0) go to 100
       nb=len_trim(infile)
       i=nb-3
       if(i.lt.1)i=1
       aext=infile(i:nb)
       call lowcase(aext)
       if(aext.ne.'.pst')infile=trim(infile)//'.pst'
       call addquote(infile,afile)
       open(unit=10,file=infile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(6,111) trim(afile)
111      format(' *** Cannot open file ',a,'. Try again ***')
         go to 100
       end if

! -- First we read the file once to determine the number of observations.

       iline=0
       nobs=0
       do
         iline=iline+1
         read(10,'(a)',err=9000,end=9050) cline
         if(cline(1:1).eq.'*')then
           call lowcase(cline)
           if(cline(1:18).eq.'* observation data') exit
         end if
       end do
       jline=iline
       do
         iline=iline+1
         read(10,'(a)',err=9000,end=140) cline
         cline=adjustl(cline)
         if(cline.eq.' ') cycle
         if(cline(1:1).eq.'#') cycle
         if(cline(1:2).eq.'++') cycle
         if(cline(1:1).eq.'*') go to 140
         nobs=nobs+1
       end do
140    continue
       if(nobs.eq.0)then
         write(amessage,150) trim(afile)
150      format('No observations found in "* observation data" section of file ',a,'.')
         go to 9890
       end if

! -- We now allocate memory for the observations and then read them.

       rewind(unit=10)
       do iline=1,jline
         read(10,*)
       end do
       allocate(aobs(nobs),oval(nobs),weight(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200
       iobs=0
       iline=jline
       nobswtnonzero=0
       do iobs=1,nobs
151      continue
         iline=iline+1
         read(10,'(a)',err=9000,end=9000) cline
         if(cline.eq.' ') go to 151
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') go to 151
         if(cline(1:2).eq.'++') go to 151
         call linesplit(ifail,3,lw,rw,cline)
         if(ifail.ne.0) go to 9100
         aobs(iobs)=cline(lw(1):rw(1))
         call lowcase(aobs(iobs))
         call drealread(ifail,cline(lw(2):rw(2)),oval(iobs))
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,180) trim(aline),trim(afile)
180        format('Cannot read observation value from line ',a,' of file ',a,'.')
           go to 9890
         end if
         call drealread(ifail,cline(lw(3):rw(3)),weight(iobs))
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,190) trim(aline),trim(afile)
190        format('Cannot read observation weight from line ',a,' of file ',a,'.')
           go to 9890
         end if
         if(weight(iobs).lt.0.0)then
           call writint(aline,iline)
           write(amessage,191) trim(aobs(iobs)),trim(aline),trim(afile)
191        format('Observation "',a,'" has a negative weight at line ',a,' of file ',a,'.')
           go to 9890
         end if
         if(weight(iobs).gt.0.0)nobswtnonzero=nobswtnonzero+1
       end do
145    continue
       close(unit=10)
       call writint(anum,nobs)
       write(6,210) trim(anum),trim(afile)
210    format(' - ',a,' observations read from file ',a,'.')
       call writint(anum,nobswtnonzero)
       write(6,211) trim(anum)
211    format(' - ',a,' of these have a nonzero weight.')
       if(nobswtnonzero.le.0)then
         write(amessage,351) trim(afile)
351      format('There are no non-zero-weighted observations in file ',a,'.')
         go to 9890
       end if

! -- We transform observations if desired.

       allocate(offset(nobs),scale(nobs),otrans(nobs),baseval(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200
       baseval=0.0d0  !an array
       write(6,*)
229    write(6,230,advance='no')
230    format(' Enter name of transformation file (<Enter> if none): ')
       read(5,'(a)',err=229) afile
       if(afile.eq.' ')then
         offset=0.0        ! array
         otrans=0          ! array
         scale=1.0         ! array
       else
         ibeg=1
         iend=len(afile)
         call getfile(ifail,afile,transfile,ibeg,iend)
         if(ifail.ne.0) go to 229
         call addquote(transfile,afile)
         open(unit=10,file=transfile,status='old',iostat=ierr)
         if(ierr.ne.0)then
           write(6,251) trim(afile)
251        format(' *** Cannot open file ',a,'. Try again ***')
           go to 229
         end if
270      write(6,280,advance='no')
280      format(' Skip a line at the top of this file?  [y/n]: ')
         read(5,'(a)') ab
         if((ab.eq.'Y').or.(ab.eq.'y'))then
           read(10,'(a)',end=9150) cline
           iline=1
         else if((ab.eq.'N').or.(ab.eq.'n'))then
           iline=0
           continue
         else
           go to 270
         end if
         do iobs=1,nobs
290        continue
           iline=iline+1
           read(10,'(a)',err=9000,end=9150) cline
           if(cline.eq.' ') go to 290
           cline=adjustl(cline)
           if(cline(1:1).eq.'#') go to 290
           call linesplit(ifail,4,lw,rw,cline)
           if(ifail.ne.0) go to 9100
           bobs=cline(lw(1):rw(1))
           call lowcase(bobs)
           if(bobs.ne.aobs(iobs))then
             call writint(aline,iline)
             write(amessage,310) trim(bobs),trim(aline),trim(afile)
310          format('Observation "',a,'" not expected at line ',a,' of file ',a,'.')
             go to 9890
           end if
           call drealread(ifail,cline(lw(2):rw(2)),scale(iobs))
           if(ifail.ne.0)then
             call writint(aline,iline)
             write(amessage,311) trim(aline),trim(afile)
311          format('Error reading scale value from line ',a,' of file ',a,'.')
             go to 9890
           end if
           if(scale(iobs).eq.0.0d0)then
             call writint(aline,iline)
             write(amessage,3111) trim(aline),trim(afile)
3111         format('Zero scale value supplied at line ',a,' of file ',a,'.')
             go to 9890
           end if
           call drealread(ifail,cline(lw(3):rw(3)),offset(iobs))
           if(ifail.ne.0)then
             call writint(aline,iline)
             write(amessage,320) trim(aline),trim(afile)
320          format('Error reading offset value from line ',a,' of file ',a,'.')
             go to 9890
           end if
           call intread(ifail,cline(lw(4):rw(4)),otrans(iobs))
           if(ifail.ne.0)then
             call writint(aline,iline)
             write(amessage,330) trim(aline),trim(afile)
330          format('Error reading log-transformation variable at line ',a,' of file ',a,'.')
             go to 9890
           end if
           if((otrans(iobs).ne.0).and.(otrans(iobs).ne.1))then
             call writint(aline,iline)
             write(amessage,340) trim(aline),trim(afile)
340          format('Fourth variable on line ',a,' of file ',a,' must be 0 or 1.')
             go to 9890
           end if
           if(otrans(iobs).eq.1)then
             call linesplit(ifail,5,lw,rw,cline)
             if(ifail.ne.0)then
               call writint(aline,iline)
               write(amessage,341) trim(aobs(iobs)),trim(aline),trim(afile)
341            format('Observation "',a,'" is subject to log-transformation at line ',a,   &
               ' of file ',a,'. However an emergency base value has not been provided in column ',  &
               '5 of this file.')
               go to 9890
             end if
             call realread(ifail,cline(lw(5):rw(5)),baseval(iobs))
             if(ifail.ne.0)then
               call writint(aline,iline)
               write(amessage,343) trim(aline),trim(afile)
343            format('Cannot read emergency base value at line ',a,' of file ',a,'.')
               go to 9890
             end if
             if(baseval(iobs).le.0.0d0)then
               call writint(aline,iline)
               write(amessage,342) trim(aline),trim(afile)
342            format('Emergency base value must be positive at line ',a,' of file ',a,'.')
               go to 9890
             end if
             if(baseval(iobs).gt.scale(iobs)*oval(iobs)+offset(iobs))then
               write(amessage,344) trim(aobs(iobs)),trim(afile)
344            format('Emergency base value supplied for observation "',a,'" in file ',a,     &
               ' is greater than observed value provided in PEST control file times scale plus offset ',  &
               'provided in this file.')
               go to 9890
             end if
           end if
         end do
         close(unit=10)
         write(6,350) trim(afile)
350      format(' - file ',a,' read ok.')
       end if

! -- Observed values are now transformed.

       allocate(orig_oval(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200
       orig_oval=oval             ! arrays
       do iobs=1,nobs
         oval(iobs)=scale(iobs)*oval(iobs)+offset(iobs)
         if(otrans(iobs).eq.1)then
           if(oval(iobs).lt.baseval(iobs)) oval(iobs)=baseval(iobs)
           oval(iobs)=log10(oval(iobs))
         end if
       end do

! -- To make arrays smaller we compress observations to get rid of those with zero weights.

       allocate(obs2comp(nobs),comp2obs(nobswtnonzero),stat=ierr)
       if(ierr.ne.0) go to 9200
       obs2comp=0           ! an array
       jobs=0
       do iobs=1,nobs
         if(weight(iobs).gt.0.0)then
           jobs=jobs+1
           obs2comp(iobs)=jobs
           comp2obs(jobs)=iobs
         end if
       end do
       do iobs=1,nobswtnonzero
         oval(iobs)=oval(comp2obs(iobs))
       end do

! -- Measurement noise standard deviations are calculated from weights.

       do iobs=1,nobs
         if(weight(iobs).gt.0.0) weight(iobs)=1.0/weight(iobs)
       end do

! -- The RRF file is now read to obtain model output values. At the same time random noise is added to these values.

       write(6,*)
370    write(6,380,advance='no')
380    format(' Enter name of calibration RRF file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 370
       ibeg=1
       iend=len(afile)
       call getfile(ifail,afile,rrffile,ibeg,iend)
       if(ifail.ne.0) go to 370
       call addquote(rrffile,afile)
       open(unit=10,file=rrffile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(6,111) trim(afile)
         go to 370
       end if
       iline=1
       read(10,'(a)',err=9000,end=9150) cline
       if(cline.ne.'* case dimensions')then
         write(amessage,390) trim(afile)
390      format('File ',a,' does not appear to be a RRF file.')
         go to 9890
       end if
       iline=iline+1
       read(10,*,err=9000,end=9150) npar,mobs
       if(mobs.ne.nobs)then
         write(amessage,400)
400      format('Number of observations featured in RRF file differs from that ',  &
         'featured in PEST control file.')
         go to 9890
       end if
       do
         iline=iline+1
         read(10,'(a)',err=9000,end=9070) cline
         if(cline.eq.'* observation names') exit
       end do
       do iobs=1,nobs
         iline=iline+1
         read(10,*,err=9000,end=9150) bobs
         call lowcase(bobs)
         if(bobs.ne.aobs(iobs))then
           call writint(aline,iline)
           write(amessage,410) trim(bobs),trim(aline)
410        format('Order of observations is not the same in RRF and PST files. First ', &
           'discrepancy is for observation "',a,'" at line ',a,' of RRF file.')
           go to 9890
         end if
       end do
420    write(6,430,advance='no')
430    format(' How many realizations to read from this file? ')
       read(5,*,err=420) nreal
       if(nreal.le.0) go to 420
       if(nreal.eq.1)then
         write(6,431)
431      format(' *** Must be greater than one. Try again ***')
         go to 420
       end if
       allocate(rval(nobswtnonzero,nreal),stat=ierr)
       if(ierr.ne.0) go to 9200
       do ireal=1,nreal
         do
           iline=iline+1
           read(10,'(a)',err=9000,end=9150) cline
           if(cline(1:1).eq.'*')then
             if(cline.eq.'* model output values') exit
           end if
         end do
         do iobs=1,nobs
           iline=iline+1
           read(10,*,err=9000,end=9150) dtemp
           itemp=obs2comp(iobs)
           if(itemp.ne.0)then
             dtemp1=random_normal()
             rval(itemp,ireal)=dtemp+dtemp1*weight(iobs)
           end if
         end do
       end do
       close(unit=10)
       write(6,440) trim(afile)
440    format(' - file ',a,' read ok.')

! -- We apply the transformation to model output calibration-counterparts.

       do jobs=1,nobswtnonzero
         iobs=comp2obs(jobs)
         dtemp=offset(iobs)
         dtemp2=scale(iobs)
         itemp=otrans(iobs)
         do ireal=1,nreal
           rval(jobs,ireal)=dtemp2*rval(jobs,ireal)+dtemp
         end do
         if(itemp.eq.1)then
           dtemp1=baseval(iobs)
           do ireal=1,nreal
             if(rval(jobs,ireal).lt.dtemp1) rval(jobs,ireal)=dtemp1
             rval(jobs,ireal)=log10(rval(jobs,ireal))
           end do
         end if
       end do

! -- An RRF file is now read to obtain model prediction values.

       allocate(pval(nreal),stat=ierr)
       if(ierr.ne.0) go to 9200
       write(6,*)
       call writint(anum,nreal)
500    write(6,510,advance='no') trim(anum)
510    format(' Enter name of RRF file holding ',a,' prediction realizations: ')
       read(5,'(a)',err=500) afile
       if(afile.eq.' ') go to 500
       ibeg=1
       iend=len(afile)
       call getfile(ifail,afile,prrffile,ibeg,iend)
       if(ifail.ne.0) go to 500
       open(unit=10,file=prrffile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(6,111) trim(afile)
         go to 500
       end if
520    write(6,530,advance='no')
530    format(' Enter name of prediction: ')
       read(5,*,err=520) apred
       call lowcase(apred)
       call addquote(prrffile,afile)
       iline=1
       read(10,'(a)',err=9000,end=9150) cline
       if(cline.ne.'* case dimensions')then
         write(amessage,390) trim(afile)
         go to 9890
       end if
       do
         iline=iline+1
         read(10,'(a)',err=9000,end=9070) cline
         if(cline.eq.'* observation names') exit
       end do
       jobs=0
       do
         iline=iline+1
         jobs=jobs+1
         read(10,'(a)',err=9000,end=9150) cline
         if(cline(1:1).eq.'*')then
           write(6,550) trim(apred)
550        format(' *** Prediction "',a,'" not found. Try again ***')
           rewind(unit=10)
           go to 520
         end if
         cline=adjustl(cline)
         call linesplit(ifail,1,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         call lowcase(cline)
         if(cline(lw(1):rw(1)).eq.apred)exit
       end do
       do ireal=1,nreal
         do
           iline=iline+1
           read(10,'(a)',err=9000,end=9150) cline
           if(cline(1:1).eq.'*')then
             if(cline.eq.'* model output values') exit
           end if
         end do
         do iobs=1,jobs
           iline=iline+1
           read(10,*,err=9000,end=9150) dtemp
         end do
         pval(ireal)=dtemp
       end do
       close(unit=10)
       call writint(anum,nreal)
       write(6,564) trim(anum),trim(afile)
564    format(' - ',a,' predictive realizations read from file ',a,'.')
       minpval=minval(pval)
       maxpval=maxval(pval)

! -- Do we apply a scale, an offset and log transform to the prediction?

       write(6,*)
565    write(6,566,advance='no')
566    format(' Transform prediction? [y/n]: ')
       read(5,'(a)') aa
       if((aa.eq.'y').or.(aa.eq.'Y'))then
         aa='y'
       else if((aa.eq.'N').or.(aa.eq.'n'))then
         aa='n'
       else
         go to 565
       end if
       if(aa.eq.'y')then
466      write(6,467,advance='no')
467      format(' Enter scale to apply to prediction: ')
         read(5,*,err=466) pscale
         if(pscale.eq.0.0d0)then
           write(6,581)
581        format(' *** Must not be zero. Try again ***')
           go to 466
         end if
570      write(6,580,advance='no')
580      format(' Enter offset to apply to prediction: ')
         read(5,*,err=570) poffset
590      write(6,600,advance='no')
600      format(' Log-transform scaled offset prediction? ')
         read(5,'(a)') aa
         if((aa.eq.'y').or.(aa.eq.'Y'))then
           ptrans=1
         else if((aa.eq.'n').or.(aa.eq.'N'))then
           ptrans=0
         else
           go to 590
         end if
         if(ptrans.eq.1)then
602        write(6,603,advance='no')
603        format(' Enter emergency base value for scaled offset prediction: ')
           read(5,*,err=602) pbaseval
           if(pbaseval.le.0.0d0)then
             write(6,604)
604          format(' *** Must be greater than zero. Try again ***')
             go to 602
           end if
         end if
         do ireal=1,nreal
           pval(ireal)=pscale*pval(ireal)+poffset
           if(ptrans.eq.1)then
             if(pval(ireal).le.pbaseval) pval(ireal)=pbaseval
             pval(ireal)=log10(pval(ireal))
           end if
         end do
       else
         pscale=1.0d0
         poffset=0.0d0
         ptrans=0
         pbaseval=0.0d0
       end if

! -- Find out if we do normal score transforms.

       write(6,*)
605    write(6,606,advance='no')
606    format(' Work in normal score transform space? [y/n]: ')
       read(5,'(a)',err=605) ans
       if((ans.eq.'Y').or.(ans.eq.'y'))then
         ans='y'
       else if((ans.eq.'N').or.(ans.eq.'n'))then
         ans='n'
       else
         go to 605
       end if

! -- Ask for name of output file:

       write(6,*)
614    write(6,615,advance='no')
615    format(' Enter name for output file: ')
       read(5,'(a)',err=614) afile
       if(afile.eq.' ') go to 614
       ibeg=1
       iend=len(afile)
       call getfile(ifail,afile,outfile,ibeg,iend)
       if(ifail.ne.0) go to 614
       open(unit=20,file=outfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(6,6151)
6151     format(' *** Cannot open file for output. Try again ***')
         go to 614
       end if

! -- Options for inverted data covariance matrix are obtained.

       write(6,*)
       write(6,620)
620    format(' For decomposed inverted data covariance matrix:-')
       write(6,621)
621    format('   If compute only      - enter 0: ')
       write(6,622)
622    format('   If compute and store - enter 1: ')
       write(6,623)
623    format('   If read and use      - enter 2: ')
6231   write(6,6243,advance='no')
6243   format(' Enter your choice: ')
       read(5,*,err=6231) ichoice
       if((ichoice.ne.0).and.(ichoice.ne.1).and.(ichoice.ne.2)) go to 6231
       if(ichoice.eq.1)then
625      write(6,626,advance='no')
626      format(' Enter name of binary file for storage: ')
         read(5,'(a)',err=625) afile
         if(afile.eq.' ') go to 625
         ibeg=1
         iend=len(afile)
         call getfile(ifail,afile,storefile,ibeg,iend)
         if(ifail.ne.0) go to 625
         open(unit=30,file=storefile,action='write',access='stream',iostat=ierr)
         if(ierr.ne.0)then
           write(6,6151)
           go to 625
         end if
       else if(ichoice.eq.2)then
6279     write(6,628,advance='no')
628      format(' Enter name of binary file to retrieve matrix: ')
         read(5,'(a)',err=6279) afile
         if(afile.eq.' ') go to 6279
         ibeg=1
         iend=len(afile)
         call getfile(ifail,afile,storefile,ibeg,iend)
         if(ifail.ne.0) go to 6279
         open(unit=30,file=storefile,action='read',access='stream',status='old',iostat=ierr)
         if(ierr.ne.0) then
           write(6,629)
629        format(' *** Cannot open file. Try again ***')
           go to 6279
         end if
       end if

       allocate(iwork1(max(nobs,nreal)),stat=ierr)
       if(ierr.ne.0) go to 9200

       if(ans.eq.'y')then

! -- If working in normal score transform space, we now set this up.

         write(6,*)
         write(6,1100)
1100     format(' - setting up normal score transform space...')
         allocate(nstval(nreal),wwork1(nreal),wwork2(nreal),orig_pval_sorted(nreal),stat=ierr)
         if(ierr.ne.0) go to 9200
         allocate(oval_rank(nobswtnonzero),stat=ierr)
         if(ierr.ne.0) go to 9200
         tol=1.0e-6
         nsamp=100000
         call randrealgen(nreal,nstval,wwork1,wwork2,tol,nsamp)

! -- We now assign normal scores to the observation realization array and observations themselves.

         write(6,1120)
1120     format(' - normal score transforming observations...')
         do iobs=1,nobswtnonzero
           wwork1=rval(iobs,:)
           call mysort_index(nreal,wwork1,iwork1)
           do ireal=1,nreal
             rval(iobs,iwork1(ireal))=nstval(ireal)
           end do
           call nst_transform(nreal,nstval,wwork1,oval(iobs),oval_rank(iobs))
         end do

         write(6,1130)
1130     format(' - normal score transforming predictions...')
         wwork1=pval
         call mysort_index(nreal,wwork1,iwork1)
         orig_pval_sorted=wwork1          ! arrays
         do ireal=1,nreal
           pval(iwork1(ireal))=nstval(ireal)
         end do

       end if

! -- We now compute the mu2 vector (i.e. the mean of each model output)

       allocate(mu2(nobswtnonzero),stat=ierr)
       if(ierr.ne.0) go to 9200
       do iobs=1,nobswtnonzero
         sum=0.0d0
         do ireal=1,nreal
           sum=sum+rval(iobs,ireal)
         end do
         mu2(iobs)=sum/nreal
       end do

! -- We now adjust the rval vector by subtracting mu2

       do iobs=1,nobswtnonzero
         dtemp=mu2(iobs)
         do ireal=1,nreal
           rval(iobs,ireal)=rval(iobs,ireal)-dtemp
         end do
       end do

! -- Next we compute mu1 and var1

       sum=0.0d0
       do ireal=1,nreal
         sum=sum+pval(ireal)
       end do
       mu1=sum/nreal
       sum=0.0d0
       do ireal=1,nreal
         sum=sum+(pval(ireal)-mu1)*(pval(ireal)-mu1)
       end do
       var1=sum/(nreal-1)

! -- We compute C12

       allocate(c12(nobswtnonzero),stat=ierr)
       if(ierr.ne.0) go to 9200
       do iobs=1,nobswtnonzero
         sum=0.0d0
         do ireal=1,nreal
           sum=sum+(pval(ireal)-mu1)*rval(iobs,ireal)
         end do
         c12(iobs)=sum/(nreal-1)   ! Divide by NREAL when realizations are low to prevent very low var2
!         c12(iobs)=sum/(nreal)   ! Divide by NREAL when realizations are low to prevent very low var2
       end do

       if((ichoice.eq.1).or.(ichoice.eq.0))then

! -- Divide RVAL by sqrt(NREAL-1)

         dtemp=nreal-1
         dtemp=sqrt(1.0d0/dtemp)
         rval=rval*dtemp          ! matrix

! -- Now we compute ZtZ.

         allocate(ztz(nreal,nreal),stat=ierr)
         if(ierr.ne.0) go to 9200
         write(6,6292)
6292     format(' - computing ZtZ matrix...')
         do icol=1,nreal
           do irow=1,icol
             sum=0.0d0
             do iobs=1,nobswtnonzero
               sum=sum+rval(iobs,irow)*rval(iobs,icol)
             end do
             ztz(icol,irow)=sum
             if(icol.ne.irow) ztz(irow,icol)=sum
           end do
         end do

! -- We now do SVD on ZtZ.

         lwork=max(nreal*8,nobs)
         allocate(work(lwork),usneg1(nobswtnonzero,nreal),s(nreal),stat=ierr)
         if(ierr.ne.0) go to 9200
         write(6,6293)
6293     format(' - performing singular value decomposition of ZtZ matrix...')
         jobu='O'
         jobvt='N'
         ldvt=1
         ldu=1
         call dgesvd(jobu,jobvt,nreal,nreal,ztz,nreal,s,u,    &
         ldu,vt,ldvt,work,lwork,info)
         if(info.ne.0)then
           write(amessage,650)
650        format('Failure in singular value decomposition of ZtZ matrix.')
           go to 9890
         end if

! -- Now we formulate US(-1) matrix.

         write(6,651)
651      format(' - formulation of US(-1) matrix...')
         do ireal=1,nreal
           do iobs=1,nobswtnonzero
             sum=0.0d0
             do jreal=1,nreal
               sum=sum+rval(iobs,jreal)*ztz(jreal,ireal)
             end do
             usneg1(iobs,ireal)=sum
           end do
         end do
         s1=s(1)
         thresh=1.0d-7                     ! arbitrary
         thresh=s1*thresh
         do ireal=1,nreal
           if(s(ireal).gt.thresh)then
             dtemp=1.0d0/s(ireal)
             usneg1(:,ireal)= usneg1(:,ireal)*dtemp
           else
             usneg1(:,ireal)=0.0d0
           end if
         end do

! -- We now store the US(-1) matrix

         if(ichoice.eq.1)then
           write(6,680)
680        format(' - saving root of inverted data covariance matrix...')
           itemp1=0
           if(ans.eq.'y') itemp1=1
           write(30) itemp1
           write(30) nobs,nobswtnonzero
           write(30) (aobs(iobs),iobs=1,nobs)
           write(30) (scale(iobs),iobs=1,nobs)
           write(30) (offset(iobs),iobs=1,nobs)
           write(30) (otrans(iobs),iobs=1,nobs)
           do ireal=1,nreal
             write(30) (usneg1(iobs,ireal),iobs=1,nobswtnonzero)
           end do
           close(unit=30)
           call addquote(storefile,afile)
           write(6,6901) trim(afile)
6901       format(' - file ',a,' written ok.')
         end if
       else if(ichoice.eq.2)then
         lwork=nobs
         allocate(work(lwork),usneg1(nobswtnonzero,nreal),stat=ierr)
         if(ierr.ne.0) go to 9200
         afile=storefile
         write(6,6271)
6271     format(' - reading root of inverted data covariance matrix...')
         read(30,err=9250,end=9300) itemp1
         if((itemp1.eq.0).and.(ans.eq.'y')) go to 9370
         if((itemp1.eq.1).and.(ans.eq.'n')) go to 9370
         read(30,err=9250,end=9300) itemp1,itemp2
         if((itemp1.ne.nobs).or.(itemp2.ne.nobswtnonzero)) go to 9350
         read(30,err=9250,end=9300) (atemp20,iobs=1,nobs)
         if(atemp20.ne.aobs(nobs)) go to 9350                     ! admittedly, not a good check.
         read(30,err=9250,end=9300) (work(iobs),iobs=1,nobs)
         do iobs=1,nobs
           if(.not.dequals(work(iobs),scale(iobs))) go to 9350
         end do
         read(30,err=9250,end=9300) (work(iobs),iobs=1,nobs)
         do iobs=1,nobs
           if(.not.dequals(work(iobs),offset(iobs))) go to 9350
         end do
         read(30,err=9250,end=9300) (iwork1(iobs),iobs=1,nobs)
         do iobs=1,nobs
           if(iwork1(iobs).ne.otrans(iobs)) go to 9350
         end do
         do ireal=1,nreal
           read(30,err=9250,end=9300) (usneg1(iobs,ireal),iobs=1,nobswtnonzero)
         end do
         close(unit=30)
         write(6,6272) trim(afile)
6272     format(' - file ',a,' read ok.')
       end if
       if(allocated(rval)) deallocate(rval,stat=ierr)

! -- Some information is recorded in output file

6918   continue
       write(20,6919)
6919   format(' PROBLEM DETAILS ---->')
       write(20,*)
       call addquote(infile,afile)
       write(20,692) trim(afile)
692    format(' Name of PEST control file                        : ',a)
       if(transfile.eq.' ') then
         afile='none'
       else
         call addquote(transfile,afile)
       end if
       write(20,693) trim(afile)
693    format(' Name of observation transform file               : ',a)
       call addquote(rrffile,afile)
       write(20,694) trim(afile)
694    format(' Name of observation RRF file                     : ',a)
       call addquote(prrffile,afile)
       write(20,695) trim(afile)
695    format(' Name of predictive RRF file                      : ',a)
       call writint(anum,nreal)
       write(20,6951) trim(anum)
6951   format(' Number of realizations read from these files     : ',a)
       write(20,696) trim(apred)
696    format(' Name of prediction                               : ',a)
       call writreal(anum,minpval)
       write(20,6961) trim(anum)
6961   format(' Minimum sampled prediction value                 : ',a)
       call writreal(anum,maxpval)
       write(20,6962) trim(anum)
6962   format(' Maximum sampled prediction value                 : ',a)
       write(20,697)
697    format(' Prediction transformations                       :')
       call writreal(anum,pscale)
       write(20,6971) trim(anum)
6971   format('     Scale                = ',a)
       call writreal(anum,poffset)
       write(20,6972) trim(anum)
6972   format('     Offset               = ',a)
       if(ptrans.eq.1)then
         anum='yes'
       else
         anum='no'
       end if
       write(20,6973) trim(anum)
6973   format('     Log-transform        = ',a)
       if(ptrans.eq.1)then
         call writreal(anum,pbaseval)
       else
         anum='na'
       end if
       write(20,6974) trim(anum)
6974   format('     Emergency base value = ',a)
       if(ans.eq.'y')then
         anum='yes'
       else
         anum='no'
       end if
       write(20,6969) trim(anum)
6969   format(' Work in normal score space                       : ',a)
       if((ichoice.eq.1).or.(ichoice.eq.0))then
         anum='calculate'
       else if (ichoice.eq.2)then
         anum='read'
       end if
       write(20,6976) trim(anum)
6976   format(' Calculate or read inverted data covariance matrix: ',a)
       if(ichoice.eq.1)then
         call addquote(storefile,afile)
         write(20,69761) trim(afile)
69761    format(' Name of file in which to store matrix            : ',a)
       else if(ichoice.eq.2)then
         call addquote(storefile,afile)
         write(20,69762) trim(afile)
69762    format(' Name of file from which to read matrix           : ',a)
       end if

       write(20,*)
       write(20,*)
       write(20,6975)
6975   format(' OUTCOMES OF ANALYSIS ---->')
       write(20,*)

       write(6,700)
700    format(' - computing posterior stats...')
       do iobs=1,nobswtnonzero
         oval(iobs)=oval(iobs)-mu2(iobs)   ! oval is now x2-mu2
       end do

! -- Now we calculate C22(-1)C12. This is done in two parts.

       allocate(work1(nreal),stat=ierr)
       if(ierr.ne.0) go to 9200
       do ireal=1,nreal
         sum=0.0d0
         do jobs=1,nobswtnonzero
           sum=sum+usneg1(jobs,ireal)*c12(jobs)
         end do
         work1(ireal)=sum
       end do

! -- Now the second part.

       do iobs=1,nobswtnonzero
         sum=0.0d0
         do ireal=1,nreal
           sum=sum+usneg1(iobs,ireal)*work1(ireal)
         end do
         work(iobs)=sum
       end do

! -- Now the posterior mean

       mu1_prior=mu1          ! Keep it for later
       sum=0.0d0
       do iobs=1,nobswtnonzero
         sum=sum+oval(iobs)*work(iobs)
       end do
       mu1=mu1+sum

! -- Finally the posterior variance.

       sum=0.0d0
       do iobs=1,nobswtnonzero
         sum=sum+c12(iobs)*work(iobs)
       end do
       var2=var1-sum
       if(var2.ge.0.0d0)then
         var2=sqrt(var2)
       else
         var2=0.0d0
       end if

! -- Calculate and record confidence limits in normal score transformed space.

       mid_prior=mu1_prior
       mid_post=mu1
       std_dev_prior=sqrt(var1)
       std_dev_post=var2
       up68_prior=mid_prior+std_dev_prior
       up95_prior=mid_prior+2.0*std_dev_prior
       low68_prior=mid_prior-std_dev_prior
       low95_prior=mid_prior-2.0*std_dev_prior
       up68_post=mid_post+std_dev_post
       up95_post=mid_post+2.0*std_dev_post
       low68_post=mid_post-std_dev_post
       low95_post=mid_post-2.0*std_dev_post
       if(ans.eq.'y')then
         write(20,*)
         write(20,789)
789      format(' Predictive confidence limits in normal score transform space:-')
         write(20,*)
         write(cline,800) 'distribution','low_95','low_68','mean','high_68','high_95'
800      format(1x,a,t20,a,t40,a,t60,a,t80,a,t100,a)
         write(20,'(a)') trim(cline)
         do i=1,len_trim(cline)
           if(cline(i:i).ne.' ')cline(i:i)='-'
         end do
         write(20,'(a)') trim(cline)
         call writreal(atemp1,low95_prior)
         call writreal(atemp2,low68_prior)
         call writreal(atemp3,mid_prior)
         call writreal(atemp4,up68_prior)
         call writreal(atemp5,up95_prior)
         write(20,810) 'prior',trim(atemp1),trim(atemp2),trim(atemp3),trim(atemp4),trim(atemp5)
810      format(1x,a,t20,a,t40,a,t60,a,t80,a,t100,a)
         call writreal(atemp1,low95_post)
         call writreal(atemp2,low68_post)
         call writreal(atemp3,mid_post)
         call writreal(atemp4,up68_post)
         call writreal(atemp5,up95_post)
         write(20,810) 'posterior',trim(atemp1),trim(atemp2),trim(atemp3),trim(atemp4),trim(atemp5)

! -- Back-transformation from normal score transform domain is now undertaken.

         call nst_back_transform(nreal,nstval,orig_pval_sorted,mid_prior,mid_prior_lim)
         call nst_back_transform(nreal,nstval,orig_pval_sorted,up68_prior,up68_prior_lim)
         call nst_back_transform(nreal,nstval,orig_pval_sorted,up95_prior,up95_prior_lim)
         call nst_back_transform(nreal,nstval,orig_pval_sorted,low68_prior,low68_prior_lim)
         call nst_back_transform(nreal,nstval,orig_pval_sorted,low95_prior,low95_prior_lim)
         call nst_back_transform(nreal,nstval,orig_pval_sorted,mid_post,mid_post_lim)
         call nst_back_transform(nreal,nstval,orig_pval_sorted,up68_post,up68_post_lim)
         call nst_back_transform(nreal,nstval,orig_pval_sorted,up95_post,up95_post_lim)
         call nst_back_transform(nreal,nstval,orig_pval_sorted,low68_post,low68_post_lim)
         call nst_back_transform(nreal,nstval,orig_pval_sorted,low95_post,low95_post_lim)
       end if

! -- And again from scale/offset/log domain

       if(ptrans.eq.1)then
         mid_prior=10**mid_prior
         up68_prior=10**up68_prior
         up95_prior=10**up95_prior
         low68_prior=10**low68_prior
         low95_prior=10**low95_prior
         mid_post=10**mid_post
         up68_post=10**up68_post
         up95_post=10**up95_post
         low68_post=10**low68_post
         low95_post=10**low95_post
       end if
       mid_prior=(mid_prior-poffset)/pscale
       up68_prior=(up68_prior-poffset)/pscale
       up95_prior=(up95_prior-poffset)/pscale
       low68_prior=(low68_prior-poffset)/pscale
       low95_prior=(low95_prior-poffset)/pscale
       mid_post=(mid_post-poffset)/pscale
       up68_post=(up68_post-poffset)/pscale
       up95_post=(up95_post-poffset)/pscale
       low68_post=(low68_post-poffset)/pscale
       low95_post=(low95_post-poffset)/pscale

! -- Now we record the values.

       write(6,7891)
7891   format(' - recording output file...')
       write(20,*)
       write(20,*)
       if(ans.eq.'y')then
         write(20,790)
790      format(' Predictive confidence limits in untransformed space:-')
       else
         write(20,791)
791      format(' Predictive confidence limits:-.')
       end if
       write(20,*)
       write(cline,800) 'distribution','low_95','low_68','mean','high_68','high_95'
       write(20,'(a)') trim(cline)
       do i=1,len_trim(cline)
         if(cline(i:i).ne.' ')cline(i:i)='-'
       end do
       write(20,'(a)') trim(cline)
       call wwritreal(atemp1,low95_prior,low95_prior_lim)
       call wwritreal(atemp2,low68_prior,low68_prior_lim)
       call wwritreal(atemp3,mid_prior,mid_prior_lim)
       call wwritreal(atemp4,up68_prior,up68_prior_lim)
       call wwritreal(atemp5,up95_prior,up95_prior_lim)
       write(20,810) 'prior',trim(atemp1),trim(atemp2),trim(atemp3),trim(atemp4),trim(atemp5)
       call wwritreal(atemp1,low95_post,low95_post_lim)
       call wwritreal(atemp2,low68_post,low68_post_lim)
       call wwritreal(atemp3,mid_post,mid_post_lim)
       call wwritreal(atemp4,up68_post,up68_post_lim)
       call wwritreal(atemp5,up95_post,up95_post_lim)
       write(20,810) 'posterior',trim(atemp1),trim(atemp2),trim(atemp3),trim(atemp4),trim(atemp5)

! -- We now record observation ranks.

       if(ans.eq.'y')then
         write(20,*)
         write(20,*)
         call writint(anum,nreal)
         write(20,1200) trim(anum)
1200     format(' Realization-ranking of non-zero-weighted observations (1 to ',a,'):-')
         write(20,*)
         write(cline,1210)
1210     format(' Observation',t28,'Value',t45,'Realization_rank')
         write(20,'(a)') trim(cline)
         do i=1,len_trim(cline)
           if(cline(i:i).ne.' ')cline(i:i)='-'
         end do
         write(20,'(a)') trim(cline)
         do iobs=1,nobswtnonzero
           i=comp2obs(iobs)
           write(20,1220) trim(aobs(i)),orig_oval(i),oval_rank(iobs)
1220       format(' ',a,t25,1pg14.7,t45,1pg14.7)
         end do
         icount1=0
         icount2=0
         do iobs=1,nobswtnonzero
           rtemp=oval_rank(iobs)
           if(rtemp.lt.1.0) icount1=icount1+1
           if(rtemp.gt.nreal) icount2=icount2+1
         end do
         write(20,*)
         call writint(anum,icount1)
         write(20,1230) trim(anum)
1230     format(' Number of observations with realization rank less than 1.0',t72,'= ',a)
         call writint(anum,icount2)
         call writint(anum1,nreal)
         write(20,1240) trim(anum1),trim(anum)
1240     format(' Number of observations with realization rank greater than ',a,t72,'= ',a)
       end if

       close(unit=20)
       write(6,730) trim(outfile)
730    format(' - file ',a,' written ok.')

       go to 9900

9000   call writint(aline,iline)
       write(amessage,9010) trim(aline),trim(afile)
9010   format('Error reading line ',a,' of file ',a,'.')
       go to 9890

9050   write(amessage,9060) trim(afile)
9060   format('Cannot find "* observation data" section in file ',a,'.')
       go to 9890

9070   write(amessage,9080) trim(afile)
9080   format('Cannot find "* observation names" section in file ',a,'.')
       go to 9890

9100   call writint(aline,iline)
       write(amessage,9110) trim(aline),trim(afile)
9110   format('Insufficient items on line ',a,' of file ',a,'.')
       go to 9890

9150   write(amessage,9160) trim(afile)
9160   format('Premature end encountered to file ',a,'.')
       go to 9890

9200   write(amessage,9210)
9210   format('Cannot allocate sufficient memory to continue execution.')
       go to 9890

9250   write(amessage,9260) trim(afile)
9260   format('Error encountered while reading binary file ',a,'.')
       go to 9890

9300   write(amessage,9310) trim(afile)
9310   format('Premature end encountered to binary file ',a,'.')
       go to 9890

9350   write(amessage,9360) trim(afile)
9360   format('Observations and/or transformations used in formulating the matrix ',   &
       'stored in file ',a,' are not the same as those used in current problem.')
       go to 9890

9370   write(amessage,9380) trim(afile)
9380   format('The normal score transformation status of data in file ',a,' does not ',  &
       'accord with that of the present problem.')
       go to 9890

9890   continue
       amessage=' '//trim(amessage)
       call writmess(6,amessage)

9900   continue
       deallocate(seed, stat=ierr)
       deallocate(otrans,obs2comp,comp2obs,stat=ierr)
       deallocate(iwork1,stat=ierr)
       deallocate(rval,pval,baseval,stat=ierr)
       deallocate(mu2,oval,weight,offset,scale,s,c12,stat=ierr)
       deallocate(ztz,work1,work,usneg1,stat=ierr)
       deallocate(aobs,stat=ierr)
       deallocate(nstval,wwork1,wwork2,orig_pval_sorted,stat=ierr)
       deallocate(oval_rank,orig_oval,stat=ierr)

       end



       subroutine nst_back_transform(nreal,nstval,val,value,ilim)

! -- This subroutine does back-transformation from normal score transform space.
!    NSTVAL contains values of the normal score, ordered.
!    VAL    contains value of the original array, ordered.
!    VALUE  is supplied as the value to be back-transformed; it is returned as the
!           back-transformed value.

       implicit none

       integer, intent(in)              :: nreal
       real, intent(in)                 :: nstval(nreal)
       real, intent(in)                 :: val(nreal)      ! Must be ordered
       double precision, intent(inout)  :: value           ! Note
       integer, intent(out)             :: ilim

       integer              :: ireal
       real                 :: nstdiff,diff,nstdist

       ilim=0
       if(value.lt.nstval(1))then
         ilim=-1
         value=val(1)
       else if(value.gt.nstval(nreal))then
         ilim=1
         value=val(nreal)
       else
         do ireal=2,nreal
           if(value.le.nstval(ireal))then
             nstdiff=nstval(ireal)-nstval(ireal-1)
             diff=val(ireal)-val(ireal-1)
             if((nstdiff.le.0.0).or.(diff.le.0.0))then
               value=val(ireal-1)
             else
               nstdist=value-nstval(ireal-1)
               value=val(ireal-1)+(nstdist/nstdiff)*diff
             end if
             exit
           end if
         end do
       end if

       return
       end



       subroutine nst_transform(nreal,nstval,val,value,rank)

! -- This subroutine does transformation to normal score transform space.
!    NSTVAL contains values of the normal score, ordered.
!    VAL    contains value of the original array, ordered.
!    VALUE  is supplied as the value to be transformed; it is returned as the
!           transformed value.

       implicit none

       integer, intent(in)              :: nreal
       real, intent(in)                 :: nstval(nreal)
       real, intent(in)                 :: val(nreal)      ! Must be ordered
       double precision, intent(inout)  :: value           ! Note
       real, intent(out)                :: rank

       integer              :: ireal
       real                 :: nstdiff,diff,dist

       if(value.lt.val(1))then
         rank=0
         value=nstval(1)
       else if(value.gt.val(nreal))then
         rank=nreal+1
         value=nstval(nreal)
       else
         do ireal=2,nreal
           if(value.le.val(ireal))then
             nstdiff=nstval(ireal)-nstval(ireal-1)
             diff=val(ireal)-val(ireal-1)
             if((nstdiff.le.0.0).or.(diff.le.0.0))then
               value=nstval(ireal-1)
               rank=ireal
             else
               dist=value-val(ireal-1)
               rank=dist/diff
               value=nstval(ireal-1)+rank*nstdiff
               rank=rank+ireal-1
             end if
             exit
           end if
         end do
       end if

       return
       end



       subroutine randrealgen(nreal,rval,work1,work2,tol,nsamp)

! -- Subroutine RANDREALGEN generates sequences of arrays of normally distributed numbers.
!    Each such array has NREAL elements.
!    It calculates the average value of all of the NREAL ranks (i.e. the average value of
!    each of the NREAL elements after sorting). It does this until convergence is achieved
!    or until a user-specified number of samples has been taken.
!    It then reports the number of samples required.

       implicit none

       integer, intent(in)              :: nreal
       real, intent(in)                 :: tol
       integer, intent(inout)           :: nsamp
       real, intent(out)                :: rval(nreal)
       real, intent(out)                :: work1(nreal)
       real, intent(out)                :: work2(nreal)

       integer                          :: i,numsort,nsamp_in
       real                             :: dtemp

       rval=0.0d0                        ! an array
       nsamp_in=nsamp
       nsamp=0
       numsort=(nreal+1)/2
       do
         nsamp=nsamp+1
         call random_normal_vector(nreal,work1,work2)
         call mysort(nreal,work1,numsort)
         if(nsamp.gt.1)then
           work2(1:numsort)=rval(1:numsort)/(nsamp-1)
           rval(1:numsort)=rval(1:numsort)+work1(1:numsort)
           dtemp=maxval(abs(rval(1:numsort)/nsamp-work2(1:numsort)))
           if(dtemp.le.tol) exit
         end if
         if(nsamp.eq.nsamp_in) exit
       end do
       rval(1:numsort)=rval(1:numsort)/nsamp                   ! an array
       do i=numsort+1,nreal
         rval(i)=-rval(nreal+1-i)
       end do

       return
       end


       subroutine mysort(nsamp,work,numsort)

       implicit none
       integer, intent(in)  :: nsamp
       real, intent(inout)  :: work(nsamp)
       integer, intent(in)  :: numsort

       integer          :: isamp
       integer          :: imin(1),ii
       real             :: dtemp

       do isamp=1,min(nsamp-1,numsort)
         dtemp=work(isamp)
         imin=minloc(work(isamp+1:nsamp))
         ii=imin(1)+isamp
         if(work(ii).lt.dtemp)then
           work(isamp)=work(ii)
           work(ii)=dtemp
         end if
       end do

       return
       end


       subroutine mysort_index(nsamp,work,iwork)

! -- This subroutine sorts a set of numbers in place.
!    On return WORK contains the sorted vector. IWORK contains indices of the
!    original vector after sorting.

       implicit none
       integer, intent(in)  :: nsamp
       real, intent(inout)  :: work(nsamp)
       integer, intent(out) :: iwork(nsamp)

       integer          :: isamp,itemp
       integer          :: imin(1),ii
       real             :: dtemp

       do isamp=1,nsamp
         iwork(isamp)=isamp
       end do

       do isamp=1,nsamp-1
         dtemp=work(isamp)
         itemp=iwork(isamp)
         imin=minloc(work(isamp+1:nsamp))
         ii=imin(1)+isamp
         if(work(ii).lt.dtemp)then
           work(isamp)=work(ii)
           work(ii)=dtemp
           iwork(isamp)=iwork(ii)
           iwork(ii)=itemp
         end if
       end do

       return
       end



       double precision function random_normal()

       implicit none

       double precision :: pi
       double precision :: d1,d2

       pi=3.14159265358979323846264338
       call random_number(d1)
       call random_number(d2)
       random_normal = sqrt ( - 2.0D+00 * log ( d1 ) ) * cos ( 2.0D+00 * pi * d2 )

       return
       end




       subroutine random_normal_vector(ndim,rval,work)

       implicit none

       integer, intent(in) :: ndim
       real, intent(out)   :: rval(ndim)
       real, intent(out)   :: work(ndim)

       real :: pi

       pi=3.14159265358979323846264338
       call random_number(rval)
       call random_number(work)
       rval = sqrt ( - 2.0D+00 * log ( rval ) ) * cos ( 2.0D+00 * pi * work )

       return
       end





       subroutine writreal(atemp,dtemp)

       implicit none

       character (len=*), intent(out) :: atemp
       double precision, intent(in) :: dtemp
       write(atemp,'(1pg14.7)') dtemp
       atemp=adjustl(atemp)
       return
       end



       subroutine wwritreal(atemp,dtemp,lim)

       implicit none

       character (len=*), intent(out) :: atemp
       double precision, intent(in)   :: dtemp
       integer, intent(in)            :: lim

       write(atemp,'(1pg14.7)') dtemp
       atemp=adjustl(atemp)
       if(lim.eq.1)then
         atemp='>'//trim(atemp)
       else if(lim.eq.-1)then
         atemp='<'//trim(atemp)
       end if
       return
       end




subroutine addquote(afile,aqfile)

! -- Subroutine ADDQUOTE adds quotes to a filename if it has a space in it.

! -- Arguments are as follows:-
!        afile:       the name of the file
!        aqfile:      the name of the file with quotes added

        character (len=*), intent(in)   :: afile
        character (len=*), intent(out)  :: aqfile
        integer nbb

        if(index(trim(afile),' ').eq.0)then
          aqfile=afile
        else
          aqfile(1:1)='"'
          aqfile(2:)=trim(afile)
          nbb=len_trim(aqfile)+1
          aqfile(nbb:nbb)='"'
        end if

        return
end subroutine addquote



SUBROUTINE LINESPLIT(IFAIL,NUM,LW,RW,CLINE)

! -- Subroutine LINESPLIT splits a string into blank-delimited fragments.

        INTEGER IFAIL,NW,NBLC,J,I
        INTEGER NUM,NBLNK
        INTEGER LW(NUM),RW(NUM)
        CHARACTER*(*) CLINE

        IFAIL=0
        NW=0
        NBLC=len_trim(CLINE)
        IF((NBLC.NE.0).AND.(INDEX(CLINE,CHAR(9)).NE.0)) THEN
          CALL TABREM(CLINE)
          NBLC=len_trim(CLINE)
        ENDIF
        IF(NBLC.EQ.0) THEN
          IFAIL=-1
          RETURN
        END IF
        J=0
5       IF(NW.EQ.NUM) RETURN
        DO 10 I=J+1,NBLC
          IF((CLINE(I:I).NE.' ').AND.(CLINE(I:I).NE.',').AND.   &
          (ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10      CONTINUE
        IFAIL=1
        RETURN
20      NW=NW+1
        LW(NW)=I
        DO 30 I=LW(NW)+1,NBLC
          IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',').OR.     &
          (ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30      CONTINUE
        RW(NW)=NBLC
        IF(NW.LT.NUM) IFAIL=1
        RETURN
40      RW(NW)=I-1
        J=RW(NW)
        GO TO 5

END SUBROUTINE LINESPLIT


SUBROUTINE TABREM(CLINE)

! -- Subroutine TABREM removes tabs from a string.

        INTEGER I
        CHARACTER*(*) CLINE

        DO 10 I=1,LEN(CLINE)
10      IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '

        RETURN
END SUBROUTINE TABREM


SUBROUTINE intREAD(IFAIL,CLINE,iTEMP)

! -- Subroutine intREAD reads a real number from a string.

        INTEGER IFAIL
        integer iTEMP
        CHARACTER*6 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(i   )'
        WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
        READ(CLINE,AFMT,ERR=100) iTEMP
        RETURN

100     IFAIL=1
        RETURN

END SUBROUTINE INTREAD


        SUBROUTINE DREALREAD(IFAIL,CLINE,RTEMP)

! -- Subroutine REALREAD reads a real number from a string.

        INTEGER IFAIL
        double precision RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') len_trim(CLINE)
        READ(CLINE,AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END


        SUBROUTINE REALREAD(IFAIL,CLINE,RTEMP)

! -- Subroutine REALREAD reads a real number from a string.

        INTEGER IFAIL
        REAL RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') len_trim(CLINE)
        READ(CLINE,AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END



        SUBROUTINE LOWCASE(ASTRNG)

! -- Subroutine LOWCASE converts a string to lower case.

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,len_trim(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN
        END


        subroutine writint(atemp,ival)

!       Subroutine WRITINT writes an integer to a character variable.

        integer*4 ival
        character*6 afmt
        character*(*) atemp

        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(atemp)
        write(atemp,afmt)ival
        atemp=adjustl(atemp)
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
             write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:i)
          end if
          j=i
          go to 5
        end if
        end do
        if(itake.eq.0)then
          write(junit,'(a)') amessage(j+1:jend)
          itake=2+leadblank
        else
          write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:jend)
        end if
        j=jend
        go to 5
100     jend=nblc
        if(itake.eq.0)then
          write(junit,'(a)') amessage(j+1:jend)
        else
          write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:jend)
        end if
        return

      end


      logical function dequals(r1,r2)

        implicit none
        double precision, intent(in)      :: r1
        double precision, intent(in)      :: r2
        double precision                  :: rtemp

        rtemp=abs(5.0*spacing(r1))
        if(abs(r1-r2).lt.rtemp)then
          dequals=.true.
        else
          dequals=.false.
        end if
        return

      end function dequals


      subroutine getfile(ifail,cline,filename,ibeg,iend)

! Subroutine getfile extracts a filename from a string.

! -- Arguments are as follows:-
!       ifail: returned as zero if filename successfully read
!       cline: a character string containing the file name
!       filename: the name of the file read from the string
!       ibeg: character position at which to begin search for filename
!       iend: on input  - character position at which to end search for filename
!             on output - character postion at which filename ends


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
!          do j=i+1,iend
          do j=i+1,len_trim(cline)              !note
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
            if((cline(j:j).eq.' ').or.(cline(j:j).eq.',').or.(cline(j:j).eq.char(9)))then
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

      end subroutine getfile

