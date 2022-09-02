       program scalepar

C -- Program SCALEPAR re-formulates a parameter estimation problem in
C    terms of scaled parameters.

       implicit none

       logical lopened,skipline,ltemp
       integer i,ierr,j,iline,npar,nobs,npargp,nprior,nobsgp,ntplfle,
     + ninsfle,numcom,jacfile,messfile,ipar,icount,jpar,iunit,junit,
     + itype,ifail,jpartype,vardim,covdim,ipestmode,nespar,ies,
     + nespar1,nxrow1,nxrow,irow,iobs,jes,kes
       integer modelflag
       integer nn,jjflag
       double precision rtemp,scale,offset,ddtemp
       double precision sd,dtemp
       character*1 aa
       character*5 atemp
       character*10 aprecis,apoint,atrans,atype,aline
       character*12 aversion
       character*12 apar1,apar2,agroup
       character*15 aparchglim,pestmode,nulstring,sline
       character*20 aobs1
       character*200 pestinfile,afile,bfile,uncertfile,pestoutfile,
     + pardatfile,partempfile,scalefile,iscalefile,valtempfile,
     + parvalfile,modcomline,batchfile,jcofile
       character*200 covfile,newfile
       character*300 cline
       character*500 amessage

       integer                   :: lw(10),rw(10)
       double precision          :: parvar(1)

       integer, allocatable          :: itrans(:)
       real, allocatable             :: pval(:)
       double precision, allocatable :: parcov(:,:),x(:,:)
       character*12, allocatable     :: bpar(:),apar(:)
       character*14, allocatable     :: astring(:)
       character*20, allocatable     :: aobs(:)
       character*200, allocatable    :: tempfile(:),modinfile(:),
     + insfile(:),outfile(:)

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' SCALEPAR Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

       batchfile='scalebatch.bat'
       modelflag=0

C -- Data is acquired from the user.

20     write(6,30,advance='no')
30     format(' Enter name of existing PEST control file: ')
       read(5,'(a)') pestinfile
       if(pestinfile.eq.' ') go to 20
       call remchar(pestinfile,'"')
       call remchar(pestinfile,'''')
       pestinfile=adjustl(pestinfile)
       i=len_trim(pestinfile)
       j=i-4
       if(j.lt.1)j=1
       atemp=pestinfile(j:i)
       if(index(atemp,'.').eq.0) then
         pestinfile=trim(pestinfile)//'.pst'
       end if
       i=len_trim(pestinfile)
       j=max(i-3,1)
       atemp=pestinfile(j:i)
       call lowcas(atemp)
       if(atemp(1:4).ne.'.pst')then
         write(6,35)
35       format(/,' PEST control file must have an extension of ',
     +   '".pst" - try again.',/)
         go to 20
       end if
#ifndef UNIX
       call lowcas(pestinfile)
#endif
       open(unit=10,file=pestinfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(pestinfile,afile)
         write(6,38) trim(afile)
38       format(/,' Cannot open file ',a,' - try again.',/)
         go to 20
       end if

40     write(6,50,advance='no')
50     format(' Enter name of parameter uncertainty file: ')
       read(5,'(a)') uncertfile
       if(uncertfile.eq.' ') go to 40
       call remchar(uncertfile,'"')
       call remchar(uncertfile,'''')
       uncertfile=adjustl(uncertfile)
#ifndef UNIX
       call lowcas(uncertfile)
#endif
       open(unit=11,file=uncertfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(uncertfile,afile)
         write(6,38) trim(afile)
         go to 40
       end if
       write(6,*)

60     write(6,70,advance='no')
70     format(' Enter name for new PEST control file: ')
       read(5,'(a)') pestoutfile
       if(pestoutfile.eq.' ') go to 60
       i=len_trim(pestoutfile)
       if(i.gt.2)then
         if(pestoutfile(i-1:i).eq.'~^')then
           modelflag=1
           pestoutfile(i-1:i)=' '
         end if
       end if
       call remchar(pestoutfile,'"')
       call remchar(pestoutfile,'''')
       pestoutfile=adjustl(pestoutfile)
       i=len_trim(pestoutfile)
       j=i-4
       if(j.lt.1)j=1
       atemp=pestoutfile(j:i)
       if(index(atemp,'.').eq.0) then
         pestoutfile=trim(pestoutfile)//'.pst'
       end if
       i=len_trim(pestoutfile)
       j=max(i-3,1)
       atemp=pestoutfile(j:i)
       call lowcas(atemp)
       if(atemp(1:4).ne.'.pst')then
         write(6,35)
         go to 60
       end if
#ifndef UNIX
       call lowcas(pestoutfile)
#endif
       open(unit=20,file=pestoutfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(pestoutfile,afile)
         write(6,75) trim(afile)
75       format(/,' Cannot open file ',a,' for output - try again.',/)
         go to 60
       end if

80     write(6,85,advance='no')
85     format(' Enter name for PAR2PAR input file (<Enter> ',
     + 'if p2p###.dat): ')
       read(5,'(a)') pardatfile
       if(pardatfile.eq.' ') then
         pardatfile='p2p###.dat'
       else
         call remchar(pardatfile,'"')
         call remchar(pardatfile,'''')
         pardatfile=adjustl(pardatfile)
#ifndef UNIX
         call lowcas(pardatfile)
#endif
       end if


90     write(6,95,advance='no')
95     format(' Enter name for template of this file (<Enter> if ',
     + 'p2p###.tpl): ')
       read(5,'(a)') partempfile
       if(partempfile.eq.' ') then
         partempfile='p2p###.tpl'
       else
         call remchar(partempfile,'"')
         call remchar(partempfile,'''')
         partempfile=adjustl(partempfile)
       end if
#ifndef UNIX
       call lowcas(partempfile)
#endif
       open(unit=21,file=partempfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(partempfile,afile)
         write(6,75) trim(afile)
         go to 90
       end if

96     write(6,97,advance='no')
97     format(' Enter name for parameter value file (<Enter> ',
     + 'if p###.par): ')
       read(5,'(a)') parvalfile
       if(parvalfile.eq.' ') then
         parvalfile='p###.par'
       else
         call remchar(parvalfile,'"')
         call remchar(parvalfile,'''')
         parvalfile=adjustl(parvalfile)
#ifndef UNIX
         call lowcas(parvalfile)
#endif
       end if


98     write(6,99,advance='no')
99     format(' Enter name for template of this file (<Enter> if ',
     + 'p###.tpl): ')
       read(5,'(a)') valtempfile
       if(valtempfile.eq.' ') then
         valtempfile='p###.tpl'
       else
         call remchar(valtempfile,'"')
         call remchar(valtempfile,'''')
         valtempfile=adjustl(valtempfile)
       end if
#ifndef UNIX
       call lowcas(valtempfile)
#endif
       open(unit=24,file=valtempfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(valtempfile,afile)
         write(6,75) trim(afile)
         go to 98
       end if

100    write(6,105,advance='no')
105    format(' Enter name for scaling matrix file (<Enter> if ',
     + 'scale.mat): ')
       read(5,'(a)') scalefile
       if(scalefile.eq.' ') then
         scalefile='scale.mat'
       else
         call remchar(scalefile,'"')
         call remchar(scalefile,'''')
         scalefile=adjustl(scalefile)
       end if
#ifndef UNIX
       call lowcas(scalefile)
#endif
       open(unit=22,file=scalefile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(scalefile,afile)
         write(6,75) trim(afile)
         go to 100
       end if

110    write(6,120,advance='no')
120    format(' Enter name for inverse scaling matrix file (<Enter> if ',
     + 'iscale.mat): ')
       read(5,'(a)') iscalefile
       if(iscalefile.eq.' ') then
         iscalefile='iscale.mat'
       else
         call remchar(iscalefile,'"')
         call remchar(iscalefile,'''')
         iscalefile=adjustl(iscalefile)
       end if
       open(unit=23,file=iscalefile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(iscalefile,afile)
         write(6,75) trim(afile)
         go to 110
       end if

121    write(6,122,advance='no')
122    format(' Run model in silent or verbose mode  [s/v] ',
     + '(<Enter for "s"): ')
       read(5,'(a)') aa
       if(aa.eq.' ') aa='s'
       if(aa.eq.'S')aa='s'
       if(aa.eq.'V')aa='v'
       if((aa.ne.'s').and.(aa.ne.'v')) go to 121

       write(6,*)

C -- The PEST control file is processed.

C -- Some control data is read.

       call addquote(pestinfile,afile)
       write(6,125) trim(afile)
125    format(' - reading PEST control file ',a,'...')
       iline=1
       read(10,'(a)',err=9000,end=9000) cline
       if(cline(1:3).ne.'pcf') go to 9000
126    iline=iline+1
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 126
       if((cline(1:1).ne.'*').or.(index(cline,'control').eq.0))
     + go to 9000
127    iline=iline+1
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 127
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       pestmode=cline(lw(2):rw(2))
       call lowcas(pestmode)
       if(pestmode(1:5).eq.'estim')then
         ipestmode=1
       else if(pestmode(1:7).eq.'predict')then
         ipestmode=2
       else if(pestmode(1:5).eq.'regul')then
         ipestmode=3
       else
         go to 9000
       end if
128    continue
       iline=iline+1
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 128
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
       if(nprior.ne.0)then
         write(amessage,140) trim(afile)
140      format(' PEST control file ',a,' must not contain prior ',
     +   'information if it is to be used with SCALEPAR.')
         go to 9800
       end if
       if(npar.eq.0)then
         call writint(aline,iline)
         write(amessage,141) trim(aline),trim(afile)
141      format(' Number of parameters NPAR provided as zero at line ',
     +   a,' of PEST control file ',a,'.')
         go to 9800
       end if
       if(nobs.eq.0)then
         call writint(aline,iline)
         write(amessage,145) trim(aline),trim(afile)
145      format(' Number of observations NOBS provided as zero at ',
     +   'line ',a,' of PEST control file ',a,'.')
         go to 9800
       end if
146    iline=iline+1
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 146
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(1):rw(1)),ntplfle)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(2):rw(2)),ninsfle)
       if(ifail.ne.0) go to 9000
       aprecis=cline(lw(3):rw(3))
       call lowcas(aprecis)
       if((aprecis.ne.'single').and.(aprecis.ne.'double')) go to 9000
       apoint=cline(lw(4):rw(4))
       call lowcas(apoint)
       if((apoint.ne.'point').and.(apoint.ne.'nopoint')) go to 9000
       numcom=1
       jacfile=0
       messfile=0
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.eq.0)then
         call intread(ifail,cline(lw(5):rw(5)),numcom)
         if(ifail.ne.0) go to 9000
         call linspl(ifail,6,lw,rw,cline)
         if(ifail.eq.0) then
           call intread(ifail,cline(lw(6):rw(6)),jacfile)
           if(ifail.ne.0) go to 9000
           call linspl(ifail,7,lw,rw,cline)
           if(ifail.eq.0)then
             call intread(ifail,cline(lw(7):rw(7)),messfile)
             if(ifail.ne.0) go to 9000
           end if
         end if
       end if
       if(numcom.ne.1)then
         write(amessage,142) trim(afile)
142      format(' NUMCOM must be one (or ommitted) in PEST ',
     +   'control file ',a,' if this file is to be used with ',
     +   'SCALEPAR.')
         go to 9800
       end if
       if(jacfile.ne.0)then
         write(amessage,143) trim(afile)
143      format(' JACFILE must be set to zero (or ommitted) in PEST ',
     +   'control file ',a,' if this file is to be used with ',
     +   'SCALEPAR.')
         go to 9800
       end if

C -- Some memory is allocated.

       allocate(bpar(npar),itrans(npar),pval(npar),aobs(nobs),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,150)
150      format(' Cannot allocate sufficient memory to continue ',
     +   'execution.')
         go to 9800
       end if
       allocate(tempfile(ntplfle),modinfile(ntplfle),insfile(ninsfle),
     + outfile(ninsfle),stat=ierr)
       if(ierr.ne.0) then
         write(amessage,150)
         go to 9800
       end if

C -- The "parameter data" section is found.

       do
151      iline=iline+1
         read(10,'(a)',err=9000,end=200) cline
         if(skipline(cline)) go to 151
         call lowcas(cline)
         if(cline(1:1).eq.'*') then
           if(index(cline,'parameter da').ne.0) go to 220
         end if
       end do
200    write(amessage,210) trim(afile)
210    format(' Cannot find "parameter data" section of file ',a,'.')
       go to 9800
220    do ipar=1,npar
221      continue
         iline=iline+1
         read(10,'(a)',err=9000,end=9000) cline
         if(skipline(cline)) go to 221
         cline=adjustl(cline)
         call lowcas(cline)
         nn=index(cline,'=')
         if(nn.eq.1) go to 9000
         if(nn.ne.0) go to 221
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         if(cline(lw(2):rw(2)).eq.'file_parameter') go to 221
         call linspl(ifail,9,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         bpar(ipar)=cline(lw(1):rw(1))
         atrans=cline(lw(2):rw(2))
         call drealrd(ifail,cline(lw(4):rw(4)),ddtemp)
         if(ifail.ne.0) go to 9000
         pval(ipar)=ddtemp
         agroup=cline(lw(7):rw(7))
         call drealrd(ifail,cline(lw(8):rw(8)),scale)
         if(ifail.ne.0) go to 9000
         call drealrd(ifail,cline(lw(9):rw(9)),offset)
         if(ifail.ne.0) go to 9000
         if(scale.ne.1.0)then
           write(amessage,230) trim(bpar(ipar)),trim(afile)
230        format(' Parameter "',a,'" has a non-unity SCALE in file ',
     +     a,'. SCALEPAR does not permit this.')
           go to 9800
         end if
         if(offset.ne.0.0)then
           write(amessage,240) trim(bpar(ipar)),trim(afile)
240        format(' Parameter "',a,'" has a non-zero OFFSET in file ',
     +     a,'. SCALEPAR does not permit this.')
           go to 9800
         end if
         if(atrans.eq.'log')then
           itrans(ipar)=1
         else if (atrans.eq.'none')then
           itrans(ipar)=0
         else if(atrans.eq.'fixed')then
           itrans(ipar)=-1000000
         else if(atrans.eq.'tied')then
           itrans(ipar)=-1
         else
           go to 9000
         end if
         if((itrans(ipar).eq.1).and.(pval(ipar).le.0.0))then
           call writint(aline,iline)
           write(amessage,246) trim(aline),trim(afile)
246        format(' Log-transformed parameter is zero or negative ',
     +     'at line ',a,' of PEST control file ',a,'.')
           go to 9800
         end if
       end do
       icount=0
       do ipar=1,npar
         if(itrans(ipar).eq.-1)then
           icount=icount+1
         end if
       end do
       if(icount.ne.0)then
         do i=1,icount
247        continue
           iline=iline+1
           read(10,'(a)',err=9000,end=9000) cline
           if(skipline(cline)) go to 247
           cline=adjustl(cline)
           call lowcas(cline)
           nn=index(cline,'=')
           if(nn.eq.1) go to 9000
           if(nn.ne.0) go to 247
           call linspl(ifail,2,lw,rw,cline)
           if(ifail.ne.0) go to 9000
           if(cline(lw(2):rw(2)).eq.'file_parameter') go to 247
           apar1=cline(lw(1):rw(1))
           apar2=cline(lw(2):rw(2))
           if(apar1.eq.apar2) go to 9000
           do ipar=1,npar
             if(apar1.eq.bpar(ipar)) go to 260
           end do
           call writint(aline,iline)
           write(amessage,255) trim(apar1),trim(aline),trim(afile)
255        format(' "',a,'" is not a tied parameter at line ',a,
     +     ' of PEST control file ',a,'.')
           go to 9800
260        do jpar=1,npar
             if(apar2.eq.bpar(jpar)) go to 280
           end do
           call writint(aline,iline)
           write(amessage,265) trim(apar2),trim(aline),trim(afile)
265        format(' "',a,'" is not a suitable parent parameter to a ',
     +     'tied parameter at line ',a,' of PEST control file ',a,'.')
           go to 9800
280        if(itrans(jpar).lt.0) then
             call writint(aline,iline)
             write(amessage,265) trim(apar2),trim(aline),trim(afile)
             go to 9800
           end if
           if(pval(jpar).eq.0.0)then
             call writint(aline,iline)
             write(amessage,266) trim(aline),trim(afile)
266          format(' Parent parameter to tied parameter has initial ',
     +       'value of zero at line ',a,' of PEST control file ',a,'.')
             go to 9800
           end if
           itrans(ipar)=-jpar
         end do
       end if

C -- We now find the "observation data" section.

       do
         iline=iline+1
         read(10,'(a)',err=9000,end=270) cline
         ltemp=skipline(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'observation data').ne.0) go to 275
         end if
       end do
270    write(amessage,271) trim(afile)
271    format(' Cannot find "observation data" section of PEST ',
     + 'control file ',a,'.')
       go to 9800

C -- Observation names are read.

275    continue
       do i=1,nobs
2751     iline=iline+1
         read(10,'(a)',err=9000,end=9000) cline
         if(skipline(cline)) go to 2751
         call linspl(ifail,1,lw,rw,cline)
         aobs(i)=cline(lw(1):rw(1))
         call lowcas(aobs(i))
       end do

C -- We now find the "model command line" section.

       do
         iline=iline+1
         read(10,'(a)',err=9000,end=300) cline
         ltemp=skipline(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'model command').ne.0) go to 320
         end if
       end do
300    write(amessage,310) trim(afile)
310    format(' Cannot find "model command line" section of PEST ',
     + 'control file ',a,'.')
       go to 9800
320    iline=iline+1
       read(10,'(a)',err=9000,end=9000) modcomline
       if(skipline(modcomline)) go to 320
       call remchar(modcomline,'"')
       call remchar(modcomline,'''')
       modcomline=adjustl(modcomline)
       if(modcomline.eq.' ') go to 9000
#ifndef UNIX
       call lowcas(modcomline)
#endif
       i=len_trim(modcomline)
       j=i-3
       if(j.lt.1)j=1
       atemp=modcomline(j:i)
       if(modelflag.eq.0)then
#ifndef UNIX
       if(atemp.ne.'.bat')then
         write(amessage,330) trim(afile)
330      format(' Model command line must be a batch file, and ',
     +   'terminate in ".bat" in PEST control file ',a,'.')
         go to 9800
       end if
#endif
       end if

C -- We now read the model template and input filenames.

331    iline=iline+1
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 331
       if(cline(1:10).ne.'* model in')then
         call writint(aline,iline)
         write(amessage,350) trim(aline),trim(afile)
350      format(' "* model input(/output)" section header expected at ',
     +   'line ',a,' of file ',a,'.')
         go to 9800
       end if
       jjflag=0
       if(index(cline,'output').eq.0) jjflag=1
       do i=1,ntplfle
3501     iline=iline+1
         read(10,'(a)',err=9000,end=9000) cline
         if(skipline(cline)) go to 3501
         call spacesub(cline)
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         tempfile(i)=cline(lw(1):rw(1))
         call remchar(tempfile(i),char(211))
         modinfile(i)=cline(lw(2):rw(2))
         call remchar(modinfile(i),char(211))
       end do
       if(jjflag.eq.1)then
3502     iline=iline+1
         read(10,'(a)',err=9000,end=9000) cline
         if(skipline(cline)) go to 3502
         call lowcas(cline)
         if(cline.ne.'* model output')then
           call writint(aline,iline)
           write(amessage,3503) trim(aline),trim(afile)
3503       format(' "* model output" section header expected at line ',
     +     a,' of file ',a,'.')
           go to 9800
         end if
       end if
       do i=1,ninsfle
3504     iline=iline+1
         read(10,'(a)',err=9000,end=9000) cline
         if(skipline(cline)) go to 3504
         call spacesub(cline)
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         insfile(i)=cline(lw(1):rw(1))
         call remchar(insfile(i),char(211))
         outfile(i)=cline(lw(2):rw(2))
         call remchar(outfile(i),char(211))
       end do

C -- The PEST control file is now closed. (It is closed rather than rewound in case
C    this file is cited in the uncertainty file.)

       close(unit=10)
       write(6,351) trim(afile)
351    format(' - PEST control file ',a,' read ok.')

C -- We form an array of adjustable parameters.

       nespar=0
       do ipar=1,npar
         if(itrans(ipar).ge.0) nespar=nespar+1
       end do
       if(nespar.eq.0)then
         call addquote(pestinfile,afile)
         write(amessage,370) trim(afile)
370      format(' No adjustable parameters were found in PEST ',
     +   'control file ',a,'.')
         go to 9800
       end if
       allocate(apar(nespar),parcov(nespar,nespar),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,150)
         go to 9800
       end if
       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           apar(ies)=bpar(ipar)
         end if
       end do

C -- The parameter uncertainty file is now read.

       call addquote(uncertfile,afile)
       write(6,400) trim(afile)
400    format(/,' - reading parameter uncertainty file ',a,'...')
       iunit=11
       junit=12
       itype=2
       vardim=1
       covdim=nespar
       atype='parameter'
       call read_uncert_data_file(ifail,iunit,junit,itype,jpartype,
     + nespar,vardim,covdim,parvar,parcov,atype,uncertfile,amessage,
     + cline,apar)
       if(ifail.ne.0) go to 9800
       write(6,410)
410    format(' - parameter uncertainty data read ok.')
       write(6,*)

C -- The PAR2PAR template file is now written.

       write(21,415)
415    format('ptf $')
       write(21,416)
416    format('* parameter data')
       jpar=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           jpar=jpar+1
           sd=parcov(jpar,jpar)
           if(sd.le.0.0)then
             call addquote(uncertfile,afile)
             write(amessage,420) trim(apar(jpar))
420          format(' Covariance of parameter "',a,'" not provided ',
     +       'as positive.')
             go to 9800
           end if
           sd=sqrt(sd)
           if(itrans(ipar).eq.0)then
             write(21,440) trim(apar(jpar)),trim(apar(jpar)),sd,
     +       pval(ipar)
440          format(1x,a,' = $',a12,'$ * ',1pg14.7,' + ',1pg14.7)
           else if(itrans(ipar).eq.1)then
             write(21,450) trim(apar(jpar)),trim(apar(jpar)),sd,
     +       log10(pval(ipar))
450          format(1x,a,' = 10**($',a12,'$ * ',1pg14.7,' + ',
     +       1pg14.7,')')
           end if
         end if
       end do
       do ipar=1,npar
         if((itrans(ipar).lt.0).and.(itrans(ipar).gt.-1000000))then
           write(21,460) trim(bpar(ipar)),trim(bpar(-itrans(ipar))),
     +     pval(ipar),pval(-itrans(ipar))
460        format(1x,a,' = ',a12,' * ',1pg14.7,' / ',1pg14.7)
         else if (itrans(ipar).eq.-1000000)then
           write(21,470) trim(bpar(ipar)),pval(ipar)
470        format(1x,a,' = ',1pg14.7)
         end if
       end do
       write(21,480)
480    format('* template and model input files')
       do i=1,ntplfle
         call addquote(tempfile(i),afile)
         call addquote(modinfile(i),bfile)
         write(21,490) trim(afile),trim(bfile)
490      format(1x,a,1x,a)
       end do
       call addquote(valtempfile,afile)
       call addquote(parvalfile,bfile)
       write(21,490) trim(afile),trim(bfile)
       write(21,500)
500    format('* control data')
       write(21,510) trim(aprecis),trim(apoint)
510    format(1x,a,2x,a)
       close(unit=21)
       call addquote(partempfile,afile)
       write(6,520) trim(afile)
520    format(' - file ',a,' written ok.')

C -- The template of the parameter value file is written.

       write(24,521)
521    format('ptf $')
       write(24,522) trim(aprecis),trim(apoint)
522    format(a,2x,a)
       do ipar=1,npar
         write(24,523) trim(bpar(ipar)),trim(bpar(ipar))
523      format(1x,a,t15,'$',a12,'$    1.0    0.0')
       end do
       close(unit=24)
       call addquote(valtempfile,afile)
       write(6,520) trim(afile)

C -- The scale matrix file is written.

       write(22,530) nespar,nespar,-1
530    format(3i5)
       do jpar=1,nespar
         sd=sqrt(parcov(jpar,jpar))
         write(22,535) 1.0d0/sd
535      format(1x,1pg14.7)
       end do
       write(22,540)
540    format('* row and column names')
       do jpar=1,nespar
         write(22,550) trim(apar(jpar))
550      format(1x,a)
       end do
       close(unit=22)
       call addquote(scalefile,afile)
       write(6,560) trim(afile)
560    format(' - file ',a,' written ok.')

C -- The inverse scale matrix file is written.

       write(23,530) nespar,nespar,-1
       do jpar=1,nespar
         sd=sqrt(parcov(jpar,jpar))
         write(23,535) sd
       end do
       write(23,540)
       do jpar=1,nespar
         write(23,550) trim(apar(jpar))
       end do
       close(unit=23)
       call addquote(iscalefile,afile)
       write(6,560) trim(afile)

C -- The new PEST control file is written. But first the old PEST file is re-opened.

       write(6,*)
       call addquote(pestinfile,afile)
       write(6,568) trim(afile)
568    format(' - re-reading PEST control file ',a,'...')
       open(unit=10,file=pestinfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(pestinfile,afile)
         write(amessage,569) trim(afile)
569      format(' Cannot re-open PEST control file file ',a,'.')
         go to 9800
       end if

       do i=1,3
5691     continue
         read(10,'(a)',err=9200,end=9200) cline
         if(skipline(cline)) go to 5691
         write(20,'(a)') trim(cline)
       end do
5692   continue
       read(10,'(a)',err=9200,end=9200) cline
       if(skipline(cline)) go to 5692
       write(20,570) nespar,nobs,1,nprior,nobsgp
570    format(5i7)
5701   continue
       read(10,'(a)',err=9200,end=9200) cline
       if(skipline(cline)) go to 5701
       write(20,575) 1,ninsfle,trim(aprecis),trim(apoint),numcom,
     + jacfile,messfile
575    format(i4,i4,2x,a,2x,a,i4,i4,i4)
5751   continue
       read(10,'(a)',err=9200,end=600) cline
       if(skipline(cline)) go to 5751
       write(20,'(a)') trim(cline)
5752   continue
       read(10,'(a)',err=9200,end=600) cline
       if(skipline(cline)) go to 5752
       call linspl(ifail,1,lw,rw,cline)
       if(ifail.ne.0) rw(1)=1
       write(20,576) trim(cline(rw(1)+1:))
576    format(' 0.1  ',a)
       do
5761     continue
         read(10,'(a)',err=9200,end=600) cline
         if(skipline(cline)) go to 5761
         write(20,'(a)') trim(cline)
         if(index(cline,'*').ne.0)then
           call lowcas(cline)
           if(cline(1:1).eq.'*')then
             call lowcas(cline)
             if(index(cline,'parameter gr').ne.0) go to 620
           end if
         end if
       end do
600    call addquote(pestinfile,afile)
       write(amessage,610) trim(afile)
610    format(' Cannot find "parameter groups" section of PEST ',
     + 'control file ',a,'.')
       go to 9800
620    continue
       write(20,630)
630    format(' pargroup absolute 0.01 0.0 switch 2.0 parabolic')
       write(20,640)
640    format('* parameter data')
       do ipar=1,nespar
         write(20,650) trim(apar(ipar))
650      format(1x,a,t15,'none relative 10.0 7.0 13.0 pargroup ',
     +   '1.0 -10.0  1')
       end do
       do
6501     continue
         read(10,'(a)',end=680) cline
         if(skipline(cline)) go to 6501
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'observation gr').ne.0) go to 700
         end if
       end do
680    call addquote(pestinfile,afile)
       write(amessage,690) trim(afile)
690    format(' Cannot find "observation groups" section of PEST ',
     + 'control file ',a,'.')
       go to 9800
700    write(20,710)
710    format('* observation groups')
       do
7101     continue
         read(10,'(a)',end=730) cline
         if(skipline(cline)) go to 7101
         write(20,'(a)') trim(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'* model com').ne.0) go to 750
         end if
       end do
730    continue
       call addquote(pestinfile,afile)
       write(amessage,735)
735    format(' Cannot find "model command line" section of PEST ',
     + 'control file ',a,'.')
       go to 9800
750    continue
       write(20,'(1x,a)') trim(batchfile)
       write(20,760)
760    format('* model input/output')
       call addquote(partempfile,afile)
       call addquote(pardatfile,bfile)
       write(20,770) trim(afile),trim(bfile)
770    format(1x,a,1x,a)
       do i=1,ninsfle
         call addquote(insfile(i),afile)
         call addquote(outfile(i),bfile)
         write(20,770) trim(afile),trim(bfile)
       end do
       if(ipestmode.eq.3)then
         do
7701       continue
           read(10,'(a)',err=9200,end=800) cline
           if(skipline(cline)) go to 7701
           call lowcas(cline)
           if(cline(1:7).eq.'* regul')then
             write(20,780)
780          format('* regularisation')
             do i=1,3
7801           continue
               read(10,'(a)',err=9300,end=9300) cline
               if(skipline(cline)) go to 7801
               write(20,'(a)') trim(cline)
             end do
             go to 850
           end if
         end do
800      call addquote(pestinfile,afile)
         write(amessage,806) trim(afile)
806      format(' Cannot find "regularisation" section of PEST ',
     +   'control file ',a,'.')
         go to 9800
       else if(ipestmode.eq.2)then
         do
8061       continue
           read(10,'(a)',err=9200,end=820) cline
           if(skipline(cline)) go to 8061
           call lowcas(cline)
           if(cline(1:9).eq.'* predict')then
             write(20,840)
840          format('* predictive analysis')
             do i=1,5
8401           continue
               read(10,'(a)',err=9400,end=9400) cline
               if(skipline(cline)) go to 8401
               write(20,'(a)') trim(cline)
             end do
             go to 850
           end if
         end do
820      call addquote(pestinfile,afile)
         write(amessage,845) trim(afile)
845      format(' Cannot find "predictive analysis" section of PEST ',
     +   'control file ',a,'.')
         go to 9800
       end if

850    continue
       close(unit=20)
       call addquote(pestoutfile,afile)
       write(6,779) trim(afile)
779    format(' - file ',a,' written ok.')
       write(6,*)

C -- The new model batch file is now written.

       if(modelflag.eq.0)then
       open(unit=10,file=modcomline,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(modcomline,afile)
         write(amessage,785) trim(afile)
785      format(' Cannot open model batch/script file ',a,'.')
         go to 9800
       end if
       open(unit=20,file=batchfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(batchfile,afile)
         write(amessage,790) trim(afile)
790      format(' Cannot open file ',a,' for output.')
         go to 9800
       end if
       if(aa.eq.'s')then
#ifndef UNIX
         write(20,791)
791      format('@echo off')
#endif
       end if
       nulstring=' '
       if(aa.eq.'s')then
#ifdef UNIX
         nulstring=' > /dev/null'
#else
         nulstring=' > nul'
#endif
       end if
       write(20,801)
801    format('REM The following lines of this model batch/script ',
     + 'file were written by SCALEPAR.')
       write(20,810)
810    format('REM')
       write(20,812)
812    format('REM Model input files written by PAR2PAR are deleted.')
       write(20,810)
       do i=1,ntplfle
         call addquote(modinfile(i),afile)
         write(20,811) trim(afile),trim(nulstring)
#ifdef UNIX
811      format('/bin/rm ',a,a)
#else
811      format('del ',a,a)
#endif
       end do
       write(20,810)
       write(20,813)
813    format('REM PAR2PAR is run.')
       write(20,810)
       call addquote(pardatfile,afile)
       write(20,821) trim(afile),trim(nulstring)
821    format('par2par ',a,a)
       write(20,810)
       write(20,830)
830    format('REM The remainder of the file is a copy of the ',
     + 'original model batch/script file.')
       write(20,810)
       do
         read(10,'(a)',end=835) cline
#ifndef UNIX
         sline=cline
         call lowcas(sline)
         if(sline.eq.'@echo off') cycle
#endif
         write(20,'(a)') trim(cline)
       end do
       close(unit=10)
       close(unit=20)
835    continue
       call addquote(modcomline,afile)
       write(6,846) trim(afile)
846    format(' - old model batch/script file ',a,' read ok.')
       call addquote(batchfile,bfile)
       write(6,855) trim(bfile)
855    format(' - new model batch/script file ',a,' written ok.')
       end if

C -- It is now established whether a new Jacobian matrix is required.

       write(6,*)
890    write(6,900,advance='no')
900    format(' Write a JCO file for this case [y/n] ',
     +'(<Enter> for "n"): ')
       read(5,'(a)') aa
       if(aa.eq.' ')aa='n'
       if(aa.eq.'N')aa='n'
       if(aa.eq.'Y')aa='y'
       if((aa.ne.'y').and.(aa.ne.'n')) go to 890
       if(aa.eq.'n') go to 1000

C -- We establish the name of the JCO file.

       i=len_trim(pestinfile)
       j=i-3
       if(j.lt.1)j=1
       jcofile=pestinfile(1:j)//'jco'
       call addquote(jcofile,afile)

C -- The Jacobian matrix file is read.

       write(6,915) trim(afile)
915    format(/,' - reading Jacobian matrix file ',a,'...')
       call open_unformatted_file(ifail,10,'read',jcofile,amessage)
       if(ifail.ne.0) go to 9800
       read(10,err=9350,end=9350)nespar1,nxrow1
       if(nespar1.ge.0)then
         write(amessage,920) trim(afile)
920      format(' Jacobian matrix file ',a,' uses old format; ',
     +   'use JCOTRANS utility to translate it to new format.')
         go to 9800
       end if
       nxrow=nobs
       nespar1=abs(nespar1)
       nxrow1=abs(nxrow1)
       if((nespar1.ne.nespar).or.(nxrow1.ne.nobs)) go to 9500
       allocate(x(nxrow,nespar),stat=ierr)
       if(ierr.ne.0) then
         write(amessage,150)
         go to 9800
       end if
       do i=1,nespar
         do j=1,nxrow
           x(j,i)=0.0d0
         end do
       end do
       read(10,err=9350,end=9350)icount
       do i=1,icount
         read(10,err=9350,end=9350) j,dtemp
         ies=(j-1)/nxrow+1
         irow=j-(ies-1)*nxrow
         x(irow,ies)=dtemp
       end do
       do ies=1,nespar
         read(10,err=9350,end=9350) apar1
         call lowcas(apar1)
         if(apar1.ne.apar(ies)) go to 9500
       end do
       do irow=1,nxrow
         read(10,err=9350,end=9350) aobs1
         call lowcas(aobs1)
         if(aobs1.ne.aobs(irow)) go to 9500
       end do
       close(unit=10)
       write(6,930) trim(afile)
930    format(' - Jacobian matrix file ',a,' read ok.')

C -- The Jacobian matrix file is now scaled.

       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           sd=parcov(ies,ies)
           sd=sqrt(sd)
           do iobs=1,nobs
             x(iobs,ies)=x(iobs,ies)*sd
           end do
         end if
       end do

C -- The new Jacobian matrix file is now written.

       i=len_trim(pestoutfile)
       j=i-3
       if(j.lt.1)j=1
       jcofile=pestoutfile(1:j)//'jco'
       call addquote(jcofile,afile)
       call open_unformatted_file(ifail,20,'write',jcofile,amessage)
       if(ifail.ne.0) go to 9800
       write(20,err=9600) -nespar,-nobs
       icount=0
       do i=1,nespar
          do j=1,nobs
            if(x(j,i).ne.0.0d0) icount=icount+1
          end do
       end do
       write(20,err=9600) icount
       do i=1,nespar
         do j=1,nobs
           if(x(j,i).ne.0.0d0)then
             icount=(i-1)*nobs+j
             write(20,err=9600)icount,x(j,i)
           end if
         end do
       end do
       do ies=1,nespar
         write(20,err=9600) apar(ies)
       end do
       do iobs=1,nobs
         write(20,err=9600) aobs(iobs)
       end do
       close(unit=20)
       call addquote(jcofile,afile)
       write(6,950) trim(afile)
950    format(' - file ',a,' written ok.')

1000   continue
       write(6,*)
1005   write(6,1010,advance='no')
1010   format(' Write scaled uncertainty file? [y/n]: '
     + ,'(<Enter> for "n"): ')
       read(5,'(a)') aa
       if((aa.eq.' ').or.(aa.eq.'n').or.(aa.eq.'N')) then
         go to 9900
       else if ((aa.eq.'Y').or.(aa.eq.'y')) then
         continue
       else
         go to 1005
       end if
1020   write(6,1030,advance='no')
1030   format(' Enter name for scaled uncertainty file: ',
     + '(<Enter> if p###.unc): ')
       read(5,'(a)') newfile
       if(newfile.eq.' ') then
         newfile='p###.unc'
       else
         call remchar(newfile,'"')
         call remchar(newfile,'''')
         newfile=adjustl(newfile)
#ifndef UNIX
         call lowcas(newfile)
#endif
       end if
       open(unit=30,file=newfile)
       if(jpartype.eq.1)then
         write(30,1040)
1040     format('START STANDARD_DEVIATION')
         do ies=1,nespar
           write(30,1050) trim(apar(ies))
1050       format(4x,a,t20,'1.000')
         end do
         write(30,1052)
1052     format('END STANDARD_DEVIATION')
         close(unit=30)
         call addquote(newfile,afile)
         write(6,520) trim(afile)
       else
         write(6,1060,advance='no')
1060     format(' Enter name for cov mat file cited therein ',
     +   '(<Enter> if p###.cov): ')
         read(5,'(a)') covfile
         if(covfile.eq.' ')then
           covfile='p###.cov'
         else
           call remchar(covfile,'"')
           call remchar(covfile,'''')
           covfile=adjustl(covfile)
         end if
#ifndef UNIX
         call lowcas(covfile)
#endif
         write(30,1070)
1070     format(' START COVARIANCE_MATRIX')
         call addquote(covfile,afile)
         write(30,1080) trim(afile)
1080     format(3x,'file ',a)
         write(30,1090)
1090     format(3x,'variance_multiplier 1')
         write(30,1100)
1100     format(' END COVARIANCE_MATRIX')
         close(unit=30)
         call addquote(newfile,afile)
         write(6,520) trim(afile)

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

         open(unit=30,file=covfile)
         write(30,*) nespar,nespar,1
         allocate(astring(nespar),stat=ierr)
         if(ierr.ne.0)then
           write(amessage,1120)
1120       format(' Cannot allocate sufficient memory to continue ',
     +     'execution.')
           go to 9800
         end if
         do ies=1,nespar
           do jes=1,nespar
             if(parcov(ies,jes).eq.0.0)then
               astring(jes)='0             '
             else
               write(astring(jes),'(1pg14.7)') parcov(ies,jes)
             end if
           end do
           write(30,1130) (trim(astring(jes)),jes=1,nespar)
1130       format(10(1x,a))
         end do
         write(30,1140)
1140     format('* row and column names')
         do ies=1,nespar
           write(30,1150) trim(apar(ies))
1150       format(1x,a)
         end do
         close(unit=30)
         call addquote(covfile,afile)
         write(6,520) trim(afile)
         deallocate(astring,stat=ierr)
       end if

       go to 9900

9000   call writint(aline,iline)
       write(amessage,9010) trim(aline),trim(afile)
9010   format(' Error reading line ',a,' of PEST control file ',a,
     + ': check this file with PESTCHEK.')
       go to 9800
9200   call addquote(pestinfile,afile)
       write(amessage,9210) trim(afile)
9210   format(' Error re-reading PEST control file ',a,'.')
       go to 9800
9300   call addquote(pestinfile,afile)
       write(amessage,9310) trim(afile)
9310   format(' Error reading data from "regularisation" section of ',
     + 'PEST control file ',a,'.')
       go to 9800
9350   call addquote(jcofile,afile)
       write(amessage,9360) trim(afile)
9360   format(' Error reading Jacobian matrix from file ',a,'.')
       go to 9800
9400   call addquote(pestinfile,afile)
       write(amessage,9410) trim(afile)
9410   format(' Error reading data from "predictive analysis" section ',
     + 'of PEST control file ',a,'.')
       go to 9800
9500   call addquote(jcofile,afile)
       call addquote(pestinfile,bfile)
       write(amessage,9510) trim(afile),trim(bfile)
9510   format(' The Jacobian matrix contained in file ',a,
     + ' is not compatible with that of the inversion problem ',
     + 'supplied in PEST control file ',a,'.')
       go to 9800
9600   call addquote(jcofile,afile)
       write(amessage,9610) trim(afile)
9610   format(' Error writing to Jacobian matrix file ',a,'.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9900   continue

       deallocate(itrans,pval,parcov,bpar,apar,tempfile,modinfile,
     + insfile,outfile,aobs,stat=ierr)
       deallocate(x,stat=ierr)

       inquire(unit=10,opened=lopened)
       if(lopened) close(unit=10,iostat=ierr)
       inquire(unit=11,opened=lopened)
       if(lopened) close(unit=11,iostat=ierr)

       inquire(unit=20,opened=lopened)
       if(lopened) close(unit=20,iostat=ierr)
       inquire(unit=21,opened=lopened)
       if(lopened) close(unit=21,iostat=ierr)
       inquire(unit=22,opened=lopened)
       if(lopened) close(unit=22,iostat=ierr)
       inquire(unit=23,opened=lopened)
       if(lopened) close(unit=23,iostat=ierr)
       inquire(unit=24,opened=lopened)
       if(lopened) close(unit=24,iostat=ierr)

9999   continue

       end


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


