       program pwtadj2

C -- Program PWTADJ2 adjusts weights in a PEST control file so that contributions made
C    to the objective function at the end of the first iteration by each observation group
C    is equal to the (corrected) number of observations in the group.


       implicit none

       integer, parameter :: MAXIFCOUNT=100
       logical    :: lopened,skipline,ltemp
       integer    :: ifail,i,j,ierr,iline,ipestmode,npar,nobs,npargp,
     +               nprior,nobsgp,iobsgp,icount,n,jobsgp,iprior,ne,
     +               iobs,nb,itt,ipread,jline
       integer    :: paramcorrect,inztot
       integer    :: ifcount
       integer    :: lw(5),rw(5)
       real            :: oval,owght,totphi,rtemp,correction,totphigp,
     +                    totfactor
       character*2     :: ang
       character*5     :: atemp
       character*12    :: aaobsgp,aline
       character*12    :: aversion
       character*20    :: pestmode,atemp1,anum,aobs,aweight
       character*200   :: casename,pestinfile,pestoutfile,afile,
     +                    recinfile,bfile
       character*300   :: cline
       character*700   :: amessage
       character*1000  :: dline,eline,fline

       integer, allocatable      :: inzwt(:)
       real, allocatable         :: phigp(:),factor(:)
       integer, allocatable      :: covind(:)
       character*12, allocatable :: aobsgp(:)
       character*300                  :: ffline(MAXIFCOUNT)


#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' PWTADJ2 version ',a,'. Watermark Numerical ',
     + 'Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

C -- The command line is parsed.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       casename=cline(lw(1):rw(1))
       call remchar(casename,char(211))
       pestoutfile=cline(lw(2):rw(2))
       call remchar(pestoutfile,char(211))
       ang=cline(lw(3):rw(3))
       call lowcas(ang)
       if((ang.ne.'g').and.(ang.ne.'ng')) go to 8900
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0)then
         paramcorrect=0
       else
         atemp1=cline(lw(4):rw(4))
         call intread(ifail,atemp1,paramcorrect)
         if(ifail.ne.0) go to 8900
         if(paramcorrect.lt.0) then
           write(amessage,2)
2          format(' Parameter correction term (fourth item in ',
     +     'PWTADJ2 command line) must not be negative.')
           go to 9800
         end if
       end if

#ifndef UNIX
       call lowcas(casename)
       call lowcas(pestoutfile)
#endif


C -- The command line arguments are further processed.

       i=len_trim(casename)
       j=i-4
       if(j.lt.1)j=1
       atemp=casename(j:i)
#ifdef UNIX
       if((index(atemp,'.').ne.0).and.
     +    (index(atemp,'./').eq.0)) then
#else
       if((index(atemp,'.').ne.0).and.
     +    (index(atemp,'.\').eq.0)) then
#endif
         pestinfile=casename
       else
         pestinfile=trim(casename)//'.pst'
       end if
       i=len_trim(pestinfile)
       j=max(i-3,1)
       atemp=pestinfile(j:i)
       call lowcas(atemp)
       if(atemp(1:4).ne.'.pst')then
         call addquote(pestinfile,afile)
         write(amessage,35) trim(afile)
35       format(' PEST control file ',a,' does not have an ',
     +   'extension of ".pst".')
         go to 9800
       end if
       open(unit=10,file=pestinfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(pestinfile,afile)
         write(amessage,36) trim(afile)
36       format(' Cannot open PEST control file ',a,'.')
         go to 9800
       end if

       i=len_trim(pestinfile)
       if(i-3.ge.1)then
         recinfile=pestinfile(1:i-3)//'rec'
       else
         recinfile='rec'
       end if
       open(unit=11,file=recinfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(recinfile,afile)
         write(amessage,40) trim(afile)
40       format(' Cannot open PEST run record file ',a,'.')
         go to 9800
       end if

       i=len_trim(pestoutfile)
       j=i-4
       if(j.lt.1)j=1
       atemp=pestoutfile(j:i)
#ifdef UNIX
       if((index(atemp,'.').ne.0).and.
     +    (index(atemp,'./').eq.0)) then
#else
       if((index(atemp,'.').ne.0).and.
     +    (index(atemp,'.\').eq.0)) then
#endif
         continue
       else
         pestoutfile=trim(pestoutfile)//'.pst'
       end if
       i=len_trim(pestoutfile)
       j=max(i-3,1)
       atemp=pestoutfile(j:i)
       call lowcas(atemp)
       if(atemp(1:4).ne.'.pst')then
         call addquote(pestoutfile,afile)
         write(amessage,35) trim(afile)
         go to 9800
       end if
       open(unit=20,file=pestoutfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(pestoutfile,afile)
         write(amessage,50) trim(afile)
50       format(' Cannot write to PEST control file ',a,'.')
         go to 9800
       end if

C -- The existing PEST control file is read in order to obtain a limited
C    amount of information.

       call addquote(pestinfile,afile)
       write(6,80) trim(afile)
80     format(/,' - reading PEST control file ',a,' for first time...')
       iline=1
       read(10,'(a)',err=9000,end=9000) cline
       if(cline(1:3).ne.'pcf') go to 9000
81     iline=iline+1
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 81
       if((cline(1:1).ne.'*').or.(index(cline,'control').eq.0))
     + go to 9000
82     iline=iline+1
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 82
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
83     iline=iline+1
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 83
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.ne.0)then
         call writint(aline,iline)
         write(amessage,85) trim(aline),trim(afile)
85       format(' Five entries are expected at line ',a,' of file ',
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
         write(amessage,90) trim(aline),trim(afile)
90       format(' NOBSGP supplied as zero at line ',a,' of PEST ',
     +   'control file ',a,'.')
         go to 9800
       end if
       if(nobsgp.lt.0)then
         call writint(aline,iline)
         write(amessage,91) trim(aline),trim(afile)
91       format(' NOBSGP supplied as negative at line ',a,' of PEST ',
     +   'control file ',a,'.')
         go to 9800
       end if

C -- Some memory is allocated.

       allocate(aobsgp(nobsgp),covind(nobsgp),phigp(nobsgp),
     + factor(nobsgp),inzwt(nobsgp),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,100)
100      format(' Cannot allocate sufficient memory to continue ',
     +   'execution.')
         go to 9800
       end if

C -- The "observation groups" section is found and observation groups read.

       do
         iline=iline+1
         read(10,'(a)',err=9000,end=110) cline
         if(index(cline,'*').ne.0) then
           ltemp=skipline(cline)
           if(cline(1:1).eq.'*')then
             call lowcas(cline)
             if(index(cline,'observation gr').ne.0) go to 120
           end if
         end if
       end do
110    write(amessage,115) trim(afile)
115    format(' Cannot find "observation groups" section of PEST ',
     + 'control file ',a,'.')
       go to 9800
120    do iobsgp=1,nobsgp
121      iline=iline+1
         read(10,'(a)',err=9000,end=9000) cline
         if(skipline(cline)) go to 121
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.eq.0)then
           covind(iobsgp)=1
         else
           covind(iobsgp)=0
         end if
         aobsgp(iobsgp)=cline(lw(1):rw(1))
         call lowcas(aobsgp(iobsgp))
         if(aobsgp(iobsgp)(1:1).eq.'*') go to 9000
         if(iobsgp.gt.1)then
           do jobsgp=1,iobsgp-1
             if(aobsgp(jobsgp).eq.aobsgp(iobsgp))then
               call writint(aline,iline)
               write(amessage,123) trim(aline),trim(afile)
123            format(' Duplicate observation group name found at ',
     +         'line ',a,' of PEST control file ',a,'.')
               go to 9800
             end if
           end do
         end if
       end do
       if(ipestmode.eq.2)then
         do iobsgp=1,nobsgp
           if(aobsgp(iobsgp)(1:7).eq.'predict') go to 125
         end do
         write(amessage,122) trim(afile)
122      format(' File ',a,' instructs PEST to run in "predictive ',
     +   'analysis" mode. However there is no observation group ',
     +   'named "predict".')
         go to 9800
125      continue
       else if(ipestmode.eq.3)then
         icount=0
         do iobsgp=1,nobsgp
           if(aobsgp(iobsgp)(1:5).eq.'regul') icount=icount+1
         end do
         if(icount.eq.0)then
           write(amessage,127) trim(afile)
127        format(' PEST control file ',a,' instructs PEST to run ',
     +     'in "regularisation" mode. However there are no ',
     +     'observation groups whose names begin with "regul" ',
     +     'cited in this file.')
           go to 9800
         end if
       end if

C -- The run record file is now perused to read objective function contributions
C    corresponding to initial conditions.

       call addquote(recinfile,afile)
       write(6,150) trim(afile)
150    format(/,' - reading PEST run record file ',a,'...')
       jline=0
       do
         jline=jline+1
         read(11,'(a)',err=9100,end=9150) cline
         if(index(cline,'INITIAL CONDITIONS').ne.0) go to 160
       end do
160    continue
       if((ipestmode.eq.1).or.(ipestmode.eq.2))then
         do
           jline=jline+1
           read(11,'(a)',err=9200,end=9200) cline
           call lowcas(cline)
           if(index(cline,'ie phi').ne.0)then
             n=index(cline,'=')
             if(n.eq.0) go to 9200
             atemp1=cline(n+1:)
             call realread(ifail,atemp1,totphi)
             if(ifail.ne.0) go to 9200
             go to 161
           end if
         end do
161      continue
       end if
       if((ipestmode.eq.1).and.(nobsgp.eq.1))then
         phigp(1)=totphi
         go to 200
       else if ((ipestmode.eq.2).and.(nobsgp.eq.2))then
         if(aobsgp(1).eq.'predict')then
           phigp(2)=totphi
         else
           phigp(1)=totphi
         end if
         go to 200
       end if
       do
         jline=jline+1
         read(11,'(a)',err=9200,end=9200) cline
         call lowcas(cline)
         if(index(cline,'contribution').ne.0) then
           if((ipestmode.eq.2).and.
     +        (index(cline,'predictive error').ne.0)) cycle
           go to 170
         end if
       end do
170    continue
       itt=nobsgp
       if(ipestmode.eq.2)itt=nobsgp-1
       do iobsgp=1,itt
         n=index(cline,'"')
         if(n.eq.0) go to 9200
         atemp1=cline(n+1:n+14)
         n=index(atemp1,'"')
         if(n.eq.0) go to 9200
         atemp1=atemp1(1:n-1)
         call lowcas(atemp1)
         atemp1=adjustl(atemp1)
         do jobsgp=1,nobsgp
           if(atemp1.eq.aobsgp(jobsgp)) go to 180
         end do
         go to 9300
180      continue
         n=index(cline,'=')
         if(n.eq.0) go to 9200
         cline=adjustl(cline(n+1:))
         anum=cline(1:20)
         call realread(ifail,anum,phigp(jobsgp))
         if(ifail.ne.0) go to 9200
         if(iobsgp.eq.nobsgp) go to 200
         jline=jline+1
         read(11,'(a)',err=9200,end=9200) cline
       end do
200    continue
       close(unit=11)
       write(6,210) trim(afile)
210    format(' - file ',a,' read ok.')

       totphigp=0.0
       do iobsgp=1,nobsgp
         if(ipestmode.eq.1)then
           totphigp=totphigp+phigp(iobsgp)
         else if(ipestmode.eq.2)then
           if(aobsgp(iobsgp).ne.'predict')then
             totphigp=totphigp+phigp(iobsgp)
           end if
         else if(ipestmode.eq.3)then
           if(aobsgp(iobsgp)(1:5).ne.'regul') then
             totphigp=totphigp+phigp(iobsgp)
           end if
         end if
       end do

C -- The following code is a modification of PTWTADJ1, and therefore is a little messy.
C    However it does the job and no-one should complain.

C -- We now skip some lines of code here (to which we will return later), and
C    read the PEST control file a first time.

       inzwt=0               ! an array
       if(ipread.eq.0) go to 231


211    continue
C       write(6,130) trim(afile)
C130    format(' - file ',a,' read ok.')
       ipread=1

C -- The total number of non-zero-weighted observations is evaluated.

       inztot=0
       do iobsgp=1,nobsgp
         aaobsgp=aobsgp(iobsgp)
         if(ipestmode.eq.2)then
           if(aaobsgp.eq.'predict') cycle
         else if (ipestmode.eq.3)then
           if(aaobsgp(1:5).eq.'regul') cycle
         end if
         inztot=inztot+inzwt(iobsgp)
       end do
       if(inztot.eq.0)then
         call addquote (pestinfile,afile)
         write(amessage,132) trim(afile)
132      format(' The number of non-zero-weighted, non-regularisation,'
     +   ' non-prediction observations in file ',a,' is zero.')
         go to 9800
       end if
       rtemp=inztot-paramcorrect
       if(rtemp.le.0.0)then
         call addquote(pestinfile,afile)
         write(amessage,133)
133      format(' The number of non-zero-weighted, non-regularisation,',
     +   ' non-prediction observations corrected by subtraction ',
     +   'of the parameter correction term is zero or negative. ',
     +   'Re-run PWTADJ2 with PARAMCORRECT reduced or set to zero.')
         go to 9800
       end if
       correction=float(inztot-paramcorrect)/float(inztot)

C -- We now evaluate the factors to apply to the different observation groups.

       totfactor = sqrt(float(inztot-paramcorrect)/totphigp)
       factor=-1.0d35        ! an array
       icount=0
       do iobsgp=1,nobsgp
         aaobsgp=aobsgp(iobsgp)
         if(ipestmode.eq.2)then
           if(aaobsgp.eq.'predict') cycle
         else if (ipestmode.eq.3)then
           if(aaobsgp(1:5).eq.'regul') cycle
         end if
         if(phigp(iobsgp).ne.0.0)then
             icount=icount+1
             if(ang.eq.'g')then
                factor(iobsgp)=
     +          sqrt(inzwt(iobsgp)/phigp(iobsgp)*correction)
             else
                factor(iobsgp)=totfactor
             end if
         end if
       end do
       if(icount.eq.0)then
         write(amessage,220) trim(afile)
220      format(' The contribution to the total initial objective ',
     +   'function of all non-regularisation and/or ',
     +   'non-prediction observation groups cited in run ',
     +   'record file ',a,' is zero.')
         go to 9800
       end if

C -- The original PEST control file is rewound.

       call addquote(pestinfile,afile)
       call addquote(pestoutfile,bfile)
       write(6,225) trim(afile),trim(bfile)
225    format(/,' - re-reading file ',a,' and writing file ',a,'...')
       rewind(unit=10,iostat=ierr)
       if(ierr.ne.0) then
         write(amessage,230) trim(afile)
230      format(' Cannot rewind PEST control file ',a,'.')
         go to 9800
       end if

C -- A direct copy is made up until the "observation data" section.
C -- However NOPTMAX is set to 50.

       iline=0
       do i=1,8
2311     iline=iline+1
         read(10,'(a)',err=9400,end=9400) cline
         write(20,'(a)',err=9500) trim(cline)
         if(skipline(cline)) go to 2311
       end do
232    iline=iline+1
       read(10,'(a)',err=9400,end=9400) cline
       dline=cline
       if(skipline(cline))then
         write(20,'(a)') trim(dline)
         go to 232
       end if
       call linspl(ifail,2,lw,rw,dline)
       if(ifail.ne.0) go to 9400
       dline='   50  '//dline(lw(2):)
       write(20,'(a)',err=9500) trim(dline)

231    continue

       do
         iline=iline+1
         read(10,'(a)',err=9400,end=240) cline
         if(ipread.eq.1) write(20,'(a)',err=9500) trim(cline)
         if(index(cline,'*').ne.0)then
           ltemp=skipline(cline)
           if(cline(1:1).eq.'*')then
             call lowcas(cline)
             if(index(cline,'observation data').ne.0) go to 250
           end if
         end if
       end do
240    write(amessage,245) trim(afile)
245    format(' Cannot find "observation data" section of ',
     + 'PEST control file ',a,'.')
       go to 9800
250    do iobs=1,nobs
257      iline=iline+1
         read(10,'(a)',err=9000,end=9000) cline
         dline=cline
         if(skipline(cline)) then
           if(ipread.ne.0)then
             write(20,'(a)') trim(dline)
           end if
           go to 257
         end if
         call linspl(ifail,4,lw,rw,dline)
         if(ifail.ne.0) go to 9000
         aobs=dline(lw(1):rw(1))
         call lowcas(aobs)
         aaobsgp=dline(lw(4):rw(4))
         call lowcas(aaobsgp)
         call realread(ifail,dline(lw(2):rw(2)),oval)
         if(ifail.ne.0) go to 9000
         call realread(ifail,dline(lw(3):rw(3)),owght)
         if(ifail.ne.0) go to 9000
         if(owght.lt.0.0)then
           call writint(aline,iline)
           write(amessage,251) trim(aline),trim(afile)
251        format(' Negative observation weight encountered at line ',
     +     a,' of PEST control file ',a,'.')
           go to 9800
         end if
         call lowcas(aobs)
         call lowcas(aaobsgp)
         do iobsgp=1,nobsgp
           if(aaobsgp.eq.aobsgp(iobsgp))then
             if(ipread.eq.0)then
               if((owght.gt.0.0d0).or.(covind(iobsgp).ne.0))then
                 inzwt(iobsgp)= inzwt(iobsgp)+1
               end if
             else
               if(covind(iobsgp).eq.0)then
                 if(factor(iobsgp).gt.-1.0e34)then
                   owght=owght*factor(iobsgp)
                 end if
               end if
               if(dline(rw(4)+1:).eq.' ')then
                 write(20,260) trim(aobs),oval,owght,trim(aaobsgp)
260              format(1x,a,t23,1pg14.7,2x,1pg13.6,2x,a)
               else
                 write(20,261) trim(aobs),oval,owght,trim(aaobsgp),
     +           trim(dline(rw(4)+1:))
261              format(1x,a,t23,1pg14.7,2x,1pg13.6,2x,a,1x,a)
               end if
             end if
             go to 280
           end if
         end do
         call writint(aline,iline)
         write(amessage,270) trim(aaobsgp),trim(aline),trim(afile)
270      format(' Unknown observation group "',a,'" at line ',a,
     +   ' of PEST control file ',a,'.')
         go to 9800
280      continue
       end do

C -- Observations have all be taken care of. If necessary we now take care of
C    prior information.

       if(nprior.eq.0) then
         if(ipread.eq.1)then
           go to 410
         else
           go to 211
         end if
       end if
       ifcount=0
       do
         iline=iline+1
         read(10,'(a)',err=9400,end=300) cline
         if(ipread.ne.0) write(20,'(a)',err=9500) trim(cline)
         if(index(cline,'*').ne.0)then
           ltemp=skipline(cline)
           if(cline(1:1).eq.'*')then
             call lowcas(cline)
             if(index(cline,'prior information').ne.0) go to 320
           end if
         end if
       end do
300    write(amessage,310) trim(afile)
310    format(' Cannot find "prior information" section of ',
     + 'PEST control file ',a,'.')
       go to 9800
320    continue
       do iprior=1,nprior
         dline=' '
         if(iprior.eq.1)then
330        iline=iline+1
           read(10,'(a)',err=9600,end=9600) cline
           fline=cline
           if(skipline(cline))then
             if(ipread.eq.1) write(20,'(a)') trim(fline)
             go to 330
           end if
         end if
         dline=cline
350      iline=iline+1
         if(iprior.ne.nprior)then
           read(10,'(a)',err=9600,end=9600) cline
         else
           read(10,'(a)',iostat=ierr)cline
           if(ierr.ne.0) cline=' '
         end if
         if(ierr.eq.0)then
           fline=cline
           if(skipline(cline))then
             ifcount=ifcount+1
             if(ifcount.le.MAXIFCOUNT)then
               ffline(ifcount)=fline
             end if
             go to 350
           end if
         else
           cline=' '
         end if
         cline=adjustl(cline)
         if(cline(1:1).eq.'&')then
           cline(1:1)=' '
           cline=adjustl(cline)
           dline=trim(dline)//' '//trim(cline)
           go to 350
         end if
         ne=index(dline,'=')
         if(ne.eq.0) go to 9600
         eline=dline
         dline=dline(ne+1:)
         call linspl(ifail,3,lw,rw,dline)
         if(ifail.ne.0) then
           call linspl(ifail,2,lw,rw,dline)
           if(ifail.eq.0)then
             call writint(aline,iline-1)
             write(amessage,359) trim(aline),trim(afile)
359          format(' Observation group name missing from prior ',
     +       'information equation at or about line ',a,' of PEST ',
     +       'control file ',a,'.')
             go to 9800
           else
             go to 9600
           end if
         end if
         aaobsgp=dline(lw(3):rw(3))
         call lowcas(aaobsgp)
         do iobsgp=1,nobsgp
           if(aaobsgp.eq.aobsgp(iobsgp)) go to 370
         end do
         call writint(aline,iline-1)
         write(amessage,360) trim(aaobsgp),trim(aline),trim(afile)
360      format(' Unknown observation group "',a,'" in prior ',
     +   'information equation at or about line ',a,' of PEST ',
     +   'control file ',a,'.')
         go to 9800
370      continue
         aweight=dline(lw(2):rw(2))
         call realread(ifail,aweight,owght)
         if(ifail.ne.0)then
           call writint(aline,iline-1)
           write(amessage,380) trim(aline),trim(afile)
380        format(' Cannot read weight for prior information equation ',
     +     'at or about line ',a,' of PEST control file ',a,'.')
           go to 9800
         end if
         if(owght.lt.0.0)then
           call writint(aline,iline-1)
           write(amessage,383) trim(aline),trim(afile)
383        format(' Negative weight is assigned to prior information',
     +     ' equation at or about line ',a,' of PEST control file ',
     +     a,'.')
           go to 9800
         end if
         if(ipread.eq.0)then
           if((owght.gt.0.0d0).or.(covind(iobsgp).ne.0))then
             inzwt(iobsgp)=inzwt(iobsgp)+1
           end if
         else
           if(covind(iobsgp).eq.0)then
             if(factor(iobsgp).gt.-1.0e34)then
                owght=owght*factor(iobsgp)
                write(aweight,'(1pg13.6)') owght
                dline=dline(1:lw(2)-1)//trim(aweight)//dline(rw(2)+1:)
                eline=eline(1:ne)//' '//trim(dline)
             end if
           end if
         end if
         if(ipread.eq.1)then
385        continue
           nb=len_trim(eline)
           if(nb.lt.200)then
             write(20,'(a)',err=9500) trim(eline)
           else
             do i=200,1,-1
               if(eline(i:i).eq.' ')then
                 write(20,'(a)',err=9500) eline(1:i)
                 eline(1:i)=' '
                 eline='& '//adjustl(eline)
                 go to 385
               end if
             end do
           end if
           if(ifcount.gt.0)then
             if(ifcount.gt.MAXIFCOUNT)ifcount=MAXIFCOUNT
             do i=1,ifcount
               write(20,'(a)') trim(ffline(i))
             end do
           end if
         end if
         ifcount=0
       end do
       if(ipread.eq.0) go to 211

C -- The remainder of the old PEST control file is now transferred to the new one.

400    if(ierr.eq.0) write(20,'(a)',err=9500) trim(fline)
410    continue
       do
         read(10,'(a)',err=9400,end=500) cline
         write(20,'(a)',err=9500) trim(cline)
       end do

500    close(unit=10)
       close(unit=20)
       write(6,520) trim(afile)
520    format(' - file ',a,' read ok.')
       write(6,530) trim(bfile)
530    format(' - file ',a,' written ok.')

       icount=0
       do iobsgp=1,nobsgp
         if(covind(iobsgp).eq.1)then
           if(factor(iobsgp).gt.-1.0e34)icount=icount+1
         end if
       end do

       if(icount.ne.0)then
         write(6,540)
540      format(/,' At least one observation group uses a ',
     +   'covariance matrix instead of weights.')
         write(6,550)
550      format(' A factor must be applied to the covariance matrix ',
     +   'manually using, for')
         write(6,560)
560      format(' example the MATSMUL utility. Factors are as ',
     +   'follows:-',/)
         write(6,570)
570      format(' Observation Group        Factor')
         write(6,580)
580      format(' ----------------         ------')
         do iobsgp=1,nobsgp
           if(covind(iobsgp).eq.1)then
             if(factor(iobsgp).gt.-1.0d34)then
               write(6,590) trim(aobsgp(iobsgp)),
     +         1.0/(factor(iobsgp)*factor(iobsgp))
590            format(3x,a,t25,1pg13.6)
             end if
           end if
         end do
       end if

C -- Finally a warning may be issued.

       icount=0
       do iobsgp=1,nobsgp
         if((inzwt(iobsgp).ne.0).and.(phigp(iobsgp).eq.0.0))then
           if(ipestmode.eq.3)then
             if(aobsgp(iobsgp)(1:5).eq.'regul') cycle
           else if(ipestmode.eq.2)then
             if(aobsgp(iobsgp).eq.'predict') cycle
           end if
           icount=icount+1
           if(icount.eq.1) write(6,*)
           write(6,620) trim(aobsgp(iobsgp))
620        format(' Warning: an adjustment factor cannot not be ',
     +     'computed for members of ',/,' obsevation group "',a,
     +     '" as contribution to objective function of this ',
     +     /,' group is zero.')
         end if
       end do

       go to 9900



8900   continue
       write(6,8910)
8910   format(/,' PWTADJ2 is run using the command:',/)
       write(6,8920)
8920   format('     pwtadj2 casename pestoutfile use_groups ',
     + '[param_correction]',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     casename         is an existing PEST casename,')
       write(6,8950)
8950   format('     pestoutfile      is the name of a new PEST control ',
     + 'file,')
       write(6,8955)
8955   format('     use_groups       must be "g" or "ng", and ')
       write(6,8960)
8960   format('     param_correction is an optional "parameter ',
     + 'correction" term.')

       go to 9999


9000   call writint(aline,iline)
       write(amessage,9010) trim(aline),trim(afile)
9010   format(' Error reading line ',a,' of PEST control file ',a,
     + ': check this file with PESTCHEK.')
       go to 9800

9100   call writint(aline,jline)
       write(amessage,9110) trim(afile)
9110   format(' Error encountered while reading line ',a,' of run ',
     + 'record file ',a,'.')
       go to 9800

9150   write(amessage,9160) trim(afile)
9160   format(' Cannot find objective function contributions ',
     + 'for initial parameter values in PEST run ',
     + 'record file ',a,'.')
       go to 9800

9200   write(amessage,9210) trim(afile)
9210   format(' Error or end-of-file condition encountered while ',
     + 'reading objective function contributions for initial ',
     + 'parameter values from PEST run record file ',a,'.')
       go to 9800

9300   call addquote(pestinfile,bfile)
       write(amessage,9310) trim(afile),trim(bfile)
9310   format(' Run record file ',a,' appears to be incompatible ',
     + 'with PEST control file ',a,'; observation groups are not the ',
     + 'same in these two files.')
       go to 9800

9400   call writint(aline,iline)
       write(amessage,9410) trim(afile)
9410   format(' Error re-reading line ',a,' of PEST control file ',
     + a,'.')
       go to 9800

9500   write(amessage,9510) trim(bfile)
9510   format(' Error writing to new PEST control file ',a,'.')
       go to 9800

9600   write(amessage,9610) trim(afile)
9610   format(' Error reading prior information section of PEST ',
     + 'control file ',a,': check this file with PESTCHEK.')
       go to 9800


9800   continue
       call writmess(6,amessage)

9900   continue

       deallocate(phigp,factor,covind,aobsgp,inzwt,stat=ierr)

       inquire(unit=10,opened=lopened)
       if(lopened) close(unit=10,iostat=ierr)
       inquire(unit=11,opened=lopened)
       if(lopened) close(unit=11,iostat=ierr)
       inquire(unit=20,opened=lopened)
       if(lopened) close(unit=20,iostat=ierr)

9999   continue

       end


      subroutine addquote(afile,aqfile)

C -- Subroutine ADDQUOTE adds quotes to a filename if it has a space in it.

        implicit none

        character*(*) afile
        character*(*) aqfile
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


        subroutine linspl(ifail,num,lw,rw,cline)

c -- Subroutine LINSPL splits a line into whitespace-separated substrings.

        integer ifail,nw,nblc,j,i
        integer num
        integer lw(num),rw(num)
        character*(*) cline

        ifail=0
        nw=0
        nblc=len_trim(cline)
        if(nblc.eq.0) then
          ifail=1
          return
        end if
        j=0
5       if(nw.eq.num) return
        do 10 i=j+1,nblc
        if((cline(i:i).ne.' ').and.(cline(i:i).ne.',')
     +  .and.(ichar(cline(i:i)).ne.9)) go to 20
10      continue
        ifail=1
        return
20      nw=nw+1
        lw(nw)=i
        do 30 i=lw(nw)+1,nblc
        if((cline(i:i).eq.' ').or.(cline(i:i).eq.',')
     +  .or.(ichar(cline(i:i)).eq.9)) go to 40
30      continue
        rw(nw)=nblc
        if(nw.lt.num) ifail=1
        return
40      rw(nw)=i-1
        j=rw(nw)
        go to 5

        end



        subroutine lowcas(astrng)

C -- Subroutine LOWCAS converts a string to lower case.

        integer i,j
        character*(*) astrng

        do 10 i=1,len_trim(astrng)
        j=ichar(astrng(i:i))
        if((j.ge.65).and.(j.le.90)) astrng(i:i)=char(j+32)
10      continue
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



        subroutine writint(atemp,ival)

c       Subroutine WRITINT writes an integer to a character variable.

        integer*4 ival
        character*6 afmt
        character*(*) atemp

        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(atemp)
        write(atemp,afmt)ival
        atemp=adjustl(atemp)
        return
        end



        SUBROUTINE REALREAD(IFAIL,CLINE,RTEMP)

C -- Subroutine REALREAD reads a real number from a string.

        INTEGER IFAIL
        real RTEMP
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
