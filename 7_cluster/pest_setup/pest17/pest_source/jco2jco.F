       program jco2jco

! --   Program jco2jco reads a JCO file corresponding to one PEST control file
!      and rewrites it for another.

       implicit none
       logical equals,lexist,skipline
       integer j,k,nespar1,itemp1,itemp2,bn,ies,irow,jes,ipp,
     +         n,ierr,i,npar1,nobs1,npargp1,nprior1,icount,n1,
     +         npar2,nobs2,npargp2,nprior2,nespar2,iobs,kes,
     +         ifail,nobs3,ii,jj,idifflag,jjj
       integer iextraflag,iextraflag_1,itemp
       integer iline,iprior,ipar,jmul,jmath,jobs,jwght,jname,jognme,
     +         jx,neg,nobsgp1,iflag,n2,jstart,nblc,ilog,nxrow2
       integer neqq
       integer lw(11),rw(11)
       integer, allocatable :: itrans1(:),itrans2(:)
       double precision dtemp,dtemp1,logfac,ilogfac,ratio,ratio1,
     + ratio2,dd1,dd2,scalefac,rstemp1,rstemp2,dtemp2
       double precision rtemp,rrtemp,tx,rtemp1
       double precision, allocatable :: pval1(:),pval2(:),scale1(:),
     + scale2(:),offset1(:),offset2(:)
       double precision, allocatable :: refobs1(:),oval1(:),prval2(:)
       double precision, allocatable :: refobs3(:),oval3(:),refprior2(:)
       double precision, allocatable :: xp(:,:)
       double precision, allocatable :: x1(:,:),x2(:,:),x3(:,:)
       character*10 how
       character*12 aversion
       character*12 aapar,aspar1,aspar2
       character*15 atemp,atemp1,atemp2,aext
       character*15 fmt
       character*20 aaobs
       character*40 aprefix
       character*12, allocatable :: apar1(:),apar2(:)
       character*20, allocatable :: aobs1(:),aobs2(:)
       character*20, allocatable :: aprior2(:)
       character*200 pestfile1,pestfile2,jcofile1,jcofile2,afile,bfile
       character*400 cline,errmsg

! -- Initialisation

       logfac=log(10.0d0)
       ilogfac=1.0d0/logfac
       idifflag=0

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(' JCO2JCO Version ',a,'. Watermark Numerical Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

! -- The command line is parsed.

       if(cline.eq.' ') go to 9000
       call spacesub(cline)
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.eq.0) go to 9000
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       pestfile1=cline(lw(1):rw(1))
       pestfile2=cline(lw(2):rw(2))
       call remchar(pestfile1,char(211))
       call remchar(pestfile2,char(211))
       n=len_trim(pestfile1)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=pestfile1(n1:n)
       call lowcas(aext)
       if(aext.eq.'.pst')pestfile1(n1:n)=' '
       jcofile1=pestfile1
       n=len_trim(pestfile1)
       pestfile1(n+1:)='.pst'
       jcofile1(n+1:)='.jco'
       n=len_trim(pestfile2)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=pestfile2(n1:n)
       call lowcas(aext)
       if(aext.eq.'.pst')pestfile2(n1:n)=' '
       jcofile2=pestfile2
       n=len_trim(pestfile2)
       pestfile2(n+1:)='.pst'
       jcofile2(n+1:)='.jco'

! -- Input files are opened.

       open(unit=10,file=pestfile1,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(pestfile1,afile)
         write(errmsg,110) trim(afile)
110      format('Cannot open PEST control file ',a,'.')
         call write_message(errmsg)
         stop
       end if
       how='read'
       call open_unformatted_file(ifail,15,how,jcofile1,errmsg)
       if(ifail.ne.0) go to 9890

! -- Read the first control file - parameter and observation data only
!    (not prior information).

       do i=1,3
111      continue
         read(10,'(a)',err=9100,end=9100) cline
         if(skipline(cline)) go to 111
       end do
112    continue
       iline=iline+1
       read(10,'(a)',err=9100,end=9100) cline
       if(skipline(cline)) go to 112
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0) go to 9100
       call intread(ifail,cline(lw(1):rw(1)),npar1)
       if(ifail.ne.0) go to 9100
       call intread(ifail,cline(lw(2):rw(2)),nobs1)
       if(ifail.ne.0) go to 9100
       call intread(ifail,cline(lw(3):rw(3)),npargp1)
       if(ifail.ne.0) go to 9100
       call intread(ifail,cline(lw(4):rw(4)),nprior1)
       if(ifail.ne.0) go to 9100
       if(npar1.le.0) go to 9100
       allocate(apar1(npar1),pval1(npar1),itrans1(npar1),
     + aobs1(nobs1),scale1(npar1),offset1(npar1),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,125)
125      format('Cannot allocate sufficient memory to continue ',
     +   'JCO2JCO execution.')
         go to 9890
       end if
       do
         read(10,'(a)',err=9100,end=130) cline
         if(skipline(cline))cycle
         call lowcas(cline)
         if(index(cline,'* parameter data').ne.0) go to 150
       end do
130    continue
       call addquote(pestfile1,afile)
       write(errmsg,140) trim(afile)
140    format('Cannot find "parameter data" section of PEST ',
     + 'control file ',a,'.')
       go to 9890
150    do i=1,npar1
151      continue
         read(10,'(a)',err=9100,end=9100) cline
         if(skipline(cline)) go to 151
         neqq=index(cline,'=')
         if(neqq.gt.1) go to 151
         call lowcas(cline)
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9100
         if(cline(lw(2):rw(2)).eq.'file_parameter')go to 151
         call linspl(ifail,9,lw,rw,cline)
         if(ifail.ne.0) go to 9100
         apar1(i)=cline(lw(1):rw(1))
         atemp=cline(lw(2):rw(2))
         call drealrd(ifail,cline(lw(4):rw(4)),pval1(i))
         if(ifail.ne.0) go to 9100
         call drealrd(ifail,cline(lw(8):rw(8)),scale1(i))
         if(ifail.ne.0) go to 9100
         call drealrd(ifail,cline(lw(9):rw(9)),offset1(i))
         if(ifail.ne.0) go to 9100
         if(atemp.eq.'log')then
           itrans1(i)=1
         else if(atemp.eq.'none')then
           itrans1(i)=0
         else if(atemp.eq.'fixed')then
           itrans1(i)=-1000000
         else if(atemp.eq.'tied')then
           itrans1(i)=-1
         else
           go to 9100
         end if
       end do
       icount=0
       do i=1,npar1
         if(itrans1(i).eq.-1)icount=icount+1
       end do
       if(icount.gt.0)then
         do i=1,icount
152        continue
           read(10,'(a)',err=9100,end=9100) cline
           if(skipline(cline)) go to 152
           neqq=index(cline,'=')
           if(neqq.gt.1) go to 152
           call lowcas(cline)
           call linspl(ifail,2,lw,rw,cline)
           if(ifail.ne.0) go to 9100
           atemp1=cline(lw(1):rw(1))
           atemp2=cline(lw(2):rw(2))
           if(atemp2.eq.'file_parameter')go to 152
           do j=1,npar1
             if(atemp1.eq.apar1(j)) go to 170
           end do
           go to 9100
170        continue
           do k=1,npar1
             if(atemp2.eq.apar1(k)) go to 180
           end do
           go to 9100
180        continue
           if(itrans1(j).ne.-1) go to 9100
           if(itrans1(k).lt.0) go to 9100
           itrans1(j)=-k
         end do
       end if

       nobsgp1=-999
       do
         read(10,'(a)',err=9100,end=200) cline
         if(skipline(cline)) cycle
         call lowcas(cline)
         if(index(cline,'* observation g').ne.0) nobsgp1=1
         if(index(cline,'* observation data').ne.0) go to 220
       end do
200    call addquote(pestfile1,afile)
       write(errmsg,210) trim(afile)
210    format('Cannot find "observation data" section of file ',a,'.')
       go to 9890
220    do i=1,nobs1
221      continue
         read(10,'(a)',err=9100,end=9100) cline
         if(skipline(cline)) go to 221
         call linspl(ifail,1,lw,rw,cline)
         if(ifail.ne.0) go to 221
         aobs1(i)=cline(lw(1):rw(1))
         call lowcas(aobs1(i))
       end do
       close(unit=10)

       nespar1=0
       do i=1,npar1
         if(itrans1(i).ge.0)nespar1=nespar1+1
       end do
       if(nespar1.eq.0)then
         call addquote(pestfile1,afile)
         write(errmsg,231) trim(afile)
231      format('There are no adjustable parameters in file ',a,'.')
         go to 9890
       end if

       call addquote(pestfile1,afile)
       write(6,230) trim(afile)
230    format(' - file ',a,' read ok.')

! -- The Jacobian matrix file is read.

       read(15,err=9250,end=9250)itemp1,itemp2
       if(abs(itemp1).ne.nespar1) go to 9200
       bn=abs(itemp2)
       if(itemp1.lt.0)then
         continue
       else
         call addquote(jcofile1,afile)
         write(errmsg,240) trim(afile)
240      format('Jacobian matrix file ',a,' was not produced by ',
     +   'version 8 or later of PEST; use program JCOTRANS to ',
     +   'convert this to new format.')
         go to 9890
       end if

       allocate(x1(nobs1,nespar1),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,125)
         go to 9890
       end if
       allocate(refobs1(nobs1),oval1(nobs1),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,125)
         go to 9890
       end if

       do i=1,nespar1
         do j=1,nobs1
           x1(j,i)=0.0d0
         end do
       end do
       read(15,err=9250,end=9250)icount
       do i=1,icount
         read(15,err=9250,end=9250) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         if(irow.le.nobs1) x1(irow,ies)=dtemp
       end do
       do i=1,npar1
         if(itrans1(i).lt.0) cycle
         read(15,err=9250,end=9250) aapar
         if(aapar.ne.apar1(i)) go to 9200
       end do
       do i=1,nobs1
         read(15,err=9250,end=9250) aaobs
         call lowcas(aaobs)
         if(aaobs.ne.aobs1(i)) then
           go to 9200
         end if
       end do

! -- Extra information is read from the bottom of the file if it is present.

       iextraflag=0
       if(nprior1.gt.0)then
         do iprior=1,nprior1
           read(15,err=241,end=241) aaobs
         end do
       end if
       do i=1,nobs1
         read(15,err=241,end=241) refobs1(i),oval1(i)
       end do
       if(nprior1.gt.0)then
         do iprior=1,nprior1
           read(15,err=241,end=241) dtemp,dtemp
         end do
       end if
       iextraflag=1
241    continue

! -- Further extra information is read if this is available.

       iextraflag_1=0
       do i=1,npar1
         read(15,err=242,end=242) dtemp,itemp,dtemp1,dtemp2
         if(.not.equals(dtemp,pval1(i))) go to 242
         if(.not.equals(dtemp1,scale1(i))) go to 242
         if(.not.equals(dtemp2,offset1(i))) go to 242
         if(itemp.ne.itrans1(i)) go to 242
       end do
       iextraflag_1=1
242    continue

       close(unit=15)
       call addquote(jcofile1,afile)
       write(6,230) trim(afile)

! -- The second PEST control file is read and its Jacobian file opened, ready
!    for writing.

! -- But first an existing Jacobian file is deleted.

       inquire (file=jcofile2,exist=lexist)
       if(lexist)then
         how='read'
         call open_unformatted_file(ifail,30,how,jcofile2,errmsg)
         close(unit=30,status='delete',iostat=ierr)
       end if

       how='write'
       call open_unformatted_file(ifail,15,how,jcofile2,errmsg)
       if(ifail.ne.0) go to 249
       go to 251
249    continue
       if(ierr.ne.0)then
         call addquote(jcofile2,afile)
         write(errmsg,250) trim(afile)
250      format('Cannot open Jacobian matrix file ',a,' for output.')
         go to 9890
       end if
251    continue

! -- Data is read from the second PEST control file.

       open(unit=10,file=pestfile2,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(pestfile2,afile)
         write(errmsg,110) trim(afile)
         call write_message(errmsg)
         stop
       end if

       do i=1,3
252      continue
         read(10,'(a)',err=9300,end=9300) cline
         if(skipline(cline)) go to 252
       end do
253    continue
       iline=iline+1
       read(10,'(a)',err=9300,end=9300) cline
       if(skipline(cline)) go to 253
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0) go to 9300
       call intread(ifail,cline(lw(1):rw(1)),npar2)
       if(ifail.ne.0) go to 9300
       call intread(ifail,cline(lw(2):rw(2)),nobs2)
       if(ifail.ne.0) go to 9300
       call intread(ifail,cline(lw(3):rw(3)),npargp2)
       if(ifail.ne.0) go to 9300
       call intread(ifail,cline(lw(4):rw(4)),nprior2)
       if(ifail.ne.0) go to 9300
       if(npar2.le.0) go to 9300
       allocate(apar2(npar2),pval2(npar2),itrans2(npar2),
     + aobs2(nobs2),scale2(npar2),offset2(npar2),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,125)
         go to 9890
       end if
       do
         read(10,'(a)',err=9300,end=330) cline
         if(skipline(cline)) cycle
         if(index(cline,'* parameter data').ne.0) go to 350
       end do
330    continue
       call addquote(pestfile2,afile)
       write(errmsg,140) trim(afile)
       go to 9890
350    do i=1,npar2
351      continue
         read(10,'(a)',err=9300,end=9300) cline
         if(skipline(cline)) go to 351
         neqq=index(cline,'=')
         if(neqq.gt.1) go to 351
         call lowcas(cline)
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9300
         if(cline(lw(2):rw(2)).eq.'file_parameter')go to 351
         call linspl(ifail,9,lw,rw,cline)
         if(ifail.ne.0) go to 9300
         apar2(i)=cline(lw(1):rw(1))
         atemp=cline(lw(2):rw(2))
         call drealrd(ifail,cline(lw(4):rw(4)),pval2(i))
         if(ifail.ne.0) go to 9300
         call drealrd(ifail,cline(lw(8):rw(8)),scale2(i))
         if(ifail.ne.0) go to 9300
         call drealrd(ifail,cline(lw(9):rw(9)),offset2(i))
         if(ifail.ne.0) go to 9300
         if(atemp.eq.'log')then
           itrans2(i)=1
         else if(atemp.eq.'none')then
           itrans2(i)=0
         else if(atemp.eq.'fixed')then
           itrans2(i)=-1000000
         else if(atemp.eq.'tied')then
           itrans2(i)=-1
         else
           go to 9300
         end if
       end do
       icount=0
       do i=1,npar2
         if(itrans2(i).eq.-1)icount=icount+1
       end do
       if(icount.gt.0)then
         do i=1,icount
352        continue
           read(10,'(a)',err=9300,end=9300) cline
           if(skipline(cline)) go to 352
           neqq=index(cline,'=')
           if(neqq.gt.1) go to 352
           call lowcas(cline)
           call linspl(ifail,2,lw,rw,cline)
           if(ifail.ne.0) go to 9300
           atemp1=cline(lw(1):rw(1))
           atemp2=cline(lw(2):rw(2))
           if(atemp2.eq.'file_parameter')go to 352
           do j=1,npar2
             if(atemp1.eq.apar2(j)) go to 370
           end do
           go to 9300
370        continue
           do k=1,npar2
             if(atemp2.eq.apar2(k)) go to 380
           end do
           go to 9300
380        continue
           if(itrans2(j).ne.-1) go to 9300
           if(itrans2(k).lt.0) go to 9300
           itrans2(j)=-k
         end do
       end if

       do
         read(10,'(a)',err=9300,end=400) cline
         if(skipline(cline)) cycle
         call lowcas(cline)
         if(index(cline,'* observation data').ne.0) go to 420
       end do
400    call addquote(pestfile2,afile)
       write(errmsg,210) trim(afile)
       go to 9890
420    do i=1,nobs2
421      continue
         read(10,'(a)',err=9300,end=9300) cline
         if(skipline(cline)) go to 421
         call linspl(ifail,1,lw,rw,cline)
         if(ifail.ne.0) go to 421
         aobs2(i)=cline(lw(1):rw(1))
         call lowcas(aobs2(i))
       end do

! -- Prior information is read from the second PEST control file.

       nespar2=0
       do ipar=1,npar2
         if(itrans2(ipar).ge.0) nespar2=nespar2+1
       end do
       if(nprior2.ne.0)then
         allocate (xp(nprior2,nespar2),aprior2(nprior2),
     +   prval2(nprior2),stat=ierr)
         if(ierr.ne.0)then
           write(errmsg,125)
           go to 9890
         end if
         xp = 0.0d0    ! an array
         do
           read(10,'(a)',err=9300,end=9300) cline
           if(skipline(cline)) cycle
           call lowcas(cline)
           cline=adjustl(cline)
           if(cline(1:1).eq.'*')then
             if(index(cline,'prior in').ne.0) exit
           end if
         end do

         iline=0
         iprior=0
         IPAR=1
         JMUL=0
         JMATH=0
         JOBS=0
         JWGHT=0
         JNAME=0
         JOGNME=0
         JX=0
         NEG=0
         DO 101 iprior=1,NPRIOR2
           jstart=1
           DO IES=1,NESPAR2
             xp(iprior,IES)=0.0D0
           end do
35         N2=0
3511       continue
           READ(10,'(a)',END=9300) CLINE
           if(skipline(cline)) go to 3511
           ILINE=ILINE+1
           NBLC=LEN_TRIM(CLINE)
38         CALL GETINT(IFAIL,CLINE,N1,N2,NBLC,' ')
           IF(N1.EQ.0) GO TO 35
           IF(CLINE(N1:N1).EQ.'&') GO TO 38
           IF(JSTART.EQ.1) THEN
             aprior2(iprior)=CLINE(N1:N2)
             CALL lowcas(aprior2(iprior))
             JSTART=0
             JX=1
             NEG=0
           ELSE IF(CLINE(N1:N2).EQ.'=') THEN
             IF(JMATH.EQ.0) GO TO 9201
             JMATH=0
             JOBS=1
           ELSE IF(CLINE(N1:N2).EQ.'+') THEN
             IF(JMATH.EQ.0) GO TO 9201
             JMATH=0
             JX=1
             NEG=0
           ELSE IF(CLINE(N1:N2).EQ.'-') THEN
             IF(JMATH.EQ.0) GO TO 9201
             JMATH=0
             JX=1
             NEG=1
           ELSE IF(CLINE(N1:N2).EQ.'*') THEN
             IF(JMUL.EQ.0) GO TO 9201
             JMUL=0
             JNAME=1
           ELSE IF(JX.EQ.1) THEN
             WRITE(FMT,20,ERR=9201) N2-N1+1
20           FORMAT('(F',I3,'.0)')
             READ(CLINE(N1:N2),FMT,ERR=9201) RTEMP
             IF(NEG.EQ.1) RTEMP=-RTEMP
             JX=0
             JMUL=1
             NEG=0
           ELSE IF(JNAME.EQ.1) THEN
             CALL lowcas(CLINE(N1:N2))
             IF(CLINE(N1:N1+2).EQ.'log') THEN
               AAPAR=CLINE(N1+4:N2-1)
               ILOG=1
             ELSE
               AAPAR=CLINE(N1:N2)
               ILOG=0
             END IF
             CALL WHICH1(IFAIL,NPAR2,IPAR,APAR2,AAPAR)
             IF(IFAIL.NE.0) GO TO 9301
             IF(ITRANS2(IPAR).LT.0) GO TO 9401
             IF((ILOG.EQ.0).AND.(ITRANS2(IPAR).EQ.1)) GO TO 9501
             IF((ILOG.EQ.1).AND.(ITRANS2(IPAR).EQ.0)) GO TO 9601
             IES=0
             DO 50 J=1,NPAR2
               IF(ITRANS2(J).LT.0) GO TO 50
               IES=IES+1
               IF(J.EQ.IPAR) GO TO 60
50           CONTINUE
60           CONTINUE
             IF(RTEMP.NE.0.0D0) THEN
               xp(iprior,IES)=RTEMP
             END IF
             JNAME=0
             JMATH=1
           ELSE IF(JOBS.EQ.1) THEN
             WRITE(FMT,20,ERR=9201) N2-N1+1
             READ(CLINE(N1:N2),FMT,ERR=9201) prval2(iprior)
             JOBS=0
             JWGHT=1
           ELSE IF(JWGHT.EQ.1) THEN
             WRITE(FMT,20,ERR=9201) N2-N1+1
             READ(CLINE(N1:N2),FMT,ERR=9201) rtemp1
             JWGHT=0
             IF(CLINE(N2+1:).EQ.' ')THEN
               go to 101
             ELSE
               JOGNME=1
             END IF
           ELSE IF(JOGNME.EQ.1)THEN
             IF(NOBSGP1.EQ.-999)THEN
               go to 101
             END IF
             ATEMP=CLINE(N1:N2)
             JOGNME=0
             go to 101
           ELSE
             GO TO 9201
           END IF
           GO TO 38
101      continue
       end if

       close(unit=10)
       call addquote(pestfile2,afile)
       write(6,230) trim(afile)

       aprefix='JCO2JCO cannot write second JCO file: '

C -- Dimensions of the second derivatives matrix are evaluated.

       nespar2=0
       do i=1,npar2
         if(itrans2(i).ge.0) nespar2=nespar2+1
       end do
       if(nespar2.gt.nespar1)then
         write(errmsg,450) trim(aprefix)
450      format(a,' second PEST control file has more ',
     +   'adjustable parameters than first.')
         go to 9890
       end if
       nobs3=nobs2
       allocate(x3(nobs3,nespar1),x2(nobs3,nespar2),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,125)
         go to 9890
       end if
       allocate(refobs3(nobs3),oval3(nobs3),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,125)
         go to 9890
       end if
       if(nprior2.ne.0)then
         allocate(refprior2(nprior2),stat=ierr)
         if(ierr.ne.0)then
           write(errmsg,125)
           go to 9890
         end if
       end if

C -- Observations in the first file are re-ordered (and some left out if necessary).

       ii=1
       do iobs=1,nobs2
         aaobs=aobs2(iobs)
         do i=ii,nobs1
           if(aobs1(i).eq.aaobs) go to 600
         end do
         if(ii.ne.1)then
           do i=1,ii-1
             if(aobs1(i).eq.aaobs) go to 600
           end do
         end if
         write(errmsg,590) trim(aprefix),trim(aaobs)
590      format(a,' observation "',a,'" is cited in second ',
     +   'PEST control file but not in first.')
         go to 9890
600      ii=i
         do i=1,nespar1
           x3(iobs,i)=x1(ii,i)
         end do
         refobs3(iobs)=refobs1(ii)
         oval3(iobs)=oval1(ii)
         ii=ii+1
       end do

! -- All derivatives in first file are converted to natural derivatives.

       ies=0
       do i=1,npar1
         if(itrans1(i).lt.0) cycle
         ies=ies+1
         if(itrans1(i).eq.1)then
           dtemp1=pval1(i)
           if(dtemp1.le.0.0)then
             call addquote(pestfile1,afile)
             write(errmsg,460) trim(apar1(i)),trim(afile)
460          format('Zero or negative log-transformed parameter ',
     +       '("',a,'") in PEST control file ',a,'.')
             go to 9890
           end if
           dtemp1=ilogfac/dtemp1
           do j=1,nobs3
             x3(j,ies)=x3(j,ies)*dtemp1
           end do
         end if
       end do

! -- Some issues with respect to scale are worked out.

       do i=1,npar1
         if((itrans1(i).lt.0).and.(itrans1(i).gt.-1000000))then
           aspar1=apar1(i)
           do j=1,npar2
             if(apar2(j).eq.aspar1)then
               if(abs(scale1(i)-scale2(j)).gt.
     +           1.0e-6*abs(scale1(i)+scale2(j)))then
                 write(errmsg,461) trim(aprefix),trim(aspar1)
461              format(a,' parameter "',a,'" is tied ',
     +           'parameter in first PEST control file but has a ',
     +           'different scale in second PEST control file.')
                 go to 9890
               end if
               go to 462
             end if
           end do
         end if
462      continue
       end do
       do i=1,npar2
         if((itrans2(i).lt.0).and.(itrans2(i).gt.-1000000))then
           aspar2=apar2(i)
           do j=1,npar1
             if(apar1(j).eq.aspar2)then
               if(abs(scale2(i)-scale1(j)).gt.
     +           1.0e-6*abs(scale2(i)+scale1(j)))then
                 write(errmsg,459) trim(aprefix),trim(aspar2)
459              format(a,' parameter "',a,'" is tied ',
     +           'parameter in second PEST control file but has a ',
     +           'different scale in first PEST control file.')
                 go to 9890
               end if
               go to 458
             end if
           end do
         end if
458      continue
       end do

! -- The second Jacobian matrix file is built.

       ies=0
       do i=1,npar2
         if(itrans2(i).lt.0)cycle
         ies=ies+1
         aapar=apar2(i)
         do jj=1,npar1
           if(apar1(jj).eq.aapar) go to 620
         end do
         write(errmsg,431) trim(aprefix),trim(aapar)
431      format(a,' parameter "',a,'" is adjustable in second ',
     +   'PEST control file but is not found in first.')
         go to 9890
620      continue
         if(itrans1(jj).lt.0)then
           write(errmsg,430) trim(aprefix),trim(aapar)
430        format(a,' parameter "',a,'" is adjustable in second ',
     +     'PEST control file but not in first.')
           go to 9890
         end if
         dd2=pval2(i)
         dd1=pval1(jj)
         if(dd1.eq.dd2)then
           continue
         else
           if(abs(dd1-dd2).gt.abs(1.0d-6*(dd1+dd2)))then
             rstemp2=dd2*scale2(i)
             rstemp1=dd1*scale1(jj)
             if(abs(rstemp2-rstemp1).gt.abs(1.0d-6*(rstemp2+rstemp1)))
     +       idifflag=idifflag+1
           end if
         end if
         if(scale1(jj).eq.scale2(i))then
           continue
         else
           if(abs(scale1(jj)-scale2(i)).gt.1.0d-6*
     +        abs(scale1(jj)+scale2(i))) then
              do jjj=1,npar1
                if(itrans1(jjj).eq.-jj)then
                  write(errmsg,442) trim(aprefix),trim(apar1(jj))
442               format(a,' parameter "',a,'" has different scale ',
     +            'in second PEST control file to that in first. ',
     +            'This is only allowed if no parameter is tied ',
     +            'to this parameter in either file.')
                  go to 9890
                end if
              end do
              do jjj=1,npar2
                if(itrans2(jjj).eq.-i)then
                  write(errmsg,442) trim(aprefix),trim(apar1(jj))
                  go to 9890
                end if
              end do
              if((itrans1(jj).ne.0).or.(itrans2(i).ne.0))then
                write(errmsg,446) trim(aprefix),trim(apar1(jj))
446             format(a,' parameter "',a,'" has a different scale ',
     +          'in second PEST control file from that in first. ',
     +          'This is only allowed if the parameter is not log ',
     +          'transformed, tied or fixed in either file. If ',
     +          'appropriate, ',
     +          'consider running JCO2JCO more than once with only ',
     +          'incremental changes to PEST control file on each ',
     +          'occasion.')
                go to 9890
             end if
           end if
           if((scale1(jj).eq.0.0d0).or.(scale2(i).eq.0.0d0))then
             write(errmsg,444) trim(aprefix),trim(apar1(jj))
444          format(a,' parameter "',a,'" has a zero scale in ',
     +       'either first PEST control file or second PEST control ',
     +       'file or both.')
             go to 9890
           end if
         end if
         if(offset1(jj).eq.offset2(i))then
           continue
         else
           if(abs(offset1(jj)-offset2(i)).gt.1.0d-6*
     +        abs(offset1(jj)+offset2(i))) then
              write(errmsg,443) trim(aprefix),trim(apar1(jj))
443           format(a,' parameter "',a,'" has different offset ',
     +        'in second PEST control file to that in first.')
              go to 9890
           end if
         end if

         do j=1,npar1
           if(-itrans1(j).eq.jj)then
             do k=1,npar2
               if(apar2(k).eq.apar1(j))then
                 if(-itrans2(k).ne.i)then
                   write(errmsg,630) trim(aprefix),trim(apar1(j)),
     +             trim(apar1(jj))
630                format(a,' parameter "',a,'" is tied to parameter "',
     +             a,'" in first PEST control file, but not in ',
     +             'second.')
                   go to 9890
                 else
                   go to 640
                 end if
               end if
             end do
             write(errmsg,630) trim(aprefix),trim(apar1(j)),
     +       trim(apar1(jj))
             go to 9890
           end if
640        continue
         end do
         kes=0
         do j=1,jj
           if(itrans1(j).lt.0) cycle
           kes=kes+1
         end do
         if(abs(scale1(jj)-scale2(i)).gt.1.0d-6*
     +      abs(scale1(jj)+scale2(i))) then
           scalefac=scale2(i)/scale1(jj)
           do iobs=1,nobs3
             x2(iobs,ies)=x3(iobs,kes)*scalefac
           end do
         else
           do iobs=1,nobs3
             x2(iobs,ies)=x3(iobs,kes)
           end do
         end if
         jes=0
         do j=1,npar2
           if(-itrans2(j).eq.i)then
             aapar=apar2(j)
             jes=0
             do k=1,npar1
               if(itrans1(k).ge.0)jes=jes+1
               if(apar1(k).eq.aapar)then
                 if(-itrans1(k).eq.jj) then
                   if(pval1(jj).eq.0.0d0) go to 9100
                   ratio1=pval1(k)/pval1(jj)
                   if(pval2(i).eq.0.0d0) go to 9300
                   ratio2=pval2(j)/pval2(i)
                   if(abs(ratio1-ratio2).gt.abs(1.0e-6* ratio1))then
                     write(errmsg,669)trim(apar2(i))
669                  format('Parameters tied to parameter "',a,
     +               '" in two PEST control files are not tied in ',
     +               'the same ratio.')
                     go to 9890
                   end if
                   continue
                 else if(itrans1(k).eq.-1000000)then
                   write(errmsg,670) trim(aprefix),trim(aapar)
670                format(a,' parameter "',a,'" is fixed in first ',
     +             'PEST control file but not in second.')
                   go to 9890
                 else
                   ratio=pval2(j)/pval2(i)
                   do iobs=1,nobs3
                     x2(iobs,ies)=x2(iobs,ies)+x3(iobs,jes)*ratio
                   end do
                 end if
                 go to 680
               end if
             end do
             write(errmsg,690) trim(aprefix),trim(aapar)
690          format(a,' parameter "',a,'"  is in second PEST control ',
     +       'file but not in first.')
             go to 9890
680          continue
           end if
         end do
       end do

C -- Log-transformation is undertaken as necessary.

       ies=0
       do i=1,npar2
         if(itrans2(i).lt.0) cycle
         ies=ies+1
         if(itrans2(i).eq.1)then
           if(pval2(i).le.0.0d0)then
             write(errmsg,695) trim(apar2(i)),trim(pestfile2)
695          format('Parameter "',a,'" cannot be log-transformed in ',
     +       'PEST control file "',a,
     +       '" as it has a non-positive value.')
             go to 9890
           end if
           dtemp=logfac*pval2(i)
           do iobs=1,nobs3
             x2(iobs,ies)=x2(iobs,ies)*dtemp
           end do
         end if
       end do

C -- The Jacobian matrix is written.

!#ifdef LAHEY
!#ifdef UNIX
!       open(unit=15,file=jcofile2,form='unformatted',
!C     + action='write',err=9400)
!     + err=9400)    ! caused a problem with Lahey lf95 unix
!#else
!       open(unit=15,file=jcofile2,form='unformatted',
!     + access='transparent',action='write',err=9400)
!#endif
!#else
!       open(unit=15,file=jcofile2,form='unformatted',
!C     + action='write',err=9400)    ! caused a problem with Lahey lf95 unix
!     + err=9400)
!#endif

       nxrow2=nobs2+nprior2
       write(15,err=9400) -nespar2,-nxrow2
       icount=0
       do i=1,nespar2
          do j=1,nobs2
            if(x2(j,i).ne.0.0d0) icount=icount+1
          end do
       end do
       if(nprior2.ne.0)then
         do i=1,nespar2
           do iprior=1,nprior2
             if(xp(iprior,i).ne.0.0d0) icount=icount+1
           end do
         end do
       end if
       write(15,err=9400) icount
       do i=1,nespar2
         do j=1,nobs2
           if(x2(j,i).ne.0.0d0)then
             icount=(i-1)*nxrow2+j
             write(15,err=9400)icount,x2(j,i)
           end if
         end do
       end do
       do i=1,nespar2
         do j=1,nprior2
           if(xp(j,i).ne.0.0d0)then
             icount=(i-1)*nxrow2+j+nobs2
             write(15,err=9400)icount,xp(j,i)
           end if
         end do
       end do
       do ipp=1,npar2
         if((itrans2(ipp).eq.0).or.(itrans2(ipp).eq.1))then
            write(15,err=9400) apar2(ipp)
         end if
       end do
       do iobs=1,nobs2
         write(15,err=9400) aobs2(iobs)
       end do
       do iprior=1,nprior2
         write(15,err=9400) aprior2(iprior)
       end do

! -- Observation values are added to the JCO file if possible.

       if(iextraflag.ne.0)then
C         if(idifflag.ne.0) go to 705
C         if(npar2.ne.npar1) go to 705
C         do ipp=1,npar1
C           if(itrans1(ipp).ne.itrans2(ipp)) go to 705
C         end do
         DO iprior=1,nprior2
           IFLAG=0
           RRTEMP=0.0
           IES=0
           DO IPP=1,NPAR2
             IF(ITRANS2(IPP).LT.0) cycle
             IES=IES+1
             TX=Xp(iprior,IES)
             IF(TX.EQ.0.0) cycle
             IF(ITRANS2(IPP).EQ.1) then
               if(pval2(ipp).le.0.0d0)then
                 write(errmsg,702) trim(apar2(ipp)),trim(pestfile2)
702              format('Parameter "',a,'" has non-positive value ',
     +           'but is log-transformed in PEST control file ',a,'.')
                 go to 9890
               end if
               RTEMP=LOG10(PVAL2(IPP))
             ELSE IF(ITRANS2(IPP).EQ.0) then
               RTEMP=PVAL2(IPP)
             end if
             RRTEMP=RRTEMP+TX*RTEMP
           end do
           REFprior2(Iprior)=RRTEMP
         end do
         do iobs=1,nobs2
           write(15,err=9400) refobs3(iobs),oval3(iobs)
         end do
         do iprior=1,nprior2
           write(15,err=9400) refprior2(iprior),prval2(iprior)
         end do

! -- Corresponding parameter values are added if necessary.

         if(iextraflag_1.ne.0)then
           if(idifflag.eq.0)then
             do i=1,npar2
               write(15,err=9400) pval2(i),itrans2(i),scale2(i),
     +         offset2(i)
             end do
           end if
         end if

       end if
705    continue

       close(unit=15)
       call addquote(jcofile2,afile)
       write(6,490) trim(afile)
490    format(' - file ',a,' written ok.')
       if(idifflag.ne.0)then
         write(6,495)
495      format(/,' Warning: at least one adjustable parameter from ',
     +   'second PEST control file has ',/,
     +   ' different value from that of parameter of same name in ',
     +   'first PEST control file.')
       end if

       go to 9900

9000   continue

       write(6,9010)
9010   format(/,' JCO2JCO is run using the command:',/)
       write(6,9020)
9020   format('     jco2jco casename1 casename2',/)
       write(6,9030)
9030   format(' where',/)
       write(6,9050)
9050   format('     casename1 pertains to a case for which a ',
     + 'JCO file exists, and')
       write(6,9060)
9060   format('     casename2 pertains to a case for which a ',
     + 'JCO file must be written.')
       go to 9999

9100   call addquote(pestfile1,afile)
       write(errmsg,9110) trim(afile)
9110   format('Error reading PEST control file ',a,'; check this ',
     + 'file with PESTCHEK.')
       go to 9890

9200   continue
       call addquote(jcofile1,afile)
       call addquote(pestfile1,bfile)
       write(errmsg,9210) trim(afile),trim(bfile)
9210   format('Jacobian matrix file ',a,' is incompatible with PEST ',
     + 'control file ',a,'.')
       go to 9890
9250   call addquote(jcofile1,afile)
       write(errmsg,9260) trim(afile)
9260   format('Error reading Jacobian matrix file ',a,'.')
       go to 9890
9300   call addquote(pestfile2,afile)
       write(errmsg,9110) trim(afile)
       go to 9890
9400   call addquote(jcofile2,afile)
       write(errmsg,9410) trim(afile)
9410   format('Cannot write to file ',a,'.')
       go to 9890

! -- The following are prior information-related errors.

9201   CALL WRITINT(atemp,ILINE)
       call addquote(pestfile2,afile)
       WRITE(ERRMSG,9211) TRIM(atemp),TRIM(AFILE)
9211   FORMAT('Error reading prior information from line ',A,
     + ' of prior information section of file ',A)
       GO TO 9890
9301   CALL WRITINT(atemp,ILINE)
       call addquote(pestfile2,afile)
       WRITE(ERRMSG,9311) TRIM(atemp),TRIM(AFILE)
9311   FORMAT('Incorrect parameter name or improper syntax in prior ',
     + 'information: line ',A,' of prior information section ',
     + 'of file ',A)
       GO TO 9890
9401   CALL WRITINT(atemp,ILINE)
       call addquote(pestfile2,afile)
       WRITE(ERRMSG,9411) TRIM(APAR2(IPAR)),TRIM(atemp),TRIM(AFILE)
9411   FORMAT('Parameter cited in prior information is fixed or ',
     + 'tied: parameter "',A,'" on line ',A,' of prior information ',
     + 'section of file ',A)
       GO TO 9890
9501   CALL WRITINT(atemp,ILINE)
       call addquote(pestfile2,afile)
       WRITE(ERRMSG,9511) TRIM(APAR2(IPAR)),TRIM(atemp),TRIM(AFILE)
9511   FORMAT('Log transformed parameter referenced as ',
     + 'untransformed in prior information: parameter "',A,
     + '" on line ',A,' of prior information section of file ',A)
       GO TO 9890
9601   CALL WRITINT(atemp,ILINE)
       call addquote(pestfile2,afile)
       WRITE(ERRMSG,9611) TRIM(APAR2(IPAR)),TRIM(atemp),TRIM(AFILE)
9611   FORMAT('Untransformed parameter referenced as ',
     + 'log-transformed in prior information: parameter "',A,
     + '" on line ',A,' of prior information section of file ',A)
       GO TO 9890

9890   call write_message(errmsg)
9900   continue

       deallocate (itrans1,itrans2,pval1,pval2,x1,x2,apar1,aobs1,
     + apar2,aobs2,x3,scale1,scale2,offset1,offset2,stat=ierr)
       deallocate (refobs1,oval1,xp,aprior2,prval2,
     + refobs3,oval3,refprior2,stat=ierr)

9999   continue
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

C -- Subroutine lowcas converts a string to lower case.

        integer i,j
        character*(*) astrng

        do 10 i=1,len_trim(astrng)
        j=ichar(astrng(i:i))
        if((j.ge.65).and.(j.le.90)) astrng(i:i)=char(j+32)
10      continue
        return
        end


        subroutine addquote(afile,aqfile)

C -- Subroutine ADDQUOTE adds quotes to a filename if it has a space in it.

        character*(*) afile,aqfile
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


        subroutine write_message(errmsg)

C -- Subroutine write_message formats and writes a message to the screen.

        implicit none

        integer  junit
        integer  jend,i,nblc,ierr,j
        character*(*) errmsg

        junit=6
        write(junit,*)

        j=0
        nblc=len_trim(errmsg)
5       jend=j+78
        if(jend.ge.nblc) go to 100
        do i=jend,j+1,-1
          if(errmsg(i:i).eq.' ') then
            write(junit,50,iostat=ierr) errmsg(j+1:i)
50          format(1x,a)
            j=i
            go to 5
          end if
        end do
        write(junit,50,iostat=ierr) errmsg(j+1:jend)
        j=jend
        go to 5
100     jend=nblc
        write(junit,50,iostat=ierr) errmsg(j+1:jend)
        write(junit,*)
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


        SUBROUTINE GETINT(IFAIL,BUF,N1,N2,NBLB,MRKDEL)

C -- SUBROUTINE GETINT GETS THE NEXT STORED INSTRUCTION FOR PROCESSING

        INTEGER N1,N2,NBLB,I,II
        INTEGER IFAIL
        CHARACTER MRKDEL
        CHARACTER*(*) BUF

        IFAIL=0
        IF(N2.GE.NBLB) THEN
          N1=0
          RETURN
        END IF
        DO 10 I=N2+1,NBLB
        IF((BUF(I:I).NE.' ').AND.(ICHAR(BUF(I:I)).NE.9)) GO TO 50
10      CONTINUE
        N1=0
        RETURN
50      N1=I
        IF(BUF(N1:N1).NE.MRKDEL)THEN
          I=INDEX(BUF(N1:NBLB),' ')
          II=INDEX(BUF(N1:NBLB),CHAR(9))
          IF((I.EQ.0).AND.(II.EQ.0))THEN
            I=0
          ELSE IF(I.EQ.0)THEN
            I=II
          ELSE IF(II.EQ.0) THEN
            I=I
          ELSE
            I=MIN(I,II)
          END IF
          IF(I.NE.0) THEN
            N2=N1+I-2
          ELSE
            N2=NBLB
          END IF
        ELSE
          IF(N1.EQ.NBLB)THEN
            IFAIL=1
            RETURN
          END IF
          I=INDEX(BUF(N1+1:NBLB),MRKDEL)
          IF(I.EQ.0) THEN
            IFAIL=1
            RETURN
          END IF
          N2=N1+I
        END IF

        RETURN
        END


       logical function equals(r1,r2)

       double precision,  intent(in)      :: r1,r2
       double precision                   :: rtemp

       rtemp=abs(7.0*spacing(r1))
       if(abs(r1-r2).lt.rtemp)then
         equals=.true.
       else
         equals=.false.
       end if

       end



        SUBROUTINE DREALRD(IFAIL,CLINE,RTEMP)

C -- Subroutine DREALRD reads a real number from a string.

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
