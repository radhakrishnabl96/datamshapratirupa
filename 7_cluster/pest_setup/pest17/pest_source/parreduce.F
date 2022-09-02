       program parreduce

! --   Program PARREDUCE tests whether parameters can be eliminated without cost to fitting data.

       implicit none
       logical equals
       integer j,k,nespar1,itemp1,itemp2,bn,ies,irow,jes,
     +         n,ierr,i,npar1,nobs1,npargp1,nprior1,icount,n1,
     +         npar2,nobs2,npargp2,nprior2,nespar2,iobs,kes,
     +         ifail,nobs3,ii,jj,idifflag,jjj
       integer itemp
       integer iprior,ipar,nobsgp1
       integer nrow,ncol,nnrow,lwork,info,nsing,icol
       integer iwarn_p,iwarn_c,nobsgp2
       integer lda,ldu,ldvt
       integer iscale
       integer lw(5),rw(5)
       integer, allocatable :: itrans1(:),itrans2(:)
       double precision dtemp,dtemp1,logfac,ilogfac,ratio,ratio1,
     + ratio2,dd1,dd2,scalefac,rstemp1,rstemp2,dtemp2
       double precision ww,eigthresh,dtemp3
       double precision, allocatable :: pval1(:),pval2(:),scale1(:),
     + scale2(:),offset1(:),offset2(:)
       double precision, allocatable :: x1(:,:),x2(:,:),x3(:,:)
       double precision, allocatable :: weight1(:),weight2(:)
       double precision, allocatable :: crzp(:),crz(:),crx(:)
       double precision, allocatable :: vt(:,:),a(:,:),u(:,:),s(:),
     + work(:),uut(:,:)
       double precision, allocatable :: lbound1(:),ubound1(:)
       character*10 how
       character*10 anum
       character*12 aversion
       character*12 aapar,aspar1,aspar2
       character*15 atemp,atemp1,atemp2,aext
       character*20 aaobs
       character*40 aprefix
       character*12, allocatable :: apar1(:),apar2(:)
       character*20, allocatable :: aobs1(:),aobs2(:)
       character*200 pestfile1,pestfile2,jcofile1,afile,bfile
       character*200 outfile1
       character*400 cline,errmsg

! -- Initialisation

       logfac=log(10.0d0)
       ilogfac=1.0d0/logfac
       idifflag=0
       iwarn_p=0
       iwarn_c=0

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(' PARREDUCE Version ',a,
     + '. Watermark Numerical Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

! -- The command line is parsed.

       iscale=0
       if(cline.eq.' ') go to 9000
       call spacesub(cline)
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.eq.0)then
         if((cline(lw(5):rw(5)).eq.'/b').or.
     +      (cline(lw(5):rw(5)).eq.'/B'))then
            iscale=1
            cline(lw(5):rw(5))=' '
         else
            go to 9000
         end if
       end if
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.eq.0) go to 9000
       call linspl(ifail,4,lw,rw,cline)
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
       n=len_trim(pestfile2)
       pestfile2(n+1:)='.pst'
       call drealread(ifail,cline(lw(3):rw(3)),eigthresh)
       if(ifail.ne.0) go to 9000
       if(eigthresh.le.0.0)then
         write(errmsg,9)
9        format('EIGTHRESH must be greater than zero in PARREDUCE ',
     +   'argument list.')
         go to 9890
       end if
       if(eigthresh.ge.1.0)then
         write(errmsg,11)
11       format('EIGTHRESH must be less than 1.0 in PARREDUCE argument ',
     +   'list.')
         go to 9890
       end if
       outfile1=cline(lw(4):rw(4))
       call remchar(outfile1,char(211))

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
         read(10,*,err=9100,end=9100)
       end do
       read(10,*,err=9100,end=9100) npar1,nobs1,npargp1,nprior1
       allocate(apar1(npar1),pval1(npar1),itrans1(npar1),
     + aobs1(nobs1),scale1(npar1),offset1(npar1),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,125)
125      format('Cannot allocate sufficient memory to continue ',
     +   'PARREDUCE execution.')
         go to 9890
       end if
       allocate(weight1(nobs1),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,125)
         go to 9890
       end if
       if(nprior1.ne.0) iwarn_p=1
       allocate(lbound1(npar1),ubound1(npar1),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,125)
         go to 9890
       end if
       do
         read(10,'(a)',err=9100,end=130) cline
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
         read(10,*,err=9100,end=9100) apar1(i),atemp,atemp1,pval1(i),
     +   lbound1(i),ubound1(i),atemp1,scale1(i),offset1(i)
         call lowcas(apar1(i))
         call lowcas(atemp)
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
           read(10,*,err=9100,end=9100) atemp1,atemp2
           call lowcas(atemp1)
           call lowcas(atemp2)
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
         call lowcas(cline)
         if(index(cline,'* observation g').ne.0) go to 1400
         if(index(cline,'* observation data').ne.0) go to 220
       end do
1400   continue
       nobsgp1=0
       do
         read(10,'(a)',end=200) cline
         if(index(cline,'* observation data').ne.0) go to 220
         nobsgp1=nobsgp1+1
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.eq.0) iwarn_c=1
       end do
200    call addquote(pestfile1,afile)
       write(errmsg,210) trim(afile)
210    format('Cannot find "observation data" section of file ',a,'.')
       go to 9890
220    do i=1,nobs1
         read(10,*,err=9100,end=9100) aobs1(i),dtemp,weight1(i)
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
       write(6,*)
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
         if(aaobs.ne.aobs1(i)) go to 9200
       end do

       if(iscale.ne.0)then
         call addquote(pestfile1,afile)
         ies=0
         do ipar=1,npar1
           if(itrans1(ipar).lt.0) cycle
           ies=ies+1
           dtemp1=ubound1(ipar)
           dtemp=lbound1(ipar)
           if(itrans1(ipar).eq.1)then
             if((dtemp1.le.0.0).or.(dtemp.le.0.0)) go to 9700
             dtemp1=log10(dtemp1)
             dtemp=log10(dtemp)
           end if
           dtemp=(dtemp1-dtemp)*0.25             ! Assumes that bounds mark 4 standard deviations.
           if(dtemp.le.0.0) go to 9700
           do i=1,nobs1
             x1(i,ies)=x1(i,ies)*dtemp
           end do
         end do
       end if

       close(unit=15)
       call addquote(jcofile1,afile)
       write(6,230) trim(afile)

! -- The second PEST control file is read.

! -- Data is read from the second PEST control file.

       open(unit=10,file=pestfile2,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(pestfile2,afile)
         write(errmsg,110) trim(afile)
         call write_message(errmsg)
         stop
       end if

       do i=1,3
         read(10,*,err=9300,end=9300)
       end do
       read(10,*,err=9300,end=9300) npar2,nobs2,npargp2,nprior2
       allocate(apar2(npar2),pval2(npar2),itrans2(npar2),
     + aobs2(nobs2),scale2(npar2),offset2(npar2),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,125)
         go to 9890
       end if
       allocate(weight2(nobs2),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,125)
         go to 9890
       end if
       if(nprior2.ne.0) iwarn_p=1

       do
         read(10,'(a)',err=9300,end=330) cline
         if(index(cline,'* parameter data').ne.0) go to 350
       end do
330    continue
       call addquote(pestfile2,afile)
       write(errmsg,140) trim(afile)
       go to 9890
350    do i=1,npar2
         read(10,*,err=9300,end=9300) apar2(i),atemp,atemp1,pval2(i),
     +   dtemp,dtemp,atemp1,scale2(i),offset2(i)
         call lowcas(apar2(i))
         call lowcas(atemp)
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
           read(10,*,err=9300,end=9300) atemp1,atemp2
           call lowcas(atemp1)
           call lowcas(atemp2)
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

       nobsgp2=-999
       do
         read(10,'(a)',err=9300,end=400) cline
         call lowcas(cline)
         if(index(cline,'* observation g').ne.0) go to 1401
         if(index(cline,'* observation data').ne.0) go to 420
       end do
1401   continue
       nobsgp2=0
       do
         read(10,'(a)',end=400) cline
         if(index(cline,'* observation data').ne.0) go to 420
         nobsgp2=nobsgp2+1
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.eq.0) iwarn_c=1
       end do
400    call addquote(pestfile2,afile)
       write(errmsg,210) trim(afile)
       go to 9890
420    do i=1,nobs2
         read(10,*,err=9300,end=9300) aobs2(i),dtemp,weight2(i)
         call lowcas(aobs2(i))
       end do

       nespar2=0
       do ipar=1,npar2
         if(itrans2(ipar).ge.0) nespar2=nespar2+1
       end do
       nprior2=0

       close(unit=10)
       call addquote(pestfile2,afile)
       write(6,230) trim(afile)

       aprefix='Cannot continue with calculations: '

C -- Dimensions of the second derivatives matrix are evaluated.

       if(nobs1.ne.nobs2)then
         write(errmsg,451)
451      format('The two PEST control files have different numbers ',
     +   'of observations.')
         go to 9890
       end if

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

       do iobs=1,nobs1
         if(aobs1(iobs).ne.aobs2(iobs))then
           write(errmsg,453)
453        format('The two PEST control files must cite the same ',
     +     'observations in the same order.')
           go to 9890
         end if
         if(.not.equals(weight1(iobs),weight2(iobs)))then
           write(errmsg,454) trim(aobs1(iobs))
454        format('Weights supplied for observations of the same ',
     +     'name in the two PEST control files must be the same. ',
     +     'Violation occurs for observation "',a,'".')
           go to 9890
         end if
       end do
       x3=x1        ! arrays

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
         aspar1=apar1(i)
         do j=1,npar2
           if(apar2(j).eq.aspar1)then
             if(.not.equals(scale1(i),scale2(j)))then
               write(errmsg,461) trim(aspar1)
461            format('Parameter "',a,'" has a different SCALE ',
     +         'in the two PEST control files.')
               go to 9890
             end if
             if(.not.equals(pval1(i),pval2(j)))then
               write(errmsg,462) trim(aspar1)
462            format('Parameter "',a,'" has a different value ',
     +         'in the two PEST control files.')
               go to 9890
             end if
           end if
         end do
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
         if(itrans2(i).eq.1)then
           if(itrans1(jj).eq.0)then
             write(errmsg,433) trim(aprefix),trim(aapar)
433          format(a,' parameter "',a,'" is log transformed in ',
     +       'second PEST control file but not in first.')
             go to 9890
           end if
         else if(itrans2(i).eq.0)then
           if(itrans1(jj).eq.1)then
             write(errmsg,432) trim(aprefix),trim(aapar)
432          format(a,' parameter "',a,'" is log-transformed in ',
     +       'first PEST control file but not in second.')
             go to 9890
           end if
         end if
         dd2=pval2(i)
         dd1=pval1(jj)
         if(.not.equals(offset1(jj),offset2(i)))then
           write(errmsg,443) trim(aprefix),trim(apar1(jj))
443        format(a,' parameter "',a,'" has different offset ',
     +    'in second PEST control file to that in first.')
          go to 9890
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
         do iobs=1,nobs3
           x2(iobs,ies)=x3(iobs,kes)
         end do
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

C -- Residual variance calculations are done.

       do iobs=1,nobs2
         ww=weight2(iobs)
         do ies=1,nespar2
           x2(iobs,ies)=x2(iobs,ies)*ww
         end do
         do ies=1,nespar1
           x3(iobs,ies)=x3(iobs,ies)*ww
         end do
       end do

C -- First we calculate the innate variability of the residuals.

       allocate(crzp(nobs2),crz(nobs2),crx(nobs2),stat=ierr)
       if(ierr.ne.0) then
         write(errmsg,125)
         go to 9890
       end if
       allocate(uut(nobs2,nobs2),stat=ierr)
       if(ierr.ne.0) then
         write(errmsg,125)
         go to 9890
       end if
       do iobs=1,nobs1
         dtemp=0.0d0
         do k=1,nespar1
           dtemp=dtemp+x3(iobs,k)*x3(iobs,k)
         end do
         crzp(iobs)=dtemp
       end do

C -- We undertake SVD on Z.

       nrow=nobs3
       ncol=nespar1
       nnrow=min(ncol,nrow)
       lwork=2*max(3*min(nrow,ncol)+max(nrow,ncol),5*min(nrow,ncol))
       allocate(a(nrow,ncol),u(nrow,nrow),s(nnrow),
     + vt(ncol,ncol),work(lwork),stat=ierr)
       if(ierr.ne.0) then
         write(errmsg,125)
         go to 9890
       end if
       do icol=1,ncol
         do irow=1,nrow
           a(irow,icol)=x3(irow,icol)
         end do
       end do
       lda=nrow
       ldu=nrow
       ldvt=ncol

C -- Now undertake SVD.

       write(6,1120)
1120   format(/,' - undertaking SVD on Z...')

       call dgesvd('A','A',nrow,ncol,a,lda,s,u,ldu,vt,ldvt,
     + work,lwork,info)
       if(info.ne.0)then
         write(errmsg,1130)
1130     format('SVD was not successful.')
         go to 9890
       end if

C -- Calculate normalized residuals.

       dtemp=s(1)*s(1)
       do i=2,min(nrow,ncol)
         dtemp1=s(i)*s(i)
         if(dtemp1/dtemp.lt.eigthresh) then
           nsing=i-1
           go to 1131
         end if
       end do
       nsing=min(nrow,ncol)
1131   continue
       call writint(anum,nsing)
       write(6,1132) trim(anum)
1132   format(' - number of singular values used = ',a)

       if(nsing.lt.min(nrow,ncol))then
         do irow=1,nrow
           dtemp=0.0d0
           do k=nsing+1,min(nrow,ncol)
             dtemp=dtemp+u(irow,k)*u(irow,k)*s(k)*s(k)
           end do
           crz(irow)=dtemp
         end do
       else
         crz=0.0   ! an array
       end if

C -- We now undertake SVD on the X matrix.

       if(nespar2.eq.nespar1)then             ! There is no parameter simplification
         crx=crz                              ! arrays
         go to 1305
       end if

       ncol=nespar2
       nrow=nobs2
       do icol=1,ncol
         do irow=1,nrow
           a(irow,icol)=x2(irow,icol)
         end do
       end do

       write(6,1220)
1220   format(/,' - undertaking SVD on X...')

       call dgesvd('A','A',nrow,ncol,a,lda,s,u,ldu,vt,ldvt,
     + work,lwork,info)
       if(info.ne.0)then
         write(errmsg,1130)
         go to 9890
       end if
       dtemp=s(1)*s(1)
       do i=2,min(nrow,ncol)
         dtemp1=s(i)*s(i)
         if(dtemp1/dtemp.lt.eigthresh) then
           nsing=i-1
           go to 1231
         end if
       end do
       nsing=min(nrow,ncol)
1231   continue
       call writint(anum,nsing)
       write(6,1132) trim(anum)

       do icol=1,nobs2
         do irow=1,icol
           dtemp=0.0d0
           do k=1,nsing
             dtemp=dtemp+u(irow,k)*u(icol,k)
           end do
           uut(irow,icol)=-dtemp
         end do
       end do
       do icol=1,nobs2-1
         do irow=icol+1,nobs2
           uut(irow,icol)=uut(icol,irow)
         end do
       end do
       do icol=1,nobs2
         uut(icol,icol)=uut(icol,icol)+1.0d0
       end do
       do icol=1,nespar1
         do irow=1,nobs2
           dtemp=0
           do k=1,nobs2
             dtemp=dtemp+uut(irow,k)*x3(k,icol)
           end do
           x1(irow,icol)=dtemp
         end do
       end do
       do irow=1,nobs2
         dtemp=0.0d0
         do k=1,nespar1
           dtemp=dtemp+x1(irow,k)*x1(irow,k)
         end do
         crx(irow)=dtemp
       end do

! -- Output files is written.

1305   continue
       open(unit=20,file=outfile1)
       write(20,1310)
1310   format(' Residual standard devations ',
     + '(no measurement noise assumed)')
       write(20,*)
       write(20,1320)
1320   format('Observation',t25,'No_calibration',t50,
     +'Calibration_with_Z',t75,'Calibration_with_X')
       dtemp1=0.0d0
       dtemp2=0.0d0
       dtemp3=0.0d0
       do iobs=1,nobs2
         write(20,1330) trim(aobs2(iobs)),sqrt(crzp(iobs)),
     +   sqrt(crz(iobs)),sqrt(crx(iobs))
1330     format(2x,a,t25,2x,1pg14.7,t50,2x,1pg14.7,t75,2x,1pg14.7)
         dtemp1=dtemp1+crzp(iobs)*crzp(iobs)
         dtemp2=dtemp2+crz(iobs)*crz(iobs)
         dtemp3=dtemp3+crx(iobs)*crx(iobs)
       end do
       write(20,*)
       write(20,1330)'sum_of_squares',dtemp1,dtemp2,dtemp3
       close(unit=20)
       write(6,*)
       write(6,740) trim(outfile1)
740    format(' - file ',a,' written ok.')

       if(iwarn_p.ne.0)then
         write(6,1350)
1350     format(/,'   Warning: PARREDUCE ignores all prior ',
     +   'information and includes all ')
         write(6,1360)
1360     format('   observations in calculating variability of ',
     +   'residuals.')
       end if
       if(iwarn_c.ne.0)then
         write(6,1370)
1370     format(/,'   Warning: PARREDUCE ignores observation ',
     +   'covariance matrices. It uses ')
         write(6,1380)
1380     format('   observation weights instead.')
       end if

       go to 9900

9000   continue

       write(6,9010)
9010   format(/,' PARREDUCE is run using the command:',/)
       write(6,9020)
9020   format('     parreduce pestfile1 pestfile2 eigthresh ',
     + 'outfile [/b]',/)
       write(6,9030)
9030   format(' where',/)
       write(6,9050)
9050   format('     pestfile1 is a PEST control file for which a ',
     + 'JCO file exists,')
       write(6,9060)
9060   format('     pestfile2 is a PEST control file in which some ',
     + 'parameters are tied/fixed,')
       write(6,9065)
9065   format('     eigthresh is the eigenvalue truncation ',
     + 'threshold,')
       write(6,9070)
9070   format('     outfile   is the name of the PARREDUCE output ',
     + 'file, and')
       write(6,9080)
9080   format('     "/b"      activates optional parameter bounds ',
     +'scaling.')
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
9700   write(errmsg,9710) trim(afile)
9710   format('Parameter bounds error in file ',a,'. Check with ',
     + 'PESTCHEK.')
       go to 9890

9890   call write_message(errmsg)
9900   continue

       deallocate (itrans1,itrans2,pval1,pval2,x1,x2,apar1,aobs1,
     + apar2,aobs2,x3,scale1,scale2,offset1,offset2,stat=ierr)
       deallocate(weight1,weight2,stat=ierr)
       deallocate(crzp,crz,crx,uut,stat=ierr)
       deallocate(vt,a,u,s,work,stat=ierr)
       deallocate(lbound1,ubound1,stat=ierr)

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

