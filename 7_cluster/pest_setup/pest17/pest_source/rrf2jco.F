       program rrf2jco

C -- Program RRF2JCO builds a JCO file from an RRF file.

       use pestdata
       implicit none

       integer             :: ifail,ierr,n,n1,info,iline,i,kline
       integer             :: iline_end_header,icount,ncount
       integer             :: irestart,pfile
       integer             :: ies,jes,kes,ipar,iobs,jpar,jobs
       integer             :: ldvt,lwork
       integer             :: itemp1,itemp2
       integer             :: nbcount,ibcount
       integer             :: jpartype,iunit,junit,itype,vardim,covdim
       integer             :: itemp,found_mean,sameflag
       integer             :: incx,incy

       integer             :: lw(10),rw(10)

       real                :: rtime
       double precision    :: ddot,alpha,beta
       double precision    :: sum,dtemp1,dtemp2,sthresh,threshfrac,dtemp
       double precision    :: vt(1,1),parvar(1)

       character (len=1)   :: jobu,jobvt,uplo,trans
       character (len=5)   :: aext
       character (len=8)   :: aaext
       character (len=10)  :: atype,acount,aline,how,auf,amf
       character (len=11)  :: result1,result2
       character (len=12)  :: aversion,aapar
       character (len=20)  :: aaobs
       character (len=256) :: pestfile,rrffile,jcofile,afile
       character (len=256) :: uncertfile,meanfile

       double precision, allocatable   :: u(:,:),cp(:,:),s(:),work(:)
       double precision, allocatable   :: covmat(:,:)
       double precision, allocatable   :: extparmean(:),extobsmean(:)
       double precision, allocatable   :: pval_keep(:)
       character (len=12), allocatable :: adjapar(:)

C -- Initialization

#ifdef MKL
       write(6,*) ' Doing MKL stuff'
#endif
       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' RRF2JCO Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

! -- The command line is parsed.

!       call date_and_time(date=result1,time=result2)    !debug
!       write(6,*) trim(result1)                         !debug
!       write(6,*) trim(result2)                         !debug
       threshfrac=5.0d-7
       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,7,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,6,lw,rw,cline)
       if(ifail.eq.0)then
         call drealrd(ifail,cline(lw(6):rw(6)),threshfrac)
         if(ifail.ne.0) go to 8900
       end if
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       pestfile=cline(lw(1):rw(1))
       meanfile=cline(lw(2):rw(2))
       uncertfile=cline(lw(3):rw(3))
       rrffile=cline(lw(4):rw(4))
       jcofile=cline(lw(5):rw(5))
       call remchar(pestfile,char(211))
       call remchar(meanfile,char(211))
       call remchar(uncertfile,char(211))
       call remchar(rrffile,char(211))
       call remchar(jcofile,char(211))
       n=len_trim(pestfile)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=pestfile(n1:n)
       call lowcas(aext)
       if(aext.ne.'.pst')pestfile(n+1:)='.pst'
       n=len_trim(rrffile)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=rrffile(n1:n)
       call lowcas(aext)
       if(aext.ne.'.rrf')rrffile(n+1:)='.rrf'
       n=len_trim(jcofile)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=jcofile(n1:n)
       call lowcas(aext)
       if(aext.ne.'.jco')jcofile(n+1:)='.jco'

#ifndef UNIX
       call lowcas(pestfile)
       call lowcas(jcofile)
       call lowcas(rrffile)
       call lowcas(uncertfile)
       call lowcas(meanfile)
#endif

       if((threshfrac.lt.0.0d0).or.
     +    (threshfrac.ge.1.0d0))then
         write(errmsg,234)
234      format('If supplied, the singular value truncation ',
     +   'ratio must be less than 1 and non-negative.')
         go to 9890
       end if
       auf=uncertfile
       call lowcas(auf)
       amf=meanfile
       call lowcas(amf)
!       if(amf.ne.'nul')then
!         n=len_trim(meanfile)
!         n1=n-6
!         if(n1.lt.1)n1=1
!         aaext=meanfile(n1:n)
!         call lowcas(aaext)
!         if(aaext.ne.'.parobs')meanfile(n+1:)='.parobs'
!       end if

       call addquote(pestfile,afile)
       write(6,235) trim(afile)
235    format(' - reading PEST control file ',a,'...')
       irestart=0
       p1start=0
       pfile=0
       call read_pest_data(ifail,irestart,p1start,pfile,pestfile)
       if(ifail.ne.0) go to 9890
       if(maxcompdim.gt.1)then
         write(errmsg,240) trim(afile)
240      format('RRF2JCO requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
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
       write(6,275) trim(afile)
275    format(' - file ',a,' read ok.')

C -- We can delete some unused PEST memory.

       deallocate(parchglim,parlbnd,parubnd,
     + ipargp,scale,offset,dercom,owght,nobgnm,stat=ierr)

! -- If requested, we read the external Ckk covariance matrix.

       allocate(cp(nespar,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       jpartype=0

       if(auf.ne.'nul')then
         allocate(adjapar(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0) then
             ies=ies+1
             adjapar(ies)=apar(ipar)
           end if
         end do
         call addquote(uncertfile,afile)
         write(6,570)
570      format(/,' - reading parameter uncertainty file...')
         open(unit=12,file=uncertfile,action='read',status='old',
     +   iostat=ierr)
         if(ierr.ne.0)then
           write(errmsg,95) trim(afile)
95         format('Cannot open parameter uncertainty file ',a,'.')
           go to 9890
         end if
         iunit=12
         junit=15
         itype=2
         vardim=1
         covdim=nespar
         atype='parameter'
         call read_uncert_data_file(ifail,iunit,junit,itype,jpartype,
     +   nespar,vardim,covdim,parvar,cp,atype,uncertfile,errmsg,
     +   cline,adjapar)
         if(ifail.ne.0) then
           errmsg=adjustl(errmsg)
           go to 9890
         end if
         if((jpartype.ne.1).and.(jpartype.ne.2)) then
           write(errmsg,96)
96         format('Unexpected JPARTYPE value. Contact programmer.')
           go to 9890
         end if
         write(6,580) trim(afile)
580      format(' - file ',a,' read ok.')
       end if

C -- If requested we read the external mean parameter value file.

       if(amf.ne.'nul')then
         allocate(extparmean(npar),extobsmean(nobs),stat=ierr)
         if(ierr.ne.0) go to 9200
         extparmean=-1.1d301             ! an array
         extobsmean=-1.1d301             ! an array
         if(amf.eq.'pst') go to 589
         call addquote(meanfile,afile)
         write(6,581)
581      format(/,' - reading parobs mean value file...')
         open(unit=12,file=meanfile,action='read',status='old',
     +   iostat=ierr)
         if(ierr.ne.0)then
           write(errmsg,582) trim(afile)
582        format('Cannot open parobs mean value file ',a,'.')
           go to 9890
         end if
         read(12,'(a)',err=9420,end=9440) cline
         call lowcas(cline)
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9460
         if((cline(lw(1):rw(1)).ne.'single').and.
     +      (cline(lw(1):rw(1)).ne.'double')) go to 9460
         if((cline(lw(2):rw(2)).ne.'point').and.
     +      (cline(lw(2):rw(2)).ne.'nopoint')) go to 9460
         kline=1
         do ipar=1,npar
           kline=kline+1
           read(12,'(a)',err=9420,end=9440) cline
           call linspl(ifail,4,lw,rw,cline)
           if(ifail.ne.0) then
             call writint(aline,kline)
             write(errmsg,5823) trim(aline),trim(afile)
5823         format('Insufficient entries on line ',a,' of file ',a,'.')
             go to 9890
           end if
           aapar=cline(lw(1):rw(1))
           call lowcas(aapar)
           call which1(ifail,npar,jpar,apar,aapar)
           if(ifail.ne.0)then
             call writint(aline,kline)
             write(errmsg,5821) trim(aapar),trim(aline),trim(afile)
5821         format('Unknown parameter "',a,'" at line ',a,
     +       ' of file ',a,'.')
             go to 9890
           end if
           if(extparmean(jpar).gt.-1.0d300)then
             call writint(aline,kline)
             write(errmsg,5822) trim(aapar),trim(aline),trim(afile)
5822         format('Parameter name "',a,'" repeated at line ',a,
     +       ' of file ',a,'.')
             go to 9890
           end if
           call drealrd(ifail,cline(lw(2):rw(2)),extparmean(jpar))
           if(ifail.ne.0)then
             call writint(aline,kline)
             write(errmsg,583) trim(aline),trim(afile)
583          format('Cannot read parameter value from line ',a,
     +       ' of file ',a,'.')
             go to 9890
           end if
           if(itrans(jpar).eq.1)then
             if(extparmean(jpar).le.0.0d0)then
               write(errmsg,584) trim(aapar)
584            format('Parameter "',a,'" is log-transformed in PEST ',
     +         'control file. It must have a positive value in ',
     +         'the parobs mean value file.')
               go to 9890
             end if
             extparmean(jpar)=log10(extparmean(jpar))
           end if
         end do
         do iobs=1,nobs
           kline=kline+1
           read(12,'(a)',err=9420,end=9440) cline
           call linspl(ifail,2,lw,rw,cline)
           if(ifail.ne.0) then
             call writint(aline,kline)
             write(errmsg,5823) trim(aline),trim(afile)
             go to 9890
           end if
           aaobs=cline(lw(1):rw(1))
           call lowcas(aaobs)
           call which1(ifail,nobs,jobs,aobs,aaobs)
           if(ifail.ne.0)then
             call writint(aline,kline)
             write(errmsg,585) trim(aaobs),trim(aline),trim(afile)
585          format('Unknown observation "',a,'" at line ',a,
     +       ' of file ',a,'.')
             go to 9890
           end if
           if(extobsmean(jobs).gt.-1.0d300)then
             call writint(aline,kline)
             write(errmsg,5851) trim(aaobs),trim(aline),trim(afile)
5851         format('Observation name "',a,'" repeated at line ',a,
     +       ' of file ',a,'.')
             go to 9890
           end if
           call drealrd(ifail,cline(lw(2):rw(2)),extobsmean(jobs))
           if(ifail.ne.0)then
             call writint(aline,kline)
             write(errmsg,586) trim(aline),trim(afile)
586          format('Cannot read model output value from line ',a,
     +       ' of file ',a,'.')
             go to 9890
           end if
         end do
         close(unit=12)
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             ies=ies+1
             if(extparmean(ipar).lt.-1.0d300) then
               write(errmsg,587) trim(apar(ipar)),trim(afile)
587            format('At least one adjustable parameter (namely ',
     +         'parameter "',a,'") is not cited in file ',a,'.')
               go to 9890
             end if
           end if
         end do
         do iobs=1,nobs
           if(extobsmean(iobs).lt.-1.0d300)then
             write(errmsg,588) trim(aobs(iobs)),trim(afile)
588          format('At least one model output (namely ',
     +       'model output "',a,'") is not cited in file ',a,'.')
             go to 9890
           end if
         end do
         write(6,580) trim(afile)
       end if
589    continue
       if(amf.eq.'pst')then
         allocate(pval_keep(npar),stat=ierr)
         if(Ierr.ne.0) go to 9200
       end if

C -- We open the run results file.

       found_mean=0
       open(unit=10,file=rrffile,status='old',action='read',
     + iostat=ierr)
       if(ierr.ne.0)then
         call addquote(rrffile,afile)
         write(errmsg,280) trim(afile)
280      format('Cannot open run results file ',a,'.')
         go to 9890
       end if

! -- We now read the run results file.

       call addquote(rrffile,afile)
       write(6,*)
       write(6,630)
630    format(' - reading run results file...')
       iline=1
       read(10,'(a)',err=9000,end=9020) cline
       call lowcas(cline)
       if(cline.ne.'* case dimensions')then
         write(errmsg,640) trim(afile)
640      format('Line 1 of file ',a,' expected to be ',
     +   '"* case dimensions".')
         go to 9890
       end if
       iline=iline+1
       read(10,'(a)',err=9000,end=9020) cline
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(1):rw(1)),itemp1)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(2):rw(2)),itemp2)
       if(ifail.ne.0) go to 9000
       if((itemp1.ne.npar).or.(itemp2.ne.nobs)) go to 9060
       iline=iline+1
       read(10,'(a)',err=9000,end=9020) cline
       do ipar=1,npar
         iline=iline+1
         read(10,'(a)',err=9000,end=9020) cline
         if(cline.eq.' ') go to 9000
         call linspl(ifail,1,lw,rw,cline)
         aapar=cline(lw(1):rw(1))
         call lowcas(aapar)
         if(aapar.ne.apar(ipar)) go to 9061
       end do
       iline=iline+1
       read(10,'(a)',err=9000,end=9020) cline
       do iobs=1,nobs
         iline=iline+1
         read(10,'(a)',err=9000,end=9020) cline
         if(cline.eq.' ') go to 9000
         call linspl(ifail,1,lw,rw,cline)
         aaobs=cline(lw(1):rw(1))
         call lowcas(aaobs)
         if(aaobs.ne.aobs(iobs)) go to 9062
       end do
       iline_end_header=iline

! -- Run results are read a first time to compute mean parameter and model output values.
!    (Or to find these values if amf is "rrf".)

       ncount=0
       nbcount=0
       if(amf.eq.'pst')then
         pval_keep=pval ! arrays
       end if
       pval=0.0d0       ! an array
       oval=0.0d0       ! an array
       do
         iline=iline+1
         read(10,'(a)',err=9000,end=800) cline
         if(cline.ne.'* parameter set index')then
           call writint(aline,iline)
           write(errmsg,650) trim(aline),trim(afile)
650        format('"* parameter set index" expected at line ',a,
     +     ' of file ',a,'.')
           go to 9890
         end if
         do i=1,3
           iline=iline+1
           read(10,'(a)',err=9000,end=800) cline
         end do
         iline=iline+1
         read(10,'(a)',err=9000,end=800) cline
         if(cline.ne.'* parameter values')then
           call writint(aline,iline)
           write(errmsg,655)trim(aline),trim(afile)
655        format('Line ',a,' of file ',a,' should be ',
     +     '"* parameter values".')
           go to 9890
         end if
         do ipar=1,npar
           iline=iline+1
           read(10,'(a)',err=9000,end=800) cline
           if(cline.eq.' ') go to 9000
           call linspl(ifail,1,lw,rw,cline)
           call drealrd(ifail,cline(lw(1):rw(1)),pval_o(ipar))
           if(ifail.ne.0) go to 9000
           if(itrans(ipar).eq.1)then
             if(pval_o(ipar).le.0.0d0)then
               call writint(aline,iline)
               write(errmsg,656) trim(apar(ipar)),trim(aline),
     +         trim(afile)
656            format('Parameter "',a,'" is log transformed in PEST ',
     +         'control file. However it has a non-positive value at ',
     +         'line ',a,' of run results file ',a,'.')
               go to 9890
             end if
           end if
         end do
         sameflag=0
         if(amf.eq.'pst')then
           do ipar=1,npar
             if(abs(pval_keep(ipar)-pval_o(ipar)).gt.
     +          (abs(pval_keep(ipar))+abs(pval_o(ipar)))*1.0d-4)
     +          go to 657
           end do
           sameflag=1
           do ipar=1,npar
             if(itrans(ipar).le.0)then
               extparmean(ipar)=pval_o(ipar)
             else
               extparmean(ipar)=log10(pval_o(ipar))
             end if
           end do
           found_mean=1
657        continue
         end if
         iline=iline+1
         read(10,'(a)',err=9000,end=800) cline
         if(cline.ne.'* model output values')then
           call writint(aline,iline)
           write(errmsg,660)trim(aline),trim(afile)
660        format('Line ',a,' of file ',a,' should be ',
     +     '"* model output values".')
           go to 9890
         end if
         do iobs=1,nobs
           iline=iline+1
           read(10,'(a)',err=9000,end=800) cline
           if(cline.eq.' ') go to 9000
           call linspl(ifail,1,lw,rw,cline)
           call drealrd(ifail,cline(lw(1):rw(1)),modval_o(iobs))
           if(ifail.ne.0) go to 9000
         end do
         do iobs=1,nobs
           if(modval_o(iobs).lt.-1.0d35)then
             nbcount=nbcount+1
             go to 661
           end if
         end do
         if(sameflag.eq.1)then
           do iobs=1,nobs
             extobsmean(iobs)=modval_o(iobs)
           end do
         end if
         ncount=ncount+1
!         write(6,*) ' ncount = ',ncount   !debug
         do ipar=1,npar
           if(itrans(ipar).eq.1)then
             pval(ipar)=pval(ipar)+log10(pval_o(ipar))
           else if(itrans(ipar).eq.0)then
             pval(ipar)=pval(ipar)+pval_o(ipar)
           end if
         end do
         do iobs=1,nobs
           oval(iobs)=oval(iobs)+modval_o(iobs)
         end do
661      continue
       end do

800    continue
       if(ncount.eq.0)then
         write(errmsg,810) trim(afile)
810      format('No useable parameter/observation sets were found in ',
     +   'file ',a,'.')
         go to 9890
       else if(ncount.eq.1)then
         write(errmsg,820) trim(afile)
820      format('Only one useable parameter/observation set was ',
     +   'found in file ',a,'.')
         go to 9890
       end if
       if(amf.eq.'pst')then
         if(found_mean.eq.0)then
           write(6,*)
           write(6,821)
821        format('   Warning: a set of parameter values corresponding',
     +     ' to those in the')
           write(6,822)
822        format('   PEST control file could not be found.')
           write(6,823)
823        format('   Parameter and observation means will be computed ',
     +     'as RRF-file averages.',/)
           amf='nul'
         else
           do iobs=1,nobs
             if(extobsmean(iobs).lt.-1.0d35)then
               write(6,*)
               write(6,824)
824            format('   Warning: model outputs in rrf file ',
     +         'corresponding to')
               write(6,825)
825            format('   parameters in PEST control file indicate ',
     +         'failed model run.',/)
               write(6,823)
               amf='nul'
               go to 826
             end if
           end do
826        continue
         end if
       end if
       if(amf.ne.'nul')then
         do ipar=1,npar
           pval(ipar)=extparmean(ipar)
         end do
         do iobs=1,nobs
           oval(iobs)=extobsmean(iobs)
         end do
       else
         pval=pval/ncount                ! an array
         oval=oval/ncount                ! an array
       end if
       rewind(unit=10)
       do iline=1,iline_end_header
         read(10,*)
       end do

! -- We now calculate the Chk covariance matrix and (optionally) the Ckk covariance matrix.

       allocate(covmat(nobs,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       covmat=0.0d0   ! a matrix
       if(jpartype.eq.0) cp=0.0d0       ! a matrix
       ibcount=0
       do icount=1,ncount
!         write(6,*) ' icount = ',icount   !debug
829      continue
         iline=iline+1
         read(10,'(a)',err=9100,end=9100) cline
         do i=1,3
           iline=iline+1
           read(10,'(a)',err=9100,end=9100) cline
         end do
         iline=iline+1
         read(10,'(a)',err=9100,end=9100) cline
         do ipar=1,npar
           iline=iline+1
           read(10,*,err=9100,end=9100) pval_o(ipar)
         end do
         iline=iline+1
         read(10,'(a)',err=9100,end=9100) cline
         do iobs=1,nobs
           iline=iline+1
           read(10,*,err=9100,end=9100) modval_o(iobs)
         end do
         if(ibcount.lt.nbcount)then
           do iobs=1,nobs
             if(modval_o(iobs).lt.-1.0d35)then
               ibcount=ibcount+1
               go to 829
             end if
           end do
         end if
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             if(itrans(ipar).eq.1)then
               dtemp2=log10(pval_o(ipar))-pval(ipar)
             else
               dtemp2=pval_o(ipar)-pval(ipar)
             end if
             ies=ies+1
             do iobs=1,nobs
               dtemp1=modval_o(iobs)-oval(iobs)
               covmat(iobs,ies)=covmat(iobs,ies)+dtemp1*dtemp2
             end do
             if(jpartype.eq.0)then
               jes=0
               do jpar=1,npar
                 if(itrans(jpar).ge.0)then
                   jes=jes+1
                   if(jes.le.ies)then
                     if(itrans(jpar).eq.1)then
                       dtemp1=log10(pval_o(jpar))-pval(jpar)
                     else
                       dtemp1=pval_o(jpar)-pval(jpar)
                     end if
                     cp(jes,ies)=cp(jes,ies)+dtemp2*dtemp1
                   end if
                 end if
               end do
             end if
           end if
         end do
       end do
       if(jpartype.eq.0)then
         do ies=1,nespar-1
           do jes=ies+1,nespar
             cp(jes,ies)=cp(ies,jes)
           end do
         end do
       end if
       if(amf.ne.'nul')then
         covmat=covmat/ncount                       ! a matrix
         if(jpartype.eq.0) cp=cp/ncount             ! a matrix
       else
         covmat=covmat/(ncount-1)                   ! a matrix
         if(jpartype.eq.0) cp=cp/(ncount-1)         ! a matrix
       end if
       close(unit=10)
       if(nbcount.eq.0)then
         call writint(acount,ncount)
         write(6,830) trim(acount),trim(afile)
830      format(' - ',a,' sets of run results read from file ',a,'.')
       else
         call writint(acount,ncount)
         write(6,832) trim(acount),trim(afile)
832      format(' - ',a,' sets of usable results read from ',
     +   'file ',a,'.')
         call writint(acount,nbcount)
         write(6,831) trim(acount)
831      format(' - ',a,' sets of unusable results also read ',
     +   'from this file.')
       end if

!       open(unit=25,file='debug.dat')                       !debug
!        do iobs=1,nobs                                      !debug
!         write(25,840) (covmat(iobs,ies),ies=1,nespar)      !debug
!840      format(8(1x,1pg14.7))                              !debug
!       end do                                               !debug
!       close(unit=25)                                       !debug

C -- The parameter covariance matrix is inverted. We do this using SVD because it allows us
C    to accomodate a singular matrix.

       write(6,*)
       write(6,590)
590    format(' - inverting prior covariance matrix ...')
       if(jpartype.eq.1)then
         do ies=1,nespar
           if(cp(ies,ies).le.0.0d0)then
             write(errmsg,600)
600          format('At least one diagonal element of the ',
     +       'user-supplied covariance matrix is non-positive.')
             go to 9890
           end if
           cp(ies,ies)=1.0d0/cp(ies,ies)
         end do
       else
         write(6,591)
591      format(' - singular value decomposition ...')
         write(6,*) ' just before dgesvd call...'              !debug
         call cpu_time(rtime)               !debug
         write(6,*) ' time = ',rtime        !debug
         call date_and_time(date=result1,time=result2)    !debug
         write(6,*) trim(result1)                         !debug
         write(6,*) trim(result2)                         !debug
         jobu='A'
         jobvt='N'
         ldvt=1
         lwork=8*nespar
         allocate(u(nespar,nespar),s(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         allocate(work(lwork),stat=ierr)
         if(ierr.ne.0) go to 9200
         call dgesvd(jobu,jobvt,nespar,nespar,cp,nespar,s,u,
     +   nespar,vt,ldvt,work,lwork,info)
         if(info.ne.0)then
           write(errmsg,610)
610        format('Singular value decomposition failure when ',
     +     'implemented on covariance matrix.')
           go to 9890
         end if
         write(6,*) ' just after dgesvd call...'                  !debug
         call cpu_time(rtime)               !debug
         write(6,*) ' time = ',rtime        !debug
         call date_and_time(date=result1,time=result2)    !debug
         write(6,*) trim(result1)                         !debug
         write(6,*) trim(result2)                         !debug

!         open(unit=80,file='debug.dat')   !debug
         sthresh=s(1)*threshfrac
         do ies=1,nespar
!           write(80,*) s(ies)        !debug
           if(amf.eq.'nul')then
             itemp=ncount-1
           else
             itemp=ncount
           end if
           if(ies.le.itemp)then
             if(s(ies).lt.0.0d0)then
               write(errmsg,620)
620            format('At least one singular value of covariance ',
     +         'matrix has been returned less than zero.')
               go to 9890
             else if(s(ies).gt.sthresh)then
               s(ies)=1.0d0/s(ies)
             else
               s(ies)=0.0d0
             end if
           else
             s(ies)=0.0d0
           end if
         end do
         write(6,*) ' location a'                 !debug
         call cpu_time(rtime)               !debug
         write(6,*) ' time = ',rtime        !debug
         call date_and_time(date=result1,time=result2)    !debug
         write(6,*) trim(result1)                         !debug
         write(6,*) trim(result2)                         !debug

         write(6,592)
592      format(' - matrix reconstruction ...')
         do ies=1,nespar
           s(ies)=sqrt(s(ies))
         end do
         do ies=1,nespar
           dtemp=s(ies)
           do jes=1,nespar
             u(jes,ies)=u(jes,ies)*dtemp
           end do
         end do
#ifdef MKL
         write(6,*) ' Doing MKL stuff'
         alpha=1.0d0
         beta=0.0d0
         uplo='u'
         trans='n'
         call dsyrk(uplo,trans,nespar,nespar,alpha,u,nespar,beta,cp,
     +   nespar)
         do ies=1,nespar-1
           do jes=ies+1,nespar
             cp(jes,ies)=cp(ies,jes)
           end do
         end do
#else
         do ies=1,nespar
           do jes=1,ies
             sum=0.0d0
             do kes=1,nespar
               sum=sum+u(ies,kes)*u(jes,kes)
             end do
             cp(jes,ies)=sum
             cp(ies,jes)=sum
           end do
         end do
#endif
       end if

       write(6,*) ' location b'           !debug
       call cpu_time(rtime)               !debug
       write(6,*) ' time = ',rtime        !debug
       call date_and_time(date=result1,time=result2)    !debug
       write(6,*) trim(result1)                         !debug
       write(6,*) trim(result2)                         !debug

!       open(unit=80,file='debug.mat')                !debug
!       write(80,*) nespar,nespar,1                   !debug
!       do ies=1,nespar                               !debug
!         write(80,621) (cp(ies,jes),jes=1,nespar)    !debug
!621      format(8(1x,1pg14.7))                       !debug
!       end do                                        !debug
!       write(80,622)                                 !debug
!622    format('* row and column names')              !debug
!       do ipar=1,npar                                !debug
!         if(itrans(ipar).ge.0)then                   !debug
!           write(80,'(a)') trim(apar(ipar))          !debug
!         end if                                      !debug
!       end do                                        !debug
!       close(unit=80)                                !debug

! -- We now form the new Jacobian matrix.

       write(6,593)
593    format(' - forming new Jacobian matrix ...')
       call cpu_time(rtime)                             !debug
       write(6,*) ' time = ',rtime                      !debug
       call date_and_time(date=result1,time=result2)    !debug
       write(6,*) trim(result1)                         !debug
       write(6,*) trim(result2)                         !debug
       do iobs=1,nobs    ! nobs - not nxrow - so as not to destroy prior information
         do ies=1,nespar
           x(iobs,ies)=0.0d0
         end do
       end do
       if(jpartype.eq.1)then
         do iobs=1,nobs
           do ies=1,nespar
             x(iobs,ies)=covmat(iobs,ies)*cp(ies,ies)
           end do
         end do
       else
#ifdef MKL
         write(6,*) ' Doing MKL stuff'
         alpha=1.0d0
         beta=0.0d0
         call dgemm('N','N',nobs,nespar,nespar,alpha,covmat,nobs,
     +   cp,nespar,beta,x,nxrow)
#else
         do iobs=1,nobs
           do ies=1,nespar
             sum=0.0d0
             do jes=1,nespar
               sum=sum+covmat(iobs,jes)*cp(jes,ies)
             end do
             x(iobs,ies)=sum
           end do
         end do
#endif
       end if

       write(6,*) ' location c'                         !debug
       call cpu_time(rtime)                             !debug
       write(6,*) ' time = ',rtime                      !debug
       call date_and_time(date=result1,time=result2)    !debug
       write(6,*) trim(result1)                         !debug
       write(6,*) trim(result2)                         !debug

!       open(unit=25,file='debug1.dat')                      !debug
!       do iobs=1,nxrow                                      !debug
!         write(25,840) (x(iobs,ies),ies=1,nespar)           !debug
!       end do                                               !debug
!       close(unit=25)                                       !debug

! -- We now record the Jacobian matrix.

       call addquote(jcofile,afile)
       write(6,*)
       write(6,850)
850    format(' - writing Jacobian matrix file...')
       how='write'
       call open_unformatted_file(ifail,20,how,jcofile,errmsg)
       if(ifail.ne.0) go to 9890
       write(20,err=9400) -nespar,-nxrow
       icount=0
       do ies=1,nespar
          do iobs=1,nxrow
            if(x(iobs,ies).ne.0.0d0) icount=icount+1
          end do
       end do
       write(20,err=9400) icount
       do ies=1,nespar
         do iobs=1,nxrow
           if(x(iobs,ies).ne.0.0d0)then
             icount=(ies-1)*nxrow+iobs
             write(20,err=9400)icount,x(iobs,ies)
           end if
         end do
       end do
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
            write(20,err=9400) apar(ipar)
         end if
       end do
       do iobs=1,nxrow
         write(20,err=9400) aobs(iobs)
       end do
       close(unit=20)
       write(6,860) trim(afile)
860    format(' - file ',a,' written ok.')

       go to 9990

8900   continue
       write(6,8910)
8910   format(/,' RRF2JCO is run using the command:',/)
       write(6,8920)
8920   format('     rrf2jco pestfile mfile/nul/pst ufile/nul rrffile ',
     + 'jcofile [sthresh]',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     pestfile      is an existing PEST control file,')
       write(6,8941)
8941   format('     mfile/nul/pst is a parobs mean ',
     + 'value file or "nul" or "pst",')
       write(6,8945)
8945   format('     ufile/nul     is an existing parameter uncert ',
     + 'file or "nul",')
       write(6,8950)
8950   format('     rrffile       is an existing PEST_HP run results ',
     + 'file,')
       write(6,8970)
8970   format('     jcofile       is a JCO file which RRF2JCO must ',
     + 'write, and')
       write(6,8980)
8980   format('     sthresh       is the singular value truncation ',
     + 'ratio [<<1.0]')

       go to 9999

9000   call writint(aline,iline)
       write(errmsg,9010) trim(aline),trim(afile)
9010   format('Error reading line ',a,' of file ',a,'.')
       go to 9890

9020   write(errmsg,9030) trim(afile)
9030   format('Unexpected end encountered to file ',a,' while ',
     + 'reading header to this file.')
       go to 9890

9060   write(errmsg,9070)
9070   format('Run results file is not compatible with PEST control ',
     + 'file.')
       go to 9890

9061   call writint(aline,iline)
       write(errmsg,9071) trim(aline),trim(afile)
9071   format('Run results file is not compatible with PEST control ',
     + 'file - parameter mismatch at line ',a,' of file ',a,'.')
       go to 9890

9062   call writint(aline,iline)
       write(errmsg,9072) trim(aline),trim(afile)
9072   format('Run results file is not compatible with PEST control ',
     + 'file - observation mismatch at line ',a,' of file ',a,'.')
       go to 9890

9100   call writint(aline,iline)
       write(errmsg,9110) trim(aline),trim(afile)
9110   format('Error re-reading line ',a,' of file ',a,'.')
       go to 9890


9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9400   write(errmsg,9410) trim(afile)
9410   format('Cannot write to file ',a,'.')
       go to 9890

9420   call writint(aline,kline)
       write(errmsg,9430) trim(aline),trim(afile)
9430   format('Error reading line ',a,' of file ',a,'.')
       go to 9890

9440   write(errmsg,9450) trim(afile)
9450   format('Premature end encountered to file ',a,'.')
       go to 9890

9460   write(errmsg,9470) trim(afile)
9470   format('The header to file ',a,' does not identify it as a ',
     + 'parobs file.')
       go to 9890

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)

9990   continue
       deallocate(u,cp,covmat,s,work,adjapar,stat=ierr)
       deallocate(extparmean,extobsmean,stat=ierr)
       deallocate(pval_keep,stat=ierr)
       call pest_data_deallocate(ifail)

9999   continue
       end



        SUBROUTINE NUMRD(IFAIL,ITYPE,LW,RW,IVAL,RVAL,CLINE)

C -- SUBROUTINE NUMRD READS A NUMBER FROM PART OF A STRING

        INTEGER IFAIL,RW,LW,LN,IVAL
        INTEGER ITYPE
        DOUBLE PRECISION RVAL
        CHARACTER*(*) CLINE
        CHARACTER*10 FMT

        IFAIL=0
        LN=RW-LW+1
        IF(ITYPE.EQ.0) THEN
          WRITE(FMT,10) LN
10        FORMAT('(I',I2,')')
          READ(CLINE(LW:RW),FMT,ERR=50) IVAL
        ELSE IF(ITYPE.EQ.1) THEN
          WRITE(FMT,20) LN
20        FORMAT('(F',I2,'.0)')
          READ(CLINE(LW:RW),FMT,ERR=50) RVAL
        END IF
        RETURN

50      IFAIL=1
        RETURN

        END


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


        SUBROUTINE UPCAS(ASTRNG)

C -- SUBROUTINE UPCAS CONVERTS A STRING TO UPPER CASE

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,LEN_TRIM(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.97).AND.(J.LE.122)) ASTRNG(I:I)=CHAR(J-32)
10      CONTINUE
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


