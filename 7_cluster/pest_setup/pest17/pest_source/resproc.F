C -- Note: the LSQR subroutine is included in this file. It is slightly modified from
C          the LSQR subroutine which PEST uses.


        module lsqr_data_exchange
          integer                       :: nparx
          double precision, allocatable :: tbx(:,:),eigmat(:,:)
        end module lsqr_data_exchange


       program resproc

C -- Program RESPROC is a PEST post-processor which calculates the resolution
C    and parameter solution matrices.

       use lsqr_data_exchange
       implicit none


       logical :: lexist,lopened
       integer :: svdmode,svda,ifail,n,n1,ierr,iline,ireg,npar,nobs,
     +            npargp,nprior,nobsgp,nxrow,ies,ipar,nespar,iobs,
     +            nespar1,nxrow1,nobsgp1,icountregobs,icountregnz,
     +            ireg1,svdmode1,svda1,numlam,numeiglst,imcovflag,
     +            irow,bnpar,bnespar,bnxrow,izfile,i,j,jes,blwork,
     +            info,izcompat,itemp1,itemp2,icount,lwork,k,numnonreg,
     +            nesdim,bbnxrow,jj,nscaladj,iflagwarn
       integer :: bnrow,bncol,bicode,ipp,jpp,ii,icol,ntemp,
     +            svda_extsuper
       integer :: nuseobs,irr
       integer :: lsqrmode,lsqrmode1,lamoption,iflag,jacfile,iistop,
     +            iitn,ndim1,ndim3,itemp,jjcount
       integer :: iiw(1)

       double precision :: rlambda1,lamlst,alphlst,phim,phir,dtemp,
     +                     rtemp,rtemp1,rtemp2,sftemp
       double precision :: dtemp1,dtemp2,anorm,acond,rnorm,arnorm,
     +                     xnorm,rrtemp

       double precision :: rrw(1),se(1)

       character*5   :: aext
       character*5   :: aoption
       character*10  :: aline,arow
       character*12  :: aversion
       character*12  :: atemp,atemp1,atemp2,aapar
       character*20  :: aaobs
       character*200 :: casename, outfile,pestfile,basepestfile,
     +                  basejacfile,resfile,jcofile,zfile,
     +                  tempbasejacfile
       character*200 :: afile,bfile,cfile,dfile,efile,sfile
       character*300 :: cline
       character*600 :: amessage


C -- Non-allocatable arrays

       integer            :: lw(10),rw(10)
       integer            :: icode(10)
       double precision   :: u(1,1),vt(1,1)
       double precision   :: rcode(10)
       character*200      :: acode(10)

C -- Allocatable arrays

       integer, allocatable          :: itrans(:),irgp(:),nobgnm(:),
     +                                  bitrans(:)
       integer, allocatable          :: jindex(:)
       double precision, allocatable :: x(:,:),owght(:),bowght(:),
     +                                  blhs(:,:),bwork(:),bsss(:),
     +                                  bx(:,:),work(:),sss(:),sf(:)
       double precision, allocatable :: mat1(:,:),mat2(:,:),mat3(:,:),
     +                                  res(:,:),g(:,:)
       double precision, allocatable :: bb(:),vv(:),ww(:),xx(:),rr(:)
       double precision, allocatable :: eigmat1(:,:),xtx(:,:),mat4(:,:)

       character*12, allocatable :: apar(:),obgnme(:),bapar(:)
       character*20, allocatable ::  aobs(:)

       external bprod


#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' RESPROC Version ',a,'. Watermark Numerical ',
     + 'Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

C -- Initialisation

       svdmode=0
       svda=0
       iline=0
       ireg=0
       nscaladj=0
       iflagwarn=0
       tempbasejacfile=' '

! -- The command line is parsed.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       casename=cline(lw(1):rw(1))
       call remchar(casename,char(211))
       n=len_trim(casename)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=casename(n1:)
       call lowcas(aext)
       if(aext.eq.'.pst ')casename(n1:n)=' '
#ifndef UNIX
       call lowcas(casename)
#endif

       outfile=cline(lw(2):rw(2))
       call remchar(outfile,char(211))
       call addquote(outfile,dfile)
       call open_unformatted_file(ifail,20,'write',outfile,amessage)
       if(ifail.ne.0) go to 9800

       lamoption=1
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.eq.0)then
         aoption=cline(lw(3):rw(3))
         call lowcas(aoption)
         if(aoption.eq.'/nl')then
           lamoption=0
         else if(aoption.eq.'/l')then
           lamoption=1
         else
           go to 8900
         end if
       end if

! -- The PEST control file is partially read.

        aext='.pst '
        pestfile=trim(casename)//trim(aext)
        call addquote(pestfile,afile)
        write(6,60) trim(afile)
60      format(/,' - reading PEST control file ',a,'...')
        open(unit=10,file=pestfile,status='old',iostat=ierr)
        if(ierr.ne.0)then
          write(amessage,50) trim(afile)
50        format(' Cannot open PEST control file ',a,'.')
          go to 9800
        end if
        iline=0

        iline=iline+1
        read(10,'(a)',err=9000,end=9050) cline
        call lowcas(cline)
        cline=adjustl(cline)
        if(cline(1:4).ne.'pcf ')then
          write(amessage,80) trim(afile)
80        format(' First line of PEST control file ',a,
     +    ' should be "pcf".')
          go to 9800
        end if

        iline=iline+1
        read(10,'(a)',err=9000,end=9050) cline
        call lowcas(cline)
        cline=adjustl(cline)
        if((cline(1:1).ne.'*').or.(index(cline,'control').eq.0))then
          write(amessage,90) trim(afile)
90        format(' Second line of PEST control file ',a,
     +    ' should be "* control data".')
          go to 9800
        end if

        iline=iline+1
        read(10,'(a)',err=9000,end=9050) cline
        call lowcas(cline)
        call linspl(ifail,2,lw,rw,cline)
        if(ifail.ne.0) go to 9100
        atemp=cline(lw(2):rw(2))
        call lowcas(atemp)
        if(index(atemp,'regul').ne.0) ireg=1

        iline=iline+1
        read(10,'(a)',err=9000,end=9050) cline
        call linspl(ifail,5,lw,rw,cline)
        if(ifail.ne.0) go to 9100
        atemp=cline(lw(1):rw(1))
        call intread(ifail,atemp,npar)
        if(ifail.ne.0) go to 9000
        atemp=cline(lw(2):rw(2))
        call intread(ifail,atemp,nobs)
        if(ifail.ne.0) go to 9000
        atemp=cline(lw(3):rw(3))
        call intread(ifail,atemp,npargp)
        if(ifail.ne.0) go to 9000
        atemp=cline(lw(4):rw(4))
        call intread(ifail,atemp,nprior)
        if(ifail.ne.0) go to 9000
        atemp=cline(lw(5):rw(5))
        call intread(ifail,atemp,nobsgp)
        if(ifail.ne.0) go to 9000

        if(npar.le.0)then
          call writint(aline,iline)
          write(amessage,110) trim(aline),trim(afile)
110       format(' NPAR must be greater than zero at line ',a,
     +    ' of PEST control file ',a,'.')
          go to 9800
        end if
        if(nobs.le.0)then
          call writint(aline,iline)
          write(amessage,120) trim(aline),trim(afile)
120       format(' NOBS must be greater than zero at line ',a,
     +    ' of PEST control file ',a,'.')
          go to 9800
        end if
        if(nobsgp.le.0)then
          call writint(aline,iline)
          write(amessage,125) trim(aline),trim(afile)
125       format(' NOBSGP must be greater than zero at line ',a,
     +    ' of PEST control file ',a,'.')
          go to 9800
        end if
        nparx=npar

        jacfile=0
        iline=iline+1
        read(10,'(a)',err=9000,end=9050) cline
        call linspl(ifail,6,lw,rw,cline)
        if(ifail.eq.0)then
          atemp=cline(lw(6):rw(6))
          call intread(ifail,atemp,jacfile)
          if(ifail.ne.0) go to 9000
          if((jacfile.lt.0).or.(jacfile.gt.2)) go to 9000
        end if

C -- Space is allocated for some arrays.

        nxrow=nobs+nprior
        allocate(apar(npar),itrans(npar),stat=ierr)
        if(ierr.ne.0) go to 9150
        allocate(aobs(nxrow),nobgnm(nxrow),owght(nxrow),stat=ierr)
        if(ierr.ne.0) go to 9150
        allocate(obgnme(nobsgp),irgp(nobsgp),stat=ierr)
        if(ierr.ne.0) go to 9150


C -- SVD and SVDA data is read.
C -- LSQR data is also read.

        do
          iline=iline+1
          read(10,'(a)',err=9000,end=150) cline
          call lowcas(cline)
          cline=adjustl(cline)
          if(cline(1:1).ne.'*') cycle
          if(index(cline,'singular').ne.0) then
145         iline=iline+1
            read(10,'(a)',err=9000,end=9050) cline
            if(cline.eq.' ') go to 145
            if(cline(1:1).eq.'#') go to 145
            call linspl(ifail,1,lw,rw,cline)
            call intread(ifail,cline(lw(1):rw(1)),svdmode)
            if(ifail.ne.0)then
              call writint(aline,iline)
              write(amessage,146) trim(aline),trim(afile)
146           format(' Cannot read value for SVDMODE variable from ',
     +        'line ',a,' of file ',a,'.')
              go to 9800
            end if
            cycle
          end if
          if(index(cline,'lsqr').ne.0) then
141         iline=iline+1
            read(10,'(a)',err=9000,end=9050) cline
            if(cline.eq.' ') go to 141
            if(cline(1:1).eq.'#') go to 141
            call linspl(ifail,1,lw,rw,cline)
            call intread(ifail,cline(lw(1):rw(1)),lsqrmode)
            if(ifail.ne.0)then
              call writint(aline,iline)
              write(amessage,149) trim(aline),trim(afile)
149           format(' Cannot read value for LSQRMODE variable from ',
     +        'line ',a,' of file ',a,'.')
              go to 9800
            end if
            cycle
          end if
          if(lsqrmode.ne.0)then
            write(amessage,142)
142         format(' LSQRMODE in PEST control file is set to 1; ',
     +      'resolution data cannot be computed with regularisation ',
     +      'implemented using LSQR.')
            go to 9800
          end if

          if(index(cline,'assist').ne.0)then
            svda=1
c            if(jacfile.ne.0)then
c              write(amessage,159)
c159           format(' As presently programmed RESPROC cannot ',
c     +        'handle the case where SVD-assist is operative ',
c     +        'and base parameter derivatives are supplied ',
c     +        'externally by the model.')
c              go to 9800
c            end if
147         iline=iline+1
            read(10,'(a)',err=9000,end=9050) cline
            if((cline.eq.' ').or.(cline(1:1).eq.'#')) go to 147
            call spacesub(cline)
            call linspl(ifail,1,lw,rw,cline)
            basepestfile=cline(lw(1):rw(1))
            call remchar(basepestfile,char(211))
#ifndef UNIX
            call lowcas(basepestfile)
#endif
            call addquote(basepestfile,sfile)
148         iline=iline+1
            read(10,'(a)',err=9000,end=9050) cline
            if((cline.eq.' ').or.(cline(1:1).eq.'#')) go to 148
            call spacesub(cline)
            call linspl(ifail,1,lw,rw,cline)
            basejacfile=cline(lw(1):rw(1))
            call remchar(basejacfile,char(211))
#ifndef UNIX
            call lowcas(basejacfile)
#endif
            cycle
          end if
          if(index(cline,'parameter da').ne.0) go to 170
          cycle
150       write(amessage,160) trim(afile)
160       format(' Unexpected end encountered to file ',a,' while ',
     +    'looking for "parameter data" section.')
          go to 9800
        end do

170     continue
C        if((ireg.eq.0).and.(svdmode.eq.0).and.(svda.eq.0))then
C          write(amessage,172) trim(afile)
C172       format(' In PEST control file ',a,' PEST is not asked to ',
C     +    'run in "regularisation" mode. Nor is SVD activated. Nor ',
C     +    'is SVD-assist actived. As no form of regularisation is ',
C     +    'thus undertaken, postprocessing with RESPROC is ',
C     +    'inappropriate.')
C          go to 9800
C        end if

        ies=0
        do ipar=1,npar
          iline=iline+1
          read(10,'(a)',err=9000,end=9050) cline
          call linspl(ifail,9,lw,rw,cline)
          if(ifail.ne.0) go to 9100
          atemp1=cline(lw(1):rw(1))
          atemp2=cline(lw(2):rw(2))
          call lowcas(atemp1)
          call lowcas(atemp2)
          if(atemp1(1:1).eq.'*') go to 9300
          apar(ipar)=atemp1
          if(atemp1(1:3).eq.'iw_')then
            write(amessage,173) trim(afile)
173         format(' PEST control file ',a,' uses "iw" parameters; ',
     +      'functionality does not yet exist to write a resolution ',
     +      'matrix under these conditions.')
            go to 9800
          end if
          if(atemp2.eq.'log')then
            itrans(ipar)=1
            ies=ies+1
          else if(atemp2.eq.'none')then
            itrans(ipar)=0
            ies=ies+1
          else if(atemp2.eq.'fixed')then
            itrans(ipar)=-1000000
          else if(atemp2.eq.'tied')then
            itrans(ipar)=-1
          else
            go to 9300
          end if
          if(itrans(ipar).lt.0)then
            if(svda.ne.0)then
              write(amessage,174) trim(afile)
174           format(' If PEST was run for SVD-assisted inversion ',
     +        'then no parameter can be tied or fixed for RESPROC ',
     +        'to operate properly; this is trangressed in PEST ',
     +        'control file ',a,'.')
              go to 9800
            end if
          end if
        end do
        nespar=ies

C -- Space is allocated for more arrays.

        allocate (x(nxrow,nespar),stat=ierr)
        if(ierr.ne.0) go to 9150

C -- Observation group names are now read.

        do
          iline=iline+1
          read(10,'(a)',err=9000,end=210) cline
          call lowcas(cline)
          cline=adjustl(cline)
          if(cline(1:1).ne.'*') cycle
          if(index(cline,'observation gr').ne.0) go to 220
          cycle
210       write(amessage,215) trim(afile)
215       format(' Unexpected end encountered to PEST control file ',
     +    a,' while looking for "observation groups" section.')
          go to 9800
        end do
220     continue
        do i=1,nobsgp
          iline=iline+1
          read(10,'(a)',err=9000,end=9050) cline
          if(cline.eq.' ')go to 9300
          call linspl(ifail,2,lw,rw,cline)
          obgnme(i)=cline(lw(1):rw(1))
          call lowcas(obgnme(i))
          if(ifail.eq.0)then
            if(ireg.eq.0)then
              write(amessage,222) trim(afile)
              go to 9800
            else
              if(obgnme(i)(1:5).ne.'regul')then
                write(amessage,222) trim(afile)
222             format(' PEST control file ',a,' cites a covariance',
     +          ' matrix for observations and/or prior information ',
     +          'that is not used for regularisation purposes. This ',
     +          'is not allowed for present version of RESPROC.')
                go to 9800
              end if
            end if
          end if
        end do

! -- Observation names are now read.

        do
          iline=iline+1
          read(10,'(a)',err=9000,end=250) cline
          call lowcas(cline)
          cline=adjustl(cline)
          if(cline(1:1).ne.'*') cycle
          if(index(cline,'observation da').ne.0) go to 270
          cycle
250       write(amessage,260) trim(afile)
260       format(' Unexpected end encountered to PEST control file ',
     +    a,' while looking for "observation data" section.')
          go to 9800
        end do
270     continue
        do iobs=1,nobs
          iline=iline+1
          read(10,'(a)',err=9000,end=9050) cline
          if(cline.eq.' ') go to 9300
          call linspl(ifail,1,lw,rw,cline)
          aobs(iobs)=cline(lw(1):rw(1))
          call lowcas(aobs(iobs))
        end do
290     continue
        if(nprior.ne.0)then
          do
            iline=iline+1
            read(10,'(a)',err=9000,end=300) cline
            call lowcas(cline)
            cline=adjustl(cline)
            if(cline(1:1).ne.'*') cycle
            if(index(cline,'prior inf').ne.0) go to 320
            cycle
300         write(amessage,310) trim(afile)
310         format(' Unexpected end encountered to PEST control file ',
     +      a,' while looking for "prior information" section.')
            go to 9800
          end do
320       continue
          iobs=nobs
          do
            iline=iline+1
            read(10,'(a)',err=9000,end=9050) cline
            if(cline.eq.' ')cycle
            call linspl(ifail,1,lw,rw,cline)
            atemp=cline(lw(1):rw(1))
            if(atemp.eq.'&')cycle
            iobs=iobs+1
            aobs(iobs)=atemp
            call lowcas(aobs(iobs))
            if(iobs.eq.nxrow) go to 350
          end do
        end if
350     continue

        close(unit=10)
        write(6,360) trim(afile)
360     format(' - PEST control file ',a,' read ok.')

C -- The "rsd" file written by PEST is now read.

        aext='.rsd '
        resfile=trim(casename)//trim(aext)
        call addquote(resfile,bfile)
        write(6,380) trim(bfile)
380     format(/,' - reading resolution data file ',a,'...')
        call open_unformatted_file(ifail,10,'read',resfile,amessage)
        if(ifail.ne.0) go to 9800

        read(10,err=9200,end=9200) nespar1,nxrow1,nobsgp1
        if((nespar1.ne.nespar).or.(nxrow1.ne.nxrow).or.
     +     (nobsgp1.ne.nobsgp))then
           write(amessage,390) trim(bfile),
     +     trim(afile)
390        format(' Resolution data file ',a,' appears to be ',
     +     'incompatible with PEST control file ',a,'.')
           go to 9800
        end if

        read(10,err=9200,end=9200) icountregobs,icountregnz
cjd        if(icountregnz.ne.0)then
cjd          write(amessage,430) trim(bfile)
cjd430       format(' According to file ',a,' previous PEST run was ',
cjd     +    'in regularisation mode. Some regularisation observations ',
cjd     +    'or regularisation prior information equations have ',
cjd     +    'non-zero values; a resolution matrix cannot be built ',
cjd     +    'under these conditions.')
cjd          go to 9800
cjd        end if

        read(10,err=9200,end=9200) ireg1,svdmode1,svda1,lsqrmode1
        if(ireg1.ne.ireg)then
          write(amessage,400) trim(afile),trim(bfile)
400       format(' PESTMODE setting in PEST control file ',a,
     +    ' differs from that recorded in resolution data file ',a,'.')
          go to 9800
        end if
        if(svdmode1.ne.svdmode)then
          write(amessage,410) trim(afile),trim(bfile)
410       format(' SVDMODE setting in PEST control file ',a,
     +    ' differs from that recorded in resolution data file ',a,'.')
          go to 9800
        end if
        if(svda1.ne.svda)then
          write(amessage,420) trim(afile),trim(bfile)
420       format(' SVD-assist setting in PEST control file ',a,
     +    ' differs from that recorded in resolution data file ',a,'.')
          go to 9800
        end if
        if(lsqrmode1.ne.lsqrmode)then
          write(amessage,426) trim(afile),trim(bfile)
426       format(' LSQRMODE setting in PEST control file ',a,
     +    ' differs from that recorded in resolution data file ',a,'.')
          go to 9800
        end if

        read(10,err=9200,end=9200) numlam,rlambda1,lamlst,alphlst,
     +  numeiglst
cnl        alphlst=0.0d0
cnl        rlambda1=0.0d0
        if(lamoption.eq.0)then
          lamlst=0.0d0
          alphlst=0.0d0
          rlambda1=0.0d0
        end if

        read(10,err=9200,end=9200) imcovflag

        read(10,err=9200,end=9200) phim, phir

C -- The (rotated) weight matrix is read.

        read(10,err=9200,end=9200) (owght(i),i=1,nxrow)

C -- The observation group indicator matrix is read.

        read(10,err=9200,end=9200) (nobgnm(i),i=1,nxrow)

C -- The observation group regularisation indicator array is read.

        read(10,err=9200,end=9200) (irgp(i),i=1,nobsgp)

C -- If PEST ran in SVD assist mode some further information is read.

        if(svda.ne.0)then
          read(10,err=9200,end=9200) bnpar,bnespar,bnxrow,nscaladj,
     +    svda_extsuper
          allocate(bowght(bnxrow),bapar(bnpar),bitrans(bnpar),sf(bnpar),
     +    stat=ierr)
          if(ierr.ne.0) go to 9150
          read(10,err=9200,end=9200) (bitrans(i),i=1,bnpar)
          read(10,err=9200,end=9200) (bowght(i),i=1,bnxrow)
          read(10,err=9200,end=9200) (sf(i),i=1,bnpar)
          do ipar=1,bnpar
            read(10,err=9200,end=9200) bapar(ipar)
          end do
          read(10,err=9200,end=9200) tempbasejacfile
        end if
        close(unit=10)
        write(6,422) trim(bfile)
422     format(' - resolution data file ',a,' read ok.')

        if(svda.ne.0)then
          allocate(blhs(bnespar,npar),stat=ierr)
          if(ierr.ne.0) go to 9150
          nuseobs=0
          do irow=1,bnxrow
            if(bowght(irow).gt.0.0d0) nuseobs=nuseobs+1
          end do
        end if

C -- The Jacobian matrix if now read from the JCO file.

        aext='.jco '
        jcofile=trim(casename)//'.jco'
        call addquote(jcofile,cfile)
        write(6,425) trim(cfile)
425     format(/,' - reading Jacobian matrix file ',a)
        call open_unformatted_file(ifail,10,'read',jcofile,amessage)
        if(ifail.ne.0) go to 9800
        read(10,err=9350,end=9350)itemp1,itemp2
        if(itemp1.ge.0)then
          write(amessage,460) trim(cfile)
460       format(' Jacobian matrix file ',a,' uses old format; ',
     +    'use JCOTRANS utility to translate it to new format.')
          go to 9800
        end if
        if(abs(itemp1).ne.nespar) go to 9400
        if(abs(itemp2).ne.nxrow) go to 9400
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
        ies=0
        do ipar=1,npar
          if(itrans(ipar).ge.0)then
            read(10,err=9350,end=9350) aapar
            call lowcas(aapar)
            if(aapar.ne.apar(ipar)) go to 9400
          end if
        end do
        do iobs=1,nxrow
          read(10,err=9350,end=9350) aaobs
          call lowcas(aaobs)
          if(aaobs.ne.aobs(iobs)) go to 9400
        end do
        close(unit=10)
        write(6,465) trim(cfile)
465     format(' - Jacobian matrix file ',a,' read ok.')

! -- If PEST ran in SVD assist mode, the base Jacobian matrix file is read.

C -- Note that provision is made for the fact that the JCO file may have been the
C    product of a PRE-SVDA sensitivity run that was done using SVD and in which
C    prior information was omitted. JCO2JCO can use a JCO file produced in this way
C    to produce a JCO file which can then be used by the base parameter file, even
C    if prior information is then added to this base parameter file. RESPROC
C    must be able to accomodate this sitation too.

        if(svda.ne.0)then
          if(svda_extsuper.ne.1)then
            call addquote(basejacfile,cfile)
            write(6,370) trim(cfile)
370         format(/,' - reading base Jacobian matrix file ',a,'...')
            call open_unformatted_file(ifail,10,'read',basejacfile,
     +      amessage)
            if(ifail.ne.0) go to 9800
            read(10,err=9350,end=9350)itemp1,itemp2
            if(itemp1.ge.0)then
              write(amessage,460) trim(cfile)
              go to 9800
            end if
            if(abs(itemp1).ne.bnespar) then
              write(amessage,375) trim(cfile)
375           format(' Number and names of base parameters cited in ',
     +        'base Jacobian matrix file ',a,' is not in accord with ',
     +        'that used in previous PEST run.')
              go to 9800
            end if
            if(jacfile.ne.0)then
              if(abs(itemp2).ne.bnxrow) then
                write(amessage,378) trim(cfile)
378             format(' Number of observations and/or prior ',
     +          'information equations cited in ',
     +          'Jacobian matrix file ',a,' is not in accord with ',
     +          'that used in previous PEST run.')
                go to 9800
              end if
            end if
            bbnxrow=abs(itemp2)
            allocate(bx(bbnxrow,bnespar),stat=ierr)
            if(ierr.ne.0) go to 9150
            do i=1,bnespar
              do j=1,bbnxrow
                bx(j,i)=0.0d0
              end do
            end do
            read(10,err=9350,end=9350)icount
            do i=1,icount
              read(10,err=9350,end=9350) j,dtemp
              ies=(j-1)/bbnxrow+1
              irow=j-(ies-1)*bbnxrow
              bx(irow,ies)=dtemp
            end do
            ies=0
            do ipar=1,bnpar
              if((bitrans(ipar).ge.0).or.(bitrans(ipar).eq.-1000001)
     +                               .or.(bitrans(ipar).eq.-1000002))
     +                               then
                read(10,err=9350,end=9350) aapar
                call lowcas(aapar)
                if(aapar.ne.bapar(ipar))then
                  write(amessage,375) trim(cfile)
                  go to 9800
                end if
              end if
c              if((bitrans(ipar).eq.-1000001).or.
C     +           (bitrans(ipar).eq.-1000002))then
c              write(amessage,379) trim(bapar(ipar))
c379           format(' Parameter "',a,'" hit its upper or lower ',
c     +        'bound on the ',
c     +        'previous SVD-assisted parameter estimation run. ',
c     +        'Unfortunately, this invalidates calculation of the ',
c     +        'resolution matrix. Think about using PREDVAR1 based ',
c     +        'on sensitivities calculated on the basis of optimised ',
c     +        'parameter values.')
c              go to 9800
c              end if
            end do
            izcompat=0
            iflag=0
            if(nxrow.ge.bbnxrow)then
              do iobs=1,bbnxrow
                read(10,err=9350,end=9350) aaobs
                call lowcas(aaobs)
                if(aaobs.ne.aobs(iobs)) iflag=1
              end do
              if(iflag.eq.1) go to 458
              if(bbnxrow.lt.nxrow)then
                do irow=bbnxrow+1,nxrow
                  jj=nobgnm(irow)
                  if(irgp(jj).eq.0) iflag=1
                end do
              end if
              if(iflag.eq.1) go to 458
              izcompat=1
            else
              do iobs=1,bbnxrow
                read(10,err=9350,end=9350) aaobs
              end do
            end if
458         continue
            if(abs(svda_extsuper).eq.2)then
              itemp=max(nuseobs,npar)
              allocate(bb(itemp),stat=ierr)
              if(ierr.ne.0) go to 9150
              irr=0
              do iobs=1,bbnxrow
                read(10,err=9350,end=9350) dtemp1,dtemp2
                if(bowght(iobs).gt.0.0d0)then
                  irr=irr+1
                  bb(irr)=-(dtemp1-dtemp2)*sqrt(bowght(iobs))
                end if
              end do
            end if
            close(unit=10)
            write(6,381) trim(cfile)
381         format(' - base Jacobian matrix file ',a,' read ok.')
          else
            call addquote(basejacfile,cfile)
            open(unit=10,file=basejacfile,status='old',iostat=ierr)
            if(ierr.ne.0)then
              write(amessage,1210) trim(cfile),trim(afile)
1210          format(' Cannot open super parameter definition ',
     +        'file ',a,' cited in PEST control file ',a,'.')
              go to 9800
            end if
            read(10,'(a)',iostat=ierr) cline
            if(ierr.ne.0) go to 9301
            call linspl(ifail,3,lw,rw,cline)
            if(ifail.ne.0) go to 9301
            call intread(ifail,cline(lw(1):rw(1)),bnrow)
            if(ifail.ne.0) go to 9301
            call intread(ifail,cline(lw(2):rw(2)),bncol)
            if(ifail.ne.0) go to 9301
            call intread(ifail,cline(lw(3):rw(3)),bicode)
            if(ifail.ne.0) go to 9301
            if((bnrow.le.0).or.(bncol.le.0)) go to 9351
            if((bicode.ne.-1).and.(bicode.ne.1).and.(bicode.ne.2))
     +      go to 9351
            if(bnrow.lt.bnespar) go to 9401
            if(bncol.lt.npar)then
              write(amessage,1220) trim(cfile),trim(afile)
1220          format(' The matrix supplied in file ',a,' must have at ',
     +        'least as many columns as there are super parameters in',
     +        ' file ',a,'.')
              go to 9800
            end if
            allocate(jindex(bnrow),stat=ierr)
            if(ierr.ne.0)then
              write(amessage,1230)
1230          format(' Cannot allocate sufficient memory to continue ',
     +        'execution.')
              go to 9800
            end if
            jindex=0                       ! an array
            blhs=0.0d0                     ! an array
            do
              read(10,'(a)',err=9451,end=9501) cline
              cline=adjustl(cline)
              if(cline(1:1).ne.'*') cycle
              call lowcas(cline)
              if(index(cline,'row').ne.0) go to 1250
            end do
1250        continue
            jpp=1
            do i=1,bnrow
              read(10,*,err=9451,end=9551) aapar
              call lowcas(aapar)
              do ipp=jpp,bnpar
                if(aapar.eq.bapar(ipp))then
                  if((bitrans(ipp).ge.0).or.
     +               (bitrans(ipp).eq.-1000001).or.
     +               (bitrans(ipp).eq.-1000002))then
                    jindex(i)=ipp
                  end if
                  jpp=ipp
                  go to 1240
                end if
              end do
              if(jpp.ne.1)then
                do ipp=jpp-1,1,-1
                  if(aapar.eq.bapar(ipp))then
                    if((bitrans(ipp).ge.0).or.
     +                 (bitrans(ipp).eq.-1000001).or.
     +                 (bitrans(ipp).eq.-1000002))then
                      jindex(i)=ipp
                    end if
                    jpp=ipp
                    go to 1240
                  end if
                end do
              end if
1240          continue
            end do
            ii=1
            ies=0
            do ipp=1,bnpar
              if((bitrans(ipp).lt.0).and.
     +           (bitrans(ipp).ne.-1000001).and.
     +           (bitrans(ipp).ne.-1000002)) cycle
c              if(bitrans(ipp).eq.-1000001)then
c                write(amessage,379) trim(bapar(ipp))
c                go to 9800
c              end if
              ies=ies+1
              do i=ii,bnrow
                if(jindex(i).eq.ipp) then
                  jindex(i)=-ies
                  ii=i
                  go to 1251
                end if
              end do
              if(ii.ne.1)then
                do i=ii-1,1,-1
                  if(jindex(i).eq.ipp)then
                    jindex(i)=-ies
                    ii=i
                    go to 1251
                  end if
                end do
              end if
              write(amessage,1241) trim(bapar(ipp)),trim(sfile),
     +        trim(cfile),trim(afile)
1241          format(' Base parameter name "',a,'" in base parameter ',
     +        'PEST control file ',a,' not cited as a matrix row in ',
     +        'super parameter defintion file ',a,' cited in super ',
     +        'PEST control file ',a,'.')
              go to 9800
1251          continue
            end do
            do i=1,bnrow
              jindex(i)=-jindex(i)
            end do
            rewind(unit=10)
            read(10,*)
            do irow=1,bnrow
              if(jindex(irow).eq.0)then
                if(bicode.ne.-1)then
                  read(10,*,err=9601) (dtemp,icol=1,bncol)
                else
                  read(10,*,err=9601) dtemp
                end if
              else
                jj=jindex(irow)
                if(bicode.ne.-1)then
                  if(npar.eq.bncol)then
                    read(10,*,err=9601) (blhs(jj,icol),icol=1,bncol)
                  else
                    ntemp=bncol-npar
                    read(10,*,err=9601) (blhs(jj,icol),icol=1,npar),
     +              (dtemp,icol=1,ntemp)
                  end if
                else
                  read(10,*,err=9601) dtemp
                  if(irow.le.npar) blhs(jj,irow)=dtemp
                end if
              end if
            end do
            close(unit=10)
            write(6,1260) trim(cfile)
1260        format(/,' - super parameter defintion file ',a,
     +      ' read ok.')
          end if

C -- If model-generated derivatives were supplied for base parameters, the temporary
C    base Jacobian file written by PEST on the previous run is now read.

          if(jacfile.ne.0)then
            call addquote(tempbasejacfile,cfile)
            write(6,371) trim(cfile)
371         format(/,' - reading temporary Jacobian matrix file ',
     +      a,'...')
            call open_unformatted_file(ifail,10,'read',tempbasejacfile,
     +      amessage)
            if(ifail.ne.0) go to 9800
            read(10,err=9350,end=9350)itemp1,itemp2
            if(abs(itemp1).ne.bnespar) then
              write(amessage,372) trim(cfile)
372           format(' Temporary Jacobian file ',a,' is not ',
     +        'compatible with current problem.')
              go to 9800
            end if
            if(bbnxrow.ne.abs(itemp2))then
              write(amessage,372) trim(cfile)
              go to 9800
            end if
            do ies=1,bnespar
              do irow=1,bbnxrow
                bx(irow,ies)=0.0d0
              end do
            end do
            read(10,err=9350,end=9350)icount
            do i=1,icount
              read(10,err=9350,end=9350) j,dtemp
              ies=(j-1)/bbnxrow+1
              irow=j-(ies-1)*bbnxrow
              bx(irow,ies)=dtemp
            end do
            close(unit=10)
            write(6,465) trim(cfile)
          end if

        end if

C -- If previous PEST run used SVD-assist, a question is asked of the user.

        if(svda.ne.0)then
          if(jacfile.eq.0)then
481         continue
            if(svda_extsuper.ne.1)then
              write(6,*)
459           write(6,461)
461           format(' Select option for obtaining base parameter ',
     +        'sensitivities:-')
              write(6,462) trim(cfile)
462           format('     enter "1" to use those in base ',
     +        'Jacobian matrix file ',a)
              write(6,463)
463           format('     enter "2" to read from another JCO file')
              write(6,464,advance='no')
464           format(' Enter your choice: ')
              read(5,*,err=471) izfile
              if((izfile.ne.1).and.(izfile.ne.2)) go to 471
              go to 473
471           write(6,*)
              go to 459
473           continue
            else
              izfile=2
              write(6,*)
            end if
            if(izfile.eq.2)then
479           continue
              if(svda_extsuper.ne.1)then
468             write(6,467,advance='no')
467             format(' Enter name of JCO file: ')
              else
                write(6,469,advance='no')
469             format(' Enter name of base parameter sensitivity ',
     +          'matrix file: ')
              end if
              read(5,'(a)')zfile
              if(zfile.eq.' ') go to 479
              call remchar(zfile,'"')
              call remchar(zfile,'''')
              zfile=adjustl(zfile)
              i=len_trim(zfile)
              j=i-3
              if(j.lt.1)j=1
              atemp=zfile(j:i)
#ifndef UNIX
              call lowcas(atemp)
#endif
              if(atemp(1:4).ne.'.jco')then
                write(6,472)
472             format(/,' Base parameter sensitivity file must have ',
     +          'an extension of ".jco" - try again.',/)
                go to 479
              end if
#ifndef UNIX
              call lowcas(zfile)
#endif
              inquire(file=zfile,exist=lexist)
              if(.not.lexist)then
                call addquote(zfile,efile)
                write(6,476) trim(efile)
476             format(/,' Cannot open file ',a,' - try again.',/)
                go to 479
              end if
            else
              if(izcompat.ne.1)then
                write(amessage,490) trim(cfile),trim(afile)
490             format(' The base Jacobian matrix contained in file ',
     +          a,' cannot also serve as a base parameter sensitivity ',
     +          'matrix for resolution matrix calculation because ',
     +          'the PEST file on which it is based does not ',
     +          'cite the same non-regularisation observations ',
     +          'and prior information ',
     +          'equations in the same ',
     +          'order as PEST control file ',a,'.')
                call writmess(6,amessage)
                write(6,477)
477             format(/,' Try again.',/)
                go to 481
              end if
            end if
          else
            izfile=1
          end if

          if(svda_extsuper.eq.1) go to 482

C -- If the previous run was undertaken using SVD-Assist, base-XtQX
C    is created and then singular value decomposition of this is now undertaken.

          write(6,466)
466       format(/,' - undertaking SVD for super parameter ',
     +    'definition...')

          allocate(tbx(nuseobs,bnespar),stat=ierr)
          if(ierr.ne.0) go to 9150
          ies=0
          do ies=1,bnespar
            irr=0
            do irow=1,bnxrow
              if(bowght(irow).gt.0.0d0)then
                irr=irr+1
                tbx(irr,ies)=bx(irow,ies)*sqrt(bowght(irow))
              end if
            end do
          end do

          if(nscaladj.ne.0)then
            ies=0
            do i=1,bnpar
              if((bitrans(i).ge.0).or.(bitrans(i).eq.-1000001)
     +                            .or.(bitrans(i).eq.-1000002))then
                ies=ies+1
                rtemp=sf(i)
                if(rtemp.ne.1.0d0)then
                  do irow=1,nuseobs
                    tbx(irow,ies)=tbx(irow,ies)*rtemp
                  end do
                end if
              end if
            end do
          end if

          if(svda_extsuper.eq.0)then
            blwork=max(3*min(bnespar,nuseobs)+max(bnespar,nuseobs),
     +              5*min(bnespar,nuseobs))+max(bnespar,nuseobs)
            allocate(bwork(blwork),bsss(bnespar),stat=ierr)
            if(ierr.ne.0) go to 9150

            call dgesvd( 'N','O',nuseobs,bnespar,tbx,nuseobs,bsss,u,
     +                   1, vt, 1, bwork, blwork, info )
            if(info.ne.0)then
              write(amessage,595)
595           format(' Subroutine DGESVD returned INFO as non-zero. ',
     +        'Cannot continue execution.')
              go to 9800
            end if
            do ipp=1,npar
              do ies=1,bnespar
                blhs(ies,ipp)=tbx(ipp,ies)
              end do
            end do
            deallocate(tbx,bowght,bwork,bsss,stat=ierr)
          else if(abs(svda_extsuper).eq.2)then
            if(svda_extsuper.eq.2)then
              allocate(vv(bnespar),ww(bnespar),xx(bnespar),
     +        eigmat(bnespar,npar),stat=ierr)
              if(ierr.ne.0) go to 9150
            else if(svda_extsuper.eq.-2)then
              ndim1=max(bnespar,npar)
              ndim3=npar*npar/2+npar
              allocate(vv(bnespar),ww(ndim1),xx(bnespar),
     +        eigmat(bnespar,npar),eigmat1(bnespar,npar),
     +        rr(ndim3),stat=ierr)
              if(ierr.ne.0) go to 9150
            end if
            call lsqr(nuseobs,bnespar,bprod,0.0d0,.false.,
     +                1,1,iiw,rrw,
     +                bb,vv,ww,xx,se,
     +                1.0d-10,1.0d-10,1.0d200,npar,0,
     +                iistop,iitn,anorm,acond,rnorm,arnorm,xnorm,1)
            if(svda_extsuper.eq.-2)then
              call qrfact(bnespar,npar,eigmat,bnespar,eigmat1,
     +        bnespar,rr,vv,bb,ww,xx,info)
              if(info.ne.0)then
                do j=1,npar
                  do i=1,bnespar
                    blhs(i,j)=eigmat(i,j)
                  end do
                end do
              else
                do j=1,npar
                  do i=1,bnespar
                    blhs(i,j)=eigmat1(i,j)
                  end do
                end do
              end if
              deallocate(rr,eigmat1,stat=ierr)
            else
              do j=1,npar
                do i=1,bnespar
                  blhs(i,j)=eigmat(i,j)
                end do
              end do
            end if
            deallocate(vv,ww,xx,eigmat,bb,tbx,stat=ierr)
          else if(svda_extsuper.eq.3)then
            allocate(xtx(bnespar,bnespar),stat=ierr)
            if(ierr.ne.0) go to 9150
            blwork=8*bnespar
            allocate(bwork(blwork),bsss(bnespar),stat=ierr)
            if(ierr.ne.0) go to 9150
            do i=1,bnespar
              do j=1,i
                rtemp=0.0d0
                do k=1,nuseobs
                  rtemp=rtemp+tbx(k,i)*tbx(k,j)
                end do
                xtx(j,i)=rtemp
              end do
            end do
            do i=1,bnespar-1
              do j=i+1,bnespar
                xtx(j,i)=xtx(i,j)
              end do
            end do
            call dgesvd( 'N','O',bnespar,bnespar,xtx,bnespar,bsss,
     +                   u, 1, vt, 1, bwork, blwork, info )
            if(info.ne.0)then
              write(amessage,596)
596           format(' INFO returned as zero in subroutine DGESVD.')
              go to 9800
            end if
            do i=1,npar
              do j=1,bnespar
                blhs(j,i)=xtx(i,j)
              end do
            end do
            deallocate(xtx,tbx,bsss,stat=ierr)

          end if

C -- We now fill the z matrix.

482       continue

          if(izfile.eq.1)then
            continue
          else
            if(svda_extsuper.ne.1)then
              if(nxrow.gt.bbnxrow)then
                deallocate (bx, stat=ierr)
                allocate(bx(nxrow,bnespar),stat=ierr)
                if(ierr.ne.0) go to 9150
              end if
            else
              if(allocated(bx)) deallocate(bx)
              allocate(bx(nxrow,bnespar),stat=ierr)
            end if
            call addquote(zfile,cfile)
            write(6,820) trim(cfile)
820         format(/,' - reading base parameter sensitivity file ',
     +      a,'...')
            call open_unformatted_file(ifail,10,'read',zfile,amessage)
            if(ifail.ne.0) go to 9800

            read(10,err=9350,end=9350)itemp1,itemp2
            if(itemp1.ge.0)then
              write(amessage,830) trim(cfile)
830           format(' Jacobian matrix file ',a,' uses old format; ',
     +        'use JCOTRANS utility to translate it to new format.')
              go to 9800
            end if
            if((abs(itemp2).gt.nxrow).or.(abs(itemp1).ne.bnespar))
     +      go to 9450
            do i=1,bnespar
              do j=1,nxrow
                bx(j,i)=0.0d0
              end do
            end do
            bbnxrow=abs(itemp2)
            read(10,err=9350,end=9350)icount
            do i=1,icount
              read(10,err=9350,end=9350) j,dtemp
              ies=(j-1)/bbnxrow+1
              irow=j-(ies-1)*bbnxrow
              bx(irow,ies)=dtemp
            end do
            do ipar=1,bnpar
              if((bitrans(ipar).ge.0).or.
     +           (bitrans(ipar).eq.-1000001).or.
     +           (bitrans(ipar).eq.-1000002))then
                read(10,err=9350,end=9350) aapar
                call lowcas(aapar)
                if(aapar.ne.bapar(ipar)) go to 9450
              end if
            end do
            do iobs=1,bbnxrow
              read(10,err=9350,end=9350) aaobs
              call lowcas(aaobs)
              if(aaobs.ne.aobs(iobs))go to 9450
            end do
            if(bbnxrow.lt.nxrow)then
              do irow=bbnxrow+1,nxrow
                jj=nobgnm(irow)
                if(irgp(jj).eq.0) go to 9450
              end do
            end if
            close(unit=10)
            write(6,880) trim(cfile)
880         format(' - base parameter sensitivity file ',a,' read ok.')
          end if
        end if

C -- If any base parameter has hit its bound an x matrix is calculated.

        if(svda.ne.0)then
          icount=0
          do i=1,bnpar
            if((bitrans(i).eq.-1000001).or.
     +         (bitrans(i).eq.-1000002))icount=icount+1
          end do
          if(icount.ne.0)then
            iflagwarn=1
            x=0.0d0
            do j=1,min(nxrow,bbnxrow)
              do i=1,npar
                dtemp=0.0d0
                do k=1,bnespar
                  dtemp=dtemp+bx(j,k)*blhs(k,i)
                end do
                x(j,i)=dtemp
              end do
            end do
          end if
        end if

C -- Important note: the above protocol for reading a sensitivity matrix allows
C    prior information to be ommitted from this matrix. If this is the case then
C    the above X matrix will lack this prior information. Thus regularisation
C    will not figure in calculation of the resolution matrix because all terms
C    pertaining to regularisation may well be zero.

C -- The resolution and parameter solution matrices are now calculated.

C -- The number of non-regularisation observations is evaluated.

        if(ireg.eq.0)then
          numnonreg=nxrow
        else
          do i=1,nxrow
            j=nobgnm(i)
            if(irgp(j).eq.0) numnonreg=numnonreg+1
          end do
        end if

C -- First more memory is allocated.

        lwork=8*nespar
        itemp=max(3*min(nxrow,nespar)+max(nxrow,nespar),
     +            5*min(nxrow,nespar))
        itemp=itemp+2*max(nxrow,nespar)  ! arbitrary
        if(lwork.lt.itemp)lwork=itemp
        allocate(mat1(nespar,nespar),work(lwork),sss(nespar),stat=ierr)
        if(ierr.ne.0) go to 9150
        if(svda.eq.0)then
          allocate(res(nespar,nespar),g(nespar,numnonreg),stat=ierr)
        else
          allocate(res(bnespar,bnespar),g(bnespar,numnonreg),stat=ierr)
        end if
        if(ierr.ne.0) go to 9150
        if(svda.eq.0)then
          allocate(mat2(nespar,nespar),stat=ierr)
        else
          allocate(mat2(nespar,bnespar),stat=ierr)
        end if
        if(ierr.ne.0) go to 9150
        if(svda.ne.0)then
          itemp1=max(nespar,bnespar)
          allocate(mat3(itemp1,nespar),stat=ierr)
          if(ierr.ne.0) go to 9150
        else
          if(svdmode.ne.0)then
            allocate(mat3(nespar,nespar),stat=ierr)
            if(ierr.ne.0) go to 9150
          end if
        end if

C -- First we calculate the "normal matrix" - excluding Marquardt lambda.

        write(6,831)
831     format(/,' - undertaking preliminary calculations...')

        if(svdmode.ne.2)then
          do ies=1,nespar
            do jes=1,ies
              rtemp1=0.0d0
              do irow=1,nxrow
                rtemp1=rtemp1+x(irow,ies)*x(irow,jes)*owght(irow)
              end do
              mat1(jes,ies)=rtemp1
            end do
          end do
          do ies=1,nespar-1
            do jes=ies+1,nespar
              mat1(jes,ies)=mat1(ies,jes)
            end do
          end do
        end if

C -- Now we evaluate the second XtQX matrix of the resolution matrix (if necessary).

        if(((svdmode.eq.1).and.(ireg.eq.0).and.(alphlst.eq.0.0d0)
     +    .and.(svda.eq.0)).or.
     +    ((svdmode.eq.2).and.(ireg.eq.0).and.(svda.eq.0)))then
          if(svdmode.eq.2)then
            allocate(mat4(nxrow,nespar),stat=ierr)
            if(ierr.ne.0) go to 9150
            do irow=1,nxrow
              rtemp=sqrt(owght(irow))
              do ies=1,nespar
                mat4(irow,ies)=x(irow,ies)*rtemp
              end do
            end do
            call dgesvd('N','O',nxrow,nespar,mat4,nxrow,sss,u,1,vt,1,
     +      work,lwork,info)
            if(info.ne.0)then
              write(amessage,620)
              go to 9800
            end if
            itemp=min(nespar,nxrow)
            do ies=1,itemp
              sss(ies)=sss(ies)*sss(ies)
            end do
            do j=1,itemp
              do ies=1,nespar
                mat1(ies,j)=mat4(j,ies)
              end do
            end do
            deallocate(mat4,stat=ierr)
          else
            call dgesvd('O','N',nespar,nespar,mat1,nespar,sss,u,1,
     +      vt,1,work,lwork,info)
            if(info.ne.0)then
              write(amessage,620)
620           format(' Unable to undertake singular value ',
     +        'decomposition in order to calculate resolution matrix.')
              go to 9800
            end if
          end if

          if((svdmode.eq.2).and.(lamlst.ne.0.0d0))then
            continue
          else
            do ies=1,numeiglst
              if(sss(ies).le.1.0d-40) sss(ies)=1.0d-40
              sss(ies)=1.0/sss(ies)
            end do
          end if

          write(6,621)
621       format(/,' - calculating resolution matrix...')

          if((svdmode.eq.2).and.(lamlst.ne.0.0d0))then
            do ies=1,nespar
              do jes=1,ies
                rtemp1=0.0d0
                rtemp2=0.0d0
                do i=1,numeiglst
                  rtemp=mat1(jes,i)*mat1(ies,i)
                  rrtemp=1.0d0/(sss(i)+lamlst)
                  rtemp1=rtemp1+rtemp*sss(i)*rrtemp
                  rtemp2=rtemp2+rtemp*rrtemp
                end do
                res(jes,ies)=rtemp1
                mat2(jes,ies)=rtemp2
              end do
            end do
          else
            do ies=1,nespar
              do jes=1,ies
                rtemp1=0.0d0
                rtemp2=0.0d0
                do i=1,numeiglst
                  rtemp=mat1(jes,i)*mat1(ies,i)
                  rtemp1=rtemp1+rtemp
                  rtemp2=rtemp2+rtemp*sss(i)
                end do
                res(jes,ies)=rtemp1
                mat2(jes,ies)=rtemp2
              end do
            end do
          end if
          do ies=1,nespar-1
            do jes=ies+1,nespar
              res(jes,ies)=res(ies,jes)
            end do
          end do
          write(6,622)
622       format(/,' - calculating parameter solution matrix...')
          do ies=1,nespar-1
            do jes=ies+1,nespar
              mat2(jes,ies)=mat2(ies,jes)
            end do
          end do
          do irow=1,nxrow
            do ies=1,nespar
              rtemp=0.0d0
              do jes=1,nespar
                rtemp=rtemp+mat2(ies,jes)*x(irow,jes)
              end do
              g(ies,irow)=rtemp*owght(irow)
            end do
          end do
          go to 1100
        else
          if(svda.eq.0)then
            if(ireg.eq.0)then
              if(svdmode.eq.2)then              ! because it was not calculated above.
                do ies=1,nespar
                  do jes=1,ies
                    rtemp1=0.0d0
                    do irow=1,nxrow
                      rtemp1=rtemp1+x(irow,ies)*x(irow,jes)*owght(irow)
                    end do
                    mat1(jes,ies)=rtemp1
                  end do
                end do
                do ies=1,nespar-1
                  do jes=ies+1,nespar
                    mat1(jes,ies)=mat1(ies,jes)
                  end do
                end do
              end if
              do ies=1,nespar
                do jes=1,nespar
                  mat2(ies,jes)=mat1(ies,jes)
                end do
              end do
            else
              do ies=1,nespar
                do jes=1,ies
                  rtemp2=0.0d0
                  do irow=1,nxrow
                    j=nobgnm(irow)
                    if(irgp(j).eq.0) then
                      rtemp2=rtemp2+x(irow,ies)*x(irow,jes)*owght(irow)
                    end if
                  end do
                  mat2(jes,ies)=rtemp2
                end do
              end do
              do ies=1,nespar-1
                do jes=ies+1,nespar
                  mat2(jes,ies)=mat2(ies,jes)
                end do
              end do
            end if
          else
            if(ireg.eq.0)then
              do ies=1,bnespar
                do jes=1,nespar
                  rtemp2=0.0d0
                  do irow=1,nxrow
                    rtemp2=rtemp2+x(irow,jes)*bx(irow,ies)*owght(irow)
                  end do
                  mat2(jes,ies)=rtemp2
                end do
              end do
            else
              do ies=1,bnespar
                do jes=1,nespar
                  rtemp2=0.0d0
                  do irow=1,nxrow
                    j=nobgnm(irow)
                    if(irgp(j).eq.0) then
                      rtemp2=rtemp2+x(irow,jes)*bx(irow,ies)*owght(irow)
                    end if
                  end do
                  mat2(jes,ies)=rtemp2
                end do
              end do
            end if
          end if
        end if

C -- Now we add the Marquardt lambda to the normal matrix if necessary.

        if(svdmode.ne.2)then
          if(alphlst.ne.0.0d0)then
            do ies=1,nespar
              mat1(ies,ies)=mat1(ies,ies)+alphlst
            end do
          end if
        end if

        if(svdmode.eq.0)then
          call dpofa(mat1,nespar,nespar,info)
          if(info.ne.0)then
            write(amessage,650)
650         format(' Cannot perform required matrix inversion to ',
     +      'obtain resolution matrix.')
            go to 9800
          end if
          info=1
          call dpodi(mat1,nespar,nespar,work,info)
          do ies=1,nespar-1
            do jes=ies+1,nespar
              mat1(jes,ies)=mat1(ies,jes)
            end do
          end do
        else
          if(svdmode.eq.1)then
            call dgesvd('O','N',nespar,nespar,mat1,nespar,sss,u,1,
     +      vt,1,work,lwork,info)
            if(info.ne.0)then
              write(amessage,660)
660           format(' Cannot carry out truncated singular value ',
     +        'decomposition for inversion of normal matrix.')
              go to 9800
            end if
            do ies=1,numeiglst
              if(sss(ies).lt.1.0d-40)sss(ies)=1.0d-40
              sss(ies)=1.0/sss(ies)
            end do
          else if (svdmode.eq.2)then
            allocate(mat4(nxrow,nespar),stat=ierr)
            if(ierr.ne.0) go to 9150
            do irow=1,nxrow
              rrtemp=sqrt(owght(irow))
              do ies=1,nespar
                mat4(irow,ies)=x(irow,ies)*rrtemp
              end do
            end do
            call dgesvd('N','O',nxrow,nespar,mat4,nxrow,sss,u,1,vt,1,
     +      work,lwork,info)
            if(info.ne.0)then
              write(amessage,660)
              go to 9800
            end if
            itemp=min(nespar,nxrow)
            do ies=1,itemp
              sss(ies)=sss(ies)*sss(ies)+lamlst
            end do
            do ies=1,numeiglst
              if(sss(ies).lt.1.0d-40)sss(ies)=1.0d-40
              sss(ies)=1.0/sss(ies)
            end do
            do j=1,itemp
              do ies=1,nespar
                mat1(ies,j)=mat4(j,ies)
              end do
            end do
            deallocate(mat4,stat=ierr)
          end if
          do ies=1,nespar
            do jes=1,ies
              rtemp1=0.0d0
              do i=1,numeiglst
                rtemp1=rtemp1+mat1(jes,i)*mat1(ies,i)*sss(i)
              end do
              mat3(jes,ies)=rtemp1
            end do
          end do
          do ies=1,nespar
            do jes=1,ies
              mat1(jes,ies)=mat3(jes,ies)
            end do
          end do
          do ies=1,nespar-1
            do jes=ies+1,nespar
              mat1(jes,ies)=mat1(ies,jes)
            end do
          end do
        end if

C -- The first and second matrices are now multiplied to form the resolution matrix.

        if(svda.eq.0)then
          write(6,621)
          do ies=1,nespar
            do jes=1,nespar
              rtemp=0.0d0
              do i=1,nespar
                rtemp=rtemp+mat1(jes,i)*mat2(i,ies)
              end do
              res(jes,ies)=rtemp
            end do
          end do
          write(6,622)
          if(ireg.ne.0)then
            iobs=0
            do irow=1,nxrow
              j=nobgnm(irow)
              if(irgp(j).ne.0) cycle
              iobs=iobs+1
              do ies=1,nespar
                rtemp=0.0d0
                do jes=1,nespar
                  rtemp=rtemp+mat1(ies,jes)*x(irow,jes)
                end do
                g(ies,iobs)=rtemp*owght(irow)
              end do
            end do
          else
            do irow=1,nxrow
              do ies=1,nespar
                rtemp=0.0d0
                do jes=1,nespar
                  rtemp=rtemp+mat1(ies,jes)*x(irow,jes)
                end do
                g(ies,irow)=rtemp*owght(irow)
              end do
            end do
          end if
        else
          write(6,621)
          do ies=1,nespar
            do jes=1,bnespar
              rtemp=0.0d0
              do k=1,nespar
                rtemp=rtemp+blhs(jes,k)*mat1(k,ies)
              end do
              mat3(jes,ies)=rtemp
            end do
          end do
          do ies=1,bnespar
            do jes=1,bnespar
              rtemp=0.0d0
              do k=1,nespar
                rtemp=rtemp+mat3(jes,k)*mat2(k,ies)
              end do
              res(jes,ies)=rtemp
            end do
          end do
          write(6,622)
          if(ireg.eq.0)then
            do irow=1,nxrow
              do ies=1,bnespar
                rtemp=0.0d0
                do k=1,nespar
                  rtemp=rtemp+mat3(ies,k)*x(irow,k)
                end do
                g(ies,irow)=rtemp*owght(irow)
              end do
            end do
          else
            iobs=0
            do irow=1,nxrow
              j=nobgnm(irow)
              if(irgp(j).ne.0) cycle
              iobs=iobs+1
              do ies=1,bnespar
                rtemp=0.0d0
                do k=1,nespar
                  rtemp=rtemp+mat3(ies,k)*x(irow,k)
                end do
                g(ies,iobs)=rtemp*owght(irow)
              end do
            end do
          end if
        end if

C -- Finally, if SVDA was activated, the effect of scaling is taken into account.

1100    continue

        if(nscaladj.ne.0)then
          ies=0
          do ipar=1,bnpar
            if((bitrans(ipar).lt.0).and.
     +         (bitrans(ipar).ne.-1000001).and.
     +         (bitrans(ipar).ne.-1000002))cycle
            ies=ies+1
            sftemp=sf(ipar)
            if(sftemp.ne.1.0d0)then
              do jes=1,bnespar
                res(ies,jes)=res(ies,jes)*sftemp
              end do
C              do jes=1,bnespar                    ! This is not needed because the Z matrix
C                res(jes,ies)=res(jes,ies)/sftemp  ! that we read in has not had its columns
C              end do                              ! altered in accordance with the scale factor.
              iobs=0
              do irow=1,nxrow
                j=nobgnm(irow)
                if(irgp(j).ne.0) cycle
                iobs=iobs+1
                g(ies,iobs)=g(ies,iobs)*sftemp
              end do
            end if
          end do
        end if

C -- The resolution and G matrices have been calculated. Now they are stored.


C -- But first we insert some codes for future identification.

        icode=0         ! an array
        rcode=0.0d0     ! an array
        acode=' '       ! an array

        icode(1)=1      ! Version number   Does anything use this?
        icode(2)=svda
        if(svdmode.ne.0) icode(3)=numeiglst
        icode(4)=ireg
        if(svda.ne.0) then
          icode(5)=izfile
          icode(6)=nespar
        end if

        rcode(1)=alphlst             ! Does anything use this?
        rcode(2)=lamlst

        acode(1)=pestfile
        if(svda.ne.0)then
          acode(2)=basepestfile
          acode(3)=basejacfile
          if(izfile.ne.0) acode(4)=zfile
          if(jacfile.ne.0)acode(5)=tempbasejacfile
        end if

        if(svda.eq.0)then
          nesdim=nespar
        else
          nesdim=bnespar
        end if

        write(6,910) trim(dfile)
910     format(/,' - writing unformatted output file ',a,'...')

        write(20,err=9700) (icode(i),i=1,10)
        write(20,err=9700) (rcode(i),i=1,10)
        do i=1,10
          write(20,err=9700) acode(i)
        end do
        write(20,err=9700) nesdim,numnonreg
        write(20,err=9700) ((res(i,j),i=1,nesdim),j=1,nesdim)
        write(20,err=9700) ((g(i,j),i=1,nesdim),j=1,numnonreg)
        if(svda.eq.0)then
          do i=1,npar
            if(itrans(i).ge.0) write(20,err=9700) apar(i)
          end do
        else
          do i=1,bnpar
            if((bitrans(i).ge.0).or.(bitrans(i).eq.-1000001)
     +                          .or.(bitrans(i).eq.-1000002))
     +      write(20,err=9700) bapar(i)
          end do
        end if
        if(ireg.eq.0)then
          do irow=1,nxrow
            write(20,err=9700) aobs(irow)
          end do
        else
          do irow=1,nxrow
            j=nobgnm(irow)
            if(irgp(j).eq.0) write(20,err=9700) aobs(irow)
          end do
        end if
        close(unit=20)

        call addquote(outfile,afile)
        write(6,920) trim(afile)
920     format(' - file ',a,' written ok.')

        if(iflagwarn.eq.1)then
          jjcount=0
          do i=1,bnpar
            if(bitrans(i).ge.0)jjcount=jjcount+1
          end do
          if(jjcount.ge.npar)then
            if(jacfile.eq.0)then
              write(amessage,930)
930           format(' Warning: at least one base parameter hit its ',
     +        'bounds during the preceding inversion process. Hence ',
     +        'calculation ',
     +        'of the resolution and G matrices is approximate, ',
     +        'especially if an updated base Jacobian matrix was not ',
     +        'read by RESPROC.')
              call writmess(6,amessage)
            else
              write(amessage,931)
931           format(' Warning: at least one base parameter hit its ',
     +        'bounds during the preceding inversion process. Hence ',
     +        'calculation ',
     +        'of the resolution and G matrices is approximate.')
              call writmess(6,amessage)
            end if
          else
            write(amessage,932)
932         format(' Warning: less super parameters were employed ',
     +      'in previous SVD-assisted PEST run then there were ',
     +      'adjustable base parameters and base parameters ',
     +      'that had not hit their bounds. The resolution ',
     +      'and G matrices are to be taken with a canister ',
     +      'of salt.')
            call writmess(6,amessage)
          end if
        end if

        go to 9900

8900   continue

       write(6,8910)
8910   format(/,' RESPROC is run using the command:',/)
       write(6,8920)
8920   format('     resproc casename outfile [/(n)L]',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8950)
8950   format('     casename is the filename base for a ',
     + 'completed PEST run,')
       write(6,8980)
8980   format('     outfile  is the unformatted RESPROC output file',
     + ' and')
       write(6,8985)
8985   format('     "/L", "/nL" activates, deactivates use ',
     + 'of Marquardt lambda (default "/L").')


       go to 9999

9000   call writint(aline,iline)
       write(amessage,9010) trim(aline),trim(afile)
9010   format(' Error reading line ',a,' of PEST control file ',a,'.')
       go to 9800
9050   write(amessage,9060) trim(afile)
9060   format(' Premature end encountered to PEST control file ',a,'.')
       go to 9800
9100   call writint(aline,iline)
       write(amessage,9110) trim(aline),trim(afile)
9110   format(' Insufficient entries on line ',a,
     + ' of PEST control file ',a,'.')
       go to 9800
9150   write(amessage,9160)
9160   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800
9200   write(amessage,9210) trim(bfile)
9210   format(' Error reading unformatted file ',a,'.')
       go to 9800
9300   write(amessage,9310) trim(afile)
9310   format(' Error reading PEST control file ',a,'; check ',
     + 'this file with PESTCHEK.')
       go to 9800
9350   write(amessage,9360) trim(cfile)
9360   format(' Error reading Jacobian matrx file ',a,'.')
       go to 9800
9400   write(amessage,9410) trim(cfile),trim(afile)
9410   format(' Jacobian matrix file ',a,' is incompatible with PEST ',
     + 'control file ',a,'.')
       go to 9800
9450   call addquote(basepestfile,efile)
       if(svda_extsuper.ne.1)then
       write(amessage,9460) trim(cfile),trim(efile),trim(afile),
     + trim(cfile)
9460   format(' Jacobian matrix file ',a,' must cite the ',
     + 'same adjustable base parameters in the same order ',
     + 'as the base PEST control file ',a,' and the same ',
     + 'observations in the same order as the super parameter ',
     + 'PEST control file ',a,'. This will happen automatically ',
     + 'if the latter PEST control file is built from the ',
     + 'former using SVDAPREP and the new base Jacobian ',
     + 'file ',a,' was written by PEST on the basis of a PEST ',
     + 'control file written by PARREP after completion ',
     + 'of the super parameter estimation process. See PEST ',
     + 'documentation for details.')
       else
       write(amessage,9462) trim(cfile),trim(efile),trim(afile)
9462   format(' Jacobian matrix file ',a,' must cite the ',
     + 'same adjustable base parameters in the same order ',
     + 'as the base PEST control file ',a,' and the same ',
     + 'observations in the same order as the super parameter ',
     + 'PEST control file ',a,'.')
       end if
       go to 9800
9500   write(amessage,9510) trim(dfile),trim(afile)
9510   format(' Jacobian matrix file ',a,' does not cite ',
     + 'same adjustable parameters and/or non-regularisation ',
     + 'observations as PEST control file ',a,'.')
       go to 9800
9700   write(amessage,9710) trim(dfile)
9710   format(' Error writing to unformatted file ',a,'.')
       go to 9800

9301   write(amessage,9311) trim(cfile),trim(afile)
9311   format(' Error reading first line (i.e. header line) ',
     + 'of super parameter definition file ',a,' cited in PEST ',
     + 'control file ',a,'.')
       go to 9800
9351   write(amessage,9361) trim(cfile),trim(afile)
9361   format(' Improper header on first line ',
     + 'of super parameter definition file ',a,' cited in PEST ',
     + 'control file ',a,'.')
       go to 9800
9401   call addquote(basepestfile,afile)
       write(amessage,9411) trim(cfile),trim(afile)
9411   format(' There are less rows in super parameter file ',a,
     + ' than the number ',
     + 'of adjustable base parameters cited in PEST control ',
     + 'file 'a,'.')
       go to 9800
9451   write(amessage,9461) trim(cfile),trim(afile)
9461   format(' Error encountered in super parameter definition ',
     + 'file ',a,' cited in PEST control file ',a,'.')
       go to 9800
9501   continue
       if(bicode.eq.2)then
         write(amessage,9511) trim(cfile),trim(afile)
9511     format(' Cannot find "* row names" header in super ',
     +   'parameter definition file ',a,' cited in PEST ',
     +   'control file ',a,'.')
         go to 9800
       else
         write(amessage,9521) trim(cfile),trim(afile)
9521     format(' Cannot find "* row and column names" header in ',
     +   'super parameter definition file ',a,' cited in PEST ',
     +   'control file ',a,'.')
         go to 9800
       end if
9551   write(amessage,9561) trim(cfile),trim(afile)
9561   format(' Unexpected end encountered to super parameter ',
     + 'definition file ',a,' cited in PEST control file ',a,'.')
       go to 9800
9601   call writint(arow,irow)
       write(amessage,9611) trim(arow),trim(cfile),trim(afile)
9611   format(' Error encountered when reading matrix row ',a,
     + ' from super parameter definition file ',a,' cited in ',
     + 'PEST control file ',a,'.')
       go to 9800

9800   call writmess(6,amessage)
9900   continue
       deallocate(itrans,irgp,nobgnm,bitrans,stat=ierr)
       deallocate(x,owght,blhs,bx,work,sss,stat=ierr)
       deallocate(mat1,mat2,mat3,res,g,stat=ierr)
       deallocate(apar,obgnme,bapar,stat=ierr)
       deallocate(aobs,stat=ierr)
       if(allocated(jindex)) deallocate(jindex,stat=ierr)
       if(allocated(bwork)) deallocate(bwork,stat=ierr)
       if(allocated(bsss)) deallocate(bsss,stat=ierr)
       if(allocated(tbx)) deallocate(tbx,stat=ierr)
       if(allocated(bowght)) deallocate(bowght,stat=ierr)
       if(allocated(bb)) deallocate(bb,stat=ierr)
       if(allocated(vv)) deallocate(vv,stat=ierr)
       if(allocated(ww)) deallocate(ww,stat=ierr)
       if(allocated(xx)) deallocate(xx,stat=ierr)
       if(allocated(rr)) deallocate(rr,stat=ierr)
       if(allocated(eigmat1)) deallocate(eigmat1,stat=ierr)
       if(allocated(xtx)) deallocate(xtx,stat=ierr)
       if(allocated(mat3)) deallocate(mat4,stat=ierr)

       inquire(unit=10,opened=lopened)
       if(lopened)close(unit=10)

9999   continue
       end program



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


	subroutine writint(atemp,ival)

c	Subroutine WRITINT writes an integer to a character variable.

	integer*4 ival
	character*6 afmt
	character*(*) atemp

	afmt='(i   )'
	write(afmt(3:5),'(i3)') len(atemp)
	write(atemp,afmt)ival
	atemp=adjustl(atemp)
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

C###############################################################################

C                  LSQR CODE (SLIGHTLY MODIFIED) FOLLOWS

C                  USER-SUPPLIED SUBROUTINE ALSO FOLLOWS

C###############################################################################


*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     File lsqr   fortran   (double precision)
*
*     LSQR     d2norm
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine LSQR  ( m, n, aprod, damp, wantse,
     $                  leniw, lenrw, iw, rw,
     $                  u, v, w, x, se,
     $                  atol, btol, conlim, itnlim, nout,
     $                  istop, itn, anorm, acond, rnorm, arnorm, xnorm,
     $                  ipestflag)


      use lsqr_data_exchange, only: eigmat,nparx             !jd

      implicit           double precision(a-h,o-z)


      integer ii,eigcol,ipestflag                            !jd


      external           aprod
      logical            wantse
      integer            m, n, leniw, lenrw, itnlim, nout, istop, itn
      integer            iw(leniw)
      double precision   rw(lenrw), u(m), v(n), w(n), x(n), se(*),
     $                   atol, btol, conlim, damp,
     $                   anorm, acond, rnorm, arnorm, xnorm
*     ------------------------------------------------------------------
*
*     LSQR  finds a solution x to the following problems:
*
*     1. Unsymmetric equations --    solve  A*x = b
*
*     2. Linear least squares  --    solve  A*x = b
*                                    in the least-squares sense
*
*     3. Damped least squares  --    solve  (   A    )*x = ( b )
*                                           ( damp*I )     ( 0 )
*                                    in the least-squares sense
*
*     where A is a matrix with m rows and n columns, b is an
*     m-vector, and damp is a scalar.  (All quantities are real.)
*     The matrix A is intended to be large and sparse.  It is accessed
*     by means of subroutine calls of the form
*
*                call aprod ( mode, m, n, x, y, leniw, lenrw, iw, rw )
*
*     which must perform the following functions:
*
*                If mode = 1, compute  y = y + A*x.
*                If mode = 2, compute  x = x + A(transpose)*y.
*
*     The vectors x and y are input parameters in both cases.
*     If  mode = 1,  y should be altered without changing x.
*     If  mode = 2,  x should be altered without changing y.
*     The parameters leniw, lenrw, iw, rw may be used for workspace
*     as described below.
*
*     The rhs vector b is input via u, and subsequently overwritten.
*
*
*     Note:  LSQR uses an iterative method to approximate the solution.
*     The number of iterations required to reach a certain accuracy
*     depends strongly on the scaling of the problem.  Poor scaling of
*     the rows or columns of A should therefore be avoided where
*     possible.
*
*     For example, in problem 1 the solution is unaltered by
*     row-scaling.  If a row of A is very small or large compared to
*     the other rows of A, the corresponding row of ( A  b ) should be
*     scaled up or down.
*
*     In problems 1 and 2, the solution x is easily recovered
*     following column-scaling.  Unless better information is known,
*     the nonzero columns of A should be scaled so that they all have
*     the same Euclidean norm (e.g., 1.0).
*
*     In problem 3, there is no freedom to re-scale if damp is
*     nonzero.  However, the value of damp should be assigned only
*     after attention has been paid to the scaling of A.
*
*     The parameter damp is intended to help regularize
*     ill-conditioned systems, by preventing the true solution from
*     being very large.  Another aid to regularization is provided by
*     the parameter acond, which may be used to terminate iterations
*     before the computed solution becomes very large.
*
*     Note that x is not an input parameter.
*     If some initial estimate x0 is known and if damp = 0,
*     one could proceed as follows:
*
*       1. Compute a residual vector     r0 = b - A*x0.
*       2. Use LSQR to solve the system  A*dx = r0.
*       3. Add the correction dx to obtain a final solution x = x0 + dx.
*
*     This requires that x0 be available before and after the call
*     to LSQR.  To judge the benefits, suppose LSQR takes k1 iterations
*     to solve A*x = b and k2 iterations to solve A*dx = r0.
*     If x0 is "good", norm(r0) will be smaller than norm(b).
*     If the same stopping tolerances atol and btol are used for each
*     system, k1 and k2 will be similar, but the final solution x0 + dx
*     should be more accurate.  The only way to reduce the total work
*     is to use a larger stopping tolerance for the second system.
*     If some value btol is suitable for A*x = b, the larger value
*     btol*norm(b)/norm(r0)  should be suitable for A*dx = r0.
*
*     Preconditioning is another way to reduce the number of iterations.
*     If it is possible to solve a related system M*x = b efficiently,
*     where M approximates A in some helpful way
*     (e.g. M - A has low rank or its elements are small relative to
*     those of A), LSQR may converge more rapidly on the system
*           A*M(inverse)*z = b,
*     after which x can be recovered by solving M*x = z.
*
*     NOTE: If A is symmetric, LSQR should not be used!
*     Alternatives are the symmetric conjugate-gradient method (cg)
*     and/or SYMMLQ.
*     SYMMLQ is an implementation of symmetric cg that applies to
*     any symmetric A and will converge more rapidly than LSQR.
*     If A is positive definite, there are other implementations of
*     symmetric cg that require slightly less work per iteration
*     than SYMMLQ (but will take the same number of iterations).
*
*
*     Notation
*     --------
*
*     The following quantities are used in discussing the subroutine
*     parameters:
*
*     Abar   =  (   A    ),          bbar  =  ( b )
*               ( damp*I )                    ( 0 )
*
*     r      =  b  -  A*x,           rbar  =  bbar  -  Abar*x
*
*     rnorm  =  sqrt( norm(r)**2  +  damp**2 * norm(x)**2 )
*            =  norm( rbar )
*
*     relpr  =  the relative precision of floating-point arithmetic
*               on the machine being used.  On most machines,
*               relpr is about 1.0e-7 and 1.0d-16 in single and double
*               precision respectively.
*
*     LSQR  minimizes the function rnorm with respect to x.
*
*
*     Parameters
*     ----------
*
*     m       input      m, the number of rows in A.
*
*     n       input      n, the number of columns in A.
*
*     aprod   external   See above.
*
*     damp    input      The damping parameter for problem 3 above.
*                        (damp should be 0.0 for problems 1 and 2.)
*                        If the system A*x = b is incompatible, values
*                        of damp in the range 0 to sqrt(relpr)*norm(A)
*                        will probably have a negligible effect.
*                        Larger values of damp will tend to decrease
*                        the norm of x and reduce the number of
*                        iterations required by LSQR.
*
*                        The work per iteration and the storage needed
*                        by LSQR are the same for all values of damp.
*
*     wantse  input      A logical variable to say if the array se(*)
*                        of standard error estimates should be computed.
*                        If m .gt. n  or  damp .gt. 0,  the system is
*                        overdetermined and the standard errors may be
*                        useful.  (See the first LSQR reference.)
*                        Otherwise (m .le. n  and  damp = 0) they do not
*                        mean much.  Some time and storage can be saved
*                        by setting wantse = .false. and using any
*                        convenient array for se(*), which won't be
*                        touched.
*
*     leniw   input      The length of the workspace array iw.
*     lenrw   input      The length of the workspace array rw.
*     iw      workspace  An integer array of length leniw.
*     rw      workspace  A real array of length lenrw.
*
*             Note:  LSQR  does not explicitly use the previous four
*             parameters, but passes them to subroutine aprod for
*             possible use as workspace.  If aprod does not need
*             iw or rw, the values leniw = 1 or lenrw = 1 should
*             be used, and the actual parameters corresponding to
*             iw or rw  may be any convenient array of suitable type.
*
*     u(m)    input      The rhs vector b.  Beware that u is
*                        over-written by LSQR.
*
*     v(n)    workspace
*
*     w(n)    workspace
*
*     x(n)    output     Returns the computed solution x.
*
*     se(*)   output     If wantse is true, the dimension of se must be
*             (maybe)    n or more.  se(*) then returns standard error
*                        estimates for the components of x.
*                        For each i, se(i) is set to the value
*                           rnorm * sqrt( sigma(i,i) / t ),
*                        where sigma(i,i) is an estimate of the i-th
*                        diagonal of the inverse of Abar(transpose)*Abar
*                        and  t = 1      if  m .le. n,
*                             t = m - n  if  m .gt. n  and  damp = 0,
*                             t = m      if  damp .ne. 0.
*
*                        If wantse is false, se(*) will not be touched.
*                        The actual parameter can be any suitable array
*                        of any length.
*
*     atol    input      An estimate of the relative error in the data
*                        defining the matrix A.  For example,
*                        if A is accurate to about 6 digits, set
*                        atol = 1.0e-6 .
*
*     btol    input      An estimate of the relative error in the data
*                        defining the rhs vector b.  For example,
*                        if b is accurate to about 6 digits, set
*                        btol = 1.0e-6 .
*
*     conlim  input      An upper limit on cond(Abar), the apparent
*                        condition number of the matrix Abar.
*                        Iterations will be terminated if a computed
*                        estimate of cond(Abar) exceeds conlim.
*                        This is intended to prevent certain small or
*                        zero singular values of A or Abar from
*                        coming into effect and causing unwanted growth
*                        in the computed solution.
*
*                        conlim and damp may be used separately or
*                        together to regularize ill-conditioned systems.
*
*                        Normally, conlim should be in the range
*                        1000 to 1/relpr.
*                        Suggested value:
*                        conlim = 1/(100*relpr)  for compatible systems,
*                        conlim = 1/(10*sqrt(relpr)) for least squares.
*
*             Note:  If the user is not concerned about the parameters
*             atol, btol and conlim, any or all of them may be set
*             to zero.  The effect will be the same as the values
*             relpr, relpr and 1/relpr respectively.
*
*     itnlim  input      An upper limit on the number of iterations.
*                        Suggested value:
*                        itnlim = n/2   for well-conditioned systems
*                                       with clustered singular values,
*                        itnlim = 4*n   otherwise.
*
*     nout    input      File number for printed output.  If positive,
*                        a summary will be printed on file nout.
*
*     istop   output     An integer giving the reason for termination:
*
*                0       x = 0  is the exact solution.
*                        No iterations were performed.
*
*                1       The equations A*x = b are probably
*                        compatible.  Norm(A*x - b) is sufficiently
*                        small, given the values of atol and btol.
*
*                2       damp is zero.  The system A*x = b is probably
*                        not compatible.  A least-squares solution has
*                        been obtained that is sufficiently accurate,
*                        given the value of atol.
*
*                3       damp is nonzero.  A damped least-squares
*                        solution has been obtained that is sufficiently
*                        accurate, given the value of atol.
*
*                4       An estimate of cond(Abar) has exceeded
*                        conlim.  The system A*x = b appears to be
*                        ill-conditioned.  Otherwise, there could be an
*                        error in subroutine aprod.
*
*                5       The iteration limit itnlim was reached.
*
*     itn     output     The number of iterations performed.
*
*     anorm   output     An estimate of the Frobenius norm of  Abar.
*                        This is the square-root of the sum of squares
*                        of the elements of Abar.
*                        If damp is small and if the columns of A
*                        have all been scaled to have length 1.0,
*                        anorm should increase to roughly sqrt(n).
*                        A radically different value for anorm may
*                        indicate an error in subroutine aprod (there
*                        may be an inconsistency between modes 1 and 2).
*
*     acond   output     An estimate of cond(Abar), the condition
*                        number of Abar.  A very high value of acond
*                        may again indicate an error in aprod.
*
*     rnorm   output     An estimate of the final value of norm(rbar),
*                        the function being minimized (see notation
*                        above).  This will be small if A*x = b has
*                        a solution.
*
*     arnorm  output     An estimate of the final value of
*                        norm( Abar(transpose)*rbar ), the norm of
*                        the residual for the usual normal equations.
*                        This should be small in all cases.  (arnorm
*                        will often be smaller than the true value
*                        computed from the output vector x.)
*
*     xnorm   output     An estimate of the norm of the final
*                        solution vector x.
*
*
*     Subroutines and functions used
*     ------------------------------
*
*     USER               aprod
*     LSQR               d2norm
*     BLAS               dcopy, dnrm2, dscal (see Lawson et al. below)
*
*
*     Precision
*     ---------
*
*     The number of iterations required by LSQR will usually decrease
*     if the computation is performed in higher precision.
*     At least 15-digit arithmetic should normally be used.
*     To convert LSQR and D2NORM between single and double precision,
*     change
*                        double precision
*                        dcopy, dnrm2, dscal
*     to the appropriate FORTRAN and BLAS equivalents.
*     Also change 'd+' or 'e+' in the parameter statement.
*
*
*     References
*     ----------
*
*     C.C. Paige and M.A. Saunders,  LSQR: An algorithm for sparse
*          linear equations and sparse least squares,
*          ACM Transactions on Mathematical Software 8, 1 (March 1982),
*          pp. 43-71.
*
*     C.C. Paige and M.A. Saunders,  Algorithm 583, LSQR: Sparse
*          linear equations and least-squares problems,
*          ACM Transactions on Mathematical Software 8, 2 (June 1982),
*          pp. 195-209.
*
*     C.L. Lawson, R.J. Hanson, D.R. Kincaid and F.T. Krogh,
*          Basic linear algebra subprograms for Fortran usage,
*          ACM Transactions on Mathematical Software 5, 3 (Sept 1979),
*          pp. 308-323 and 324-325.
*     ------------------------------------------------------------------
*
*
*     LSQR development:
*     22 Feb 1982: LSQR sent to ACM TOMS to become Algorithm 583.
*     15 Sep 1985: Final F66 version.  LSQR sent to "misc" in netlib.
*     13 Oct 1987: Bug (Robert Davies, DSIR).  Have to delete
*                     if ( (one + dabs(t)) .le. one ) GO TO 200
*                  from loop 200.  The test was an attempt to reduce
*                  underflows, but caused w(i) not to be updated.
*     17 Mar 1989: First F77 version.
*     04 May 1989: Bug (David Gay, AT&T).  When the second beta is zero,
*                  rnorm = 0 and
*                  test2 = arnorm / (anorm * rnorm) overflows.
*                  Fixed by testing for rnorm = 0.
*     05 May 1989: Sent to "misc" in netlib.
*     14 Mar 1990: Bug (John Tomlin via IBM OSL testing).
*                  Setting rhbar2 = rhobar**2 + dampsq can give zero
*                  if rhobar underflows and damp = 0.
*                  Fixed by testing for damp = 0 specially.
*     15 Mar 1990: Converted to lower case.
*     21 Mar 1990: d2norm introduced to avoid overflow in numerous
*                  items like  c = sqrt( a**2 + b**2 ).
*     04 Sep 1991: wantse added as an argument to LSQR, to make
*                  standard errors optional.  This saves storage and
*                  time when se(*) is not wanted.
*     13 Feb 1992: istop now returns a value in [1,5], not [1,7].
*                  1, 2 or 3 means that x solves one of the problems
*                  Ax = b,  min norm(Ax - b)  or  damped least squares.
*                  4 means the limit on cond(A) was reached.
*                  5 means the limit on iterations was reached.
*     07 Dec 1994: Keep track of dxmax = max_k norm( phi_k * d_k ).
*                  So far, this is just printed at the end.
*                  A large value (relative to norm(x)) indicates
*                  significant cancellation in forming
*                  x  =  D*f  =  sum( phi_k * d_k ).
*                  A large column of D need NOT be serious if the
*                  corresponding phi_k is small.
*     27 Dec 1994: Include estimate of alfa_opt in iteration log.
*                  alfa_opt is the optimal scale factor for the
*                  residual in the "augmented system", as described by
*                  A. Bjorck (1992),
*                  Pivoting and stability in the augmented system method,
*                  in D. F. Griffiths and G. A. Watson (eds.),
*                  "Numerical Analysis 1991",
*                  Proceedings of the 14th Dundee Conference,
*                  Pitman Research Notes in Mathematics 260,
*                  Longman Scientific and Technical, Harlow, Essex, 1992.
*
*
*     Michael A. Saunders                  mike@sol-michael.stanford.edu
*     Dept of Operations Research          na.Msaunders@na-net.ornl.gov
*     Stanford University
*     Stanford, CA 94305-4022              (415) 723-1875
*-----------------------------------------------------------------------

      external           d2norm, dnrm2, dcopy, dscal
      double precision   d2norm, dnrm2

*     Local variables

      logical            damped, extra
      integer            i, maxdx, nchar, nconv, nstop
      double precision   alpha, beta, bnorm,
     $                   cs, cs1, cs2, ctol,
     $                   delta, dknorm, dnorm, dxk, dxmax,
     $                   gamma, gambar, phi, phibar, psi,
     $                   res2, rho, rhobar, rhbar1,
     $                   rhs, rtol, sn, sn1, sn2,
     $                   t, tau, temp, test1, test2, test3,
     $                   theta, t1, t2, t3, xnorm1, z, zbar

      double precision   zero,           one
      parameter        ( zero = 0.0d+0,  one = 1.0d+0 )

      character*14       enter, exit
      character*53       msg(0:5)

      data               enter /' Enter LSQR.  '/
      data               exit  /' Exit  LSQR.  '/
      data               msg
     $ / 'The exact solution is  x = 0',
     $   'A solution to Ax = b was found, given atol, btol',
     $   'A least-squares solution was found, given atol',
     $   'A damped least-squares solution was found, given atol',
     $   'Cond(Abar) seems to be too large, given conlim',
     $   'The iteration limit was reached' /
*-----------------------------------------------------------------------


      eigcol=0            !jd

*     Initialize.

      if (nout .gt. 0) then
         write(nout, 1000) enter, m, n, damp, wantse,
     $                     atol, conlim, btol, itnlim
      end if

      damped =   damp .gt. zero
      extra  =  .true.     ! true for extra printing below.
      itn    =   0
      istop  =   0
      nstop  =   0
      maxdx  =   0
      ctol   =   zero
      if (conlim .gt. zero) ctol = one / conlim
      anorm  =   zero
      acond  =   zero
      dnorm  =   zero
      dxmax  =   zero
      res2   =   zero
      psi    =   zero
      xnorm  =   zero
      xnorm1 =   zero
      cs2    = - one
      sn2    =   zero
      z      =   zero

*     ------------------------------------------------------------------
*     Set up the first vectors u and v for the bidiagonalization.
*     These satisfy  beta*u = b,  alpha*v = A(transpose)*u.
*     ------------------------------------------------------------------
      do 10  i = 1, n
         v(i)  =  zero
         x(i)  =  zero
   10 continue

      if ( wantse ) then
         do 20  i = 1, n
            se(i) =  zero
   20    continue
      end if

      alpha  =   zero
      beta   =   dnrm2 ( m, u, 1 )

      if (beta .gt. zero) then
         call dscal ( m, (one / beta), u, 1 )
         call aprod ( 2, m, n, v, u, leniw, lenrw, iw, rw )
         alpha  =   dnrm2 ( n, v, 1 )
      end if

      if (alpha .gt. zero) then
         call dscal ( n, (one / alpha), v, 1 )
         if(ipestflag.eq.1)then
           eigcol=eigcol+1                   !jd
           if(eigcol.gt.nparx) return        !jd
           do ii=1,n                         !jd
             eigmat(ii,eigcol)=v(ii)         !jd
           end do                            !jd
         end if
         call dcopy ( n, v, 1, w, 1 )
      end if

      arnorm =   alpha * beta
      if (arnorm .eq. zero) go to 800

      rhobar =   alpha
      phibar =   beta
      bnorm  =   beta
      rnorm  =   beta

      if (nout   .gt.  0  ) then
         if ( damped ) then
             write(nout, 1300)
         else
             write(nout, 1200)
         end if
         test1  = one
         test2  = alpha / beta

         if ( extra ) then
            write(nout, 1400)
         end if
         write(nout, 1500) itn, x(1), rnorm, test1, test2
         write(nout, 1600)
      end if


*     ==================================================================
*     Main iteration loop.
*     ==================================================================
  100 itn    = itn + 1

*     ------------------------------------------------------------------
*     Perform the next step of the bidiagonalization to obtain the
*     next  beta, u, alpha, v.  These satisfy the relations
*                beta*u  =  A*v  -  alpha*u,
*               alpha*v  =  A(transpose)*u  -  beta*v.
*     ------------------------------------------------------------------
      call dscal ( m, (- alpha), u, 1 )
      call aprod ( 1, m, n, v, u, leniw, lenrw, iw, rw )
      beta   =   dnrm2 ( m, u, 1 )

*     Accumulate  anorm = || Bk ||
*                       =  sqrt( sum of  alpha**2 + beta**2 + damp**2 ).

      temp   =   d2norm( alpha, beta )
      temp   =   d2norm( temp , damp )
      anorm  =   d2norm( anorm, temp )

      if (beta .gt. zero) then
         call dscal ( m, (one / beta), u, 1 )
         call dscal ( n, (- beta), v, 1 )
         call aprod ( 2, m, n, v, u, leniw, lenrw, iw, rw )
         alpha  =   dnrm2 ( n, v, 1 )
         if (alpha .gt. zero) then
            call dscal ( n, (one / alpha), v, 1 )
           if(ipestflag.eq.1)then
             eigcol=eigcol+1                   !jd
             if(eigcol.gt.nparx) return        !jd
             do ii=1,n                         !jd
               eigmat(ii,eigcol)=v(ii)         !jd
             end do                            !jd
           end if
         end if
      end if

*     ------------------------------------------------------------------
*     Use a plane rotation to eliminate the damping parameter.
*     This alters the diagonal (rhobar) of the lower-bidiagonal matrix.
*     ------------------------------------------------------------------
      rhbar1 = rhobar
      if ( damped ) then
         rhbar1 = d2norm( rhobar, damp )
         cs1    = rhobar / rhbar1
         sn1    = damp   / rhbar1
         psi    = sn1 * phibar
         phibar = cs1 * phibar
      end if

*     ------------------------------------------------------------------
*     Use a plane rotation to eliminate the subdiagonal element (beta)
*     of the lower-bidiagonal matrix, giving an upper-bidiagonal matrix.
*     ------------------------------------------------------------------
      rho    =   d2norm( rhbar1, beta )
      cs     =   rhbar1 / rho
      sn     =   beta   / rho
      theta  =   sn * alpha
      rhobar = - cs * alpha
      phi    =   cs * phibar
      phibar =   sn * phibar
      tau    =   sn * phi

*     ------------------------------------------------------------------
*     Update  x, w  and (perhaps) the standard error estimates.
*     ------------------------------------------------------------------
      t1     =   phi   / rho
      t2     = - theta / rho
      t3     =   one   / rho
      dknorm =   zero

      if ( wantse ) then
         do 200  i =  1, n
            t      =  w(i)
            x(i)   =  t1*t  +  x(i)
            w(i)   =  t2*t  +  v(i)
            t      = (t3*t)**2
            se(i)  =  t     +  se(i)
            dknorm =  t     +  dknorm
  200    continue
      else
         do 220  i =  1, n
            t      =  w(i)
            x(i)   =  t1*t  +  x(i)
            w(i)   =  t2*t  +  v(i)
            dknorm = (t3*t)**2  +  dknorm
  220    continue
      end if

*     ------------------------------------------------------------------
*     Monitor the norm of d_k, the update to x.
*     dknorm = norm( d_k )
*     dnorm  = norm( D_k ),        where   D_k = (d_1, d_2, ..., d_k )
*     dxk    = norm( phi_k d_k ),  where new x = x_k + phi_k d_k.
*     ------------------------------------------------------------------
      dknorm = sqrt( dknorm )
      dnorm  = d2norm( dnorm, dknorm )
      dxk    = abs( phi * dknorm )
      if (dxmax .lt. dxk ) then
          dxmax   =  dxk
          maxdx   =  itn
      end if

*     ------------------------------------------------------------------
*     Use a plane rotation on the right to eliminate the
*     super-diagonal element (theta) of the upper-bidiagonal matrix.
*     Then use the result to estimate  norm(x).
*     ------------------------------------------------------------------
      delta  =   sn2 * rho
      gambar = - cs2 * rho
      rhs    =   phi    - delta * z
      zbar   =   rhs    / gambar
      xnorm  =   d2norm( xnorm1, zbar  )
      gamma  =   d2norm( gambar, theta )
      cs2    =   gambar / gamma
      sn2    =   theta  / gamma
      z      =   rhs    / gamma
      xnorm1 =   d2norm( xnorm1, z     )

*     ------------------------------------------------------------------
*     Test for convergence.
*     First, estimate the norm and condition of the matrix  Abar,
*     and the norms of  rbar  and  Abar(transpose)*rbar.
*     ------------------------------------------------------------------
      acond  =   anorm * dnorm
      res2   =   d2norm( res2 , psi    )
      rnorm  =   d2norm( res2 , phibar )
      arnorm =   alpha * abs( tau )

*     Now use these norms to estimate certain other quantities,
*     some of which will be small near a solution.

      alfopt =   sqrt( rnorm / (dnorm * xnorm) )
      test1  =   rnorm /  bnorm
      test2  =   zero
      if (rnorm .gt. zero) test2 = arnorm / (anorm * rnorm)
      test3  =   one   /  acond
      t1     =   test1 / (one  +  anorm * xnorm / bnorm)
      rtol   =   btol  +  atol *  anorm * xnorm / bnorm

*     The following tests guard against extremely small values of
*     atol, btol  or  ctol.  (The user may have set any or all of
*     the parameters  atol, btol, conlim  to zero.)
*     The effect is equivalent to the normal tests using
*     atol = relpr,  btol = relpr,  conlim = 1/relpr.

      t3     =   one + test3
      t2     =   one + test2
      t1     =   one + t1
      if (itn .ge. itnlim) istop = 5
      if (t3  .le. one   ) istop = 4
      if (t2  .le. one   ) istop = 2
      if (t1  .le. one   ) istop = 1

*     Allow for tolerances set by the user.

      if (test3 .le. ctol) istop = 4
      if (test2 .le. atol) istop = 2
      if (test1 .le. rtol) istop = 1

*     ------------------------------------------------------------------
*     See if it is time to print something.
*     ------------------------------------------------------------------
      if (nout  .le.  0       ) go to 600
      if (n     .le. 40       ) go to 400
      if (itn   .le. 10       ) go to 400
      if (itn   .ge. itnlim-10) go to 400
      if (mod(itn,10) .eq. 0  ) go to 400
      if (test3 .le.  2.0*ctol) go to 400
      if (test2 .le. 10.0*atol) go to 400
      if (test1 .le. 10.0*rtol) go to 400
      if (istop .ne.  0       ) go to 400
      go to 600

*     Print a line for this iteration.
*     "extra" is for experimental purposes.

  400 if ( extra ) then
         write(nout, 1500) itn, x(1), rnorm, test1, test2, anorm, acond
     $                   , phi, dknorm, dxk, alfopt
      else
         write(nout, 1500) itn, x(1), rnorm, test1, test2, anorm, acond
      end if
      if (mod(itn,10) .eq. 0) write(nout, 1600)

*     ------------------------------------------------------------------
*     Stop if appropriate.
*     The convergence criteria are required to be met on  nconv
*     consecutive iterations, where  nconv  is set below.
*     Suggested value:  nconv = 1, 2  or  3.
*     ------------------------------------------------------------------
  600 if (istop .eq. 0) then
         nstop  = 0
      else
         nconv  = 1
         nstop  = nstop + 1
         if (nstop .lt. nconv  .and.  itn .lt. itnlim) istop = 0
      end if
      if (istop .eq. 0) go to 100

*     ==================================================================
*     End of iteration loop.
*     ==================================================================

*     Finish off the standard error estimates.

      if ( wantse ) then
         t    =   one
         if (m .gt. n)  t = m - n
         if ( damped )  t = m
         t    =   rnorm / sqrt( t )

         do 700  i = 1, n
            se(i)  = t * sqrt( se(i) )
  700    continue
      end if

*     Decide if istop = 2 or 3.
*     Print the stopping condition.

  800 if (damped  .and.  istop .eq. 2) istop = 3
      if (nout .gt. 0) then
         write(nout, 2000) exit, istop, itn,
     $                     exit, anorm, acond,
     $                     exit, bnorm, xnorm,
     $                     exit, rnorm, arnorm
         write(nout, 2100) exit, dxmax, maxdx,
     $                     exit, dxmax/(xnorm + 1.0d-20)
         write(nout, 3000) exit, msg(istop)
      end if

  900 return

*     ------------------------------------------------------------------
 1000 format(// 1p, a, '     Least-squares solution of  Ax = b'
     $    / ' The matrix  A  has', i7, ' rows   and', i7, ' columns'
     $    / ' damp   =', e22.14, 3x,        'wantse =', l10
     $    / ' atol   =', e10.2, 15x,        'conlim =', e10.2
     $    / ' btol   =', e10.2, 15x,        'itnlim =', i10)
 1200 format(// '   Itn       x(1)           Function',
     $   '     Compatible   LS        Norm A    Cond A')
 1300 format(// '   Itn       x(1)           Function',
     $   '     Compatible   LS     Norm Abar Cond Abar')
 1400 format(80x, '    phi    dknorm   dxk  alfa_opt')
 1500 format(1p, i6, 2e17.9, 4e10.2, e9.1, 3e8.1)
 1600 format(1x)
 2000 format(/ 1p, a, 5x, 'istop  =', i2,   15x, 'itn    =', i8
     $       /     a, 5x, 'anorm  =', e12.5, 5x, 'acond  =', e12.5
     $       /     a, 5x, 'bnorm  =', e12.5, 5x, 'xnorm  =', e12.5
     $       /     a, 5x, 'rnorm  =', e12.5, 5x, 'arnorm =', e12.5)
 2100 format(  1p, a, 5x, 'max dx =', e8.1 , ' occurred at itn ', i8,
     $       /     a, 5x, '       =', e8.1 , '*xnorm' )
 3000 format( a, 5x, a )

*     End of LSQR
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      function          d2norm( a, b )

      double precision  d2norm, a, b

*     ------------------------------------------------------------------
*     d2norm  returns  sqrt( a**2 + b**2 )  with precautions
*     to avoid overflow.
*
*     21 Mar 1990: First version.
*     ------------------------------------------------------------------

      intrinsic         abs, sqrt
      double precision  scale
      double precision  zero
      parameter       ( zero = 0.0d+0 )

      scale  = abs( a ) + abs( b )
      if (scale .eq. zero) then
         d2norm = zero
      else
         d2norm = scale * sqrt( (a/scale)**2   +  (b/scale)**2 )
      end if

*     end of d2norm
      end



      subroutine bprod(mode,m,n,x,y,leniw,lenrw,iw,rw)


      use lsqr_data_exchange, only : tbx
      implicit none

      integer mode,m,n,leniw,lenrw
      integer i,j
      integer iw(leniw)
      double precision rw(lenrw)
      double precision x(n),y(m)
      double precision sum

      if(mode.eq.1)then
        do i=1,m
          sum=y(i)
          do j=1,n
            sum=sum+tbx(i,j)*x(j)
          end do
          y(i)=sum
        end do
      else
        do j=1,n
          sum=x(j)
          do i=1,m
            sum=sum+tbx(i,j)*y(i)
          end do
          x(j)=sum
        end do
      end if

      return
      end




C###############################################################################

C                END OF LSQR CODE AND USER-SUPPLIED SUBROUTINE

C###############################################################################





C -- What about when JACFILE does not equal zero? What Jacobian matrix should we read?
C    Should itbe the temporary base Jacobian file?
C    Or should we simply store super parameters - best fit super parameters - on each
C    PEST iteration and dispense with making this file.
C -- Check that reading the temporary base Jacobian file works ok where we just do
C    one iteration.
