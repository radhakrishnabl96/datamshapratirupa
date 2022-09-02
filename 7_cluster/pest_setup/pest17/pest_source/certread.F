
        subroutine read_uncert_data_file(ifail,iunit,junit,itype,jtype,
     +  nobs,vardim,covdim,var,covar,atype,covdatfile,amessage,cline,
     +  aobs)

C -- Subroutine READ_UNCERT_DATA_FILE reads an uncertainty data file.

        implicit none

        integer    :: ifail         ! return as zero if successful
        integer    :: iunit         ! unit number from which file will be read
        integer    :: junit         ! unit number from which another file can be read
        integer    :: itype         ! 1 if only variances permitted; 2 if also covariances
        integer    :: jtype         ! return as 1 if only variances supplied; 2 if also covars
        integer    :: nobs          ! number of items for which uncertainty required
        integer    :: vardim        ! dimension of variance vector
        integer    :: covdim        ! dimension of covariance matrix

        double precision  :: var(vardim)            ! variance vector
        double precision  :: covar(covdim,covdim)   ! covariance matrix

        character*(*)  :: atype      ! name of data type for which information is sought
        character*(*)  :: covdatfile ! name of the covariance data file
        character*(*)  :: amessage   ! string to write error message to
        character*(*)  :: cline      ! a general text string
        character*(*)  :: aobs(nobs) ! names of data elements

        integer          :: iline,iobs,j,jfail,nb,currentobs,i,icount,
     +                      priorflag,k,ient
        integer          :: index1,index2,nexpect
        integer          :: junit1
        integer          :: lw(2),rw(2)
        double precision :: vmul,stdmul,dtemp

        character*4      :: aext
        character*10     :: aline
        character*10     :: aatemp
        character*20     :: atemp,ablock,akey
        character*200    :: afile,pestfile,matfile,bfile,listfile


C -- Initialisation

        ifail=0
        call addquote(covdatfile,afile)
        iline=0

        if(itype.eq.1)then
          do iobs=1,nobs
            var(iobs)=-1.1d35
          end do
        else
          covar=0.0d0
          do iobs=1,nobs
            covar(iobs,iobs)=-1.1d35
          end do
        end if
        jtype=1

        currentobs=0
        pestfile=' '

C -- We now look for a block.

100     iline=iline+1
        read(iunit,'(a)',err=9000,end=5000) cline
        if(cline.eq.' ') go to 100
        if(cline(1:1).eq.'#') go to 100
        call writint(aline,iline)
        cline=adjustl(cline)
        call linspl(jfail,2,lw,rw,cline)
        if(jfail.ne.0) go to 9100
        atemp=cline(lw(1):rw(1))
        call lowcas(atemp)
        if(atemp.ne.'start')then
          write(amessage,120) trim(aline),trim(afile)
120       format(' Block should commence with START at line ',
     +    a,' of file ',a,'.')
          go to 9800
        end if
        ablock=cline(lw(2):rw(2))
        call lowcas(ablock)
        if(ablock(1:12).eq.'pest_control')then
          if(atype.eq.'parameter')then
            call writint(aline,iline)
            write(amessage,1005) trim(aline),trim(afile)
1005        format(' A PEST_CONTROL_FILE block is cited on line ',a,
     +      ' of file ',a,'; this is only allowed for observations - ',
     +      'not parameters.')
            go to 9800
          end if
          vmul=1.0d0
1010      continue
          iline=iline+1
          call writint(aline,iline)
          read(iunit,'(a)',err=9000,end=9050) cline
          if(cline.eq.' ') go to 1010
          if(cline(1:1).eq.'#') go to 1010
          call spacesub(cline)
          call linspl(jfail,2,lw,rw,cline)
          if(jfail.ne.0) go to 9100
          akey=cline(lw(1):rw(1))
          call lowcas(akey)
          if(akey.eq.'file')then
            pestfile=cline(lw(2):)
            call remchar(pestfile,char(211))
#ifndef UNIX
            call lowcas(pestfile)
#endif
            nb=len_trim(pestfile)
            i=nb-3
            if(i.lt.1) i=1
            aext=pestfile(i:nb)
            call lowcas(aext)
            if(aext.ne.'.pst')then
              write(amessage,1015) trim(aline),trim(afile)
1015          format(' PEST control file must have an extension of ',
     +        '".pst" at line ',a,' of file ',a,'.')
              go to 9800
            end if
            go to 1010
          else if(akey.eq.'variance_multiplier')then
            atemp=cline(lw(2):rw(2))
            call drealrd(jfail,atemp,vmul)
            if(jfail.ne.0)then
              write(amessage,1020) trim(aline),trim(afile)
1020          format(' Error reading variance multiplier from line ',
     +        a,' of file ',a,'.')
              go to 9800
            end if
            if(vmul.le.0.0d0)then
              write(amessage,1030) trim(aline),trim(afile)
1030          format(' Variance multiplier must be greater than ',
     +        'zero at line ',a,' of file ',a,'.')
              go to 9800
            end if
            go to 1010
          else if(akey.eq.'end')then
            if(pestfile.eq.' ')then
              write(amessage,1035) trim(afile)
1035          format(' No FILE keyword has been provided in ',
     +        '"PEST_CONTROL_FILE" block of file ',a,'.')
              go to 9800
            end if
            priorflag=1
            call read_pest_control_file(jfail,junit,itype,jtype,
     +      priorflag,vardim,covdim,nobs,vmul,var,covar,pestfile,
     +      amessage,aobs,cline)
            if(jfail.gt.0) go to 9800
            if((jfail.eq.-1).or.(jfail.eq.-11))then
              call addquote(pestfile,bfile)
              write(amessage,1200) trim(bfile)
1200          format(' Warning: PEST control file ',a,' cites at ',
     +        'least one covariance matrix file in "observation ',
     +        'groups" section. For present version ',
     +        'of PARAMERR this covariance file is ignored and ',
     +        'observation weights are used instead for observation ',
     +        'variance computation.')
              call writmess(6,amessage)
              if(jfail.gt.-10) write(6,*)
            end if
            if(jfail.le.-10)then
              call addquote(pestfile,bfile)
              write(amessage,1210) trim(bfile)
1210          format(' Warning: at least one weight in PEST control ',
     +        'file ',a,' is supplied as zero. This was altered to ',
     +        '1.0E-30 for the purpose of observation variance ',
     +        'calculation.')
              call writmess(6,amessage)
              write(6,*)
            end if
            go to 100
          else
            call writint(aline,iline)
            write(amessage,1190) trim(akey),trim(aline),trim(afile)
1190        format(' Unknown keyword "',a,'" at line ',a,' of file ',
     +      a,'.')
            go to 9800
          end if

        else if(ablock(1:12).eq.'standard_dev')then
          stdmul=1.0d0
          ient=0
2010      continue
          iline=iline+1
          read(iunit,'(a)',err=9000,end=9250) cline
          if(cline.eq.' ') go to 2010
          if(cline(1:1).eq.'#') go to 2010
          ient=ient+1
          call linspl(ifail,2,lw,rw,cline)
          if(ifail.ne.0) go to 9100
          atemp=cline(lw(1):rw(1))
          call lowcas(atemp)
          if(atemp.eq.'end')then
            atemp=cline(lw(2):rw(2))
            call lowcas(atemp)
            if(atemp(1:12).eq.'standard_dev') go to 100
          end if
          if(atemp.eq.'std_multiplier')then
            if(ient.ne.1)then
              call writint(aline,iline)
              write(amessage,2015) trim(aline),trim(afile)
2015          format(' If a STD_MULTIPLIER is supplied, it must be ',
     +        'the first entry in a STANDARD_DEVIATION block. This ',
     +        'is violated on line ',a,' of file ',a,'.')
              go to 9800
            end if
            atemp=cline(lw(2):rw(2))
            call drealrd(jfail,atemp,stdmul)
            if(jfail.ne.0)then
              call writint(aline,iline)
              write(amessage,2020) trim(aline),trim(afile)
2020          format(' Cannot read standard deviation multiplier from ',
     +        'line ',a,' of file ',a,'.')
              go to 9800
            else
              if(stdmul.le.0.0d0)then
                call writint(aline,iline)
                write(amessage,2030) trim(aline),trim(afile)
2030            format(' Standard deviation multiplier must be ',
     +          'positive at line ',a,' of file ',a,'.')
                go to 9800
              end if
            end if
            stdmul=stdmul*stdmul
            go to 2010
          end if
          call whichone(jfail,nobs,currentobs,aobs,atemp)
          if(jfail.ne.0) go to 2010
          atemp=cline(lw(2):rw(2))
          call drealrd(jfail,atemp,dtemp)
          if(jfail.ne.0)then
            call writint(aline,iline)
            write(amessage,2050) trim(aline),trim(afile)
2050        format(' Cannot read standard deviation at line ',a,
     +      ' of file ',a,'.')
            go to 9800
          end if
          if(dtemp.le.0.0)then
            call writint(aline,iline)
            write(amessage,2060) trim(aline),trim(afile)
2060        format(' Standard deviation must be positive at ',
     +      'line ',a,' of file ',a,'.')
            go to 9800
          end if
          dtemp=dtemp*dtemp*stdmul
          if(itype.eq.1)then
            if(var(currentobs).gt.-1.0d35) go to 9300
            var(currentobs)=dtemp
          else
            if(covar(currentobs,currentobs).gt.-1.0d35) go to 9300
            covar(currentobs,currentobs)=dtemp
          end if
          go to 2010

        else if(ablock(1:14).eq.'covariance_mat')then
          if(itype.eq.1)then
            write(amessage,3010) trim(afile),trim(atype)
3010        format(' A COVARIANCE MATRIX block exists in file ',
     +      a,'; this is not presently allowed for characterisation ',
     +      'of ',a,' data variance.')
            go to 9800
          end if
          jtype=2
          vmul=1.0d0
          matfile=' '
          listfile=' '
          junit1=0
          index1=0
          index2=0
          nexpect=0
3020      iline=iline+1
          read(iunit,'(a)',err=9000,end=9050) cline
          if(cline.eq.' ') go to 3020
          if(cline(1:1).eq.'#') go to 3020
          call writint(aline,iline)
          call linspl(jfail,2,lw,rw,cline)
          if(jfail.ne.0) go to 9100
          akey=cline(lw(1):rw(1))
          call lowcas(akey)
          if(akey.eq.'end')then
            if(matfile.eq.' ')then
              call writint(aline,iline)
              write(amessage,3011) trim(aline),trim(afile)
3011          format(' The COVARIANCE MATRIX block ',
     +        'terminating at line ',a,' of file ',a,' does not ',
     +        'include the name of a matrix file.')
              go to 9800
            end if
            if(index1.ne.0)then
              if(index2.eq.0)then
                write(amessage,3012)
3012            format(' If one of the "first_parameter" or ',
     +          '"last_parameter" keywords are supplied, then both ',
     +          'of them must be supplied in a COVARIANCE_MATRIX ',
     +          'block.')
                go to 9800
              end if
            end if
            if(index2.ne.0)then
              if(index1.eq.0)then
                write(amessage,3012)
                go to 9800
              end if
            end if
            if(index1.gt.0)then
              if(index2.lt.index1)then
                write(amessage,3013)
3013            format(' Error in COVARIANCE matrix block. ',
     +          'Parameter provided with "last_parameter" ',
     +          'keyword does not follow that provided with ',
     +          '"first_parameter" keyword in the PEST control file.')
                go to 9800
              end if
              if(listfile.ne.' ')then
                write(amessage,3014)
3014            format(' A "parameter_list_file" keyword cannot ',
     +          'be supplied in a COVARIANCE_MATRIX block if a ',
     +          '"first_parameter" and/or "last_parameter" keyword is ',
     +          'also supplied.')
                go to 9800
              end if
              nexpect=index2-index1+1
            end if
            if(listfile.ne.' ') junit1=junit
            if((nexpect.eq.0).and.(junit1.eq.0))then
              call read_covar_mat_file(jfail,junit,covdim,nobs,vmul,
     +        covar,matfile,amessage,aobs,cline)
            else
              call read_mat_and_list_file(jfail,junit,covdim,nobs,vmul,
     +        covar,matfile,amessage,aobs,cline,index1,nexpect,
     +        listfile,junit1)
            end if
            if(jfail.ne.0) go to 9800
            go to 100
          else if(akey.eq.'file')then
            matfile=cline(lw(2):)
            call remchar(matfile,'''')
            call remchar(matfile,'"')
            matfile=adjustl(matfile)
#ifndef UNIX
            call lowcas(matfile)
#endif
            go to 3020
          else if(akey.eq.'variance_multiplier')then
            atemp=cline(lw(2):rw(2))
            call drealrd(jfail,atemp,vmul)
            if(jfail.ne.0)then
              write(amessage,1020) trim(aline),trim(afile)
              go to 9800
            end if
            if(vmul.le.0.0d0)then
              write(amessage,1030) trim(aline),trim(afile)
              go to 9800
            end if
            go to 3020
          else if(akey.eq.'first_parameter')then
            atemp=cline(lw(2):rw(2))
            call lowcas(atemp)
            call whichone(jfail,nobs,index1,aobs,atemp)
            if(jfail.ne.0) then
              aatemp='first'
              write(amessage,3021) trim(atemp),trim(aatemp)
3021          format(' Parameter "',a,'" supplied with the ',
     +        '"',a,'_parameter" keyword in a COVARIANCE_MATRIX ',
     +        'block is not featured as ',
     +        'an adjustable parameter in the PEST control file.')
              go to 9800
            end if
            go to 3020
          else if(akey.eq.'last_parameter')then
            atemp=cline(lw(2):rw(2))
            call lowcas(atemp)
            call whichone(jfail,nobs,index2,aobs,atemp)
            if(jfail.ne.0) then
              aatemp='last'
              write(amessage,3021) trim(atemp),trim(aatemp)
              go to 9800
            end if
            go to 3020
          else if(akey.eq.'parameter_list_file')then
            listfile=cline(lw(2):)
            call remchar(listfile,'''')
            call remchar(listfile,'"')
            listfile=adjustl(listfile)
#ifndef UNIX
            call lowcas(listfile)
#endif
            go to 3020
          else
            write(amessage,3025) trim(aline),trim(afile)
3025        format(' Unrecognised keyword at line ',a,' of file ',a,'.')
            go to 9800
          end if

        else
          write(amessage,3100) trim(aline),trim(afile)
3100      format(' Unrecognised block name at line ',a,' of file ',
     +    a,'.')
          go to 9800

        end if

C -- Tidying up.

5000    continue

        icount=0
        if(itype.eq.1)then
          do iobs=1,nobs
            if(var(iobs).lt.-1.0d35) icount=icount+1
          end do
        else
          do iobs=1,nobs
            if(covar(iobs,iobs).lt.-1.0d35) icount=icount+1
          end do
        end if
        if(icount.ne.0)then
          if(atype.eq.'parameter')then
            write(amessage,5011) trim(atype),trim(afile),trim(atype)
5011        format(' Variances have not been provided for all ',
     +      'adjustable ',a,
     +      's in file ',a,' (and any files cited therein). Missing ',
     +      a,'s include:-')
          else
            write(amessage,5010) trim(atype),trim(afile),trim(atype)
5010        format(' Variances have not been provided for all ',a,
     +      's and/or prior information in file ',a,
     +      ' (and any files cited therein). Missing ',
     +      a,'s include:-')
          end if
          nb=len(amessage)
          do iobs=1,nobs
            if(itype.eq.1)then
              if(var(iobs).gt.-1.0d35) cycle
            else if(itype.eq.2)then
              if(covar(iobs,iobs).gt.-1.0d35) cycle
            end if
            k=len_trim(amessage)
            j=len_trim(aobs(iobs))+3
            if(k+j.gt.nb) go to 5050
            write(amessage(k+2:),5015) trim(aobs(iobs))
5015        format('"',a,'"')
          end do
5050      continue
          go to 9800
        end if

        return

9000    call writint(aline,iline)
        write(amessage,9010) trim(aline),trim(afile)
9010    format(' Error reading line ',a,' of file ',a,'.')
        go to 9800
9050    write(amessage,9060) trim(afile)
9060    format(' Unexpected end encountered to file ',a,'.')
        go to 9800
9100    call writint(aline,iline)
        write(amessage,9110) trim(aline),trim(afile)
9110    format(' Insufficient entries on line ',a,
     +  ' of file ',a,'.')
        go to 9800
9250    write(amessage,9260) trim(afile)
9260    format(' Cannot find "END STANDARD_DEVIATION" line in file ',
     +  a,'.')
        go to 9800
9300    write(amessage,9310) trim(atype),trim(aobs(currentobs)),
     +  trim(afile)
9310    format(' More than one variance has been supplied for ',a,' "',
     +  a,'" in file ',a,' (and any files cited therein).')
        go to 9800

9800    ifail=1

        return

        end


        subroutine read_pest_control_file(jfail,iunit,itype,jtype,
     +  priorflag,vecdim,covdim, nobs,vmul,var,covar,pestfile,
     +  amessage,aobs,cline)

C -- Subroutine read_pest_control_file reads weights from a PEST control file
C    and converts them to variances.

        implicit none

        integer     :: jfail      ! returned as zero if no error
        integer     :: iunit      ! unit number to open PEST control file
        integer     :: itype      ! 1 if only variances permitted; 2 if also covariances
        integer     :: jtype      ! return as 1 if only variances supplied; 2 if also covars

        integer        :: priorflag  ! 0 if ignore prior info; 1 otherwise
        integer        :: nobs       ! dimensions of aobs

        integer        :: vecdim     ! dimension of var vector
        integer        :: covdim     ! dimension of covar array

        double precision  :: vmul                 ! variance multiplier
        double precision  :: var(vecdim)          ! variance vector
        double precision  :: covar(covdim,covdim) ! covariance matrix

        character*(*)    :: pestfile   ! name of pest control file
        character*(*)    :: amessage   ! text to write error message to
        character*(*)    :: cline      ! general text string
        character*(*)    :: aobs(nobs) ! names of observations/prior information

        integer       :: currentobs,ifail,numcov,ierr,iline,ireg,npar,
     +                   npargp,nprior,nobsgp,i,icountobs,iwarn,
     +                   iprior,ie,nobs1
        integer       :: lw(5),rw(5)

        double precision :: dtemp
        character*10  :: aline
        character*20  :: atemp,aaobs
        character*200 :: afile
        character*600 :: pline


C -- Initialisation.

        jfail=0
        ireg=0
        iwarn=0
        jtype=1     ! may change with future programming
        iline=0
        currentobs=0
        numcov=0
        call addquote(pestfile,afile)
C        write(6,10) trim(afile)
C10      format(' - reading PEST control file ',a,'...')

C -- The PEST control file is opened.

        open(unit=iunit,file=pestfile,status='old',iostat=ierr)
        if(ierr.ne.0)then
          write(amessage,50) trim(afile)
50        format(' Cannot open PEST control file ',a,'.')
          go to 9800
        end if

C -- The control data section of the file is found.

        do
          iline=iline+1
          read(iunit,'(a)',err=9000,end=200) cline
          cline=adjustl(cline)
          call lowcas(cline)
          if(cline(1:1).ne.'*') cycle
          if(index(cline,'control').ne.0) go to 230
          cycle
200       write(amessage,210) trim(afile)
210       format(' Cannot find "control data" section of ',
     +    'PEST control file ',a,'.')
          go to 9800
        end do
230     continue
        iline=iline+1
        read(iunit,'(a)',err=9000,end=9050) cline
        call lowcas(cline)
        call linspl(ifail,2,lw,rw,cline)
        if(ifail.ne.0) go to 9100
        atemp=cline(lw(2):rw(2))
        call lowcas(atemp)
        if(index(atemp,'regul').ne.0) ireg=1
        iline=iline+1
        read(iunit,'(a)',err=9000,end=9050) cline
        call linspl(ifail,5,lw,rw,cline)
        if(ifail.ne.0) go to 9100
        atemp=cline(lw(1):rw(1))
        call intread(ifail,atemp,npar)
        if(ifail.ne.0) go to 9000
        atemp=cline(lw(2):rw(2))
        call intread(ifail,atemp,nobs1)
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

C -- We now find the "observation groups" section of this file.

        do
          iline=iline+1
          read(iunit,'(a)',err=9000,end=300) cline
          cline=adjustl(cline)
          if(cline(1:1).ne.'*') cycle
          call lowcas(cline)
          if(index(cline,'observation gr').ne.0) go to 330
          cycle
300       write(amessage,310) trim(afile)
310       format(' Cannot find "observation groups" section of PEST ',
     +    'control file ',a,'.')
          go to 9800
        end do
330     do i=1,nobsgp
          iline=iline+1
          read(iunit,'(a)',err=9000,end=9050) cline
          if(cline.eq.' ') go to 9300
          if(cline(1:1).eq.'#') go to 9300
          if(cline(1:1).eq.'*') go to 9300
          call linspl(ifail,2,lw,rw,cline)
          if(ifail.eq.0)then
            numcov=numcov+1
          end if
        end do

C -- We now find the "observation data" section of this file.

        do
          iline=iline+1
          read(iunit,'(a)',err=9000,end=400) cline
          cline=adjustl(cline)
          if(cline(1:1).ne.'*') cycle
          call lowcas(cline)
          if(index(cline,'observation data').ne.0) go to 430
          cycle
400       write(amessage,410) trim(afile)
410       format(' Cannot find "observation data" section of file ',
     +    a,'.')
          go to 9800
        end do
430     continue
        icountobs=0
        do i=1,nobs1
          iline=iline+1
          read(iunit,'(a)',err=9000,end=9050) cline
          if((cline.eq.' ').or.(cline(1:1).eq.'#')) go to 9300
          call linspl(ifail,4,lw,rw,cline)
          if(ifail.ne.0) go to 9100
          aaobs=cline(lw(1):rw(1))
          call lowcas(aaobs)
          call whichone(ifail,nobs,currentobs,aobs,aaobs)
          if(ifail.eq.0)then
            atemp=cline(lw(3):rw(3))
            call drealrd(ifail,atemp,dtemp)
            if(ifail.ne.0)then
              call writint(aline,iline)
              write(amessage,433) trim(aline),trim(afile)
433           format(' Cannot read observation weight from line ',a,
     +        ' of PEST control file ',a,'.')
              go to 9800
            end if
            if(dtemp.eq.0.0d0) then
              dtemp=1.0d-30
              iwarn=1
            else if(dtemp.lt.0.0d0)then
              call writint(aline,iline)
              write(amessage,435) trim(aline),trim(afile)
435           format(' Negative observation weight not permitted at ',
     +        'line ',a,' of PEST control file ',a,'.')
              go to 9800
            end if
            icountobs=icountobs+1
            dtemp=dtemp*dtemp
            if(itype.eq.1)then
              if(var(currentobs).gt.-1.0d35) go to 9400
              var(currentobs)=vmul/dtemp
            else
              if(covar(currentobs,currentobs).gt.-1.0d35) go to 9400
              covar(currentobs,currentobs)=vmul/dtemp
            end if
          end if
        end do
        if(nprior.eq.0) go to 1000
        if(icountobs.eq.nobs) go to 1000
        if(priorflag.eq.0) go to 1000

C -- We now look for the prior information section.

        do
          iline=iline+1
          read(iunit,'(a)',err=9000,end=500) cline
          if(cline(1:1).ne.'*') cycle
          call lowcas(cline)
          if(index(cline,'prior inf').ne.0) go to 530
          cycle
500       write(amessage,510) trim(afile)
510       format(' Cannot find "prior information" section of PEST ',
     +    'control file ',a,'.')
          go to 9800
        end do
530     continue
        read(iunit,'(a)',err=9000,end=9300) cline
        do iprior=1,nprior
          pline=cline
          cline=' '
540       iline=iline+1
          read(iunit,'(a)',err=9000,end=545) cline
          cline=adjustl(cline)
          if(cline(1:1).eq.'&')then
            cline(1:1)=' '
            pline=trim(pline)//trim(cline)
            go to 540
          end if
545       continue
          if(pline.eq.' ') go to 9300
          call linspl(ifail,1,lw,rw,pline)
          aaobs=pline(lw(1):rw(1))
          call lowcas(aaobs)
          call whichone(ifail,nobs,currentobs,aobs,aaobs)
          if(ifail.eq.0)then
            ie=index(pline,'=')
            if(ie.eq.0) go to 9300
            pline=pline(ie+1:)
            call linspl(ifail,2,lw,rw,pline)
            if(ifail.ne.0) go to 9300
            atemp=pline(lw(2):rw(2))
            call drealrd(ifail,atemp,dtemp)
            if(ifail.ne.0) go to 9300
            if(dtemp.le.0.0d0) then
              dtemp=1.0d-30
              iwarn=1
            end if
            icountobs=icountobs+1
            dtemp=dtemp*dtemp
            if(itype.eq.1)then
              if(var(currentobs).gt.-1.0d35) go to 9500
              var(currentobs)=vmul/dtemp
            else
              if(covar(currentobs,currentobs).gt.-1.0d35) go to 9500
              covar(currentobs,currentobs)=vmul/dtemp
            end if
          end if
        end do

1000    continue
        close(unit=iunit)
C        write(6,1010) trim(afile)
C1010    format(' - file ',a,' read ok.')


        go to 9900

9000    call writint(aline,iline)
        write(amessage,9010) trim(aline),trim(afile)
9010    format(' Error reading line ',a,' of PEST control file ',
     +  a,'.')
        go to 9800
9050    write(amessage,9060) trim(afile)
9060    format(' Unexpected end encountered to PEST control file ',
     +  a,'.')
        go to 9800
9100    call writint(aline,iline)
        write(amessage,9150) trim(aline),trim(afile)
9150    format(' Insufficient entries on line ',a,' of PEST control ',
     +  'file ',a,'.')
        go to 9800
9300    write(amessage,9310) trim(afile)
9310    format(' File ',a,' is an illegal PEST control file: check ',
     +  'it with PESTCHEK.')
        go to 9800
9400    call writint(aline,iline)
        write(amessage,9410) trim(aobs(currentobs)),
     +  trim(aline),trim(afile)
9410    format(' Observation "',a,'" cited at line ',a,' of PEST ',
     +  'control file ',a,' has already been assigned a variance.')
        go to 9800
9500    call writint(aline,iline)
        write(amessage,9510) trim(aobs(currentobs)),
     +  trim(aline),trim(afile)
9510    format(' Observation "',a,'" cited on or about line ',a,
     +  ' of PEST control file ',a,' has already been assigned a ',
     +  'variance.')
        go to 9800



9800    jfail=1

9900    continue
        if(jfail.eq.0)then
          if(numcov.ne.0) jfail=-1
          if(iwarn.ne.0) jfail=jfail-10
        end if

        return

        end


        subroutine read_covar_mat_file(ifail,iunit,covdim,
     +      nobs,vmul,covar,matfile,amessage,aobs,cline)

C -- Subroutine read_covar_mat_file reads a covariance matrix file.

        implicit none

        integer     :: ifail            ! return as non-zero if there is an error
        integer     :: iunit            ! unit number to read from
        integer     :: covdim           ! dimension of overall problem covariance matrix
        integer     :: nobs             ! number of observations

        double precision  :: vmul       ! variance multiplier

        double precision  :: covar(covdim,covdim)  ! covariance matrix

        character*(*) matfile           ! matrix file
        character*(*) amessage          ! error message string
        character*(*) cline             ! a working string
        character*(*) aobs(nobs)        ! observations

        integer          :: ierr,j,jfail,nrow,ncol,icode,
     +                      icount,nb,k,irow,icol,ir,ic,currentobs
        integer          :: lw(3),rw(3)
        double precision :: dtemp
        character*20     :: atemp
        character*200    :: afile

        integer, allocatable          :: iloc(:)
        double precision, allocatable :: mat(:,:)
        character*20, allocatable     :: bobs(:)


C -- Initialisation

        ifail=0
        currentobs=0
        call addquote(matfile,afile)
C        write(6,10) trim(afile)
C10      format(' - reading covariance matrix file ',a,'...')


C -- The matrix file is opened.

        open(unit=iunit,file=matfile,status='old',iostat=ierr)
        if(ierr.ne.0)then
          write(amessage,100) trim(afile)
100       format(' Cannot open covariance matrix file ',a,'.')
          go to 9800
        end if

        read(iunit,'(a)',err=9000,end=9050) cline
        call linspl(jfail,3,lw,rw,cline)
        if(jfail.ne.0) go to 9100
        atemp=cline(lw(1):rw(1))
        call intread(jfail,atemp,nrow)
        if(jfail.ne.0) go to 9000
        atemp=cline(lw(2):rw(2))
        call intread(jfail,atemp,ncol)
        if(jfail.ne.0) go to 9000
        atemp=cline(lw(3):rw(3))
        call intread(jfail,atemp,icode)
        if(jfail.ne.0) go to 9000
        if(nrow.ne.ncol)then
          write(amessage,120) trim(afile)
120       format(' Number of columns does not equal number of rows ',
     +    'according to integer header of covariance matrix file ',
     +    a,'.')
          go to 9800
        end if
        if((nrow.le.0).or.(ncol.le.0))then
          write(amessage,130) trim(afile)
130       format(' Improper integer header to matrix in file ',a,'.')
          go to 9800
        end if
        if((icode.ne.1).and.(icode.ne.-1))then
          write(amessage,140) trim(afile)
140       format(' Last integer entry on first line of covariance ',
     +    'matrix file ',a,' should be "1" or "-1".')
          go to 9800
        end if

C -- Memory is allocated.

        allocate(mat(nrow,ncol),bobs(nrow),iloc(nrow),stat=ierr)
        if(ierr.ne.0)then
          write(amessage,150)
150       format(' Cannot allocate sufficient memory to continue ',
     +    'execution.')
          go to 9800
        end if

C -- The rest of the file is read.

        if(icode.eq.-1)then
          mat=0.0d0           ! a matrix
          do irow=1,nrow
            read(iunit,*,err=9000,end=9050) mat(irow,irow)
          end do
        else
          do irow=1,nrow
            read(iunit,*,err=9000,end=9050) (mat(irow,icol),icol=1,ncol)
          end do
        end if
        read(iunit,'(a)',err=9000,end=9150) cline
        if(cline.eq.' ') go to 9150
        cline=adjustl(cline)
        if(cline(1:13).ne.'* row and col')then
          write(amessage,160) trim(afile)
160       format(' Line following matrix in file ',a,' should be ',
     +    '"* row and column names".')
          go to 9800
        end if
        do irow=1,nrow
          read(iunit,'(a)',err=9000,end=9050) bobs(irow)
          call lowcas(bobs(irow))
          bobs(irow)=adjustl(bobs(irow))
        end do

C -- Entities in the matrix file are linked to observation names.

        icount=0
        do irow=1,nrow
          call whichone(jfail,nobs,currentobs,aobs,bobs(irow))
          if(jfail.eq.0)then
            icount=icount+1
            iloc(irow)=currentobs
            if(covar(currentobs,currentobs).gt.-1.0d35)then
              write(amessage,165) trim(aobs(currentobs)),trim(afile)
165           format(' Parameter "',a,'" cited in matrix file ',a,
     +        ' has already been assigned a variance.')
              go to 9800
            end if
          else
            iloc(irow)=0
          end if
        end do
        if(icount.eq.0) go to 9900
        if(icount.ne.nrow)then
          write(amessage,170) trim(afile)
170       format(' All parameters, or no parameters, ',
     +    'cited in the covariance matrix ',
     +    'file ',a,' must be pertinent to the current parameter ',
     +    'estimation problem. ',
     +    'Some of them are, but some of them are not. The latter ',
     +    'include:-')
          nb=len(amessage)
          do irow=1,nrow
            if(iloc(irow).eq.0)then
              k=len_trim(amessage)
              j=len_trim(bobs(irow))+3
              if(k+j.gt.nb) go to 9800
              write(amessage(k+2:),180) trim(bobs(irow))
180           format('"',a,'"')
            end if
          end do
          go to 9800
        end if

C -- Pertinent elements of the global covariance matrix are now written.

        do icol=1,ncol
          do irow=1,nrow
            ir=iloc(irow)
            ic=iloc(icol)
            dtemp=mat(irow,icol)
            covar(ir,ic)=dtemp*vmul
          end do
        end do
        close(unit=iunit,iostat=ierr)
        write(6,185) trim(afile)
185     format('    - covariance matrix file ',a,' read ok.')

        go to 9900



9000    write(amessage,9010) trim(afile)
9010    format(' Error encountered in reading matrix file ',a,'.')
        go to 9800
9050    write(amessage,9060) trim(afile)
9060    format(' Unexpected end encountered to matrix file ',a,'.')
        go to 9800
9100    write(amessage,9110) trim(afile)
9110    format(' Three integers are expected on first line ',
     +  'of matrix file ',a,'.')
        go to 9800
9150    write(amessage,9160) trim(afile)
9160    format(' The matrix file ',a,
     +  ' does not contain row and column ',
     +  'names. Maybe it was written by PLPROC. Anyway, the ',
     +  'COVARIANCE_MATRIX block thus requires a ',
     +  '"parameter_list_file" keyword or "first/last_parameter" ',
     +  'keywords.')
        go to 9800

9800    ifail=1

9900    continue
        deallocate(mat,bobs,iloc,stat=ierr)

        return
        end



        subroutine read_mat_and_list_file(ifail,iunit,covdim,
     +      nobs,vmul,covar,matfile,amessage,aobs,cline,
     +      index1,nexpect,listfile,iunit1)

C -- Subroutine read_mat_and_list_file reads a covariance matrix file and
C    an accompanying parameter list file, or obtains parameter indices
C    from the calling program.

        implicit none

        integer     :: ifail            ! return as non-zero if there is an error
        integer     :: iunit            ! unit number to read from
        integer     :: covdim           ! dimension of overall problem covariance matrix
        integer     :: nobs             ! number of observations
        integer     :: index1           ! first parameter index
        integer     :: nexpect          ! expected size of matrix
        integer     :: iunit1           ! unit number of list file

        double precision  :: vmul       ! variance multiplier

        double precision  :: covar(covdim,covdim)  ! covariance matrix

        character*(*) matfile           ! matrix file
        character*(*) amessage          ! error message string
        character*(*) cline             ! a working string
        character*(*) aobs(nobs)        ! observations
        character*(*) listfile          ! file to read parameter list

        integer          :: ierr,j,jfail,nrow,ncol,icode,
     +                      icount,nb,k,irow,icol,ir,ic,currentobs
        integer          :: jrow
        integer          :: jcol
        integer          :: lw(3),rw(3)
        double precision :: dtemp
        character*20     :: atemp
        character*20     :: bobs
        character*200    :: afile

        integer, allocatable          :: iloc(:)
        double precision, allocatable :: mat(:,:)

C -- Initialisation

        ifail=0
        currentobs=0
        call addquote(matfile,afile)
C        write(6,10) trim(afile)
C10      format(' - reading covariance matrix file ',a,'...')

C -- The matrix file is opened.

        open(unit=iunit,file=matfile,status='old',iostat=ierr)
        if(ierr.ne.0)then
          write(amessage,100) trim(afile)
100       format(' Cannot open covariance matrix file ',a,'.')
          go to 9800
        end if

C -- The header to the matrix file is read

        read(iunit,'(a)',err=9030,end=9050) cline
        call linspl(jfail,3,lw,rw,cline)
        if(jfail.ne.0) go to 9100
        atemp=cline(lw(1):rw(1))
        call intread(jfail,atemp,nrow)
        if(jfail.ne.0) go to 9030
        atemp=cline(lw(2):rw(2))
        call intread(jfail,atemp,ncol)
        if(jfail.ne.0) go to 9030
        atemp=cline(lw(3):rw(3))
        call intread(jfail,atemp,icode)
        if(jfail.ne.0) go to 9030
        if(nrow.ne.ncol)then
          write(amessage,120) trim(afile)
120       format(' Number of columns does not equal number of rows ',
     +    'according to integer header of covariance matrix file ',
     +    a,'.')
          go to 9800
        end if
        if((nrow.le.0).or.(ncol.le.0))then
          write(amessage,130) trim(afile)
130       format(' Improper integer header to matrix in file ',a,'.')
          go to 9800
        end if
        if((icode.ne.1).and.(icode.ne.-1))then
          write(amessage,140) trim(afile)
140       format(' Last integer entry on first line of covariance ',
     +    'matrix file ',a,' should be "1" or "-1".')
          go to 9800
        end if
        if(nexpect.ne.0)then
          if(nexpect.ne.nrow)then
            write(amessage,200) trim(afile)
200         format(' The size of the matrix in file ',a,' is not ',
     +      'in accordance with the parameter range as defined by ',
     +      'the "first_parameter" and "last_parameter" keywords ',
     +      'in the COVARIANCE_MATRIX block which cites this file.')
            go to 9800
          end if
        end if

C -- Memory is allocated.

        allocate(mat(nrow,ncol),stat=ierr)
        if(ierr.ne.0)then
          write(amessage,150)
150       format(' Cannot allocate sufficient memory to continue ',
     +    'execution.')
          go to 9800
        end if
        if(iunit1.ne.0)then
          allocate(iloc(nrow),stat=ierr)
          if(ierr.ne.0)then
            write(amessage,150)
            go to 9800
          end if
          do irow=1,nrow
            iloc(irow)=0
          end do
        end if

C -- The rest of the file is read.

        if(icode.eq.-1)then
          mat=0.0d0           ! a matrix
          do irow=1,nrow
            read(iunit,*,err=9000,end=9050) mat(irow,irow)
          end do
        else
          do irow=1,nrow
            read(iunit,*,err=9000,end=9050) (mat(irow,icol),icol=1,ncol)
          end do
        end if
        close(unit=iunit,iostat=ierr)
        write(6,185) trim(afile)
185     format('    - covariance matrix file ',a,' read ok.')

        if(nexpect.ne.0)then
          do icol=index1,index1+nexpect-1
            if(covar(icol,icol).gt.-1.0d35)then
              write(amessage,186) trim(aobs(icol))
186           format(' Parameter "',a,'" is ',
     +        'assigned a variance more than once.')
              go to 9800
            end if
          end do
          do icol=index1,index1+nexpect-1
            jcol=icol-index1+1
            do irow=index1,index1+nexpect-1
              covar(irow,icol)=mat(irow-index1+1,jcol)*vmul
            end do
          end do
          go to 9900
        end if

C -- The list file is opened

        call addquote(listfile,afile)
        open(unit=iunit1,file=listfile,status='old',iostat=ierr)
        if(ierr.ne.0)then
          write(amessage,101) trim(afile)
101       format(' Cannot open parameter list file ',a,'.')
          go to 9800
        end if

C        write(6,1861) trim(afile)
C1861    format(' - reading parameter list file ',a,'...')
        do irow=1,nrow
187       continue
          read(iunit1,'(a)',err=9200,end=9250) cline
          if(cline.eq.' ') go to 187
          cline=adjustl(cline)
          if(cline(1:1).eq.'#') go to 187
          call linspl(jfail,1,lw,rw,cline)
          bobs=cline(lw(1):rw(1))
          call lowcas(bobs)
          call whichone(jfail,nobs,currentobs,aobs,bobs)
          if(jfail.eq.0)then
            if(irow.gt.1)then
              do jrow=1,irow-1
                if(iloc(jrow).eq.currentobs)then
                  write(amessage,188) trim(bobs),trim(afile)
188               format(' Parameter "',a,'" is repeated in parameter ',
     +            'list file ',a,'.')
                  go to 9800
                end if
              end do
            end if
            iloc(irow)=currentobs
            if(covar(currentobs,currentobs).gt.-1.0d35)then
              write(amessage,165) trim(aobs(currentobs)),trim(listfile)
165           format(' Parameter "',a,'" cited in list file ',a,
     +        ' has already been assigned a variance.')
              go to 9800
            end if
          else
            write(amessage,166) trim(bobs),trim(afile)
166         format(' Parameter "',a,'" cited in list file ',a,
     +      ' is not an adjustable parameter in the PEST ',
     +      'control file.')
            go to 9800
          end if
        end do
        close(unit=iunit1)
        write(6,1661) trim(afile)
1661    format('    - parameter list file ',a,' read ok.')

C -- Pertinent elements of the global covariance matrix are now recorded.

        do icol=1,ncol
          ic=iloc(icol)
          do irow=1,nrow
            ir=iloc(irow)
            dtemp=mat(irow,icol)
            covar(ir,ic)=dtemp*vmul
          end do
        end do

        go to 9900

9000    write(amessage,9010) trim(afile)
9010    format(' Error encountered in reading matrix file ',a,'.')
        go to 9800
9030    write(amessage,9040) trim(afile)
9040    format(' Error reading three-integer header line of ',
     +  'matrix file ',a,'.')
        go to 9800
9050    write(amessage,9060) trim(afile)
9060    format(' Unexpected end encountered to matrix file ',a,'.')
        go to 9800
9100    write(amessage,9110) trim(afile)
9110    format(' Three integers are expected on first line ',
     +  'of matrix file ',a,'.')
        go to 9800
9200    write(amessage,9210) trim(afile)
9210    format(' Error encountered in reading parameter list ',
     +  'file ',a,'.')
        go to 9800
9250    write(amessage,9260) trim(afile)
9260    format(' The number of parameters listed in file ',a,
     +  ' is less than the size of the matrix featured in the ',
     +  'accompanying covariance matrix file.')
        go to 9800

9800    ifail=1

9900    continue
        deallocate(mat,iloc,stat=ierr)

        return
        end



        subroutine whichone(ifail,npar,ipar,apar,tpar)

C -- Subroutine whichone locates a string in an array. Note that both the
C    search string and the existing array of strings are assumed to be
C    in the same case.

        integer npar,ipar,i
        integer ifail
        character*(*) tpar
        character*(*) apar(npar)

        ifail=0
        if((ipar.lt.1).or.(ipar.gt.npar)) ipar=1
        if(tpar.eq.apar(ipar)) return
        if(ipar.ne.npar)then
          do 20 i=ipar+1,npar
          if(tpar.eq.apar(i))then
            ipar=i
            return
          end if
20        continue
        end if
        if(ipar.ne.1)then
          do 40 i=ipar-1,1,-1
          if(tpar.eq.apar(i)) then
            ipar=i
            return
          end if
40        continue
        end if
        ifail=1
        return
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


C -- In manual mention that we look at prior info.
C -- Explain difference between variance multiplier and std multiplier.
