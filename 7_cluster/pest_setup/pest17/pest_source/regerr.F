        program regerr

C -- Program REGERR reads a RESPROC output file and a parameter uncertainty file.
C    It also reads a Jacobian matrix file. It then calculates a regularisation-
C    induced model output error covariance matrix for observations cited in a
C    Jacobian matrix file.

        implicit none

        integer       :: ifail,ierr,i,j,ipar,iobs,npar,nobs,iunit,
     +                   junit,itype,vardim,covdim,jpartype,
     +                   nb,itemp1,itemp2,nespar,nxrow,icount,ies,irow,
     +                   k,l,jrow,jes
        integer       :: icode(10)
        integer, allocatable  :: istart(:),iend(:)

        double precision :: dtemp
        double precision :: rcode(10)
        double precision, allocatable :: res(:,:),parcov(:,:),
     +                                   parvar(:),x(:,:),tempmat(:,:),
     +                                   covmat(:,:)
        character*4     :: aext
        character*12    :: aapar
        character*12    :: aversion
        character*15    :: atype
        character*20    :: aaobs
        character*200   :: resprocfile,parcovfile,afile,jcofile,bfile,
     +                     outfile

        character*500   :: cline
        character*600   :: amessage


        character*200   :: acode(10)
        character*12, allocatable :: apar(:)
        character*20, allocatable :: aobs(:)

#ifndef UNIX
#ifdef LAHEY
        open(unit=*,action='read',carriagecontrol='list')
#endif       
#endif

        include 'version.inc'
        write(6,5) trim(aversion)
5       format(/,' REGERR Version ',a,'. Watermark Numerical ',
     +  'Computing.',/)

10      write(6,11,advance='no')
11      format(' Enter name of RESPROC output file: ')
        read(5,'(a)') resprocfile
        if(resprocfile.eq.' ') go to 10
        call remchar(resprocfile,'''')
        call remchar(resprocfile,'"')
        resprocfile=adjustl(resprocfile)
#ifndef UNIX
        call lowcas(resprocfile)
#endif
        call addquote(resprocfile,afile)
        call open_unformatted_file(ifail,15,'read',resprocfile,amessage)
        if(ifail.ne.0) then
          write(6,6) trim(afile)
6         format(/,' Cannot open unformatted file ',a,' - try again.',/)
          go to 10
        end if

        write(6,*)
31      write(6,32,advance='no')
32      format(' Enter name of parameter uncertainty file: ')
        read(5,'(a)')parcovfile
        if(parcovfile.eq.' ') go to 31
        call remchar(parcovfile,'''')
        call remchar(parcovfile,'"')
        parcovfile=adjustl(parcovfile)
#ifndef UNIX
        call lowcas(parcovfile)
#endif
        call addquote(parcovfile,afile)
        open(unit=10,file=parcovfile,status='old',iostat=ierr)
        if(ierr.ne.0)then
          write(6,34) trim(afile)
34        format(/,' Cannot open file ',a,' - try again.',/)
          go to 31
        end if

        write(6,*)
310     write(6,320,advance='no')
320     format(' Enter name of Jacobian matrix file: ')
        read(5,'(a)')jcofile
        if(jcofile.eq.' ') go to 310
        call remchar(jcofile,'''')
        call remchar(jcofile,'"')
        jcofile=adjustl(jcofile)
#ifndef UNIX
        call lowcas(jcofile)
#endif
        nb=len_trim(jcofile)
        i=nb-3
        if(i.lt.1)i=1
        aext=jcofile(i:nb)
        call lowcas(aext)
        if(aext.ne.'.jco')then
          call addquote(jcofile,afile)
          write(6,315)
315       format(/,' Expected extension for Jacobian matrix file ',
     +    'is ".jco". Try again.',/)
          go to 310
        end if
        call open_unformatted_file(ifail,16,'read',jcofile,amessage)
        if(ifail.ne.0)then
          call addquote(jcofile,afile)
          write(6,330) trim(afile)
330       format(/,' Cannot open unformatted file ',a,'. Try again.',/)
          go to 310
        end if

332     write(6,*)
333     write(6,334,advance='no')
334     format(' Enter name for output covariance matrix file: ')
        read(5,'(a)') outfile
        if(outfile.eq.' ') go to 333
        open(unit=20,file=outfile,action='write',iostat=ierr)
        if(ierr.ne.0)then
          call addquote(outfile,afile)
          write(6,335) trim(afile)
335       format(/,' Cannot open file ',a,' for output - try again.',/)
          go to 333
        end if

C -- The RESPROC output file read.

        call addquote(resprocfile,afile)
        write(6,30) trim(afile)
30      format(/,' - reading RESPROC output file ',a,'...')
        read(15,err=9000,end=9000) (icode(i),i=1,10)
        read(15,err=9000,end=9000) (rcode(i),i=1,10)
        do i=1,10
          read(15,err=9000,end=9000) acode(i)
        end do
        read(15,err=9000,end=9000) npar,nobs
        allocate(res(npar,npar),apar(npar),istart(npar),iend(npar),
     +  stat=ierr)
        if(ierr.ne.0) go to 9100
        read(15,err=9000,end=9000) ((res(i,j),i=1,npar),j=1,npar)
        read(15,err=9000,end=9000) ((dtemp,i=1,npar),j=1,nobs)
        do ipar=1,npar
          read(15,err=9000,end=9000) apar(ipar)
        end do
        do iobs=1,nobs
          read(15,err=9000,end=9000) aaobs
        end do
        close(unit=15)
        write(6,40) trim(afile)
40      format(' - file ',a,' read ok.')

C -- More memory is allocated.

        allocate(parcov(npar,npar),parvar(1),stat=ierr)
        if(ierr.ne.0) go to 9100

C -- The Jacobian matrix file is read.

        call addquote(jcofile,afile)
        write(6,362) trim(afile)
362     format(/,' - reading Jacobian matrix file ',a,'...')
        read(16,err=9350,end=9350)itemp1,itemp2
        if(itemp1.ge.0)then
          write(amessage,361) trim(afile)
361       format(' Jacobian matrix file ',a,' uses old format; ',
     +    'use JCOTRANS utility to translate it to new format.')
          go to 9800
        end if
        nespar=abs(itemp1)
        nxrow=abs(itemp2)
        if((nespar.le.0).or.(nxrow.le.0))then
          write(amessage,365) trim(afile)
365       format(' NROW or NCOL not positive in file ',a,
     +    '; is this a valid Jacobian matrix file?')
          go to 9800
        end if
        if(nespar.ne.npar)then
          call addquote(resprocfile,bfile)
          write(amessage,366) trim(afile),trim(bfile)
          go to 9800
        end if
        allocate(x(nxrow,nespar),aobs(nxrow),tempmat(nxrow,nespar),
     +  stat=ierr)
        if(ierr.ne.0) go to 9100
        do i=1,nespar
          do j=1,nxrow
            x(j,i)=0.0d0
          end do
        end do
        read(16,err=9350,end=9350)icount
        do i=1,icount
          read(16,err=9350,end=9350) j,dtemp
          ies=(j-1)/nxrow+1
          irow=j-(ies-1)*nxrow
          x(irow,ies)=dtemp
        end do
        do ies=1,nespar
          read(16,err=9350,end=9350) aapar
          call lowcas(aapar)
          if(aapar.ne.apar(ies))then
            call addquote(resprocfile,bfile)
            write(amessage,366) trim(afile),trim(bfile)
366         format(' Jacobian matrix file ',a,' does not cite ',
     +      'same parameters in same order as RESPROC ',
     +      'output file ',a,'.')
            go to 9800
          end if
        end do
        do irow=1,nxrow
          read(16,err=9350,end=9350) aobs(irow)
          call lowcas(aobs(irow))
        end do
        close(unit=16)
        write(6,370) trim(afile)
370     format(' - Jacobian matrix file ',a,' read ok.')

C -- The parameter uncertainty file is now read.

        write(6,50)
50      format(/,' - reading parameter uncertainty data...')
        iunit=10
        junit=13
        itype=2
        vardim=1
        covdim=npar
        atype='parameter'
        call read_uncert_data_file(ifail,iunit,junit,itype,jpartype,
     +  npar,vardim,covdim,parvar,parcov,atype,parcovfile,amessage,
     +  cline,apar)
        if(ifail.ne.0) go to 9800
        write(6,60)
60      format(' - parameter uncertainty data read ok.')

C -- The structural error covariance matrix is now calculated.

        write(6,100)
100     format(/,' - calculating regularisation-induced output error ',
     +  'covariance matrix...')

        do i=1,npar
          do j=1,npar
            res(j,i)=-res(j,i)
          end do
        end do
        do i=1,npar
          res(i,i)=1.0d0+res(i,i)
        end do

C -- The first and last non-zero covariance matrix element is found.

        do ies=1,nespar
          do jes=1,nespar
            if(parcov(jes,ies).ne.0.0d0)then
              istart(ies)=jes
              go to 103
            end if
          end do
          istart(ies)=0
          iend(ies)=0
          go to 104
103       continue
          do jes=nespar,1,-1
            if(parcov(jes,ies).ne.0.0d0)then
              iend(ies)=jes
              go to 104
            end if
          end do
104       continue
        end do

C -- X(I-R) is evaluated.

        do irow=1,nxrow
          do ies=1,nespar
            dtemp=0.0d0
            do k=1,nespar
              dtemp=dtemp+x(irow,k)*res(k,ies)
            end do
            tempmat(irow,ies)=dtemp
          end do
        end do

C -- Some memory is accocated and deallocated.

        deallocate(x,stat=ierr)
        allocate(covmat(nxrow,nxrow),stat=ierr)
        if(ierr.ne.0)then
          write(amessage,480)
480       format(' Cannot allocate sufficient memory to continue ',
     +    'execution.')
          go to 9800
        end if

C -- And finally the total matrix product.

        do irow=1,nxrow
          do jrow=1,irow
            dtemp=0.0d0
            do k=1,nespar
              if(istart(k).gt.0)then
                do l=istart(k),iend(k)
                  dtemp=dtemp+tempmat(irow,k)*parcov(l,k)*
     +            tempmat(jrow,l)
                end do
              end if
            end do
            covmat(irow,jrow)=dtemp
            covmat(jrow,irow)=dtemp
          end do
        end do

C -- The output file is written in matrix format.

        call addquote(outfile,afile)
        write(20,410) nxrow,nxrow
410     format(1x,i6,1x,i6,'   1')
        do irow=1,nxrow
          write(20,430) (covmat(irow,jrow),jrow=1,nxrow)
430       format(8(1x,1pg14.7))
        end do
        write(20,440)
440     format('* row and column names')
        do irow=1,nxrow
          write(20,'(a)') trim(aobs(irow))
        end do
        close(unit=20)
        write(6,450) trim(afile)
450     format(' - file ',a,' written ok.')

        go to 9900

9000    write(amessage,9010) trim(afile)
9010    format(' Error encountered in reading unformatted ',
     +  'RESPROC output file ',a,'.')
        go to 9800
9100    write(amessage,9110)
9110    format(' Cannot allocate sufficient memory to continue ',
     +  'execution.')
        go to 9800
9350    write(amessage,9360) trim(afile)
9360    format(' Error reading Jacobian matrix from file ',a,'.')
        go to 9800

9800    continue
        call writmess(6,amessage)



9900    deallocate(res,parcov,parvar,tempmat,covmat,apar,aobs,
     +  istart,iend,stat=ierr)
        if(allocated(x))then
          deallocate(x,stat=ierr)
        end if

9999    continue

        end



