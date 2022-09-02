        program paramerr

C -- Program PARAMERR reads a RESPROC output file and parameter and measurement
C    uncertainty data. It then calculates the two terms of the parameter error
C    covariance matrix.

        implicit none

        integer       :: ifail,ierr,i,j,ipar,iobs,npar,nobs,iunit,
     +                   junit,itype,vardim,covdim,jpartype,jobstype,
     +                   k,l,ichoice
        integer       :: icode(10)

        double precision :: dtemp
        double precision :: rcode(10)
        double precision, allocatable :: res(:,:),g(:,:),parcov(:,:),
     +                                   parvar(:),obscov(:,:),
     +                                   obsvar(:),c(:,:)


        character*12    :: aversion
        character*15    :: atype
        character*200   :: resprocfile,parcovfile,obscovfile,outfile1,
     +                     outfile2,afile

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
5       format(/,' PARAMERR Version ',a,'. Watermark Numerical ',
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
12      write(6,13)
13      format(' Options are as follows:-')
        write(6,14)
14      format('   to compute (I-R)C(k)(I-R)''     - enter 1')
        write(6,15)
15      format('   to compute GC(e)G''             - enter 2')
        write(6,16)
16      format('   to compute both                - enter 3')
17      write(6,18,advance='no')
18      format(' Enter your choice: ')
        read(5,*,err=17) ichoice
        if((ichoice.ne.1).and.(ichoice.ne.2).and.(ichoice.ne.3))
     +  go to 17

        write(6,*)
        if((ichoice.eq.1).or.(ichoice.eq.3))then
31        write(6,32,advance='no')
32        format(' Enter name of parameter uncertainty file: ')
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
34          format(/,' Cannot open file ',a,' - try again.',/)
            go to 31
          end if
35        write(6,36,advance='no')
36        format(' Enter name for covariance matrix output file: ')
          read(5,'(a)') outfile1
          if(outfile1.eq.' ') go to 35
          call remchar(outfile1,'''')
          call remchar(outfile1,'"')
          outfile1=adjustl(outfile1)
#ifndef UNIX
          call lowcas(outfile1)
#endif
        end if

        if((ichoice.eq.2).or.(ichoice.eq.3))then
          write(6,*)
41        write(6,42,advance='no')
42        format(' Enter name of observation uncertainty file: ')
          read(5,'(a)')obscovfile
          if(obscovfile.eq.' ') go to 41
          call remchar(obscovfile,'''')
          call remchar(obscovfile,'"')
          obscovfile=adjustl(obscovfile)
#ifndef UNIX
          call lowcas(obscovfile)
#endif
          call addquote(obscovfile,afile)
          open(unit=11,file=obscovfile,status='old',iostat=ierr)
          if(ierr.ne.0)then
            write(6,34) trim(afile)
            go to 41
          end if
45        write(6,46,advance='no')
46        format(' Enter name for covariance matrix output file: ')
          read(5,'(a)') outfile2
          if(outfile2.eq.' ') go to 45
          call remchar(outfile2,'''')
          call remchar(outfile2,'"')
          outfile2=adjustl(outfile2)
#ifndef UNIX
          call lowcas(outfile2)
#endif
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
        allocate(res(npar,npar),g(npar,nobs),apar(npar),aobs(nobs),
     +  c(npar,npar),stat=ierr)
        if(ierr.ne.0) go to 9100
        read(15,err=9000,end=9000) ((res(i,j),i=1,npar),j=1,npar)
        read(15,err=9000,end=9000) ((g(i,j),i=1,npar),j=1,nobs)
        do ipar=1,npar
          read(15,err=9000,end=9000) apar(ipar)
        end do
        do iobs=1,nobs
          read(15,err=9000,end=9000) aobs(iobs)
        end do
        close(unit=15)
        write(6,40) trim(afile)
40      format(' - file ',a,' read ok.')

C -- More memory is allocated.

        if((ichoice.eq.1).or.(ichoice.eq.3))then
          allocate(parcov(npar,npar),parvar(1),stat=ierr)
          if(ierr.ne.0) go to 9100
        end if
        if((ichoice.eq.2).or.(ichoice.eq.3))then
          allocate(obscov(1,1),obsvar(nobs),stat=ierr)
          if(ierr.ne.0) go to 9100
        end if

C -- The parameter uncertainty file is now read.

        if((ichoice.eq.1).or.(ichoice.eq.3))then
          write(6,50)
50        format(/,' - reading parameter uncertainty data...')
          iunit=10
          junit=13
          itype=2
          vardim=1
          covdim=npar
          atype='parameter'
          call read_uncert_data_file(ifail,iunit,junit,itype,jpartype,
     +    npar,vardim,covdim,parvar,parcov,atype,parcovfile,amessage,
     +    cline,apar)
          if(ifail.ne.0) go to 9800
          write(6,60)
60        format(' - parameter uncertainty data read ok.')

C -- (I-R)C(p-p)(I-R)' is calculated.

          write(6,100)
100       format(/,' - calculating I-R contribution to parameter ',
     +    'error covariance...')
          do i=1,npar
            res(i,i)=res(i,i)-1.0d0
          end do
          if(jpartype.eq.2)then
            do i=1,npar
              do j=1,i
                dtemp=0.0d0
                do k=1,npar
                  do l=1,npar
                    dtemp=dtemp+res(j,k)*parcov(k,l)*res(i,l)
                  end do
                end do
                c(j,i)=dtemp
              end do
            end do
          else
            do i=1,npar
              do j=1,i
                dtemp=0.0d0
                do k=1,npar
                  dtemp=dtemp+res(i,k)*res(j,k)*parcov(k,k)
                end do
                c(j,i)=dtemp
              end do
            end do
          end if
          do i=1,npar-1
            do j=i+1,npar
              c(j,i)=c(i,j)
            end do
          end do

C -- This matrix is stored.

          call addquote(outfile1,afile)
          open(unit=20,file=outfile1,err=9400)
          write(20,200,err=9400) npar,npar,1
200       format(3i6)
          do i=1,npar
            write(20,210,err=9400) (c(i,j),j=1,npar)
210         format(8(1x,1pg13.6))
          end do
          write(20,230,err=9400)
230       format('* row and column names')
          do i=1,npar
            write(20,235) trim(apar(i))
235         format(a)
          end do
          close(unit=20)
          write(6,240) trim(afile)
240       format(' - file ',a,' written ok.')

        end if

C -- The observation uncertainty file is now read.


        if((ichoice.eq.2).or.(ichoice.eq.3))then
          write(6,70)
70        format(/,' - reading observation uncertainty data...')
          iunit=11
          junit=13
          itype=1
          vardim=nobs
          covdim=1
          atype='observation'
          call read_uncert_data_file(ifail,iunit,junit,itype,jobstype,
     +    nobs,vardim,covdim,obsvar,obscov,atype,obscovfile,amessage,
     +    cline,aobs)
          if(ifail.ne.0) go to 9800
          write(6,75)
75        format(' - observation uncertainty data read ok.')

C -- GC(e)G' is calculated.

          write(6,105)
105       format(/,' - calculating G contribution to parameter ',
     +    'error covariance...')

          do i=1,npar
            do j=1,i
              dtemp=0.0d0
              do k=1,nobs
                dtemp=dtemp+g(j,k)*g(i,k)*obsvar(k)
              end do
              c(j,i)=dtemp
            end do
          end do
          do i=1,npar-1
            do j=i+1,npar
              c(j,i)=c(i,j)
            end do
          end do

C -- This matrix is stored.

          call addquote(outfile2,afile)
          open(unit=20,file=outfile2,err=9400)
          write(20,200,err=9400) npar,npar,1
          do i=1,npar
            write(20,210,err=9400) (c(i,j),j=1,npar)
          end do
          write(20,230,err=9400)
          do i=1,npar
            write(20,235) trim(apar(i))
          end do
          close(unit=20)
          write(6,240) trim(afile)
        end if


        go to 9900


9000    write(amessage,9010) trim(afile)
9010    format(' Error encountered in reading unformatted ',
     +  'RESPROC output file ',a,'.')
        go to 9800
9100    write(amessage,9110)
9110    format(' Cannot allocate sufficient memory to continue ',
     +  'execution.')
        go to 9800
9400    write(amessage,9410) trim(afile)
9410    format(' Error writing to file ',a,'.')
        go to 9800


9800    continue
        call writmess(6,amessage)

9900    deallocate(res,g,parcov,parvar,obscov,obsvar,c,stat=ierr)

9999    continue

        end


