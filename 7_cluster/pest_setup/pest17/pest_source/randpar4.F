       program randpar4

C -- Program RANDPAR4 generates random realisations of PEST parameter increments. It stores them
C    in a JCB file.

       implicit none

       logical          :: skipline
       integer          :: nb,ierr,ntied,itied,j,k,ibeg,iend,iunit,
     +                     junit,itype,vardim,covdim,jpartype,numfile,
     +                     ifile,ifail,ies,jes,iseed,i
       integer          :: pestmode,npar,nobs,npargp,nprior,nobsgp,
     +                     nespar,ipar
       integer          :: seed_size
       integer          :: ldu,ldvt,lwork,info
       integer          :: irow,icount
       integer          :: nrealization
       integer          :: neqq,lw(12),rw(12)
       double precision :: sd
       double precision :: sum,xx
       double precision :: random_normal
       double precision :: parvar(1)
       double precision :: u(1,1),vt(1,1)
       character*1      :: jobu,jobvt
       character*4      :: aext
       character*10     :: aseed,how,atemp10
       character*12     :: apar1,apar2
       character*12     :: aversion
       character*15     :: atemp1,atemp2,atrans,achange,agroup,anum,
     +                     atype
       character*20     :: atemp20
       character*200    :: pestinfile,apestinfile,afile,uncertfile,
     +                     auncertfile,jcbfile
       character*200    :: atemp200
       character*300    :: cline
       character*1500   :: amessage

       integer, allocatable          :: seed(:)
       integer, allocatable          :: itrans(:)
       double precision, allocatable :: pval(:)
       double precision, allocatable :: cp(:,:)
       double precision, allocatable :: work(:),singval(:)
       double precision, allocatable :: dranvec(:)
       double precision, allocatable :: ppval(:)
       character*12, allocatable     :: apar(:),aapar(:)

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,50) trim(aversion)
50     format(/,' RANDPAR4 Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

100    write(6,110,advance='no')
110    format(' Enter name of existing PEST control file: ')
       read(5,*) pestinfile
#ifndef UNIX
       call lowcas(pestinfile)
#endif
       nb=len_trim(pestinfile)
       nb=nb-3
       if(nb.lt.1)nb=1
       aext=pestinfile(nb:)
       call lowcas(aext)
       if(aext.ne.'.pst')then
         write(6,120)
120      format(' *** file extension of ".pst" expected - ',
     +   'try again ***')
         go to 100
       end if
       call addquote(pestinfile,apestinfile)
       open(unit=10,file=pestinfile,status='old',err=100)

C -- The existing PEST control file is partly read.

       read(10,*,err=9000,end=9000) atemp1
       call lowcas(atemp1)
       if(atemp1(1:4).ne.'pcf ') go to 9000
76     continue
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 76
77     continue
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 77
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       atemp1=cline(lw(1):rw(1))
       atemp2=cline(lw(2):rw(2))
       call lowcas(atemp2)
       if(atemp2.eq.'estimation')then
         pestmode=1
       else if(atemp2.eq.'prediction')then
         pestmode=2
       else if (atemp2(1:5).eq.'regul')then
         pestmode=3
       else if (atemp2(1:5).eq.'paret')then
         pestmode=4
       else
         write(amessage,165) trim(apestinfile)
165      format(' Unknown value for PESTMODE variable on line 3 of ',
     +   'PEST control file ',a,'.')
         go to 9890
       end if
166    continue
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 166
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
       if(npar.eq.0)then
         write(amessage,168) trim(apestinfile)
168      format(' Number of parameters NPAR supplied as zero in ',
     +   'PEST control file ',a,'.')
         go to 9890
       end if

C -- Parameter data is now read.

       allocate(apar(npar),itrans(npar),pval(npar),
     + ppval(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       do
         read(10,'(a)',end=9000) cline
         if(skipline(cline)) cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'parameter da').ne.0) go to 170
         end if
       end do
170    continue
       nespar=0
       do ipar=1,npar
171      continue
         read(10,'(a)',err=9000,end=9000) cline
         if(skipline(cline)) go to 171
         neqq=index(cline,'=')
         if(neqq.gt.1) go to 171
         call lowcas(cline)
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         if(cline(lw(2):rw(2)).eq.'file_parameter')go to 171
         call linspl(ifail,4,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         apar(ipar)=cline(lw(1):rw(1))
         atrans=cline(lw(2):rw(2))
         achange=cline(lw(3):rw(3))
         call drealrd(ifail,cline(lw(4):rw(4)),pval(ipar))
         if(ifail.ne.0) go to 9000
         if(atrans.eq.'log')then
           itrans(ipar)=1
         else if(atrans.eq.'none')then
           itrans(ipar)=0
         else if(atrans.eq.'fixed')then
           itrans(ipar)=-1000000
         else if(atrans.eq.'tied')then
           itrans(ipar)=-1
         else
           go to 9000
         end if
         if(itrans(ipar).eq.1)then
           if(pval(ipar).le.0.0d0)then
             write(amessage,195) trim(apar(ipar)),trim(apestinfile)
195          format(' Parameter "',a,'" is log transformed, yet it ',
     +       'has a negative initial value in PEST control file ',
     +       a,'.')
             go to 9890
           end if
         end if
         if(itrans(ipar).ge.0) nespar=nespar+1
       end do
       if(nespar.eq.0) then
         write(amessage,230) trim(apestinfile)
230      format(' There are no adjustable parameters in PEST ',
     +   'control file ',a,'.')
         go to 9890
       end if
       call writint(anum,npar)
       write(6,210) trim(anum),trim(apestinfile)
210    format(' - ',a,' parameters read from file ',a,'.')
       call writint(anum,nespar)
       write(6,220) trim(anum)
220    format(' - ',a,' of these are adjustable.')

       close(unit=10)

C -- We now prompt for the name of the parameter uncertainty file.

       write(6,*)
340    write(6,350,advance='no')
350    format(' Enter name of parameter uncertainty file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 340
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,uncertfile,ibeg,iend)
       if(ifail.ne.0) go to 340
#ifndef UNIX
       call lowcas(uncertfile)
#endif
       call addquote(uncertfile,auncertfile)
       open(unit=12,file=uncertfile,status='old',iostat=ierr)
       if(ierr.ne.0) go to 340
       allocate(cp(nespar,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- A copy of the parameter name array is made in which only adjustable parameters
C    are cited.

       allocate(aapar(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           aapar(ies)=apar(ipar)
         end if
       end do

C -- The uncertainty file is now read.

       iunit=12
       junit=15
       itype=2
       vardim=1
       covdim=nespar
       atype='parameter'
       call read_uncert_data_file(ifail,iunit,junit,itype,jpartype,
     + nespar,vardim,covdim,parvar,cp,atype,uncertfile,amessage,
     + cline,aapar)
       if(ifail.ne.0) go to 9890
       write(6,380) trim(auncertfile)
380    format(' - parameter uncertainty file ',a,' read ok.')

! -- The name of the JCB file is acquired.

       write(6,*)
399    write(6,400,advance='no')
400    format(' Enter name for JCB file: ')
       read(5,*,err=399) jcbfile
#ifndef UNIX
       call lowcas(jcbfile)
#endif
       nb=len_trim(jcbfile)
       i=nb-3
       if(i.lt.1)i=1
       aext=jcbfile(i:nb)
       call lowcas(aext)
       if(aext.ne.'.jcb')then
         write(6,401)
401      format(' *** Expected filename extension is ".jcb" ',
     +   '- try again ***')
         go to 399
       end if
       how='write'
       call open_unformatted_file(ifail,20,how,jcbfile,amessage)
       if(ifail.ne.0) then
         write(6,4011)
4011     format(' *** Cannot write to file - try again ***')
         go to 399
       end if
420    write(6,430,advance='no')
430    format(' How many realizations do you wish to generate? ')
       read(5,*,err=420) numfile
       if(numfile.le.0) go to 420

C -- We write the first line of the JCB file.

       write(20) nespar,numfile                              !debug
       icount=numfile*nespar
       write(20) icount

C -- The random number generator is now initialised.

       write(6,*)
431    continue
       write(6,432,advance='no')
432    format(' Enter integer random number seed ',
     + '(<Enter> if default): ')
       read(5,'(a)') aseed
       if(aseed.ne.' ')then
         call intread(ifail,aseed,iseed)
         if(ifail.ne.0) go to 431
         if(iseed.le.0)then
           write(6,433)
433        format(' *** Must be positive - try again ***')
           go to 431
         end if
       else
         iseed=1111
       end if
       call random_seed(size=seed_size)           ! find out size of seed
       allocate(seed(seed_size))
       seed=iseed                                 ! an array
       call random_seed(put=seed)                 ! set current seed

C -- If we have a covariance matrix, then SVD is now done on it.

       if(jpartype.eq.2)then
         write(6,*)
         write(6,434)
434      format(' - undertaking singular value decomposition ',
     +   'of covariance matrix...')
         jobu='O'
         jobvt='N'
         ldu=1
         ldvt=1
         lwork=6*nespar
         allocate(work(lwork),singval(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         call dgesvd(jobu,jobvt,nespar,nespar,cp,nespar,singval,
     +   u,ldu,vt,ldvt,work,lwork,info)
         if(info.ne.0)then
           write(amessage,436)
436        format(' Singular value decomposition was not successful.')
           go to 9890
         end if
       end if

! -- More memory is allocated.

       allocate(dranvec(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- Random numbers are now generated and parameter value files are now written.

       nrealization=numfile
       do ifile=1,nrealization

         if(jpartype.eq.1)then
           ies=0
           do ipar=1,npar
             if(itrans(ipar).lt.0) cycle
             ies=ies+1
             if(itrans(ipar).eq.0)then
               sd=sqrt(cp(ies,ies))
               xx=random_normal()
               dranvec(ies)=xx*sd
             else
               sd=sqrt(cp(ies,ies))
               xx=random_normal()
               dranvec(ies)=10**(xx*sd)
             end if
           end do
         else
           do ies=1,nespar
             ppval(ies)=random_normal()*sqrt(singval(ies))
           end do
           do ies=1,nespar
             sum=0.0d0
             do jes=1,nespar
               sum=sum+cp(ies,jes)*ppval(jes)
             end do
             dranvec(ies)=sum
           end do
           ies=0
           do ipar=1,npar
             if(itrans(ipar).eq.0)then
               ies=ies+1
               dranvec(ies)=dranvec(ies)
             else if(itrans(ipar).eq.1)then
               ies=ies+1
               dranvec(ies)=10**(dranvec(ies))
             end if
           end do
         end if

C -- We now write a row of the JCB file

         irow=ifile-1
         do ies=1,nespar
           write(20) irow,ies-1,dranvec(ies)
         end do
       end do

C -- The last part of the JCB file is written.

       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           atemp200=apar(ipar)
           atemp200=adjustl(atemp200)
           write(20) atemp200
         end if
       end do
       do j=1,nrealization
         call writint(atemp10,j-1)
         atemp200=atemp10
         atemp200=adjustl(atemp200)
         write(20) atemp200
       end do

       close(unit=20)
       write(6,480) trim(jcbfile)
480    format(' - file ',a,' written ok.')

       go to 9900

9000   write(amessage,9010) trim(apestinfile)
9010   format(' Error encountered when reading PEST control file ',
     + a,' - check this file with PESTCHEK.')
       go to 9890
9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9890   continue
       call writmess(6,amessage)

9900   continue
       if(allocated(itrans)) deallocate(itrans,stat=ierr)
       if(allocated(ppval)) deallocate(ppval,stat=ierr)
       if(allocated(pval)) deallocate(pval,stat=ierr)
       if(allocated(cp)) deallocate(cp,stat=ierr)
       if(allocated(apar)) deallocate(apar,stat=ierr)
       if(allocated(aapar)) deallocate(aapar,stat=ierr)
       if(allocated(seed)) deallocate(seed,stat=ierr)
       if(allocated(work)) deallocate(work,stat=ierr)
       if(allocated(singval)) deallocate(singval,stat=ierr)
       if(allocated(dranvec)) deallocate(dranvec,stat=ierr)

       end


        subroutine getfile(ifail,cline,filename,ibeg,iend)

C-- Subroutine getfile extracts a filename from a string.

C -- Arguments are as follows:-
C       ifail: returned as zero if filename successfully read
C       cline: a character string containing the file name
C       filename: the name of the file read from the string
C       ibeg: character position at which to begin search for filename
C       iend: on input  - character position at which to end search for filename
C             on output - character postion at which filename ends


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
          do j=i+1,iend
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
            if((cline(j:j).eq.' ').or.(cline(j:j).eq.',').or.
     +         (cline(j:j).eq.char(9)))then
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

        end


        SUBROUTINE TABREM(CLINE)

C -- Subroutine TABREM removes tabs from a string.

        INTEGER I
        CHARACTER*(*) CLINE

        DO 10 I=1,LEN(CLINE)
10      IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '

        RETURN
        END



       double precision function random_normal()

       implicit none

       double precision :: pi
       double precision :: d1,d2

       pi=3.14159265358979323846264338
       call random_number(d1)
       call random_number(d2)
       random_normal =
     + sqrt ( - 2.0D+00 * log ( d1 ) ) * cos ( 2.0D+00 * pi * d2 )

       return
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
