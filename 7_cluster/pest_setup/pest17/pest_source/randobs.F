       program randobs

! -- Program RANDOBS generates random realizations of observations for the use of PESTPP-IES.

       implicit none

       logical          :: skipline
       integer          :: iseed
       integer          :: seed_size
       integer          :: numreal,nobs,ireal,icount,irow,j,nb,iobs
       integer          :: outtype,ierr,ifail,iline,iline_obs
       integer          :: ndim1,ndim2
       integer          :: lw(5),rw(5)
       double precision :: wfac,xx
       double precision :: random_normal
       character*1      :: ab,ar
       character*5      :: aext
       character*10     :: aseed,anum,how,aline
       character*12     :: aversion
       character*20     :: atemp20
       character*30     :: atemp30
       character*200    :: atemp200
       character*256    :: obsfile,aobsfile,outfile,aoutfile
       character*500    :: cline
       character*1500   :: amessage

       integer, allocatable          :: seed(:)
       double precision, allocatable :: oval(:),owght(:),ooval(:)
       character*200, allocatable    :: aobs(:)
       real, allocatable             :: ovalarray(:,:)

! -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif
       include 'version.inc'
       write(6,50) trim(aversion)
50     format(/,' RANDOBS Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

! -- The observation weights file is opened.

       write(6,*)
100    write(6,110,advance='no')
110    format(' Enter name of observation weights file: ')
       read(5,*) obsfile
       call addquote(obsfile,aobsfile)
       open(unit=10,file=obsfile,status='old',err=100)
60     write(6,70,advance='no')
70     format(' Enter factor to apply to all weights: ')
       read(5,*,err=60) wfac
       if(wfac.le.0.0d0)then
         write(6,71)
71       format(' *** Must be positive - try again ***')
         go to 60
       end if

       write(6,*)
74     write(6,75,advance='no')
75     format(' How many realizations to generate? ')
       read(5,*,err=74) numreal
       if(numreal.le.0)then
         write(6,71)
         go to 74
       end if
77     write(6,78,advance='no')
78     format(' Include initial observations as base ',
     + 'realization? [y/n]: ')
       read(5,'(a)') ab
       if((ab.eq.'Y').or.(ab.eq.'y'))then
         ab='y'
       else if((ab.eq.'n').or.(ab.eq.'N'))then
         ab='n'
       else
         go to 77
       end if

       write(6,*)
80     write(6,81,advance='no')
81     format(' Enter name for output file: ')
       read(5,*) outfile
       nb=len_trim(outfile)
       nb=nb-3
       if(nb.lt.1)nb=1
       aext=outfile(nb:)
       call lowcas(aext)
       if(aext.eq.'.csv')then
         outtype=1
       else if(aext.eq.'.jcb')then
         outtype=2
       else
         write(6,82)
82       format(' *** Extension must be ".csv" or ".jcb"',
     +   '- try again ***')
         go to 80
       end if
       if(outtype.eq.1)then
83       write(6,84,advance='no')
84       format(' Realizations are rows or columns in ',
     +   'csv file? [r/c]: ')
         read(5,'(a)') ar
         if((ar.eq.'R').or.(ar.eq.'r'))then
           ar='r'
         else if((ar.eq.'C').or.(ar.eq.'c'))then
           ar='c'
         else
           go to 83
         end if
       end if

C -- The random number generator is now initialised.

       write(6,*)
90     continue
       write(6,95,advance='no')
95     format(' Enter integer random number seed ',
     + '(<Enter> if default): ')
       read(5,'(a)') aseed
       if(aseed.ne.' ')then
         call intread(ifail,aseed,iseed)
         if(ifail.ne.0) go to 90
         if(iseed.le.0)then
           write(6,96)
96         format(' *** Must be positive - try again ***')
           go to 90
         end if
       else
         iseed=1111
       end if
       call random_seed(size=seed_size)           ! find out size of seed
       allocate(seed(seed_size))
       seed=iseed                                 ! an array
       call random_seed(put=seed)                 ! set current seed

! -- The observation file is pre-read to find the number of observations.

       write(6,115) trim(aobsfile)
115    format(' - reading file ',a,'...')
       iline=0
       do
         iline=iline+1
         read(10,'(a)',err=9000,end=9050) cline
         cline=adjustl(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(cline(1:18).eq.'* observation data') exit
         end if
       end do
       iline_obs=iline
       nobs=0
       do
         iline=iline+1
         read(10,'(a)',err=9000,end=150) cline
         if(skipline(cline)) cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'*') exit
         nobs=nobs+1
       end do
150    continue
       if(nobs.eq.0)then
         write(amessage,160) trim(aobsfile)
160      format('No observations found in file ',a,'.')
         go to 9890
       end if

! -- Memory is allocated.

       allocate(aobs(nobs),oval(nobs),owght(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200

! -- The file is rewound and the observation section read.

       rewind(unit=10)
       do iline=1,iline_obs
         read(10,*)
       end do
       iline=iline_obs
       do iobs=1,nobs
170      continue
         iline=iline+1
         read(10,'(a)',err=9000,end=150) cline
         if(skipline(cline)) go to 170
         call linspl(ifail,3,lw,rw,cline)
         if(ifail.ne.0) go to 9100
         aobs(iobs)=cline(lw(1):rw(1))
         call lowcas(aobs(iobs))
         call drealread(ifail,cline(lw(2):rw(2)),oval(iobs))
         if(ifail.ne.0) go to 9000
         call drealread(ifail,cline(lw(3):rw(3)),owght(iobs))
         if(ifail.ne.0) go to 9000
         if(owght(iobs).lt.0)then
           call writint(aline,iline)
           write(amessage,180) trim(aline),trim(aobsfile)
180        format('Negative weight supplied at line ',a,
     +     ' of file ',a,'.')
           go to 9890
         end if
       end do
       close(unit=10)
       call writint(anum,nobs)
       write(6,190) trim(anum),trim(aobsfile)
190    format(' - ',a,' observations read from file ',a,'.')

! -- The observation weight factor is applied.

       owght=owght*wfac            ! arrays

! -- We now open the output file.

       call addquote(outfile,aoutfile)
       write(6,250) trim(outfile)
250    format(' - generating realizations and writing file ',a,'...')
       if(outtype.eq.1)then
         open(unit=20,file=outfile,action='write',iostat=ifail)
       else
         how='write'
         call open_unformatted_file(ifail,20,how,outfile,amessage)
       end if
       if(ifail.ne.0)then
         write(amessage,260) trim(aoutfile)
260      format('Cannot write to file ',a,'.')
         go to 9890
       end if

! -- The first line of the file is written.

       if(outtype.eq.1)then
         if(ar.eq.'r')then
           write(20,270,advance='no')
270        format('real_name')
           do iobs=1,nobs-1
             write(20,280,advance='no') trim(aobs(iobs))
280          format(',',a)
           end do
           write(20,280) trim(aobs(nobs))
         else
           write(20,290,advance='no')
290        format('obs_name')
           if(ab.eq.'y')then
             write(20,300,advance='no')
300          format(',base')
           endif
           if(ab.eq.'y')then
             do ireal=1,numreal-2
               call writint(atemp20,ireal-1)
               write(20,280,advance='no') trim(atemp20)
             end do
             call writint(atemp20,numreal-2)
             write(20,280) trim(atemp20)
           else
             do ireal=1,numreal-1
               call writint(atemp20,ireal-1)
               write(20,280,advance='no') trim(atemp20)
             end do
             call writint(atemp20,numreal-1)
             write(20,280) trim(atemp20)
           end if
         end if
       else
         write(20) nobs,numreal
         icount=numreal*nobs
         write(20) icount
       end if

C -- Memory is allocated.

       allocate(ooval(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200
       if(outtype.eq.1)then
         if(ar.eq.'c')then
           ndim1=nobs
           ndim2=numreal
           allocate(ovalarray(ndim1,ndim2),stat=ierr)
           if(ierr.ne.0) go to 9200
         end if
       end if

! -- If necessary the base realization is added.

       if(ab.eq.'y')then
         ooval=oval           ! arrays
         if(outtype.eq.2)then
           irow=0
           do iobs=1,nobs
             write(20) irow,iobs-1,ooval(iobs)
           end do
         else
           if(ar.eq.'r')then
             atemp30='base'
             atemp30=adjustl(atemp30)
             write(20,320,advance='no') trim(atemp30)
320          format(a)
             do iobs=1,nobs-1
               write(atemp30,330) ooval(iobs)
330            format(1pg14.7)
               atemp30=adjustl(atemp30)
               write(20,340,advance='no') trim(atemp30)
340            format(',',a)
             end do
             write(atemp30,330) ooval(nobs)
             atemp30=adjustl(atemp30)
             write(20,340) trim(atemp30)
           else
             do iobs=1,nobs
               ovalarray(iobs,1)=ooval(iobs)
             end do
           end if
         end if
       end if

! -- We require less realizations if the base is included.

       if(ab.eq.'y')then
         numreal=numreal-1
       end if

! -- Standard deviations are calculated from weights.

       do iobs=1,nobs
         if(owght(iobs).gt.0.0d0) owght(iobs)=1.0d0/owght(iobs)
       end do

! -- Random numbers are generated.

       do ireal=1,numreal
         do iobs=1,nobs
           if(owght(iobs).eq.0)then
             ooval(iobs)=oval(iobs)
           else
             xx=random_normal()
             ooval(iobs)=oval(iobs)+xx*owght(iobs)
           end if
         end do

         if(outtype.eq.2)then
           if(ab.eq.'y')then
             irow=ireal
           else
             irow=ireal-1
           end if
           do iobs=1,nobs
             write(20) irow,iobs-1,ooval(iobs)
           end do
         else
           if(ar.eq.'r')then
             write(atemp30,'(i30)') ireal-1
             atemp30=adjustl(atemp30)
             write(20,360,advance='no') trim(atemp30)
360          format(a)
             do iobs=1,nobs-1
               write(atemp30,370) ooval(iobs)
370            format(1pg14.7)
               atemp30=adjustl(atemp30)
               write(20,380,advance='no') trim(atemp30)
380            format(',',a)
             end do
             write(atemp30,370) ooval(nobs)
             atemp30=adjustl(atemp30)
             write(20,380) trim(atemp30)
           else
             if(ab.eq.'y')then
               j=ireal+1
             else
               j=ireal
             end if
             do iobs=1,nobs
               ovalarray(iobs,j)=ooval(iobs)
             end do
           end if
         end if
       end do

C -- If realizations are columns, we now write the output file.

       if(outtype.eq.1)then
         if(ar.eq.'c')then
           do iobs=1,nobs
             write(20,370,advance='no') trim(aobs(iobs))
             do j=1,ndim2-1
               write(atemp30,370) ovalarray(iobs,j)
               atemp30=adjustl(atemp30)
               write(20,380,advance='no') trim(atemp30)
             end do
             write(atemp30,370) ovalarray(iobs,ndim2)
             atemp30=adjustl(atemp30)
             write(20,380) trim(atemp30)
           end do
         end if

       else

C -- The last part of the JCB file is written.

         do iobs=1,nobs
           write(20) aobs(iobs)
         end do
         if(ab.eq.'y')then
           atemp200='base'
           atemp200=adjustl(atemp200)
           write(20) atemp200
         end if
         do j=1,numreal
           call writint(atemp20,j-1)
           atemp200=atemp20
           atemp200=adjustl(atemp200)
           write(20) atemp200
         end do

       end if

       close(unit=20)
       write(6,480) trim(aoutfile)
480    format(' - file ',a,' written ok.')

       go to 9900

9000   call writint(aline,iline)
       write(amessage,9010) trim(aline),trim(aobsfile)
9010   format('Error reading line ',a,' of file ',a,'.')
       go to 9890

9050   write(amessage,9060) trim(aobsfile)
9060   format('Cannot find "* observation data" section in file ',
     + a,'.')
       go to 9890

9200   write(amessage,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9100   call writint(aline,iline)
       write(amessage,9110) trim(aline),trim(aobsfile)
9110   format('Insufficient entries on line ',a,' of file ',a,'.')
       go to 9890

9890   continue
       amessage=' '//trim(amessage)
       call writmess(6,amessage)

9900   continue
       deallocate(seed,stat=ierr)
       deallocate(oval,owght,ooval,stat=ierr)
       deallocate(aobs,stat=ierr)
       if(allocated(ovalarray))deallocate(ovalarray,stat=ierr)

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



        SUBROUTINE DREALREAD(IFAIL,CLINE,RTEMP)

C -- Subroutine REALREAD reads a real number from a string.

        INTEGER IFAIL
        double precision RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') LEN_TRIM(CLINE)
        READ(CLINE,AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END

