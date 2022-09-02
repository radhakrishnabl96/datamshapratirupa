       program jcotrans

C -- Program jcotrans translates an old JCO file to new format.

       implicit none
       integer          :: n,n1,ierr,nxrow,nespar,i,j,ipp,iobs,
     +                     icount,ifail
       integer          :: lw(3),rw(3)
       double precision, allocatable  :: x(:,:)
       character*12                   :: aversion
       character*15                   :: aext
       character*200                  :: jcofile1,jcofile2,afile
       character*400                  :: errmsg,cline
       character*12, allocatable      :: apar(:),bobs(:)
       character*20, allocatable      :: aobs(:)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(' JCOTRANS Version ',a,'. Watermark Numerical ',
     + 'Computing.')
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
       jcofile1=cline(lw(1):rw(1))
       jcofile2=cline(lw(2):rw(2))
       call remchar(jcofile1,char(211))
       call remchar(jcofile2,char(211))
       n=len_trim(jcofile1)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=jcofile1(n1:n)
       call lowcase(aext)
       if(aext.ne.'.jco')then
         jcofile1(n+1:)='.jco'
       end if
       n=len_trim(jcofile2)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=jcofile2(n1:n)
       call lowcase(aext)
       if(aext.ne.'.jco')then
         jcofile2(n+1:)='.jco'
       end if

C -- The first JCO file is opened.

#ifdef LAHEY
#ifdef LF90
       open(unit=15,file=jcofile1,form='unformatted',
     + access='transparent',status='old',iostat=ierr)
#else
       open(unit=15,file=jcofile1,form='binary',
     + status='old',iostat=ierr)
#endif
#else
       open(unit=15,file=jcofile1,form='unformatted',
     + access='stream',status='old',iostat=ierr)
#endif
       if(ierr.ne.0)then
         call addquote(jcofile1,afile)
         write(errmsg,120) trim(afile)
120      format('Cannot open unformatted Jacobian file ',a,'.')
         call write_message(errmsg)
         stop
       end if

       read(15,err=9250,end=9250)nespar,nxrow
       if(nespar.gt.0)then
         continue
       else
         call addquote(jcofile1,afile)
         write(errmsg,240) trim(afile)
240      format('Jacobian matrix file ',a,' appears to have ',
     +   'been written by version 8 or later of PEST; hence there ',
     +   'is no need to use JCOTRANS to translate this file into ',
     +   'the new JCO file format.')
         go to 9890
       end if

       allocate(x(nxrow,nespar),aobs(nxrow),apar(nespar),bobs(nxrow),
     + stat=ierr)
       if(ierr.ne.0)then
         call addquote(jcofile1,afile)
         write(errmsg,250) trim(afile)
250      format('Error in memory allocation; was file ',a,' really ',
     +   'written by PEST?')
         go to 9890
       end if

       read(15,err=9250,end=9250) ((x(j,i),j=1,nxrow),i=1,nespar)
       do ipp=1,nespar
         read(15,err=9250,end=9250) apar(ipp)
       end do
       do iobs=1,nxrow
         read(15,err=9250,end=9250) bobs(iobs)
       end do
       close(unit=15)
       write(6,*)
       write(6,200) trim(jcofile1)
200    format(' - file ',a,' read ok.')

       do i=1,nxrow
         aobs(i)=bobs(i)
       end do

#ifdef LAHEY
#ifdef LF90
       open(unit=20,file=jcofile2,form='unformatted',
     + access='transparent',action='write',err=9400)
#else
       open(unit=20,file=jcofile2,form='binary',
     + action='write',err=9400)
#endif
#else
       open(unit=20,file=jcofile2,form='unformatted',
     + access='stream',action='write',status='replace',err=9400)
#endif

       write(20,err=9400)-nespar,-nxrow
       icount=0
       do i=1,nespar
          do j=1,nxrow
            if(x(j,i).ne.0.0d0) icount=icount+1
          end do
       end do
       write(20,err=9400) icount
       do i=1,nespar
         do j=1,nxrow
           if(x(j,i).ne.0.0d0)then
             icount=(i-1)*nxrow+j
             write(20,err=9400)icount,x(j,i)
           end if
         end do
       end do
       do ipp=1,nespar
          write(20,err=9400) apar(ipp)
       end do
       do iobs=1,nxrow
         write(20,err=9400) aobs(iobs)
       end do
       close(unit=20)
       call addquote(jcofile2,afile)
       write(6,490) trim(afile)
490    format(' - file ',a,' written ok.')
       go to 9900

9000   continue
       write(6,9010)
9010   format(/,' JCOTRANS is run using the command:',/)
       write(6,9020)
9020   format('     jcotrans jcofile1 jcofile2',/)
       write(6,9030)
9030   format(' where',/)
       write(6,9050)
9050   format('     jcofile1 is a JCO file written using the old ',
     + 'format, and ')
       write(6,9060)
9060   format('     jcofile2 contains the same information written ',
     + 'in the new format.')
       go to 9999

9250   call addquote(jcofile1,afile)
       write(errmsg,9260) trim(afile)
9260   format('Error reading Jacobian matrix file ',a,'.')
       go to 9890

9400   call addquote(jcofile2,afile)
       write(errmsg,9410) trim(afile)
9410   format('Cannot write to file ',a,'.')
       go to 9890


9890   call write_message(errmsg)
9900   continue

       deallocate(x,apar,aobs,bobs,stat=ierr)

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



        subroutine lowcase(astrng)

C -- Subroutine LOWCASE converts a string to lower case.

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



