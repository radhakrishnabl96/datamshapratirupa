       program addcovmat

! --   Program ADDCOVMAT adds covariance matrices to a PEST control file.
! --   Note that any existing covariance matrices are removed. Note also that
!      if a GTARG variable is present for an observation group, then it too is removed.

       implicit none

       logical lopened,ltemp,skipline
       integer, parameter :: MAXGROUP=500
       integer ierr,iline,ifail,n,n1,i
       integer igroup,ngroup,icount
       integer lw(10),rw(10)
       integer ifound(MAXGROUP)
       character*5 aext
       character*10 aline
       character*12 aversion
       character*12 aobsgroup(MAXGROUP)
       character*20 agroup
       character*256 pestfile1,pestfile2,covmatfile,afile,bfile
       character*256 covfile(MAXGROUP)
       character*500 cline
       character*1500 errmsg

! -- Initialisation

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(' ADDCOVMAT Version ',a,
     + '. Watermark Numerical Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

       write(6,*)
       ifound=0    ! an array

! -- The command line is parsed.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       pestfile1=cline(lw(1):rw(1))
       covmatfile=cline(lw(2):rw(2))
       pestfile2=cline(lw(3):rw(3))
       call remchar(pestfile1,char(211))
       call remchar(covmatfile,char(211))
       call remchar(pestfile2,char(211))

#ifndef UNIX
       call lowcas(pestfile1)
       call lowcas(pestfile2)
       call lowcas(covmatfile)
#endif

       afile=pestfile1
       n=len_trim(pestfile1)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=pestfile1(n1:n)
       call lowcas(aext)
       if((aext(1:1).eq.'.').and.(aext.ne.'.pst')) go to 9150
       if(aext.eq.'.pst')pestfile1(n1:n)=' '
       n=len_trim(pestfile1)
       pestfile1(n+1:)='.pst'

       afile=pestfile2
       n=len_trim(pestfile2)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=pestfile2(n1:n)
       call lowcas(aext)
       if((aext(1:1).eq.'.').and.(aext.ne.'.pst')) go to 9150
       if(aext.eq.'.pst')pestfile2(n1:n)=' '
       n=len_trim(pestfile2)
       pestfile2(n+1:)='.pst'

C -- The file listing observation groups and covariance matrix files is opened and read.

       afile=covmatfile
       open(unit=10,file=covmatfile,status='old',action='read',
     + iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,25) trim(covmatfile)
25       format('Cannot open file ',a,'.')
         go to 9890
       end if
       igroup=0
       iline=0
       do
         iline=iline+1
         read(10,'(a)',end=200) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         call spacesub(cline)
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         agroup=cline(lw(1):rw(1))
         call remchar(agroup,char(211))
         n=len_trim(agroup)
         do i=1,n
           if((agroup(i:i).eq.' ').or.(agroup(i:i).eq.char(9)))then
             call writint(aline,iline)
             write(errmsg,40) trim(agroup),trim(aline),trim(afile)
40           format('Observation group name "',a,
     +       '" must not have a space at line ',a,' of file ',a,'.')
             go to 9890
           end if
         end do
         if(n.gt.12)then
           call writint(aline,iline)
           write(errmsg,45) trim(agroup),trim(aline),trim(afile)
45         format('Observation group name "',a,
     +     '" must be 12 characters ',
     +     'or less in length at line ',a,' of file ',a,'.')
           go to 9890
         end if
         igroup=igroup+1
         if(igroup.gt.MAXGROUP)then
           write(errmsg,50) trim(afile)
50         format('Too many observation groups cited in file ',a,
     +     ': increase MAXGROUP and re-compile program.')
           go to 9890
         end if
         call lowcas(agroup)
         aobsgroup(igroup)=agroup
         covfile(igroup)=cline(lw(2):rw(2))
         call remchar(covfile(igroup),char(211))
#ifndef UNIX
         call lowcas(covfile)
#endif
       end do
200    continue
       close(unit=10)
       if(igroup.eq.0)then
         write(errmsg,205) trim(afile)
205      format('No observation groups are featured in file ',a,'.')
         go to 9890
       end if
       call writint(agroup,igroup)
       ngroup=igroup
       write(6,210) trim(agroup)
210    format(' - ',a,' observation groups featured in file ',a,'.')

! -- One PEST control file is read and the new one is written.

       call addquote(pestfile1,afile)
       open(unit=10,file=pestfile1,status='old',action='read',
     + iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,220) trim(afile)
220      format('Cannot open PEST control file ',a,'.')
         go to 9890
       end if
       open(unit=20,file=pestfile2,action='write',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(pestfile2,afile)
         write(errmsg,230) trim(afile)
230      format('Cannot write to file ',a,'.')
         go to 9890
       end if

       iline=1
       read(10,'(a)',err=9070,end=9020) cline
       if(cline(1:4).ne.'pcf ') go to 9020
       write(20,'(a)') trim(cline)
       do
         iline=iline+1
         read(10,'(a)',err=9070,end=9100) cline
         write(20,'(a)') trim(cline)
         if(cline(1:1).eq.'*')then
           ltemp=skipline(cline)
           call lowcas(cline)
           if(cline(1:20).eq.'* observation groups') exit
         end if
       end do
       do
         iline=iline+1
         read(10,'(a)',err=9070,end=9250) cline
         if(skipline(cline))then
           write(20,'(a)') trim(cline)
           cycle
         end if
         call lowcas(cline)
         if(cline(1:1).eq.'*') go to 300
         call linspl(ifail,1,lw,rw,cline)
         agroup=cline(lw(1):rw(1))
         do igroup=1,ngroup
           if(agroup.eq.aobsgroup(igroup))then
             call addquote(covfile(igroup),bfile)
             write(20,240) trim(agroup),trim(bfile)
             ifound(igroup)=1
240          format(1x,a,t15,a)
             go to 250
           end if
         end do
         write(20,'(a)') trim(cline)
250      continue
       end do
300    continue
       write(20,'(a)') trim(cline)
       do
         iline=iline+1
         read(10,'(a)',end=400) cline
         write(20,'(a)') trim(cline)
       end do
400    continue
       close(unit=10)
       close(unit=20)
       write(6,410) trim(afile)
410    format(' - file ',a,' read ok.')
       call addquote(pestfile2,afile)
       write(6,420) trim(afile)
420    format(' - file ',a,' written ok.')
       icount=0
       do igroup=1,ngroup
         if(ifound(igroup).eq.0) icount=icount+1
       end do
       if(icount.ne.0)then
         write(6,*)
         write(6,440)
440      format(' Warning: the following observation groups ',
     +   'not found in PEST control file.')
         do igroup=1,ngroup
           if(ifound(igroup).eq.0)then
             write(6,450) trim(aobsgroup(igroup))
450          format(3x,'"',a,'"')
           end if
         end do
       end if

       go to 9900

8900   continue

       write(6,8910)
8910   format(/,' ADDCOVMAT is run using the command:',/)
       write(6,8920)
8920   format('     addcovmat pestfile1 covmatfile pestfile2')
       write(6,8930)
8930   format(/,' where',/)
       write(6,8940)
8940   format('     pestfile1  is an existing PEST control file,')
       write(6,8945)
8945   format('     covmatfile specifies observation groups and ',
     + 'covariance matrices, and')
       write(6,8950)
8950   format('     pestfile2  is a new PEST control file to be ',
     + 'written by ADDCOVMAT.')

       go to 9999

9000   call writint(aline,iline)
       write(errmsg,9010) trim(aline),trim(afile)
9010   format('Insufficient entries on line ',a,' of file ',a,'.')
       go to 9890

9020   write(errmsg,9030) trim(afile)
9030   format('File ',a,' does not appear to be a PEST control file.')
       go to 9890

9070   call writint(aline,iline)
       write(errmsg,9080) trim(aline),trim(afile)
9080   format('Error reading line ',a,' of file ',a,'.')
       go to 9890

9100   write(errmsg,9110) trim(afile)
9110   format('Cannot find "* observation groups" section ',
     + 'header in file ',a,'.')
       go to 9890

9150   write(errmsg,9160) trim(afile)
9160   format('File ',a,' does not have an extension of ".pst".')
       go to 9890

9250   write(errmsg,9260) trim(afile)
9260   format('Unexpected end encountered to file ',a,' while ',
     + 'reading "observation groups" section of this file.')
       go to 9890

9890   continue
       call write_message(errmsg)
9900   continue

       inquire(unit=10,opened=lopened)
       if(lopened) close(unit=20,status='delete')

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
