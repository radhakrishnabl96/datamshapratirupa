       program comfilnme1

C -- Program COMFILNME1 compresses filenames which include integer indices into sequential order.

       implicit none

       integer, parameter   :: MAXLIST=10000
       integer              :: ifail,ierr,iline,ilist,nb,itemp1,itemp2,
     +                         i,nlist
       integer              :: lw(10),rw(10)
       integer              :: inum(MAXLIST)
       character*12         :: aline,anum,aind,aversion
       character*200        :: parfile,listfile,alistfile
       character*200        :: outfile
       character*200        :: afile,bfile,cfile,dfile
       character*3000       :: cline
       character*500        :: amessage


#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' COMFILNME1 version ',a,'. Watermark Numerical ',
     + 'Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

C -- The command line is parsed.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,4,lw,lw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       parfile=cline(lw(1):rw(1))
       call remchar(parfile,char(211))
       listfile=cline(lw(2):rw(2))
       call remchar(listfile,char(211))
       outfile=cline(lw(3):rw(3))
       call remchar(outfile,char(211))
#ifndef UNIX
       call lowcas(parfile)
       call lowcas(listfile)
       call lowcas(outfile)
#endif

       if(outfile.eq.parfile)then
         write(amessage,11)
11       format(' New filenames must not be the same as ',
     +   'old filenames.')
         go to 9800
       end if

C -- The generic parameter value file name is tested.

       if(index(parfile,'*').eq.0)then
         write(amessage,30) trim(parfile)
30       format(' Generic data/text filename "',a,
     +   '"  must contain at least one "*" character.')
         go to 9800
       end if
       if(index(outfile,'*').eq.0)then
         write(amessage,30) trim(outfile)
         go to 9800
       end if

C -- The list file is read.

       call addquote(listfile,alistfile)
       write(6,35) trim(alistfile)
35     format(/,' - reading integer list file ',a,'...')
       open(unit=11,file=listfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,40) trim(alistfile)
40       format(' Cannot open integer list file ',a,'.')
         go to 9800
       end if

       iline=0
       ilist=0
       do
         iline=iline+1
         read(11,'(a)',end=115)cline
         if(cline.eq.' ')cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'*') cycle
         if(cline(1:1).eq.'#') cycle
         nb=index(cline,'-')
         call writint(aline,iline)
         if(nb.eq.0)then
           call linspl(ifail,2,lw,rw,cline)
           if(ifail.eq.0) go to 9250
           ilist=ilist+1
           if(ilist.gt.MAXLIST)then
             write(amessage,50) trim(alistfile)
50           format(' Too many listed or implied integers in file ',a,
     +       '. Increase MAXLIST and re-compile program.')
             go to 9800
           end if
           call intread(ifail,cline(lw(1):rw(1)),inum(ilist))
           if(ifail.ne.0)then
             write(amessage,60) trim(aline),trim(alistfile)
60           format(' Error reading integer from line ',a,
     +       ' of file ',a,'.')
             go to 9800
           end if
         else
           if(nb.eq.1)then
             write(amessage,80) trim(aline),trim(alistfile)
80           format(' Error reading first integer from line ',a,
     +       ' of file ',a,'.')
             go to 9800
           else
             anum=cline(1:nb-1)
             call intread(ifail,anum,itemp1)
             if(ifail.ne.0)then
               write(amessage,80) trim(aline),trim(alistfile)
               go to 9800
             end if
             cline=cline(nb+1:)
             if(cline.eq.' ')then
               write(amessage,90) trim(aline),trim(alistfile)
90             format(' Error reading second integer from line ',a,
     +         ' of file ',a,'.')
               go to 9800
             end if
             call linspl(ifail,1,lw,rw,cline)
             anum=cline(lw(1):rw(1))
             call intread(ifail,anum,itemp2)
             if(ifail.ne.0)then
               write(amessage,90) trim(aline),trim(alistfile)
               go to 9800
             end if
             if(itemp2.lt.itemp1)then
               write(amessage,110) trim(aline),trim(alistfile)
110            format(' Second integer does not exceed first integer ',
     +         'at line ',a,' of file ',a,'.')
               go to 9800
             end if
             do i=itemp1,itemp2
               ilist=ilist+1
               if(ilist.gt.MAXLIST)then
                 write(amessage,50) trim(alistfile)
                 go to 9800
               end if
               inum(ilist)=i
             end do
           end if
         end if
       end do
115    nlist=ilist
       close(unit=11)
       if(nlist.eq.0)then
         write(amessage,116) trim(alistfile)
116      format(' No integers are listed in file ',a,'.')
         go to 9800
       end if
       if(nlist.gt.1)then
         do ilist=2,nlist
           if(inum(ilist).le.inum(ilist-1))then
             write(amessage,117) trim(alistfile)
117          format(' Indicial integers in file ',a,' are not ',
     +       'supplied in increasing order.')
             go to 9800
           end if
         end do
       end if
       write(6,120) trim(alistfile)
120    format(' - file ',a,' read ok.')

C -- File transfer is now undertaken.

       do ilist=1,nlist
         call writint(aind,inum(ilist))
         aind=adjustl(aind)
         afile=parfile
         do
           nb=index(afile,'*')
           if(nb.eq.0) exit
           if(nb.eq.1)then
             afile=trim(aind)//trim(afile(2:))
           else
             afile=afile(1:nb-1)//trim(aind)//trim(afile(nb+1:))
           end if
         end do
         call addquote(afile,bfile)
         open(unit=10,file=afile,status='old',iostat=ierr)
         if(ierr.ne.0)then
           write(amessage,210) trim(bfile)
210        format(' Cannot open file ',a,'.')
           go to 9800
         end if
         call writint(aind,ilist)
         aind=adjustl(aind)
         cfile=outfile
         do
           nb=index(cfile,'*')
           if(nb.eq.0) exit
           if(nb.eq.1)then
             cfile=trim(aind)//trim(cfile(2:))
           else
             cfile=cfile(1:nb-1)//trim(aind)//trim(cfile(nb+1:))
           end if
         end do
         call addquote(cfile,dfile)
         write(6,*)
         write(6,212) trim(bfile),trim(dfile)
212      format(' - copying file ',a,' to file ',a,'...')
         open(unit=20,file=cfile,action='write',iostat=ierr)
         if(ierr.ne.0)then
           write(amessage,220) trim(dfile)
220        format(' Cannot write to file ',a,'.')
           go to 9800
         end if
         do
           read(10,'(a)',end=300) cline
           write(20,'(a)') trim(cline)
         end do
300      continue
         close(unit=10)
         close(unit=20)
         write(6,310)
310      format(' - copying done ok.')
       end do

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' COMFILNME1 is run using the command:',/)
       write(6,8920)
8920   format('     comflenme datfile listfile newdatfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     datfile    is a generic data or text file name,')
       write(6,8950)
8950   format('     listfile   contains a list of integer ',
     + 'indices, and.')
       write(6,8960)
8960   format('     newdatfile is a new generic data or text ',
     + 'file name.')

       go to 9900

9250   write(amessage,9260) trim(aline),trim(alistfile)
9260   format(' Illegal format for integers at line ',a,
     + ' of integer list file ',a,'.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9900   continue

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

C -- Subroutine LOWCAS converts a string to lower case.

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

