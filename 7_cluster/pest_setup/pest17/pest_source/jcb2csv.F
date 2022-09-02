       program jcb2csv

C -- Program JCB2CSV rewrites the contents of a JCB file as a CSV file.

       implicit none

       integer          :: ierr,ifail,nb,i,itemp1,itemp2,ncol,nrow,
     +                     icount,irow,icol
       integer          :: mcolname,mrowname,nn,nrec
       integer          :: lw(5),rw(5)
       double precision :: dtemp
       character*1      :: acomma
       character*4      :: aext
       character*5      :: atrans
       character*12     :: aversion
       character*200    :: aarow,aacol
       character*256    :: jcbfile,afile,csvfile,acsvfile
       character*600    :: cline
       character*1500   :: amessage

       real, allocatable               :: x(:,:)
       character*15, allocatable       :: anum(:)
       character*200, allocatable      :: acol(:)
       character*200, allocatable      :: arow(:)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' JCB2CSV version ',a,'. Watermark Numerical ',
     + 'Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

       acomma=','

C -- The command line is parsed.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,4,lw,lw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       jcbfile=cline(lw(1):rw(1))
       call remchar(jcbfile,char(211))
       csvfile=cline(lw(2):rw(2))
       call remchar(csvfile,char(211))
       atrans=cline(lw(3):rw(3))
       call remchar(atrans,char(211))
#ifndef UNIX
       call lowcas(jcbfile)
       call lowcas(csvfile)
#endif
       call lowcas(atrans)

       write(6,*)
       nb=len_trim(jcbfile)
       i=nb-3
       if(i.lt.1)i=1
       aext=jcbfile(i:nb)
       call lowcas(aext)
       if(aext.ne.'.jcb')then
         call addquote(jcbfile,afile)
         write(amessage,15) trim(afile)
15       format(' Expected extension for enhanced Jacobian matrix ',
     +   'file ',a,' is ".jcb".')
         go to 9800
       end if
       nb=len_trim(csvfile)
       i=nb-3
       if(i.lt.1)i=1
       aext=csvfile(i:nb)
       call lowcas(aext)
       if(aext.ne.'.csv')then
         call addquote(csvfile,afile)
         write(amessage,16) trim(afile)
16       format(' Expected extension for CSV ',
     +   'file ',a,' is ".csv".')
         go to 9800
       end if
       if((atrans.ne.'t').and.(atrans.ne.'nt'))then
         write(amessage,17)
17       format(' The third entry on the JCB2CSV command line must ',
     +   'be "t" or "nt" for "transpose" or "not transpose".')
         go to 9800
       end if

C -- The JCB file is read.

       call addquote(jcbfile,afile)
       write(6,30) trim(afile)
30     format(' - reading binary matrix file ',a,'...')

       call open_unformatted_file(ifail,10,'read',jcbfile,amessage)
       if(ifail.ne.0) go to 9800

       read(10,err=9350,end=9350)itemp1,itemp2
       if(itemp1.le.0)then
         write(amessage,60) trim(afile)
60       format(' File ',a,' does not appear to use enhanced ',
     +   'Jacobian matrix file format. The first integer in the ',
     +   'file must be positive.')
         go to 9800
       end if
       ncol=abs(itemp1)
       nrow=abs(itemp2)
       if((ncol.le.0).or.(nrow.le.0))then
         write(amessage,65) trim(afile)
65       format(' NROW or NCOL not positive in file ',a,
     +   '; is this a valid enhanced Jacobian matrix file?')
         go to 9800
       end if
       allocate(x(nrow,ncol),acol(ncol),arow(nrow),stat=ierr)
       if(ierr.ne.0) go to 9200
       x=0.0d0   ! an array
       read(10,err=9350,end=9350)icount
       do i=1,icount
         read(10,err=9350,end=9350) irow,icol,dtemp
         irow=irow+1
         icol=icol+1
         if((icol.le.0).or.(irow.le.0).or.(icol.gt.ncol)
     +   .or.(irow.gt.nrow))then
           call writint(aacol,icol-1)
           call writint(aarow,irow-1)
           write(amessage,111) trim(aarow),trim(aacol),trim(afile)
111        format(' [row,col] [',a,',',a,
     +     '] out of range in file ',a,'.')
           go to 9800
         end if
         x(irow,icol)=dtemp
       end do
       do icol=1,ncol
         read(10,err=9350,end=9350) acol(icol)
         acol(icol)=adjustl(acol(icol))
       end do
       do irow=1,nrow
         read(10,err=9350,end=9350) arow(irow)
         arow(irow)=adjustl(arow(irow))
       end do
       close(unit=10)
       write(6,70) trim(afile)
70     format(' - binary matrix file ',a,' read ok.')

C -- We find the maximum length of row and column names.

       mcolname=0
       do icol=1,ncol
         nn=len_trim(acol(icol))
         if(nn.gt.mcolname)mcolname=nn
       end do
       mrowname=0
       do irow=1,nrow
         nn=len_trim(arow(irow))
         if(nn.gt.mrowname)mrowname=nn
       end do

C -- We now work out the record length of the CSV file that must be written.

       if(atrans.eq.'nt')then
         nrec=1
         do icol=1,ncol
           nrec=nrec+len_trim(acol(icol))+1
         end do
         if(nrec.lt.15*ncol+mrowname+1)nrec=15*ncol+mrowname+1
       else
         nrec=1
         do irow=1,nrow
           nrec=nrec+len_trim(arow(irow))+1
         end do
         if(nrec.lt.15*nrow+mcolname+1)nrec=15*nrow+mcolname+1
       end if

C -- The CSV file is opened.

       call addquote(csvfile,acsvfile)
       open(unit=20,file=csvfile,action='write',recl=nrec+10,
     + iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,75) trim(acsvfile)
75       format(' Cannot open file ',a,' for writing.')
         go to 9800
       end if

       if(atrans.eq.'nt')then
         write(20,80)(acomma,trim(acol(icol)),icol=1,ncol)
#ifdef LAHEY
80       format(32000(a,a))
#else
80       format(10000000(a,a))
#endif
         allocate(anum(ncol),stat=ierr)
         if(ierr.ne.0) go to 9200
         do irow=1,nrow
           do icol=1,ncol
             write(anum(icol),'(1pg14.7)')x(irow,icol)
             anum(icol)=adjustl(anum(icol))
           end do
           write(20,90) trim(arow(irow)),
     +     (acomma,trim(anum(icol)),icol=1,ncol)
#ifdef LAHEY
90         format(a,32000(a,a))
#else
90         format(a,10000000(a,a))
#endif
         end do
       else
         write(20,80)(acomma,trim(arow(irow)),irow=1,nrow)
         allocate(anum(nrow),stat=ierr)
         if(ierr.ne.0) go to 9200
         do icol=1,ncol
           do irow=1,nrow
             write(anum(irow),'(1pg14.7)')x(irow,icol)
             anum(irow)=adjustl(anum(irow))
           end do
           write(20,90) trim(acol(icol)),
     +     (acomma,trim(anum(irow)),irow=1,nrow)
         end do
       end if

       close(unit=20,iostat=ierr)
       write(6,260) trim(acsvfile)
260    format(' - file ',a,' written ok.')

       go to 9999

8900   continue
       write(6,8910)
8910   format(/,' JCB2CSV is run using the command:',/)
       write(6,8920)
8920   format('     JCB2CSV jcbfile csvfile transpose',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     jcbfile   is the name of an enhanced ',
     + 'Jacobian matrix (ie JCB) file, ')
       write(6,8950)
8950   format('     csvfile   is the name of a comma-delimited ',
     + '(ie CSV) file, and')
       write(6,8960)
8960   format('     transpose must be either "t" or "nt" for ',
     + '"transpose" or "not transpose".')
       go to 9999

9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800
9350   write(amessage,9360) trim(afile)
9360   format(' Error reading matrix from ',
     + 'file ',a,'.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9999   deallocate(x,acol,arow,anum,stat=ierr)
       close(unit=10,iostat=ierr)
       close(unit=20,iostat=ierr)

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
