
       program postjactest

C -- Program POSTJACTEST reads the tabular data file written by JACTEST, identifying
C    problematical entries in that file.

       implicit none

       integer, parameter  ::  MAXCOL=200
       integer  ifail,ierr,i,ncol,iline,icol
       integer lw(MAXCOL),rw(MAXCOL)
       double precision thresh,bigdiff,mindiff,maxdiff,diff,rtemp
       real oval(MAXCOL)
       character*10 aline
       character*12 aversion
       character*20 aobs
       character*200 infile,outfile
       character*500 amessage
       character*3000 cline

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' POSTJACTEST Version ',a,'. Watermark Numerical ',
     + 'Computing.')


#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

! -- The commmand line is parsed.

       if(cline.eq.' ') go to 9400
       call spacesub(cline)
       call linspl(ifail,4,lw,lw,cline)
       if(ifail.eq.0) go to 9400
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 9400
       infile=cline(lw(1):rw(1))
       call remchar(infile,char(211))
       outfile=cline(lw(3):rw(3))
       call remchar(outfile,char(211))
       call drealread(ifail,cline(lw(2):rw(2)),thresh)
       if(ifail.ne.0) go to 9400
       if(thresh.le.0.0)then
         write(amessage,20)
20       format(' Threshold supplied on POSTJACTEST command ',
     +   'line must be greater than zero.')
         go to 9890
       end if

#ifndef UNIX
       call lowcas(infile)
       call lowcas(outfile)
#endif

C -- The input file is opened.

       open(unit=10,file=infile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,50) trim(infile)
50       format(' Cannot open JACTEST output file ',a,'.')
         go to 9890
       end if

C -- The first line of the file is read.

       read(10,'(a)',err=9000) cline
       if(cline.eq.' ') go to 9000
       cline=adjustl(cline)
       call lowcas(cline)
       if(cline(1:6).ne.'param_')then
         write(amessage,60) trim(infile)
60       format(' File ',a,' does not appear to be a JACTEST output ',
     +   'file.')
         go to 9890
       end if

C -- Establish how many data columns in the file.

       do i=1,MAXCOL
         call linspl(ifail,i+1,lw,rw,cline)
         if(ifail.ne.0)then
           ncol=i-1
           go to 100
         end if
       end do
       write(amessage,70) trim(infile)
70     format(' Too many data columns in file ',a,'. Increase MAXCOL ',
     + 'and re-compile program.')
       go to 9890

C -- Open the output file.

100    continue
       if(ncol.lt.3)then
         write(amessage,105) trim(infile)
105      format(' There must be at least 3 data columns in file ',
     +   a,'.')
         go to 9890
       end if
       open(unit=20,file=outfile)
       write(20,106)
106    format(1x,'Observation_name',t25,
     + 'Relative_difference_discrepancy')

C -- Now process the input file and write to the output file.

       iline=1
       do
         iline=iline+1
         read(10,*,end=500,err=9100) aobs,(oval(icol),icol=1,ncol)
         call lowcas(aobs)
         bigdiff=oval(ncol)-oval(1)
         mindiff=1.0e35
         maxdiff=-1.0e35
         do icol=1,ncol-1
           diff=oval(icol+1)-oval(icol)
           if(diff.lt.mindiff)mindiff=diff
           if(diff.gt.maxdiff)maxdiff=diff
         end do
         if((abs(maxdiff).le.thresh).and.(abs(bigdiff).le.thresh))cycle
         if(abs(bigdiff).eq.0.0)then
           if(abs(maxdiff).eq.0.0)then
             rtemp=0.0
           else
             rtemp=1.0e35
           end if
         else
           rtemp=abs((maxdiff-mindiff)/bigdiff)*ncol
         end if
         write(20,250) trim(aobs),rtemp
250      format(1x,a,t30,1pg14.7)
       end do

500    continue
       close(unit=10)
       write(6,510) trim(infile)
510    format(' - file ',a,' read ok.')
       close(unit=20)
       write(6,520) trim(outfile)
520    format(' - file ',a,' written ok.')

       go to 9999

9000   write(amessage,9010) trim(infile)
9010   format(' Error encountered in reading first line of file ',a,
     + '.')
       go to 9890
9100   call writint(aline,iline)
       write(amessage,9110) trim(aline),trim(infile)
9110   format(' Error encountered while reading line ',a,
     + ' if file ',a,'.')
       go to 9890

9400   write(6,9410)
9410   format(/,' POSTJACTEST is run using the command:',/)
       write(6,9420)
9420   format('     postjactest jactestfile thresh outfile',/)
       write(6,9430)
9430   format(' where',/)
       write(6,9450)
9450   format('     jactestfile  is the name of a JACTEST ',
     + 'output file,')
       write(6,9451)
9451   format('     thresh       is a threshold model output ',
     + 'difference, and')
       write(6,9453)
9453   format('     outfile      is the name of the POSTJACTEST ',
     + 'output file.')
       go to 9999

9890   continue
       call writmess(6,amessage)

9999   continue

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



        subroutine drealread(ifail,cline,rtemp)

C -- Subroutine DREALREAD reads a double precision number from a string.

        integer ifail
        double precision rtemp
        character*8 afmt
        character*(*) cline

        ifail=0
        afmt='(f   .0)'
        write(afmt(3:5),'(i3)') len_trim(cline)
        read(cline,afmt,err=100) rtemp
        return

100     ifail=1
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

