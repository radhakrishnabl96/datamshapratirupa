       program phistats

! --   Program PHISTATS reads a run record file. It writes a brief report of
!      initial and completion objective function statistics to the screen.

       implicit none

       integer, parameter :: MAXCLINE=200
       integer ifail,ierr,iline,i,nequals
       integer n,n1,inum,nspace,pestmode,iregflag,ncline
       integer yy,mm,dd,hh,min
       integer lw(10),rw(10)
       real ss,predval,prederr
       character*4 aext
       character*10 anum,blank
       character*12 aversion
       character*15 result1,result2
       character*200 cline,atemp
       character*256 recfile,qrecfile
       character*500 amessage
       character*150 ccline(MAXCLINE)

       include 'version.inc'
       write(6,5) trim(aversion)
5      format(' PHISTATS Version ',a,'. Watermark Numerical Computing.')
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
       recfile=cline(lw(1):rw(1))
       anum=cline(lw(2):rw(2))
       call remchar(recfile,char(211))
       n=len_trim(recfile)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=recfile(n1:n)
       call lowcas(aext)
       if(aext.ne.'.rec')recfile=trim(recfile)//'.rec'
       call intread(ifail,anum,inum)
       if(ifail.ne.0) go to 9000

! -- Some initialization

       blank=' '
       nspace=3

! -- We write a header. This is really intended for an output file.

       call addquote(recfile,qrecfile)
       write(6,*)
       write(6,10)
10     format(79('-'))
       call date_and_time(date=result1,time=result2)
       read(result1,'(i4,i2,i2)') yy,mm,dd
       read(result2,'(i2,i2,f6.0)') hh,min,ss
       call writint(anum,inum)
       write(6,20) trim(anum)
20     format(' User specified index',t25,': ',a)
       write(6,25) trim(qrecfile)
25     format(' Run record file',t25,': ',a)
       write(6,30) dd,mm,yy
30     format(' Date [dd/mm/yyyy]',t25,': ',i2.2,'/',i2.2,'/',i4.4)
       write(6,35) hh,min,ss
35     format(' Time [hh:mm.ss]',t25,': ',i2.2,':',i2.2,'.',f4.1)

! -- The initial PEST control file is opened.

       open(unit=10,file=recfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,50) trim(qrecfile)
50       format('Cannot open PEST run record file ',a,'.')
         go to 9890
       end if

! -- It is checked that the file is indeed a run record file.

       iline=0
       do i=1,3
         iline=iline+1
         read(10,'(a)',err=9100,end=9150) cline
         if(index(cline,'PEST RUN RECORD').ne.0) go to 70
       end do
       write(amessage,60) trim(qrecfile)
60     format('File ',a,' does not appear to be a PEST run record ',
     + 'file.')
       go to 9890

! -- We establish what mode PEST is run in.

70     continue
       do i=1,20
         iline=iline+1
         read(10,'(a)',err=9100,end=9200) cline
         call lowcas(cline)
         if(index(cline,'run mode').ne.0) then
           iline=iline+1
           read(10,'(a)',err=9100,end=9300) cline
           if(cline.ne.' ') go to 9200
           iline=iline+1
           read(10,'(a)',err=9100,end=9300) cline
           call lowcas(cline)
           if(index(cline,'estimation').ne.0)then
             pestmode=1
             write(6,72) 'estimation'
           else if(index(cline,'regul').ne.0)then
             pestmode=3
             write(6,72) 'regularisation'
           else if(index(cline,'predict').ne.0)then
             pestmode=2
             write(6,72)'predictive analysis'
72           format(' PEST run mode',t25,': ',a)
           else if(index(cline,'pareto').ne.0)then
             write(amessage,73)
73           format(' PHISTATS cannot read a run record file ',
     +       'documenting a PEST run in Pareto mode.')
             go to 9890
           else
             go to 9200
           end if
           go to 100
         end if
       end do
       go to 9200
100    continue

! -- We now look for the initial conditions reports.

       do
         iline=iline+1
         read(10,'(a)',err=9100,end=9300) cline
         if(index(cline,'INITIAL CONDITIONS:').ne.0) go to 150
       end do
150    continue
       write(6,*)
       write(6,170)
170    format(' Initial model run:-')
       iregflag=0
       do
         iline=iline+1
         read(10,'(a)',err=9100,end=9350) cline
         if(cline.eq.' ') then
           if(pestmode.ne.3) then
             go to 200
           else
             if(iregflag.eq.0)then
               iregflag=1
               write(6,*)
               cycle
             else
               go to 200
             end if
           end if
         end if
         cline=adjustl(cline)
         if(nequals.eq.0)then
           nequals=index(cline,'=')
           if(nequals.eq.0) nequals=59
         end if
         cline=blank(1:nspace)//trim(cline)
         write(6,'(a)') trim(cline)
       end do
200    continue

! -- Now we look for the completion statistics.

       do
         iline=iline+1
         read(10,'(a)',err=9100,end=9400) cline
         if(index(cline,'OPTIMISATION RESULTS').ne.0) go to 250
       end do
250    continue

       if(pestmode.eq.2)then
         do
           iline=iline+1
           read(10,'(a)',err=9100,end=9400) cline
           if(index(cline,'Prediction ----->').ne.0) go to 270
         end do
270      continue
         do i=1,2
           iline=iline+1
           read(10,'(a)',err=9100,end=9400) cline
         end do
         iline=iline+1
         read(10,'(a)',err=9100,end=9400) cline
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9400
         call realread(ifail,cline(lw(2):rw(2)),predval)
         if(ifail.ne.0) go to 9450
         call linspl(ifail,4,lw,rw,cline)
         if(ifail.eq.0) then
           call realread(ifail,cline(lw(4):rw(4)),prederr)
           if(ifail.ne.0) prederr=-1.1e35
         else
           prederr=-1.1e35
         end if
       end if
       do
         iline=iline+1
         read(10,'(a)',err=9100,end=9400) cline
         if(index(cline,'Objective function ----->').ne.0) go to 300
       end do
300    continue
       iline=iline+1
       read(10,'(a)',err=9100,end=9400) cline
       write(6,310)
310    format(/,' Completion of PEST run:-')
       if((pestmode.eq.1).or.(pestmode.eq.2))then
         do
           iline=iline+1
           read(10,'(a)',err=9100,end=9350) cline
           if(cline.eq.' ') then
             if(pestmode.eq.1) then
               go to 700
             else
               go to 320
             end if
           end if
           cline=adjustl(cline)
           n=index(cline,'=')
           if(n.ne.0)then
             atemp=cline(n:)
             cline=cline(1:n-1)
             cline(nequals:)=trim(atemp)
           end if
           cline=blank(1:nspace)//trim(cline)
           write(6,'(a)') trim(cline)
         end do
320      continue
         cline='Prediction'
         cline(nequals:nequals)='='
         write(cline(nequals+1:),340) predval
340      format(1x,1pg11.4)
         cline=blank(1:nspace)//trim(cline)
         write(6,'(a)') trim(cline)
         if(prederr.gt.-1.0e35)then
           cline='Predictive error'
           cline(nequals:nequals)='='
           write(cline(nequals+1:),340) prederr
           cline=blank(1:nspace)//trim(cline)
           write(6,'(a)') trim(cline)
         end if
         go to 700
       else if(pestmode.eq.3)then
         do i=1,MAXCLINE
           iline=iline+1
           read(10,'(a)',err=9100,end=9350) ccline(i)
           if(ccline(i).eq.' ') then
             ncline=i-1
             go to 380
           end if
           ccline(i)=adjustl(ccline(i))
         end do
         do
           iline=iline+1
           read(10,'(a)',err=9100,end=9350) cline
           if(cline.eq.' ') then
             ncline=MAXCLINE
             go to 380
           end if
         end do
380      continue
         do
           iline=iline+1
           read(10,'(a)',err=9100,end=9350) cline
           if(cline.eq.' ') go to 390
           cline=adjustl(cline)
           n=index(cline,'=')
           if(n.ne.0)then
             atemp=cline(n:)
             cline=cline(1:n-1)
             cline(nequals:)=trim(atemp)
           end if
           cline=blank(1:nspace)//trim(cline)
           write(6,'(a)') trim(cline)
         end do
390      continue
         write(6,*)
         do i=1,ncline
           cline=trim(ccline(i))
           n=index(cline,'=')
           if(n.ne.0)then
             atemp=cline(n:)
             cline=cline(1:n-1)
             cline(nequals:)=trim(atemp)
           end if
           cline=blank(1:nspace)//trim(cline)
           write(6,'(a)') trim(cline)
         end do
         go to 700
       end if

! -- All work has been done.

700    continue
       go to 9900

9000   continue

       write(6,9010)
9010   format(/,' PHISTATS is run using the command:',/)
       write(6,9020)
9020   format('     phistats recfile index',/)
       write(6,9030)
9030   format(' where',/)
       write(6,9050)
9050   format('     recfile is the name of a PEST run record file, and')
       write(6,9060)
9060   format('     index   is an arbitrary user-supplied ',
     + 'integer index.')
       go to 9999

9100   call writint(anum,iline)
       write(amessage,9110) trim(anum),trim(qrecfile)
9110   format('Error encountered when reading line ',a,' of run ',
     + 'record file ',a,'.')
       go to 9890

9150   write(amessage,9160) trim(qrecfile)
9160   format('End of file ',a,' encountered before any useable ',
     + 'information found.')
       go to 9890

9200   write(amessage,9210) trim(qrecfile)
9210   format('Cannot ascertain PEST''s run mode from file ',a,'.')
       go to 9890

9300   write(amessage,9310) trim(qrecfile)
9310   format('End of file ',a,' encountered before finding initial ',
     + 'phi values.')
       go to 9890

9350   write(amessage,9360) trim(qrecfile)
9360   format('Unexpected end encountered to file ',a,'.')
       go to 9890

9400   write(amessage,9410) trim(qrecfile)
9410   format('Cannot find completion statistics on file ',a,'.')
       go to 9890

9450   write(amessage,9460) trim(qrecfile)
9460   format('Cannot read completion prediction data from file ',a,'.')
       go to 9890

9890   call write_message(amessage)

9900   continue
       write(6,9910)
9910   format(79('-'),/)
       close(unit=10,iostat=ierr)

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



        subroutine realread(ifail,cline,rtemp)

C -- Subroutine REALREAD reads a real number from a string.

        integer ifail
        real rtemp
        character*8 afmt
        character*(*) cline

        ifail=0
        afmt='(f   .0)'
        write(afmt(3:5),'(i3)') len(cline)
        read(cline,afmt,err=100) rtemp
        return

100     ifail=1
        return
        end
