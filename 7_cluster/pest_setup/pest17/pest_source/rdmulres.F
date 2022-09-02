       program rdmulres

C -- Program RDMULRES reads results from multiple model output files.

       use model_input_output_interface

       implicit none

       logical lexist
       integer ifail,ierr,iline,icount,ilist,nlist,nb,ind,i
       integer ntplfle,ninsfle,npar,nobs,iobs,jobs
       integer lw(10),rw(10)
       character*10 aline,aind
       character*12 aversion
       character*15 anum,aprecis,decpoint
       character*50 aheader
       character*200 infile,ainfile,insfile,modfile,outfile,aoutfile
       character*200 tempfle,infle,dummyfile,afile,bfile
       character*300 cline,dline
       character*300 instruction
       character*500 amessage

       integer, allocatable          :: ind1(:),ind2(:)
       double precision, allocatable :: oval(:)
       character*20, allocatable     :: aobs(:)

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif
       call pgetcl (cline)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' RDMULRES Version ',a,'. Watermark Numerical ',
     + 'Computing.')

       instruction=' '

       call spacesub(cline)
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.eq.0) go to 9400
       call linspl(ifail,1,lw,rw,cline)
       if(ifail.ne.0) go to 9400
       infile=cline(lw(1):rw(1))
       call remchar(infile,char(211))
#ifndef UNIX
       call lowcase(infile)
#endif
       call addquote(infile,ainfile)
       open(unit=10,file=infile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,20) trim(ainfile)
20       format('Cannot open RDMULRES input file ',a,'.')
         go to 9890
       end if

C -- The input file is pre-read in order to establish dimensions.

       write(6,25) trim(ainfile)
25     format(/,' - reading file ',a,'...')

       aheader='* observations'
       iline=0
30     continue
       iline=iline+1
       read(10,'(a)',err=9100,end=9150) cline
       if(cline.eq.' ') go to 30
       cline=adjustl(cline)
       if(cline(1:1).eq.'#') go to 30
       call lowcase(cline)
       if(cline(1:12).ne.aheader(1:12))then
         call writint(aline,iline)
         write(amessage,40) trim(aline),trim(ainfile)
40       format('"* observations" header expected at line ',a,
     +   ' of file ',a,'.')
         go to 9890
       end if

       iobs=0
       aheader='* instruction file'
       do
         iline=iline+1
         read(10,'(a)',err=9100,end=9150) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         call lowcase(cline)
         if(cline(1:12).eq.aheader(1:12)) exit
         iobs=iobs+1
       end do
       nobs=iobs
       if(nobs.eq.0)then
         write(amessage,50) trim(ainfile)
50       format('No observation names cited in "observations" ',
     +   'section of file ',a,'.')
         go to 9890
       end if
       if(nobs.gt.10000)then
         write(amessage,52) trim(ainfile)
52       format('A maximum of 10000 observations can be cited in ',
     +   'file ',a,' as a greater number than this would result in ',
     +   'excessive RDMULRES output file width.')
         go to 9890
       end if

       icount=0
       aheader='* model output file'
       do
         iline=iline+1
         read(10,'(a)',err=9100,end=9150) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         dline=cline
         call lowcase(cline)
         if(cline(1:12).eq.aheader(1:12)) exit
         icount=icount+1
         if(icount.gt.1)then
           call writint(aline,iline)
           write(amessage,60) trim(aline),trim(ainfile)
60         format('"* model output file" section header expected ',
     +     'at line ',a,' of file ',a,'.')
           go to 9890
         end if
         call spacesub(dline)
         call linspl(ifail,1,lw,rw,dline)
         insfile=dline(lw(1):rw(1))
         call remchar(insfile,char(211))
#ifndef UNIX
         call lowcase(insfile)
#endif
       end do
       if(icount.eq.0)then
         write(amessage,65) trim(ainfile)
65       format('No filename provided in "instruction file" section ',
     +   'of file ',a,'.')
         go to 9890
       end if

       icount=0
       aheader='* integer list'
       do
         iline=iline+1
         read(10,'(a)',err=9100,end=9150) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         dline=cline
         call lowcase(cline)
         if(cline(1:12).eq.aheader(1:12)) exit
         icount=icount+1
         if(icount.gt.1)then
           call writint(aline,iline)
           write(amessage,70) trim(aline),trim(ainfile)
70         format('"* integer list" section header expected ',
     +     'at line ',a,' of file ',a,'.')
           go to 9890
         end if
         call spacesub(dline)
         call linspl(ifail,1,lw,rw,dline)
         modfile=dline(lw(1):rw(1))
         call remchar(modfile,char(211))
#ifndef UNIX
         call lowcase(modfile)
#endif
         if(index(modfile,'*').eq.0)then
           call writint(aline,iline)
           write(amessage,100) trim(aline),trim(modfile)
100        format('Model output file expected to contain "*" ',
     +     'character at line ',a,' of file ',a,'.')
           go to 9890
         end if
       end do
       if(icount.eq.0)then
         write(amessage,105) trim(ainfile)
105      format('No filename provided in "model output file" section ',
     +   'of file ',a,'.')
         go to 9890
       end if

       ilist=0
       aheader='* rdmulres output file'
       do
         iline=iline+1
         read(10,'(a)',err=9100,end=9150) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         if(cline(1:12).eq.aheader(1:12)) exit
         ilist=ilist+1
       end do
       nlist=ilist
       if(nlist.eq.0)then
         write(amessage,120) trim(ainfile)
120      format('No integers provided in "integer list" section ',
     +   'of file ',a,'.')
         go to 9890
       end if

       do
         iline=iline+1
         read(10,'(a)',err=9100,end=9170) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         call spacesub(cline)
         call linspl(ifail,1,lw,rw,cline)
         outfile=cline(lw(1):rw(1))
         call remchar(outfile,char(211))
#ifndef UNIX
         call lowcase(outfile)
#endif
         call addquote(outfile,aoutfile)
         exit
       end do

C -- The RDMULRES input file has been read. Memory is now allocated
C    and parts of it are read again.

       allocate(aobs(nobs),oval(nobs),ind1(nlist),ind2(nlist),
     + stat=ierr)
       if(ierr.ne.0) go to 9200

C -- Parts of the RDMULRES input file are now re-read.

       rewind(unit=10)

       iline=0
       do
         iline=iline+1
         read(10,'(a)')cline
         if(index(cline,'* observat').ne.0) exit
       end do
       iobs=0
       do
         iline=iline+1
         read(10,'(a)') cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         call lowcase(cline)
         call linspl(ifail,1,lw,rw,cline)
         if(rw(1)-lw(1)+1.gt.20)then
           call writint(aline,iline)
           write(amessage,160) trim(aline),trim(ainfile)
160        format('Observation name greater than 20 characters ',
     +     'in length at line ',a,' of file ',a,'.')
           go to 9890
         end if
         iobs=iobs+1
         aobs(iobs)=cline(lw(1):rw(1))
         if(iobs.gt.1)then
           do jobs=1,iobs-1
             if(aobs(jobs).eq.aobs(iobs))then
               write(amessage,162) trim(ainfile)
162            format('Observation names provided in the ',
     +         '"observations" section of file ',a,' are not unique.')
               go to 9890
             end if
           end do
         end if
         if(iobs.eq.nobs) exit
       end do

       do
         iline=iline+1
         read(10,'(a)')cline
         if(index(cline,'* integer l').ne.0) exit
       end do
       ilist=0
       do
         iline=iline+1
         read(10,'(a)') cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         ilist=ilist+1
         nb=index(cline,'-')
         call writint(aline,iline)
         if(nb.eq.0)then
           call linspl(ifail,2,lw,rw,cline)
           if(ifail.eq.0) go to 9250
           call intread(ifail,cline(lw(1):rw(1)),ind1(ilist))
           if(ifail.ne.0)then
             write(amessage,180) trim(aline),trim(ainfile)
180          format('Error reading integer from line ',a,
     +       ' of file ',a,'.')
             go to 9890
           end if
           ind2(ilist)=ind1(ilist)
         else
           if(nb.eq.1)then
             write(amessage,190) trim(aline),trim(ainfile)
190          format('Error reading first integer from line ',a,
     +       ' of file ',a,'.')
             go to 9890
           else
             anum=cline(1:nb-1)
             call intread(ifail,anum,ind1(ilist))
             if(ifail.ne.0)then
               write(amessage,190) trim(aline),trim(ainfile)
               go to 9890
             end if
             cline=cline(nb+1:)
             if(cline.eq.' ')then
               write(amessage,200) trim(aline),trim(ainfile)
200            format('Error reading second integer from line ',a,
     +         ' of file ',a,'.')
               go to 9890
             end if
             call linspl(ifail,1,lw,rw,cline)
             anum=cline(lw(1):rw(1))
             call intread(ifail,anum,ind2(ilist))
             if(ifail.ne.0)then
               write(amessage,200) trim(aline),trim(ainfile)
               go to 9890
             end if
             if(ind2(ilist).lt.ind1(ilist))then
               write(amessage,210) trim(aline),trim(ainfile)
210            format('Second integer does not exceed first integer ',
     +         'at line ',a,' of file ',a,'.')
               go to 9890
             end if
           end if
         end if
         if((ind1(ilist).lt.0).or.(ind2(ilist).lt.0))then
           write(amessage,211) trim(aline),trim(ainfile)
211        format('Negative integer not allowed at line ',a,
     +     ' of file ',a,'.')
           go to 9890
         end if
         if(ilist.eq.nlist) exit
       end do
       close(unit=10)
       write(6,220) trim(ainfile)
220    format(' - file ',a,' read ok.')

C -- The model_input_output_interface module is supplied with data.

       aprecis='single'
       decpoint='point'
       ntplfle=1
       ninsfle=1
       npar=1
       call mio_initialise(ifail,ntplfle,ninsfle,npar,nobs,
     + aprecis,decpoint)
       if(ifail.ne.0) go to 9000

       tempfle='dummy.tpl'
       infle='dummy.in'
       dummyfile='dummy.out'
       call mio_put_file(ifail,1,1,tempfle)
       if(ifail.ne.0) go to 9000
       call mio_put_file(ifail,2,1,infle)
       if(ifail.ne.0) go to 9000

       call mio_put_file(ifail,3,1,insfile)
       if(ifail.ne.0) go to 9000
       call mio_put_file(ifail,4,1,dummyfile)
       if(ifail.ne.0) go to 9000
       call mio_store_instruction_set(ifail)
       if(ifail.ne.0) go to 9000

C -- A header is written for the RDMULRES output file.

       write(6,*)
       open(unit=20,file=outfile,recl=max(500,21*nobs+20),
     + action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,240) trim(aoutfile)
240      format('Cannot write model output data to file ',a,'.')
         go to 9890
       end if
       write(20,250) (aobs(iobs),iobs=1,nobs)
250    format(t2,'index',t12,10000(a21))

C -- The model output file is now read.

       do ilist=1,nlist
         do ind=ind1(ilist),ind2(ilist)
           call writint(aind,ind)
           aind=adjustl(aind)
           afile=modfile
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
           write(6,270) trim(bfile)
270        format(' - reading file ',a,'...')
           inquire(file=afile,exist=lexist)
           if(.not.lexist)then
             write(20,280) trim(aind),trim(bfile)
280          format(t2,a,t12,'File ',a,' not found.')
             write(6,290) trim(bfile)
290          format(' - file ',a,' not found.')
           else
             call mio_put_file(ifail,4,1,afile)
             if(ifail.ne.0) go to 9000
             call mio_read_model_output_files(ifail,nobs,aobs,oval,
     +       instruction)
             if(ifail.ne.0) go to 9000
             write(20,310) trim(aind),(oval(iobs),iobs=1,nobs)
310          format(t2,a,t10,10000(1x,1pg14.7,6x))
             write(6,320) trim(afile)
320          format(' - file ',a,' read ok.')
           end if
         end do
       end do

       write(6,*)
       write(6,330) trim(aoutfile)
330    format(' - file ',a,' written ok.')

       go to 9900

9000   call mio_get_message_string(ifail,amessage)
       amessage=' '//trim(amessage)
       call writmess(6,amessage)
       if(instruction.ne.' ')then
         write(6,9010)
9010     format(' Instruction follows:-')
         write(6,9020) trim(instruction)
9020     format(1x,a)
       end if
       go to 9900

9100   call writint(aline,iline)
       write(amessage,9110) trim(aline),trim(ainfile)
9110   format('Error encountered while reading line ',a,
     + ' of RDMULRES input file ',a,'.')
       go to 9890
9150   write(amessage,9160) trim(aheader),trim(ainfile)
9160   format('Cannot find "',a,'" section header in ',
     + 'RDMULRES input file ',a,'.')
       go to 9890
9170   write(amessage,9180) trim(ainfile)
9180   format('End of file ',a,' encountered before name of RDMULRES ',
     + 'output file read.')
        go to 9890

9200   write(amessage,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9250   call writint(aline,iline)
       write(amessage,9260) trim(aline),trim(ainfile)
9260   format('Illegal format for integers at line ',a,
     + ' of file ',a,'.')
       go to 9890

9400   write(6,9410)
9410   format(/,' RDMULRES is run using the command:',/)
       write(6,9420)
9420   format('     rdmulres infile',/)
       write(6,9430)
9430   format(' where',/)
       write(6,9450)
9450   format('     infile is the name of a RDMULRES ',
     + 'input file.')
       go to 9999

9890   continue
       amessage=' '//trim(amessage)
       call writmess(6,amessage)


9900   continue
       deallocate(ind1,ind2,oval,aobs,stat=ierr)
       call mio_finalise(ifail)

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


        subroutine lowcase(astrng)

C -- Subroutine LOWCAS converts a string to lower case.

        integer i,j
        character*(*) astrng

        do 10 i=1,len_trim(astrng)
        j=ichar(astrng(i:i))
        if((j.ge.65).and.(j.le.90)) astrng(i:i)=char(j+32)
10      continue
        return
        end

