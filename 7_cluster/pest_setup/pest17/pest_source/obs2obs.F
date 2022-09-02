       program obs2obs

C -- Program OBS2OBS is a PEST-compataible model post-processor.

       use equation
       use model_input_output_interface
       implicit none

       logical lval,lopened
       integer, parameter :: MAXOUTFILE=40
       integer, parameter :: MAXOBS=200000
       integer ifail,ierr,iline,jline,i,j,ii,nb,itype,ieqn
       integer nn,nn1,nn2,nn3
       integer noutfile,nobs,numin,npar,neqn
       integer lw(5),rw(5)
       double precision rval
       character*1 markdel
       character*1 bb
       character*10 aline
       character*12 aversion,precision,decpoint
       character*25 atemp
       character*200 insfile,infile,outfile,afile,bfile
       character*500 instruction,errmsg
       character*2000 cline

       double precision, allocatable :: obsval(:)
       double precision, allocatable :: totobsval(:)
       character*20, allocatable     :: totobsname(:)
       character*200 insfle(MAXOUTFILE),outfle(MAXOUTFILE)
       character*20 aobs(MAXOBS)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' OBS2OBS Version ',a,'. Watermark Numerical ',
     + 'Computing.')

       instruction=' '
       insfile=' '

       call pgetcl (cline)
       if(cline.eq.' ') go to 9800
       call spacesub(cline)
       call linesplit(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 9800
       call linesplit(ifail,3,lw,rw,cline)
       if(ifail.eq.0) then
         insfile=cline(lw(3):rw(3))
         call remchar(insfile,char(211))
#ifndef UNIX
         call lowcase(insfile)
#endif
       end if

       call linesplit(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9800
       outfile=cline(lw(2):rw(2))
       call remchar(outfile,char(211))
#ifndef UNIX
       call lowcase(outfile)
#endif

       infile=cline(lw(1):rw(1))
       call remchar(infile,char(211))
#ifndef UNIX
       call lowcase(infile)
#endif

C -- The first section of OBS2OBS input file is read.

       iline=0
       call addquote(infile,afile)
       open(unit=10,file=infile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,12) trim(afile)
12       format('Cannot open OBS2OBS input file ',a,'.')
         go to 9890
       end if
       do
         iline=iline+1
         read(10,'(a)',end=9000) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         call lowcase(cline)
         if(cline.ne.'* model output')then
           call writint(aline,iline)
           write(errmsg,20) trim(aline),trim(infile)
20         format('"* model output" string expected at line ',
     +     a,' of file ',a,'.')
           go to 9890
         end if
         exit
       end do

       noutfile=0
       do
         iline=iline+1
         read(10,'(a)',end=9150) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         if(cline(1:1).eq.'*') then
           call lowcase(cline)
           if(cline.eq.'* equations') then
             exit
           else
             call writint(aline,iline)
             write(errmsg,30) trim(aline),trim(infile)
30           format('"* equations" string expected at line ',
     +       a,' of file ',a,'.')
             go to 9890
           end if
         end if
         call spacesub(cline)
         call linesplit(ifail,2,lw,rw,cline)
         if(ifail.ne.0) then
           call writint(aline,iline)
           write(errmsg,40) trim(aline),trim(afile)
40         format('Two entries expected at line ',a,' of file ',a,'.')
           go to 9890
         end if
         noutfile=noutfile+1
         if(noutfile.gt.MAXOUTFILE)then
           write(errmsg,50) trim(afile)
50         format('Too many instruction and model output files listed ',
     +     'in "* model output file" section of file ',a,'. Increase ',
     +     'MAXOUTFILE and re-compile program.')
           go to 9890
         end if
         insfle(noutfile)=cline(lw(1):rw(1))
         call remchar(insfle(noutfile),char(211))
         outfle(noutfile)=cline(lw(2):rw(2))
         call remchar(outfle(noutfile),char(211))
#ifndef UNIX
         call lowcase(insfle(noutfile))
         call lowcase(outfle(noutfile))
#endif
         if(noutfile.gt.1)then
           do j=1,noutfile-1
             if(insfle(j).eq.insfle(noutfile))then
               call addquote(insfle(noutfile),bfile)
               write(errmsg,49) trim(bfile),trim(afile)
49             format('The name of instruction file ',a,
     +         ' is repeated in OBS2OBS input file ',a,'.')
               go to 9890
             end if
           end do
         end if
       end do

C -- Observation names are read from model output files.

       nobs=0
       do i=1,noutfile
         call addquote(insfle(i),bfile)
         open(unit=11,file=insfle(i),status='old',iostat=ierr)
         if(ierr.ne.0)then
           write(errmsg,60) trim(bfile),trim(afile)
60         format('Cannot open instruction file ',a,' cited in ',
     +     'OBS2OBS input file ',a,'.')
           go to 9890
         end if
         read(11,'(a)') cline
         call lowcase(cline)
         if(cline(1:3).ne.'pif')then
           write(errmsg,611) trim(insfle(i))
611        format('First line of instruction file ',a,' should ',
     +     'be "pif" followed by marker delimiter.')
           go to 9890
         end if
         markdel=cline(5:5)
         if(markdel.eq.' ')then
           write(errmsg,622) trim(insfle(i))
622        format('Cannot read marker delimiter on first line ',
     +     'of instruction file ',a,'.')
           go to 9890
         end if
         jline=1
         do
           instruction=' '
           jline=jline+1
           read(11,'(a)',end=55) instruction
64         continue
           if(instruction.eq.' ') cycle
           call lowcase(instruction)
           nn1=index(instruction,markdel)
           if(nn1.ne.0)then
             nn2=index(instruction(nn1+1:),markdel)
             if(nn2.eq.0)then
               call writint(aline,jline)
               write(errmsg,63)trim(aline),trim(insfle(i))
63             format('Unbalanced marker delimiters at line ',
     +         a,' of instruction file ',a,'.')
               go to 9890
             end if
             instruction(nn1:nn1+nn2)=' '
             go to 64
           end if
           ii=0
31         continue
           nn1=index(instruction(ii+1:),'[')
           nn2=index(instruction(ii+1:),'(')
           nn3=index(instruction(ii+1:),'!')
           nn=max(nn1,nn2,nn3)
           if(nn.ne.0)then
             if(nn1.ne.0)then
               if(nn1.lt.nn) nn=nn1
             end if
             if(nn2.ne.0)then
               if(nn2.lt.nn) nn=nn2
             end if
             if(nn3.ne.0)then
               if(nn3.lt.nn) nn=nn3
             end if
             bb=instruction(ii+nn:ii+nn)
             if(bb.eq.'[')then
               bb=']'
             else if(bb.eq.'(')then
               bb=')'
             else if(bb.eq.'!')then
               bb='!'
             end if
             go to 51
           end if
           go to 62
51         continue
           ii=ii+nn
           nn=index(instruction(ii+1:),bb)
           if(nn.eq.0) go to 9050
           nobs=nobs+1
           if(nobs.gt.MAXOBS)then
             call writint(aline,MAXOBS)
             write(errmsg,61) trim(aline)
61           format('Too many observations cited in instruction files ',
     +       '- only ',a,' allowed. Increase MAXOBS and re-compile ',
     +       'program.')
             go to 9890
           end if
           if(nn.gt.21) go to 9050
           if(nn.eq.1) go to 9050
           do j=ii+1,ii+nn-1
             if(instruction(j:j).eq.' ') go to 9050
           end do
           aobs(nobs)=instruction(ii+1:ii+nn-1)
           if(aobs(nobs).eq.'dum')then
             nobs=nobs-1
           end if
           ii=ii+nn
           go to 31
62         continue
         end do
55       continue
         close(unit=11)
       end do
       if(nobs.eq.0)then
         write(errmsg,52) trim(afile)
52       format('No observations cited in instruction files cited ',
     +   'in "* model output" section of OBS2OBS input file ',a,'.')
         go to 9890
       end if

C -- The MIO module is now used to read observation values.

       precision='single'
       decpoint='point'
       numin=1
       npar=1
       instruction=' '
       call mio_initialise(ifail,numin,noutfile,npar,nobs,
     + precision,decpoint)
       if(ifail.ne.0) go to 9100
       do i=1,noutfile
         call mio_put_file(ifail,3,i,insfle(i))
         if(ifail.ne.0) go to 9100
         call mio_put_file(ifail,4,i,outfle(i))
         if(ifail.ne.0) go to 9100
       end do
       call mio_store_instruction_set(ifail)
       if(ifail.ne.0) go to 9100
       allocate(obsval(nobs),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,56)
56       format('Cannot allocate sufficient memory to continue ',
     +   'execution.')
         go to 9890
       end if
       call mio_read_model_output_files(ifail,nobs,aobs,obsval,
     + instruction)
       if(ifail.ne.0) go to 9100
       call mio_finalise(ifail)

C -- Equations are read from the next section of the OBS2OBS input file. But first the number
C    of equations is counted.

C -- In future versions of this software I would like to dispense with this and read the equations
C    one by one.

       neqn=0
       do
         iline=iline+1
         read(10,'(a)',end=9300) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#')cycle
         call lowcase(cline)
         if(cline(1:1).eq.'*')then
           if(cline(1:8).eq.'* output') then
             go to 584
           else
             call writint(aline,iline)
             write(errmsg,569) trim(aline),trim(afile)
569          format('String at line ',a,' of file ',a,' should ',
     +       'be "* output".')
             go to 9890
           end if
         end if
         neqn=neqn+1
         nn=index(cline,'=')
         if((nn.eq.0).or.(nn.eq.1)) go to 9400
         if(cline(1:nn-1).eq.' ') go to 9400
         if(cline(nn+1:).eq.' ') go to 9400
         atemp=cline(1:nn-1)
         nb=len_trim(atemp)
         if(nb.gt.20)then
           call writint(aline,iline)
           write(errmsg,570) trim(aline),trim(afile)
570        format('Variable at left of "=" sign at line ',a,' of file ',
     +     a,' must be 20 characters or less in length.')
           go to 9890
         end if
         if(index(atemp(1:nb),' ').ne.0)then
           call writint(aline,iline)
           write(errmsg,580) trim(aline),trim(afile)
580        format('Variable name at left of "=" sign at line ',a,' of ',
     +     'file ',a,' must not contain a blank.')
           go to 9890
         end if
         do i=1,nobs
           if(atemp.eq.aobs(i))then
             call writint(aline,iline)
             write(errmsg,585)trim(aline),trim(afile)
585          format('Variable name at left of "=" sign at line ',a,
     +       ' of file ',a,' has same name as an observation ',
     +       'cited in an instruction file. This is not allowed.')
             go to 9890
           end if
         end do
       end do
584    continue
       if(neqn.eq.0)then
         write(errmsg,520) trim(afile)
520      format('No equations found in "* equations" section of ',
     +   'file ',a,'.')
         go to 9890
       end if

C -- The file is now re-wound to the start of the equations section.

       rewind(unit=10)
       iline=0
       do
         iline=iline+1
         read(10,'(a)')cline
         if(index(cline,'* equations').ne.0) go to 530
       end do
530    continue

C -- The equations block is used now.

C -- Note - in future versions, reuse the same equation.

       allocate(totobsval(nobs+neqn),totobsname(nobs+neqn),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,540)
540      format('Too many equations. Cannot allocate sufficient ',
     +   'memory.')
         go to 9890
       end if
       do i=1,nobs
         totobsval(i)=obsval(i)
         totobsname(i)=aobs(i)
       end do
       deallocate(obsval,stat=ierr)
       call eqn_ini(ifail,neqn)
       if(ifail.ne.0) go to 9200
       ieqn=0
       do
         iline=iline+1
         read(10,'(a)') cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#')cycle
         call lowcase(cline)
         if(cline.eq.'* output') exit
         ieqn=ieqn+1
         nn=index(cline,'=')
         atemp=cline(1:nn-1)
         cline=cline(nn+1:)
         cline=adjustl(cline)
         call eqn_install(ifail,ieqn,atemp,cline)
         if(ifail.ne.0) go to 9200
         call eqn_evaluate(ifail,ieqn,nobs+ieqn-1,totobsname,totobsval,
     +   itype,rval,lval)
         if(ifail.ne.0) go to 9200
         if(itype.eq.1)then
           call writint(aline,iline)
           write(errmsg,545) trim(aline),trim(afile)
545        format('Error evaluating equation at line ',a,' of file ',
     +     a,': the equation has a logical equation outcome when a ',
     +     'real outcome is expected.')
           go to 9890
         end if
         totobsname(ieqn+nobs)=atemp
         totobsval(ieqn+nobs)=rval
       end do

C -- The output file is now written.

       open(unit=20,file=outfile)
       if(insfile.ne.' ')then
         open(unit=21,file=insfile)
         write(21,600)
600      format('pif $')
       end if
       nn=0
       do
         iline=iline+1
         read(10,'(a)',end=800) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         call linesplit(ifail,1,lw,rw,cline)
         atemp=cline(lw(1):rw(1))
         call lowcase(atemp)
         do i=1,nobs+neqn
           if(atemp.eq.totobsname(i)) go to 620
         end do
         call writint(aline,iline)
         write(errmsg,610) trim(aline),trim(afile)
610      format('Entity named at line ',a,' of file ',a,
     +   ' is not an observation read from a model output file, nor ',
     +   'a variable whose value is assigned through equation ',
     +   'evaluation.')
         go to 9890
620      write(20,621) trim(totobsname(i)),totobsval(i)
621      format(1x,a,t25,1pg23.16)
         if(insfile.ne.' ')write(21,630) trim(totobsname(i))
630      format('l1 [',a,']24:48')
         nn=nn+1
       end do
800    continue

       if(nn.eq.0)then
         write(errmsg,632) trim(afile)
632      format('No variables are listed in the "* output" section ',
     +   'of file ',a,'.')
         go to 9890
       end if

C -- Finishing up.

       close(unit=10)
       write(6,*)
       write(6,810) trim(afile)
810    format(' - file ',a,' read ok.')
       close(unit=20)
       call addquote(outfile,afile)
       write(6,900) trim(afile)
900    format(' - file ',a,' written ok.')
       if(insfile.ne.' ')then
         call addquote(insfile,afile)
         write(6,900)trim(afile)
         close(unit=21)
       endif
       go to 9900

9000   continue
9010   write(errmsg,9020) trim(infile)
9020   format('Unexpected end encountered to OBS2OBS input file ',a,'.')
       go to 9890

9050   continue
       call writint(aline,jline)
       write(errmsg,9060) trim(aline),trim(bfile),trim(afile)
9060   format('Cannot read observation name from line ',a,' of ',
     + 'instruction file ',a' cited in OBS2OBS input file ',a,
     + '. Check this file with INSCHEK.')
       go to 9890

9100   continue
       call mio_get_message_string(ifail,errmsg)
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
       if(instruction.ne.' ')then
         write(6,9110)
9110     format(' Instruction follows:-')
         write(6,9120) trim(instruction)
9120     format(1x,a)
       end if
       go to 9900

9150   continue
       write(errmsg,9160) trim(afile)
9160   format('Unexpected end encountered to OBS2OBS input file ',
     + a,' while looking for "* equations" string.')
       go to 9890

9200   continue
       call eqn_get_message_string(ifail,errmsg)
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
       go to 9900

9300   write(errmsg,9310) trim(infile)
9310   format('Cannot find "* output" section in MOD2OBS input ',
     + 'file ',a,'.')
       go to 9890

9400   continue
       call writint(aline,iline)
       write(errmsg,9410) trim(aline),trim(afile)
9410   format('Error at line ',a,' of MOD2OBS input file ',a,
     + ': line must start with variable name followed by "=" symbol.')
       go to 9890

9800   write(6,9810)
9810   format(/,' OBS2OBS is run using the command:',/)
       write(6,9820)
9820   format('     obs2obs infile outfile [insfile]',/)
       write(6,9830)
9830   format(' where',/)
       write(6,9850)
9850   format('     infile  is the OBS2OBS input file')
       write(6,9860)
9860   format('     outfile is the OBS2OBS output file, and')
       write(6,9870)
9870   format('     insfile is an optional output instruction file.')

       go to 9999

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
       go to 9900


9900   continue
       call mio_finalise(ifail)
       call eqn_cln()
       inquire(unit=20,opened=lopened)
       if(lopened)then
         close(unit=20,status='delete')
       end if
       inquire(unit=21,opened=lopened)
       if(lopened)then
         close(unit=21,status='delete')
       end if
       if(allocated(obsval)) deallocate(obsval)
       if(allocated(totobsval)) deallocate(totobsval)
       if(allocated(totobsname)) deallocate(totobsname)

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


        subroutine linesplit(ifail,num,lw,rw,cline)

c -- Subroutine LINESPLIT splits a line into whitespace-separated substrings.

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

