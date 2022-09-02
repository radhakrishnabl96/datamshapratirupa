       program subreg1

C -- Program SUBREG1 removes regularization from a PEST control file.

       implicit none

       logical   :: skipline
       integer   :: ifail,ierr
       integer   :: npar,nobs,npargp,nprior,nobsgp
       integer   :: lw(10),rw(10)
       integer   :: nb,iline,i,iprior,n,j
       integer   :: nobsgp_regul,nobs_regul,nprior_regul

       character*4    :: aext
       character*10   :: atemp1
       character*12   :: aversion
       character*12   :: aobsgp,aline
       character*200  :: pestinfile,pestoutfile,apestinfile,apestoutfile
       character*500  :: amessage
       character*1000 :: cline,priorline,dline

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' SUBREG1 version ',a,'. Watermark Numerical ',
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
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       pestinfile=cline(lw(1):rw(1))
       call remchar(pestinfile,char(211))
       pestoutfile=cline(lw(2):rw(2))
       call remchar(pestoutfile,char(211))
#ifndef UNIX
       call lowcas(pestinfile)
       call lowcas(pestoutfile)
#endif

C -- Further analysis is done on the input and output PEST filenames.

       nb=len_trim(pestinfile)
       nb=nb-3
       if(nb.lt.1)nb=1
       aext=pestinfile(nb:)
       call lowcas(aext)
       if(aext.eq.'.pst')then
         if(nb.eq.1) go to 8900
       else
         pestinfile=trim(pestinfile)//'.pst'
       end if

       nb=len_trim(pestoutfile)
       nb=nb-3
       if(nb.lt.1)nb=1
       aext=pestoutfile(nb:)
       call lowcas(aext)
       if(aext.ne.'.pst')then
         pestoutfile=trim(pestoutfile)//'.pst'
       end if

C -- Quotes are added to filenames if necessary.

       call addquote(pestinfile,apestinfile)
       call addquote(pestoutfile,apestoutfile)

C -- The old and new PEST control files are opened.

       open(unit=10,file=pestinfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,30) trim(apestinfile)
30       format(' Cannot open PEST control file ',a,'.')
         go to 9890
       end if

       open(unit=20,file=pestoutfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,40) trim(apestoutfile)
40       format(' Cannot open file ',a,' for output.')
         go to 9890
       end if

C -- The existing PEST control file is opened and partly read.

       read(10,*,err=9000,end=9000) atemp1
       call lowcas(atemp1)
       if(atemp1.ne.'pcf') go to 9000
       iline=1
161    continue
       iline=iline+1
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 161
159    continue
       iline=iline+1
       read(10,'(a)',err=9000,end=9000)cline
       if(skipline(cline)) go to 159
       call lowcas(cline)
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0)then
         call writint(aline,iline)
         write(amessage,162) trim(aline),trim(apestinfile)
162      format(' Cannot read PESTMODE variable on line ',a,
     +   ' of file ',a,'.')
         go to 9890
       end if
       atemp1=cline(lw(2):rw(2))
       if(atemp1(1:5).ne.'regul')then
         write(amessage,163) trim(apestinfile)
163      format(' PESTMODE variable in file ',a,' must be ',
     +   'set to "regularisation".')
         go to 9890
       end if
164    continue
       iline=iline+1
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 164
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.ne.0)then
         call writint(aline,iline)
         write(amessage,165) trim(aline),trim(apestinfile)
165      format(' Cannot read number of observation groups NOBSGP ',
     +   'from line ',a,' of file ',a,'.')
         go to 9890
       end if
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
       if(nobsgp.eq.0)then
         write(amessage,169) trim(apestinfile)
169      format(' Number of observation groups NOBSGP supplied as ',
     +   'zero in PEST control file ',a,'.')
         go to 9890
       end if

C -- The number of observation groups that are regularisation groups are now read.

       do
         iline=iline+1
         read(10,'(a)',err=9000,end=9320) cline
         cline=adjustl(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'observation gr').ne.0) go to 210
         end if
       end do
210    continue
       nobsgp_regul=0
       do i=1,nobsgp
211      continue
         iline=iline+1
         read(10,'(a)',err=9000,end=9000) cline
         if(skipline(cline)) go to 211
         call linspl(ifail,1,lw,rw,cline)
         aobsgp=cline(lw(1):rw(1))
         call lowcas(aobsgp)
         if(aobsgp(1:5).eq.'regul') nobsgp_regul=nobsgp_regul+1
       end do

C -- The number of observations that belong to regularization groups is now obtained.

       do
         iline=iline+1
         read(10,'(a)',err=9000,end=9350) cline
         cline=adjustl(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'observation da').ne.0) go to 220
         end if
       end do
220    continue
       nobs_regul=0
       if(nobs.ne.0)then
         do i=1,nobs
230        iline=iline+1
           read(10,'(a)',err=9000,end=9000) cline
           if(skipline(cline)) go to 230
           call linspl(ifail,4,lw,rw,cline)
           if(ifail.ne.0) go to 9300
           aobsgp=cline(lw(4):rw(4))
           call lowcas(aobsgp)
           if(aobsgp(1:5).eq.'regul') nobs_regul=nobs_regul+1
         end do
       end if

C -- The number of prior information equations that belong to regularization groups is now obtained.

       if(nprior.eq.0)then
         nprior_regul=0
       else
         do
           iline=iline+1
           read(10,'(a)',err=9000,end=9400) cline
           cline=adjustl(cline)
           if(cline(1:1).eq.'*')then
             call lowcas(cline)
             if(index(cline,'prior info').ne.0) go to 250
           end if
         end do
250      continue
         iprior=0
251      continue
         iline=iline+1
         read(10,'(a)',err=9000,end=9000) priorline
         if(skipline(priorline)) go to 251
         do
260        continue
           iline=iline+1
           read(10,'(a)',err=9000,end=300) cline
           if(skipline(cline)) go to 260
           cline=adjustl(cline)
           if(cline(1:1).eq.'*') go to 300
           if(cline(1:1).eq.'&')then
             priorline=trim(priorline)//' '//cline(2:)
             go to 260
           end if
           iprior=iprior+1
           n=len_trim(priorline)
           if(n.le.2) then
             iline=iline-1
             go to 9500
           end if
           do j=n-1,1,-1
             if(priorline(j:j).eq.' ') go to 270
           end do
           iline=iline-1
           go to 9500
270        continue
           aobsgp=priorline(j+1:n)
           call lowcas(aobsgp)
           if(aobsgp(1:5).eq.'regul') nprior_regul=nprior_regul+1
           priorline=cline
         end do
300      continue
         iprior=iprior+1
         n=len_trim(priorline)
         if(n.le.2) then
           iline=iline-1
           go to 9500
         end if
         do j=n-1,1,-1
           if(priorline(j:j).eq.' ') go to 271
         end do
         iline=iline-1
         go to 9500
271      continue
         aobsgp=priorline(j+1:n)
         call lowcas(aobsgp)
         if(aobsgp(1:5).eq.'regul') nprior_regul=nprior_regul+1
         if(iprior.ne.nprior)then
           write(amessage,280) trim(apestinfile)
280        format('Insufficient prior information equations in ',
     +     'file ',a,'.')
           go to 9890
         end if
       end if

C -- The original PEST control file is re-wound and information transferred to the
C    new PEST control file.

       rewind(unit=10)
       read(10,*)
       write(20,650)
650    format('pcf')
651    continue
       read(10,'(a)') cline
       write(20,'(a)') trim(cline)
       if(skipline(cline)) go to 651
152    continue
       read(10,'(a)') cline
       dline=cline
       if(skipline(cline)) then
         write(20,'(a)') trim(dline)
         go to 152
       end if
       call linspl(ifail,2,lw,rw,dline)
       if(ifail.ne.0) go to 9000
       if(dline(rw(2)+1:).eq.' ')then
         write(20,660)
660      format('restart estimation')
       else
         write(20,661) trim(dline(rw(2)+1:))
661      format('restart estimation ',a)
       end if
662    continue
       read(10,'(a)') cline
       dline=cline
       if(skipline(cline))then
         write(20,'(a)') trim(dline)
         go to 662
       end if
       call linspl(ifail,5,lw,rw,dline)
       if(dline(rw(5)+1:).eq.' ')then
         write(20,670) npar,nobs-nobs_regul,npargp,
     +   nprior-nprior_regul,nobsgp-nobsgp_regul
670      format(5i8)
       else
         write(20,671) npar,nobs-nobs_regul,npargp,
     +   nprior-nprior_regul,nobsgp-nobsgp_regul,
     +   trim(dline(rw(5)+1:))
671      format(5i8,1x,a)
       end if
       do
         read(10,'(a)') cline
         write(20,'(a)') trim(cline)
         cline=adjustl(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'observation gr').ne.0) go to 700
         end if
       end do
700    continue
       do i=1,nobsgp
711      continue
         read(10,'(a)',err=9000,end=9000) cline
         dline=cline
         if(skipline(cline))then
           write(20,'(a)') trim(dline)
           go to 711
         end if
         call linspl(ifail,1,lw,rw,dline)
         aobsgp=dline(lw(1):rw(1))
         call lowcas(aobsgp)
         if(aobsgp(1:5).eq.'regul') then
           continue
         else
           write(20,'(a)') trim(dline)
         end if
       end do

       do
         read(10,'(a)',err=9000,end=9000) cline
         write(20,'(a)') trim(cline)
         cline=adjustl(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'observation da').ne.0) go to 720
         end if
       end do
720    continue
       if(nobs.ne.0)then
         if(nobs_regul.ne.0)then
           do i=1,nobs
730          continue
             read(10,'(a)',err=9000,end=9000) cline
             dline=cline
             if(skipline(cline))then
               write(20,'(a)') trim(dline)
               go to 730
             end if
             call linspl(ifail,4,lw,rw,dline)
             aobsgp=dline(lw(4):rw(4))
             call lowcas(aobsgp)
             if(aobsgp(1:5).eq.'regul') then
               continue
             else
               write(20,'(a)') trim(dline)
             end if
           end do
         else
           do i=1,nobs
735          continue
             read(10,'(a)',err=9000,end=9000) cline
             dline=cline
             if(skipline(cline))then
               write(20,'(a)') trim(dline)
               go to 735
             end if
             write(20,'(a)') trim(dline)
           end do
         end if
       end if

       if((nprior.eq.0).or.(nprior_regul.eq.nprior))then
         do
           iline=iline+1
           read(10,'(a)',end=500) cline
           cline=adjustl(cline)
           if(cline(1:1).eq.'*')then
             call lowcas(cline)
             if(index(cline,'regul').ne.0) go to 500
             if(index(cline,'predict').ne.0) go to 500
             if(index(cline,'pareto').ne.0) go to 500
             if(index(cline,'prior').ne.0) go to 500
             if(index(cline,'distribut').ne.0) go to 500
           end if
           write(20,'(a)') trim(cline)
         end do
       else
         do
           iline=iline+1
           read(10,'(a)',err=9000,end=9400) cline
           write(20,'(a)') trim(cline)
           cline=adjustl(cline)
           if(cline(1:1).eq.'*')then
             call lowcas(cline)
             if(index(cline,'prior info').ne.0) go to 750
           end if
         end do
750      continue
         iline=iline+1
         read(10,'(a)',err=9000,end=9000) priorline
         dline=priorline
         if(skipline(priorline)) then
           write(20,'(a)') trim(dline)
           go to 750
         end if
         do
760        continue
           iline=iline+1
           read(10,'(a)',err=9000,end=800) cline
           dline=cline
           if(skipline(cline)) then
             write(20,'(a)') trim(dline)
             go to 760
           end if
           cline=adjustl(cline)
           if(cline(1:1).eq.'* ') go to 800
           if(cline(1:1).eq.'&')then
             priorline=trim(priorline)//' '//cline(2:)
             go to 760
           end if
           n=len_trim(priorline)
           do j=n-1,1,-1
             if(priorline(j:j).eq.' ') go to 770
           end do
           iline=iline-1
           go to 9500
770        continue
           aobsgp=priorline(j+1:n)
           call lowcas(aobsgp)
           if(aobsgp(1:5).eq.'regul') then
             continue
           else
             write(20,'(a)') trim(priorline)
           end if
           priorline=cline
         end do
800      continue
         n=len_trim(priorline)
         do j=n-1,1,-1
           if(priorline(j:j).eq.' ') go to 820
         end do
         iline=iline-1
         go to 9500
820      continue
         aobsgp=priorline(j+1:n)
         call lowcas(aobsgp)
         if(aobsgp(1:5).eq.'regul') then
           continue
         else
           write(20,'(a)') trim(priorline)
         end if
       end if

500    continue
       call lowcas(cline)
       if((cline(1:1).eq.'*').and.(index(cline,'distribut').ne.0))then
         write(20,'(a)') trim(cline)
         do
           read(10,'(a)',end=505) cline
           write(20,'(a)') trim(cline)
         end do
505      continue
       else
         do
           read(10,'(a)',end=509) cline
           dline=cline
           if(skipline(cline)) write(20,'(a)') trim(dline)
           cline=adjustl(cline)
           if(cline(1:1).eq.'*')then
             call lowcas(cline)
             if(index(cline,'distribut').ne.0)then
               write(20,'(a)') trim(cline)
               do
                 read(10,'(a)',end=509) cline
                 write(20,'(a)') trim(cline)
              end do
             end if
           end if
         end do
509      continue
       end if
       close(unit=10)
       write(6,*)
       write(6,510) trim(apestinfile)
510    format(' - file ',a,' read ok.')
       close(unit=20)
       write(6,520) trim(apestoutfile)
520    format(' - file ',a,' written ok.')

       if(nobs_regul.ne.0)then
         write(6,549)
549      format(/,' Warning:',/)
         write(6,550)
550      format(' Some observations belong to regularisation ',
     +   'groups. These have been')
         write(6,551)
551      format(' removed from the "observation data" section ',
     +   'of the PEST control file.')
         write(6,552)
552      format(/,' The instruction set must be ammended to remove ',
     +   'these observations.')
       end if

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' SUBREG1 is run using the command:',/)
       write(6,8920)
8920   format('     subreg1 pestinfile pestoutfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     pestinfile  is an existing ',
     + 'PEST control file, and')
       write(6,8950)
8950   format('     pestoutfile is a new PEST control file.')

       go to 9900


9000   write(amessage,9010) trim(apestinfile)
9010   format(' Error encountered when reading PEST control file ',
     + a,' - check this file with PESTCHEK.')
       go to 9890
9300   call writint(aline,iline)
       write(amessage,9310) trim(aline),trim(apestinfile)
9310   format(' Insufficient items on line ',a,' of PEST control ',
     + 'file ',a,'.')
       go to 9890
9320   write(amessage,9330) trim(apestinfile)
9330   format(' Cannot find "observation groups" section in ',
     + 'file ',a,'.')
       go to 9890
9350   write(amessage,9360) trim(apestinfile)
9360   format(' Cannot find "observation data" section in file ',a,'.')
       go to 9890
9400   write(amessage,9410) trim(apestinfile)
9410   format(' Cannot find "prior information" section in file ',
     + a,'.')
       go to 9890
9500   call writint(aline,iline)
       write(amessage,9510) trim(aline),trim(apestinfile)
9510   format(' Cannot read prior information group from ',
     + 'line ',a,' of file ',a,'.')
       go to 9890


9890   continue
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
