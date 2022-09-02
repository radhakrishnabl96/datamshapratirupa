       program tpl2pst

! -- Program TPL2PST builds a partial PEST control file from a series
!    of template files.

       implicit none

       integer, parameter  :: LEN_CLINE=3000
       integer, parameter  :: MAXTPLFLE=20
       integer, parameter  :: MAXPAR=10000
       integer             :: ifail,ierr,iline,nn,i,jj
       integer             :: ibeg,iend
       integer             :: npar,npargp,nobs,nprior,nobsgp,ipar
       integer             :: ntplfle,itplfle
       integer             :: inblock
       integer             :: fileflag,mfileflag,transflag,valflag,
     +                        lbflag,ubflag,gpflag
       integer             :: nparstart,lastnpar,firstnpar
       integer             :: lw(10),rw(10)
       integer             :: itrans(MAXTPLFLE),nparfile(MAXTPLFLE)

       double precision    :: dtemp
       double precision    :: pval(MAXTPLFLE),lbound(MAXTPLFLE),
     +                        ubound(MAXTPLFLE)

       character (len=1)         :: pardelim
       character (len=12)        :: aversion
       character (len=12)        :: aaparlast,aapar
       character (len=12)        :: pargroup(MAXTPLFLE),apargp
       character (len=12)        :: apar(MAXPAR)
       character (len=15)        :: anum,aline,aparchglim
       character (len=20)        :: atemp20,keyword
       character (len=25)        :: aword,bword
       character (len=256)       :: infile,pestfile,ainfile,apestfile
       character (len=256)       :: afile,bfile
       character (len=256)       :: tplfle(MAXTPLFLE),mfile(MAXTPLFLE)
       character (len=1500)      :: amessage
       character (len=LEN_CLINE) :: cline

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' TPL2PST Version ',a,
     + '. Watermark Numerical Computing.')

#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) cline
#else
       call pgetcl(cline)
#endif

C -- The TPL2PST command line is read.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       infile=cline(lw(1):rw(1))
       call remchar(infile,char(211))
       pestfile=cline(lw(2):rw(2))
       call remchar(pestfile,char(211))
       call addquote(infile,ainfile)
       call addquote(pestfile,apestfile)

! -- Initialization

       npar=0
       npargp=0
       nobs=0
       nprior=0
       nobsgp=0
       ntplfle=0

! -- We open the input file.

       open(unit=10,file=infile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,110) trim(ainfile)
110      format('Cannot open file ',a,'.')
         go to 9890
       end if

! -- We now read the input file.

       iline=0
       inblock=0
       do
         iline=iline+1
         read(10,'(a)',end=300) cline
         call stripline(cline)
         if(cline.eq.' ') cycle
         call linspl(ifail,1,lw,rw,cline)
         aword=cline(lw(1):rw(1))
         call lowcas(aword)
         if(inblock.eq.0)then
           if(aword.eq.'end')then
             call writint(aline,iline)
             write(amessage,125) trim(aline),trim(ainfile)
125          format('Misplaced END keyword encountered at line ',
     +       a,' of file ',a,'.')
             go to 9890
           end if
           if((aword.ne.'start').and.(aword.ne.'begin'))then
             call writint(aline,iline)
             write(amessage,120) trim(aline),trim(ainfile)
120          format('Line ',a,' of file ',a,' should begin with ',
     +       'START or BEGIN.')
             go to 9890
           end if
           fileflag=0
           mfileflag=0
           transflag=0
           valflag=0
           lbflag=0
           ubflag=0
           gpflag=0
           ntplfle=ntplfle+1
           if(ntplfle.gt.MAXTPLFLE)THEN
             call writint(anum,MAXTPLFLE)
             write(amessage,121) trim(ainfile),trim(anum)
121          format('Too many template files are cited in file ',
     +       a,'. Increase MAXTPLFLE above ',a,' and ',
     +       're-compile program.')
             go to 9890
           end if
           inblock=1
         else if(inblock.ne.0)then
           call writint(aline,iline)
           if((aword.eq.'start').or.(aword.eq.'begin'))then
             write(amessage,130) trim(aline),trim(ainfile)
130          format('A block begins within a block at line ',a,
     +       ' of file ',a,'.')
             go to 9890
           end if
           nn=index(cline,'=')
           if(nn.ne.0) cline(nn:nn)=' '
           call linspl(ifail,2,lw,rw,cline)
           if(ifail.ne.0) go to 9030
           aword=cline(lw(1):rw(1))
           call lowcas(aword)
           bword=cline(lw(2):rw(2))
           call lowcas(bword)
           if((aword.eq.'template_file').or.(aword.eq.'file'))then
             if(fileflag.eq.1)then
               keyword='TEMPLATE_FILE'
               go to 9050
             end if
             fileflag=1
             afile=adjustl(cline(rw(1)+1:))
             ibeg=1
             iend=len(afile)
             call getfile(ifail,afile,tplfle(ntplfle),ibeg,iend)
             if(ifail.ne.0) go to 9000
#ifndef UNIX
             call lowcas(tplfle(ntplfle))
#endif
           else if(aword.eq.'transform')then
             if(transflag.eq.1)then
               keyword='TRANSFORM'
               go to 9050
             end if
             transflag=1
             afile=adjustl(cline(rw(1)+1:))
             ibeg=1
             iend=len(afile)
             call getfile(ifail,afile,bword,ibeg,iend)
             if(ifail.ne.0) then
               write(amessage,131) trim(aline),trim(ainfile)
131            format('Cannot read text following TRANSFORM keyword ',
     +         'at line ',a,' of file ',a,'.')
               go to 9890
             end if
             if(bword.eq.'log')then
               itrans(ntplfle)=1
             else if(bword.eq.'none')then
               itrans(ntplfle)=0
             else if(bword.eq.'fixed')then
               itrans(ntplfle)=-1000000
             else
               write(amessage,150) trim(aline),trim(ainfile)
150            format('Text following the TRANSFORM keyword must be ',
     +         '"log", "none" or "fixed" at line ',a,' of file ',
     +         a,'.')
               go to 9890
             end if
           else if(aword.eq.'value')then
             if(valflag.eq.1)then
               keyword='VALUE'
               go to 9050
             end if
             valflag=1
             call drealrd(ifail,bword,pval(ntplfle))
             if(ifail.ne.0) then
               keyword='VALUE'
               go to 9100
             end if
           else if(aword.eq.'lower_bound')then
             if(lbflag.eq.1)then
               keyword='LOWER_BOUND'
               go to 9050
             end if
             lbflag=1
             call drealrd(ifail,bword,lbound(ntplfle))
             if(ifail.ne.0) then
               keyword='LOWER_BOUND'
               go to 9100
             end if
           else if(aword.eq.'upper_bound')then
             if(ubflag.eq.1)then
               keyword='UPPER_BOUND'
               go to 9050
             end if
             ubflag=1
             call drealrd(ifail,bword,ubound(ntplfle))
             if(ifail.ne.0) then
               keyword='UPPER_BOUND'
               go to 9100
             end if
           else if(aword.eq.'pargroup')then
             afile=adjustl(cline(rw(1)+1:))
             ibeg=1
             iend=len(afile)
             call getfile(ifail,afile,atemp20,ibeg,iend)
             if(ifail.ne.0)then
               write(amessage,160) trim(aline),trim(ainfile)
160            format('Cannot read parameter group name at line ',a,
     +         ' of file ',a,'.')
               go to 9890
             end if
             nn=len_trim(atemp20)
             if(nn.gt.12)then
               write(amessage,170) trim(atemp20),trim(aline),
     +         trim(ainfile)
170            format('Parameter group name "',a,'" greater than 12 ',
     +         'characters in length at line ',a,' of file ',a,'.')
               go to 9890
             end if
             do i=1,nn
               if((atemp20(i:i).eq.' ').or.
     +            (atemp20(i:i).eq.char(9)))then
                 write(amessage,190) trim(atemp20),trim(aline),
     +           trim(ainfile)
190              format('Parameter group name "',a,
     +           '" contains a space at line ',a,' of file ',a,'.')
                 go to 9890
               end if
             end do
             call lowcas(atemp20)
             pargroup(ntplfle)=atemp20
             gpflag=1
           else if((aword.eq.'model_file').or.
     +             (aword.eq.'model_input_file'))then
             if(mfileflag.eq.1)then
               keyword='MODEL_INPUT_FILE'
               go to 9050
             end if
             mfileflag=1
             afile=adjustl(cline(rw(1)+1:))
             ibeg=1
             iend=len(afile)
             call getfile(ifail,afile,mfile(ntplfle),ibeg,iend)
             if(ifail.ne.0) go to 9000
#ifndef UNIX
             call lowcas(mfile(ntplfle))
#endif
           else if(aword.eq.'end')then
             if(fileflag.eq.0)then
               keyword='TEMPLATE_FILE'
               go to 9120
             else if(transflag.eq.0)then
               keyword='TRANSFORM'
               go to 9120
             else if(valflag.eq.0)then
               keyword='VALUE'
               go to 9120
             else if(lbflag.eq.0)then
               keyword='LOWER_BOUND'
               go to 9120
             else if(ubflag.eq.0)then
               keyword='UPPER_BOUND'
               go to 9120
             else if(gpflag.eq.0)then
               keyword='PARGROUP'
               go to 9120
             else if(mfileflag.eq.0)then
               keyword='MODEL_INPUT_FILE'
               go to 9120
             end if
             dtemp=pval(ntplfle)
             if(itrans(ntplfle).eq.1)then
               if(dtemp.le.0.0d0)then
                 write(amessage,200) trim(aline),trim(ainfile)
200              format('Parameter value is zero or negative but ',
     +           'transform is "log" for block that finishes at ',
     +           'line ',a,' of file ',a,'.')
                 go to 9890
               end if
             end if
             if(ubound(ntplfle).le.lbound(ntplfle))then
               write(amessage,205) trim(aline),trim(ainfile)
205            format('Parameter upper bound is not greater than ',
     +         'parameter lower bound for block that ends on ',
     +         'line ',a,' of file ',a,'.')
               go to 9890
             end if
             if(dtemp.gt.ubound(ntplfle))then
               write(amessage,210) trim(aline),trim(ainfile)
210            format('Parameter value exceeds upper bound for ',
     +         'block that finishes at line ',a,' of file ',a,'.')
               go to 9890
             end if
             if(dtemp.lt.lbound(ntplfle))then
               write(amessage,220) trim(aline),trim(ainfile)
220            format('Parameter value is less than lower bound ',
     +         'for block that finishes at line ',a,' of file ',a,'.')
               go to 9890
             end if
             inblock=0
           else
             call upcas(aword)
             write(amessage,161) trim(aword),trim(aline),trim(ainfile)
161          format('Unrecognized keyword "',a,'" at line ',a,' of ',
     +       'file ',a,'.')
             go to 9890
           end if
         end if
       end do

! -- We check some things.

300    continue
       if(inblock.eq.1)then
         write(amessage,310) trim(ainfile)
310      format('End of file ',a,' found before block is ',
     +   'closed with "END" keyword.')
         go to 9890
       end if
       if (ntplfle.eq.0)then
         write(amessage,320) trim(ainfile)
320      format('No data blocks found in file ',a,'.')
         go to 9890
       end if
       close(unit=10)
       write(6,330) trim(ainfile)
330    format(' - file ',a,' read ok.')

! -- We check that there are no repeated template file names.

       if(ntplfle.gt.1)then
         do itplfle=2,ntplfle
           afile=tplfle(itplfle)
           do i=1,itplfle-1
             if(tplfle(i).eq.afile)then
               write(amessage,340) trim(afile),trim(ainfile)
340            format('More than one block cites template file ',a,
     +         ' in TPL2PST input file ',a,'.')
               go to 9890
             end if
           end do
         end do
       end if
       if(ntplfle.gt.1)then
         do itplfle=2,ntplfle
           afile=mfile(itplfle)
           do i=1,itplfle-1
             if(mfile(i).eq.afile)then
               write(amessage,341) trim(afile),trim(ainfile)
341            format('More than one block cites model input file ',a,
     +         ' in TPL2PST input file ',a,'.')
               go to 9890
             end if
           end do
         end do
       end if

! -- We now read the template files and find the number of parameters.

       npar=0
       do itplfle=1,ntplfle
         aaparlast=' '
         nparstart=npar
         call addquote(tplfle(itplfle),afile)
         write(6,350) trim(afile)
350      format(/,' - reading file ',a,'...')
         open(unit=10,file=tplfle(itplfle),status='old',iostat=ierr)
         if(ierr.ne.0)then
           write(amessage,360) trim(afile)
360        format('Cannot open template file ',a,'.')
           go to 9890
         end if
         nparfile(itplfle)=0
         iline=1
         read(10,'(a)',end=9150) cline
         cline=adjustl(cline)
         call lowcas(cline)
         if(cline(1:4).ne.'ptf ') go to 9150
         if(cline(5:5).eq.' ') go to 9150
         pardelim=cline(5:5)
         do
           iline=iline+1
           read(10,'(a)',end=600) cline
           if(cline(LEN_CLINE-10:LEN_CLINE).ne.' ')then
             call writint(anum,LEN_CLINE)
             write(amessage,371) trim(afile),trim(anum)
371          format('File 'a,' is too wide. Increase LEN_CLINE ',
     +       'above ',a,' and re-compile this program.')
             go to 9890
           end if
370        continue
           nn=index(cline,pardelim)
           if(nn.eq.0) cycle
           call lowcas(cline)
           cline=cline(nn+1:)
           nn=index(cline,pardelim)
           if(nn.eq.0)then
             call writint(aline,iline)
             write(amessage,380) trim(aline),trim(afile)
380          format('Unbalanced parameter delimiters at line ',a,
     +       ' of file ',a,'.')
             go to 9890
           end if
           atemp20=adjustl(cline(1:nn-1))
           if(atemp20.eq.' ')then
             call writint(aline,iline)
             write(amessage,385) trim(aline),trim(afile)
385          format('A space between parameter delimiters ',
     +       'contains no parameter name on line ',a,' of file ',a,'.')
             go to 9890
           end if
           jj=len_trim(atemp20)
           if(jj.gt.12)then
             call writint(aline,iline)
             write(amessage,390) trim(atemp20),trim(aline),trim(afile)
390          format('Parameter name "',a,'" greater than 12 ',
     +       'characters in length at line ',a,' of file ',a,'.')
             go to 9890
           end if
           do i=1,jj
             if((atemp20(i:i).eq.' ').or.(atemp20(i:i).eq.char(9)))then
               call writint(aline,iline)
               write(amessage,490) trim(atemp20),trim(aline),
     +         trim(afile)
490            format('Parameter name "',a,'" contains a space at ',
     +         'line ',a,' of file ',a,'.')
               go to 9890
             end if
           end do
           aaparlast=aapar
           aapar=atemp20
           if(aapar.ne.aaparlast)then
             if(nparstart.ne.0)then
               do ipar=1,nparstart
                 if(aapar.eq.apar(ipar))then
                   write(amessage,500) trim(aapar),trim(afile)
500                format('Parameter name "',a,'" featured in file ',a,
     +             ' is featured in at least one other template file.')
                   go to 9890
                 end if
               end do
             end if
           end if
           if(nparfile(itplfle).ge.1)then
             do ipar=nparstart+1,npar
               if(aapar.eq.apar(ipar)) go to 520
             end do
           end if
           npar=npar+1
           if(npar.gt.MAXPAR)then
             call writint(anum,MAXPAR)
             write(amessage,510) trim(anum)
510          format('Too many parameters cited in template files. ',
     +       'Increase MAXPAR above ',a,' and re-compile program.')
             go to 9890
           end if
           nparfile(itplfle)=nparfile(itplfle)+1
           apar(npar)=aapar
520        continue
           cline=cline(nn+1:)
           go to 370
         end do
600      continue
         close(unit=10)
         if(nparfile(itplfle).eq.0)then
           write(amessage,610) trim(afile)
610        format('File ',a,' contains no parameters.')
           go to 9890
         else
           call writint(anum,nparfile(itplfle))
           write(6,620) trim(anum),trim(afile)
620        format(' - ',a,' params found in file ',a,'.')
         end if
       end do

! -- Now we write the partial PEST control file.

       write(6,*)
       write(6,630) trim(apestfile)
630    format(' - writing file ',a,'...')
       open(unit=20,file=pestfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,640) trim(apestfile)
640      format('Cannot write to file ',a,'.')
         go to 9890
       end if
       write(20,650)
650    format('pcf')
       write(20,660)
660    format('* control data')
       write(20,670)
670    format('restart estimation')
       npargp=0
       do itplfle=1,ntplfle
         if(itplfle.eq.1)then
           npargp=1
         else
           apargp=pargroup(itplfle)
           do i=1,itplfle-1
             if(apargp.eq.pargroup(i)) go to 690
           end do
           npargp=npargp+1
690        continue
         end if
       end do
       write(20,700) npar,nobs,npargp,nprior,nobsgp
700    format(5i10)
       write(20,710) ntplfle
710    format(i5,'  0    single  point')
       write(20,720)
720    format(' 10.0  -3.0  0.3  0.03  10  lamforgive')
       write(20,730)
730    format(' 10.0  10.0  0.001')
       write(20,740)
740    format(' 0.1')
       write(20,750)
750    format(' 50  0.005  4  4  0.005  4')
       write(20,760)
760    format(' 0  0  0')
       write(20,770)
770    format('* singular value decomposition')
       write(20,780)
780    format('1')
       write(20,790) max(npar,5000)
790    format(i6,'  5e-7')
       write(20,800)
800    format('0')
       write(20,810)
810    format('* parameter groups')
       do itplfle=1,ntplfle
         if(itplfle.eq.1)then
           write(20,820) trim(pargroup(itplfle))
820        format(a,t15,'relative   0.015   0.0  switch',
     +     '  2  parabolic')
         else
           do i=1,itplfle-1
             if(apargp.eq.pargroup(i)) go to 840
           end do
           write(20,820) trim(pargroup(itplfle))
840        continue
         end if
       end do
       write(20,850)
850    format('* parameter data')
       lastnpar=0
       do itplfle=1,ntplfle
         firstnpar=lastnpar+1
         lastnpar=lastnpar+nparfile(itplfle)
         if(itrans(itplfle).eq.0)then
           atemp20='none'
         else if(itrans(itplfle).eq.1)then
           atemp20='log'
         else if(itrans(itplfle).eq.-1000000)then
           atemp20='fixed'
         end if
         if(lbound(itplfle).gt.0.0d0)then
           aparchglim='factor'
         else
           aparchglim='relative'
         end if
         do ipar=firstnpar,lastnpar
           write(20,860) trim(apar(ipar)),trim(atemp20),
     +     trim(aparchglim),
     +     pval(itplfle),lbound(itplfle),ubound(itplfle),
     +     trim(pargroup(itplfle))
860        format(a,t15,a,t22,a,t32,3(1pg14.7,2x),a12,3x,'1.0   0.0')
         end do
       end do
       write(20,870)
870    format('* observation groups')
       write(20,880)
880    format('* observation data')
       write(20,890)
890    format('* model command line')
       write(20,900)
900    format('* model input/output')
       do itplfle=1,ntplfle
         call addquote(tplfle(itplfle),afile)
         call addquote(mfile(itplfle),bfile)
         write(20,910) trim(afile),trim(bfile)
910      format(a,'    ',a)
       end do
       close(unit=20,iostat=ierr)
       write(6,920) trim(apestfile)
920    format(' - file ',a,' written ok.')
       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' TPL2PST is run using the command:',/)
       write(6,8920)
8920   format('     tpl2pst infile pestfile ',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8950)
8950   format('     infile   is a TPL2PST input file, and')
       write(6,8980)
8980   format('     pestfile is the partial PEST control file ',
     + 'it must write.')
       go to 9999

9000   call writint(aline,iline)
       write(amessage,9010) trim(aline),trim(ainfile)
9010   format('Cannot read filename on line ',a,' of file ',a,'.')
       go to 9890

9030   write(amessage,9040) trim(aline),trim(infile)
9040   format('Insufficient entries on line ',a,' of file ',a,'.')
       go to 9890

9050   write(amessage,9060) trim(keyword),trim(aline),trim(ainfile)
9060   format('The ',a,' keyword is repeated in the same block ',
     + 'at line ',a,' of file ',a,'.')
       go to 9890

9100   write(amessage,9110) trim(keyword),trim(aline),trim(ainfile)
9110   format('Cannot read number associated with ',a,' keyword at ',
     + 'line ',a,' of file ',a,'.')
       go to 9890

9120   write(amessage,9130) trim(keyword),trim(aline),trim(ainfile)
9130   format('The ',a,' keyword is missing from the block that ',
     + 'finishes at line ',a,' of file ',a,'.')
       go to 9890

9150   write(amessage,9160) trim(afile)
9160   format('The first line of template file ',a,' should be "ptf" ',
     + 'followed by a space followed by a parameter delimiter.')
       go to 9890

9890   continue
       amessage=' '//trim(amessage)
       call writmess(6,amessage)

9900   continue
       close(unit=20,iostat=ierr)
       close(unit=10,iostat=ierr)

9999   continue

       end




       subroutine stripline(cline)

! -- Subroutine stripline makes a line blank if the leading character is "#".

       implicit none
       character*(*), intent(inout) :: cline
       integer  :: nn

       nn=index(cline,'#')
       if(nn.ne.0)then
         if(nn.eq.1)then
           cline=' '
         else
           if(cline(1:nn-1).eq.' ')then
             cline=' '
           end if
         end if
       end if

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

C -- Subroutine lowcas converts a string to lower case.

        integer i,j
        character*(*) astrng

        do 10 i=1,len_trim(astrng)
        j=ichar(astrng(i:i))
        if((j.ge.65).and.(j.le.90)) astrng(i:i)=char(j+32)
10      continue
        return
        end


        SUBROUTINE UPCAS(ASTRNG)

C -- SUBROUTINE UPCAS CONVERTS A STRING TO UPPER CASE

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,LEN_TRIM(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.97).AND.(J.LE.122)) ASTRNG(I:I)=CHAR(J-32)
10      CONTINUE
        RETURN
        END



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



        SUBROUTINE DREALRD(IFAIL,CLINE,RTEMP)

C -- Subroutine DREALRD reads a real number from a string.

        INTEGER IFAIL
        DOUBLE PRECISION RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
        READ(CLINE,AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END



        subroutine getfile(ifail,cline,filename,ibeg,iend)

C-- Subroutine getfile extracts a filename from a string.

C -- Arguments are as follows:-
C       ifail: returned as zero if filename successfully read
C       cline: a character string containing the file name
C       filename: the name of the file read from the string
C       ibeg: character position at which to begin search for filename
C       iend: on input  - character position at which to end search for filename
C             on output - character postion at which filename ends


        integer, intent(out)               :: ifail
        integer, intent(in)                :: ibeg
        integer, intent(inout)             :: iend
        character (len=*), intent(in)      :: cline
        character (len=*), intent(out)     :: filename

        integer                            :: i,j,k
        character (len=1)                  :: aa

        ifail=0
        do i=ibeg,iend
          aa=cline(i:i)
          if((aa.ne.' ').and.(aa.ne.',').and.(aa.ne.char(9)))go to 50
        end do
        ifail=1
        return

50      if((aa.eq.'"').or.(aa.eq.''''))then
          do j=i+1,iend
            if(cline(j:j).eq.aa) go to 60
          end do
          ifail=1
          return
60        iend=j
          if(i+1.gt.j-1)then
            ifail=1
            return
          else
            filename=cline(i+1:j-1)
          end if
        else
          do j=i+1,iend
            if((cline(j:j).eq.' ').or.(cline(j:j).eq.',').or.
     +         (cline(j:j).eq.char(9)))then
              k=j-1
              go to 100
            end if
          end do
          k=iend
100       filename=cline(i:k)
          if(cline(k:k).eq.'"')then
            ifail=1
            return
          else if(cline(k:k).eq.'''')then
            ifail=1
            return
          end if

          iend=k
        end if
        filename=adjustl(filename)
        return

        end

