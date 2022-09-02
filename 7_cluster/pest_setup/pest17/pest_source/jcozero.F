       program jcozero

! --   Program JCOZERO allows a user to zero out elements of an existing JCO file.

       implicit none

       logical, external :: string_match
       logical lexist
       integer, parameter :: NUMPAR=5000
       integer, parameter :: NUMOBS=5000
       logical lmp(NUMPAR),lmo(NUMOBS)
       integer ifail,ierr,n,n1,itemp1,itemp2,nn
       integer nespar,bn,nxrow
       integer icount,i,j,ies,irow,iend,numblock,iline
       integer iipar,iiobs,nnpar,nnobs
       integer lw(10),rw(10)
       double precision dtemp
       character*5 aext
       character*10 how,ablock,aline
       character*12 aversion,pastring
       character*12 pstring(NUMPAR)
       character*20 atext,atext_alt,anumpar,anumobs,oastring
       character*20 ostring(NUMOBS)
       character*30 astring
       character*256 jcofile1,jcofile2,cleanfile,afile
       character*500 cline
       character*1500 errmsg

       double precision, allocatable :: x(:,:)
       character*12, allocatable :: apar(:)
       character*20, allocatable :: aobs(:)

! -- Initialisation

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(' JCOZERO Version ',a,'. Watermark Numerical Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

! -- The command line is parsed.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       jcofile1=cline(lw(1):rw(1))
       cleanfile=cline(lw(2):rw(2))
       jcofile2=cline(lw(3):rw(3))
       call remchar(jcofile1,char(211))
       call remchar(cleanfile,char(211))
       call remchar(jcofile2,char(211))

#ifndef UNIX
       call lowcas(jcofile1)
       call lowcas(jcofile2)
       call lowcas(cleanfile)
#endif

       afile=jcofile1
       n=len_trim(jcofile1)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=jcofile1(n1:n)
       call lowcas(aext)
       if((aext(1:1).eq.'.').and.(aext.ne.'.jco')) go to 9150
       if(aext.eq.'.jco')jcofile1(n1:n)=' '
       n=len_trim(jcofile1)
       jcofile1(n+1:)='.jco'

       afile=jcofile2
       n=len_trim(jcofile2)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=jcofile2(n1:n)
       call lowcas(aext)
       if((aext(1:1).eq.'.').and.(aext.ne.'.jco')) go to 9150
       if(aext.eq.'.jco')jcofile2(n1:n)=' '
       n=len_trim(jcofile2)
       jcofile2(n+1:)='.jco'

! -- The JCO file is opened and read.

       write(6,*)
       call addquote(jcofile1,afile)
       write(6,11) trim(afile)
11     format(' - reading first JCO file ',a,'...')
       how='read'
       call open_unformatted_file(ifail,10,how,jcofile1,errmsg)
       if(ifail.ne.0) go to 9890
       read(10,err=9250,end=9250)itemp1,itemp2
       if(itemp1.lt.0)then
         nespar=abs(itemp1)
         bn=abs(itemp2)
         nxrow=bn
       else
         write(errmsg,240) trim(afile)
240      format('Jacobian matrix file ',a,' was not produced by ',
     +   'version 8 or later of PEST; use program JCOTRANS to ',
     +   'convert this to new format.')
         go to 9890
       end if
       allocate(x(nxrow,nespar),apar(nespar),aobs(nxrow),stat=ierr)
       if(ierr.ne.0) go to 9200
       x=0.0d0         ! an array
       read(10,err=9250,end=9250)icount
       do i=1,icount
         read(10,err=9250,end=9250) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         x(irow,ies)=dtemp
       end do
       do i=1,nespar
         read(10,err=9250,end=9250) apar(i)
         call lowcas(apar(i))
       end do
       do i=1,nxrow
         read(10,err=9250,end=9250) aobs(i)
         call lowcas(aobs(i))
       end do
       close(unit=10)
       write(6,20) trim(afile)
20     format(' - file ',a,' read ok.')


! -- Next the existing Jacobian file is deleted.

       inquire (file=jcofile2,exist=lexist)
       if(lexist)then
         how='read'
         call open_unformatted_file(ifail,30,how,jcofile2,errmsg)
         close(unit=30,status='delete',iostat=ierr)
       end if

! -- The zeroing file is read block by block.
!    At the same time, Jacobian matrix zeroing is undertaken.

       write(6,*)
       call addquote(cleanfile,afile)
       write(6,21) trim(afile)
21     format(' - reading and processing file ',a,'...')
       open(unit=10,file=cleanfile,status='old',action='read',
     + iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,30) trim(afile)
30       format('Cannot open file ',a,'.')
         go to 9890
       end if

! -- We read one block of parameters/observations.

       iend=0
       numblock=0
       iline=0

! -- First we find the initial parameter sub-block.

100    continue
       atext='* parameters'
       atext_alt='*parameters'
       do
         iline=iline+1
         read(10,'(a)',end=9020) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         call lowcas(cline)
         if((cline(1:12).eq.trim(atext)).or.
     +      (cline(1:11).eq.trim(atext_alt)))exit
         go to 9000
       end do

! -- Next we read a parameter/observation block.

101    continue
       numblock=numblock+1
       call writint(ablock,numblock)
       write(6,102) trim(ablock)
102    format(' - parameter/observation block ',a,'...')
       atext='* observations'
       atext_alt='*observations'
       iipar=0
       do
         iline=iline+1
         read(10,'(a)',end=9020) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         call lowcas(cline)
         if((cline(1:14).eq.trim(atext)).or.
     +      (cline(1:13).eq.trim(atext_alt))) go to 150
         iipar=iipar+1
         if(iipar.gt.NUMPAR)then
           call writint(anumpar,NUMPAR)
           write(errmsg,120) trim(anumpar)
120        format('Too many parameter names in one block. Increase ',
     +     'NUMPAR above ',a,' and re-compile program.')
           go to 9890
         end if
         call linspl(ifail,1,lw,rw,cline)
         astring=cline(lw(1):rw(1))
         nn=len_trim(astring)
         if(nn.gt.12)then
           call writint(aline,iline)
           write(errmsg,130) trim(astring),trim(aline),trim(afile)
130        format('The string "',a,'" must be 12 characters or less ',
     +     'in length at line ',a,' of file ',a,'.')
           go to 9890
         end if
         icount=0
         do i=1,nn
           if((astring(i:i).eq.'*').or.(astring(i:i).eq.'?'))
     +     icount=icount+1
           if(icount.gt.1) go to 9450
         end do
         pstring(iipar)=astring
       end do
150    continue
       atext='* parameters'
       atext_alt='*parameters'
       iiobs=0
       do
         iline=iline+1
         read(10,'(a)',end=199) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         call lowcas(cline)
         if((cline(1:12).eq.trim(atext)).or.
     +      (cline(1:11).eq.trim(atext_alt))) go to 200
         iiobs=iiobs+1
         if(iiobs.gt.NUMOBS)then
           call writint(anumobs,NUMOBS)
           write(errmsg,160) trim(anumobs)
160        format('Too many observation names in one block. Increase ',
     +     'NUMOBS above ',a,' and re-compile program.')
           go to 9890
         end if
         call linspl(ifail,1,lw,rw,cline)
         astring=cline(lw(1):rw(1))
         nn=len_trim(astring)
         if(nn.gt.20)then
           call writint(aline,iline)
           write(errmsg,170) trim(astring),trim(aline),trim(afile)
170        format('The string "',a,'" must be 20 characters or less ',
     +     'in length at line ',a,' of file ',a,'.')
           go to 9890
         end if
         icount=0
         do i=1,nn
           if((astring(i:i).eq.'*').or.(astring(i:i).eq.'?'))
     +     icount=icount+1
           if(icount.gt.1) go to 9450
         end do
         ostring(iiobs)=astring
       end do

! -- Now we process the information in the parameter/observation block that
!    we have just read.

199    iend=1
200    continue

       nnpar=iipar
       nnobs=iiobs
       if(nnpar.eq.0)then
         call writint(ablock,numblock)
         write(errmsg,210) trim(ablock),trim(afile)
210      format('There are no parameters cited in ',
     +   'parameter/observation block number ',a,' in file ',a,'.')
         go to 9890
       end if
       if(nnobs.eq.0)then
         call writint(ablock,numblock)
         write(errmsg,220) trim(ablock),trim(afile)
220      format('There are no observations cited in ',
     +   'parameter/observation block number ',a,' in file ',a,'.')
         go to 9890
       end if
       do iipar=1,nnpar
         lmp(iipar)=.FALSE.
       end do
       do iiobs=1,nnobs
         lmo(iiobs)=.FALSE.
       end do
       do iipar=1,nnpar
         pastring=pstring(iipar)
         do ies=1,nespar
           if(string_match(apar(ies),pastring))then
             lmp(iipar)=.TRUE.
             do iiobs=1,nnobs
               oastring=ostring(iiobs)
               do irow=1,nxrow
                 if(string_match(aobs(irow),oastring))then
                   lmo(iiobs)=.TRUE.
                   x(irow,ies)=0.0d0
                   if((index(oastring,'*').eq.0).and.
     +                (index(oastring,'?').eq.0)) go to 225
                 end if
               end do
225            continue
             end do
             if((index(pastring,'*').eq.0).and.
     +          (index(pastring,'?').eq.0)) go to 235
           end if
         end do
235      continue
       end do
       icount=0
       do iipar=1,nnpar
         if(.not.lmp(iipar))then
           write(6,236) trim(pstring(iipar))
236        format('   Warning: no match found for parameter ',
     +     'string "',a,'".')
           icount=icount+1
         end if
       end do
       if(icount.ne.nnpar)Then
         do iiobs=1,nnobs
           if(.not.lmo(iiobs))then
             write(6,237) trim(ostring(iiobs))
237          format('   Warning: no match found for observation ',
     +       'string "',a,'".')
           end if
         end do
       end if
       if(iend.eq.0) go to 101

1000   continue
       close(unit=10)
!       call writint(ablock,numblock)
!       write(6,1010) trim(ablock),trim(afile)
!1010   format(' - ',a,' parameter/observation blocks read from file ',
!     + a,'.')

! -- The new JCO file is written.

       write(6,*)
       call addquote(jcofile2,afile)
       write(6,700) trim(afile)
700    format(' - writing new JCO file ',a,'...')

       how='write'
       call open_unformatted_file(ifail,15,how,jcofile2,errmsg)
       if(ifail.ne.0) go to 9300

       write(15,err=9300) -nespar,-nxrow
       icount=0
       do i=1,nespar
          do j=1,nxrow
            if(x(j,i).ne.0.0d0) icount=icount+1
          end do
       end do
       write(15,err=9300) icount
       do i=1,nespar
         do j=1,nxrow
           if(x(j,i).ne.0.0d0)then
             icount=(i-1)*nxrow+j
             write(15,err=9300)icount,x(j,i)
           end if
         end do
       end do
       do i=1,nespar
         write(15,err=9300) apar(i)
       end do
       do i=1,nxrow
         write(15,err=9300) aobs(i)
       end do
705    continue
       close(unit=15)
       write(6,490) trim(afile)
490    format(' - file ',a,' written ok.')

       go to 9900

8900   continue

       write(6,8910)
8910   format(/,' JCOZERO is run using the command:',/)
       write(6,8920)
8920   format('     jcozero jcofile1 cleanfile jcofile2')
       write(6,8930)
8930   format(/,' where',/)
       write(6,8940)
8940   format('     jcofile1 is an existing JCO file,')
       write(6,8945)
8945   format('     zerofile is a file specifying which JCO elements ',
     + 'to zero, and')
       write(6,8950)
8950   format('     jcofile2 is a new JCO file to be written by',
     + ' JCOZERO.')

       go to 9999

9000   call writint(aline,iline)
       write(errmsg,9010) trim(aline),trim(afile),trim(atext)
9010   format('Line ',a,' of file ',a,' should comprise the ',
     + 'text "',a,'".')
       go to 9890

9020   write(errmsg,9030) trim(afile)
9030   format('Unexpected end encountered to file ',a,'.')
       go to 9890

9150   write(errmsg,9160) trim(afile)
9160   format('File ',a,' does not have an extension of ".jco".')
       go to 9890

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9250   write(errmsg,9260) trim(afile)
9260   format('Error reading Jacobian matrix file ',a,'.')
       go to 9890

9300   write(errmsg,9310) trim(afile)
9310   format('Cannot write to file ',a,'.')
       go to 9890

9450   call writint(aline,iline)
       write(errmsg,9460) trim(astring),trim(aline),trim(afile)
9460   format('The string "',a,'" at line ',a,' of file ',a,
     + ' must contain only a single "*" or "?" character.')
       go to 9890

9890   continue
       call write_message(errmsg)
9900   continue

       deallocate(x,apar,aobs,stat=ierr)

9999   continue
       end



        logical function string_match(astring,agen)

! -- Function STRING_MATCH inquires whether the generic string AGEN matches a real string ASTRING.
! -- It is assumed that both strings have already been converted to lower case, and that neither
!    string has a blank or tab.
!    It is also assumed that only a single * or a single ? exists in the string.

        implicit none
        character (len=*), intent(in)      :: astring
        character (len=*), intent(in)      :: agen

        logical                            :: star,ques
        integer                            :: nnn,ii,lt,ns,ng
        character (len=21)                 :: as,ag,sag,sas

! -- Initialization

        star=.false.
        ques=.false.
        as=adjustl(astring)
        ag=adjustl(agen)
        nnn=index(ag,'*')
        if(nnn.ne.0)then
          star=.true.
        else
          nnn=index(ag,'?')
          if(nnn.ne.0) ques=.true.
        end if

! -- First we look for the obvious matches.

        if(nnn.eq.0)then
          if(as.eq.ag) then
            go to 8000
          else
            go to 9000
          end if
        end if
        if(as.eq.' ')then
          if(ag.eq.' ') then
            go to 8000
          else
            go to 9000
          end if
        end if

        if(ag.eq.'*') go to 8000
        if(ag.eq.'?') then
          if(len_trim(as).eq.1) go to 8000
        end if

        ns=len_trim(as)
        ng=len_trim(ag)

        if(nnn.eq.1)then
          sag=ag(2:)
          lt=len_trim(sag)+1
          ii=index(as,sag(1:lt))
          if((ii.eq.0).or.(ii.eq.1)) then
            go to 9000
          else if(ii.eq.2) then
            go to 8000
          else
            if(star) then
              go to 8000
            else
              go to 9000
            end if
          end if
        else if(nnn.eq.ng)then
          sag=ag(1:ng-1)
          if((as(1:ng-1).eq.sag).and.(as(ng:ng).ne.' '))then
            if(as(ng+1:ng+1).eq.' ')then
              go to 8000
            else
              if(star)then
                go to 8000
              else
                go to 9000
              end if
            end if
          else
            go to 9000
          end if
        else
          if(as(1:nnn-1).ne.ag(1:nnn-1)) go to 9000
          sag=ag(nnn+1:)
          sas=as(nnn:)
          lt=len_trim(sag)+1
          ii=index(sas,sag(1:lt))
          if((ii.eq.0).or.(ii.eq.1)) then
            go to 9000
          else if(ii.eq.2) then
            go to 8000
          else
            if(star)then
              go to 8000
            else
              go to 9000
            end if
          end if
        end if

8000    continue
        string_match=.TRUE.
        go to 9900

9000    continue
        string_match=.FALSE.
        go to 9900

9900    continue
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


