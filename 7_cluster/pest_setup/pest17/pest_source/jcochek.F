       program jcochek

! --   Program jcochek checks that a JCO file is comptible with a corresponding PST file.

       implicit none

       logical skipline
       integer ifail,ierr
       integer itemp1,itemp2
       integer n,n1,i,icount,iprior,bn,j,k,nb
       integer npar,nobs,npargp,nprior,nxrow,nespar
       integer neqq
       integer lw(11),rw(11)
       double precision dtemp
       character*10 how
       character*12 aapar
       character*12 aversion
       character*15 atemp,atemp1,atemp2
       character*20 aaobs
       character*4 aext
       character*200 pestfile,jcofile,afile,afile1,afile2
       character*400  cline,errmsg

       integer, allocatable :: itrans(:)
       character*12, allocatable :: apar(:)
       character*20, allocatable :: aobs(:)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(' JCOCHEK Version ',a,'. Watermark Numerical Computing.')
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
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.eq.0) go to 9000
       call linspl(ifail,1,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       pestfile=cline(lw(1):rw(1))
       call remchar(pestfile,char(211))
       n=len_trim(pestfile)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=pestfile(n1:n)
       call lowcas(aext)
       if(aext.eq.'.pst')pestfile(n1:n)=' '
       jcofile=pestfile
       n=len_trim(pestfile)
       pestfile(n+1:)='.pst'
       jcofile(n+1:)='.jco'

! -- Input files are opened.

       open(unit=10,file=pestfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(pestfile,afile)
         write(errmsg,110) trim(afile)
110      format('Cannot open PEST control file ',a,'.')
         go to 9890
       end if

       how='read'
       call open_unformatted_file(ifail,15,how,jcofile,errmsg)
       if(ifail.ne.0)then
         call addquote(jcofile,afile)
         write(errmsg,120) trim(afile)
120      format('Cannot open unformatted Jacobian matrix file ',a,'.')
         go to 9890
       end if

! -- Read the PEST control file.

       do i=1,3
124      continue
         read(10,'(a)',err=9100,end=9100) cline
         if(skipline(cline)) go to 124
       end do
112    continue
       read(10,'(a)',err=9100,end=9100) cline
       if(skipline(cline)) go to 112
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0) go to 9100
       call intread(ifail,cline(lw(1):rw(1)),npar)
       if(ifail.ne.0) go to 9100
       call intread(ifail,cline(lw(2):rw(2)),nobs)
       if(ifail.ne.0) go to 9100
       call intread(ifail,cline(lw(3):rw(3)),npargp)
       if(ifail.ne.0) go to 9100
       call intread(ifail,cline(lw(4):rw(4)),nprior)
       if(ifail.ne.0) go to 9100
       if(npar.le.0) go to 9100
       nxrow=nobs+nprior
       allocate(apar(npar),itrans(npar),aobs(nxrow),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,125)
125      format('Cannot allocate sufficient memory to continue ',
     +   'JCOCHEK execution.')
         go to 9890
       end if
       do
         read(10,'(a)',err=9100,end=130) cline
         if(skipline(cline)) cycle
         call lowcas(cline)
         if(index(cline,'* parameter data').ne.0) go to 150
       end do
130    continue
       call addquote(pestfile,afile)
       write(errmsg,140) trim(afile)
140    format('Cannot find "parameter data" section of PEST ',
     + 'control file ',a,'.')
       go to 9890
150    do i=1,npar
151      continue
         read(10,'(a)',err=9100,end=9100) cline
         if(skipline(cline)) go to 151
         neqq=index(cline,'=')
         if(neqq.gt.1) go to 151
         call lowcas(cline)
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9100
         if(cline(lw(2):rw(2)).eq.'file_parameter')go to 151
         call linspl(ifail,3,lw,rw,cline)
         if(ifail.ne.0) go to 9100
         apar(i)=cline(lw(1):rw(1))
         atemp=cline(lw(2):rw(2))
         atemp1=cline(lw(3):rw(3))
         if(atemp.eq.'log')then
           itrans(i)=1
         else if(atemp.eq.'none')then
           itrans(i)=0
         else if(atemp.eq.'fixed')then
           itrans(i)=-1000000
         else if(atemp.eq.'tied')then
           itrans(i)=-1
         else
           go to 9100
         end if
       end do
       icount=0
       do i=1,npar
         if(itrans(i).eq.-1)icount=icount+1
       end do
       if(icount.gt.0)then
         do i=1,icount
152        continue
           read(10,'(a)',err=9100,end=9100) cline
           if(skipline(cline)) go to 152
           neqq=index(cline,'=')
           if(neqq.gt.1) go to 152
           call lowcas(cline)
           call linspl(ifail,2,lw,rw,cline)
           if(ifail.ne.0) go to 9100
           atemp1=cline(lw(1):rw(1))
           atemp2=cline(lw(2):rw(2))
           if(atemp2.eq.'file_parameter')go to 152
           do j=1,npar
             if(atemp1.eq.apar(j)) go to 170
           end do
           go to 9100
170        continue
           do k=1,npar
             if(atemp2.eq.apar(k)) go to 180
           end do
           go to 9100
180        continue
           if(itrans(j).ne.-1) go to 9100
           if(itrans(k).lt.0) go to 9100
           itrans(j)=-k
         end do
       end if

       do
         read(10,'(a)',err=9100,end=200) cline
         if(skipline(cline)) cycle
         call lowcas(cline)
         if(index(cline,'* observation data').ne.0) go to 220
       end do
200    call addquote(pestfile,afile)
       write(errmsg,210) trim(afile)
210    format('Cannot find "observation data" section of file ',a,'.')
       go to 9890
220    do i=1,nobs
221      continue
         read(10,'(a)',err=9100,end=9100) cline
         if(skipline(cline)) go to 221
         if(cline.eq.' ') go to 221
         call linspl(ifail,1,lw,rw,cline)
         aobs(i)=cline(lw(1):rw(1))
         call lowcas(aobs(i))
       end do
       if(nprior.ne.0)then
         do
           read(10,'(a)',err=9100,end=300) cline
           if(skipline(cline)) cycle
           call lowcas(cline)
           if(index(cline,'* prior information').ne.0) go to 335
         end do
300      call addquote(pestfile,afile)
         write(errmsg,310) trim(afile)
310      format('Cannot find "prior information" section of ',
     +   'file ',a,'.')
         go to 9890
335      continue
         do iprior=1,nprior
240        continue
           read(10,'(a)',err=9100,end=9100) cline
           if(skipline(cline)) go to 240
           cline=adjustl(cline)
           if(cline(1:1).eq.'&')then
             if(iprior.eq.1) go to 9100
             go to 240
           end if
           nb=index(cline,' ')
           aobs(nobs+iprior)=cline(1:nb-1)
           call lowcas(aobs(nobs+iprior))
         end do
       end if
       close(unit=10)

       nespar=0
       do i=1,npar
         if(itrans(i).ge.0)nespar=nespar+1
       end do
       if(nespar.eq.0)then
         call addquote(pestfile,afile)
         write(errmsg,231) trim(afile)
231      format('There are no adjustable parameters in file ',a,'.')
         go to 9890
       end if

       call addquote(pestfile,afile)
       write(6,230) trim(afile)
230    format(' - file ',a,' read ok.')

! -- The Jacobian matrix file is read.

       read(15,err=9250,end=9250)itemp1,itemp2
       if(abs(itemp1).ne.nespar) go to 9200
       bn=abs(itemp2)
       if(itemp1.lt.0)then
         continue
       else
         call addquote(jcofile,afile)
         write(errmsg,241) trim(afile)
241      format('Jacobian matrix file ',a,' was not produced by ',
     +   'version 8 or later of PEST; use program JCOTRANS to ',
     +   'convert this to new format.')
         go to 9890
       end if
       if((bn.ne.nobs).and.(bn.ne.nxrow)) go to 9200

       read(15,err=9250,end=9250)icount
       do i=1,icount
         read(15,err=9250,end=9250) j,dtemp
       end do
       do i=1,npar
         if(itrans(i).lt.0) cycle
         read(15,err=9250,end=9250) aapar
         call lowcas(aapar)
         if(aapar.ne.apar(i)) go to 9200
       end do
       do i=1,nobs
         read(15,err=9250,end=9250) aaobs
         call lowcas(aaobs)
         if(aaobs.ne.aobs(i)) go to 9200
       end do
       if(nprior.ne.0)then
         if(nb.eq.nxrow)then
           do iprior=1,nprior
             read(15,err=9250,end=9250) aaobs
             call lowcas(aaobs)
             if(aaobs.ne.aobs(iprior+nobs)) go to 9200
           end do
         end if
       end if

       close(unit=15)
       call addquote(jcofile,afile)
       write(6,230) trim(afile)

       write(6,*)
       write(6,510)
510    format(' Conclusions from analysis:-')
       call addquote(pestfile,afile1)
       call addquote(jcofile,afile2)
       if(bn.eq.nxrow)then
         write(errmsg,520) trim(afile1),trim(afile2)
520      format('PEST control file ',a,' and JCO file ',a,
     +   ' are compatible.')
       else
         write(errmsg,521) trim(afile1),trim(afile2)
521      format('PEST control file ',a,' and JCO file ',a,
     +   ' are (almost) compatible.')
       end if
       call write_message(errmsg)

       if(nprior.ne.0)then
         write(6,540)
540      format(' Warning:-')
         if(bn.eq.nobs)then
           write(errmsg,550) trim(afile2),trim(afile1)
550        format('JCO file ',a,' lacks sensitivities to prior ',
     +     'information cited in PEST control file ',a,'.')
           call write_message(errmsg)
         else
           write(errmsg,560) trim(afile2),trim(afile1)
560        format('Prior information sensitivities in JCO file ',a,
     +     ' have not been cross-checked against prior information ',
     +     'coefficients in PEST control file ',a,'.')
           call write_message(errmsg)
         end if
       end if

       go to 9900

9000   continue
       write(6,9010)
9010   format(/,' JCOCHEK is run using the command:',/)
       write(6,9020)
9020   format('     jcochek casename ',/)
       write(6,9030)
9030   format(' where',/)
       write(6,9050)
9050   format('     casename is a PEST case for which both a PST ',
     + 'and JCO file exist.')
       go to 9999

9100   call addquote(pestfile,afile)
       write(errmsg,9110) trim(afile)
9110   format('Error reading PEST control file ',a,'; check this ',
     + 'file with PESTCHEK.')
       go to 9890

9200   continue
       call addquote(jcofile,afile)
       call addquote(pestfile,afile1)
       write(errmsg,9210) trim(afile),trim(afile1)
9210   format('Jacobian matrix file ',a,' is incompatible with PEST ',
     + 'control file ',a,'.')
       go to 9890
9250   call addquote(jcofile,afile)
       write(errmsg,9260) trim(afile)
9260   format('Error reading Jacobian matrix file ',a,'.')
       go to 9890

9890   call write_message(errmsg)
9900   continue

       if(allocated(itrans)) deallocate(itrans,stat=ierr)
       if(allocated(apar)) deallocate(apar,stat=ierr)
       if(allocated(aobs)) deallocate(aobs,stat=ierr)

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
