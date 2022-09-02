       program jcosum

! --   Program jcosum forms a new JCO file by summing the contents of two existing
!      JCO files.

       implicit none

       logical lexist
       integer ifail,itemp1,itemp2,ierr
       integer n,n1,icount,i,j
       integer nespar,nxrow,bn,irow,ies
       integer lw(10),rw(10)
       double precision dtemp
       double precision jcofac1,jcofac2
       character*5 aext
       character*10 how
       character*12 aversion
       character*12 aapar
       character*20 aaobs
       character*256 jcofile1,jcofile2,jcofile3,afile
       character*500 cline
       character*1500 errmsg

       double precision, allocatable :: x1(:,:),x2(:,:)
       character*12, allocatable :: apar(:)
       character*20, allocatable :: aobs(:)

! -- Initialisation

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(' JCOSUM Version ',a,'. Watermark Numerical Computing.')
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
       call linspl(ifail,6,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       jcofile1=cline(lw(1):rw(1))
       jcofile2=cline(lw(3):rw(3))
       jcofile3=cline(lw(5):rw(5))
       call remchar(jcofile1,char(211))
       call remchar(jcofile2,char(211))
       call remchar(jcofile3,char(211))
       call drealrd(ifail,cline(lw(2):rw(2)),jcofac1)
       if(ifail.ne.0) go to 8900
       call drealrd(ifail,cline(lw(4):rw(4)),jcofac2)
       if(ifail.ne.0) go to 8900

#ifndef UNIX
       call lowcas(jcofile1)
       call lowcas(jcofile2)
       call lowcas(jcofile3)
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

       afile=jcofile3
       n=len_trim(jcofile3)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=jcofile3(n1:n)
       call lowcas(aext)
       if((aext(1:1).eq.'.').and.(aext.ne.'.jco')) go to 9150
       if(aext.eq.'.jco')jcofile3(n1:n)=' '
       n=len_trim(jcofile3)
       jcofile3(n+1:)='.jco'

! -- The first JCO file is opened and read.

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
       allocate(x1(nxrow,nespar),apar(nespar),aobs(nxrow),stat=ierr)
       if(ierr.ne.0) go to 9200
       x1=0.0d0         ! an array
       read(10,err=9250,end=9250)icount
       do i=1,icount
         read(10,err=9250,end=9250) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         x1(irow,ies)=dtemp
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

! -- The second JCO file is opened and read.

       write(6,*)
       call addquote(jcofile2,afile)
       write(6,21) trim(afile)
21     format(' - reading second JCO file ',a,'...')
       how='read'
       call open_unformatted_file(ifail,10,how,jcofile2,errmsg)
       if(ifail.ne.0) go to 9890
       read(10,err=9250,end=9250)itemp1,itemp2
       if(itemp1.lt.0)then
         if(abs(itemp1).ne.nespar) go to 9000
         if(abs(itemp2).ne.bn) go to 9000
       else
         write(errmsg,240) trim(afile)
         go to 9890
       end if
       allocate(x2(nxrow,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       x2=0.0d0    ! an array
       read(10,err=9250,end=9250)icount
       do i=1,icount
         read(10,err=9250,end=9250) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         x2(irow,ies)=dtemp
       end do
       do i=1,nespar
         read(10,err=9250,end=9250) aapar
         call lowcas(aapar)
         if(aapar.ne.apar(i)) go to 9050
       end do
       do i=1,nxrow
         read(10,err=9250,end=9250) aaobs
         call lowcas(aaobs)
         if(aaobs.ne.aobs(i)) go to 9070
       end do
       close(unit=10)
       write(6,20) trim(afile)

! -- The Jacobian summation process is undertaken.

       x2=x1*jcofac1+x2*jcofac2           ! arrays

! -- The new JCO file is written.

! -- But first an existing Jacobian file is deleted.

       call addquote(jcofile3,afile)
       inquire (file=jcofile3,exist=lexist)
       if(lexist)then
         how='read'
         call open_unformatted_file(ifail,30,how,jcofile3,errmsg)
         close(unit=30,status='delete',iostat=ierr)
       end if

       write(6,*)
       write(6,700) trim(afile)
700    format(' - writing new JCO file ',a,'...')

       how='write'
       call open_unformatted_file(ifail,15,how,jcofile3,errmsg)
       if(ifail.ne.0) go to 9300

       write(15,err=9400) -nespar,-nxrow
       icount=0
       do i=1,nespar
          do j=1,nxrow
            if(x2(j,i).ne.0.0d0) icount=icount+1
          end do
       end do
       write(15,err=9400) icount
       do i=1,nespar
         do j=1,nxrow
           if(x2(j,i).ne.0.0d0)then
             icount=(i-1)*nxrow+j
             write(15,err=9400)icount,x2(j,i)
           end if
         end do
       end do
       do i=1,nespar
         write(15,err=9400) apar(i)
       end do
       do i=1,nxrow
         write(15,err=9400) aobs(i)
       end do
705    continue
       close(unit=15)
       write(6,490) trim(afile)
490    format(' - file ',a,' written ok.')

       go to 9900

8900   continue

       write(6,8910)
8910   format(/,' JCOSUM is run using the command:',/)
       write(6,8920)
8920   format('     jcosum jcofile1 fac1 jcofile2 fac2 jcofile3')
       write(6,8930)
8930   format(/,' where',/)
       write(6,8940)
8940   format('     jcofile1 is an existing JCO file,')
       write(6,8945)
8945   format('     fac1     is the factor ',
     + 'associated with this file,')
       write(6,8950)
8950   format('     jcofile2 is an existing JCO file,')
       write(6,8955)
8955   format('     fac2     is the factor ',
     + 'associated with this file, and')
       write(6,8960)
8960   format('     jcofile3 is a new JCO file holding ',
     + 'summed JCOs.')

       go to 9999

9000   write(errmsg,9010)
9010   format('The two JCO files do not pertain to inversion ',
     + 'problems with the same number of adjustable parameters ',
     + 'and/or observations.')
       go to 9890

9050   write(errmsg,9060)
9060   format('The two JCO files do not cite the same parameters ',
     + 'in the same order.')
       go to 9890

9070   write(errmsg,9080)
9080   format('The two JCO files do not cite the same observations ',
     + 'in the same order.')
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

9400   write(errmsg,9410) trim(afile)
9410   format('Cannot write to file ',a,'.')
       go to 9890

9890   call write_message(errmsg)
9900   continue

       deallocate(x1,x2,stat=ierr)
       deallocate(apar,aobs,stat=ierr)

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
