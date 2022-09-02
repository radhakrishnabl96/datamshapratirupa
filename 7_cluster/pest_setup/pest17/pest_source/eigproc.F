C     Last change:  JD    9 Apr 2004    1:04 pm
       program eigproc

C -- Program EIGPROC processes eigendata on a PEST-generated dataset.

       implicit none
       integer   ::  i,ipar,ifail,ierr,nadjpar,neig,iline,n,
     +              ieig,num,j,bigind,isen,lda,nb1,nb2,n1
       integer   ::  lw(5),rw(5)
       integer, allocatable :: itempvec(:),curreigind(:)
       real      ::  veccomp,curreigval,rtemp
       real, allocatable    :: correl(:,:), eigmat(:,:),eigval(:),
     +                        curreigvec(:),tempvec(:),sen(:),
     +                        tcorrel(:,:)
       character*10   :: aversion
       character*12   :: aapar,aline,aext
       character*15   :: atemp
       character*200  :: casename,recfile,senfile,outfile,afile,aofile
       character*300  :: amessage,cline
       character*12, allocatable :: apar(:),atpar(:),btpar(:)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(' EIGPROC Version ',a,'. Watermark Numerical Computing.')
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
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       casename=cline(lw(1):rw(1))
       call remchar(casename,char(211))
       n=len_trim(casename)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=casename(n1:n)
       call lowcase(aext)
       if(aext.eq.'.pst')casename(n1:n)=' '

       call intread(ifail,cline(lw(2):rw(2)),neig)
       if(ifail.ne.0) go to 8900
       if(neig.le.0) go to 8900
       call realread(ifail,cline(lw(3):rw(3)),veccomp)
       if(ifail.ne.0) go to 8900
       write(6,*)
       if((veccomp.lt.0.0).or.(veccomp.ge.1.0)) then
         write(amessage,20)
20       format(' Error: exlim must be greater than or equal to 0.0 ',
     +   'and less than 1.0.')
         go to 9800
       end if


       outfile=cline(lw(4):rw(4))
       call remchar(outfile,char(211))
       call addquote(outfile,aofile)
       open(unit=20,file=outfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,30) trim(aofile)
30       format(' Cannot open file ',a,'.')
         go to 9800
       end if

C -- The PEST run record file is opened and read.

       recfile=trim(casename)//'.rec'
       call addquote(recfile,afile)
       open(unit=10,file=recfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,250) trim(afile)
250      format(' Cannot open run record file ',a)
         go to 9800
       end if
       iline=0
       do i = 1,30
         iline=iline+1
         read(10,'(a)',end=9000) cline
         if(cline.eq.' ') cycle
         if(index(cline,'of adjustable parameters').ne.0) go to 300
       end do
       write(amessage,258) trim(afile)
258    format(' Cannot read number of adjustable parameters from ',
     + 'file ',a)
       go to 9800
300    n=index(cline,':')
       cline=cline(n+1:)
       cline=adjustl(cline)
       call intread(ifail,cline(1:20),nadjpar)
       if(ifail.ne.0)then
         call writint(aline,iline)
         write(amessage,320) trim(aline),trim(afile)
320      format(' Cannot read number of adjustable parameters from ',
     +   'line ',a,' of file ',a)
         go to 9800
       end if

       allocate(correl(nadjpar,nadjpar),eigmat(nadjpar,nadjpar),
     + eigval(nadjpar),apar(nadjpar),curreigvec(nadjpar),
     + curreigind(nadjpar),tempvec(nadjpar),itempvec(nadjpar),
     + sen(nadjpar),atpar(nadjpar),tcorrel(nadjpar,nadjpar),
     + btpar(nadjpar),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,330)
330      format(' Cannot allocate sufficient memory to continue ',
     +   'execution.')
         go to 9800
       end if
       lda=nadjpar

C -- Next we will try to read the correlation coefficient matrix.

       do
         read(10,'(a)',end=400) cline
         if(index(cline,'correlation coefficient matrix').ne.0)
     +   go to 450
       end do
400    write(amessage,410) trim(afile)
410    format(' A correlation coefficient matrix cannot be found ',
     + 'in run record file ',a)
       go to 9800

450    continue
       read(10,*,err=9050,end=9000)
       read(10,*,err=9050,end=9000) (apar(ipar),ipar=1,nadjpar)
       do ipar=1,nadjpar
         call lowcase(apar(ipar))
       end do
       do ipar=1,nadjpar
         read(10,*,err=9100,end=9000) aapar,(correl(i,ipar),i=1,nadjpar)
       end do

C -- Next we will try to read the eigenvectors.

       do
         read(10,'(a)',end=490) cline
         if(index(cline,'eigenvector').ne.0) go to 500
       end do
490    write(amessage,495) trim(afile)
495    format(' A matrix of eigenvectors cannot be found ',
     + 'in run record file ',a)
       go to 9800

500    continue
       read(10,*,err=9150,end=9000)
       read(10,*,err=9150,end=9000) (aapar,ipar=1,nadjpar)
       do ipar=1,nadjpar
         read(10,*,err=9150,end=9000) aapar,(eigmat(i,ipar),i=1,nadjpar)
       end do

C -- And now the eigenvalues.

       do
         read(10,'(a)',end=520) cline
         if(index(cline,'Eigenvalue').ne.0) go to 550
       end do
520    write(amessage,530) trim(afile)
530    format(' A list of eigenvalues cannot be found ',
     + 'in run record file ',a)
       go to 9800

550    continue
       read(10,*,err=9200,end=9000) (eigval(ipar),ipar=1,nadjpar)


       close(unit=10)
       write(amessage,560) trim(afile)
560    format(' - file ',a,' read ok.')
       call writmess(6,amessage)

C -- The sensitivity file is now read.

       isen=1
       senfile=trim(casename)//'.sen'
       call addquote(senfile,afile)
       open(unit=10,file=senfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(6,*)
         write(amessage,1020) trim(afile)
1020     format(' Cannot find sensitivity file ',a)
         call writmess(6,amessage)
         write(amessage,1030) trim(aofile)
1030     format(' Sensitivity information will thus not be included ',
     +   'in output file ',a)
         call writmess(6,amessage)
         isen=0
         close(unit=10,iostat=ierr)
         go to 1500
       end if
       do
         read (10,'(a)',end=1100) cline
         if(index(cline,'COMPLETION OF OPTIMISATION').ne.0) go to 1200
       end do
1100   write(6,*)
       write(amessage,1110) trim(afile)
1110   format(' Sensitivity file ',a,' does not record completion ',
     +' sensitivities, or they are in error.')
       call writmess(6,amessage)
       write(amessage,1030) trim(aofile)
       call writmess(6,amessage)
       isen=0
       close(unit=10)
       go to 1500
1200   do
         read (10,'(a)',end=1100) cline
         if(index(cline,'all observations').ne.0) go to 1300
       end do
       go to 1100
1300   do i=1,3
         read(10,*,end=1100)
       end do
       do i=1,nadjpar
         read(10,*,end=1100,err=1100) aapar,aapar,rtemp,sen(i)
       end do
       close(unit=10)
       write(amessage,560) trim(afile)
       call writmess(6,amessage)
1500   continue


C -- The output file is now written - with each eigenvalue processed in turn,
C    starting from the highest.

       write(20,1320)
1320   format(/,' EIGENVALUE SUMMARY')
       if(neig.gt.nadjpar)neig=nadjpar
       write(20,1330) neig
1330   format(' Number of highest eigenvalues analysed:',i4)
       write(20,1340) veccomp
1340   format(' Eigenvector component exclusion limit : ',f6.3)

       do 1000 ieig=nadjpar,nadjpar-neig+1,-1

C --  Eigenvectors which exceed the limit (and their indices) are put into
C     a special vector.

         curreigval=eigval(ieig)
         num=0
         do i=1,nadjpar
           if(abs(eigmat(ieig,i)).ge.veccomp)then
             num=num+1
             curreigvec(num)=eigmat(ieig,i)
             curreigind(num)=i
           end if
         end do

         write(20,570) ieig,curreigval
570      format(/,/,/,' Eigenvalue number:',i4,'    Value = ',1pg12.5,
     +   ' -------->')
         if(num.eq.0)then
           write(20,580) veccomp
580        format(/,' There are no eigenvector components above ',
     +     f6.4,' for this eigenvalue.')
           go to 1000
         end if

C -- The eigenvector components are now sorted in order of decreasing magnitude.

         do j=1,num
           rtemp=0.0
           do i=1,num
             if(abs(curreigvec(i)).gt.rtemp)then
               rtemp=abs(curreigvec(i))
               bigind=i
             end if
           end do
           tempvec(j)=curreigvec(bigind)
           itempvec(j)=curreigind(bigind)
           curreigvec(bigind)=0.0
         end do
         do i=1,num
           curreigvec(i)=tempvec(i)
           curreigind(i)=itempvec(i)
         end do

         write(20,600)
600      format(/,' Parameter',t15,'Eigenvector',t32,'Sensitivity')
         write(20,605)
605      format(t15,'component')
         do i=1,num
           if(isen.eq.1)then
             write(atemp,'(1pg10.3)') sen(curreigind(i))
           else
             atemp='  ---'
           end if
           write(20,610) trim(apar(curreigind(i))),curreigvec(i),
     +     trim(atemp)
610        format(t3,a,t17,1pg10.3,t32,a,t47,1000(1pg10.3,3x))
         end do
         do i=1,num
           atpar(i)=apar(curreigind(i))
           nb1=len_trim(atpar(i))
           nb2=10-nb1
           if(nb2.gt.1)then
             nb2=nb2/2
             if(nb2.lt.1)nb2=1
             btpar(i)=' '
             btpar(i)(1+nb2:12)=atpar(i)(1:nb1)
           else
             btpar(i)=atpar(i)
           end if
           do j=1,num
             tcorrel(j,i)=correl(curreigind(j),curreigind(i))
           end do
         end do
         write(20,615)
615      format(/,' Correlation coefficient matrix for these ',
     +   'parameters:-')
         call writhed(ifail,20,8,num,btpar)
         call writarr(ifail,lda,20,8,num,tcorrel,atpar)
1000   continue


       call addquote(outfile,afile)
       write(amessage,616) trim(afile)
616    format(' - file ',a,' written ok.')
       go to 9800

8900   continue

       write(6,8910)
8910   format(/,' EIGPROC is run using the command:',/)
       write(6,8920)
8920   format('     eigproc casename N exlim outfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8950)
8950   format('     casename  pertains to a case for which a ',
     + 'PEST run is complete,')
       write(6,8960)
8960   format('     N         is the number of eigenvalues ',
     + 'to be processed,')
       write(6,8970)
8970   format('     exlim     is the eigencomponent ',
     + 'exclusion limit, and')
       write(6,8980)
8980   format('     outfile   is the EIGPROC output file.')

       go to 9999

9000   write(amessage,9010) trim(afile)
9010   format(' Premature end encountered to file ',a)
       go to 9800
9050   write(amessage,9060) trim(afile)
9060   format(' Error reading header to correlation coefficient ',
     + 'matrix from file ',a)
       go to 9800
9100   write(amessage,9110) trim(afile)
9110   format(' Error reading correlation coefficient matrix from ',
     + 'file ',a)
       go to 9800
9150   write(amessage,9160) trim(afile)
9160   format(' Error reading eigenvector matrix or eigenvalues from ',
     + 'file ',a)
       go to 9800
9200   write(amessage,9210) trim(afile)
9210   format(' Error reading eigenvalues from ',
     + 'file ',a)
       go to 9800

9800   call writmess(6,amessage)
9900   deallocate(correl,eigval,eigmat,apar,curreigvec,curreigind,
     + tempvec,itempvec,sen,atpar,tcorrel,btpar,stat=ierr)


9999   continue

       end program



        SUBROUTINE WRITHED(IFAIL,IUNIT,NUM,NDIM,AHEAD)

        INTEGER NUM,NDIM,J1,J2,IUNIT,IFAIL,I,J
        CHARACTER*(*) AHEAD(NDIM)

        IFAIL=0
        J1=1
        J2=NUM

        IF(NDIM.GE.NUM)THEN
          DO 30 I=1,NDIM/NUM
            WRITE(IUNIT,20,ERR=9000) (AHEAD(J),J=J1,J2)
20          FORMAT(1X,13X,8(A12,1X))
            J1=J1+8
            J2=J2+8
30        CONTINUE
        END IF
        IF(NDIM.GE.J1)WRITE(IUNIT,20,ERR=9000) (AHEAD(J),J=J1,NDIM)
        IF(NDIM.GT.NUM) WRITE(IUNIT,*,ERR=9000)
        RETURN

9000    IFAIL=1
        RETURN

        END


        SUBROUTINE WRITARR(IFAIL,LDA,IUNIT,NUM,NDIM,RARRAY,ANAME)

        INTEGER NUM,NDIM,J1,J2,IUNIT,IFAIL,I,J,II,LDA,IFLAG,NBLNK
        real RARRAY(LDA,NDIM)
        CHARACTER*(*) ANAME(NDIM)

        IFAIL=0

        DO 50 II=1,NDIM
          J1=1
          J2=NUM
          IFLAG=1
          IF(NDIM.GE.NUM)THEN
            DO 30 I=1,NDIM/NUM
              IF(IFLAG.EQ.1)THEN
                WRITE(IUNIT,20,ERR=9000)
     +          ANAME(II),(RARRAY(II,J),J=J1,J2)
20              FORMAT(1X,A12,1X,8(2X,1PG9.2,2X))
                IFLAG=0
              ELSE
                WRITE(IUNIT,21,ERR=9000) (RARRAY(II,J),J=J1,J2)
21              FORMAT(1X,12X,1X,8(2X,1PG9.2,2X))
              END IF
              J1=J1+8
              J2=J2+8
30          CONTINUE
          END IF
          IF(NDIM.GE.J1)THEN
            IF(IFLAG.EQ.1)THEN
              WRITE(IUNIT,20,ERR=9000)
     +        ANAME(II),(RARRAY(II,J),J=J1,NDIM)
              IFLAG=0
            ELSE
              WRITE(IUNIT,21,ERR=9000) (RARRAY(II,J),J=J1,NDIM)
            END IF
          END IF
          IF(NDIM.GT.NUM) WRITE(IUNIT,*,ERR=9000)
50      CONTINUE
        RETURN

9000    IFAIL=1
        RETURN

        END


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

