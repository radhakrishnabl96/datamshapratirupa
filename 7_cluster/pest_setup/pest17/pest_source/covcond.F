       include 'matman.F'

       program covcond

C -- Program COVCOND calculates a conditional covariance matrix from a full
C    covariance matrix.

       use matrix_manipulate
       implicit none

       integer              :: npar,i,npar2,j,k,l,info,npar1,i1,
     +                         j1,j2,icode,itemp,ifail,ierr
       integer              :: lw(10),rw(10)
       double precision     :: rtemp,w1(2)
       character*12         :: aversion
       character*20         :: atemp1
       character*200        :: covfile,listfile,outfile,afile,bfile
       character*300        :: cline
       character*600        :: amessage

       integer, allocatable          :: status(:)
       double precision, allocatable :: c22(:,:),c12(:,:),cc(:,:)
       type(dmatrix)                 :: covmat

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' COVCOND version ',a,'. Watermark Numerical ',
     + 'Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,4,lw,lw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       covfile=cline(lw(1):rw(1))
       call remchar(covfile,char(211))
       listfile=cline(lw(2):rw(2))
       call remchar(listfile,char(211))
       outfile=cline(lw(3):rw(3))
       call remchar(outfile,char(211))
#ifndef UNIX
       call lowcas(covfile)
       call lowcas(listfile)
       call lowcas(outfile)
#endif

C -- The matrix is read.

       call mat_read(ifail,10,covmat,covfile,amessage,cline)
       if(ifail.ne.0) go to 9890
       call addquote(covfile,afile)
       npar=covmat%nrow
       itemp=covmat%ncol
       if(npar.ne.itemp)then
         write(amessage,60) trim(afile)
60       format(' The matrix contained in file ',a,' is not square.')
         go to 9890
       end if
       write(6,*)
       write(6,50) trim(afile)
50     format(' - file ',a,' read ok.')

C -- Some memory is allocated.

       allocate(status(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       status=0            ! An array

C -- The list file is read.

       call addquote(listfile,afile)
       open(unit=10,file=listfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,70) trim(afile)
70       format(' Cannot open list file ',a,'.')
         go to 9890
       end if
       npar2=0
       j=1
       do
         read(10,*,end=75) atemp1
         atemp1=adjustl(atemp1)
         call lowcas(atemp1)
         call which1(ifail,npar,j,covmat%arow,atemp1)
         if(ifail.ne.0)then
           call addquote(covfile,bfile)
           write(amessage,80) trim(atemp1),trim(afile),trim(bfile)
80         format(' Variable "',a,'" listed in file ',a,' is not ',
     +     'cited in covariance matrix file ',a,'.')
           go to 9890
         else
           if(status(j).ne.0)then
             write(amessage,90) trim(atemp1),trim(afile)
90           format(' Variable "',a,'" is cited in listing ',
     +       'file ',a,' more than once.')
             go to 9890
           end if
           status(j)=1
         end if
         npar2=npar2+1
       end do
75     continue
       close(unit=10)
       if(npar2.eq.0)then
         write(amessage,76) trim(afile)
76       format(' No parameters are listed in file ',a,'.')
         go to 9890
       else if(npar2.eq.npar)then
         call addquote(covfile,bfile)
         write(amessage,95) trim(afile),trim(bfile)
95       format(' List file ',a,' lists every variable cited ',
     +   'in covariance matrix file ',a,'. Why would you want to do ',
     +   'this?')
         go to 9890
       end if
       write(6,50) trim(listfile)

C -- The number of elements in the new covariance matrix is calculated.
C    Memory is then allocated.

       npar1=npar-npar2
       icode=covmat%icode

C -- First we handle the case where the covariance matrix is diagonal.

       if(covmat%icode.eq.-1) go to 8000

C -- We form a matrix of removed components.

       allocate(c22(npar2,npar2),c12(npar1,npar2),cc(npar1,npar2),
     + stat=ierr)
       if(ierr.ne.0) go to 9200
       j=0
       do i=1,npar
         if(status(i).eq.1)then
           j=j+1
           k=0
           do l=1,npar
             if(status(l).eq.1)then
               k=k+1
               c22(j,k)=covmat%array(i,l)
             end if
           end do
         end if
       end do

       write(6,190)
190    format(/,' - inverting C22 matrix....')

C -- Now we find the inverse of this matrix.

       call dpofa(c22,npar2,npar2,info)
       if(info.ne.0)then
         call addquote(covfile,bfile)
         write(amessage,230) trim(bfile)
230      format(' Cannot invert C22 matrix. Matrix supplied in ',
     +   'file ',a,' is not positive definite.')
         go to 9890
       end if
       info=1
       call dpodi(c22,npar2,npar2,w1,info)
       do i=1,npar2-1
         do j=i+1,npar2
           c22(j,i)=c22(i,j)
         end do
       end do

C -- The C12 matrix is filled.

       i1=0
       do i=1,npar
         if(status(i).eq.1)cycle
         i1=i1+1
         j2=0
         do j=1,npar
           if(status(j).eq.0) cycle
           j2=j2+1
           c12(i1,j2)=covmat%array(i,j)
         end do
       end do

C -- The C11 matrix is filled.

       i1=0
       do i=1,npar
         if(status(i).eq.1) cycle
         i1=i1+1
         j1=0
         do j=1,npar
           if(status(j).eq.1)cycle
           j1=j1+1
           covmat%array(i1,j1)=covmat%array(i,j)
         end do
       end do

C -- Now we do the matrix multiplication

       write(6,159)
159    format(' - calculating conditional covariance matrix....')
       do i=1,npar1
         do j=1,npar2
           rtemp=0.0d0
           do k=1,npar2
             rtemp=rtemp+c12(i,k)*c22(k,j)
           end do
           cc(i,j)=rtemp
         end do
       end do
       do i=1,npar1
         do j=1,npar1
           rtemp=0.0d0
           do k=1,npar2
             rtemp=rtemp+cc(i,k)*c12(j,k)
           end do
           covmat%array(i,j)=covmat%array(i,j)-rtemp
         end do
       end do

8000   continue

C -- Now we write the revised covariance matrix to file.
C    Note that we do not do this using the normal matrix module subroutine
C    because we have stored the new matrix in the old structure.

       write(6,8010)
8010   format(' - writing matrix output file....')
       open(unit=20,file=outfile)
       write(20,8020) npar1,npar1,icode
8020   format(3i10)
       if(icode.eq.-1)then
         do i=1,npar
           if(status(i).eq.0) write(20,8030) covmat%vector(i)
         end do
       else
         do i=1,npar1
           write(20,8030) (covmat%array(i,j),j=1,npar1)
8030       format(7(1x,1pg14.7))
         end do
       end if

       write(20,8040)
8040   format('* row and column names')
       do i=1,npar
         if(status(i).eq.0) write(20,8050) trim(covmat%arow(i))
8050     format(a)
       end do
       close(unit=20)
       call addquote(outfile,afile)
       write(6,8060) trim(afile)
8060   format(' - file ',a,' written ok.')

       go to 9990

8900   continue
       write(6,8910)
8910   format(/,' COVCOND is run using the command:',/)
       write(6,8920)
8920   format('     covcond covfile listfile condfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     covfile  is a matrix file containing a ',
     + 'covariance matrix,')
       write(6,8950)
8950   format('     listfile contains a list of conditioning ',
     + 'variables, and')
       write(6,8960)
8960   format('     condfile will contain the conditional ',
     + 'covariance matrix.')
       go to 9990

9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9890   continue
       call writmess(6,amessage)

9990   continue
       deallocate(status,c22,c12,cc,stat=ierr)
       call mat_deallocate(ifail,covmat)

       end



        SUBROUTINE WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)

C -- SUBROUTINE WHICH1 LOCATES A STRING IN AN ARRAY

        INTEGER NPAR,IPAR,I
        INTEGER IFAIL
        CHARACTER*(*) TPAR
        CHARACTER*(*) APAR(NPAR)

        IFAIL=0
        IF((IPAR.LT.1).OR.(IPAR.GT.NPAR)) IPAR=1
        CALL LOWCAS(TPAR)
        IF(TPAR.EQ.APAR(IPAR)) RETURN
        IF(IPAR.NE.NPAR)THEN
          DO 20 I=IPAR+1,NPAR
          IF(TPAR.EQ.APAR(I))THEN
            IPAR=I
            RETURN
          END IF
20        CONTINUE
        END IF
        IF(IPAR.NE.1)THEN
          DO 40 I=IPAR-1,1,-1
          IF(TPAR.EQ.APAR(I)) THEN
            IPAR=I
            RETURN
          END IF
40        CONTINUE
        END IF
        IFAIL=1
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



