        program dercomb1

C -- Program DERCOMB1 combines the contents of a PEST external derivatives file with that
C    of a matrix file, to compute a new external derivatives file.


        implicit none

        integer       ifail,ierr
        integer       nb_pst,i
        integer       npar,nobs,npargp,nprior,nobsgp,ipar,iobs,jpar,
     +                jobs,nnpar,nnobs
        integer       icode,nrow,ncol,irow,icol,jrow,jcol,icompress,
     +                jcompress

        integer       lw(6),rw(6)

        double precision rtemp

        character*4   aext
        character*12  aversion
        character*12  aapar
        character*30  aheader
        character*200 pestfile,matfile,derfile1,derfile2,
     +  tempfile
        character*200 afile,bfile
        character*500 cline
        character*600 errmsg

        integer, allocatable          :: indcol(:),indrow(:)
        integer, allocatable          :: indobs(:),indpar(:)
        double precision, allocatable :: jac(:)
        double precision, allocatable :: mat(:,:)
        character*12, allocatable     :: apar(:)
        character*20, allocatable     :: aobs(:)
        character*20, allocatable     :: arow(:),acol(:)

C -- Initialization

        tempfile='t###.###'

#ifdef NO_CMDLINE
        write(*,*)
1       write(*,'(A)') ' ENTER COMMAND LINE ARGUMENTS > '
        read(*,'(A)',err=1) cline
#else
        call pgetcl (cline)
#endif        

        include 'version.inc'
        write(6,10) trim(aversion)
10      format(/,' DERCOMB1 Version ',a,'. Watermark Numerical ',
     +  'Computing.')

        call spacesub(cline)
        call linspl(ifail,5,lw,rw,cline)
        if(ifail.eq.0) go to 9400
        call linspl(ifail,4,lw,rw,cline)
        if(ifail.ne.0) go to 9400
        pestfile=cline(lw(1):rw(1))
        matfile=cline(lw(2):rw(2))
        derfile1=cline(lw(3):rw(3))
        derfile2=cline(lw(4):rw(4))
        call remchar(pestfile,char(211))
        call remchar(matfile,char(211))
        call remchar(derfile1,char(211))
        call remchar(derfile2,char(211))

#ifndef UNIX
        call lowcas(pestfile)
        call lowcas(matfile)
        call lowcas(derfile1)
        call lowcas(derfile2)
#endif

C -- The PEST control file is opened.

        nb_pst=len_trim(pestfile)
        nb_pst=nb_pst-3
        if(nb_pst.lt.1)nb_pst=1
        aext=pestfile(nb_pst:)
        call lowcas(aext)
        if(aext.ne.'.pst')then
          write(errmsg,40)
40        format('PEST control file must have an extension of ',
     +    '".pst".')
          go to 9890
        end if
        open(unit=10,file=pestfile,status='old',iostat=ierr)
        if(ierr.ne.0)then
          call addquote(pestfile,afile)
          write(errmsg,50) trim(afile)
50        format('Cannot open PEST control file ',a,'.')
          go to 9890
        end if

C -- The matrix file is opened.

        open(unit=11,file=matfile,status='old',iostat=ierr)
        if(ierr.ne.0)then
          call addquote(matfile,afile)
          write(errmsg,60) trim(afile)
60        format('Cannot open matrix file ',a,'.')
          go to 9890
        end if

C -- The existing derivatives file is opened.

        open(unit=12,file=derfile1,status='old',iostat=ierr)
        if(ierr.ne.0)then
          call addquote(derfile1,afile)
          write(errmsg,70) trim(afile)
70        format('Cannot open external derivatives file ',a,'.')
          go to 9890
        end if

C -- The new derivatives file is opened.

        open(unit=20,file=derfile2,action='write',iostat=ierr)
        if(ierr.ne.0)then
          call addquote(derfile2,afile)
          write(errmsg,75) trim(afile)
75        format('Cannot open file ',a,' for writing.')
          go to 9890
        end if

C -- The PEST control file is now read to obtain parameter and observation names.

        call addquote(pestfile,afile)
        write(6,80) trim(afile)
80      format(/,' - reading PEST control file ',a,'...')
        read(10,*,err=9000,end=9000) aheader
        aheader=adjustl(aheader)
        call lowcas(aheader)
        if(aheader(1:3).ne.'pcf') go to 9000
        read(10,'(a)',err=9000,end=9000) cline
        cline=adjustl(cline)
        call lowcas(cline)
        if(cline(1:14).ne.'* control data') go to 9000
        read(10,'(a)',err=9000,end=9000) cline
        read(10,*,err=9000,end=9000) npar,nobs,npargp,nprior,nobsgp
        allocate(apar(npar),aobs(nobs),stat=ierr)
        if(ierr.ne.0) go to 9200

        do
          read(10,'(a)',err=9000,end=100) cline
          call lowcas(cline)
          if(index(cline,'* parameter da').ne.0) go to 120
        end do
100     write(errmsg,110) trim(afile)
110     format('Cannot find "parameter data" section of PEST control ',
     +  'file ',a,'.')
        go to 9890
120     continue
        do ipar=1,npar
          read(10,*,err=9000,end=9000) apar(ipar)
          call lowcas(apar(ipar))
        end do

        do
          read(10,'(a)',err=9000,end=140) cline
          call lowcas(cline)
          if(index(cline,'* observation da').ne.0) go to 160
        end do
140     write(errmsg,150) trim(afile)
150     format('Cannot find "observation data" section of PEST control ',
     +  'file ',a,'.')
        go to 9890
160     do iobs=1,nobs
          read(10,*,err=9000,end=9000) aobs(iobs)
          call lowcas(aobs(iobs))
        end do
        close(unit=10)
        write(6,170) trim(afile)
170     format(' - file ',a,' read ok.')

C -- The matrix file is now read.

        call addquote(matfile,afile)
        write(6,171) trim(afile)
171     format(/,' - reading matrix file ',a,'...')
        read(11,'(a)',iostat=ierr) cline
        if(ierr.ne.0)then
          write(errmsg,310) trim(afile)
310       format('Error encountered when reading first line of matrix ',
     +    'file ',a,'.')
          go to 9890
        end if
        call linspl(ifail,3,lw,rw,cline)
        if(ifail.ne.0)then
          write(errmsg,310) trim(afile)
          go to 9890
        end if
        call intread(ifail,cline(lw(1):rw(1)),nrow)
        if(ifail.ne.0)then
          write(errmsg,310) trim(afile)
          go to 9890
        end if
        call intread(ifail,cline(lw(2):rw(2)),ncol)
        if(ifail.ne.0)then
          write(errmsg,310) trim(afile)
          go to 9890
        end if
        call intread(ifail,cline(lw(3):rw(3)),icode)
        if(ifail.ne.0)then
          write(errmsg,310) trim(afile)
          go to 9890
        end if
        if((nrow.le.0).or.(ncol.le.0))then
          write(errmsg,310) trim(afile)
          go to 9890
        end if
        if((icode.ne.-1).and.(icode.ne.1).and.(icode.ne.2))then
          write(errmsg,311) trim(afile)
311       format('ICODE must be "-1", "1" or "2" on first line of ',
     +    'matrix file ',a,'.')
          go to 9890
        end if
        if(abs(icode).eq.1)then
          if(ncol.ne.nrow)then
            write(errmsg,312) trim(afile)
312         format('ICODE on first line of file ',a,' must be "2" ',
     +      'unless NCOL is equal to NROW.')
            go to 9890
          end if
        end if
        allocate(mat(nrow,ncol),stat=ierr)
        if(ierr.ne.0) go to 9200
        allocate(arow(nrow),acol(ncol),stat=ierr)
        if(ierr.ne.0) go to 9200
        if(icode.ne.-1)then
          do irow=1,nrow
            read(11,*,err=9300,end=9350) (mat(irow,icol),icol=1,ncol)
          end do
        else
          mat=0.0d0         ! an array
          do irow=1,nrow
            read(11,*,err=9300,end=9350) mat(irow,irow)
          end do
        end if
        read(11,'(a)',err=9500,end=9500) aheader
        call lowcas(aheader)
        if(abs(icode).eq.1)then
          if(index(aheader,'row and column').eq.0) go to 9500
        else
          if(index(aheader,'row name').eq.0) go to 9500
        end if
        do irow=1,nrow
          read(11,*,err=9600,end=9600) arow(irow)
          call lowcas(arow(irow))
        end do
        if(abs(icode).eq.1)then
          do icol=1,ncol
            acol(icol)=arow(icol)
          end do
        else
          read(11,'(a)',err=9550,end=9550) aheader
          call lowcas(aheader)
          if(index(aheader,'* column nam').eq.0) go to 9550
          do icol=1,ncol
            read(11,*,err=9650,end=9650) acol(icol)
            call lowcas(acol(icol))
          end do
        end if
        close(unit=11)

C -- Matrix rows and columns are alligned with observations and rows.

        allocate(indcol(ncol),indrow(nrow),stat=ierr)
        if(ierr.ne.0) go to 9200
        jpar=1
        do icol=1,ncol
          aapar=acol(icol)
          call whichone(ifail,npar,jpar,apar,aapar)
          if(ifail.ne.0)then
            call addquote(pestfile,afile)
            write(errmsg,410) trim(afile),trim(acol(icol))
410         format('No parameter name cited in PEST control file ',a,
     +      ' corresponds to matrix column name ',a,
     +      ' cited in matrix file.')
            go to 9890
          end if
          indcol(icol)=jpar
        end do
        jobs=1
        do irow=1,nrow
          call whichone(ifail,nobs,jobs,aobs,arow(irow))
          if(ifail.ne.0)then
            call addquote(pestfile,afile)
            write(errmsg,420) trim(afile),trim(arow(irow))
420         format('No observation name cited in PEST control file ',a,
     +      ' corresponds to matrix row name ',a,
     +      ' cited in matrix file.')
            go to 9890
          end if
          indrow(irow)=jobs
        end do
        write(6,430) trim(afile)
430     format(' - file ',a,' read ok.')

C -- The external derivatives file is now read and a new one re-written.

        call addquote(derfile1,afile)
        write(6,200)
200     format(/,' - reading and writing external derivatives files...')
        read(12,'(a)',err=9100,end=9100) cline
        call linspl(ifail,2,lw,rw,cline)
        if(ifail.ne.0) go to 9100
        call intread(ifail,cline(lw(1):rw(1)),nnpar)
        if(ifail.ne.0) go to 9100
        call intread(ifail,cline(lw(2):rw(2)),nnobs)
        if(ifail.ne.0) go to 9100
        if((nnpar.ne.npar).or.(nnobs.ne.nobs))then
          call addquote(pestfile,bfile)
          write(errmsg,210) trim(afile),trim(bfile)
210       format('Number of parameters and observations cited ',
     +    'on first line of file ',a,' does not agree with that ',
     +    'provided in PEST control file ',a,'.')
          go to 9890
        end if
        call linspl(ifail,3,lw,rw,cline)
        if(ifail.ne.0)then
          icompress=0
        else
          call intread(ifail,cline(lw(3):rw(3)),icompress)
          if(ifail.ne.0) then
            icompress=0
          else if(icompress.lt.0) then
            go to 9100
          end if
        end if

        if(icompress.eq.0)then
          write(20,211) npar,nobs
211       format(2i10)
          allocate(jac(npar),stat=ierr)
          if(ierr.ne.0) go to 9200
          jrow=1
          do iobs=1,nobs
            read(12,*,err=9150,end=9150) (jac(ipar),ipar=1,npar)
            call whichone_i(ifail,nrow,jrow,indrow,iobs)
            if(ifail.ne.0)then
              continue
            else
              do icol=1,ncol
                jac(indcol(icol))=mat(jrow,icol)
              end do
            end if
            write(20,215) (jac(ipar),ipar=1,npar)
215         format(8(1x,1pg14.7))
          end do
        else
          jcompress=0
          open(unit=21,file=tempfile,action='write',iostat=ierr)
          if(ierr.ne.0)then
            write(errmsg,240) trim(tempfile)
240         format('Cannot open temporary file ',a,' for writing.')
            go to 9890
          end if
          allocate(indobs(nobs),indpar(npar),stat=ierr)
          if(ierr.ne.0) go to 9200
          jrow=1
          do iobs=1,nobs
            call whichone_i(ifail,nrow,jrow,indrow,iobs)
            if(ifail.eq.0)then
              indobs(iobs)=jrow
            else
              indobs(iobs)=-1
            end if
          end do
          jcol=1
          do ipar=1,npar
            call whichone_i(ifail,ncol,jcol,indcol,ipar)
            if(ifail.eq.0)then
              indpar(ipar)=jcol
            else
              indpar(ipar)=-1
            end if
          end do
          do i=1,icompress
            read(12,*,err=9150,end=9150) ipar,iobs,rtemp
            if((indpar(ipar).gt.0).and.(indobs(iobs).gt.0))then
              rtemp=mat(indobs(iobs),indpar(ipar))
              mat(indobs(iobs),indpar(ipar))=-1.0d300
            end if
            if(rtemp.ne.0.0d0)then
              write(21,216) ipar,iobs,rtemp
216           format(i10,i10,1x,1pg14.7)
              jcompress=jcompress+1
            end if
          end do
          do icol=1,ncol
            do irow=1,nrow
              if(mat(irow,icol).gt.-1.0d299)then
                if(mat(irow,icol).ne.0.0d0)then
                  write(21,216) indcol(icol),indrow(irow),mat(irow,icol)
                  jcompress=jcompress+1
                end if
              end if
            end do
          end do
          close(unit=21)
          write(20,250) npar,nobs,jcompress
250       format(3i10)
          open(unit=21,file=tempfile,status='old')
          do
            read(21,'(a)',end=270) cline
            write(20,'(a)') trim(cline)
          end do
270       close(unit=21,status='delete')
        end if
        close(unit=12)
        close(unit=20)
        write(6,220) trim(derfile1)
220     format(' - file ',a,' read ok.')
        write(6,230) trim(derfile2)
230     format(' - file ',a,' written ok.')

        go to 9900

9000    write(errmsg,9010) trim(afile)
9010    format('Error or premature end encountered in PEST control ',
     +  'file ',a,'; check this file with PESTCHEK.')
        go to 9890
9100    write(errmsg,9110) trim(afile)
9110    format('Error encountered when reading first line of external ',
     +  'derivatives file ',a,'.')
        go to 9890
9150    write(errmsg,9160) trim(afile)
9160    format('Error reading derivatives from file ',a,'.')
        go to 9890
9200    write(errmsg,9210)
9210    format('Cannot allocate sufficient memory to continue ',
     +  'execution.')
        go to 9890
9300    write(errmsg,9310) trim(afile)
9310    format('Error encountered in reading matrix from file ',a,'.')
        go to 9890
9350    write(errmsg,9360) trim(afile)
9360    format('Premature end encountered to matrix file ',a,
     +  ' while reading matrix.')
        go to 9890

9400    WRITE(6,9410)
9410    FORMAT(/,' DERCOMB1 is run using the command:',/)
        WRITE(6,9420)
9420    FORMAT('     DERCOMB1 pestfile matfile derfile1 derfile2',/)
        WRITE(6,9430)
9430    FORMAT(' where',/)
        WRITE(6,9450)
9450    FORMAT('     pestfile is the name of a PEST ',
     +  'control file,')
        WRITE(6,9451)
9451    FORMAT('     matfile  is the name of matrix file,')
        WRITE(6,9452)
9452    FORMAT('     derfile1 is the name of an existing external ',
     +  'derivatives file, and')
        WRITE(6,9453)
9453    FORMAT('     derfile2 is the name of a new external ',
     +  'derivatives file.')
        go to 9900

9500    continue
        if(icode.eq.2)then
          write(errmsg,9510) trim(afile)
9510      format('Error reading "* row names" header following matrix ',
     +    'from matrix file ',a,'.')
          go to 9890
        else
          write(errmsg,9520) trim(afile)
9520      format('Error reading "* row and column names" header ',
     +    'following matrix from matrix file ',a,'.')
          go to 9890
        end if
9550    write(errmsg,9560) trim(afile)
9560    format('Error reading "* column names" header from matrix ',
     +  'file ',a,'.')
        go to 9890
9600    write(errmsg,9610) trim(afile)
9610    format('Error reading row names from matrix file ',a,'.')
        go to 9890
9650    write(errmsg,9660) trim(matfile)
9660    format('Error reading column names from matrix file ',a,'.')
        go to 9890


9890    continue
        errmsg=' '//trim(errmsg)
        call writmess(6,errmsg)

9900    continue

        if(allocated(indcol)) deallocate(indcol,stat=ierr)
        if(allocated(indrow)) deallocate(indrow,stat=ierr)
        if(allocated(indobs)) deallocate(indobs,stat=ierr)
        if(allocated(indpar)) deallocate(indpar,stat=ierr)
        if(allocated(jac)) deallocate(jac,stat=ierr)
        if(allocated(mat)) deallocate(mat,stat=ierr)
        if(allocated(apar)) deallocate(apar,stat=ierr)
        if(allocated(aobs)) deallocate(aobs,stat=ierr)
        if(allocated(arow)) deallocate(arow,stat=ierr)
        if(allocated(acol)) deallocate(acol,stat=ierr)

        close(unit=10,iostat=ierr)
        close(unit=11,iostat=ierr)
        close(unit=12,iostat=ierr)
        close(unit=20,iostat=ierr)
        close(unit=21,iostat=ierr)


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


        subroutine whichone_i(ifail,npar,ipar,ival,jval)

C -- Subroutine whichone_i locates an integer value in an array of integers.

        integer npar,ipar,i
        integer ifail
        integer jval
        integer ival(npar)

        ifail=0
        if((ipar.lt.1).or.(ipar.gt.npar)) ipar=1
        if(jval.eq.ival(ipar)) return
        if(ipar.ne.npar)then
          do 20 i=ipar+1,npar
          if(jval.eq.ival(i))then
            ipar=i
            return
          end if
20        continue
        end if
        if(ipar.ne.1)then
          do 40 i=ipar-1,1,-1
          if(jval.eq.ival(i)) then
            ipar=i
            return
          end if
40        continue
        end if
        ifail=1
        return

        end subroutine whichone_i



        subroutine whichone(ifail,npar,ipar,apar,tpar)

C -- Subroutine whichone locates a string in an array. Note that both the
C    search string and the existing array of strings are assumed to be
C    in the same case.

        integer npar,ipar,i
        integer ifail
        character*(*) tpar
        character*(*) apar(npar)

        ifail=0
        if((ipar.lt.1).or.(ipar.gt.npar)) ipar=1
        if(tpar.eq.apar(ipar)) return
        if(ipar.ne.npar)then
          do 20 i=ipar+1,npar
          if(tpar.eq.apar(i))then
            ipar=i
            return
          end if
20        continue
        end if
        if(ipar.ne.1)then
          do 40 i=ipar-1,1,-1
          if(tpar.eq.apar(i)) then
            ipar=i
            return
          end if
40        continue
        end if
        ifail=1
        return
        end


