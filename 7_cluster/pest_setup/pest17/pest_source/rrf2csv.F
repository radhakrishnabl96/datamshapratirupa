       program rrf2csv

C -- Program RRF2CSV lists the contents of an RRF file in a CSV file.

       implicit none

       integer ierr,iline,iline_end_of_header,itemp,ifail
       integer iwarn_nonseq,iwarn_fail
       integer npar,nobs,ipar,iobs
       integer ireject,iset,numset
       integer lw(10),rw(10)
       real    ppval,ooval
       character*10 aline,atype,anum
       character*12 aversion
       character*40 asection
       character*256 rrffile,outfile1,outfile2
       character*256 qrrffile,qoutfile1,qoutfile2
       character*256 parsource
       character*500 cline
       character*1500 errmsg

       integer, allocatable :: parsetindex(:)
       real,    allocatable :: pval(:,:),oval(:,:)
       character*12, allocatable :: apar(:)
       character*16, allocatable :: aval(:)
       character*20, allocatable :: aobs(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' RRF2CSV Version ',a,
     + '. Watermark Numerical Computing.')

#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) cline
#else
       call pgetcl(cline)
#endif

C -- The RRF2CSV command line is read.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       rrffile=cline(lw(1):rw(1))
       call remchar(rrffile,char(211))
       outfile1=cline(lw(2):rw(2))
       call remchar(outfile1,char(211))
       outfile2=cline(lw(3):rw(3))
       call remchar(outfile2,char(211))

C -- Initialization

       iwarn_nonseq=0        ! Set to 1 if parameter set indices are non-sequential
       iwarn_fail=0          ! Set to 1 if there is a failed model run
       numset=0
       call addquote(rrffile,qrrffile)
       call addquote(outfile1,qoutfile1)
       call addquote(outfile2,qoutfile2)
       write(6,*)

C -- The existing run results file is opened and the number of parameters ascertained.

       open(unit=10,file=rrffile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,20) trim(qrrffile)
20       format('Cannot open run results file ',a,'.')
         go to 9890
       end if

C -- The first two lines of the header to the first file is read.

       iline=0

C -- Information is obtained from the header to the first file.

       asection='* case dimensions'
       iline=iline+1
       read(10,'(a)',err=9000,end=9050) cline
       call lowcas(cline)
       if(index(cline,trim(asection)).eq.0) then
         call writint(aline,iline)
         write(errmsg,153) trim(asection),trim(aline),trim(qrrffile)
153      format('"',a,'" section header expected at line ',
     +   a,' of file ',a,'.')
         go to 9890
       end if
       iline=iline+1
       read(10,*,err=9100,end=9100) npar,nobs
       atype='NPAR'
       if(npar.le.0)then
         call writint(aline,iline)
         write(errmsg,160) trim(atype),trim(aline),trim(qrrffile)
160      format(a,' must be 1 or greater at line ',a,' of file ',
     +   a,'.')
         go to 9890
       end if
       if(nobs.le.0)then
         atype='NOBS'
         call writint(aline,iline)
         write(errmsg,160) trim(atype),trim(aline),trim(qrrffile)
         go to 9890
       end if

! -- Memory is allocated.

       allocate(apar(npar),aobs(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200

! -- The "* parameter names" section of the header is now read.

       asection='* parameter names'
       iline=iline+1
       read(10,'(a)',err=9000,end=9000)cline
       if(index(cline,trim(asection)).eq.0)then
         call writint(aline,iline)
         write(errmsg,170) trim(asection),trim(aline),trim(qrrffile)
170      format('"',a,'" section expected at line ',a,
     +   ' of file ',a,'.')
         go to 9890
       end if
       do ipar=1,npar
         iline=iline+1
         read(10,*,err=9100,end=9100) apar(ipar)
         call lowcas(apar(ipar))
       end do

       asection='* observation names'
       iline=iline+1
       read(10,'(a)',err=9000,end=9000)cline
       if(index(cline,trim(asection)).eq.0)then
         call writint(aline,iline)
         write(errmsg,170) trim(asection),trim(aline),trim(qrrffile)
         go to 9890
       end if
       do iobs=1,nobs
         iline=iline+1
         read(10,*,err=9100,end=9100) aobs(iobs)
         call lowcas(aobs(iobs))
       end do

       iline_end_of_header=iline

C -- We now read the run results file a first time, looking for errors and
C    counting parameter value indices.

       do
         asection='* parameter set index'
400      continue
         iline=iline+1
         read(10,'(a)',end=700) cline
         if(cline.eq.' ') go to 400
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         iline=iline+1
         read(10,*,err=9150,end=700) itemp
         asection='* parameter values source'
405      continue
         iline=iline+1
         read(10,'(a)',end=700) cline
         if(cline.eq.' ') go to 405
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         iline=iline+1
         read(10,'(a)',end=700) parsource
         asection='* parameter values'
407      continue
         iline=iline+1
         read(10,'(a)',end=700) cline
         if(cline.eq.' ') go to 407
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         do ipar=1,npar
           iline=iline+1
           read(10,*,err=9400,end=700) ppval
         end do
         asection='* model output values'
406      continue
         iline=iline+1
         read(10,'(a)',end=700) cline
         if(cline.eq.' ') go to 406
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         ireject=0
         do iobs=1,nobs
           iline=iline+1
           read(10,*,err=9400,end=700) ooval
           if(ooval.lt.-1.0e35) ireject=1
         end do
         if(ireject.eq.1)then
           iwarn_fail=iwarn_fail+1
         end if
         numset=numset+1
       end do

700    continue
       if(numset.eq.0)then
         write(errmsg,710) trim(qrrffile)
710      format('No parameter sets were found in file ',a,'.')
         go to 9890
       end if

C -- Memory is allocated.

       allocate(pval(npar,numset),oval(nobs,numset),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(parsetindex(numset),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(aval(numset),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- The file is rewound. We position ourself just before the first parameter set.

        rewind(unit=10,iostat=ierr)
        if(ierr.ne.0)then
          write(errmsg,720) trim(qrrffile)
720       format('Error incurred in rewinding file ',a,'.')
          go to 9890
        end if
        do iline=1,iline_end_of_header
          read(10,*,err=9420)
        end do

! -- We now read the actual data again

       iset=0
       do
         iset=iset+1
         asection='* parameter set index'
1400     continue
         iline=iline+1
         read(10,'(a)',end=1700) cline
         if(cline.eq.' ') go to 1400
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         iline=iline+1
         read(10,*,err=9150,end=1700) parsetindex(iset)
         asection='* parameter values source'
1405     continue
         iline=iline+1
         read(10,'(a)',end=1700) cline
         if(cline.eq.' ') go to 1405
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         iline=iline+1
         read(10,'(a)',end=1700) parsource
         asection='* parameter values'
1407     continue
         iline=iline+1
         read(10,'(a)',end=1700) cline
         if(cline.eq.' ') go to 1407
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         do ipar=1,npar
           iline=iline+1
           read(10,*,err=9400,end=1700) pval(ipar,iset)
         end do
         asection='* model output values'
1406     continue
         iline=iline+1
         read(10,'(a)',end=1700) cline
         if(cline.eq.' ') go to 1406
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         do iobs=1,nobs
           iline=iline+1
           read(10,*,err=9400,end=1700) oval(iobs,iset)
         end do
         if(iset.eq.numset) go to 1700
       end do

1700   continue
       close(unit=10)
       write(6,800) trim(qrrffile)
800    format(' - file ',a,' read ok.')
       if(iwarn_fail.ne.0)then
         call writint(anum,iwarn_fail)
         write(6,810) trim(anum)
810      format('   Warning: data indicate ',a,
     +   ' failed model runs.')
       end if
       do iset=1,numset
         if(parsetindex(iset).ne.iset)then
           iwarn_nonseq=1
           go to 815
         end if
       end do
815    continue
       if(iwarn_nonseq.ne.0)then
         write(6,820)
820      format('   Warning: param set indices are not ',
     +   'sequential and/or do not start at 1')
         write(6,826)
826      format('   Indices found in RRF file are recorded in ',
     +   'output files')
       end if

C -- We now write the output files.

       write(6,1290) trim(qoutfile1)
1290   format(/,' - writing file ',a,'...')
       open(unit=20,file=outfile1,recl=26+16*numset,
     + action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,1300) trim(qoutfile1)
1300     format(' Cannot open file ',a,' to write output parameter ',
     +   'value table.')
         go to 9890
       end if
       do iset=1,numset
         call writint(aval(iset),parsetindex(iset))
         aval(iset)=','//aval(iset)
       end do
       write(20,1310) (trim(aval(iset)),iset=1,numset)
1310   format('ParSetIndex',20000(a))
       do ipar=1,npar
         do iset=1,numset
           write(aval(iset),'(1pg14.7)') pval(ipar,iset)
           aval(iset)=adjustl(aval(iset))
           aval(iset)=','//aval(iset)
         end do
         write(20,1230) trim(apar(ipar)),
     +   (trim(aval(iset)),iset=1,numset)
1230     format(a,20000(a))
       end do
       close(unit=20)
       write(6,1235) trim(qoutfile1)
1235   format(' - file ',a,' written ok.')

       write(6,1290) trim(qoutfile2)
       open(unit=20,file=outfile2,recl=26+16*numset,
     + action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,1301) trim(qoutfile2)
1301     format(' Cannot open file ',a,' to write model output ',
     +   'value table.')
         go to 9890
       end if
       do iset=1,numset
         call writint(aval(iset),parsetindex(iset))
         aval(iset)=','//aval(iset)
       end do
       write(20,1311) (trim(aval(iset)),iset=1,numset)
1311   format('ObsSetIndex',20000(a))
       do iobs=1,nobs
         do iset=1,numset
           write(aval(iset),'(1pg14.7)') oval(iobs,iset)
           aval(iset)=adjustl(aval(iset))
           aval(iset)=','//aval(iset)
         end do
         write(20,1230) trim(aobs(iobs)),
     +   (trim(aval(iset)),iset=1,numset)
       end do
       close(unit=20)
       write(6,1235) trim(qoutfile2)

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' RRF2CSV is run using the command:',/)
       write(6,8920)
8920   format('     rrf2csv rrffile outfile1 outfile2',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8950)
8950   format('     rrffile  is an existing run results file,')
       write(6,8980)
8980   format('     outfile1 is the name for a CSV file for ',
     + 'parameter values, and')
       write(6,8990)
8990   format('     outfile2 is the name for a CSV file for model ',
     + 'output values.')
       go to 9999


9000   write(errmsg,9010) trim(qrrffile)
9010   format('Error or premature end encountered to run results ',
     + 'file ',a,' while reading header section of this file.')
       go to 9890

9050   write(errmsg,9060) trim(asection),trim(qrrffile)
9060   format('Cannot find "',a,'" section in file ',a,'.')
       go to 9890

9100   write(errmsg,9110) trim(asection),trim(qrrffile)
9110   format('Error reading data from "',a,'" section of file ',a,'.')
       go to 9890

9150   call writint(aline,iline)
       write(errmsg,9160) trim(aline),trim(qrrffile)
9160   format('Cannot read parameter set index from line ',a,
     + ' of file ',a,'.')
       go to 9890

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9350   write(errmsg,9360) trim(asection),trim(aline),trim(qrrffile)
9360   format('"',a,'" header expected at line ',a,' of file ',a,'.')
       go to 9890

9400   call writint(aline,iline)
       write(errmsg,9410) trim(aline),trim(qrrffile)
9410   format('Error reading data from line ',a,' of file ',a,'.')
       go to 9890

9420   write(errmsg,9430) trim(qrrffile)
9430   format('Error re-reading file ',a,'.')
       go to 9890

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
       close(unit=20,status='delete',iostat=ierr)

9900   continue
       close(unit=20,iostat=ierr)
       close(unit=10,iostat=ierr)

       deallocate(parsetindex,pval,oval,stat=ierr)
       deallocate(apar,aval,aobs,stat=ierr)

9999   continue

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


