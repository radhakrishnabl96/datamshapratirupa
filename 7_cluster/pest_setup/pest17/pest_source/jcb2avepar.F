       program jcb2avepar

C --   Program JCB2AVEPAR reads parameters stored in a JCB file and writes a single
C      PAR file that pertains to (log) averaged parameters.

       implicit none

       logical          :: skipline
       integer          :: ifail,nb,itemp1,itemp2,icount,ierr,i
       integer          :: ireal,nespar,nreal,ies
       integer          :: irow,icol
       integer          :: iline,npar,ipar,jcount,jpar,jes
       integer          :: lw(5),rw(5)
       double precision :: dtemp
       character*5      :: aext
       character*10     :: acol,arow,atemp1,atrans,aline
       character*12     :: aversion
       character*12     :: aapar,bbpar
       character*200    :: alpar
       character*256    :: jcbfile,ajcbfile,parfile,aparfile,
     +                     pestfile,apestfile
       character*600    :: cline
       character*1500   :: amessage

       integer, allocatable            :: itrans(:)
       double precision, allocatable   :: pval(:),pestval(:)
       character*12, allocatable       :: apar(:)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' JCB2AVEPAR version ',a,'. Watermark Numerical ',
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
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       jcbfile=cline(lw(1):rw(1))
       call remchar(jcbfile,char(211))
       pestfile=cline(lw(2):rw(2))
       call remchar(pestfile,char(211))
       parfile=cline(lw(3):rw(3))
       call remchar(parfile,char(211))
#ifndef UNIX
       call lowcas(jcbfile)
       call lowcas(pestfile)
       call lowcas(parfile)
#endif
       call addquote(jcbfile,ajcbfile)
       call addquote(pestfile,apestfile)
       call addquote(parfile,aparfile)

       write(6,*)
       nb=len_trim(jcbfile)
       i=nb-3
       if(i.lt.1)i=1
       aext=jcbfile(i:nb)
       call lowcas(aext)
       if(aext.ne.'.jcb')then
         write(amessage,15) trim(ajcbfile)
15       format(' File ',a,' does not have an extension of ".jcb".')
         go to 9800
       end if

       nb=len_trim(parfile)
       i=nb-3
       if(i.lt.1)i=1
       aext=parfile(i:nb)
       call lowcas(aext)
       if(aext.ne.'.par')then
         write(amessage,32)
32       format(' A parameter value file must have an extension ',
     +   'of ".par".')
         go to 9800
       end if

       nb=len_trim(pestfile)
       call addquote(pestfile,apestfile)
       i=nb-3
       if(i.lt.1)i=1
       aext=pestfile(i:nb)
       call lowcas(aext)
       if(aext.ne.'.pst')then
         write(amessage,33)
33       format(' A PEST control file must have an extension ',
     +   'of ".pst".')
         go to 9800
       end if

C -- The JCB file is opened.

       call open_unformatted_file(ifail,10,'read',jcbfile,amessage)
       if(ifail.ne.0) go to 9800

C -- The PEST control file is opened.

       open(unit=11,file=pestfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,34) trim(apestfile)
34       format(' Cannot open PEST control file ',a,'.')
         go to 9800
       end if

C -- The integrity of the JCB file is verified.

       read(10,err=9350,end=9400)itemp1,itemp2
       if(itemp1.le.0)then
         write(amessage,60) trim(ajcbfile)
60       format(' File ',a,' does not appear to use enhanced ',
     +   'Jacobian matrix file format. The first integer in the ',
     +   'file must be positive.')
         go to 9800
       end if
       nespar=abs(itemp1)
       nreal=abs(itemp2)
       if((nespar.le.0).or.(nreal.le.0))then
         write(amessage,65) trim(ajcbfile)
65       format(' NROW or NCOL is not positive in file ',a,
     +   '; is this a valid enhanced Jacobian matrix file?')
         go to 9800
       end if
       read(10,err=9350,end=9400)icount

! -- Some memory is now allocated.

       allocate(apar(nespar),pval(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(itrans(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(pestval(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       itrans=-99999999        ! an array

! -- We read to the end of the file to establish names for parameters and realizations.

       do i=1,icount
         read(10,err=9350,end=9400) irow,icol,dtemp
         irow=irow+1
         icol=icol+1
         if((icol.le.0).or.(irow.le.0).or.(icol.gt.nespar)
     +   .or.(irow.gt.nreal))then
           call writint(acol,icol-1)
           call writint(arow,irow-1)
           write(amessage,111) trim(arow),trim(acol),trim(ajcbfile)
111        format(' [row,col] [',a,',',a,
     +     '] out of range in file ',a,'.')
           go to 9800
         end if
       end do

C -- Column names are read from each file.

       do ies=1,nespar
         read(10,err=9350,end=9400) alpar
         alpar=adjustl(alpar)
         if(len_trim(alpar).gt.12)then
           write(amessage,323) trim(ajcbfile),trim(alpar)
323        format(' This program requires that parameter names ',
     +     'in file ',a,' be of 12 characters or less in length. ',
     +     'This is violated by the name "',a,'".')
           go to 9800
         end if
         apar(ies)=alpar
       end do

C -- We now find the transformation status of each parameter from the PEST control file.

       iline=1
       read(11,*,err=9060,end=9060) atemp1
       call lowcas(atemp1)
       if(atemp1.ne.'pcf') go to 9060
341    continue
       iline=iline+1
       read(11,'(a)',err=9060,end=9060) cline
       if(skipline(cline)) go to 341
342    continue
       iline=iline+1
       read(11,'(a)',err=9060,end=9060) cline
       if(skipline(cline)) go to 342
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9060
366    continue
       iline=iline+1
       read(11,'(a)',err=9060,end=9060) cline
       if(skipline(cline)) go to 366
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.ne.0) go to 9060
       call intread(ifail,cline(lw(1):rw(1)),npar)
       if(ifail.ne.0) go to 9060
       if(npar.ne.nespar)then
         write(amessage,370) trim(apestfile),trim(ajcbfile)
370      format(' Number of parameters featured in file ',a,
     +   ' differs from number of parameters featured in file ',
     +   a,'.')
         go to 9800
       end if
       do
         iline=iline+1
         read(11,'(a)',end=9000) cline
         if(cline(1:1).eq.'*')then
           if(cline(1:16).eq.'* parameter data') exit
         end if
       end do
       continue

       ies=0
       jcount=0
       do ipar=1,npar
390      continue
         iline=iline+1
         read(11,'(a)',end=9060) cline
         if(skipline(cline)) go to 390
         call linspl(ifail,4,lw,rw,cline)
         if(ifail.ne.0) go to 9020
         aapar=cline(lw(1):rw(1))
         call lowcas(aapar)
         call which1(ifail,nespar,ies,apar,aapar)
         if(ifail.ne.0) then
           write(amessage,310) trim(aapar),trim(apestfile),
     +     trim(ajcbfile)
310        format(' Parameter "',a,'" is cited in PEST control ',
     +     'file ',a,' but not in JCB file ',a,'.')
           go to 9800
         end if
         if(itrans(ies).ne.-99999999)then
           write(amessage,320) trim(aapar),trim(apestfile)
320        format(' Parameter name ',a,' is repeated in PEST control ',
     +     'file ',a,'.')
           go to 9800
         end if
         atrans=cline(lw(2):rw(2))
         call lowcas(atrans)
         if(atrans.eq.'none')then
           itrans(ies)=0
         else if(atrans.eq.'log')then
           itrans(ies)=1
         else if(atrans.eq.'fixed')then
           itrans(ies)=-1000000
         else if(atrans.eq.'tied')then
           jcount=jcount+1
           itrans(ies)=-1000001
         else
           call writint(aline,iline)
           write(amessage,330) trim(atrans),trim(aline),trim(apestfile)
330        format(' Unknown parameter transformation status - "',
     +     a,'" at line ',a,' of file ',a,'.')
           go to 9800
         end if
         call drealrd(ifail,cline(lw(4):rw(4)),pestval(ies))
         if(ifail.ne.0) go to 9060
       end do
       if(jcount.ne.0)then
         do jpar=1,jcount
3421        continue
           iline=iline+1
           read(11,'(a)',err=9060,end=9060) cline
           if(skipline(cline)) go to 3421
           call linspl(ifail,2,lw,rw,cline)
           if(ifail.ne.0) go to 9060
           aapar=cline(lw(1):rw(1))
           call lowcas(aapar)
           bbpar=cline(lw(2):rw(2))
           call lowcas(bbpar)
           call which1(ifail,nespar,ies,apar,aapar)
           if(ifail.ne.0) go to 9060
           if(itrans(ies).ne.-1000001) go to 9060
           call which1(ifail,nespar,jes,apar,bbpar)
           if(ifail.ne.0) go to 9060
           if(itrans(jes).eq.-1000000) go to 9060
           if(itrans(jes).lt.0) go to 9060
           if(ies.eq.jes) go to 9060
           itrans(ies)=-jes
         end do
       end if
       close(unit=11)
       write(6,350) trim(apestfile)
350    format(' - file ',a,' read ok.')

C -- We go back to the JCB file.
C -- The JCB file is now rewound.

       rewind(unit=10)
       read(10) itemp1,itemp2
       read(10) itemp1

       open(unit=20,file=parfile,action='write',err=9100)
       write(20,440)
440    format('single point')

       pval=0.0d0                 ! an array
       do i=1,icount
         read(10) ireal,ies,dtemp
         ireal=ireal+1
         ies=ies+1
         if(itrans(ies).eq.0)then
           pval(ies)=pval(ies)+dtemp
         else if(itrans(ies).eq.1)then
           if(dtemp.le.0.0d0)then
             write(amessage,450) trim(apar(ies)),trim(ajcbfile)
450          format(' Zero or negative value provided for parameter "',
     +       a,'" in file ',a,'. However this parameter is ',
     +       'log-transformed in PEST control file.')
             go to 9800
           else
             pval(ies)=pval(ies)+log10(dtemp)
           end if
         end if
       end do
       do ies=1,nespar
         if(itrans(ies).eq.0)then
           pval(ies)=pval(ies)/nreal
         else if(itrans(ies).eq.1)then
           pval(ies)=pval(ies)/nreal
           pval(ies)=10**pval(ies)
         else if(itrans(ies).eq.-1000000)then
           pval(ies)=pestval(ies)
         end if
       end do
       do ies=1,nespar
         if((itrans(ies).lt.0).and.(itrans(ies).gt.-1000000))then
           jes=-itrans(ies)
           pval(ies)=pval(jes)*pestval(ies)/pestval(jes)
         end if
       end do
       do ies=1,nespar
         write(20,160) trim(apar(ies)),pval(ies)
160      format(1x,a,t25,1pg14.7,t40,'  1.0    0.0')
       end do
       close(unit=10)
       write(6,350) trim(ajcbfile)
       close(unit=20)
       write(6,170) trim(aparfile)
170    format(' - file ',a,' written ok.')

       go to 9999

8900   continue
       write(6,8910)
8910   format(/,' JCB2AVEPAR is run using the command:',/)
       write(6,8920)
8920   format('     jcb2avepar jcbfile pestfile parfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     jcbfile  is the name of a JCB file,')
       write(6,8950)
8950   format('     pestfile is the name of the associated PEST ',
     + 'control file, and')
       write(6,8960)
8960   format('     parfile  is the name of a ',
     + 'parameter value file.')

       go to 9999

9000   write(amessage,9010) trim(apestfile)
9010   format(' Cannot find "parameter data" section in PEST ',
     + 'control file ',a,'.')
       go to 9800

9020   continue
       call writint(aline,iline)
       write(amessage,9030) trim(aline),trim(apestfile)
9030   format(' Insufficient entries on line ',a,' of file ',a,'.')
       go to 9800

9060   write(amessage,9070) trim(apestfile)
9070   format(' Error encountered when reading PEST control file ',
     + a,' - check this file with PESTCHEK.')
       go to 9800

9100   write(amessage,9110) trim(aparfile)
9110   format(' Cannot write to file ',a,'.')
       go to 9800

9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800

9350   write(amessage,9360) trim(ajcbfile)
9360   format(' Error encountered when reading data from ',
     + 'file ',a,'.')
       go to 9800

9400   write(amessage,9410) trim(ajcbfile)
9410   format(' Premature end encountered to file ',a,'.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9999   continue
       deallocate(pval,apar,itrans,pestval,stat=ierr)

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
