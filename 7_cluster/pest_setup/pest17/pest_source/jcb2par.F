       program jcb2par

C --   Program JCB2PAR reads parameters stored in a JCB file and writes one or a number of
C      PAR files.

       implicit none

       integer          :: ifail,nb,i,itemp1,itemp2,icount,ierr,j,itemp
       integer          :: ireal,nespar,nreal,iireal,ies
       integer          :: lastirow,lasticol,irow,icol
       integer          :: lw(5),rw(5)
       double precision :: dtemp
       character*5      :: aext
       character*10     :: acol,arow
       character*12     :: aversion
       character*20     :: realname
       character*256    :: jcbfile,ajcbfile,parfile,aoutfile,outfile
       character*600    :: cline
       character*1500   :: amessage

       integer, allocatable            :: startrow(:)
       double precision, allocatable   :: pval(:)
       character*200, allocatable      :: apar(:),areal(:)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' JCB2PAR version ',a,'. Watermark Numerical ',
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
       realname=cline(lw(2):rw(2))
       call remchar(realname,char(211))
       parfile=cline(lw(3):rw(3))
       call remchar(parfile,char(211))
#ifndef UNIX
       call lowcas(jcbfile)
       call lowcas(parfile)
#endif
       call lowcas(realname)
       call addquote(jcbfile,ajcbfile)
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
       if((realname.ne.'all').and.(realname.ne.'base'))then
         call intread(ifail,realname,ireal)
         if(ifail.ne.0)then
           write(amessage,20)
20         format(' The realization name provided on the command line ',
     +     'must be "base", "all" or a number.')
           go to 9800
         end if
       end if
       nb=len_trim(parfile)
       i=nb-3
       if(i.lt.1)i=1
       aext=parfile(i:nb)
       call lowcas(aext)
       if(aext.eq.'.par')then
         if(realname.eq.'all')then
           call addquote(parfile,aoutfile)
           write(amessage,30)
30         format(' If a realization name of "all" is provided on ',
     +     'the command line then ',
     +     'an extension of ".par" must not be provided with the ',
     +     'name of the parameter value file.')
           go to 9800
         end if
       else
         if(realname.ne.'all')then
           write(amessage,32)
32         format(' A parameter value file must have an extension ',
     +     'of ".par".')
           go to 9800
         end if
       end if

C -- The JCB file is opened.

       write(6,35) trim(ajcbfile)
35     format(' - reading file ',a,'...')
       call open_unformatted_file(ifail,10,'read',jcbfile,amessage)
       if(ifail.ne.0) go to 9800

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

       allocate(apar(nespar),pval(nespar),areal(nreal),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(startrow(nreal+1),stat=ierr)
       if(ierr.ne.0) go to 9200
       startrow(nreal+1)=icount+1

! -- We read to the end of the file to establish names for parameters and realizations.

       lastirow=0
       lasticol=0
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
         if(irow.lt.lastirow) then
           go to 9450
         end if
         if(icol.lt.lasticol)then
           if(irow.eq.lastirow)then
             go to 9450
           end if
         end if
         if(irow.ne.lastirow)then
           do j=lastirow+1,irow
             startrow(j)=i
           end do
         end if
         lastirow=irow
         lasticol=icol
       end do

C -- Column names are read from each file.

       do ies=1,nespar
         read(10,err=9350,end=9400) apar(ies)
         apar(ies)=adjustl(apar(ies))
       end do
       do ireal=1,nreal
         read(10,err=9350,end=9400) areal(ireal)
         areal(irow)=adjustl(areal(irow))
       end do

C -- We see if the realization name that we want is featured in the file.

       if(realname.ne.'all')then
         do ireal=1,nreal
           if(realname.eq.areal(ireal)) go to 120
         end do
         if(realname.eq.'base')then
           write(amessage,115) trim(realname),trim(ajcbfile)
115        format(' Realization "',a,'" not found in file ',a,'.')
         else
           write(amessage,116) trim(realname),trim(ajcbfile)
116        format(' Realization ',a,' not found in file ',a,'.')
         end if
         go to 9800
120      iireal=ireal
       end if

C -- The JCB file is now rewound.

       rewind(unit=10)
       read(10) itemp1,itemp2
       read(10) itemp1

C -- We treat the "all" case now.

       if(realname.eq.'all')then
         do ireal=1,nreal
           pval=0.0d0             ! an array
           outfile=trim(parfile)//trim(areal(ireal))//'.par'
           call addquote(outfile,aoutfile)
           open(unit=20,file=outfile,action='write',iostat=ierr)
           if(ierr.ne.0)then
             write(amessage,130) trim(aoutfile)
130          format(' Cannot write to file ',a,'.')
             go to 9800
           end if
           write(20,140)
140        format('single point')
           do i=startrow(ireal),startrow(ireal+1)-1
             read(10) itemp1,ies,dtemp
             if(itemp1+1.ne.ireal)then
               write(amessage,150) trim(aoutfile)
150            format(' Programming error encountered while re-reading ',
     +         'file ',a,'. Contact programmer.')
               go to 9800
             end if
             pval(ies+1)=dtemp
           end do
           do ies=1,nespar
             write(20,160) trim(apar(ies)),pval(ies)
160          format(1x,a,t25,1pg14.7,t40,'  1.0    0.0')
           end do
           close(unit=20)
           write(6,170) trim(aoutfile)
170        format(' - file ',a,' written ok.')
         end do
         close(unit=10)
         write(6,210) trim(ajcbfile)
       else
         pval=0.0d0  ! an array
         call addquote(parfile,aoutfile)
         open(unit=20,file=parfile,action='write',iostat=ierr)
         if(ierr.ne.0)then
           write(amessage,130) trim(aoutfile)
           go to 9800
         end if
         write(20,140)
         if(iireal.ne.1)then
           do i=1,startrow(iireal)-1
             read(10) itemp,ies,dtemp
           end do
         end if
         do i=startrow(iireal),startrow(iireal+1)-1
           read(10) itemp,ies,dtemp
           if(itemp+1.ne.iireal)then
             write(amessage,150) trim(aoutfile)
             go to 9800
           end if
           pval(ies+1)=dtemp
         end do
         do ies=1,nespar
           write(20,160) trim(apar(ies)),pval(ies)
         end do
         close(unit=10)
         write(6,210) trim(ajcbfile)
210      format(' - file ',a,' read ok.')
         close(unit=20)
         write(6,170) trim(aoutfile)
       end if

       go to 9999

8900   continue
       write(6,8910)
8910   format(/,' JCB2PAR is run using the command:',/)
       write(6,8920)
8920   format('     jcb2par jcbfile realname parfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     jcbfile  is the name of a JCB file,')
       write(6,8950)
8950   format('     realname is the name/number of a realization or ',
     + '"all", and')
       write(6,8960)
8960   format('     parfile  is the name or filename base of a ',
     + 'parameter value file.')

       go to 9999

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

9450   write(amessage,9460) trim(ajcbfile)
9460   format(' This program expects order of entries in JCB file ',a,
     + ' to be row-increasing then column-increasing.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9999   continue
       deallocate(startrow,pval,apar,areal,stat=ierr)

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
