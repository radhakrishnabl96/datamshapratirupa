       program jcb2rrf1

C --   Program JCB2RRF1 reads parameters and model outputs recorded in two JCB files written
C      by PESTPP-IES and writes these as an RRF file.

       implicit none

       integer          :: ierr,ifail,nb,i,j,itemp1,itemp2,nespar,nxrow,
     +                     nobs,icount,irow,icol,ies,itemp,iirow,iobs
       integer          :: nxrow1,icountp,icounto
       integer          :: lastirow,lasticol
       integer          :: parindex
       integer          :: lw(5),rw(5)
       double precision :: dtemp
       character*4      :: aext
       character*12     :: aversion
       character*20     :: atemp20
       character*200    :: acol,arow,atemp200
       character*256    :: jcbbase,rrffile,afile
       character*256    :: jcbfile_par,jcbfile_obs,afile_par,afile_obs
       character*600    :: cline
       character*1500   :: amessage

       integer, allocatable :: startrowp(:),startrowo(:)
       double precision, allocatable   :: pval(:),oval(:)
       character*200, allocatable      :: apar(:),aobs(:),areal(:)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' JCB2RRF version ',a,'. Watermark Numerical ',
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
       call linspl(ifail,3,lw,lw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       jcbbase=cline(lw(1):rw(1))
       call remchar(jcbbase,char(211))
       rrffile=cline(lw(2):rw(2))
       call remchar(rrffile,char(211))
#ifndef UNIX
       call lowcas(jcbbase)
       call lowcas(rrffile)
#endif

       write(6,*)
       nb=len_trim(jcbbase)
       i=nb-3
       if(i.lt.1)i=1
       aext=jcbbase(i:nb)
       call lowcas(aext)
       if(aext.eq.'.jcb')then
         write(amessage,15)
15       format(' The filename base of the JCB files must not ',
     +   'include a ".jcb" extension. Extensions ".par.jcb" ',
     +   'and ".obs.jcb" are added automatically by JCB2RRF.')
         go to 9800
       end if
       nb=len_trim(rrffile)
       i=nb-3
       if(i.lt.1)i=1
       aext=rrffile(i:nb)
       call lowcas(aext)
       if(aext.ne.'.rrf')then
         if(aext(1:1).eq.'.')then
           write(amessage,151)
151        format(' The name of the run results file must have ',
     +     'an extension of ".rrf".')
           go to 9800
         else
           rrffile=trim(rrffile)//'.rrf'
         end if
       end if

C -- The RRF file is opened.

       open(unit=20,file=rrffile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,16) trim(rrffile)
16       format(' Cannot write to file ',a,'.')
         go to 9800
       end if

C -- The Jacobian matrix files are opened.

       jcbfile_par=trim(jcbbase)//'.par.jcb'
       jcbfile_obs=trim(jcbbase)//'.obs.jcb'
       call addquote(jcbfile_par,afile_par)
       call addquote(jcbfile_obs,afile_obs)
       write(6,30)
30     format(' - reading enhanced Jacobian files...')
       call open_unformatted_file(ifail,10,'read',jcbfile_par,amessage)
       if(ifail.ne.0) go to 9800
       call open_unformatted_file(ifail,11,'read',jcbfile_obs,amessage)
       if(ifail.ne.0) go to 9800

C -- It is verified that the files have the same number of rows.

       read(10,err=9350,end=9400)itemp1,itemp2
       if(itemp1.le.0)then
         write(amessage,60) trim(afile_par)
60       format(' File ',a,' does not appear to use enhanced ',
     +   'Jacobian matrix file format. The first integer in the ',
     +   'file must be positive.')
         go to 9800
       end if
       nespar=abs(itemp1)
       nxrow=abs(itemp2)
       if((nespar.le.0).or.(nxrow.le.0))then
         write(amessage,65) trim(afile_par)
65       format(' NROW or NCOL is not positive in file ',a,
     +   '; is this a valid enhanced Jacobian matrix file?')
         go to 9800
       end if
       read(11,err=9370,end=9420)itemp1,itemp2
       if(itemp1.le.0)then
         write(amessage,60) trim(afile_obs)
         go to 9800
       end if
       nobs=abs(itemp1)
       nxrow1=abs(itemp2)
       if((nobs.le.0).or.(nxrow1.le.0))then
         write(amessage,65) trim(afile_obs)
         go to 9800
       end if
       if(nxrow.ne.nxrow1)then
         write(amessage,67) trim(afile_par),trim(afile_obs)
67       format(' Files ',a,' and ',a,' are not compatible. ',
     +   'They do not contain ',
     +   'results for the same number of realizations.')
         go to 9800
       end if
       read(10,err=9350,end=9400)icountp
       read(11,err=9370,end=9420)icounto

! -- Some memory is now allocated. Then we read to the end of the matrix in each file.

       allocate(apar(nespar),aobs(nobs),areal(nxrow),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(startrowp(nxrow+1),startrowo(nxrow+1),stat=ierr)
       if(ierr.ne.0) go to 9200
       startrowp(nxrow+1)=icountp+1
       startrowo(nxrow+1)=icounto+1

       lastirow=0
       lasticol=0
       do i=1,icountp
         read(10,err=9350,end=9400) irow,icol,dtemp
         irow=irow+1
         icol=icol+1
         if((icol.le.0).or.(irow.le.0).or.(icol.gt.nespar)
     +   .or.(irow.gt.nxrow))then
           call writint(acol,icol-1)
           call writint(arow,irow-1)
           write(amessage,111) trim(arow),trim(acol),trim(afile_par)
111        format(' [row,col] [',a,',',a,
     +     '] out of range in file ',a,'.')
           go to 9800
         end if
         if(irow.lt.lastirow) then
           afile=afile_par
           go to 9450
         end if
         if(icol.lt.lasticol)then
           if(irow.eq.lastirow)then
             afile=afile_par
             go to 9450
           end if
         end if
         if(irow.ne.lastirow)then
           do j=lastirow+1,irow
             startrowp(j)=i
           end do
         end if
         lastirow=irow
         lasticol=icol
       end do
       lastirow=0
       lasticol=0
       do i=1,icounto
         read(11,err=9370,end=9420) irow,icol,dtemp
         irow=irow+1
         icol=icol+1
         if((icol.le.0).or.(irow.le.0).or.(icol.gt.nobs)
     +   .or.(irow.gt.nxrow))then
           call writint(acol,icol-1)
           call writint(arow,irow-1)
           write(amessage,111) trim(arow),trim(acol),trim(afile_obs)
           go to 9800
         end if
         if(irow.lt.lastirow) then
           afile=afile_obs
           go to 9450
         end if
         if(icol.lt.lasticol)then
           if(irow.eq.lastirow)then
             afile=afile_obs
             go to 9450
           end if
         end if
         if(irow.ne.lastirow)then
           do j=lastirow+1,irow
             startrowo(j)=i
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
       do iobs=1,nobs
         read(11,err=9370,end=9420) aobs(iobs)
         aobs(iobs)=adjustl(aobs(iobs))
       end do

C -- The realization names are read from the par file.

       do irow=1,nxrow
         read(10,err=9350,end=9400) areal(irow)
         areal(irow)=adjustl(areal(irow))
       end do

C -- We now check them against the obs file.

       do irow=1,nxrow
         read(11,err=9370,end=9420) atemp200
         atemp200=adjustl(atemp200)
         if(atemp200.ne.areal(irow))then
!           write(amessage,130) trim(afile_par),trim(afile_obs)
!130        format(' Realization names in file ',a,' and ',a,
!     +     ' are different, or not in the same order.')
!           go to 9800
         end if
       end do

C -- We check that realizations are named as expected.

       icount=0
       do irow=1,nxrow
         call lowcas(areal(irow))
         atemp20=areal(irow)
         if(atemp20.eq.'base')then
           icount=icount+1
           if(icount.gt.1)then
!             write(amessage,140)
!140          format(' More than one realization is named "base" ',
!     +       'in JCB files.')
!             go to 9800
           end if
         else
!           call intread(ifail,atemp20,itemp)
!           if(ifail.ne.0)then
!             write(amessage,150) trim(areal(irow))
!150          format(' Except for "base", realizations are expected ',
!     +       'to have integer identifiers in user-nominated ',
!     +       'JCB files. This is not the case for realization "',a,'".')
!             go to 9800
!           end if
         end if
       end do

C -- Both files are re-wound.

       rewind(unit=10)
       read(10) itemp1,itemp2
       read(10) itemp1
       rewind(unit=11)
       read(11) itemp1,itemp2
       read(11) itemp1
       parindex=0

C -- The first part of the RRF file is recorded.

       write(20,200)
200    format('* case dimensions')
       write(20,210) nespar,nobs
210    format(2i10)
       write(20,220)
220    format('* parameter names')
       do ies=1,nespar
         write(20,'(a)') trim(apar(ies))
       end do
       write(20,230)
230    format('* observation names')
       do iobs=1,nobs
         write(20,'(a)') trim(aobs(iobs))
       end do

C -- We allocate some more memory

       allocate(pval(nespar),oval(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- We read and record realization information.

       do irow=1,nxrow
         pval=0.0d0        ! an array
         oval=0.0d0        ! an array
         do i=startrowp(irow),startrowp(irow+1)-1
           read(10) itemp,ies,dtemp
           if(itemp+1.ne.irow)then
             write(amessage,250) trim(afile_par)
250          format(' Programming error encountered while re-reading ',
     +       'file ',a,'. Contact programmer.')
             go to 9800
           end if
           pval(ies+1)=dtemp
         end do
         do i=startrowo(irow),startrowo(irow+1)-1
           read(11) itemp,iobs,dtemp
           if(itemp+1.ne.irow)then
             write(amessage,250) trim(afile_obs)
             go to 9800
           end if
           oval(iobs+1)=dtemp
         end do
         atemp20=areal(irow)
!         if(atemp20.eq.'base')then
!           iirow=-1
!         else
!           call intread(ifail,atemp20,iirow)
!         end if
         write(20,260)
260      format('* parameter set index')
         parindex=parindex+1
         write(20,270) parindex
270      format(i7)
         write(20,280)
280      format('* parameter values source')
         write(20,290) trim(areal(irow))
290      format(' realization "',a,'"')
         write(20,300)
300      format('* parameter values')
         do ies=1,nespar
           write(20,320) pval(ies)
320        format(1x,1pg14.7)
         end do
         write(20,330)
330      format('* model output values')
         do iobs=1,nobs
           write(20,340) oval(iobs)
340        format(1x,1pg14.7)
         end do
       end do

C -- Files are closed

       close(unit=10,iostat=ierr)
       write(6,400) trim(afile_par)
400    format(' - file ',a,' read ok.')
       close(unit=11,iostat=ierr)
       write(6,400) trim(afile_obs)
       close(unit=20,iostat=ierr)
       call addquote(rrffile,afile)
       write(6,420) trim(afile)
420    format(' - file ',a,' written ok.')
       go to 9999

8900   continue
       write(6,8910)
8910   format(/,' JCB2RRF1 is run using the command:',/)
       write(6,8920)
8920   format('     jcb2rrf1 jcbbase rrffile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     jcbbase is the filename base of par and obs JCB ',
     + 'files, and ')
       write(6,8960)
8960   format('     rrffile is an output run results file.')

       go to 9999

9100   write(amessage,9110) trim(afile)
9110   format(' Cannot write to matrix file ',a,'.')
       go to 9800

9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800

9350   write(amessage,9360) trim(afile_par)
9360   format(' Error encountered when reading data from ',
     + 'file ',a,'.')
       go to 9800

9370   write(amessage,9360) trim(afile_obs)
       go to 9800

9400   write(amessage,9410) trim(afile_par)
9410   format(' Premature end encountered to file ',a,'.')
       go to 9800

9420   write(amessage,9410) trim(afile_obs)
       go to 9800

9450   write(amessage,9460) trim(afile)
9460   format(' This program expects order of entries in JCB file ',a,
     + ' to be row-increasing then column-increasing.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9999   continue
       deallocate(startrowp,startrowo,stat=ierr)
       deallocate(pval,oval,stat=ierr)
       deallocate(apar,aobs,areal,stat=ierr)

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
