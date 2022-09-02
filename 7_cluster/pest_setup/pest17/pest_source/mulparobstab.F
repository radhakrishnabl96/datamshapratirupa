       program mulparobstab

C -- Program MULPAROBSTAB creates a table from multiple parameter value files.

       implicit none

       logical              :: obsnow
       integer, parameter   :: MAXLIST=500
       integer              :: ifail,ierr,iline,ilist,nb,itemp1,itemp2,
     +                         i,nlist,iflag,ipar,jpar,npar
       integer              :: iobs,nobs,jobs
       integer              :: lw(8),rw(8)
       integer              :: inum(MAXLIST)
       integer, allocatable :: ifound(:)
       real                 :: rtemp,scale,offset
       real, allocatable    :: pval(:,:),oval(:,:)
       character*10         :: aline,anum,aind,atemp
       character*12         :: aversion,aapar
       character*20         :: aaobs
       character*300        :: parfile,listfile,outfile,alistfile,
     +                         aoutfile,afile,bfile
       character*300        :: outfile1,aoutfile1
       character*300        :: cline
       character*500        :: amessage
       character*12, allocatable :: apar(:)
       character*20, allocatable :: aobs(:)
       character*14, allocatable :: aval(:)


#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' MULPAROBSTAB version ',a,'. Watermark Numerical ',
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
       call linspl(ifail,5,lw,lw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       parfile=cline(lw(1):rw(1))
       call remchar(parfile,char(211))
       listfile=cline(lw(2):rw(2))
       call remchar(listfile,char(211))
       outfile=cline(lw(3):rw(3))
       call remchar(outfile,char(211))
       outfile1=cline(lw(4):rw(4))
       call remchar(outfile1,char(211))

#ifndef UNIX
       call lowcas(parfile)
       call lowcas(listfile)
       call lowcas(outfile)
       call lowcas(outfile1)
#endif

C -- The generic parameter value file name is tested.

       if(index(parfile,'*').eq.0)then
         write(amessage,30)
30       format(' Generic PAROBS filename must contain at ',
     +   'least one "*" character.')
         go to 9800
       end if

C -- The list file is read.

       call addquote(listfile,alistfile)
       write(6,35) trim(alistfile)
35     format(/,' - reading integer list file ',a,'...')
       open(unit=11,file=listfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,40) trim(alistfile)
40       format(' Cannot open integer list file ',a,'.')
         go to 9800
       end if

       iline=0
       ilist=0
       do
         iline=iline+1
         read(11,'(a)',end=115)cline
         if(cline.eq.' ')cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'*') cycle
         if(cline(1:1).eq.'#') cycle
         nb=index(cline,'-')
         call writint(aline,iline)
         if(nb.eq.0)then
           call linspl(ifail,2,lw,rw,cline)
           if(ifail.eq.0) go to 9250
           ilist=ilist+1
           if(ilist.gt.MAXLIST)then
             call writint(anum,MAXLIST)
             write(amessage,50) trim(alistfile),trim(anum)
50           format(' The number of integer indices listed or ',
     +       'implied in file ',a,' is limited to ',a,' to prevent ',
     +       'excessive output file width. If you want more than ',
     +       'that this, increase MAXLIST and re-compile program.')
             go to 9800
           end if
           call intread(ifail,cline(lw(1):rw(1)),inum(ilist))
           if(ifail.ne.0)then
             write(amessage,60) trim(aline),trim(alistfile)
60           format(' Error reading integer from line ',a,
     +       ' of file ',a,'.')
             go to 9800
           end if
         else
           if(nb.eq.1)then
             write(amessage,80) trim(aline),trim(alistfile)
80           format(' Error reading first integer from line ',a,
     +       ' of file ',a,'.')
             go to 9800
           else
             anum=cline(1:nb-1)
             call intread(ifail,anum,itemp1)
             if(ifail.ne.0)then
               write(amessage,80) trim(aline),trim(alistfile)
               go to 9800
             end if
             cline=cline(nb+1:)
             if(cline.eq.' ')then
               write(amessage,90) trim(aline),trim(alistfile)
90             format(' Error reading second integer from line ',a,
     +         ' of file ',a,'.')
               go to 9800
             end if
             call linspl(ifail,1,lw,rw,cline)
             anum=cline(lw(1):rw(1))
             call intread(ifail,anum,itemp2)
             if(ifail.ne.0)then
               write(amessage,90) trim(aline),trim(alistfile)
               go to 9800
             end if
             if(itemp2.lt.itemp1)then
               write(amessage,110) trim(aline),trim(alistfile)
110            format(' Second integer does not exceed first integer ',
     +         'at line ',a,' of file ',a,'.')
               go to 9800
             end if
             do i=itemp1,itemp2
               ilist=ilist+1
               if(ilist.gt.MAXLIST)then
                 call writint(anum,MAXLIST)
                 write(amessage,50) trim(alistfile),trim(anum)
                 go to 9800
               end if
               inum(ilist)=i
             end do
           end if
         end if
       end do
115    nlist=ilist
       if(nlist.eq.0)then
         write(amessage,116) trim(alistfile)
116      format(' No integers are listed in file ',a,'.')
         go to 9800
       end if
       close(unit=11)
       write(6,120) trim(alistfile)
120    format(' - file ',a,' read ok.')

       allocate(ifound(nlist),stat=ierr)
       if(ierr.ne.0) go to 9200
       ifound=1

C -- Parameter value files are now read.

       iflag=0
       do ilist=1,nlist
         call writint(aind,inum(ilist))
         aind=adjustl(aind)
         afile=parfile
         do
           nb=index(afile,'*')
           if(nb.eq.0) exit
           if(nb.eq.1)then
             afile=trim(aind)//trim(afile(2:))
           else
             afile=afile(1:nb-1)//trim(aind)//trim(afile(nb+1:))
           end if
         end do
         call addquote(afile,bfile)
         write(6,200) trim(bfile)
200      format(/,' - reading parameter value file ',a,'...')
         open(unit=10,file=afile,status='old',iostat=ierr)
         if(ierr.ne.0)then
           ifound(ilist)=0
           write(6,210) trim(bfile)
210        format(' - file ',a,' not found.')
           cycle
         end if
         read(10,'(a)',err=9300,end=9300) cline
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9300
         call lowcas(cline)
         atemp=cline(lw(1):rw(1))
         if((atemp.ne.'single').and.(atemp.ne.'double')) go to 9300
         atemp=cline(lw(2):rw(2))
         if((atemp.ne.'point').and.(atemp.ne.'point')) go to 9300
         if(iflag.eq.0)then
           npar=0
           obsnow=.false.
           nobs=0
           iline=1
           do
             iline=iline+1
             read(10,'(a)',end=250) cline
             if(cline.eq.' ') cycle
             if(.not.obsnow)then
               call linspl(ifail,4,lw,rw,cline)
               if(ifail.eq.0) then
                 npar=npar+1
               else
                 call linspl(ifail,2,lw,rw,cline)
                 if(ifail.eq.0)then
                   obsnow=.true.
                   nobs=nobs+1
                 else
                   call writint(aline,iline)
                   write(amessage,245) trim(aline),trim(bfile)
245                format(' Insufficient entries on line ',a,' of ',
     +             'file ',a,'.')
                   go to 9800
                 end if
               end if
             else
               call linspl(ifail,2,lw,rw,cline)
               if(ifail.eq.0)then
                 nobs=nobs+1
               else
                 call writint(aline,iline)
                 write(amessage,245) trim(aline),trim(bfile)
                 go to 9800
               end if
             end if
           end do
250        continue
           if(npar.eq.0)then
             write(amessage,260) trim(bfile)
260          format(' No parameters listed in file ',a,'.')
             go to 9800
           end if
           if(nobs.eq.0)then
             write(amessage,261) trim(bfile)
261          format(' No observations listed in file ',a,'.')
             go to 9800
           end if
           allocate(apar(npar),pval(npar,nlist),stat=ierr)
           if(ierr.ne.0) go to 9200
           allocate(aobs(nobs),oval(nobs,nlist),stat=ierr)
           if(ierr.ne.0) go to 9200
           rewind(unit=10)
           read(10,'(a)') cline
         end if
         jpar=1
         jobs=1
         do ipar=1,npar
           read(10,*,err=9400,end=9400) aapar,rtemp,
     +     scale,offset
           call lowcas(aapar)
           if(iflag.eq.0)then
             apar(ipar)=aapar
             pval(ipar,ilist)=rtemp*scale+offset
           else
             call whichone(ifail,npar,jpar,apar,aapar)
             if(ifail.ne.0)then
               write(amessage,270)
270            format(' PAROBS files do not cite the ',
     +         'same parameters.')
               go to 9800
             end if
             pval(jpar,ilist)=rtemp*scale+offset
           end if
         end do
         do iobs=1,nobs
           read(10,*,err=9450,end=9450) aaobs,rtemp
           call lowcas(aaobs)
           if(iflag.eq.0)then
             aobs(iobs)=aaobs
             oval(iobs,ilist)=rtemp
           else
             call whichone(ifail,nobs,jobs,aobs,aaobs)
             if(ifail.ne.0)then
               write(amessage,271)
271            format(' PAROBS files do not cite the ',
     +         'same observations.')
               go to 9800
             end if
             oval(jobs,ilist)=rtemp
           end if
         end do
         if(iflag.eq.0) iflag=1
         close(unit=10)
         write(6,280) trim(bfile)
280      format(' - file ',a,' read ok.')
       end do
       if(iflag.eq.0)then
         write(amessage,281) trim(parfile)
281      format(' No PAROBS files of type "',a,'" were found.')
         go to 9800
       end if

       allocate(aval(nlist),stat=ierr)
       call addquote(outfile,aoutfile)
       write(6,290) trim(aoutfile)
290    format(/,' - writing file ',a,'...')
       open(unit=20,file=outfile,recl=26+16*nlist,
     + action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,300) trim(aoutfile)
300      format(' Cannot open file ',a,' to write output parameter ',
     +   'value table.')
         go to 9800
       end if
       write(20,310) (inum(ilist),ilist=1,nlist)
310    format(t16,1000(2x,i10,4x))
       do ipar=1,npar
         do ilist=1,nlist
           if(ifound(ilist).eq.0)then
             aval(ilist)='     -----'
           else
             write(aval(ilist),'(1pg14.7)') pval(ipar,ilist)
           end if
         end do
         write(20,230) trim(apar(ipar)),
     +   (trim(aval(ilist)),ilist=1,nlist)
230      format(1x,a,t16,1000(1x,a14,1x))
       end do
       close(unit=20)
       write(6,240) trim(aoutfile)
240    format(' - file ',a,' written ok.')

       call addquote(outfile1,aoutfile1)
       write(6,290) trim(aoutfile1)
       open(unit=20,file=outfile1,recl=26+16*nlist,
     + action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,301) trim(aoutfile1)
301      format(' Cannot open file ',a,' to write model output ',
     +   'value table.')
         go to 9800
       end if
       write(20,311) (inum(ilist),ilist=1,nlist)
311    format(t24,1000(2x,i10,4x))
       do iobs=1,nobs
         do ilist=1,nlist
           if(ifound(ilist).eq.0)then
             aval(ilist)='     -----'
           else
             write(aval(ilist),'(1pg14.7)') oval(iobs,ilist)
           end if
         end do
         write(20,231) trim(aobs(iobs)),
     +   (trim(aval(ilist)),ilist=1,nlist)
231      format(1x,a,t24,1000(1x,a14,1x))
       end do
       close(unit=20)
       write(6,240) trim(aoutfile1)

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' MULPAROBSTAB is run using the command:',/)
       write(6,8920)
8920   format('     mulpartab parfile listfile outfile1 outfile2',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     parfile  is a generic PAROBS file name,')
       write(6,8950)
8950   format('     listfile contains a list of integer indices,')
       write(6,8960)
8960   format('     outfile1 is the name for the tabular parameter ',
     + 'value file, and')
       write(6,8970)
8970   format('     outfile2 is the name of the tabular model output ',
     + 'value file.')

       go to 9999

9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800

9250   write(amessage,9260) trim(aline),trim(alistfile)
9260   format(' Illegal format for integers at line ',a,
     + ' of integer list file ',a,'.')
       go to 9800

9300   write(amessage,9310) trim(bfile)
9310   format(' First line of PAROBS file ',a,' should be ',
     + '"single" or "double" followed by "point" or "nopoint".')
       go to 9800

9400   call writint(anum,ipar)
       write(amessage,9410) trim(anum),trim(bfile)
9410   format(' Error reading data for parameter number ',a,
     + ' from file ',a,'.')
       go to 9800

9450   call writint(anum,iobs)
       write(amessage,9460) trim(anum),trim(bfile)
9460   format(' Error reading data for observation number ',a,
     + ' from file ',a,'.')
       go to 9800


9800   continue
       call writmess(6,amessage)

9900   continue
       deallocate(ifound,pval,apar,aval,stat=ierr)

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




        subroutine writint(atemp,ival)

c       Subroutine WRITINT writes an integer to a character variable.

        integer*4 ival
        character*6 afmt
        character*(*) atemp

        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(atemp)
        write(atemp,afmt)ival
        atemp=adjustl(atemp)
        return
        end


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


