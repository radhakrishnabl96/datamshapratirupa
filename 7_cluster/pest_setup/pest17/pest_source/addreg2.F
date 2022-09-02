       program addreg2

C -- Program ADDREG2 adds "preferred value regularisation" to the a PEST control file.

       implicit none

       logical   :: skipline,ltemp
       integer   :: ifail,nb,ierr,pestmode,i,ipar,j,iwarn,k,newgroup
       integer   :: npar,nobs,npargp,nprior,nobsgp,ntplfle,ninsfle
       integer   :: iregadj,nn,jjflag
       integer   :: lw(10),rw(10)
       double precision :: phimlim,phimaccept,fracphim

       character*4    :: aext
       character*12   :: agroup,anew,aversion
       character*15   :: atemp1,atemp2,atrans
       character*200  :: pestinfile,pestoutfile,apestinfile,
     + apestoutfile,casename
       character*500  :: amessage
       character*1000 :: cline,dline

       integer, allocatable          :: itrans(:),npg(:),iadd(:)
       double precision, allocatable :: pval(:)
       character*12, allocatable     :: apar(:),apargp(:),aobsgp(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' ADDREG2 version ',a,'. Watermark Numerical ',
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
       call linspl(ifail,6,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       pestinfile=cline(lw(1):rw(1))
       call remchar(pestinfile,char(211))
       pestoutfile=cline(lw(2):rw(2))
       call remchar(pestoutfile,char(211))
       call drealrd(ifail,cline(lw(3):rw(3)),phimlim)
       if(ifail.ne.0) go to 8900
       fracphim=-1.0d301
       iregadj=-9999
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0)then
         call drealrd(ifail,cline(lw(4):rw(4)),fracphim)
         if(ifail.ne.0) go to 8900
         call linspl(ifail,5,lw,rw,cline)
         if(ifail.eq.0)then
           call intread(ifail,cline(lw(5):rw(5)),iregadj)
           if(ifail.ne.0) go to 8900
         end if
       end if
       if(phimlim.le.0.0d0) then
         write(amessage,7)
7        format('The value supplied for PHIMLIM must be positive.')
         go to 9890
       end if
       if(fracphim.gt.-1.0d300)then
         if((fracphim.lt.0.0d0).or.(fracphim.gt.0.2d0))then
           write(amessage,8)
8          format('The value supplied for FRACPHIM must be positive ',
     +     'and less than 0.2.')
           go to 9890
         end if
       end if
       if(iregadj.gt.-9999)then
         if((iregadj.lt.0).or.(iregadj.gt.5))then
           write(amessage,9)
9          format('The value supplied for IREGADJ must be between ',
     +     '0 and 5 (inclusive).')
           go to 9890
         end if
       end if
       if(iregadj.eq.-9999) iregadj=1
       if(fracphim.lt.-1.0d300) fracphim=0.05

#ifndef UNIX
       call lowcas(pestinfile)
       call lowcas(pestoutfile)
#endif

       phimaccept=phimlim*1.02

C -- Further analysis is done on the input and output PEST filenames.

       nb=len_trim(pestinfile)
       nb=nb-3
       if(nb.lt.1)nb=1
       aext=pestinfile(nb:)
       call lowcas(aext)
       if(aext.eq.'.pst')then
         if(nb.eq.1) go to 8900
         casename=pestinfile(1:nb-1)
       else
         casename=pestinfile
         pestinfile=trim(casename)//'.pst'
       end if

       nb=len_trim(pestoutfile)
       nb=nb-3
       if(nb.lt.1)nb=1
       aext=pestoutfile(nb:)
       call lowcas(aext)
       if(aext.ne.'.pst')then
         pestoutfile=trim(pestoutfile)//'.pst'
       end if

C -- Quotes are added to filenames if necessary.

       call addquote(pestinfile,apestinfile)
       call addquote(pestoutfile,apestoutfile)

C -- The old and new PEST control files are opened.

       open(unit=10,file=pestinfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,30) trim(apestinfile)
30       format(' Cannot open PEST control file ',a,'.')
         go to 9890
       end if

       open(unit=20,file=pestoutfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,40) trim(apestoutfile)
40       format(' Cannot open file ',a,' for output.')
         go to 9890
       end if

C -- The existing PEST control file is opened and partly read.

       read(10,*,err=9000,end=9000) atemp1
       call lowcas(atemp1)
       if(atemp1.ne.'pcf') go to 9000
41     continue
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 41
42     continue
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 42
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       atemp1=cline(lw(1):rw(1))
       atemp2=cline(lw(2):rw(2))
       call lowcas(atemp2)
       if(atemp2.eq.'estimation')then
         pestmode=1
       else if(atemp2.eq.'prediction')then
         pestmode=2
       else if (atemp2(1:5).eq.'regul')then
         pestmode=3
       else if (atemp2(1:5).eq.'paret')then
         pestmode=4
       else
         write(amessage,165) trim(apestinfile)
165      format(' Unknown value for PESTMODE variable on line 3 of ',
     +   'PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.ne.1)then
         write(amessage,164)
164      format(' ADDREG2 requires that the first PEST control ',
     +   'have PESTMODE set to "estimation".')
         go to 9890
       end if
166    continue
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 166
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(1):rw(1)),npar)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(2):rw(2)),nobs)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(3):rw(3)),npargp)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(4):rw(4)),nprior)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(5):rw(5)),nobsgp)
       if(ifail.ne.0) go to 9000
       if(nprior.ne.0)then
         write(amessage,167) trim(apestinfile)
167      format(' PEST control file ',a,' already contains prior ',
     +   'information. This is not allowed by ADDREG2.')
         go to 9890
       end if
       if(npar.eq.0)then
         write(amessage,168) trim(apestinfile)
168      format(' Number of parameters NPAR supplied as zero in ',
     +   'PEST control file ',a,'.')
         go to 9890
       end if
169    continue
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 169
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(1):rw(1)),ntplfle)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(2):rw(2)),ninsfle)
       if(ifail.ne.0) go to 9000

C -- Parameter group names are now read.
       allocate(apargp(npargp),iadd(npargp),stat=ierr)
       if(ierr.ne.0) go to 9200
       do
         read(10,'(a)',err=9000,end=9000) cline
         cline=adjustl(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'parameter gr').ne.0) go to 180
           if(index(cline,'group defin').ne.0) go to 180
         end if
       end do
180    do i=1,npargp
181      continue
         read(10,'(a)',err=9000,end=9000) cline
         if(skipline(cline)) go to 181
         call linspl(ifail,1,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         apargp(i)=cline(lw(1):rw(1))
         call lowcas(apargp(i))
       end do

C -- Parameter data is now read.

       allocate(apar(npar),itrans(npar),pval(npar),npg(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
182    continue
       read(10,'(a)') cline
       if(skipline(cline)) go to 182
       cline=adjustl(cline)
       call lowcas(cline)
       if((cline(1:1).ne.'*').or.
     +    (index(cline,'parameter da').eq.0)) go to 9000
       do i=1,npar
189      continue
         read(10,'(a)',err=9000,end=9000) cline
         if(skipline(cline)) go to 189
         cline=adjustl(cline)
         call lowcas(cline)
         nn=index(cline,'=')
         if(nn.eq.1) go to 9000
         if(nn.ne.0) go to 189
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         if(cline(lw(2):rw(2)).eq.'file_parameter') go to 189
         call linspl(ifail,7,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         apar(i)=cline(lw(1):rw(1))
         atrans=cline(lw(2):rw(2))
         agroup=cline(lw(7):rw(7))
         call drealrd(ifail,cline(lw(4):rw(4)),pval(i))
         if(ifail.ne.0) go to 9000
         call lowcas(apar(i))
         call lowcas(agroup)
         call lowcas(atrans)
         do j=1,npargp
           if(agroup.eq.apargp(j)) then
             npg(i)=j
             go to 190
           end if
         end do
         go to 9000
190      continue
         if(atrans.eq.'log')then
           itrans(i)=1
         else if(atrans.eq.'none')then
           itrans(i)=0
         else if(atrans.eq.'fixed')then
           itrans(i)=-1000000
         else if(atrans.eq.'tied')then
           itrans(i)=-1
         else
           go to 9000
         end if
         if(itrans(i).eq.1)then
           if(pval(i).le.0.0d0)then
             write(amessage,195) trim(apar(i)),trim(apestinfile)
195          format(' Parameter "',a,'" is log transformed, yet it ',
     +       'has a negative initial value in PEST control file ',
     +       a,'.')
             go to 9890
           end if
         end if
       end do

C -- The names of observation groups are now read.

       allocate(aobsgp(nobsgp),stat=ierr)
       if(ierr.ne.0) go to 9200
       do
         read(10,'(a)',err=9000,end=9000) cline
         cline=adjustl(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'observation gr').ne.0) go to 210
         end if
       end do
210    continue
       do i=1,nobsgp
211      continue
         read(10,'(a)',err=9000,end=9000) cline
         if(skipline(cline)) go to 211
         call linspl(ifail,1,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         aobsgp(i)=cline(lw(1):rw(1))
         call lowcas(aobsgp(i))
       end do

! -- We now see whether we need to add new observation group names to the
!    PEST control file and how many.

       iwarn=0
       iadd=0           ! an array
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           j=npg(ipar)
           anew='regul_'//apargp(j)(1:6)
           do k=1,nobsgp
             if(anew.eq.aobsgp(k)) then
               iwarn=1
               go to 220
             end if
           end do
           iadd(j)=1
220        continue
         end if
       end do
       newgroup=0
       do i=1,npargp
         if(iadd(i).eq.1) newgroup=newgroup+1
       end do

C -- The number of parameters for which we require prior information is now
C    established.

       nprior=0
       do ipar=1,npar
         if(itrans(ipar).ge.0) nprior=nprior+1
       end do
       if(nprior.eq.0)then
         write(amessage,230) trim(apestinfile)
230      format(' No adjustable parameters are cited in PEST ',
     +   'control file ',a,'.')
         go to 9890
       end if

C -- The original PEST control file is re-wound and information transferred to the
C    new PEST control file.

       rewind(unit=10)
       read(10,*)
       write(20,250)
250    format('pcf')
251    continue
       read(10,'(a)') cline
       write(20,'(a)') trim(cline)
       if(skipline(cline)) go to 251
252    continue
       read(10,'(a)') cline
       dline=cline
       if(skipline(cline)) then
         write(20,'(a)') trim(dline)
         go to 252
       end if
       call linspl(ifail,2,lw,rw,dline)
       if(dline(rw(2)+1:).eq.' ')then
         write(20,260)
260      format('restart regularisation')
       else
         write(20,261) trim(dline(rw(2)+1:))
261      format('restart regularisation ',a)
       end if
262    continue
       read(10,'(a)') cline
       dline=cline
       if(skipline(cline))then
         write(20,'(a)') trim(dline)
         go to 262
       end if
       call linspl(ifail,5,lw,rw,dline)
       if(ifail.ne.0) go to 9000
       if(dline(rw(5)+1:).eq.' ')then
         write(20,270) npar,nobs,npargp,nprior,nobsgp+newgroup
270      format(5i8)
       else
         write(20,271) npar,nobs,npargp,nprior,nobsgp+newgroup,
     +   trim(dline(rw(5)+1:))
271      format(5i8,1x,a)
       end if
       do
         read(10,'(a)') cline
         write(20,'(a)') trim(cline)
         cline=adjustl(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'observation gr').ne.0) go to 300
         end if
       end do
300    continue
       do i=1,nobsgp
301      continue
         read(10,'(a)') cline
         write(20,'(a)') trim(cline)
         if(skipline(cline)) go to 301
       end do
       if(newgroup.ne.0)then
         do i=1,npargp
           if(iadd(i).ne.0)then
             anew='regul_'//apargp(i)(1:6)
             write(20,'(a)') trim(anew)
           end if
         end do
       end if

! -- The remainder of the PEST control file is now transferred.

       do
         read(10,'(a)',err=9000,end=9000) cline
         write(20,'(a)') trim(cline)
         cline=adjustl(cline)
         if(cline(1:1).eq.'*') then
           if(index(cline,'model in').ne.0) go to 400
         end if
       end do
400    continue
       jjflag=0
       ltemp=skipline(cline)
       if(cline.eq.'* model input') jjflag=1
       do i=1,ntplfle
4001     continue
         read(10,'(a)',err=9000,end=9000) cline
         write(20,'(a)') trim(cline)
         if(skipline(cline)) go to 4001
       end do
       if(jjflag.eq.1)then
4002     continue
         read(10,'(a)',err=9000,end=9000) cline
         write(20,'(a)') trim(cline)
         if(skipline(cline)) go to 4002
         call lowcas(cline)
         if(cline.ne.'* model output') then
           write(amessage,4003) trim(apestinfile)
4003       format(' Error reading "* model output" section of ',
     +     'file ',a,'.')
           go to 9890
         end if
       end if
       do i=1,ninsfle
402      continue
         read(10,'(a)',err=9000,end=9000) cline
         write(20,'(a)') trim(cline)
         if(skipline(cline)) go to 402
       end do

C -- The new prior information equations are now added.

       write(20,401)
401    format('* prior information')
       do ipar=1,npar
         if(itrans(ipar).lt.0) cycle
         j=npg(ipar)
         anew='regul_'//apargp(j)(1:6)
         if(itrans(ipar).eq.0)then
           write(20,430) trim(apar(ipar)),trim(apar(ipar)),pval(ipar),
     +     trim(anew)
430        format(a,t15,'1.0 * ',a,' = ',1pg14.7,'  1.0 ',a)
         else
           write(20,440) trim(apar(ipar)),trim(apar(ipar)),
     +     log10(pval(ipar)),trim(anew)
440        format(a,t15,'1.0 * log(',a,') = ',1pg14.7,'  1.0 ',a)
         end if
       end do

C --  We add a regularisation section to the PEST control file.

600    continue
       write(20,620)
620    format('* regularisation')
       write(20,630) phimlim,phimaccept,fracphim
630    format(3(1x,1pg14.7))
       write(20,640)
640    format(' 1.0   1.0e-10    1.0e10')
       if(iregadj.le.3)then
         write(20,650) iregadj
650      format(' 1.3   1.0e-2    ',i2)
       else if (iregadj.eq.4)then
         write(20,659) iregadj
659      format(' 1.3   1.0e-2    ',i2,'  1  20.0')
       else if(iregadj.eq.5)then
         write(20,661) iregadj
661      format(' 1.3   1.0e-2    ',i2,'  1  20.0   1.0e-5')
       end if

C -- Is there a distribution files section?

       do
         read(10,'(a)',err=655,end=655) cline
         cline=adjustl(cline)
         if(cline(1:1).eq.'*') then
           call lowcas(cline)
           if(index(cline,'distribut').ne.0) go to 601
         end if
         if(cline.ne.' ')then
           if(cline(1:1).ne.'*')then
             write(20,'(a)') trim(cline)
           end if
         end if
       end do
601    continue
       write(20,'(a)') trim(cline)
       do
         read(10,'(a)',err=655,end=655) cline
         write(20,'(a)') trim(cline)
       end do
655    continue

C -- Finishing up

       close(unit=10)
       close(unit=20)
       write(6,660) trim(apestinfile)
660    format(' - file ',a,' read ok.')
       write(6,670) trim(apestoutfile)
670    format(' - file ',a,' written ok.')

       if(iwarn.ne.0)then
         write(amessage,680) trim(apestoutfile), trim(apestinfile)
680      format(' WARNING: at least one observation ',
     +   'group name employed in regularisation prior ',
     +   'information added to file ',a,' was already cited in ',
     +   'the "observation groups" section of file ',a,'.')
         call writmess(6,amessage)
       end if

       go to 9900


8900   continue
       write(6,8910)
8910   format(/,' ADDREG2 is run using the command:',/)
       write(6,8920)
8920   format('     addreg2 pestinfile pestoutfile phimlim ',
     + '[fracphim] [iregadj]',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     pestinfile  is an existing ',
     + 'PEST control file,')
       write(6,8950)
8950   format('     pestoutfile is a new PEST control file, ')
       write(6,8960)
8960   format('     phimlim     is the target measurement objective ',
     + 'function,')
       write(6,8970)
8970   format('     fracphim    is FRACPHIM regularisation control ',
     + 'variable, and')
       write(6,8980)
8980   format('     iregadj     is IREGADJ regularisation control ',
     + 'variable.')

       go to 9900


9000   write(amessage,9010) trim(apestinfile)
9010   format(' Error encountered when reading PEST control file ',
     + a,' - check this file with PESTCHEK.')
       go to 9890
9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9890   continue
       amessage=' '//trim(amessage)
       call writmess(6,amessage)

9900   deallocate(itrans,npg,iadd,pval,apar,apargp,aobsgp,stat=ierr)

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
