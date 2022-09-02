       program jcoblank

! --   Program JCOBLANK creates a JCO blanking file and a simultaneous increments file.

       use pestdata
       implicit none

       logical, external :: string_match
       logical lexist,action
       integer ifail,i,j,ierr,iline,zerocount,ii,iii,jjfail
       integer ncount,icom
       integer pfile,irestart
       integer ies,ipar,ipgp,iogp,iobs,jpar,irow,iobsgp
       integer nrun,ctype,irun
       integer itemp1,itemp2,bn,icount,jcount
       integer idc,itemp
       integer lw(10),rw(10)
       integer itype(2)
       double precision dtemp,fracpar,fracobs
       double precision one
       character*1 ajz,asi,acs
       character*5 aext
       character*10 aline,how
       character*12 aversion
       character*12 aapar
       character*20 azero,aaobs
       character*30 atype(2)
       character*25 aterm,arun,acom
       character*30 aatype
       character*256 pestfile,jcoblankfile,cleanfile,afile
       character*256 sifile,jcofile,pjcoblankfile,locjcofile

       logical, allocatable :: xb(:,:),bbb(:)
       integer, allocatable :: runpar(:)
       integer, allocatable :: writeiobs(:)

! -- Initialisation

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,9) trim(aversion)
9      format(' JCOBLANK Version ',a,'. Watermark Numerical Computing.')

! -- The PEST control file is read.

       write(6,*)
10     write(6,12,advance='no')
12     format(' Enter name of existing PEST control file: ')
       read(5,'(a)') pestfile
       if(pestfile.eq.' ') go to 10
       call remchar(pestfile,'"')
       call remchar(pestfile,'''')
       pestfile=adjustl(pestfile)
       i=len_trim(pestfile)
       j=i-4
       if(j.lt.1)j=1
       aext=pestfile(j:i)
       if(index(aext,'.').eq.0) then
         pestfile=trim(pestfile)//'.pst'
       end if
       i=len_trim(pestfile)
       j=i-3
       if(j.lt.1)j=1
       aext=pestfile(j:i)
       call lowcas(aext)
       if(aext(1:4).ne.'.pst')then
         write(6,13)
13       format(' PEST control file must have an extension of ',
     +   '".pst" - try again.')
         go to 10
       end if
       call addquote(pestfile,afile)
       ctlfile=pestfile
       write(6,1235) trim(afile)
1235   format(' - reading PEST control file ',a,'....')
       irestart=0
       p1start=0
       pfile=0
       call read_pest_data(ifail,irestart,p1start,pfile,pestfile)
       if(ifail.ne.0) go to 9890
       if(maxcompdim.gt.1)then
         write(errmsg,1240) trim(afile)
1240     format('JCOBLANK requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if(nespar.eq.0)then
         write(errmsg,1272) trim(afile)
1272     format('There are no adjustable parameters cited in PEST ',
     +   'control file ',a,'.')
         go to 9890
       end if
       if(nxrow.eq.0)then
         write(errmsg,1273) trim(afile)
1273     format('No observations or prior information items are ',
     +   'cited in PEST control file ',a,'.')
         go to 9890
       end if
       write(6,1275) trim(afile)
1275   format(' - file ',a,' read ok.')

! -- We deallocate the Jacobian matrix to save memory. Maybe it will be re-allocated later.

       deallocate(x,stat=ierr)

! -- We now allocate the blanking matrix.

       allocate(xb(nobs,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

! -- Compress the apar, pargnme and dercom arrays.

       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           apar(ies)=apar(ipar)
           ipargp(ies)=ipargp(ipar)
           dercom(ies)=dercom(ipar)
         end if
       end do

! -- We find out what type of blanking file we have.

       write(6,*)
20     write(6,25,advance='no')
25     format(' Obtain blanking information from a zeroing file or ',
     + 'a JCO file [z/j]: ')
       read(5,'(a)') ajz
       if(ajz.eq.' ') go to 20
       call lowcas(ajz)
       if((ajz.ne.'z').and.(ajz.ne.'j')) go to 20
       if(ajz.eq.'j')then
         do iobsgp=1,nobsgp
           if(covind(iobsgp))then
             do iobs=1,nobs
               if(nobgnm(iobs).eq.iobsgp) go to 9290
             end do
           end if
         end do
27       write(6,28,advance='no')
28       format(' Enter name of JCO file: ')
         read(5,'(a)') jcofile
         if(jcofile.eq.' ') go to 27
         call remchar(jcofile,'"')
         call remchar(jcofile,'''')
         jcofile=adjustl(jcofile)
         i=len_trim(jcofile)
         j=i-4
         if(j.lt.1)j=1
         aext=jcofile(j:i)
         if(index(aext,'.').eq.0) then
           jcofile=trim(jcofile)//'.jco'
         end if
         i=len_trim(jcofile)
         j=i-3
         if(j.lt.1)j=1
         aext=jcofile(j:i)
         call lowcas(aext)
         if(aext.ne.'.jco')then
           write(6,29)
29         format(' JCO file must have an extension of ',
     +     '".jco" - try again.')
           go to 27
         end if
         call addquote(jcofile,afile)
         write(6,291) trim(afile)
291      format(' - opening file ',a,'...')
         how='read'
         call open_unformatted_file(ifail,15,how,jcofile,errmsg)
         if(ifail.ne.0) then
           write(6,292) trim(afile)
292        format(' Cannot open file ',a,' - try again.')
           go to 27
         end if
         write(6,*)
1140     write(6,1150,advance='no')
1150     format(' Enter fraction-of-max for observation blanking: ')
         read(5,*,err=1140) fracobs
         if((fracobs.gt.1.0).or.(fracobs.lt.0.0d0)) go to 1140
1160     write(6,1170,advance='no')
1170     format(' Enter fraction-of-max for parameter blanking: ')
         read(5,*,err=1160) fracpar
         if((fracpar.gt.1.0).or.(fracpar.lt.0.0d0)) go to 1160
         ctype=0

       else

! -- We obtain the name of the zero file.

30       write(6,32,advance='no')
32       format(' Enter name of zeroing file: ')
         read(5,'(a)') cleanfile
         if(cleanfile.eq.' ') go to 30
         call remchar(cleanfile,'"')
         call remchar(cleanfile,'''')
         call addquote(cleanfile,afile)
         write(6,291) trim(afile)
         open(unit=10,file=cleanfile,status='old',action='read',
     +   iostat=ierr)
         if(ierr.ne.0)then
           write(6,292) trim(afile)
           go to 30
         end if
         ctype=1
       end if

! -- We obtain the name of the blanking file.

       write(6,*)
49     write(6,601,advance='no')
601    format(' Enter name for blanking file: ')
       read(5,'(a)') jcoblankfile
       if(jcoblankfile.eq.' ') go to 49
       call remchar(jcoblankfile,'"')
       call remchar(jcoblankfile,'''')
       i=len_trim(jcoblankfile)
       j=i-3
       if(j.lt.1)j=1
       aext=jcoblankfile(j:i)
       call lowcas(aext)
       if(aext.ne.'.jcz ')then
         write(6,64)
64       format(' Filename extension must be ".jcz" - ',
     +   'try again.')
         go to 49
       end if

! -- We obtain the name of the localization file.

4911   write(6,6012,advance='no')
6012   format(' Enter name for localization JCO file: ')
       read(5,'(a)') locjcofile
       if(locjcofile.eq.' ') go to 4911
       call remchar(locjcofile,'"')
       call remchar(locjcofile,'''')
       i=len_trim(locjcofile)
       j=i-3
       if(j.lt.1)j=1
       aext=locjcofile(j:i)
       call lowcas(aext)
       if(aext.ne.'.jco ')then
         write(6,6411)
6411     format(' Filename extension must be ".jco" - ',
     +   'try again.')
         go to 4911
       end if

! -- We obtain the name of the blanking pattern file.

491    write(6,6011,advance='no')
6011   format(' Enter name for blanking pattern file: ')
       read(5,'(a)') pjcoblankfile
       if(pjcoblankfile.eq.' ') go to 491
       call remchar(pjcoblankfile,'"')
       call remchar(pjcoblankfile,'''')
       i=len_trim(pjcoblankfile)
       j=i-3
       if(j.lt.1)j=1
       aext=pjcoblankfile(j:i)
       call lowcas(aext)
       if(aext.eq.'.jcz ')then
         write(6,65)
65       format(' The filename extension must not ',
     +   'be ".jcz" - try again.')
         go to 491
       else if(aext.eq.'.jco ')then
         write(6,66)
66       format(' The filename extension must not ',
     +   'be ".jco" - try again.')
         go to 491
       end if

! -- We find out whether a simultaneous increment file is to be written.

       write(6,*)
401    write(6,42,advance='no')
42     format(' Write simultaneous increment file? [y/n]: ')
       read(5,'(a)') asi
       if(asi.eq.' ') go to 401
       call lowcas(asi)
       if((asi.ne.'y').and.(asi.ne.'n')) go to 401
       if(asi.eq.'y') then
43       write(6,41,advance='no')
41       format(' Enter name for simultaneous increment file: ')
         read(5,'(a)') sifile
         if(sifile.eq.' ') go to 43
         call remchar(sifile,'"')
         call remchar(sifile,'''')
44       write(6,451,advance='no')
451      format(' Adjust blanking to conform with ',
     +   'simultaneous increments? [y/n]: ')
         read(5,'(a)') acs
         if(acs.eq.' ') go to 44
         call lowcas(acs)
         if((acs.ne.'y').and.(acs.ne.'n')) go to 44
       else
         sifile=' '
       end if

! -- Next the existing blanking, localization and blanking pattern files are deleted.

       inquire (file=jcoblankfile,exist=lexist)
       if(lexist)then
         how='read'
         call open_unformatted_file(ifail,30,how,jcoblankfile,errmsg)
         close(unit=30,status='delete',iostat=ierr)
       end if
       inquire (file=locjcofile,exist=lexist)
       if(lexist)then
         how='read'
         call open_unformatted_file(ifail,30,how,locjcofile,errmsg)
         close(unit=30,status='delete',iostat=ierr)
       end if
       inquire (file=pjcoblankfile,exist=lexist)
       if(lexist)then
         open(unit=30,file=pjcoblankfile,status='old',iostat=ierr)
         close(unit=30,status='delete',iostat=ierr)
       end if

       write(6,*)
       if(ctype.eq.0)then
         call addquote(jcofile,afile)
         write(6,1100) trim(afile)
1100     format(' - reading Jacobian matrix file ',a,'...')
         read(15,err=9250,end=9250)itemp1,itemp2
         if((abs(itemp1).ne.nespar).or.
     +      (abs(itemp2).ne.nxrow)) go to 9270
         bn=abs(itemp2)
         if(itemp1.lt.0)then
           continue
         else
           write(errmsg,1120) trim(afile)
1120        format('Jacobian matrix file ',a,' was not produced by ',
     +     'version 8 or later of PEST; use program JCOTRANS to ',
     +     'convert this to new format.')
           go to 9890
         end if
         allocate(x(nobs,nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         x=0.0d0    ! an array
         read(15,err=9250,end=9250)icount
         do i=1,icount
           read(15,err=9250,end=9250) j,dtemp
           ies=(j-1)/bn+1
           irow=j-(ies-1)*bn
           if(irow.le.nobs) x(irow,ies)=dtemp
         end do
         do i=1,nespar
           read(15,err=9250,end=9250) aapar
           if(aapar.ne.apar(i)) go to 9270
         end do
         do i=1,nobs
           read(15,err=9250,end=9250) aaobs
           call lowcas(aaobs)
           if(aaobs.ne.aobs(i)) then
             go to 9270
           end if
         end do
         close(unit=15)
         write(6,2005) trim(afile)

! -- We multiply rows of the Jacobian matrix by weights.

         do iobs=1,nobs
           dtemp=owght(iobs)
           if(dtemp.gt.0.0d0)then
             dtemp=sqrt(dtemp)
           else
             dtemp=0.0d0
           end if
           if(dtemp.gt.0.0d0)then
             do ies=1,nespar
               x(iobs,ies)=x(iobs,ies)*dtemp
             end do
           end if
         end do

! -- We now zero the pseudo Jacobian matrix as appropriate.

         zerocount=0
         xb=.TRUE.                      ! an array
         do iobs=1,nobs
           if(owght(iobs).le.0.0d0)then
             do ies=1,nespar
               xb(iobs,ies)=.false.
             end do
           end if
         end do

         if(fracobs.gt.0.0d0)then
           do iobs=1,nobs
             if(owght(iobs).gt.0.0d0)then
               dtemp=-1.0d300
               do ies=1,nespar
                 if(abs(x(iobs,ies)).gt.dtemp) dtemp=abs(x(iobs,ies))
               end do
               dtemp=dtemp*fracobs
               do ies=1,nespar
                 if(abs(x(iobs,ies)).le.dtemp) then
                   if(xb(iobs,ies)) zerocount=zerocount+1
                   xb(iobs,ies)=.FALSE.
                 end if
               end do
             end if
           end do
         end if
         if(fracpar.gt.0.0d0)then
           do ies=1,nespar
             dtemp=-1.0d300
             do iobs=1,nobs
               if(owght(iobs).gt.0.0d0)then
                 if(abs(x(iobs,ies)).gt.dtemp) dtemp=abs(x(iobs,ies))
               end if
             end do
             dtemp=dtemp*fracpar
             do iobs=1,nobs
               if(owght(iobs).gt.0.0d0)then
                 if(abs(x(iobs,ies)).le.dtemp) then
                   if(xb(iobs,ies)) zerocount=zerocount+1
                   xb(iobs,ies)=.FALSE.
                 end if
               end if
             end do
           end do
         end if
         call writint(azero,zerocount)
         write(6,2010) trim(azero)
         go to 2011
       end if

! -- The zeroing file is read.
!    At the same time, psuedo Jacobian matrix zeroing is undertaken.

       call addquote(cleanfile,afile)
       write(6,21) trim(afile)
21     format(' - reading and processing file ',a,'...')
       jjfail=1
       iline=0
       zerocount=0
       do
         iline=iline+1
         read(10,'(a)',err=9500,end=2000) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         if(cline(1:1).ne.'*')then
           call writint(aline,iline)
           write(errmsg,40) trim(aline),trim(afile)
40         format('First character on line ',a,' of file ',a,
     +     ' expected to be "*".')
           go to 9890
         end if
         go to 45
       end do

45     continue
       cline=adjustl(cline(2:))
       call lowcas(cline)
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       if(cline(lw(1):rw(1)).ne.'start') go to 9000
       if(cline(lw(2):rw(2)).ne.'all') go to 9000
       if(cline(lw(3):rw(3)).eq.'zero')then
         xb=.FALSE.        ! an array
       else if(cline(lw(3):rw(3)).eq.'nonzero')then
         xb=.TRUE.         ! an array
       else
         go to 9000
       end if

! -- Initialize

       jjfail=0
       iobs=0
       ipar=0
       ipgp=0
       iogp=0

! -- Do the work

       do
         iline=iline+1
         read(10,'(a)',err=9500,end=2000) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         if(cline(1:1).ne.'*')then
           call writint(aline,iline)
           write(errmsg,40) trim(aline),trim(afile)
           go to 9890
         end if
         go to 50
       end do

! -- We now read a block of the file

50     continue
       cline=adjustl(cline(2:))
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 9550
       call lowcas(cline)
       if(cline(lw(1):rw(1)).eq.'zero')then
         action=.FALSE.
       else if(cline(lw(1):rw(1)).eq.'nonzero')then
         action=.TRUE.
       else
         call writint(aline,iline)
         write(errmsg,51) trim(aline),trim(afile)
51       format('First entry on line ',a,' of file ',a,
     +   'should be "zero" or "nonzero".')
         go to 9890
       end if
       do i=1,2
         if(i.eq.1)then
           aatype='Second'
         else
           aatype='Third'
         end if
         atype(i)=cline(lw(i+1):rw(i+1))
         if((atype(i).eq.'parameter').or.(atype(i).eq.'parameters'))then
           itype(i)=1
           atype(i)='adjustable parameter'
         else if((atype(i).eq.'parameter_group').or.
     +   (atype(i).eq.'parameter_groups'))then
           itype(i)=2
           atype(i)='parameter group'
         else if((atype(i).eq.'observation').or.
     +           (atype(i).eq.'observations'))then
           itype(i)=-1
           atype(i)='observation'
         else if((atype(i).eq.'observation_group').or.
     +   (atype(i).eq.'observation_groups'))then
           itype(i)=-2
           atype(i)='observation group'
         else
           call writint(aline,iline)
           write(errmsg,60) trim(aatype),trim(aline),trim(afile)
60         format(a,' item on line ',a,' of file ',a,' must be ',
     +     '"parameter", "parameter_group", "observation" or ',
     +     '"observation_group".')
           go to 9890
         end if
       end do
       if(itype(1).gt.0)then
         if(itype(2).gt.0)then
           call writint(aline,iline)
           write(errmsg,70) trim(aline),trim(afile)
70         format('If first item on line ',a,' of file ',a,
     +     ' is "parameter" or "parameter_group" then second item ',
     +     'must be "observation" or "observation_group".')
           go to 9890
         end if
       else if(itype(1).lt.0)then
         if(itype(2).lt.0)then
           call writint(aline,iline)
           write(errmsg,80) trim(aline),trim(afile)
80         format('If first item on line ',a,' of file ',a,
     +     ' is "observation" or "observation_group" then second item ',
     +     'must be "parameter" or "parameter_group".')
           go to 9890
         end if
       end if
       do
         iline=iline+1
         read(10,'(a)',err=9500,end=2000) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         if(cline(1:2).eq.'* ') go to 50
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9550
         call lowcas(cline)
         aterm=cline(lw(1):rw(1))
         if((index(aterm,'*').ne.0).or.(index(aterm,'?').ne.0))then
           call writint(aline,iline)
           write(errmsg,90) trim(aline),trim(afile)
90         format('The first item on line ',a,' of file ',a,
     +     ' must not contain a "*" or "?" character.')
           go to 9890
         end if
         iii=1         ! for error message
         if(itype(1).eq.1)then
           call which1(ifail,nespar,ipar,apar,aterm)
           if(ifail.ne.0)then
             call writint(aline,iline)
             write(6,901) trim(aline),trim(aterm)
901          format(' Warning at line ',a,
     +       ': "',a,'" is not an adjustable parameter.')
             cycle
           end if
         else if(itype(1).eq.2)then
           call which1(ifail,npargp,ipgp,pargnme,aterm)
         else if(itype(1).eq.-1)then
           call which1(ifail,nxrow,iobs,aobs,aterm)
           if(ifail.ne.0) go to 9600
           if(iobs.gt.nobs)then
             call writint(aline,iline)
             write(errmsg,91) trim(aterm),trim(aline),trim(afile)
91           format('Observation "',a,'" cited on line ',a,' of ',
     +       'file ',a,' is a prior information item. This cannot ',
     +       'be blanked.')
             go to 9890
           end if
         else if(itype(1).eq.-2)then
           call which1(ifail,nobsgp,iogp,obgnme,aterm)
           if(ifail.ne.0) go to 9600
           if(nxrow.gt.nobs)then
             do i=nobs+1,nxrow
               if(nobgnm(i).eq.iogp)then
                 call writint(aline,iline)
                 write(errmsg,92) trim(aterm),trim(aline),trim(afile)
92               format('Observation group "',a,'" cited at line ',a,
     +           ' of file ',a,' contains prior information equations.',
     +           ' These cannot be blanked.')
                 go to 9890
               end if
             end do
           end if
         end if
         if(ifail.ne.0) go to 9600
         iii=2
         aterm=cline(lw(2):rw(2))
         if((index(aterm,'*').eq.0).and.(index(aterm,'?').eq.0))then
           if(itype(2).eq.1)then
             call which1(ifail,nespar,ipar,apar,aterm)
             if(ifail.ne.0)then
               call writint(aline,iline)
               write(6,901) trim(aline),trim(aterm)
               cycle
             end if
           else if(itype(2).eq.2)then
             call which1(ifail,npargp,ipgp,pargnme,aterm)
           else if(itype(2).eq.-1)then
             call which1(ifail,nxrow,iobs,aobs,aterm)
             if(ifail.ne.0) go to 9600
             if(iobs.gt.nobs)then
               call writint(aline,iline)
               write(errmsg,91) trim(aterm),trim(aline),trim(afile)
               go to 9890
             end if
           else if(itype(2).eq.-2)then
             call which1(ifail,nobsgp,iogp,obgnme,aterm)
             if(ifail.ne.0) go to 9600
             if(nxrow.gt.nobs)then
               do i=nobs+1,nxrow
                 if(nobgnm(i).eq.iogp)then
                   call writint(aline,iline)
                   write(errmsg,92) trim(aterm),trim(aline),trim(afile)
                   go to 9890
                 end if
               end do
             end if
           end if
           if(ifail.ne.0) go to 9600
           if(itype(2).eq.1)then
             if(itype(1).eq.-1)then
               xb(iobs,ipar)=action
               zerocount=zerocount+1
             else if(itype(1).eq.-2)then
               do i=1,nobs
                 if(nobgnm(i).eq.iogp)then
                   zerocount=zerocount+1
                   xb(i,ipar)=action
                 end if
               end do
             end if
           else if(itype(2).eq.2)then
             if(itype(1).eq.-1)then
               do j=1,nespar
                 if(ipargp(j).eq.ipgp)then
                   zerocount=zerocount+1
                   xb(iobs,j)=action
                 end if
               end do
             else if(itype(1).eq.-2)then
               do j=1,nespar
                 if(ipargp(j).eq.ipgp)then
                   do i=1,nobs
                     if(nobgnm(i).eq.iogp)then
                       zerocount=zerocount+1
                       xb(i,j)=action
                     end if
                   end do
                 end if
               end do
             end if
           else if(itype(2).eq.-1)then
             if(itype(1).eq.1)then
               zerocount=zerocount+1
               xb(iobs,ipar)=action
             else if(itype(1).eq.2)then
               do j=1,nespar
                 if(ipargp(j).eq.ipgp)then
                   zerocount=zerocount+1
                   xb(iobs,j)=action
                 end if
               end do
             end if
           else if(itype(2).eq.-2)then
             if(itype(1).eq.1)then
               do i=1,nobs
                 if(nobgnm(i).eq.iogp)then
                   zerocount=zerocount+1
                   xb(i,ipar)=action
                 end if
               end do
             else if(itype(1).eq.2)then
               do i=1,nobs
                 if(nobgnm(i).eq.iogp)then
                   do j=1,nespar
                     if(ipargp(j).eq.ipgp)then
                       zerocount=zerocount+1
                       xb(i,j)=action
                     end if
                   end do
                 end if
               end do
             end if
           end if
         else
           if((itype(2).eq.2).or.(itype(2).eq.-2))then
             call writint(aline,iline)
             write(errmsg,105) trim(aline),trim(afile)
105          format('A string describing a parameter or observation ',
     +       'group must not possess a "*" or "?" character. This ',
     +       'is violated at line ',a,' of file ',a,'.')
             go to 9890
           end if
           ii=0
           do i=1,len_trim(aterm)
             if(aterm(i:i).eq.'*') ii=ii+1
           end do
           if(ii.gt.1)then
             call writint(aline,iline)
             write(errmsg,110) trim(aterm),trim(aline),trim(afile)
110          format('The string "',a,'" at line ',a,' of file ',a,
     +       ' must only have one "*" character.')
             go to 9890
           end if
           iii=0
           do i=1,len_trim(aterm)
             if(aterm(i:i).eq.'?') iii=iii+1
           end do
           if(iii.gt.1)then
             call writint(aline,iline)
             write(errmsg,120) trim(aterm),trim(aline),trim(afile)
120          format('The string "',a,'" at line ',a,' of file ',a,
     +       ' must only have one "?" character.')
             go to 9890
           end if
           if((ii.gt.0).and.(iii.gt.0))then
             call writint(aline,iline)
             write(errmsg,115) trim(aterm),trim(aline),trim(afile)
115          format('The string "',a,'" at line ',a,' of file ',a,
     +       ' can possess a single "*" or "?" character, but ',
     +       'not both of these.')
             go to 9890
           end if
           if(itype(2).eq.1)then
             if(itype(1).eq.-1)then
               ncount=0
               do i=1,nespar
                 if(string_match(apar(i),aterm))then
                   ncount=ncount+1
                   zerocount=zerocount+1
                   xb(iobs,i)=action
                 end if
               end do
               if(ncount.eq.0)then
                 call writint(aline,iline)
                 write(6,1151) trim(aline),trim(aterm)
1151             format(' Warning at line ',a,': string "',a,
     +           '" matches no adjustable parameters.')
               end if
             else if(itype(1).eq.-2)then
               ncount=0
               do i=1,nespar
                 if(string_match(apar(i),aterm))then
                   ncount=ncount+1
                   do j=1,nobs
                     if(nobgnm(j).eq.iogp)then
                       zerocount=zerocount+1
                       xb(j,i)=action
                     end if
                   end do
                 end if
               end do
               if(ncount.eq.0)then
                 call writint(aline,iline)
                 write(6,1151) trim(aline),trim(aterm)
               end if
             end if
           else if(itype(2).eq.-1)then
             ncount=0
             if(itype(1).eq.1)then
               do j=1,nobs
                 if(string_match(aobs(j),aterm))then
                   ncount=ncount+1
                   zerocount=zerocount+1
                   xb(j,ipar)=action
                 end if
               end do
               if(ncount.eq.0)then
                 aatype='observations'
                 go to 9650
               end if
             else if(itype(1).eq.2)then
               ncount=0
               do j=1,nobs
                 if(string_match(aobs(j),aterm))then
                   ncount=ncount+1
                   do i=1,nespar
                     if(ipargp(i).eq.ipgp)then
                       zerocount=zerocount+1
                       xb(j,i)=action
                     end if
                   end do
                 end if
               end do
               if(ncount.eq.0)then
                 aatype='observations'
                 go to 9650
               end if
             end if
           end if
         end if
       end do

2000   continue
       close(unit=10)
       if(jjfail.ne.0)then
         write(errmsg,2001) trim(afile)
2001     format('File ',a,' does not appear to be a proper ',
     +   'zeroing file.')
         go to 9890
       end if
       call writint(azero,zerocount)
       write(6,2005) trim(afile)
2005   format(' - file ',a,' read ok.')
       write(6,2010) trim(azero)
2010   format(' - ',a,' elements of Jacobian matrix were blanked.')

2011   continue
       if(sifile.eq.' ') go to 2012

! -- We now test possibilities for simultaneous increments.

       write(6,*)
       write(6,702)
702    format(' - calculating simultaneous increments...')
       allocate(runpar(nespar),bbb(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200
       runpar=0               ! an array
       nrun=0
       do ipar=1,nespar
         if(runpar(ipar).gt.0) cycle
         nrun=nrun+1
         runpar(ipar)=nrun
         if(ipar.ne.nespar)then
           bbb=xb(:,ipar)
           idc=dercom(ipar)
           do jpar=ipar+1,nespar
             if(runpar(jpar).eq.0)then
               if(dercom(jpar).eq.idc)then
                 do iobs=1,nobs
                   if(bbb(iobs).and.xb(iobs,jpar)) go to 800
                 end do
                 runpar(jpar)=nrun
                 do iobs=1,nobs
                   if(xb(iobs,jpar))bbb(iobs)=.TRUE.
                 end do
800              continue
               end if
             end if
           end do
         end if
       end do
       write(6,*)
       call addquote(sifile,afile)
       write(6,701) trim(afile)
701    format(' - writing simultaneous increment file ',a,'...')
       open(unit=20,file=sifile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,810) trim(afile)
810      format('Cannot write to file ',a,'.')
         go to 9890
       end if
       call writint(arun,npar)
       write(20,819) trim(arun)
819    format('Number of parameters                         = ',a)
       call writint(arun,nespar)
       write(20,820) trim(arun)
820    format('Number of adjustable parameters              = ',a)
       if(numcom.gt.1)then
         do icom=1,numcom
           call writint(acom,icom)
           icount=0
           do ies=1,nespar
             if(dercom(ies).eq.icom) icount=icount+1
           end do
           call writint(arun,icount)
           write(20,8202) trim(acom),trim(arun)
8202       format('Number of adjustable parameters ',
     +     '[DERCOM=',a,']   = ',a)
         end do
       end if
       call writint(arun,nobs)
       write(20,8201) trim(arun)
8201   format('Number of observations                       = ',a)
       iii=0
       do i=1,nobs
         if(owght(i).gt.0.0d0)iii=iii+1
       end do
       call writint(arun,iii)
       write(20,822) trim(arun)
822    format('Number of non-zero-weighted observations     = ',a)
       call writint(arun,nrun)
       write(20,821) trim(arun)
821    format('Number of simultaneous increments            = ',a)
       if(numcom.gt.1)then
         do icom=1,numcom
           call writint(acom,icom)
           icount=0
           do i=1,nrun
             do ies=1,nespar
               if(runpar(ies).eq.i)then
                 if(dercom(ies).eq.icom)then
                   icount=icount+1
                 end if
                 go to 8204
               end if
             end do
8204         continue
           end do
           call writint(arun,icount)
           write(20,8203) trim(acom),trim(arun)
8203       format('Number of simultaneous increments ',
     +     '[DERCOM=',a,'] = ',a)
         end do
       end if
       write(20,*)
       write(20,825)
825    format(' parameter',t20,'increment_number')
       do ipar=1,nespar
         write(20,827) trim(apar(ipar)),runpar(ipar)
827      format(1x,a,t20,i5)
       end do
       write(20,*)
       write(20,830)
830    format(' run_number',t15,'num_params',t28,'parameters')
       do i=1,nrun
         cline=' '
         ncount=0
         call writint(arun,i)
         write(cline,831) trim(arun)
831      format(3x,a)
         do ipar=1,nespar
           if(runpar(ipar).eq.i)then
             ncount=ncount+1
             if(ncount.eq.1)then
               cline(28:)=trim(apar(ipar))
             else
               cline=trim(cline)//' '//trim(apar(ipar))
             end if
           end if
         end do
         write(cline(15:27),833)ncount
833      format(i5)
         write(20,'(a)') trim(cline)
       end do
       close(unit=20)
       write(6,490) trim(afile)
       write(6,*)
       call writint(arun,nespar)
       write(6,880) trim(arun)
880    format('   Number of adjustable parameters   = ',a)
       call writint(arun,nrun)
       write(6,890) trim(arun)
890    format('   Number of simultaneous increments = ',a)

! -- If necessary, put code here to alter the blanking.

       if(acs.eq.'y')then
         jcount=count(xb)
         write(6,*)
         write(6,900)
900      format(' - adjusting blanking to sync with increments...')
         do irun=1,nrun
           icount=0
           do ipar=1,nespar
             if(runpar(ipar).eq.irun)then
               icount=icount+1
               if(icount.eq.1) ies=ipar
             end if
           end do
           if(icount.eq.1)then
             xb(:,ies)=.TRUE.
           else
             bbb=.FALSE.                       ! an array
             do ipar=ies,nespar
               if(runpar(ipar).eq.irun)then
                 do iobs=1,nobs
                   if(xb(iobs,ipar))bbb=.TRUE.
                 end do
               end if
             end do
             do iobs=1,nobs
               if(.not.bbb(iobs))xb(iobs,ies)=.TRUE.
             end do
           end if
         end do
         icount=count(xb)
         icount=icount-jcount
         call writint(arun,icount)
         write(6,910) trim(arun)
910      format('   Number of elements unblanked = ',a)
       end if

! -- The new JCB file is written.

2012   continue
       write(6,*)
       call addquote(jcoblankfile,afile)
       write(6,700) trim(afile)
700    format(' - writing JCO blanking file ',a,'...')
       how='write'
       call open_unformatted_file(ifail,15,how,jcoblankfile,errmsg)
       if(ifail.ne.0) go to 9300
       write(15,err=9300) -nespar,-nobs
       allocate(writeiobs(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200
       do ies=1,nespar
         itemp=count(.not.(xb(:,ies)))
         write(15,err=9300) ies,itemp
         if(itemp.ne.0)then
           i=0
           do iobs=1,nobs
             if(.not.xb(iobs,ies)) then
               i=i+1
               writeiobs(i)=iobs
             end if
           end do
           write(15) (writeiobs(j),j=1,i)
         end if
       end do
       do i=1,nespar
         write(15,err=9300) apar(i)
       end do
       do i=1,nobs
         write(15,err=9300) aobs(i)
       end do
       close(unit=15)
       write(6,490) trim(afile)
490    format(' - file ',a,' written ok.')

! -- The new JCO file is written.

       one=1.0d0
       write(6,*)
       call addquote(locjcofile,afile)
       write(6,7011) trim(afile)
7011   format(' - writing localization JCO file ',a,'...')
       how='write'
       call open_unformatted_file(ifail,15,how,locjcofile,errmsg)
       if(ifail.ne.0) go to 9300
       write(15,err=9300) -nespar,-nobs
       icount=0
       do ies=1,nespar
         do iobs=1,nobs
           if(xb(iobs,ies)) icount=icount+1
         end do
       end do
       write(15,err=9300) icount
       do ies=1,nespar
         do iobs=1,nobs
           if(xb(iobs,ies))then
             j=(ies-1)*nobs+iobs
             write(15,err=9300) j,one
           end if
         end do
       end do
       do i=1,nespar
         write(15,err=9300) apar(i)
       end do
       do i=1,nobs
         write(15,err=9300) aobs(i)
       end do
       close(unit=15)
       write(6,490) trim(afile)

! -- The JCO blanking pattern file is written.

       write(6,*)
       call addquote(pjcoblankfile,afile)
       write(6,699) trim(afile)
699    format(' - writing JCO blanking pattern file ',a,'...')
       open(unit=15,file=pjcoblankfile,action='write',err=9300)
       write(15,519) nobs,nespar
519    format(2i10)
       do iobs=1,nobs
         write(15,520) (xb(iobs,ies),ies=1,nespar)
520      format(10000L2)
       end do
       close(unit=15)
       write(6,490) trim(afile)

       go to 9900

9000   call writint(aline,iline)
       write(errmsg,9010) trim(aline),trim(afile)
9010   format('"start all zero" or "start all nonzero" expected ',
     + 'at line ',a,' of file ',a,'.')
       go to 9890

9100   write(errmsg,9110) trim(afile)
9110   format('File ',a,' does not have an extension of ".pst".')
       go to 9890

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficent memory to continue execution.')
       go to 9890

9250   write(errmsg,9260) trim(afile)
9260   format('Error encountered in reading binary Jacobian matrix ',
     + 'file ',a,'.')
       go to 9890

9270   write(errmsg,9280) trim(afile)
9280   format('The Jacobian matrix contained in file ',a,
     + ' is not compatible with the PEST input dataset.')
       go to 9890

9290   write(errmsg,9295)
9295   format('If a JCO file is read instead of a zero file then ',
     + 'no covariance matrices must be assigned to observation ',
     + 'groups which are not purely prior information groups ',
     + 'in the PEST control file.')
       go to 9890

9300   write(errmsg,9310) trim(afile)
9310   format('Cannot write to file ',a,'.')
       go to 9890

9500   call writint(aline,iline)
       write(errmsg,9510) trim(aline),trim(afile)
9510   format('Error reading line ',a,' of file ',a,'.')
       go to 9890

9550   call writint(aline,iline)
       write(errmsg,9560) trim(aline),trim(afile)
9560   format('Insufficient items on line ',a,' of file ',a,'.')
       go to 9890

9600   call writint(aline,iline)
       write(errmsg,9610) trim(aterm),trim(atype(iii)),
     + trim(aline),trim(afile)
9610   format('Entity "',a,'" does not correspond to a ',a,' at ',
     + 'line ',a,' of file ',a,'.')
       go to 9890

9650   call writint(aline,iline)
       write(errmsg,9660) trim(aterm),trim(aatype),trim(aline),
     + trim(afile)
9660   format('String "',a,'" does not match any ',a,' at line ',a,
     + ' of file ',a,'.')
       go to 9890

9890   continue
       call write_message(errmsg)
9900   continue

       call pest_data_deallocate(ifail)
       deallocate(xb,stat=ierr)
       deallocate(runpar,bbb,stat=ierr)
       deallocate(writeiobs,stat=ierr)

9999   continue
       end



        logical function string_match(astring,agen)

! -- Function STRING_MATCH inquires whether the generic string AGEN matches a real string ASTRING.
! -- It is assumed that both strings have already been converted to lower case, and that neither
!    string has a blank or tab.
!    It is also assumed that only a single * or a single ? exists in the string.

        implicit none
        character (len=*), intent(in)      :: astring
        character (len=*), intent(in)      :: agen

        logical                            :: star,ques
        integer                            :: nnn,ii,lt,ns,ng
        character (len=21)                 :: as,ag,sag,sas

! -- Initialization

        star=.false.
        ques=.false.
        as=adjustl(astring)
        ag=adjustl(agen)
        nnn=index(ag,'*')
        if(nnn.ne.0)then
          star=.true.
        else
          nnn=index(ag,'?')
          if(nnn.ne.0) ques=.true.
        end if

! -- First we look for the obvious matches.

        if(nnn.eq.0)then
          if(as.eq.ag) then
            go to 8000
          else
            go to 9000
          end if
        end if
        if(as.eq.' ')then
          if(ag.eq.' ') then
            go to 8000
          else
            go to 9000
          end if
        end if

        if(ag.eq.'*') go to 8000
        if(ag.eq.'?') then
          if(len_trim(as).eq.1) go to 8000
        end if

        ns=len_trim(as)
        ng=len_trim(ag)

        if(nnn.eq.1)then
          sag=ag(2:)
          lt=len_trim(sag)+1
          ii=index(as,sag(1:lt))
          if((ii.eq.0).or.(ii.eq.1)) then
            go to 9000
          else if(ii.eq.2) then
            go to 8000
          else
            if(star) then
              go to 8000
            else
              go to 9000
            end if
          end if
        else if(nnn.eq.ng)then
          sag=ag(1:ng-1)
          if((as(1:ng-1).eq.sag).and.(as(ng:ng).ne.' '))then
            if(as(ng+1:ng+1).eq.' ')then
              go to 8000
            else
              if(star)then
                go to 8000
              else
                go to 9000
              end if
            end if
          else
            go to 9000
          end if
        else
          if(as(1:nnn-1).ne.ag(1:nnn-1)) go to 9000
          sag=ag(nnn+1:)
          sas=as(nnn:)
          lt=len_trim(sag)+1
          ii=index(sas,sag(1:lt))
          if((ii.eq.0).or.(ii.eq.1)) then
            go to 9000
          else if(ii.eq.2) then
            go to 8000
          else
            if(star)then
              go to 8000
            else
              go to 9000
            end if
          end if
        end if

8000    continue
        string_match=.TRUE.
        go to 9900

9000    continue
        string_match=.FALSE.
        go to 9900

9900    continue
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


        SUBROUTINE NUMRD(IFAIL,ITYPE,LW,RW,IVAL,RVAL,CLINE)

C -- SUBROUTINE NUMRD READS A NUMBER FROM PART OF A STRING

        INTEGER IFAIL,RW,LW,LN,IVAL
        INTEGER ITYPE
        DOUBLE PRECISION RVAL
        CHARACTER*(*) CLINE
        CHARACTER*10 FMT

        IFAIL=0
        LN=RW-LW+1
        IF(ITYPE.EQ.0) THEN
          WRITE(FMT,10) LN
10        FORMAT('(I',I2,')')
          READ(CLINE(LW:RW),FMT,ERR=50) IVAL
        ELSE IF(ITYPE.EQ.1) THEN
          WRITE(FMT,20) LN
20        FORMAT('(F',I2,'.0)')
          READ(CLINE(LW:RW),FMT,ERR=50) RVAL
        END IF
        RETURN

50      IFAIL=1
        RETURN

        END


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


        SUBROUTINE UPCAS(ASTRNG)

C -- SUBROUTINE UPCAS CONVERTS A STRING TO UPPER CASE

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,LEN_TRIM(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.97).AND.(J.LE.122)) ASTRNG(I:I)=CHAR(J-32)
10      CONTINUE
        RETURN

        END


        SUBROUTINE GETINT(IFAIL,BUF,N1,N2,NBLB,MRKDEL)

C -- SUBROUTINE GETINT GETS THE NEXT STORED INSTRUCTION FOR PROCESSING

        INTEGER N1,N2,NBLB,I,II
        INTEGER IFAIL
        CHARACTER MRKDEL
        CHARACTER*(*) BUF

        IFAIL=0
        IF(N2.GE.NBLB) THEN
          N1=0
          RETURN
        END IF
        DO 10 I=N2+1,NBLB
        IF((BUF(I:I).NE.' ').AND.(ICHAR(BUF(I:I)).NE.9)) GO TO 50
10      CONTINUE
        N1=0
        RETURN
50      N1=I
        IF(BUF(N1:N1).NE.MRKDEL)THEN
          I=INDEX(BUF(N1:NBLB),' ')
          II=INDEX(BUF(N1:NBLB),CHAR(9))
          IF((I.EQ.0).AND.(II.EQ.0))THEN
            I=0
          ELSE IF(I.EQ.0)THEN
            I=II
          ELSE IF(II.EQ.0) THEN
            I=I
          ELSE
            I=MIN(I,II)
          END IF
          IF(I.NE.0) THEN
            N2=N1+I-2
          ELSE
            N2=NBLB
          END IF
        ELSE
          IF(N1.EQ.NBLB)THEN
            IFAIL=1
            RETURN
          END IF
          I=INDEX(BUF(N1+1:NBLB),MRKDEL)
          IF(I.EQ.0) THEN
            IFAIL=1
            RETURN
          END IF
          N2=N1+I
        END IF

        RETURN

        END


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
