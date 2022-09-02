       program assesspar

C -- Program ASSESSPAR allows assessment of parameter confidence based on
C    normal and chi square distributions.

       implicit none

       logical skipline
       integer ifail,ierr,i,j,k,iline
       integer npar,nobs,npargp,nprior,nobsgp
       integer nespar,nespar1,ipar,jpar,ies,ntied,itied
       integer iunit,junit,itype,vardim,covdim,jpartype
       integer ipredict,info
       integer lwork,inum
       integer lw(10),rw(10)

       double precision chivar,predmag,normvar,sum,dtemp
       double precision parvar(1),vt(1:1)

       character*12   aversion
       character*12   atemp,apar1,apar2,anum,atype,aline,tpar
       character*15   atrans,achange
       character*256  parfile,pestfile,uncfile,outfile,senfile
       character*256  qparfile,qpestfile,quncfile,qoutfile,qsenfile
       character*500  dline,cline
       character*1000 amessage

       integer,      allocatable     :: itrans(:),iitrans(:)
       double precision, allocatable :: pval(:),ppval(:),poval(:)
       double precision, allocatable :: predvec(:),tempvec(:)
       double precision, allocatable :: f(:),work(:),e(:,:),m(:)
       double precision, allocatable :: cp(:,:)
       character*12, allocatable     :: apar(:),aapar(:)

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' ASSESSPAR Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) dline
#else
       call pgetcl(dline)
#endif

C -- The ASSESSPAR command line is read.

       if(dline.eq.' ') go to 8900
       call spacesub(dline)
       call linspl(ifail,6,lw,rw,dline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,4,lw,rw,dline)
       if(ifail.ne.0) go to 8900
       parfile=dline(lw(1):rw(1))
       call remchar(parfile,char(211))
       pestfile=dline(lw(2):rw(2))
       call remchar(pestfile,char(211))
       uncfile=dline(lw(3):rw(3))
       call remchar(uncfile,char(211))
       outfile=dline(lw(4):rw(4))
       call remchar(outfile,char(211))
       call linspl(ifail,5,lw,rw,dline)
       if(ifail.eq.0) then
         ipredict=1
         senfile=dline(lw(5):rw(5))
         call remchar(senfile,char(211))
       else
         senfile=' '
         ipredict=0
       end if

#ifndef UNIX
       call lowcas(parfile)
       call lowcas(pestfile)
       call lowcas(uncfile)
       call lowcas(outfile)
       call lowcas(senfile)
#endif
       call addquote(parfile,qparfile)
       call addquote(uncfile,quncfile)
       call addquote(outfile,qoutfile)
       qsenfile=' '
       if(ipredict.eq.1) call addquote(senfile,qsenfile)

C -- The PEST control file is opened.

       i=len_trim(pestfile)
       j=i-4
       if(j.lt.1)j=1
       atemp=pestfile(j:i)
       if(index(atemp,'.').eq.0) then
         pestfile=trim(pestfile)//'.pst'
       end if
       call addquote(pestfile,qpestfile)
       i=len_trim(pestfile)
       atemp=pestfile(i-3:i)
       call lowcas(atemp)
       if(atemp(1:4).ne.'.pst')then
         write(amessage,20)
20       format(' PEST control file must have an extension of ',
     +   '".pst".')
         go to 9890
       end if
       open(unit=10,file=pestfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(6,115) trim(qpestfile)
115      format(' Cannot open PEST control file ',a,'.')
         go to 9890
       end if

C -- The PEST control file is partly read.

       read(10,*,err=9000,end=9000) atemp
       call lowcas(atemp)
       if(atemp(1:4).ne.'pcf ') go to 9000
116    continue
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 116
117    continue
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 117
118    continue
       read(10,'(a)',err=9000,end=9000) cline
       if(skipline(cline)) go to 118
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
       if(npar.eq.0)then
         write(amessage,168) trim(qpestfile)
168      format(' Number of parameters NPAR supplied as zero in ',
     +   'PEST control file ',a,'.')
         go to 9890
       end if

C -- Parameter data is now read.

       allocate(apar(npar),itrans(npar),pval(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       do
         read(10,'(a)',end=9000) cline
         cline=adjustl(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'parameter da').ne.0) go to 170
         end if
       end do
170    continue
       nespar=0
       do ipar=1,npar
171      continue
         read(10,'(a)',err=9000,end=9000) cline
         if(skipline(cline)) go to 171
         call linspl(ifail,4,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         apar(ipar)=cline(lw(1):rw(1))
         atrans=cline(lw(2):rw(2))
         achange=cline(lw(3):rw(3))
         call drealrd(ifail,cline(lw(4):rw(4)),pval(ipar))
         if(ifail.ne.0) go to 9000
         call lowcas(apar(ipar))
         call lowcas(atrans)
         if(atrans.eq.'log')then
           itrans(ipar)=1
         else if(atrans.eq.'none')then
           itrans(ipar)=0
         else if(atrans.eq.'fixed')then
           itrans(ipar)=-1000000
         else if(atrans.eq.'tied')then
           itrans(ipar)=-1
         else
           go to 9000
         end if
         if(itrans(ipar).eq.1)then
           if(pval(ipar).le.0.0d0)then
             write(amessage,195) trim(apar(ipar)),trim(qpestfile)
195          format(' Parameter "',a,'" is log transformed, yet it ',
     +       'has a negative initial value in PEST control file ',
     +       a,'.')
             go to 9890
           end if
         end if
         if(itrans(ipar).ge.0) nespar=nespar+1
       end do
       if(nespar.eq.0) then
         write(amessage,230) trim(qpestfile)
230      format(' There are no adjustable parameters in PEST ',
     +   'control file ',a,'.')
         go to 9890
       end if

C -- We see if there are any tied parameters.

       ntied=0
       do ipar=1,npar
         if(itrans(ipar).eq.-1) ntied=ntied+1
       end do
       if(ntied.ne.0)then
         do itied=1,ntied
2301       continue
           read(10,'(a)',err=9000,end=9000) cline
           if(skipline(cline)) go to 2301
           call linspl(ifail,2,lw,rw,cline)
           if(ifail.ne.0) go to 9000
           apar1=cline(lw(1):rw(1))
           apar2=cline(lw(2):rw(2))
           call lowcas(apar1)
           call lowcas(apar2)
           do j=1,npar
             if(apar1.eq.apar(j)) go to 325
           end do
           go to 9000
325        if(itrans(j).ne.-1) go to 9000
           do k=1,npar
             if(apar2.eq.apar(k)) go to 326
           end do
           go to 9000
326        if(itrans(k).lt.0) go to 9000
           itrans(j)=-k
         end do
       end if
       call writint(anum,npar)
       write(6,210) trim(anum),trim(qpestfile)
210    format(' - ',a,' parameters read from file ',a,'.')
       call writint(anum,nespar)
       write(6,220) trim(anum)
220    format(' - ',a,' of these are adjustable.')
       close(unit=10)
       write(6,*)

C -- We now read the parameter uncertainty file.

       open(unit=12,file=uncfile,status='old',iostat=ierr)
       if(ierr.ne.0) then
         write(amessage,231) trim(quncfile)
231      format(' Cannot open parameter uncertainty file ',a,'.')
         go to 9890
       end if
       allocate(cp(nespar,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- A copy of the parameter name array is made in which only adjustable parameters
C    are cited.

       allocate(aapar(nespar),iitrans(nespar),ppval(nespar),
     + poval(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           aapar(ies)=apar(ipar)
           iitrans(ies)=itrans(ipar)
           ppval(ies)=pval(ipar)
         end if
       end do

C -- The uncertainty file is now read.

       iunit=12
       junit=15
       itype=2
       vardim=1
       covdim=nespar
       atype='parameter'
       call read_uncert_data_file(ifail,iunit,junit,itype,jpartype,
     + nespar,vardim,covdim,parvar,cp,atype,uncfile,amessage,
     + cline,aapar)
       if(ifail.ne.0) go to 9890
       write(6,380) trim(quncfile)
380    format(' - file ',a,' read ok.')

C       open(unit=90,file='debug.dat')                  !debug
C       do ipar=1,npar                                  !debug
C         write(90,381) (cp(ipar,jpar),jpar=1,nespar)   !debug
C381      format(10(1x,1pg12.5))                        !debug
C       end do                                          !debug

C -- The parameter value file is now read.

       open(unit=10,file=parfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,390) trim(qparfile)
390      format(' Cannot open parameter value file ',a,'.')
         go to 9890
       end if
       iline=1
       read(10,'(a)',err=9050,end=9050) cline
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0)then
         write(amessage,400) trim(qparfile)
400      format(' Two entries expected on first line of ',
     +   'parameter value file ',a,'.')
         go to 9890
       end if
       atemp=cline(lw(1):rw(1))
       call lowcas(atemp)
       if((atemp.ne.'single').and.(atemp.ne.'double'))then
         write(amessage,410) trim(qparfile)
410      format(' First entry on first line of file ',a,
     +   ' is expected to be "single" or "double".')
         go to 9890
       end if
       poval=-1.1d300           ! an array
       do
415      continue
         iline=iline+1
         read(10,'(a)',err=9050,end=500) cline
         if(cline.eq.' ') go to 415
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,420) trim(aline),trim(qparfile)
420        format(' Insufficient entries found on line ',a,
     +     ' of file ',a,'.')
           go to 9890
         end if
         tpar=cline(lw(1):rw(1))
         call lowcas(tpar)
         call whichone(ifail,nespar,ipar,aapar,tpar)
         if(ifail.eq.0)then
           call drealrd(ifail,cline(lw(2):rw(2)),poval(ipar))
           if(ifail.ne.0) go to 9050
           if(iitrans(ipar).eq.1)then
             if(poval(ipar).le.0.0d0)then
               write(amessage,425) trim(aapar(ipar)),trim(qparfile)
425            format(' Parameer ',a,' is designated as log-',
     +         'transformed in PEST control file but has a zero or ',
     +         'negative value in parameter value file ',a,'.')
               go to 9890
             end if
           end if
         end if
       end do
500    continue
       close(unit=10)
       inum=0
       do ipar=1,nespar
         if(poval(ipar).lt.-1.0d300)then
           inum=inum+1
           if(inum.eq.1)then
             write(amessage,510)
510          format(' The following adjustable parameters are ',
     +       'featured in the PEST control file but are not found ',
     +       'in the parameter value file:-')
           end if
           amessage=trim(amessage)//' '//trim(aapar(ipar))
           if(inum.eq.10) go to 9890
         end if
       end do
       if(inum.ne.0) go to 9890
       write(6,520) trim(qparfile)
520    format(' - file ',a,' read ok.')

C -- The log is taken of log-transformed parameters.

       do ipar=1,nespar
         if(iitrans(ipar).eq.1)then
           ppval(ipar)=log10(ppval(ipar))
           poval(ipar)=log10(poval(ipar))
         end if
       end do

C -- The predictive sensitivity file is now read.

       if(ipredict.ne.0)then
         open(unit=10,file=senfile,status='old',iostat=ierr)
         if(ierr.ne.0)then
           write(amessage,1380) trim(qsenfile)
1380       format(' Cannot open file ',a,' to read ',
     +     'predictive sensitivities.')
           go to 9890
         end if
         read(10,*,err=9400,end=9400) nespar1
         if(nespar1.ne.nespar)then
           write(amessage,1390) trim(qsenfile),trim(qpestfile)
1390       format(' File ',a,' cites a different number of ',
     +     'adjustable parameters from those in PEST control file ',
     +     a,'.')
           go to 9890
         end if
         allocate(tempvec(nespar1),predvec(nespar1),stat=ierr)
         if(ierr.ne.0) go to 9200
         do ipar=1,nespar1
           read(10,*,err=9400,end=9400) tempvec(ipar)
         end do
         read(10,'(a)',err=9400,end=9400) cline
         call lowcas(cline)
         if(index(cline,'name').eq.0)then
           call writint(aline,nespar+2)
           write(amessage,1410) trim(aline),trim(qsenfile)
1410       format(' "* row names" header expected to follow ',
     +     'predictive sensitivities and precede parameter names ',
     +     'at line ',a,' of file ',a,'.')
           go to 9890
         end if
         predvec=-1.1d300          ! an array
         do i=1,nespar1
           read(10,*,err=9450,end=9450) tpar
           call lowcas(tpar)
           call whichone(ifail,nespar,ipar,aapar,tpar)
           if(ifail.eq.0) predvec(ipar)=tempvec(i)
         end do
         close(unit=10)
         inum=0
         do ipar=1,nespar
           if(predvec(ipar).lt.-1.0d300)then
             inum=inum+1
             if(inum.eq.1)then
               write(amessage,1420)
1420           format(' The following adjustable parameters are ',
     +         'featured in the PEST control file but are not found ',
     +         'in the parameter sensitivity file:-')
             end if
             amessage=trim(amessage)//' '//trim(aapar(ipar))
             if(inum.eq.10) go to 9890
           end if
         end do
         if(inum.ne.0) go to 9890
         write(6,520) trim(qsenfile)

C         write(90,*)                                   !debug
C         write(90,*) 'predictive sensitivities'        !debug
C         do ipar=1,nespar                              !debug
C           write(90,*) trim(aapar(ipar)),predvec(ipar) !debug
C         end do                                        !debug

       end if

! -- If the covariance matrix is diagonal the calculations are easy.

       allocate(f(nespar),m(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

       if(jpartype.eq.1)then
         do ipar=1,nespar
           if(cp(ipar,ipar).lt.1.0d-300)then
             write(amessage,525) trim(aapar(ipar))
525          format(' The covariance matrix has at least one zero ',
     +       'or near-zero diagonal element, this belonging to ',
     +       'parameter ',a,'.')
             go to 9890
           end if
         end do
         sum=0.0
         do ipar=1,nespar
           dtemp=poval(ipar)-ppval(ipar)
           sum=sum+dtemp*dtemp/cp(ipar,ipar)
         end do
         chivar=sum
         if(ipredict.ne.0)then
           predmag=0.0d0
           do ipar=1,nespar
             predmag=predmag+predvec(ipar)*predvec(ipar)*
     +       cp(ipar,ipar)
           end do
           predmag=sqrt(predmag)
           normvar=0.0d0
           do ipar=1,nespar
             normvar=normvar+predvec(ipar)*(poval(ipar)-ppval(ipar))
           end do
           normvar=normvar/predmag
         end if
         do ipar=1,nespar
           f(ipar)=cp(ipar,ipar)
           m(ipar)=(poval(ipar)-ppval(ipar))/sqrt(f(ipar))
         end do

       else

! -- Singular value decomposition is carried out on the covariance matrix.

         write(6,530)
530      format(/,' Undertaking SVD of covariance matrix...',/)

         lwork=10*nespar
         allocate(e(nespar,nespar),work(lwork),stat=ierr)
         if(ierr.ne.0) go to 9200
         call dgesvd('A','N',nespar,nespar,cp,nespar,f,e,nespar,vt,1,
     +   work,lwork,info)
         if(info.ne.0)then
           write(amessage,540)
540        format(' SVD was not successful.')
           go to 9890
         end if
         do ipar=1,nespar
           if(f(ipar).lt.1.0d-30)then
             call writint(anum,ipar)
             write(amessage,550) trim(anum)
550          format(' Covariance matrix is singular or near-singular.',
     +       ' Singular values become too low or zero at singular ',
     +       'value number ',a,'.')
             go to 9890
           end if
         end do
         do ipar=1,nespar
           ppval(ipar)=poval(ipar)-ppval(ipar)
         end do
         do ipar=1,nespar
           sum=0.0d0
           do jpar=1,nespar
             sum=sum+e(jpar,ipar)*ppval(jpar)
           end do
           m(ipar)=sum/sqrt(f(ipar))
         end do
         sum=0.0
         do ipar=1,nespar
           dtemp=m(ipar)
           sum=sum+dtemp*dtemp
         end do
         chivar=sum
         if(ipredict.ne.0)then
           do ipar=1,nespar
             sum=0.0d0
             do jpar=1,nespar
               sum=sum+cp(ipar,jpar)*predvec(jpar)
             end do
             tempvec(ipar)=sum
           end do
           predmag=0.0d0
           do ipar=1,nespar
             predmag=predmag+tempvec(ipar)*predvec(ipar)
           end do
           predmag=sqrt(predmag)
           normvar=0.0d0
           do ipar=1,nespar
             normvar=normvar+predvec(ipar)*ppval(ipar)         ! subtraction of poval above
           end do
           normvar=normvar/predmag
         end if
       end if

! -- The output file is now written.

       open(unit=20,file=outfile)
       write(20,710) nespar
710    format(  ' Number of adjustable parameters    = ',i5)
       if(ipredict.ne.0)then
         write(20,720) normvar
720      format(' Prediction-specific normal variate = ',1pg14.7)
       end if
       write(20,730) chivar
730    format(  ' Chi-square variate                 = ',1pg14.7)
       write(20,*)
       write(20,*)
       write(20,740)
740    format(' Standardized parameter set')
       write(20,750)
750    format(' --------------------------')
       write(20,*)
       write(20,770)
770    format(' singular_value_number   singular_value  ',
     + 'standardized_parameter_value')
       do ipar=1,nespar
         write(20,780) ipar,f(ipar),m(ipar)
780      format(t10,i5,t26,1pg14.7,t45,1pg14.7)
       end do
       close(unit=20)
       write(6,790) trim(outfile)
790    format(' - file ',a,' written ok.')

       write(6,*)
       if(ipredict.ne.0)then
         write(6,600) normvar
600      format(' Prediction-specfic normal variate = ',1pg14.7)
       end if
       write(6,700) chivar
700    format(  ' Chi-square variate                = ',1pg14.7)

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' ASSESSPAR is run using the command:',/)
       write(6,8920)
8920   format('     assesspar parfile pestfile uncfile outfile ',
     + '[senfile]',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8950)
8950   format('     parfile  is a parameter value file, ')
       write(6,8980)
8980   format('     pestfile is a PEST control file containing mean ',
     + 'parameter values,')
       write(6,8990)
8990   format('     uncfile  is a parameter uncertainty file,')
       write(6,8995)
8995   format('     outfile  is the transformed parameter output ',
     + 'file, and')
       write(6,8996)
8996   format('     senfile  is an optional predictive sensitivity ',
     + 'file.')

       go to 9999

9000   write(amessage,9010) trim(qpestfile)
9010   format(' Error encountered when reading PEST control file ',
     + a,' - check this file with PESTCHEK.')
       go to 9890

9050   call writint(aline,iline)
       write(amessage,9060) trim(aline),trim(qparfile)
9060   format(' Error reading line ',a,' of parameter value file ',
     + a,'.')
       go to 9890

9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9400   write(amessage,9410) trim(qsenfile)
9410   format(' Error reading predictive sensitivities from file ',
     + a,'.')
       go to 9890

9450   write(amessage,9460) trim(qsenfile)
9460   format(' Error reading parameter names from second part ',
     + 'of file ',a,'.')
       go to 9890

9890   continue
       call writmess(6,amessage)

9900   continue

       deallocate(itrans,iitrans,stat=ierr)
       deallocate(pval,ppval,poval,stat=ierr)
       deallocate(predvec,tempvec,stat=ierr)
       deallocate(f,work,e,m,stat=ierr)
       deallocate(cp,stat=ierr)
       deallocate(apar,aapar,stat=ierr)

9999   continue

       end


        SUBROUTINE TABREM(CLINE)

C -- Subroutine TABREM removes tabs from a string.

        INTEGER I
        CHARACTER*(*) CLINE

        DO 10 I=1,LEN(CLINE)
10      IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '

        RETURN
        END


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
