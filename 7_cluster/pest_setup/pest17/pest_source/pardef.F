#ifndef PESTMOD

C -- Note: a copy of this module heads the file PESTMOD.FOR.

        MODULE SVD_PARM_DATA

          INTEGER      ::  BASE_CALL
          INTEGER      ::  BPESTMODE
          INTEGER      ::  BNPAR,BNOBS,BNPARGP,BNPRIOR,BNOBSGP
          INTEGER      ::  BNESPAR,BNXROW,BNSTMP
          INTEGER      ::  BNTPFLE
          INTEGER      ::  BIBOUNDSCALE
          INTEGER      ::  BLWORK
          INTEGER      ::  INCLUDE_PRIOR=0
          INTEGER      ::  SVDA_SAVE=1
          INTEGER      ::  NUSEOBS
          INTEGER      ::  ICPRIOR
          INTEGER      ::  BNPRIORCOUNT=0
          INTEGER      ::  NEWBASEDER=0
          INTEGER      ::  QRFAILFLAG=0
          INTEGER      ::  LDTBX
          INTEGER      ::  PARAM_COUNT=0
          INTEGER      ::  SUPER_NPAR
          INTEGER, ALLOCATABLE           :: BITRANS(:)
          INTEGER, ALLOCATABLE           :: USEOBSGP(:)
          INTEGER, ALLOCATABLE           :: BIPARGP(:)
          INTEGER, ALLOCATABLE           :: BGTRANS(:),IMEM(:)
          INTEGER, ALLOCATABLE           :: JINDEX(:)
          INTEGER, ALLOCATABLE           :: ROWINDEX(:),COLINDEX(:)
          DOUBLE PRECISION               :: VT(1,1),U(1,1)
          DOUBLE PRECISION, ALLOCATABLE  :: BPVAL(:),BLBOUND(:),
     +                                      BUBOUND(:),SVAL(:),
     +                                      SVALUE(:),
     +                                      BSCALE(:),BOFFSET(:)
          DOUBLE PRECISION, ALLOCATABLE  :: BOWGHT(:)
          DOUBLE PRECISION, ALLOCATABLE  :: EIGMAT(:,:),SSS(:),
     +                                      WORK(:),TBX(:,:)
          DOUBLE PRECISION, ALLOCATABLE  :: XTX(:,:)
          DOUBLE PRECISION, ALLOCATABLE  :: AVSEN(:),SENFACTOR(:),SF(:)
          CHARACTER*12, ALLOCATABLE      :: BAPAR(:),BOBGNME(:)
          CHARACTER*12, ALLOCATABLE      :: BPARGPNME(:)
          CHARACTER*200                  :: BASEPARFILE
          CHARACTER*200                  :: TEMPBASEJACFILE='p###.jco'
          CHARACTER*200, ALLOCATABLE     :: BTPFLE(:),BINFLE(:)

C -- The following are used in the LSQR implementation.

          INTEGER                        :: LENIW=1
          INTEGER                        :: LENRW=1
          INTEGER                        :: IIW(1)
          DOUBLE PRECISION               :: RRW(1)
          DOUBLE PRECISION, ALLOCATABLE  :: UU(:),VV(:),WW(:),XX(:),
     +                                      BB(:),RR(:),EIGMAT1(:,:)
          DOUBLE PRECISION               :: SE(1)
          INTEGER                        :: IISTOP,IITN
          DOUBLE PRECISION               :: ANORM,ACOND,RNORM,ARNORM,
     +                                      XNORM

C -- The following are needed for BEOPEST.

          integer      :: svda_beoparset=0
          integer      :: last_nregadjpar
          integer      :: last_nlscaladj
          integer      :: ndim_tbx

        END MODULE SVD_PARM_DATA
#endif

#ifdef MKL
        MODULE LSQR_X_MKL
          integer use_mkl_flag
          double precision, allocatable :: x_mkl(:,:)
        END MODULE LSQR_X_MKL
#endif

       subroutine initialise_base(ifail,nlscaladj,beoslaveflag,set_bs)

C -- Subroutine initialise_base reads base parameter details in order to
C    implement SVD_ASSIST functionality.

       use pestdata, only: basepestfile,basejacfile,errmsg,cline,
     +                      npar,offset,pval,aobs,apar,isda,isdu,
     +                      svda_extsuper,svda_supdercalc,nxrow,
     +                      ninsfle,insfle,outfle,jacfile,
     +                      pestmode,par_nobgnm,obgnme,nobsgp,
     +                      svda_par_excl
       use svd_parm_data
       implicit none

       integer     i,ifail,ierr,jfail,icount,j,k,itemp1,itemp2,
     +             ies,irow,ipp,nn,bn,blwork1,iprior,n,nlscaladj,
     +             ic,tbgtrans,isum
       integer     beoslaveflag
       integer     bnrow,bncol,bicode,jpp,ii,jj,icol,ntemp
       integer     irr,ndim1,ndim2,ndim3,itemp
       integer     lw(3),rw(3)
       integer     workdim,sssdim
       integer     jjflag
       integer     set_bs
       integer     jjfail
       double precision dtemp,dtemp1,dtemp2,sum,standsen,rtemp,
     +                  avlogsen
       character*12     atemp,atemp1,atemp2,aapar,arow
       character*20     atempobs
       character*200    qfile,qfile1,aprior

C -- Initialisation

       ifail=0
       base_call=0
       nn=len_trim(basepestfile)-4
       if(nn.lt.1)nn=1
       baseparfile=basepestfile(1:nn)//'.bpa'
       call addquote(basepestfile,qfile)
       super_npar=npar
       param_count=0
       svda_beoparset=0
       last_nlscaladj = nlscaladj
       ndim_tbx=0

       do i=1,ninsfle
         if((index(insfle(i),'picalc.ins').gt.0).and.
     +      (index(outfle(i),'picalc.out').gt.0)) then
            include_prior=1
            go to 21
         end if
       end do
21     continue

! -- Parameter data is read from the base PEST file.

       open(unit=isda,file=basepestfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,20) trim(qfile)
20       format('Cannot open base PEST file ',a,'.')
         go to 9890
       end if
       read(isda,'(a)',err=9100,end=9100) cline
       read(isda,'(a)',err=9100,end=9100) cline
       read(isda,*,err=9100,end=9100) atemp1,atemp2
       call lowcas(atemp2)
       bpestmode=1
       if(atemp2(1:5).eq.'predi')then
         bpestmode=2
       else if(atemp2(1:5).eq.'regul')then
         bpestmode=3
       else if(atemp2(1:6).eq.'pareto')then
         bpestmode=4
       end if
       read(isda,*,err=9100,end=9100) bnpar,bnobs,bnpargp,
     + bnprior,bnobsgp
       if(bnpar.eq.0) go to 9100
       if(bnpargp.eq.0) go to 9100
       bnxrow=bnobs+bnprior

       if(svda_supdercalc.ne.0)then
         if(bnxrow.ne.nxrow)then
           svda_supdercalc=0
C           write(errmsg,22)
C22         format('Number of observations plus prior information ',
C     +     'equations cited in base PEST control file is not ',
C     +     'equal to that cited in super PEST control file. ',
C     +     'Hence SVDA_SUPDERCALC must be set to zero in ',
C     +     'super PEST control file.')
C           go to 9890
         end if
       end if

       read(isda,*,err=9100,end=9100) bntpfle

       biboundscale=0
       if(set_bs.ne.0)then
         biboundscale=1
       else
         do i=1,3
48         continue
           read(isda,'(a)',err=9100,end=9100) cline
           if(cline.eq.' ') go to 48
         end do
         call lowcas(cline)
         nn=index(cline,' noboundscale')
         if(nn.ne.0)then
           biboundscale=0
           cline(nn+1:nn+12)=' '
           if(index(cline,'boundscale').ne.0) go to 9100
         end if
         nn=index(cline,' boundscale')
         if(nn.ne.0)then
           biboundscale=1
           cline(nn+1:nn+10)=' '
           if(index(cline,'boundscale').ne.0) go to 9100
         end if
       end if
       if(biboundscale.ne.0) nlscaladj=4

       if(beoslaveflag.eq.0)then
         allocate(bpargpnme(bnpargp),bipargp(bnpar),senfactor(bnpargp),
     +   stat=ierr)
         if(ierr.ne.0) then
           write(errmsg,200)
           go to 9890
         end if
         do
           read(isda,'(a)',end=1050,err=9100) cline
           call lowcas(cline)
           if(index(cline,'* parameter gro').ne.0) go to 1060
         end do
1050     write(errmsg,1055) trim(qfile)
1055     format('Cannot find "parameter groups" section of base PEST ',
     +   'control file ',a,'.')
         go to 9890
1060     continue
         do i=1,bnpargp
           read(isda,*,err=9100,end=9100) bpargpnme(i)
           call lowcas(bpargpnme(i))
         end do
       end if

       do
         read(isda,'(a)',end=50,err=9100) cline
         call lowcas(cline)
         if(index(cline,'* parameter data').ne.0) go to 60
       end do
50     continue
       write(errmsg,55) trim(qfile)
55     format('Cannot find "parameter data" section of file ',a,'.')
       go to 9890
60     continue
       allocate(bapar(bnpar),bpval(bnpar),blbound(bnpar),bubound(bnpar),
     + bitrans(bnpar),sval(bnpar),svalue(npar),bscale(bnpar),
     + boffset(bnpar),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,200)
         go to 9890
       end if
       allocate(btpfle(bntpfle),binfle(bntpfle),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,200)
         go to 9890
       end if
       if(beoslaveflag.eq.0)then
         allocate(bowght(bnxrow),
     +   useobsgp(bnobsgp),bobgnme(bnobsgp),stat=ierr)
         if(ierr.ne.0) then
           write(errmsg,200)
           go to 9890
         end if
       end if
       do i=1,bnpar
         read(isda,*,err=9100,end=9100) bapar(i),atemp,atemp1,bpval(i),
     +   blbound(i),bubound(i),atemp2,bscale(i),boffset(i)
         call lowcas(bapar(i))
         call lowcas(atemp)
         if(atemp.eq.'log')then
           bitrans(i)=1
         else if(atemp.eq.'none')then
           bitrans(i)=0
         else if(atemp.eq.'fixed')then
           bitrans(i)=-1000000
         else if(atemp.eq.'tied')then
           bitrans(i)=-1
         else
           go to 9100
         end if
         if(beoslaveflag.eq.0)then
           call lowcas(atemp2)
           do j=1,bnpargp
             if(atemp2.eq.bpargpnme(j))then
               bipargp(i)=j
               go to 1070
             end if
           end do
           write(errmsg,1080) trim(bapar(i)),trim(atemp2),trim(qfile)
1080       format('Parameter "',a,'" is assigned to unknown ',
     +     'parameter group "',a,'" in base PEST control file ',a,'.')
           go to 9890
1070       continue
         end if
       end do
       if(beoslaveflag.ne.0) go to 411

       icount=0
       do i=1,bnpar
         if(bitrans(i).eq.-1)icount=icount+1
       end do
       do i=1,icount
         read(isda,*,err=9100,end=9100) atemp1,atemp2
         call lowcas(atemp1)
         call lowcas(atemp2)
         do j=1,bnpar
           if(atemp1.eq.bapar(j)) go to 70
         end do
         go to 9100
70       continue
         if(bitrans(j).ne.-1) go to 9100
         do k=1,bnpar
           if(atemp2.eq.bapar(k)) go to 80
         end do
         go to 9100
80       continue
         if(bitrans(k).lt.0) go to 9100
         bitrans(j)=-k
       end do

C -- Observation group names are read

       if(svda_par_excl.ne.0)then
         if(pestmode.ne.4) go to 9750
         if(nobsgp.ne.bnobsgp) go to 9750
         if(svda_extsuper.eq.1) go to 9750
       end if
       do
         read(isda,'(a)',err=9100,end=540) cline
         call lowcas(cline)
         if(index(cline,'* observation group').ne.0) go to 550
       end do
540    continue
       write(errmsg,545) trim(qfile)
545    format('Cannot find "observation groups" section of ',
     +'base PEST control file ',a,'.')
       go to 9890
550    continue
       do i=1,bnobsgp
         read(isda,'(a)',err=9100,end=9100) cline
         if(cline.eq.' ') go to 9100
         call linspl(jfail,2,lw,rw,cline)
         bobgnme(i)=cline(lw(1):rw(1))
         call lowcas(bobgnme(i))
         if(svda_par_excl.ne.0)then
           do jj=1,nobsgp
             if(obgnme(jj).eq.bobgnme(i)) go to 5501
           end do
           go to 9750
5501       continue
           jjflag=jj
         end if
C -- we get rid a number that may indicate we have observation-group-specific target objective functions.
         if(bpestmode.eq.3)then
           if(jfail.eq.0)then
             call drealrd(jjfail,cline(lw(2):rw(2)),dtemp)
             if(jjfail.eq.0)then
               cline(lw(2):rw(2))=' '
               call linspl(jfail,2,lw,rw,cline)
             end if
           end if
         end if
         if((svda_extsuper.eq.0).or.(abs(svda_extsuper).eq.2).or.
     +      (svda_extsuper.eq.3))then
           if(jfail.eq.0)then
             if(bpestmode.eq.3)then
               if(bobgnme(i)(1:5).ne.'regul')then
                 write(errmsg,560) trim(qfile)
560              format('Base PEST control file ',a,' cites a non-',
     +           'regularisation observation group that is assigned ',
     +           'a covariance matrix. This is not allowed if ',
     +           'SVDA_EXTSUPER is set to 0, 2, -2 or 3.')
                 go to 9890
               end if
             else if(pestmode.eq.4)then
               if(svda_par_excl.eq.1)then
                 if(jjflag.ne.par_nobgnm)then
                   continue
                 else
                   write(errmsg,5601) trim(qfile)
5601               format('Base PEST control file ',a,' cites an ',
     +             'observation group that is assigned ',
     +             'a covariance matrix. If ',
     +             'SVDA_PAR_EXCL is set to 1 this is only allowed if ',
     +             'the covariance matrix is assigned to a group ',
     +             'which is NOT the '
     +             'group with Pareto-adjustable weights.')
                   go to 9890
                 end if
               else if(svda_par_excl.eq.-1)then
                 if(jjflag.eq.par_nobgnm)then
                   continue
                 else
                   write(errmsg,5602) trim(qfile)
5602               format('Base PEST control file ',a,' cites an ',
     +             'observation group that is assigned ',
     +             'a covariance matrix. If ',
     +             'SVDA_PAR_EXCL is set to -1 this is only allowed ',
     +             'if the covariance matrix is assigned to the ',
     +             'group with Pareto-adjustable weights.')
                   go to 9890
                 end if
               else
                 write(errmsg,5603) trim(qfile)
5603             format('Base PEST control file ',a,' cites an ',
     +           'observation group that is assigned ',
     +           'a covariance matrix. This is only allowed '
     +           'if SVDA_PAR_EXCL is set to 1/-1 and the ',
     +           'covariance matrix is assigned to the group ',
     +           'without/with Pareto-adjustable weights.')
                 go to 9890
               end if
             else
               write(errmsg,560) trim(qfile)
               go to 9890
             end if
           end if
         end if
         useobsgp(i)=1
         if(pestmode.eq.4)then
           if(svda_par_excl.eq.1)then
             if(jjflag.ne.par_nobgnm)then
               useobsgp(i)=0
             end if
           else if(svda_par_excl.eq.-1)then
             if(jjflag.eq.par_nobgnm)then
               useobsgp(i)=0
             end if
           end if
         else
           if(bpestmode.eq.2)then
             if(bobgnme(i).eq.'predict')then
               useobsgp(i)=0
             end if
           else if(bpestmode.eq.3)then
             if(bobgnme(i)(1:5).eq.'regul')then
               useobsgp(i)=0
             end if
           end if
         end if
       end do

C -- Some observation data is read from the base PEST control file.

       do
         read(isda,'(a)',err=9100,end=82) cline
         call lowcas(cline)
         if(index(cline,'* observation data').ne.0) go to 81
       end do
82     continue
       write(errmsg,83) trim(qfile)
83     format('Cannot find "observation data" section of ',
     + 'base PEST control file ',a,'.')
       go to 9890
81     continue
       do i=1,bnobs
         read(isda,*,err=9100,end=9100) atempobs,dtemp1,dtemp2,atemp
         bowght(i)=dtemp2*dtemp2
         call lowcas(atemp)
         if(bpestmode.ne.1)then
           do j=1,bnobsgp
             if(atemp.eq.bobgnme(j))then
               if(useobsgp(j).eq.0) bowght(i)=0.0d0
               go to 410
             end if
           end do
           call lowcas(atempobs)
           write(errmsg,420) trim(atempobs),trim(atemp),trim(qfile)
420        format('Observation "',a,'" is assigned to an unknown ',
     +     'observation group "',a,'" in base PEST ',
     +     'control file ',a,'.')
           go to 9890
         end if
410      continue
       end do

C -- The names of template and corresponding model input files are acquired.


411    continue
       do
         read(isda,'(a)',err=9100,end=85) cline
         call lowcas(cline)
         if(index(cline,'* model input').ne.0) go to 86
       end do
85     continue
       write(errmsg,84) trim(qfile)
84     format('Cannot find "model input/output" section of ',
     + 'base PEST control file ',a)
       go to 9890
86     continue
       do i=1,bntpfle
         read(isda,'(a)',err=9100,end=9100) cline
         call spacesub(cline)
         call linspl(jfail,2,lw,rw,cline)
         if(jfail.ne.0) go to 9100
         btpfle(i)=cline(lw(1):rw(1))
         call remchar(btpfle(i),char(211))
         binfle(i)=cline(lw(2):rw(2))
         call remchar(binfle(i),char(211))
       end do
       if(beoslaveflag.ne.0)then
         allocate(sf(bnpar),stat=ierr)
         if(ierr.ne.0) then
           write(errmsg,200)
           go to 9890
         end if
         sf=1.0        ! The entire array is filled.
         close(unit=isda)
         return
       end if

C -- Prior information is now read from the base PEST control file if necessary.

       icprior=0
       if(bnprior.ne.0)then
         do
           read(isda,'(a)',err=9100,end=610) cline
           call lowcas(cline)
           if(index(cline,'* prior info').ne.0) go to 620
         end do
610      continue
         write(errmsg,615) trim(qfile)
615      format('Cannot find "prior information" section of ',
     +   'base PEST control file ',a,'.')
         go to 9890
620      continue
         iprior=0
         do
           read(isda,'(a)',err=9100,end=9100) cline
630        n=index(cline,'=')
           if(n.ne.0)then
             aprior=cline(n+1:)
             aprior=adjustl(aprior)
           else
             read(isda,'(a)',err=9100,end=9100) cline
             cline=adjustl(cline)
             if(cline(1:1).ne.'&') go to 9100
             go to 630
           end if
640        call linspl(jfail,3,lw,rw,aprior)
           if(jfail.ne.0)then
             read(isda,'(a)',err=9100,end=9100) cline
             cline=adjustl(cline)
             if(cline(1:1).ne.'&') go to 9100
             aprior=trim(aprior)//' '//trim(cline(2:))
             go to 640
           end if
           iprior=iprior+1
           if(bpestmode.ne.1)then
             atemp1=aprior(lw(3):rw(3))
             call lowcas(atemp1)
             do j=1,bnobsgp
               if(atemp1.eq.bobgnme(j)) go to 650
             end do
             write(errmsg,645) trim(atemp1)
645          format('Prior information group "',a,'" not named ',
     +       'in "observation groups" section of base PEST ',
     +       'control file ',a,'.')
             go to 9890
650          if(useobsgp(j).eq.0)then
               bowght(bnobs+iprior)=0.0d0
               go to 651
             end if
           end if
           call drealrd(jfail,aprior(lw(2):rw(2)),dtemp)
           if(jfail.ne.0) go to 9100
           bowght(bnobs+iprior)=dtemp*dtemp
           icprior=icprior+1
651        continue
           if(iprior.eq.bnprior) go to 670
         end do
       end if
670    continue
       if((bnprior.gt.0).and.(icprior.eq.0))then
         icprior=1
       else
         icprior=0
       end if
       close(unit=isda)

! -- The number of useable observations is evaluated.

       nuseobs=0
       do irow=1,bnxrow
         if(bowght(irow).gt.0.0d0) nuseobs=nuseobs+1
       end do
       if(npar.gt.nuseobs)then
         write(errmsg,671)
671      format('Execution cannot proceed: the requested number ',
     +   'of super parameters exceeds the number of non-zero-',
     +   'weighted, non-regularisation observations plus prior ',
     +   'information.')
         go to 9890
       end if

! -- The number of adjustable parameters is evaluated.

       bnespar=0
       do i=1,bnpar
         if(bitrans(i).ge.0) bnespar=bnespar+1
       end do
       if(bnespar.eq.0) go to 9100
       bnstmp=bnespar

! -- Standard values are assigned to base parameters. It is assumed that
!    initial PEST parameter values correspond to these.

       do i=1,bnpar
         sval(i)=bpval(i)
       end do
       do i=1,npar
         if(apar(i)(1:3).eq.'iw_') cycle
         svalue(i)=pval(i)+offset(i)
       end do

! -- The Jacobian matrix file is now read, if necessary.
!    But first the sf array is allocated and filled before we potentially branch
!    to another location.

       allocate(sf(bnpar),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,200)
         go to 9890
       end if
       sf=1.0        ! The entire array is filled.

       if(svda_extsuper.eq.1) go to 1200

       call open_jacobian(ierr,isdu,basejacfile)
       if(ierr.ne.0)then
         call addquote(basejacfile,qfile)
         write(errmsg,90) trim(qfile)
90       format('Cannot open unformatted Jacobian matrix file ',a,'.')
         go to 9890
       end if
       read(isdu,err=9200,end=9200)itemp1,itemp2
       if(abs(itemp1).ne.bnespar) go to 9200
       bn=abs(itemp2)
       if(itemp1.ge.0)then
         call addquote(basejacfile,qfile)
         write(errmsg,91) trim(qfile)
91       format('Base Jacobian matrix file ',a,' uses old format; ',
     +   'use JCOTRANS utility to translate it to new format.')
         go to 9890
       end if
       if((bn.ne.bnxrow).and.(bn.ne.bnobs))then
         call addquote(basejacfile,qfile1)
         write(errmsg,92) trim(qfile),trim(qfile1)
92       format('Base PEST control file ',a,' and base Jacobian ',
     +   'matrix file ',a,' cite different numbers of observations.')
         go to 9890
       end if
       if(svda_supdercalc.eq.1)then
         if(bn.ne.nxrow)then
           svda_supdercalc=0
C           write(errmsg,93)
C93         format('Number of observations plus prior information ',
C     +     'equations cited in super PEST control file is not ',
C     +     'equal to that cited in base Jacobian matrix file. ',
C     +     'Hence SVDA_SUPDERCALC must be set to zero in ',
C     +     'super parameter PEST control file.')
C           go to 9890
         end if
       end if
       if(icprior.ne.0)then
         allocate(rowindex(bnobs),colindex(bnespar),stat=ierr)
       else
         allocate(rowindex(bnxrow),colindex(bnespar),stat=ierr)
       end if
       if(ierr.ne.0)then
         write(errmsg,200)
200      format('Cannot allocate sufficient memory to hold base ',
     +   'parameter data.')
         go to 9890
       end if
       rowindex=0    ! an array
       itemp=bnobs
       if(icprior.eq.0) itemp=bnxrow
       irr=0
       do irow=1,itemp
         if(bowght(irow).gt.0.0d0) then
           irr=irr+1
           rowindex(irow)=irr
         end if
       end do
       do ies=1,bnespar
         colindex(ies)=ies
       end do


       blwork=max(3*min(bnespar,nuseobs)+max(bnespar,nuseobs),
     +            5*min(bnespar,nuseobs))+max(bnespar,nuseobs)
       if(svda_extsuper.eq.3)then
         blwork=8*bnespar
       end if
       blwork1=max(blwork,bnespar)
       ldtbx=max(npar,nuseobs)
       if(svda_extsuper.eq.3) ldtbx=max(ldtbx,bnespar)
       allocate(tbx(ldtbx,bnespar),stat=ierr)
       if(ierr.ne.0) then
         write(errmsg,200)
         go to 9890
       end if
       if((svda_extsuper.eq.0).or.(svda_extsuper.eq.3))then
         workdim=blwork1
         if(jacfile.ne.0)workdim=max(blwork1,bnpar)
         sssdim=bnespar
         if(jacfile.ne.0)sssdim=max(bnespar,bnpar)
         allocate(work(workdim),sss(sssdim),stat=ierr)
         if(ierr.ne.0) then
           write(errmsg,200)
           go to 9890
         end if
       else if(abs(svda_extsuper).eq.2)then
         if(svda_supdercalc.eq.1)then
           workdim=bnespar
           if(jacfile.ne.0)workdim=max(bnespar,bnpar)
           sssdim=1
           if(jacfile.ne.0)sssdim=bnpar
           allocate(work(workdim),sss(sssdim),stat=ierr)    ! note that supdercalc must be 1 if jacfile is not 0
           if(ierr.ne.0) then
             write(errmsg,200)
             go to 9890
           end if
         end if
       end if
       if(svda_extsuper.eq.2)then
         allocate(uu(nuseobs),vv(bnespar),ww(bnespar),xx(bnespar),
     +   bb(nuseobs),eigmat(bnespar,npar),stat=ierr)
         if(ierr.ne.0)then
           write(errmsg,200)
           go to 9890
         end if
       else if(svda_extsuper.eq.-2)then
         ndim1=max(bnespar,npar)
         ndim2=max(nuseobs,npar)
         ndim3=npar*npar/2+npar
         allocate(uu(ndim2),vv(bnespar),ww(ndim1),xx(bnespar),
     +   bb(nuseobs),eigmat(bnespar,npar),eigmat1(bnespar,npar),
     +   rr(ndim3),stat=ierr)
         if(ierr.ne.0)then
           write(errmsg,200)
           go to 9890
         end if
       end if
       do i=1,bnespar
         do j=1,nuseobs
           tbx(j,i)=0.0d0
         end do
       end do
       read(isdu,err=9200,end=9200)icount
       do i=1,icount
         read(isdu,err=9200,end=9200) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         if(irow.gt.bnobs) bnpriorcount=bnpriorcount+1
         irr=0
         if(icprior.ne.0)then
           if(irow.le.bnobs) irr=rowindex(irow)
         else
           irr=rowindex(irow)
         end if
         if(irr.ne.0) then
           tbx(irr,ies)=dtemp*sqrt(bowght(irow))
         end if
       end do

       j=0
       do ipp=1,bnespar
         read(isdu,err=9200,end=9200) atemp
         call lowcas(atemp)
210      j=j+1
         if(j.gt.bnpar) go to 9200
         if(bitrans(j).lt.0) go to 210
         if(bapar(j).ne.atemp) go to 9200
       end do
       if(abs(svda_extsuper).eq.2)then
         do irow=1,bn
           read(isdu,end=9700,err=9700)  atempobs
         end do
         irr=0
         if(icprior.ne.0)then
           itemp=bnobs
         else
           itemp=bnxrow
         end if
         do irow=1,itemp
           read(isdu,end=9720,err=9720) dtemp1,dtemp2
           if(bowght(irow).gt.0.0d0)then
             irr=irr+1
             bb(irr)=-(dtemp1-dtemp2)*sqrt(bowght(irow))
           end if
         end do
       end if
       close(unit=isdu)

       deallocate(bobgnme,useobsgp,stat=ierr)

! -- If non-log-transformed parameter scale adjustment is operative
!    this is now handled.

       if(nlscaladj.eq.0) then
         deallocate(bipargp,stat=ierr)
         return
       end if

! -- Options -3, 3, -4, 4 are handled first.

       if(abs(nlscaladj).ge.3)then
         do i=1,bnpar
           if(bitrans(i).eq.0)then
             rtemp=bubound(i)-blbound(i)
             if(rtemp.le.0.0d0) go to 9100
             sf(i)=rtemp*0.25
             if(nlscaladj.lt.0)then
               if(sf(i).gt.1.0d0) sf(i)=1.0d0
             end if
           else
             sf(i)=1.0d0
           end if
         end do
         ies=0
         do i=1,bnpar
           if(bitrans(i).ge.0) ies=ies+1
           if(bitrans(i).eq.0)then
             rtemp=sf(i)
             do irow=1,nuseobs
               tbx(irow,ies)=tbx(irow,ies)*rtemp
             end do
           end if
         end do

         if(abs(nlscaladj).eq.3) return
         do i=1,bnpar
           if(bitrans(i).eq.1)then
             rtemp=log10(bubound(i)/blbound(i))
             if(rtemp.le.0.0d0) go to 9100
             sf(i)=rtemp*0.25
             if(nlscaladj.lt.0)then
               if(sf(i).gt.1.0d0) sf(i)=1.0d0
             end if
           end if
         end do
         ies=0
         do i=1,bnpar
           if(bitrans(i).ge.0) ies=ies+1
           if(bitrans(i).eq.1)then
             rtemp=sf(i)
             do irow=1,nuseobs
               tbx(irow,ies)=tbx(irow,ies)*rtemp
             end do
           end if
         end do
         return
       end if

! -- Now options -2, -1, 1, and 2 are handled.
! -- First we declare a group as untransformed or log-transformed.

       allocate(bgtrans(bnpargp),avsen(bnpargp),imem(bnpargp),
     + stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,200)
         go to 9890
       end if

       do i=1,bnpargp
         bgtrans(i)=-9999
         ic=0
         do j=1,bnpar
           if(bipargp(j).eq.i)then
             if(bitrans(j).eq.0)then
               ic=ic+1
               tbgtrans=0
             else if(bitrans(j).eq.1)then
               ic=ic+1
               tbgtrans=1
             else
               cycle
             end if
             if(ic.eq.1)then
               bgtrans(i)=tbgtrans
             else
               if(tbgtrans.ne.bgtrans(i))then
                 call addquote(basepestfile,qfile)
                 write(errmsg,1110) trim(bpargpnme(i)),trim(qfile)
1110             format('If base parameter ',
     +           'scaling of type -2, -1, 1 or 2 is activated, ',
     +           'all adjustable base parameters ',
     +           'belonging to any parameter group must be either ',
     +           'log-transformed or untransformed; this does not ',
     +           'apply to group "',a,'" in file ',a,'.')
                 go to 9890
               end if
             end if
             go to 1130
           end if
1130       continue
         end do
       end do

! -- Now we find the average sensitivity for the group.

       do i=1,bnpargp
         avsen(i)=0.0d0
         imem(i)=0
       end do
       ies=0
       do i=1,bnpar
         if(bitrans(i).lt.0) cycle
         ies=ies+1
         sum=0.0d0
         irr=0
         itemp=bnxrow
         if(icprior.ne.0) itemp=bnobs
         do j=1,itemp
           if(bowght(j).gt.0.0d0) then
             irr=irr+1
             sum=sum+tbx(irr,ies)*tbx(irr,ies)
           end if
         end do
         j=bipargp(i)
         avsen(j)=avsen(j)+sqrt(sum)
         imem(j)=imem(j)+1
       end do
       sum=0.0d0
       isum=0
       do i=1,bnpargp
         if(imem(i).gt.0)then
           if(bgtrans(i).eq.1)then
             isum=isum+imem(i)
             sum=sum+avsen(i)
           end if
           avsen(i)=avsen(i)/imem(i)
         end if
       end do
       if(isum.eq.0)then
         call addquote(basepestfile,qfile)
         write(errmsg,1139) trim(qfile)
1139     format('If base parameter scaling of ',
     +   'type -2, -1, 1 or 2 is activated, at least one adjustable ',
     +   'parameter must be log-transformed in base PEST ',
     +   'control file ',a,'.')
         go to 9890
       end if
       avlogsen=sum/isum

! -- At least one parameter group must be log-transformed

       do i=1,bnpargp
         if(bgtrans(i).eq.1) go to 1140
       end do
       call addquote(basepestfile,qfile)
       write(errmsg,1139) trim(qfile)
       go to 9890
1140   continue

! -- We find the log-transformed group with the highest sensitivity.

       standsen=0.0d0
       do i=1,bnpargp
         if(bgtrans(i).eq.1)then
           if(avsen(i).gt.standsen) standsen=avsen(i)
         end if
       end do
       if(standsen.eq.0.0d0)then
         call addquote(basepestfile,qfile)
         write(errmsg,1141) trim(qfile)
1141     format('Base parameter scaling of ',
     +   'type -2, -1, 1 or 2 cannot be activated as all ',
     +   'log-transformed parameters cited in base PEST ',
     +   'control file ',a,' are insensitive.')
         go to 9890
       end if

! -- Now we find a scaling factor for non-log-transformed groups.

       do i=1,bnpargp
         senfactor(i)=-1.1d35
       end do
       do i=1,bnpargp
         if(bgtrans(i).eq.0)then
           if(avsen(i).eq.0.0d0)then
             senfactor(i)=1.0d0
           else if (avsen(i).lt.1.0d-30)then   !arbitrary
             senfactor(i)=1.0d0
           else
             if(abs(nlscaladj).eq.1)then
               senfactor(i)=standsen/avsen(i)
             else if(abs(nlscaladj).eq.2)then
               senfactor(i)=avlogsen/avsen(i)
             end if
             if(nlscaladj.lt.0)then
               if(senfactor(i).ge.1.0d0) senfactor(i)=1.0d0
             end if
           end if
         end if
       end do

! -- Now all of the sensitivities for each parameter are multiplied by that factor.

       ies=0
       do i=1,bnpar
         if(bitrans(i).ge.0) ies=ies+1
         if(bitrans(i).eq.0)then
           j=bipargp(i)
           rtemp=senfactor(j)
           if(rtemp.gt.0.0d0)then
             if(rtemp.ne.1.0d0)then
               do irow=1,nuseobs
                 tbx(irow,ies)=tbx(irow,ies)*rtemp
               end do
             end if
           end if
         end if
       end do

! -- The SF array is filled.

       do i=1,bnpar
         j=bipargp(i)
         if(senfactor(j).gt.0.0d0)then
           sf(i)=senfactor(j)
         else
           sf(i)=1.0d0
         end if
       end do

       deallocate(bipargp,bgtrans,avsen,imem,stat=ierr)

       return

C -- The situation where the eigenvalue matrix is provided is now
C    accomodated.

1200   continue

       nlscaladj=0
       last_nlscaladj = nlscaladj
       call addquote(basejacfile,qfile)
       open(unit=isdu,file=basejacfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,1210) trim(qfile)
1210     format('Cannot open super parameter definition ',
     +   'file ',a,'.')
         go to 9890
       end if
       read(isdu,'(a)',iostat=ierr) cline
       if(ierr.ne.0) go to 9300
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 9300
       call numrd(ifail,0,lw(1),rw(1),bnrow,dtemp,cline)
       if(ifail.ne.0) go to 9300
       call numrd(ifail,0,lw(2),rw(2),bncol,dtemp,cline)
       if(ifail.ne.0) go to 9300
       call numrd(ifail,0,lw(3),rw(3),bicode,dtemp,cline)
       if(ifail.ne.0) go to 9300
       if((bnrow.le.0).or.(bncol.le.0)) go to 9350
       if((bicode.ne.-1).and.(bicode.ne.1).and.(bicode.ne.2))
     + go to 9350
       if(bnrow.lt.bnespar) go to 9400
       if(bncol.lt.npar)then
         write(errmsg,1220) trim(qfile)
1220     format('The matrix supplied in file ',a,' must have at ',
     +   'least as many columns as there are super parameters in',
     +   ' the present SVD-assisted inversion problem.')
         go to 9890
       end if
       allocate(tbx(npar,bnespar),jindex(bnrow),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,1230)
1230     format('Cannot allocate sufficient memory to continue ',
     +   'execution.')
         go to 9890
       end if
       jindex=0                       ! an array
       if(bicode.eq.-1) tbx=0.0d0  ! an array
       do
         read(isdu,'(a)',err=9450,end=9500) cline
         cline=adjustl(cline)
         if(cline(1:1).ne.'*') cycle
         call lowcas(cline)
         if(index(cline,'row').ne.0) go to 1250
       end do
1250   continue
       jpp=1
       do i=1,bnrow
         read(isdu,*,err=9450,end=9550) aapar
         call lowcas(aapar)
         do ipp=jpp,bnpar
           if(aapar.eq.bapar(ipp))then
             if(bitrans(ipp).ge.0)then
               jindex(i)=ipp
             end if
             jpp=ipp
             go to 1240
           end if
         end do
         if(jpp.ne.1)then
           do ipp=jpp-1,1,-1
             if(aapar.eq.bapar(ipp))then
               if(bitrans(ipp).ge.0)then
                 jindex(i)=ipp
               end if
               jpp=ipp
               go to 1240
             end if
           end do
         end if
1240     continue
       end do
       ii=1
       ies=0
       do ipp=1,bnpar
         if(bitrans(ipp).lt.0) cycle
         ies=ies+1
         do i=ii,bnrow
           if(jindex(i).eq.ipp) then
             jindex(i)=-ies
             ii=i
             go to 1251
           end if
         end do
         if(ii.ne.1)then
           do i=ii-1,1,-1
             if(jindex(i).eq.ipp)then
               jindex(i)=-ies
               ii=i
               go to 1251
             end if
           end do
         end if
         write(errmsg,1241) trim(bapar(ipp)),trim(qfile)
1241     format('Base parameter name "',a,'" not cited as a ',
     +   'matrix row in super parameter file ',a,'.')
         go to 9890
1251     continue
       end do
       do i=1,bnrow
         jindex(i)=-jindex(i)
       end do
       rewind(unit=isdu)
       read(isdu,*)
       do irow=1,bnrow
         if(jindex(irow).eq.0)then
           if(bicode.ne.-1)then
             read(isdu,*,err=9600) (dtemp,icol=1,bncol)
           else
             read(isdu,*,err=9600) dtemp
           end if
         else
           jj=jindex(irow)
           if(bicode.ne.-1)then
             if(npar.eq.bncol)then
               read(isdu,*,err=9600) (tbx(icol,jj),icol=1,bncol)
             else
               ntemp=bncol-npar
               read(isdu,*,err=9600) (tbx(icol,jj),icol=1,npar),
     +         (dtemp,icol=1,ntemp)
             end if
           else
             read(isdu,*,err=9600) dtemp
             if(irow.le.npar) tbx(irow,jj)=dtemp
           end if
         end if
       end do
       close(unit=isdu)

       return

9100   continue
       write(errmsg,9110) trim(qfile)
9110   format('Error reading base PEST control file ',a,
     + '; check it with PESTCHEK.')
       go to 9890
9200   call addquote(basejacfile,qfile1)
       write(errmsg,9210) trim(qfile1),trim(qfile)
9210   format('Error reading unformatted base Jacobian matrix file ',a,
     + '. File is incompatible with base PEST control file file ',
     + a,'.')
        go to 9890
9300   write(errmsg,9310) trim(qfile)
9310   format('Error reading first line (i.e. header line) ',
     + 'of super parameter matrix file ',a,'.')
       go to 9890
9350   write(errmsg,9360) trim(qfile)
9360   format('Improper header on first line ',
     + 'of super parameter definition file ',a,'.')
       go to 9890
9400   call addquote(basepestfile,qfile1)
       write(errmsg,9410) trim(qfile),trim(qfile1)
9410   format('There are less rows in file ',a,' than the number ',
     + 'of adjustable base parameters cited in PEST control ',
     + 'file 'a,'.')
       go to 9890
9450   write(errmsg,9460) trim(qfile)
9460   format('Error encountered in super parameter definition ',
     + 'file ',a,'.')
       go to 9890
9500   continue
       if(bicode.eq.2)then
         write(errmsg,9510) trim(qfile)
9510     format('Cannot find "* row names" header in super ',
     +   'parameter definition file ',a,'.')
         go to 9890
       else
         write(errmsg,9520) trim(qfile)
9520     format('Cannot find "* row and column names" header in ',
     +   'super parameter definition file ',a,'.')
         go to 9890
       end if
9550   write(errmsg,9560) trim(qfile)
9560   format('Unexpected end encountered to super parameter ',
     + 'definition file ',a,'.')
       go to 9890
9600   call writint(arow,irow)
       write(errmsg,9610) trim(arow),trim(qfile)
9610   format('Error encountered when reading matrix row ',a,
     + ' from super parameter definition file ',a,'.')
       go to 9890
9700   call addquote(basejacfile,qfile1)
       write(errmsg,9710) trim(qfile1)
9710   format('Error reading observation data from base ',
     + 'Jacobian matrix file ',a,'; was this file written ',
     + 'with using version 11 or later of PEST?')
       go to 9890
9720   call addquote(basejacfile,qfile1)
       call addquote(basepestfile,qfile)
       write(errmsg,9730) trim(qfile1),trim(qfile),trim(qfile1)
9730   format('Error reading observation data from base ',
     + 'Jacobian matrix file ',a,'. Maybe JCOPCAT was used ',
     + 'in production of this file. Run PEST on base PEST control ',
     + 'file ',a,' using /i switch with NOPTMAX set to -2 to re-write ',
     + 'this JCO file. Respond with ',a,' when asked for name of ',
     + 'existing JCO file.')
       go to 9890
9750   write(errmsg,9760)
9760   format('SVDA_PAR_EXCL must not be set to 1 or -1 unless PEST ',
     + 'is run in "pareto" mode, unless super-parameters are defined ',
     + 'using base PEST control file (i.e. not defined externally), ',
     + 'and unless base and super PEST control files have ',
     + 'identically-named observation groups.')
       go to 9890

9890   ifail=1
       return

#ifdef PESTMOD
       end subroutine initialise_base
#else
       end
#endif




       subroutine base_pardef(ifail,irec,ireason,nregadjpar,iopt,
     + nlscaladj)

C -- Subroutine base_pardef defines super parameters on the basis of base parameters.
C    It then writes the template to a PARCALC
C    input file.

       use pestdata, only : npar,scale,offset,pval,errmsg,apar,
     +                      isda,isdu,svda_extsuper,svda_supdercalc,
     +                      basejacfile,basepestfile
       use svd_parm_data
       implicit none

       integer          ifail,ies,i,j,icount,ipp,iess,jes,irec,
     +                  irow,info,ireason,jfail,jcount,jj,
     +                  nregadjpar,iopt,nlscaladj,itemp1,itemp2,
     +                  itemp,k
       integer          tempbnstmp,bn
       integer          irr,ierr,icol
       double precision rtemp,dtemp
       character*200    qfile1,qfile2,infile

       external bprod

C -- Initialisation

       base_call=base_call+1
       ifail=0
       if(ireason.eq.2) return

C -- First we calculate base parameter values using super parameter values.

       if(base_call.eq.1)then
         do i=1,bnpar
           bpval(i)=sval(i)
         end do
       else
         ies=0
         do i=1,bnpar
           if(bitrans(i).le.-1000000) then
             bpval(i)=sval(i)
           else if(bitrans(i).ge.0)then
             ies=ies+1
             if(bitrans(i).eq.0)then
               rtemp=sval(i)
             else
               rtemp=log10(sval(i))
             end if
             jj=0
             do j=1,npar
               if(scale(j).lt.-1.0d35) cycle
               jj=jj+1
               if(nlscaladj.eq.0)then
                 rtemp=rtemp+tbx(jj,ies)*(pval(j)+
     +           offset(j)-svalue(j))
               else
                 rtemp=rtemp+tbx(jj,ies)*(pval(j)+
     +           offset(j)-svalue(j))*sf(i)
! Note that SF is not allocated unless nlscaladj.ne.0
               end if
             end do
             if(bitrans(i).eq.1)then
                bpval(i)=10**rtemp
             else
               bpval(i)=rtemp
             end if
           end if
         end do

! -- Bounds are now enforced; note that they are not enforced on tied
!    parameters.

         do i=1,bnpar
           if(bitrans(i).ge.0)then
             if(bpval(i).ge.bubound(i))bpval(i)=bubound(i)
             if(bpval(i).lt.blbound(i))bpval(i)=blbound(i)
          end if
        end do

! -- The values of tied parameters are calculated.

         do i=1,bnpar
           if((bitrans(i).lt.0).and.(bitrans(i).gt.-1000000)) then
             j=-bitrans(i)
             bpval(i)=bpval(j)*sval(i)/sval(j)
           end if
         end do
       end if
       if(svda_save.eq.1)then
         call bprmsav(jfail,iopt)
         if(jfail.ne.0) go to 9890
         svda_save=0
       end if
       if(ireason.eq.1) return

C -- Now we identify which base parameters have hit their bounds and freeze them
C    accordingly.

       if(svda_extsuper.eq.1) jindex=0     ! an array
       icount=0
       ies=0
       do i=1,bnpar
         if(bitrans(i).ge.0)then
           ies=ies+1
           if(bpval(i).ge.bubound(i))then
             bpval(i)=bubound(i)
             if(bitrans(i).eq.0)then
               bitrans(i)=-1000001
             else
               bitrans(i)=-1000002
             end if
             icount=icount+1
             if(svda_extsuper.eq.1)then
               jindex(icount)=ies
             end if
           end if
           if(bpval(i).le.blbound(i))then
             bpval(i)=blbound(i)
             if(bitrans(i).eq.0)then
               bitrans(i)=-1000001
             else
               bitrans(i)=-1000002
             end if
             icount=icount+1
             if(svda_extsuper.eq.1)then
               jindex(icount)=ies
             end if
           end if
         end if
       end do

C -- If no parameters have hit their bounds, then there is no need to re-formulate the
C    super-parameter problem. So we can skip the following.

       if((svda_extsuper.eq.0).or.(abs(svda_extsuper).eq.2).or.
     +    (svda_extsuper.eq.3))then
         if(newbaseder.gt.0) go to 593
         if((base_call.gt.1).and.(icount.eq.0)) go to 601
         if((base_call.eq.1).and.(icount.eq.0)) then
           go to 594
         end if
       else if(svda_extsuper.eq.1)then
         if(icount.eq.0) go to 601
       end if

       if(svda_extsuper.eq.1) then
         tempbnstmp=bnstmp
         bnstmp=bnstmp-icount
         if(bnstmp.le.0)then
           write(errmsg,410)
           go to 9890
         end if
         do i=1,icount
           ies=jindex(i)
           if(ies.lt.tempbnstmp)then
             ies=ies-(i-1)
             do ipp=1,npar
               do jes=ies+1,tempbnstmp-(i-1)
                 tbx(ipp,jes-1)=tbx(ipp,jes)
               end do
             end do
           end if
         end do
         go to 601
       end if

C -- We need to re-read the base jacobian matrix. But first we need to establish
C    which parameters are tied or fixed.

593    continue
       if(newbaseder.eq.0)then
         infile=basejacfile
       else
         infile=tempbasejacfile
       end if
       ies=0
       iess=0
       do ipp=1,bnpar
         if((bitrans(ipp).ge.0).or.
     +      (bitrans(ipp).le.-1000001))then
            ies=ies+1
            if(bitrans(ipp).le.-1000001)then
              colindex(ies)=0
            else
              iess=iess+1
              colindex(ies)=iess
            end if
         end if
       end do
       bnstmp=iess
       if(bnstmp.eq.0)then
          write(errmsg,410)
410       FORMAT('Cannot define any super parameters as all ',
     +    'model parameters are fixed, tied or at their bounds')
          go to 9890
       end if

       call open_jacobian(ierr,isdu,infile)
       if(ierr.ne.0)then
         call addquote(infile,qfile1)
         write(errmsg,90) trim(qfile1)
90       format('Cannot open unformatted Jacobian matrix file ',a,'.')
         go to 9890
       end if
       read(isdu,err=9200,end=9200)itemp1,itemp2
       if(abs(itemp1).ne.bnespar) go to 9200
       bn=abs(itemp2)
       if(itemp1.ge.0)then
         call addquote(infile,qfile1)
         write(errmsg,91) trim(qfile1)
91       format('Base Jacobian matrix file ',a,' uses old format; ',
     +   'use JCOTRANS utility to translate it to new format.')
         go to 9890
       end if
       if((bn.ne.bnxrow).and.(bn.ne.bnobs))then
         call addquote(infile,qfile1)
         call addquote(basepestfile,qfile2)
         write(errmsg,92) trim(qfile2),trim(qfile1)
92       format('Base PEST control file ',a,' and base Jacobian ',
     +   'matrix file ',a,' cite different numbers of observations.')
         go to 9890
       end if
       do i=1,bnstmp
         do j=1,nuseobs
           tbx(j,i)=0.0d0
         end do
       end do
       read(isdu,err=9200,end=9200)icount
       do i=1,icount
         read(isdu,err=9200,end=9200) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         icol=colindex(ies)
         if(icol.gt.0)then
           irr=0
           if(icprior.ne.0)then
             if(irow.le.bnobs)irr=rowindex(irow)
           else
             irr=rowindex(irow)
           end if
           if(irr.gt.0) tbx(irr,icol)=dtemp*sqrt(bowght(irow))
         end if
       end do
       close(unit=isdu)

C -- Scale factor adjustment is now done on this array. Note that the same scale factor
C    adjustment is done as that which was done on the original array based on original
C    sensitivities. This is not quite applicable if derivatives have bave been updated
C    because of a non-zero JACFILE setting.

       if(nlscaladj.ne.0)then
         ies=0
         do i=1,bnpar
           if((bitrans(i).ge.0).or.(bitrans(i).le.-1000001)) then
             ies=ies+1
             rtemp=sf(i)
             if(rtemp.ne.1.0d0)then
               itemp=colindex(ies)
               if(itemp.ne.0)then
                 do irow=1,nuseobs
                   tbx(irow,itemp)=tbx(irow,itemp)*rtemp
                 end do
               end if
             end if
           end if
         end do
       end if

594    continue

       if(svda_extsuper.eq.3)then
         allocate(xtx(bnstmp,bnstmp),stat=ierr)
         if(ierr.ne.0)then
           write(errmsg,598)
598        format('Cannot allocate sufficient memory - ',
     +     'try an SVDA_EXTSUPER setting other than 3.')
           go to 9890
         end if
         do i=1,bnstmp
           do j=1,i
             rtemp=0.0d0
             do k=1,nuseobs
               rtemp=rtemp+tbx(k,i)*tbx(k,j)
             end do
             xtx(j,i)=rtemp
           end do
         end do
         do i=1,bnstmp-1
           do j=i+1,bnstmp
             xtx(j,i)=xtx(i,j)
           end do
         end do
         call dgesvd( 'O','N',bnstmp,bnstmp,xtx,bnstmp,sss,tbx,
     +                 1, vt, 1, work, blwork, info )
         if(info.ne.0)then
           write(errmsg,595)
           go to 9890
         end if
         do i=1,bnstmp
           do j=1,bnstmp
             tbx(j,i)=xtx(i,j)
           end do
         end do
         deallocate(xtx,stat=ierr)
         go to 601
       end if
599    continue
       if(svda_extsuper.eq.0)then
         call dgesvd( 'N','O',nuseobs,bnstmp,tbx,nuseobs,sss,u,
     +                 1, vt, 1, work, blwork, info )
         if(info.ne.0)then
           write(errmsg,595)
595        format('DGESVD returned INFO as non-zero in subroutine ',
     +     'BASE_PARDEF.')
           go to 9890
         end if

!Regarding LSQR
!            what happens with resproc etc when we compute the resolution matrix.
!            is it possible to orthogonalise without having to use eigmat1??
       else
         if(abs(svda_extsuper).eq.2)then
596        uu=bb                 ! arrays
           call lsqr(nuseobs,bnstmp,bprod,0.0d0,.false.,
     +               1,1,iiw,rrw,
     +               uu,vv,ww,xx,se,
     +               1.0d-10,1.0d-10,1.0d200,npar,0,
     +               iistop,iitn,anorm,acond,rnorm,arnorm,xnorm,1)
           if((svda_extsuper.eq.-2).and.(qrfailflag.eq.0))then
             call qrfact(bnstmp,npar,eigmat,bnespar,eigmat1,
     +       bnespar,rr,vv,uu,ww,xx,info)
             if(info.ne.0)then
               qrfailflag=1
               go to 596
             else
               do j=1,npar
                 do i=1,bnstmp
                   tbx(j,i)=eigmat1(i,j)
                 end do
               end do
             end if
           else
             do j=1,npar
               do i=1,bnstmp
                 tbx(j,i)=eigmat(i,j)
               end do
             end do
           end if
         end if
       end if

C -- Parameter reference values are assigned.

601     continue
        do i=1,bnpar
          sval(i)=bpval(i)
        end do
        do i=1,npar
          if(scale(i).lt.-1.0d35) cycle
          svalue(i)=pval(i)+offset(i)
        end do

C -- If any super parameters are too close to zero, their offset is re-assigned.

        jcount=0
        do i=1,npar
          if(scale(i).lt.-1.0d35) cycle
          if(pval(i).lt.6.0d0)then
            jcount=jcount+1
            if(jcount.eq.1) write(irec,*)
            offset(i)=offset(i)-10.0d0
            pval(i)=pval(i)+10.0d0
            write(irec,602) trim(apar(i))
602         format('    Offset for super parameter "',a,
     +      '" increased by 10.')
          end if
        end do

C -- The template of the PARCALC input file is written.

        call write_parcalc_tpl(ifail,nregadjpar,nlscaladj)
        if(ifail.ne.0) go to 9890

        svda_beoparset=1
        last_nregadjpar = nregadjpar
        last_nlscaladj=nlscaladj

        return

9100    write(errmsg,9110)
9110    format('Cannot write to PARCALC template file parcalc.tpl ',
     +  'to update this file.')
        go to 9890

9200    call addquote(infile,qfile1)
        call addquote(basepestfile,qfile2)
        write(errmsg,9210) trim(qfile1),trim(qfile2)
9210    format('Error reading unformatted base Jacobian matrix file ',a,
     +  'or file is incompatible with base PEST control file ',
     +  a,'.')
        go to 9890

9890    ifail=1
        return

#ifdef PESTMOD
        end subroutine base_pardef
#else
        end
#endif



        SUBROUTINE bprmsav(JFAIL,iopt)

C -- SUBROUTINE bprmsav SAVES CURRENT base PARAMETER VALUES TO A PEST PARAMETER
C -- VALUE FILE

        use svd_parm_data
        use pestdata, only: errmsg,ipfl,pestmode,ippd
        implicit none

        integer jfail,i,iopt,ii
        character*10 atemp
        character*200 qfile,afile

        jfail=0
        do ii=1,2
          if(pestmode.eq.4)then
            if(ii.eq.1)cycle
          end if
          if(ii.eq.1)then
            call addquote(baseparfile,afile)
            open(unit=ipfl,file=baseparfile,err=9000)
          else
            if(iopt.lt.0) return
            call writint(atemp,iopt)
            afile=trim(baseparfile)//'.'//trim(atemp)
            call addquote(afile,qfile)
            open(unit=ipfl,file=afile,err=9000)
          end if
          write(ipfl,10,err=9000)
10        format('single point')
          do i=1,bnpar
            write(ipfl,20,err=9000) trim(bapar(i)),bpval(i),
     +      bscale(i),boffset(i)
20          format(1x,a,t15,1pg14.7,2x,1pg13.6,2x,1pg13.6)
          end do
          close(unit=ipfl)
          if((pestmode.eq.4).and.(iopt.ne.0))then
            write(ippd) (bpval(i),i=1,bnpar)
#ifdef FLUSHFILE
            call flush(ippd)
#endif
          end if
        end do

        return

9000    continue
        write(errmsg,9010) trim(qfile)
9010    format('Cannot write to parameter value file ',a)
        jfail=1

        return
#ifdef PESTMOD
        end SUBROUTINE bprmsav
#else
        end
#endif



        subroutine base_supdercalc(ifail,nlscaladj,imode)

        use pestdata, only : npar,nxrow,nespar,x,ncompdim,xc,ixc,
     +                       maxcompdim,istart_c,ifound_c,derzerolim,
     +                       isdu,basejacfile,basepestfile,errmsg
        use svd_parm_data
        implicit none

        integer ifail,imode
        integer j,i,jfail,ierr,itemp1,itemp2,bn,icount,ies,irow,
     +  ipp,iess,nlscaladj
        double precision dtemp,rtemp
        character*200 qfile1,qfile2
        character*200 infile
#ifdef INTEL
        integer (8) :: i8
#else
        integer        i8
#endif


C -- The base Jacobian matrix is once again read.

C -- First the base Jacobian file is opened (don't forget to close it later).

        if(imode.eq.1)then
          infile=basejacfile
        else
          infile=tempbasejacfile
        end if
        call open_jacobian(ierr,isdu,infile)
        if(ierr.ne.0)then
          call addquote(infile,qfile1)
          write(errmsg,90) trim(qfile1)
90        format('Cannot open unformatted Jacobian matrix file ',a,'.')
          go to 9890
        end if
        read(isdu,err=9200,end=9200)itemp1,itemp2
        if(abs(itemp1).ne.bnespar) go to 9200
        bn=abs(itemp2)
        if(itemp1.ge.0)then
          call addquote(infile,qfile1)
          write(errmsg,91) trim(qfile1)
91        format('Base Jacobian matrix file ',a,' uses old format; ',
     +    'use JCOTRANS utility to translate it to new format.')
          go to 9890
        end if
        if(bn.ne.nxrow)then
            write(errmsg,93)
93          format('Number of observations plus prior information ',
     +      'equations cited in super PEST control file is not ',
     +      'equal to that cited in base Jacobian matrix file. ',
     +      'Hence SVDA_SUPDERCALC must be set to zero in ',
     +      'super parameter PEST control file.')
            go to 9890
        end if


        ifail=0
        istart_c=1
        if(maxcompdim.le.1)then
          x=0.0d0                  ! an array
        else
          if(ncompdim.gt.0)then
            do i8=1,ncompdim
              xc(i8)=0.0d0
            end do
          end if
        end if

        if(nlscaladj.ne.0)then
          ies=0
          do i=1,bnpar
            if((bitrans(i).ge.0).or.
     +         (bitrans(i).le.-1000001))then
              ies=ies+1
              iess=colindex(ies)
              if(iess.ne.0)work(iess)=sf(i)
            end if
          end do
        end if

        istart_c=1
        read(isdu,err=9200,end=9200)icount
        do i=1,icount
          read(isdu,err=9200,end=9200) j,dtemp
          ies=(j-1)/bn+1
          irow=j-(ies-1)*bn
          iess=colindex(ies)
          if(iess.ne.0)then
            if(nlscaladj.ne.0) dtemp=dtemp*work(iess)
            if(maxcompdim.le.1)then
              do ipp=1,npar
                x(irow,ipp)=x(irow,ipp)+dtemp*tbx(ipp,iess)
              end do
            else
              if(abs(dtemp).gt.0.0d0)then
                do ipp=1,npar
                  rtemp=0.0d0
                  if(ncompdim.ne.0)
     +            call get_value(ncompdim,xc,ixc,rtemp,irow,ipp)
                  rtemp=rtemp+dtemp*tbx(ipp,iess)
                  call store_value(jfail,ncompdim,xc,ixc,rtemp,irow,ipp)
                  if(jfail.ne.0) go to 9000
                  istart_c=ifound_c
                end do
              end if
            end if
          end if
        end do
        close(unit=isdu)

        go to 9900

9000    ifail=1
        return

9200    call addquote(infile,qfile1)
        call addquote(basepestfile,qfile2)
        write(errmsg,9210) trim(qfile1),trim(qfile2)
9210    format('Error reading unformatted base Jacobian matrix file ',
     +  a,'or file is incompatible with base PEST control file ',
     +  a,'.')
        go to 9890

9890    ifail=2
        return

9900    continue
        return

#ifdef PESTMOD
        end subroutine base_supdercalc
#else
        end
#endif




        subroutine svda_deallocate

C -- Subroutine SVDA_DEALLOCATE deallocates arrays in the SVD_PARM_DATA module.

          use svd_parm_data
          use pestdata, only: svda_extsuper
          implicit none

          integer ierr

          if(allocated(bitrans))deallocate(bitrans,stat=ierr)
          if(allocated(useobsgp))deallocate(useobsgp,stat=ierr)
          if(allocated(bpval))deallocate(bpval,stat=ierr)
          if(allocated(blbound))deallocate(blbound,stat=ierr)
          if(allocated(bubound))deallocate(bubound,stat=ierr)
          if(allocated(sval))deallocate(sval,stat=ierr)
          if(allocated(svalue))deallocate(svalue,stat=ierr)
          if(allocated(bscale))deallocate(bscale,stat=ierr)
          if(allocated(boffset))deallocate(boffset,stat=ierr)
          if(allocated(bowght))deallocate(bowght,stat=ierr)
          if(allocated(sss))deallocate(sss,stat=ierr)
          if(allocated(work))deallocate(work,stat=ierr)
          if(allocated(bapar))deallocate(bapar,stat=ierr)
          if(allocated(bobgnme))deallocate(bobgnme,stat=ierr)
          if(allocated(btpfle))deallocate(btpfle,stat=ierr)
          if(allocated(binfle))deallocate(binfle,stat=ierr)

          if(allocated(bpargpnme))deallocate(bpargpnme,stat=ierr)
          if(allocated(jindex))deallocate(jindex,stat=ierr)
          if(allocated(senfactor))deallocate(senfactor,stat=ierr)
          if(allocated(sf))deallocate(sf,stat=ierr)
          if(allocated(bipargp))deallocate(bipargp,stat=ierr)
          if(allocated(bgtrans))deallocate(bgtrans,stat=ierr)
          if(allocated(imem))deallocate(imem,stat=ierr)
          if(allocated(avsen))deallocate(avsen,stat=ierr)
          if(allocated(tbx))deallocate(tbx,stat=ierr)
          if(allocated(rowindex))deallocate(rowindex,stat=ierr)
          if(allocated(colindex))deallocate(colindex,stat=ierr)
          if(abs(svda_extsuper).eq.0)then
            if(allocated(work)) deallocate(work,stat=ierr)
            if(allocated(sss)) deallocate(sss,stat=ierr)
          end if
          if(abs(svda_extsuper).eq.3)then
            if(allocated(xtx)) deallocate(xtx,stat=ierr)
          end if
          if(abs(svda_extsuper).eq.2)then
            if(allocated(eigmat)) deallocate(eigmat,stat=ierr)
            if(allocated(uu)) deallocate(uu,stat=ierr)
            if(allocated(vv)) deallocate(vv,stat=ierr)
            if(allocated(ww)) deallocate(ww,stat=ierr)
            if(allocated(xx)) deallocate(xx,stat=ierr)
            if(allocated(bb)) deallocate(bb,stat=ierr)
            if(allocated(work)) deallocate(work,stat=ierr)
            if(svda_extsuper.eq.-2)then
              if(allocated(rr)) deallocate(rr,stat=ierr)
              if(allocated(eigmat1)) deallocate(eigmat1,stat=ierr)
            end if
          end if
          return

#ifdef PESTMOD
        end SUBROUTINE svda_deallocate
#else
        end
#endif


        SUBROUTINE RESDAT(IFAIL,JUNIT,NESPAR,NXROW,NOBSGP,IREG,SVDMODE,
     +  SVDA,NUMLAM,IMCOVFLAG,ICOUNTREGOBS,ICOUNTREGNZ,RLAMBDA1,LAMLST,
     +  ALPHLST,NUMEIGLST,PHIM,PHIR,NOBGNM,IRGP,OWGHT,CLINE,ERRMSG,
     +  NLSCALADJ,SVDA_EXTSUPER,LSQRMODE)

C -- Subroutine RESMAT stores information from which the resolution matrix can be built.

        USE SVD_PARM_DATA
#ifdef INTEL
        USE COMMON_MOD, ONLY: FLENME,CASEFL
#endif

        IMPLICIT NONE

C -- Declaration of variables occurring in subroutine argument list.

        INTEGER IFAIL,JUNIT,NESPAR,NXROW,NOBSGP,IREG,SVDMODE,SVDA,
     +  NUMLAM,IMCOVFLAG,ICOUNTREGOBS,ICOUNTREGNZ,NUMEIGLST,NLSCALADJ,
     +  SVDA_EXTSUPER,LSQRMODE
        INTEGER NOBGNM(NXROW),IRGP(NOBSGP)
        DOUBLE PRECISION RLAMBDA1,LAMLST,PHIM,PHIR,ALPHLST
        DOUBLE PRECISION OWGHT(NXROW)
        CHARACTER*(*) CLINE,ERRMSG

C -- Declaration of variables appearing in common blocks.

#ifndef INTEL
        CHARACTER*100 CASEFL
        CHARACTER*200 FLENME
#endif

C -- Declaration of other variables.

        INTEGER JFAIL,I,IUNIT

#ifndef INTEL
        COMMON /CASE/CASEFL
        COMMON /FLENME/FLENME
#endif


C -- First the resolution matrix data file is opened.

        IUNIT=JUNIT
        IFAIL=0
        FLENME=TRIM(CASEFL)//'.rsd'
        CALL FFOPEN(JFAIL,-IUNIT,'w',' ',187,CLINE)
        IF(JFAIL.NE.0) GO TO 9891

        WRITE(IUNIT,ERR=100) NESPAR,NXROW,NOBSGP
        WRITE(IUNIT,ERR=100) ICOUNTREGOBS,ICOUNTREGNZ
        WRITE(IUNIT,ERR=100) IREG,SVDMODE,SVDA,LSQRMODE
        WRITE(IUNIT,ERR=100) NUMLAM,RLAMBDA1,LAMLST,ALPHLST,NUMEIGLST
        WRITE(IUNIT,ERR=100) IMCOVFLAG
        WRITE(IUNIT,ERR=100) PHIM,PHIR

! -- The rotated weight matrix is stored.

        WRITE(IUNIT,ERR=100) (OWGHT(I),I=1,NXROW)

! -- The observation group indicator matrix is stored.

        WRITE(IUNIT,ERR=100) (NOBGNM(I),I=1,NXROW)

! -- The observation group regularisation indicator array is stored.

        WRITE(IUNIT,ERR=100) (IRGP(I),I=1,NOBSGP)

C -- Further data is stored if SVD-assist functionality was activated.

        IF(SVDA.NE.0)THEN
          WRITE(IUNIT,ERR=100) BNPAR,BNESPAR,BNXROW,NLSCALADJ,
     +    SVDA_EXTSUPER
          WRITE(IUNIT,ERR=100) (BITRANS(I),I=1,BNPAR)
          WRITE(IUNIT,ERR=100) (BOWGHT(I),I=1,BNXROW)
          WRITE(IUNIT,ERR=100) (SF(I),I=1,BNPAR)
          DO I=1,BNPAR
            WRITE(IUNIT,ERR=100) BAPAR(I)
          END DO
          WRITE(IUNIT,ERR=100) TEMPBASEJACFILE
        END IF

        CLOSE(UNIT=IUNIT)
        RETURN


100     CONTINUE
        WRITE(ERRMSG,110) TRIM(FLENME)
110     FORMAT('Cannot write to unformatted file ',a,'.')
        IFAIL=2
        RETURN

9891    IFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE RESDAT
#else
        END
#endif



       subroutine open_jacobian(ierr,isdu,basejacfile)

       implicit none

       integer ierr,isdu
       character*(*) basejacfile

       ierr=0
#ifdef LAHEY
#ifdef LF90
       open(unit=isdu,file=basejacfile,form='unformatted',
     + access='transparent',status='old',iostat=ierr)
#else
       open(unit=isdu,file=basejacfile,form='binary',
     + status='old',iostat=ierr)
#endif
#else
       open(unit=isdu,file=basejacfile,form='unformatted',
     + access='stream',status='old',iostat=ierr)
#endif

       return
       end



       subroutine process_base_par_deriv(ifail)

       use pestdata, only: extderfle,cline,errmsg,basepestfile,
     +                     basejacfile,inst,irsf
       use svd_parm_data

       implicit none
       integer          lw(4),rw(4),nnpar,nnobs,icompress,jcount,
     +                  jjcount,ierr,ifail,ipp,i,irow,ies,icount,
     +                  isdu,itemp1,itemp2,bn,j
       double precision rtemp,logfac,rrtemp,dtemp
       character*20  atemp
       character*200 qfile1,qfile2

       ifail=0
       logfac=log(10.0)
       open(unit=inst,file=extderfle,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,1010) trim(extderfle)
1010     format('Cannot open file ',a,' to read model-generated ',
     +   'base parameter derivatives.')
         go to 9890
       end if
       read(inst,'(a)',err=9300,end=9300) cline
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9300
       call numrd(ifail,0,lw(1),rw(1),nnpar,rtemp,cline)
       if(ifail.ne.0) go to 9300
       call numrd(ifail,0,lw(2),rw(2),nnobs,rtemp,cline)
       if(ifail.ne.0) go to 9300
       if((nnpar.ne.bnpar).or.(nnobs.ne.bnobs))then
         write(errmsg,1020) trim(extderfle),trim(basepestfile)
1020     format('Number of base parameters and/or observations ',
     +   'cited in external derivatives file ',a,
     +  ' is not the same as the number of ',
     +   'base parameters and observations cited in file ',a,'.')
         go to 9890
       end if
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0)then
         icompress=0
       else
         call numrd(ifail,0,lw(3),rw(3),icompress,rtemp,cline)
         if(ifail.ne.0) then
           icompress=0
         else
           if(icompress.lt.0) then
             go to 9300
           else if(icompress.gt.0)then
             write(errmsg,1021) trim(extderfle)
1021         format('Compressed storage not allowed in external ',
     +       'base derivatives file ',a,'.')
             go to 9890
           end if
         end if
       end if

C -- We now open the temporary base Jacobian file.

       call ffopen(ifail,-irsf,'w',tempbasejacfile,25,cline)
       if(ifail.ne.0)then
         write(errmsg,1022) trim(tempbasejacfile)
1022     format('Cannot open temporary base Jacobian file ',a,
     +   ' for writing.')
         go to 9890
       end if
       write(irsf) -bnespar,-bnxrow
       icount=bnespar*bnobs+bnpriorcount
       write(irsf) icount

C -- To save time in later processing we list parameters which are parent parameters using
C    a spare array.

        do ipp=1,bnpar
          work(ipp)=0.0d0
          if(bitrans(ipp).ge.0)then
            do i=1,bnpar
              if(-bitrans(i).eq.ipp)then
                work(ipp)=work(ipp)+1.0d0
              end if
            end do
          end if
        end do

C -- Now we read the external derivatives file.

        do 1100 irow=1,bnobs
          read(inst,*,err=9400,end=9500) (sss(ipp),ipp=1,bnpar)
          ies=0
          do 1080 ipp=1,bnpar
            if((bitrans(ipp).lt.0).and.(bitrans(ipp).gt.-1000001))
     +      go to 1080
            ies=ies+1
            rtemp=sss(ipp)
            if((rtemp.lt.-1.1d33).and.(rtemp.gt.-1.2d33))then
              call writint(atemp,irow)
              write(errmsg,1045) trim(bapar(ipp)),trim(atemp),
     +        trim(extderfle)
1045          format('Derivative wrt parameter "',a,'" for ',
     +        'observation number ',a,' is out of range in file ',
     +        a,'.')
              go to 9890
            end if
            rtemp=rtemp*bscale(ipp)
            if(work(ipp).gt.0.0d0)then
              jcount=nint(work(ipp))
              jjcount=0
              do 1070 i=1,bnpar
                if(-bitrans(i).eq.ipp)then
                  jjcount=jjcount+1
                  rrtemp=sss(i)
                  if((rrtemp.lt.-1.1d33).and.(rrtemp.gt.-1.2d33))
     +              then
                    call writint(atemp,irow)
                    write(errmsg,1045) trim(bapar(i)),trim(atemp),
     +              trim(extderfle)
                    go to 9890
                  end if
                  rrtemp=rrtemp*bscale(i)
                  rtemp=rtemp+rrtemp*sval(i)/sval(ipp)   ! Chechk this.
                  if(jjcount.eq.jcount) go to 1071
                end if
1070          continue
1071          continue
            end if
            icount=(ies-1)*bnxrow+irow
            if((bitrans(ipp).eq.0).or.(bitrans(ipp).eq.-1000001)) then
              write(irsf) icount,rtemp
            else if((bitrans(ipp).eq.1).or.(bitrans(ipp).eq.-1000002))
     +      then
              write(irsf) icount,sval(ipp)*rtemp*logfac      !check this (i.e. use of sval)
            end if
1080      continue
1100    continue
        close(unit=inst)

C -- We now read prior information derivatives from the old Jacobian file and transfer
C    them to the temporary Jacobian file.

       call open_jacobian(ierr,isdu,basejacfile)
       if(ierr.ne.0)then
         call addquote(basejacfile,qfile1)
         write(errmsg,90) trim(qfile1)
90       format('Cannot open unformatted Jacobian matrix file ',a,'.')
         go to 9890
       end if
       read(isdu,err=9100,end=9100)itemp1,itemp2
       if(abs(itemp1).ne.bnespar) go to 9200
       bn=abs(itemp2)
       if(itemp1.ge.0)then
         call addquote(basejacfile,qfile1)
         write(errmsg,91) trim(qfile1)
91       format('Base Jacobian matrix file ',a,' uses old format; ',
     +   'use JCOTRANS utility to translate it to new format.')
         go to 9890
       end if
       if((bn.ne.bnxrow).and.(bn.ne.bnobs))then
         call addquote(basejacfile,qfile1)
         call addquote(basepestfile,qfile2)
         write(errmsg,92) trim(qfile2),trim(qfile1)
92       format('Base PEST control file ',a,' and base Jacobian ',
     +   'matrix file ',a,' cite different numbers of observations.')
         go to 9890
       end if
       read(isdu,err=9100,end=9100)icount
       do i=1,icount
         read(isdu,err=9100,end=9100) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         if(irow.gt.bnobs) write(irsf) j,dtemp
       end do
       close(unit=isdu)
       close(unit=irsf)

C -- The new derivatives flag is updated

       newbaseder=newbaseder+1

       return

9100   call addquote(basejacfile,qfile1)
9110   format('Error reading unformatted base Jacobian matrix file ',
     + a,'.')
       go to 9890
9200   call addquote(basejacfile,qfile1)
       call addquote(basepestfile,qfile2)
       write(errmsg,9210) trim(qfile1),trim(qfile2)
9210   format('Error reading unformatted base Jacobian matrix file ',a,
     + '. File is incompatible with base PEST control file ',
     + a,'.')
        go to 9890
9300    write(errmsg,9310) trim(extderfle)
9310    format('Error reading first line of base parameter ',
     +  'derivatives file ',a,'.')
        go to 9890
9400    write(errmsg,9410) trim(extderfle)
9410    format('Error encountered while reading model-calculated ',
     +  'base parameter derivatives from file ',a,'.')
        go to 9890
9500    write(errmsg,9510) trim(extderfle)
9510    format('Unexpected end encountered to model-calculated ',
     +  'base parameter derivatives file ',a,'.')
        go to 9890


9890    ifail=1
        return

#ifdef PESTMOD
        end subroutine process_base_par_deriv
#else
        end
#endif


       subroutine get_base_refobs(ifail,refobs)

C -- Subroutine GET_BASE_REFOBS reads reference observations from the base
C    parameter JCO file. This saves the need to do an initial model run.


       use pestdata, only: nxrow,npar,isdu,basejacfile,svda_supdercalc
       use svd_parm_data

       implicit none

       logical sequals
       integer ifail
       double precision refobs(nxrow)
       integer itemp
       integer ierr,itemp1,itemp2,icount,i,j,ipp,irow,bn
       double precision dtemp,dtemp1,dtemp2
       character*12 atemp12
       character*20 atemp20

       ifail=0

       if(svda_supdercalc.eq.0) go to 9890
       call open_jacobian(ierr,isdu,basejacfile)
       if(ierr.ne.0) go to 9890
       read(isdu,err=9890,end=9890)itemp1,itemp2
       bn=abs(itemp2)
       read(isdu,err=9890,end=9890)icount
       do i=1,icount
         read(isdu,err=9890,end=9890) j,dtemp
       end do
       do ipp=1,bnespar
         read(isdu,err=9890,end=9890) atemp12
       end do
       do irow=1,bn
         read(isdu,end=9890,err=9890)  atemp20
       end do
       do irow=1,bnxrow
         read(isdu,end=9890,err=9890) refobs(irow),dtemp
       end do
       do ipp=1,npar
         read(isdu,end=9890,err=9890) dtemp,itemp,dtemp1,dtemp2
         if(.not.sequals(dtemp,bpval(ipp))) go to 9890
         if(.not.sequals(dtemp1,bscale(ipp))) go to 9890
         if(.not.sequals(dtemp2,boffset(ipp))) go to 9890
         if(itemp.ne.bitrans(ipp))then
           if(itemp.eq.0)then
             if(bitrans(ipp).ne.-1000001) go to 9890
           else if(itemp.eq.1)then
             if(bitrans(ipp).ne.-1000002) go to 9890
           else
             go to 9890
           end if
         end if
       end do
       go to 9900

9890   ifail=1

9900   continue
       close(unit=isdu,iostat=ierr)

       return

#ifdef PESTMOD
        end subroutine get_base_refobs
#else
        end
#endif



       logical function sequals(r1,r2)

         double precision,  intent(in)      :: r1,r2
         real                               :: rtemp
         real                               :: sr1,sr2

         sr1=r1
         sr2=r2
         rtemp=abs(7.0*spacing(sr1))
         if(abs(sr1-sr2).lt.rtemp)then
           sequals=.true.
         else
           sequals=.false.
         end if

       end



        subroutine write_parcalc_tpl(ifail,nregadjpar,nlscaladj)

C -- Subroutine WRITE_PARCALC_TPL writes the template of the PARCALC input file.

        use pestdata, only : npar,scale,offset,pval,errmsg,apar,
     +                      isda,isdu,svda_extsuper,svda_supdercalc,
     +                      basejacfile,basepestfile
        use svd_parm_data
        implicit none
        integer          ifail,nregadjpar,nlscaladj
        integer          i,j
        character*200    qfile1,qfile2

        ifail=0
        open(unit=isda,file='parcalc.tpl',action='write',err=9100)
        write(isda,605,err=9100)
605     format('ptf $')
        write(isda,609,err=9100)
609     format('* control data')
        write(isda,*,err=9100) bnpar,npar-nregadjpar,bnstmp,
     +  nlscaladj
        if(include_prior.eq.0)then
          write(isda,*,err=9100) bntpfle
        else
          write(isda,*,err=9100) bntpfle+1
        end if
        write(isda,610,err=9100)
610     format('* PEST parameter data')
        do i=1,npar
          if(scale(i).lt.-1.0d35) cycle
          write(isda,620,err=9100) trim(apar(i)),svalue(i)
620       format('$',a23,'$  ',1pg23.16)
        end do
        write(isda,621,err=9100)
621     format('* base parameter data')
        do i=1,bnpar
          write(isda,619,err=9100) trim(bapar(i)),bitrans(i),sval(i),
     +    blbound(i),bubound(i),bscale(i),boffset(i)
619       format(1x,a,t15,i8,5(2x,1pg23.16))
c619       format(1x,a,t15,i8,1pg23.16,4(2x,1pg14.7))
        end do
        write(isda,622,err=9100)
622     format('* model input files')
        do i=1,bntpfle
          call addquote(btpfle(i),qfile1)
          call addquote(binfle(i),qfile2)
          write(isda,623,err=9100) trim(qfile1),trim(qfile2)
623       format(1x,a,2x,a)
        end do
        if(include_prior.eq.1)then
          write(isda,623,err=9100) 'picalc.tpl', 'picalc.in'
        end if
        write(isda,630,err=9100)
630     format('* eigenvector matrix')
        if(bnstmp.lt.npar-nregadjpar)then
          do i=1,bnstmp
            do j=bnstmp+1,npar-nregadjpar
              tbx(j,i)=0.0d0
            end do
          end do
        end if
        do i=1,bnstmp
          write(isda,640,err=9100) (tbx(j,i),j=1,npar-nregadjpar)
640       format(5(1x,1pg23.16))
        end do
        if(nlscaladj.ne.0)then
          write(isda,650,err=9100)
650       format('* scale factor matrix')
          write(isda,640,err=9100) (sf(i),i=1,bnpar)
        end if

        close(unit=isda,err=9100)
        return

9100    write(errmsg,9110)
9110    format('Cannot write to PARCALC template file parcalc.tpl ',
     +  'to update this file.')
        ifail = 1
        return

        end

C -- We should be able to get rid of insisting that there are no non-regularisation
C       covariance matrices now that we are dealing with Q(1/2)X rather than ZtQX.
C -- Check BPROD
C -- Check repurcussions for re-allowing full bnxrow in RESPROC.
C -- Check that LSQR works ok if number of super parameters reduced.
