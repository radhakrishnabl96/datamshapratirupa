       program SVDAPREP

C -- Program SVDAPREP builds a new PEST control file using SVD-based
C    "super parameters" from an existing one.

       implicit none

       logical       lexist,violine
       integer       nsup,i,j,npar,nobs,npargp,nprior,nobsgp,ntpfle,
     +               ninsfle,icount,nespar,itemp,k,ierr,nn,ifail,
     +               imode,icovind,ising,svdmode,maxsing,eigwrite,
     +               jfail,maxcompdim,numcom,nlscaladj,extsuper,
     +               mm,npnt,supdercalc
       integer       rmul,bmul,jmul,n
       integer       ilsqr,lsqr_itnlim,lsqrmode,lsqrwrite,ipardef,
     +               idercomflag,jacfile,iderflag
       integer       jacupdate,noptmax
       integer       nparsec,nparfile
       integer       par_excl
       integer       ii,irefobs,imodbatch,lamforgive,derforgive
       integer       lw(12), rw(12)
       integer       noptswitch
       integer       jj,irefobs_pause
       integer       isenreuse
       integer       iline
       integer       jjflag
       integer       orr_not_first,uptestlim,uptestmin
       double precision offset,rlpmx,eigthresh,rlamfac,rlambda1
       double precision lsqr_atol,lsqr_btol,lsqr_conlim
       double precision gtarg
       double precision run_slow_fac,run_abandon_fac,win_mrun_hours
       double precision rtemp,jcowarnthresh,jcozerothresh
       character*1   aa,bb,ad,am,asp,aspd
       character*2   appg
       character*2   dumfile
       character*5   amul
       character*8   atemp8,ascale,adercom
       character*12  atrans,atemp1,atemp2,aapar,atemp,pestmode,atemp3
       character*12  aline
       character*12  aversion
       character*12  par_fd
       character*13  nulstring
       character*20  atempx
       character*20  vartext,upvartext
       character*25  atemp25
       character*200 pestfile,outfile,modbatch,afile,qfile,superfile,
     + model_command,aafile,comjac
       character*256 oldfile,newfile
       character*300 termline,covline
       character*300 outline
       character*500 errmsg
       character*2000 cline

#ifdef SYS_FUNCTION
       integer nnn
       integer system
#endif

       integer, allocatable       :: itrans(:)
       character*12,allocatable   :: apar(:)
       character*20,allocatable   :: aprior(:)
       character*200, allocatable :: tplfle(:),infle(:)

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif
       ilsqr=0
       ising=0
       extsuper=0
       comjac=' '
       idercomflag=0
       par_fd=' '
       jacfile=0
       iderflag=0
       par_excl=0
       lamforgive=0
       derforgive=0

C -- User input is acquired from the terminal.

       include 'version.inc'
       write(6,50) trim(aversion)
50     format(/,' SVDAPREP Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

100    write(6,110,advance='no')
110    format(' Enter name of existing PEST control file: ')
       read(5,'(a)') pestfile
       if(pestfile.eq.' ') go to 100
       call remchar(pestfile,'"')
       call remchar(pestfile,'''')
       pestfile=adjustl(pestfile)
       i=len_trim(pestfile)
       j=i-4
       if(j.lt.1)j=1
       atemp=pestfile(j:i)
       if(index(atemp,'.').eq.0) then
         pestfile=trim(pestfile)//'.pst'
       end if
       i=len_trim(pestfile)
       atemp=pestfile(i-3:i)
       call lowcase(atemp)
       if(atemp(1:4).ne.'.pst')then
         write(6,109)
109      format(/,' PEST control file must have an extension of ',
     +   '".pst" - try again.',/)
         go to 100
       end if
       open(unit=10,file=pestfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(pestfile,qfile)
         write(6,115) trim(qfile)
115      format(/,' Cannot open file ',a,' - try again.',/)
         go to 100
       end if

101    write(6,102,advance='no')
102    format(' Use pre-defined super-parameter file? [y/n]  ',
     + '(<Enter> if "no"): ')
       read(5,'(a)') asp
       if(asp.ne.' ')then
         if(asp.eq.'Y')asp='y'
         if(asp.eq.'N')asp='n'
         if((asp.ne.'n').and.(asp.ne.'y')) go to 101
         if(asp.eq.'y')then
           extsuper=1
103        write(6,104,advance='no')
104        format(' Enter name of this file: ')
           read(5,*,err=103) superfile
#ifndef UNIX
           call lowcas(superfile)
#endif
         else
           extsuper=0
         end if
       end if
       if(extsuper.eq.0)then
127      continue
         write(6,121)
121      format(' For computation of super parameters:-')
         write(6,122)
122      format('    if SVD on Q^(1/2)X                - enter 1')
         write(6,123)
123      format('    if SVD on XtQX                    - enter 2')
         write(6,124)
124      format('    if LSQR without orthogonalisation - enter 3')
         write(6,125)
125      format('    if LSQR with orthogonalisation    - enter 4')
         write(6,126,advance='no')
126      format(' Enter your choice (<Enter> if 1): ')
         read(5,'(a)') atemp
         if(atemp.eq.' ')then
           ipardef=1
         else
           call intread(ifail,atemp,ipardef)
           if(ifail.ne.0) go to 127
           if((ipardef.ne.1).and.(ipardef.ne.2).and.(ipardef.ne.3).
     +        and.(ipardef.ne.4)) go to 127
         end if
         if(ipardef.eq.1)then
           extsuper=0
         else if(ipardef.eq.2)then
           extsuper=3
         else if(ipardef.eq.3)then
           extsuper=2
         else if(ipardef.eq.4)then
           extsuper=-2
         end if
       end if

       if(asp.ne.'y')then
         afile=pestfile(1:i-3)//'jco'
         inquire(file=afile,exist=lexist)
         if(.not.lexist)then
           call addquote(afile,qfile)
           write(errmsg,116) trim(qfile)
116        format('Cannot find Jacobian array file ',a)
           go to 9880
         end if
       end if

C -- The first part of the PEST control file is read.

       do i=1,3
         read(10,'(a)',err=9150,end=9150) cline
         if(violine(cline)) go to 9780
       end do
       call linesplit(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9150
       pestmode=cline(lw(2):rw(2))
       call lowcase(pestmode)
       if(pestmode.eq.'estimation')then
         imode=1
       else if(pestmode.eq.'prediction')then
         imode=2
       else if (pestmode(1:5).eq.'regul')then
         imode=3
       else if(pestmode(1:6).eq.'pareto')then
         imode=4
       else
         write(errmsg,111)
111      format('PEST control file has unknown mode.')
         go to 9880
       end if
       read(10,'(a)',err=9150,end=9150) cline
       if(violine(cline)) go to 9780
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.ne.0) go to 9150

       vartext='nparsec'
       upvartext='NPARSEC'
       rtemp=0.0D0             ! default value
       iline=4
       call writint(aline,iline)
       call read_mrun_var(ifail,rtemp,vartext,upvartext,
     + iline,pestfile,cline,errmsg,1,dumfile)
       if(ifail.ne.0) go to 9880
       nparsec=nint(rtemp)
       if(nparsec.lt.0)then
         write(errmsg,143) trim(upvartext),trim(aline),trim(pestfile)
143      format(a,' must be either zero or greater ',
     +   'at line ',a,' of file ',a,'.')
         go to 9880
       end if
       if(nparsec.gt.0)then
         write(errmsg,144)
144      format('SVD-assisted inversion cannot be undertaken if a ',
     +   'base parameter file cites secondary parameters and NPARSEC ',
     +   'is thus not set to zero in that file.')
         go to 9880
       end if
       vartext='nparfile'
       upvartext='NPARFILE'
       rtemp=0.0d0             ! default value
       call read_mrun_var(ifail,rtemp,vartext,upvartext,
     + iline,pestfile,cline,errmsg,1,dumfile)
       if(ifail.ne.0) go to 9880
       nparfile=nint(rtemp)
       if(nparfile.lt.0)then
         write(errmsg,143) trim(upvartext),trim(aline),trim(pestfile)
         go to 9880
       end if
       if(nparfile.gt.0)then
         write(errmsg,1441)
1441     format('SVD-assisted inversion cannot be undertaken if a ',
     +   'base parameter file cites file-parameters and NPARFILE ',
     +   'is thus not set to zero in that file.')
         go to 9880
       end if

       call intread(ifail,cline(lw(1):rw(1)),npar)
       if(ifail.ne.0) go to 9150
       call intread(ifail,cline(lw(2):rw(2)),nobs)
       if(ifail.ne.0) go to 9150
       call intread(ifail,cline(lw(3):rw(3)),npargp)
       if(ifail.ne.0) go to 9150
       call intread(ifail,cline(lw(4):rw(4)),nprior)
       if(ifail.ne.0) go to 9150
       call intread(ifail,cline(lw(5):rw(5)),nobsgp)
       if(ifail.ne.0) go to 9150
       maxcompdim=-999
       call linspl(ifail,6,lw,rw,cline)
       if(ifail.eq.0)then
         call intread(ifail,cline(lw(6):rw(6)),maxcompdim)
         if(ifail.ne.0) maxcompdim=-999
       end if
       read(10,'(a)',err=9150,end=9150) cline
       if(violine(cline)) go to 9780
       call lowcas(cline)
       irefobs=999
162    continue
       ii=index(cline,'noobsreref')
       if(ii.ne.0)then
         irefobs=0
         do jj=ii,len_trim(cline)
           if(cline(jj:jj).eq.' ') go to 162
           cline(jj:jj)=' '
         end do
       end if
164    continue
       irefobs_pause=0
       ii=index(cline,'obsreref')
       if(ii.ne.0)then
         if(irefobs.eq.0)then
           write(errmsg,161)
161        format('Contradictory values have been supplied for ',
     +     'the "obsreref" variable.')
           go to 9880
         end if
         irefobs=1
         if(cline(ii+8:ii+8).eq.'_')then
           atemp=cline(ii+9:)
           do jj=1,len_trim(atemp)
             if(atemp(jj:jj).eq.' ') then
               atemp(jj:)=' '
               exit
             end if
           end do
           if(atemp.eq.' ')then
             write(errmsg,1612)
             go to 9880
           end if
           call intread(ifail,atemp,irefobs_pause)
           if(ifail.ne.0) then
             write(errmsg,1612)
1612         format('Cannot read integer following "irefobs_" string.')
             go to 9880
           end if
           if(irefobs_pause.le.0) then
             write(errmsg,1613)
1613         format('Integer following "irefobs_" string must ',
     +       'be positive.')
             go to 9880
           end if
         end if
         do jj=ii,len_trim(cline)
           if(cline(jj:jj).eq.' ') go to 164
           cline(jj:jj)=' '
         end do
       end if
       if(irefobs.eq.999) irefobs=0
       orr_not_first=0
1621   continue
       ii=index(cline,'orr_not_first')
       if(ii.ne.0)then
         orr_not_first=1
         do jj=ii,len_trim(cline)
           if(cline(jj:jj).eq.' ') exit
           cline(jj:jj)=' '
         end do
         go to 1621
       end if
       if(orr_not_first.ne.0)then
         if(irefobs.eq.0)then
           write(errmsg,1622)
1622       format('"orr_not_first" cannot be specified ',
     +     'if observation re-referencing is ',
     +     'not switched on.')
           go to 9880
         end if
       end if
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9150
       call intread(ifail,cline(lw(1):rw(1)),ntpfle)
       if(ifail.ne.0) go to 9150
       call intread(ifail,cline(lw(2):rw(2)),ninsfle)
       if(ifail.ne.0) go to 9150
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.eq.0) then
         call intread(ifail,cline(lw(5):rw(5)),numcom)
         if(ifail.ne.0) go to 9150
         if(numcom.gt.1)then
           write(errmsg,145)
145        format('NUMCOM variable in base PEST control file ',
     +     'is greater than zero. Multiple command lines are not ',
     +     'permitted if SVD-assist functionality is activated.')
           go to 9880
         end if
       end if
       call linspl(ifail,6,lw,rw,cline)
       if(ifail.eq.0)then
         call intread(ifail,cline(lw(6):rw(6)),jacfile)
         if(ifail.ne.0) go to 9150
         if(jacfile.eq.-1)then
           write(errmsg,1451)
1451       format('As presently programmed PEST will not accommodate ',
     +     'a binary external derivatives file. Hence JACFILE ',
     +     'must not be set to -1.')
           go to 9880
         end if
         if((jacfile.ne.0).and.(jacfile.ne.1).and.(jacfile.ne.2))
     +   go to 9150
       end if
       allocate(apar(npar),itrans(npar),infle(ntpfle),
     + tplfle(ntpfle))
       if(nprior.ne.0)then
         allocate(aprior(nprior))
       end if
       read(10,'(a)',err=9150,end=9150) cline
       if(violine(cline)) go to 9780
       call lowcas(cline)
       ii=index(cline,' nolamforgive')
       if(ii.ne.0)then
         lamforgive=0
         cline(ii+1:ii+12)=' '
       end if
       ii=index(cline,' lamforgive')
       if(ii.ne.0)then
         lamforgive=1
         cline(ii+1:ii+10)=' '
       end if
       ii=index(cline,' noderforgive')
       if(ii.ne.0)then
         derforgive=0
         cline(ii+1:ii+12)=' '
       end if
       ii=index(cline,' derforgive')
       if(ii.ne.0)then
         derforgive=1
         cline(ii+1:ii+10)=' '
       end if

C -- A set of variables are read.

       iline=6
       call writint(aline,iline)
       vartext='run_slow_fac'
       upvartext='RUN_SLOW_FAC'
       run_slow_fac=-1.1d100          !default value
       call read_mrun_var(ifail,run_slow_fac,vartext,upvartext,
     + iline,pestfile,cline,errmsg,1,dumfile)
       if(ifail.ne.0) go to 9880

       vartext='run_abandon_fac'
       upvartext='RUN_ABANDON_FAC'
       run_abandon_fac=0.0d0           !default value
       call read_mrun_var(ifail,run_abandon_fac,vartext,upvartext,
     + iline,pestfile,cline,errmsg,1,dumfile)
       if(ifail.ne.0) go to 9880

       vartext='win_mrun_hours'
       upvartext='WIN_MRUN_HOURS'
       win_mrun_hours=-1.0d0            !default value
       call read_mrun_var(ifail,win_mrun_hours,vartext,upvartext,
     + iline,pestfile,cline,errmsg,1,dumfile)
       if(ifail.ne.0) go to 9880
       if(win_mrun_hours.le.0.0d0) win_mrun_hours=-1.0d0

       vartext='uptestlim'
       upvartext='UPTESTLIM'
       rtemp=-1.0d0             ! default value
       call read_mrun_var(ifail,rtemp,vartext,upvartext,
     + iline,pestfile,cline,errmsg,1,dumfile)
       if(ifail.ne.0) go to 9880
       uptestlim=nint(rtemp)

       vartext='uptestmin'
       upvartext='UPTESTMIN'
       rtemp=-1.0d0             ! default value
       call read_mrun_var(ifail,rtemp,vartext,upvartext,
     + iline,pestfile,cline,errmsg,1,dumfile)
       if(ifail.ne.0) go to 9880
       uptestmin=nint(rtemp)

       call linspl(ifail,5,lw,rw,cline)
       if(ifail.ne.0) go to 9150
       call drealread(ifail,cline(lw(1):rw(1)),rlambda1)
       if(ifail.ne.0) go to 9150
       call drealread(ifail,cline(lw(2):rw(2)),rlamfac)
       if(ifail.ne.0) go to 9150
       jacupdate=-999
       call linspl(ifail,6,lw,rw,cline)
       if(ifail.eq.0)then
         call intread(ifail,cline(lw(6):rw(6)),jacupdate)
         if(ifail.ne.0) go to 9150
       end if
       read(10,'(a)',err=9150,end=9150) cline
       if(violine(cline)) go to 9780
       read(10,'(a)',err=9150,end=9150) cline
       if(violine(cline)) go to 9780
       noptswitch=-99999999
       call linesplit(ifail,2,lw,rw,cline)
       if(ifail.eq.0)then
         call intread(ifail,cline(lw(2):rw(2)),noptswitch)
         if(ifail.ne.0) noptswitch=-99999999
       endif
       isenreuse=0
       call lowcase(cline)
       vartext='jcowarnthresh'
       upvartext='JCOWARNTHRESH'
       jcowarnthresh=0.0d0           ! default value
       call read_mrun_var(ifail,jcowarnthresh,vartext,upvartext,
     + iline,pestfile,cline,errmsg,1,dumfile)
       if(ifail.ne.0) go to 9880
       vartext='jcozerothresh'
       upvartext='JCOZEROTHRESH'
       jcozerothresh=0.0d0           ! default value
       call read_mrun_var(ifail,jcozerothresh,vartext,upvartext,
     + iline,pestfile,cline,errmsg,1,dumfile)
       if(ifail.ne.0) go to 9880
       if(index(cline,'senreuse').ne.0)then
         if(index(cline,'nosenreuse').eq.0)then
           isenreuse=1
         end if
       end if
       read(10,'(a)',err=9150,end=9150) cline
       if(violine(cline)) go to 9780
       call linesplit(ifail,6,lw,rw,cline)
       if(ifail.ne.0) go to 9150
       call intread(ifail,cline(lw(1):rw(1)),noptmax)
       if(ifail.ne.0) go to 9150
       termline=cline(rw(6)+1:)
       read(10,'(a)',err=9150,end=9150) cline
       if(violine(cline)) go to 9780
       covline=cline(1:300)
       call lowcase(covline)
       if(index(covline,'rrfsave').ne.0)then
         if(index(covline,'norrfsave').eq.0)then
           write(errmsg,149)
149        format('The string "rrfsave" cannot appear in the ',
     +     '"control data" section of the PEST control file. PEST_HP ',
     +     'cannot write a run results file using base parameters ',
     +     'when implementing SVD-assisted inversion and hence ',
     +     'estimating super parameters.')
           go to 9880
         end if
       end if

C -- More information is acquired from the user.

150    write(6,160,advance='no')
160    format(' Enter number of super parameters to estimate: ')
       read(5,*,err=150) nsup
       if(nsup.le.0) go to 150
200    write(6,210,advance='no')
210    format(' Enter name for new super pest control file: ')
       read(5,'(a)') outfile
       if(outfile.eq.' ') go to 200
       call remchar(outfile,'"')
       call remchar(outfile,'''')
       i=len_trim(outfile)
       j=i-3
       if(j.lt.1)j=1
       atemp=outfile(j:i)
       call lowcase(atemp)
       if(atemp.ne.'.pst')then
         if(index(atemp,'.').eq.0)then
           outfile=trim(outfile)//'.pst'
         else
           write(6,209)
209        format(/,' PEST control file must have an extension ',
     +     'of ".pst" - try again.',/)
           go to 200
         end if
       end if
       outfile=adjustl(outfile)
       open(unit=20,file=outfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(outfile,qfile)
         write(6,211) trim(qfile)
211      format(/,' Cannot open file ',a,' for output - try again.',/)
         go to 200
       end if
230    write(6,240,advance='no')
240    format(' Enter offset for super parameters  ',
     + '(<Enter> if 10): ')
       read(5,'(a)') atempx
       if(atempx.eq.' ')then
         offset=10.0d0
       else
         call drealread(ifail,atempx,offset)
         if(ifail.ne.0) go to 230
         if(offset.le.0.0)then
           write(6,241)
241        format(/,' Offset must be positive - try again.',/)
           go to 230
         end if
       end if
       aa='n'
       if(nprior.gt.0)then
         aa='y'
c260      write(6,270,advance='no')
c270      format(' Preserve prior information?  [y/n]: ')
c         read(5,'(a)') aa
c         if(aa.eq.'Y')aa='y'
c         if(aa.eq.'N')aa='n'
c         if((aa.ne.'y').and.(aa.ne.'n')) go to 260
       end if
       if(imode.eq.2)then
         bb='p'
       else if(imode.eq.4)then
         bb='a'
       else
         bb='e'
         if(imode.eq.3)then
           bb='r'
c272        write(6,271,advance='no')
c271        format(' Run in estimation or regularisation mode? ',
c     +     '[e/r]: ')
c           read(5,'(a)') bb
c           if(bb.eq.'E')bb='e'
c           if(bb.eq.'R')bb='r'
c           if((bb.ne.'e').and.(bb.ne.'r')) go to 272
         end if
       end if
274    write(6,273,advance='no')
273    format(' Enter value for RELPARMAX (<Enter> if 0.1): ')
       read(5,'(a)') atempx
       if(atempx.eq.' ')then
         rlpmx=0.1d0
       else
         call drealread(ifail,atempx,rlpmx)
         if(ifail.ne.0) go to 274
         if(rlpmx.le.0.0d0)then
           write(6,278)
278        format(/,' Must be greater than zero - try again.',/)
           go to 274
         end if
       end if

264    continue
       amul=' '
       bmul=0
       jmul=0
       rmul=0
281    write(6,282,advance='no')
282    format(' Write multiple BPA, JCO, REI, none [b/j/r/n] files ',
     + '(<Enter> if "n")? ')
       read(5,'(a)') amul
       call lowcase(amul)
       amul=adjustl(amul)
       if(index(amul,'n').ne.0)then
         if(len_trim(amul).ne.1) go to 264
       else if(index(amul,'y').ne.0)then
         if(len_trim(amul).ne.1) go to 264
         bmul=1
       else
         n=index(amul,'b')
         if(n.ne.0)then
           bmul=1
           amul(n:n)=' '
         end if
         n=index(amul,'j')
         if(n.ne.0)then
           jmul=1
           amul(n:n)=' '
         end if
         n=index(amul,'r')
         if(n.ne.0)then
           rmul=1
           amul(n:n)=' '
         end if
         if(amul.ne.' ') go to 264
       end if
       if(imode.eq.4) bmul=1

       if(extsuper.ne.1)then
283      write(6,284,advance='no')
284      format(' Parameter scale adjustment [SVDA_SCALADJ] setting',
     +   ' (<Enter> if 2): ')
         read(5,'(a)') ascale
         if(ascale.eq.' ')then
           nlscaladj=2
         else
           call intread(ifail,ascale,nlscaladj)
           if(ifail.ne.0) go to 283
           if(abs(nlscaladj).gt.4)then
             write(6,285)
285          format(' *** must be -4, -3, -2, -1, 0, 1, 2, 3 or 4 - ',
     +       'try again ***')
             go to 283
           end if
         end if
       else
         nlscaladj=0
       end if

       ad='s'
       if(imode.eq.4)then
         ad='2'
       end if
c275    write(6,276,advance='no')
c276    format(' Use 2 pt, 3 pt or switch for derivatives?  ',
c     + '[2/3/s]: ')
c       read(5,'(a)') ad
c       if(ad.eq.'S')ad='s'
c       if((ad.ne.'2').and.(ad.ne.'3').and.(ad.ne.'s'))go to 275
       am='v'
277    write(6,279,advance='no')
#ifdef UNIX
279    format(' Make new model script file silent or ',
     + 'verbose?  [s/v] (<Enter> if "s") : ')
#else
279    format(' Make new model batch file silent or ',
     + 'verbose?  [s/v] (<Enter> if "s") : ')
#endif
       read(5,'(a)') am
       if(am.eq.' ')then
         am='s'
       else
         if(am.eq.'S')am='s'
         if(am.eq.'V')am='v'
         if((am.ne.'s').and.(am.ne.'v')) go to 277
       end if
       if(am.eq.'v')then
         nulstring=' '
       else
#ifdef UNIX
         nulstring=' > /dev/null'
#else
         nulstring=' > nul'
#endif
       end if

       aspd='n'
       if(jacfile.ne.0)then
         aspd='y'
       else
         if(extsuper.ne.1)then
287        write(6,288,advance='no')
288        format(' Automatic calc. of 1st itn. super param. derivs? ',
     +     ' [y/n] (<Enter> if "y") : ')
           read(5,'(a)') aspd
           if(aspd.eq.' ')then
             aspd='y'
           else
             if(aspd.eq.'Y') aspd='y'
             if(aspd.eq.'N') aspd='n'
             if((aspd.ne.'y').and.(aspd.ne.'n')) go to 287
           end if
         end if
       end if

       if(imode.eq.4)then
         if(extsuper.ne.1)then
2882       write(6,2881,advance='no')
2881       format(' Provide setting for SVDA_PAR_EXCL ',
     +     '[0, 1, or -1] (<Enter> if 0) :')
           read(5,'(a)') appg
           appg=adjustl(appg)
           if((appg.eq.' ').or.(appg.eq.'0 '))then
             par_excl=0
           else if(appg.eq.'1 ')then
             par_excl=1
           else if(appg.eq.'-1')then
             par_excl=-1
           else
             go to 2882
           end if
         end if
       end if

C -- Parameter data is obtained from the existing PEST control file.

       do
         read(10,'(a)',err=9150,end=9150) cline
!         if(violine(cline)) go to 9780
         call lowcase(cline)
         if(index(cline,'* svd assist').ne.0)then
             write(errmsg,309)
309          format('Input PEST control file already instructs PEST ',
     +       'to use SVD-assist.')
             go to 9880
         end if
         if(index(cline,'* parameter data').ne.0) go to 310
         if(index(cline,'* singular').ne.0)then
           ising=1
           read(10,'(a)',err=9150,end=9150) cline
           if(violine(cline)) go to 9780
           call linesplit(ifail,1,lw,rw,cline)
           if(ifail.ne.0) go to 9150
           call intread(ifail,cline(lw(1):rw(1)),svdmode)
           if(ifail.ne.0) go to 9150
           read(10,'(a)',err=9150,end=9150) cline
           if(violine(cline)) go to 9780
           call linesplit(ifail,2,lw,rw,cline)
           if(ifail.ne.0) go to 9150
           call intread(ifail,cline(lw(1):rw(1)),maxsing)
           if(ifail.ne.0) go to 9150
           call drealread(ifail,cline(lw(2):rw(2)),eigthresh)
           if(ifail.ne.0) go to 9150
           read(10,'(a)',err=9150,end=9150) cline
           if(violine(cline)) go to 9780
           call linesplit(ifail,1,lw,rw,cline)
           if(ifail.ne.0) go to 9150
           call intread(ifail,cline(lw(1):rw(1)),eigwrite)
           if(ifail.ne.0) go to 9150
         end if
         if(index(cline,'* lsqr').ne.0)then
           ilsqr=1
           read(10,'(a)',err=9150,end=9150) cline
           if(violine(cline)) go to 9780
           call linesplit(ifail,1,lw,rw,cline)
           if(ifail.ne.0) go to 9150
           call intread(ifail,cline(lw(1):rw(1)),lsqrmode)
           if(ifail.ne.0) go to 9150
           read(10,'(a)',err=9150,end=9150) cline
           if(violine(cline)) go to 9780
           call linesplit(ifail,4,lw,rw,cline)
           if(ifail.ne.0) go to 9150
           call drealread(ifail,cline(lw(1):rw(1)),lsqr_atol)
           if(ifail.ne.0) go to 9150
           call drealread(ifail,cline(lw(2):rw(2)),lsqr_btol)
           if(ifail.ne.0) go to 9150
           call drealread(ifail,cline(lw(3):rw(3)),lsqr_conlim)
           if(ifail.ne.0) go to 9150
           call intread(ifail,cline(lw(4):rw(4)),lsqr_itnlim)
           if(ifail.ne.0) go to 9150
           read(10,'(a)',err=9150,end=9150) cline
           if(violine(cline)) go to 9780
           call linesplit(ifail,1,lw,rw,cline)
           if(ifail.ne.0) go to 9150
           call intread(ifail,cline(lw(1):rw(1)),lsqrwrite)
           if(ifail.ne.0) go to 9150
         end if
       end do
310    continue
       do i=1,npar
         read(10,'(a)',err=9150,end=9150) cline
         if(violine(cline)) go to 9780
         if(cline.eq.' ') go to 9150
         call linesplit(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9150
         apar(i)=cline(lw(1):rw(1))
         atrans=cline(lw(2):rw(2))
         call lowcase(apar(i))
         call lowcase(atrans)
         if(idercomflag.eq.0)then
           call linesplit(ifail,10,lw,rw,cline)
           if(ifail.eq.0)then
             call intread(ifail,cline(lw(10):rw(10)),itemp)
             if(ifail.ne.0) go to 9150
             if(itemp.ne.0) then
               idercomflag=1
               par_fd=apar(i)
             end if
           end if
         end if
         if(apar(i)(1:3).eq.'iw_')then
           write(errmsg,311)
311        format('SVDAPREP cannot accomodate adaptive ',
     +     'regularisation parameters; the names of these parameters ',
     +     'begin with "iw_". If these are not adaptive ',
     +     'regularisation parameters alter their names. If they are ',
     +     'adaptive regularisation parameters, add this ',
     +     'functionality yourself to the control file written ',
     +     'by SVDAPREP.')
           go to 9880
         end if
         if(atrans.eq.'log')then
           itrans(i)=1
         else if(atrans.eq.'none')then
           itrans(i)=0
         else if(atrans.eq.'fixed')then
           itrans(i)=-1000000
         else if(atrans.eq.'tied')then
           itrans(i)=-1
         else
           go to 9150
         end if
       end do
       icount=0
       do i=1,npar
         if(itrans(i).eq.-1)icount=icount+1
       end do
       if(icount.ne.0)then
         do i=1,icount
           read(10,'(a)',err=9150,end=9150) cline
           if(violine(cline)) go to 9780
           call linesplit(ifail,2,lw,rw,cline)
           if(ifail.ne.0) go to 9150
           atemp1=cline(lw(1):rw(1))
           atemp2=cline(lw(2):rw(2))
           call lowcase(atemp1)
           call lowcase(atemp2)
           do k=1,npar
             if(atemp1.eq.apar(k)) go to 330
           end do
           write(errmsg,315) trim(atemp1),trim(atemp2)
315        format('Unexpected tied parameter "',a,'" to parent ',
     +     'parameter "',a,'" - check PEST control file with ',
     +     'PESTCHEK.')
           go to 9880
330        continue
           do j=1,npar
             if(atemp2.eq.apar(j)) go to 350
           end do
           write(errmsg,316) trim(atemp2),trim(atemp1)
316        format('Unexpected parent parameter "',a,'" to tied ',
     +     'parameter "',a,'" - check PEST control file with ',
     +     'PESTCHEK.')
           go to 9880
350        continue
           if(itrans(j).lt.0)then
             write(errmsg,314) trim(atemp2),trim(atemp1)
314          format('Parent parameter "',a,'" to tied parameter "',
     +       a,'" must not be tied or fixed.')
             go to 9880
           end if
           itrans(k)=-j
         end do
       end if
       nespar=0
       do i=1,npar
         if(itrans(i).ge.0) nespar=nespar+1
       end do
       if(nsup.gt.nespar)then
         call writint(atemp,nespar)
         call addquote(pestfile,qfile)
         write(errmsg,360) trim(qfile),trim(atemp)
360      format('Number of adjustable parameters in PEST control ',
     +   'file ',a,' is ',a,'; number of requested super parameters ',
     +   'must not be greater than this.')
         go to 9880
       end if

C -- The first part of the new pest control file is written.

       write(20,380,err=9200)
380    format('pcf')
       write(20,390,err=9200)
390    format('* control data')
       if(bb.eq.'e')then
         write(20,395,err=9200)
395      format('restart estimation')
       else if(bb.eq.'p')then
         write(20,294,err=9200)
294      format('restart prediction')
       else if(bb.eq.'a')then
         write(20,296,err=9200)
296      format('restart pareto')
       else
         write(20,396,err=9200)
396      format('restart regularisation')
       end if
       if(maxcompdim.eq.-999)then
         atemp8=' '
       else
         write(atemp8,'(i8)') maxcompdim
       end if
       if(aa.eq.'n')then
         write(20,400,err=9200) nsup,nobs,1,0,nobsgp,trim(atemp8)
       else
         write(20,400,err=9200) nsup,nobs+nprior,1,0,nobsgp,
     +   trim(atemp8)
       end if
400    format(5i7,2x,a)
       itemp=ninsfle
       if(aa.eq.'y')itemp=ninsfle+1
       if(irefobs.eq.0)then
         write(20,410,err=9200) 1,itemp,jacfile
410      format(2i5,' double point  1',i5,'  0')
       else
         if(irefobs_pause.eq.0)then
           write(cline,411,err=9200) 1,itemp,jacfile
411        format(2i5,' double point  1',i5,'  0  obsreref')
         else
           call writint(atemp,irefobs_pause)
           write(cline,4111,err=9200) 1,itemp,jacfile,trim(atemp)
4111       format(2i5,' double point  1',i5,'  0  obsreref_',a)
         end if
         if(orr_not_first.ne.0) cline=trim(cline)//' orr_not_first'
         write(20,'(a)') trim(cline)
       end if
       if(jacupdate.eq.-999)then
         write(outline,420,err=9200) rlambda1,rlamfac
420      format(1x,1pg12.5,2x,1pg12.5,'  0.3  0.03  10')
       else
         write(outline,421,err=9200) rlambda1,rlamfac,jacupdate
421      format(1x,1pg12.5,2x,1pg12.5,'  0.3  0.03  10  ',i6)
       end if
       if(run_slow_fac.gt.0.0d0)then
         write(atemp25,'(1pg13.6)') run_slow_fac
         atemp25=adjustl(atemp25)
         outline=trim(outline)//' run_slow_fac='//trim(atemp25)
       end if
       if(run_abandon_fac.gt.0.0d0)then
         write(atemp25,'(1pg13.6)') run_abandon_fac
         atemp25=adjustl(atemp25)
         outline=trim(outline)//' run_abandon_fac='//trim(atemp25)
       end if
       if(win_mrun_hours.gt.0.0d0)then
         write(atemp25,'(1pg13.6)') win_mrun_hours
         atemp25=adjustl(atemp25)
         outline=trim(outline)//' win_mrun_hours='//trim(atemp25)
       end if
       if(uptestlim.gt.0)then
         write(atemp25,'(i10)') uptestlim
         atemp25=adjustl(atemp25)
         outline=trim(outline)//' uptestlim='//trim(atemp25)
       end if
       if(uptestmin.gt.0)then
         write(atemp25,'(i10)') uptestmin
         atemp25=adjustl(atemp25)
         outline=trim(outline)//' uptestmin='//trim(atemp25)
       end if
       if(lamforgive.eq.1)then
         outline=trim(outline)//'  lamforgive'
       end if
       if(derforgive.eq.1)then
         outline=trim(outline)//'  derforgive'
       end if

       write(20,'(a)',err=9200) trim(outline)
       write(20,430,err=9200) rlpmx
430    format(1x,f10.3,'  10.0  0.001')

       if(noptswitch.le.0)then
         outline=' 0.1'
       else
         call writint(atemp25,noptswitch)
         outline=' 0.1  '//trim(atemp25)
       end if
       if(jcowarnthresh.gt.0.0d0)then
         write(atemp25,'(1pg13.6)') jcowarnthresh
         atemp25=adjustl(atemp25)
         outline=trim(outline)//'  jcowarnthresh='//trim(atemp25)
       end if
       if(jcozerothresh.gt.0.0d0)then
         write(atemp25,'(1pg13.6)') jcozerothresh
         atemp25=adjustl(atemp25)
         outline=trim(outline)//'  jcozerothresh='//trim(atemp25)
       end if
       write(20,'(a)',err=9200) trim(outline)

       if(noptmax.le.0) noptmax=50
       if(termline.eq.' ')then
         write(20,450,err=9200) noptmax
450      format(i6,'  0.005  4  4  0.005  4')
       else
         write(20,451,err=9200) noptmax,trim(termline)
451      format(i6,'  0.005  4  4  0.005  4   ',a)
       end if
       covline='1  1  1  1'
       if(jmul.eq.1) then
         covline=trim(covline)//' jcosaveitn'
       else
         covline=trim(covline)//' nojcosaveitn'
       end if
       if(rmul.eq.1) then
         covline=trim(covline)//' reisaveitn'
       else
         covline=trim(covline)//' noreisaveitn'
       end if
       write(20,'(a)') trim(covline)
       if(ising.ne.0)then
         write(20,461)
461      format('* singular value decomposition')
         write(20,462,err=9200) svdmode
462      format(i6)
         write(20,463,err=9200) min(maxsing,nsup),eigthresh
463      format(i6,1x,1pg14.7)
         write(20,462,err=9200) eigwrite
       end if
       if(ilsqr.ne.0)then
         write(20,464,err=9200)
464      format('* lsqr')
         write(20,462,err=9200) lsqrmode
         write(20,465,err=9200) lsqr_atol,lsqr_btol,
     +   lsqr_conlim,lsqr_itnlim
465      format(3(1x,1pg14.7),2x,i6)
         write(20,462,err=9200) lsqrwrite
       end if
       write(20,481,err=9200)
481    format('* svd assist')
       call addquote(pestfile,qfile)
       write(20,'(a)',err=9200) trim(qfile)
       if(asp.ne.'y')then
         nn=len_trim(pestfile)
         afile=pestfile(1:nn-3)//'jco'
         call addquote(afile,qfile)
         write(20,'(a)',err=9200) trim(qfile)
       else
         call addquote(superfile,qfile)
         write(20,'(a)',err=9200) trim(qfile)
       end if
       if(aspd.eq.'y')then
         supdercalc=1
       else
         supdercalc=0
       end if
       write(20,484,err=9200) bmul,nlscaladj,extsuper,supdercalc,
     + par_excl
484    format(i5,i5,i5,i5,i5)
       write(20,470,err=9200)
470    format('* parameter groups')
       if(ad.eq.'2')then
         write(20,480,err=9200)
480      format(' pargp absolute    0.015  0.0  always_2  ',
     +   '2.0 parabolic')
       else if(ad.eq.'3')then
         write(20,483,err=9200)
483      format(' pargp absolute    0.015  0.0  always_3  ',
     +   '2.0 parabolic')
       else
         write(20,482,err=9200)
482      format(' pargp absolute    0.015  0.0  switch    ',
     +   '2.0 parabolic')
       end if
       write(20,490,err=9200)
490    format('* parameter data')
       do i=1,nsup
         call writint(atemp1,i)
         aapar='par'//trim(atemp1)
         if(jacfile.eq.0)then
           adercom='  1'
         else if(jacfile.eq.1)then
           adercom='  0'
         else if(jacfile.eq.2)then
           adercom='  0'
         end if
         write(20,500,err=9200) trim(aapar),offset,-offset,trim(adercom)
500      format(a,t14,'none  relative ',1pg12.5,' 1.0e-20  1.0e20  ',
     +   'pargp  1.0 ',1pg12.5,a)
       end do

C -- Observation group data is now transferred from the first PEST control
C    file to the second.

       read(10,'(a)',err=9150,end=9150) cline
       if(violine(cline)) go to 9780
       write(20,'(a)',err=9200) trim(cline)
       call lowcas(cline)
       if(index(cline,'* observation group').eq.0) go to 9150
       do i=1,nobsgp
         gtarg=-1.1d35
         read(10,'(a)',err=9150,end=9150) cline
         if(violine(cline)) go to 9780
         write(20,'(a)',err=9200) trim(cline)
         if(extsuper.ne.1)then
           call linesplit(ifail,2,lw,rw,cline)
           if(ifail.eq.0)then
             call drealread(ifail,cline(lw(2):rw(2)),gtarg)
             if(ifail.eq.0)then
               call linesplit(ifail,3,lw,rw,cline)
               if(ifail.ne.0) go to 509
             end if
             atemp3=cline(lw(1):rw(1))
             call lowcas(atemp3)
             if(imode.eq.3)then
               if(atemp3(1:5).ne.'regul')then
                 call addquote(pestfile,qfile)
                 write(errmsg,511) trim(qfile)
511              format('Base PEST control file ',a,' cites a non-',
     +           'regularisation observation group that is assigned ',
     +           'a covariance matrix. This is not allowed with ',
     +           'present SVD-assist functionality.')
                 go to 9880
               end if
             else if(imode.eq.4)then
               continue
             else
               call addquote(pestfile,qfile)
               write(errmsg,511) trim(qfile)
               go to 9880
             end if
509          continue
           end if
         end if
       end do

C -- Observation data is now transferred from the first PEST control
C    file to the second.

       do
         read(10,'(a)',err=9150,end=9150) cline
         if(violine(cline)) go to 9780
         write(20,'(a)',err=9200) trim(cline)
         call lowcas(cline)
         if(index(cline,'* observation data').ne.0) go to 520
c         call linesplit(ifail,2,lw,rw,cline)
c         if(ifail.eq.0) then
c           icovind=icovind+1
c           write(6,519)
c519        format(/,' *** Observation covariance matrix not ',
c     +     'permitted for present ***',/,' *** version of ',
c     +     'SVDAPREP ***',/)
c           write(6,518)
c518        format(' *** Matrix will be ignored and supplied weights ',
c     +     'used instead ***',/)
c         end if
       end do
520    continue
       do i=1,nobs
         read(10,'(a)',err=9150,end=9150) cline
         if(violine(cline)) go to 9780
         write(20,'(a)',err=9200) trim(cline)
       end do

C -- If prior information must be transferred as observations, this is
C    done now.

C -- But first the first part of the PICALC input template file is written.

       if(aa.eq.'y')then
         open(unit=21,file='picalc.tpl',action='write',iostat=ierr)
         if(ierr.ne.0) go to 9300
         write(21,739,err=9300)
739      format('ptf $')
         write(21,740,err=9300)
740      format('* control data')
         write(21,741,err=9300) nespar,nprior
741      format(2i6)
         write(21,742,err=9300)
742      format('* parameter data')
         do i=1,npar
           if(itrans(i).lt.0) cycle
           write(21,750,err=9300) trim(apar(i)), itrans(i)
750        format(1x,'$',a,t15,'$',t20,i3)
         end do
         write(21,751,err=9300)
751      format('* "x" matrix')

C -- Prior information is read and transferred.

         do
           read(10,'(a)',err=9150,end=709) cline
           if(violine(cline)) go to 9780
           call lowcase(cline)
           if(index(cline,'* prior information').ne.0) go to 700
         end do
709      write(errmsg,710) trim(pestfile)
710      format('Cannot find "prior information" section in PEST ',
     +   'control file ',a)
         go to 9880
700      continue

         call prior_read(jfail,npar,nprior,nespar,itrans,apar,aprior,
     +   cline)
         if(jfail.eq.1) then
           go to 9400
         else if (jfail.eq.2)then
           go to 9300
         else if (jfail.eq.4)then
           go to 9200
         else if (jfail.eq.3)then
           write(errmsg,701) trim(pestfile)
701        format('A group name must be supplied with each ',
     +     'prior information equation in PEST control file ',a)
           go to 9880
         else if(jfail.eq.5)then
           go to 9780
         end if
         close(unit=21,err=9300)
         write(6,791)
791      format(' - file picalc.tpl written ok.')

C -- The PICALC instruction file is written.

         open(unit=21,file='picalc.ins',action='write',err=9500)
         write(21,830,err=9500)
830      format('pif $')
         do i=1,nprior
           write(21,840,err=9500) trim(aprior(i))
840        format('l1 [',a,']1:25')
         end do
         close(unit=21,err=9500)
         write(6,850)
850      format(' - file picalc.ins written ok.')
         rewind(unit=10,iostat=ierr)
         if(ierr.ne.0)then
           call addquote(pestfile,qfile)
           write(errmsg,851) trim(qfile)
851        format('Cannot rewind file ',a)
           go to 9880
         end if
         do i=1,npar+nobs+nobsgp
           read(10,'(a)',err=9150,end=9150) cline
!           if(violine(cline)) go to 9780
         end do

       end if

C -- We continue writing the new PEST control file.

       do
         read(10,'(a)',err=9150,end=9150) cline
         if(violine(cline)) go to 9780
         call lowcase(cline)
         if(index(cline,'* model comman').ne.0) go to 779
         if(index(cline,'* derivatives command').ne.0)then
#ifdef MPEST
           write(errmsg,852)
852        format('Base PEST control file has a "derivatives ',
     +     'command line" section; MPEST cannot activate SVD-assist ',
     +     'functionality if parameter derivatives are ',
     +     'model-calculated.')
           go to 9880
#else
           iderflag=1
C           if(idercomflag.ne.0)then
C             if(jacfile.ne.0)then
C             write(errmsg,853) trim(par_fd)
C853          format('Base PEST control file has a "derivatives ',
C     +       'command line" section and the JACFILE control ',
C     +       'variable is set to 1. SVDAPREP can write a super ',
C     +       'parameter PEST control file in which model-calculated ',
C     +       'derivatives are used. However for at least one ',
C     +       'parameter in the base PEST control file (viz. parameter ',
C     +       '"',a,'") derivatives are calculated using finite ',
C     +       'differences. This is not allowed.')
C             go to 9880
C             end if
C           end if
           write(20,'(a)',err=9200) trim(cline)
           read(10,'(a)',err=9150,end=9150) comjac
           if(violine(comjac)) go to 9780
           call remchar(comjac,'"')
           call remchar(comjac,'''')
           comjac=adjustl(comjac)
           write(20,562,err=9200)
#ifdef UNIX
562        format('./svdabatch_d.bat')
#else
562        format('svdabatch_d.bat')
#endif
           read(10,'(a)',err=9150,end=9150) cline
           if(violine(cline)) go to 9780
           write(20,'(a)',err=9200) trim(cline)
#endif
         end if
       end do
779    continue
       if(jacfile.eq.1)then
         if(iderflag.eq.0)then
           write(errmsg,778)
778        format('Base PEST control file has JACFILE control ',
     +     'variable set to 1. However there is no "derivatives ',
     +     'command line" section present in this file.')
           go to 9880
         end if
       end if

       write(20,550,err=9200)
550    format('* model command line')
       read(10,'(a)',err=9150,end=9150) modbatch
       if(violine(modbatch)) go to 9780
       call remchar(modbatch,'"')
       call remchar(modbatch,'''')
       modbatch=adjustl(modbatch)
#ifdef MPEST
       mm=index(modbatch,'mprun')
       if(mm.ne.0)then
         modbatch=modbatch(mm+5:)
         modbatch=adjustl(modbatch)
         mm=index(modbatch,' ')
         modbatch=modbatch(1:mm-1)
       end if
       mm=len_trim(outfile)
       model_command='mprun ./svdabatch.bat '//outfile(1:mm-4)//' 20'
       write(20,559) trim(model_command)
559    format(a)
#else
       write(20,560,err=9200)
#ifdef UNIX
560    format('./svdabatch.bat')
#else
560    format('svdabatch.bat')
#endif
#endif
       read(10,'(a)',err=9150,end=9150) cline
       if(violine(cline)) go to 9780
       call lowcas(cline)
       jjflag=0
       if(cline.eq.'* model input') jjflag=1
       write(20,570,err=9200)
570    format('* model input/output')
       do i=1,ntpfle
         read(10,'(a)',err=9150,end=9150) cline
         if(violine(cline)) go to 9780
         call spacesub(cline)
         call linesplit(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9150
         tplfle(i)=cline(lw(1):rw(1))
         call remchar1(tplfle(i),char(211))
         infle(i)=cline(lw(2):rw(2))
         call remchar1(infle(i),char(211))
       end do
       write(20,580,err=9200)
580    format('parcalc.tpl   parcalc.in')  ! used to be parcalc.dat
       if(jjflag.eq.1)then
         read(10,'(a)',err=9150,end=9150) cline
         if(violine(cline)) go to 9780
       end if
       do i=1,ninsfle
         read(10,'(a)',err=9150,end=9150) cline
         if(violine(cline)) go to 9780
         write(20,'(a)',err=9200) trim(cline)
       end do
       if(aa.eq.'y')then
         write(20,590,err=9200)
590      format('picalc.ins   picalc.out')
       end if

C -- Regularisation and/or predictive analysis information is transferred if necessary.

       if(bb.eq.'r')then
         do
           read(10,'(a)',err=9150,end=595) cline
           if(cline.eq.' ') cycle
           if(violine(cline)) go to 9780
           call lowcase(cline)
           if(index(cline,'* regul').ne.0) go to 900
         end do
595      continue
         call addquote(pestfile,qfile)
         write(errmsg,910) trim(qfile)
910      format('Cannot find "regularisation" section in PEST ',
     +   'control file ',a)
         go to 9880
900      write(20,920,err=9200)
920      format('* regularisation')
         do
           read(10,'(a)',err=9150,end=950) cline
           if(cline.eq.' ') cycle
           if(violine(cline)) go to 9780
           write(20,'(a)',err=9200) trim(cline)
         end do
950      continue
       else if(bb.eq.'p')then
         do
           read(10,'(a)',err=9150,end=596) cline
           if(cline.eq.' ') cycle
           if(violine(cline)) go to 9780
           call lowcase(cline)
           if(index(cline,'* pred').ne.0) go to 901
         end do
596      continue
         call addquote(pestfile,qfile)
         write(errmsg,911) trim(qfile)
911      format('Cannot find "predictive analysis" section in PEST ',
     +   'control file ',a)
         go to 9880
901      write(20,921,err=9200)
921      format('* predictive analysis')
         do
           read(10,'(a)',err=9150,end=948) cline
           if(cline.eq.' ') cycle
           if(violine(cline)) go to 9780
           write(20,'(a)',err=9200) trim(cline)
         end do
948      continue
       else if(bb.eq.'a')then
         do
           read(10,'(a)',err=9150,end=967) cline
           if(cline.eq.' ') cycle
           if(violine(cline)) go to 9780
           call lowcase(cline)
           if(index(cline,'* pareto').ne.0) go to 971
         end do
967      continue
         call addquote(pestfile,qfile)
         write(errmsg,961) trim(qfile)
961      format('Cannot find "pareto" section in PEST ',
     +   'control file ',a)
         go to 9880
971      write(20,962,err=9200)
962      format('* pareto')
         do
           read(10,'(a)',err=9150,end=963) cline
           if(cline.eq.' ') cycle
           if(violine(cline)) go to 9780
           write(20,'(a)',err=9200) trim(cline)
         end do
963      continue
       end if

       close(unit=10)
       close(unit=20)
       call addquote(outfile,qfile)
       write(6,930) trim(qfile)
930    format(' - file ',a,' written ok.')

C -- The model batch file is now modified. If necessary also the derivatives batch file.

       nn=len_trim(modbatch)
       nn=nn-3
       if(nn.lt.1)nn=1
       atemp=modbatch(nn:)
       call lowcase(atemp)
#ifndef UNIX
       if(atemp.ne.'.bat')then
         call addquote(pestfile,qfile)
         write(errmsg,931) trim(qfile)
931      format('The model command line in PEST control ',
     +   'file ',a,' must cite a batch file; this command ',
     +   'must end in ".bat".')
         go to 9880
       end if
#endif
       open(unit=12,file=modbatch,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(modbatch,qfile)
         write(errmsg,940) trim(qfile)
#ifdef UNIX
940      format('Cannot open model script file ',a)
#else
940      format('Cannot open model batch file ',a)
#endif
         go to 9880
       end if

       if(comjac.ne.' ')then
         nn=len_trim(comjac)
         nn=nn-3
         if(nn.lt.1)nn=1
         atemp=comjac(nn:)
         call lowcase(atemp)
#ifndef UNIX
         if(atemp.ne.'.bat')then
           call addquote(pestfile,qfile)
           write(errmsg,932) trim(qfile)
932        format('The derivatives command line in PEST control ',
     +     'file ',a,' must cite a batch file; this command ',
     +     'must end in ".bat".')
           go to 9880
         end if
#endif
         open(unit=13,file=comjac,status='old',iostat=ierr)
         if(ierr.ne.0)then
           call addquote(comjac,qfile)
           write(errmsg,941) trim(qfile)
#ifdef UNIX
941        format('Cannot open model derivatives script file ',a)
#else
941        format('Cannot open model derivatives batch file ',a)
#endif
           go to 9880
         end if
       end if

       open(unit=20,file='svdabatch.bat',action='write',err=9600)
       if(comjac.ne.' ')then
         open(unit=21,file='svdabatch_d.bat',action='write',err=9620)
       end if
#ifndef UNIX
       if(am.eq.'s')then
         write(20,949,err=9600)
         if(comjac.ne.' ') write(21,949,err=9620)
949      format('@echo off')
       end if
#endif
#ifdef MPEST
       write(20,946,err=9600)
946    format('#! /bin/tcsh')
       write(20,952,err=9600)
#endif
       write(20,951,err=9600)
       if(comjac.ne.' ') write(21,951,err=9620)
#ifdef UNIX
951    format('# This part of script file added by SVDAPREP')
#else
951    format('REM This part of batch file added by SVDAPREP')
#endif
       write(20,952,err=9600)
       if(comjac.ne.' ')write(21,952,err=9620)
#ifdef UNIX
952    format('#')
#else
952    format('REM')
#endif
       write(20,960,err=9600)
       if(comjac.ne.' ')write(21,960,err=9620)
#ifdef UNIX
960    format('# Delete model input files.')
#else
960    format('REM Delete model input files.')
#endif


#ifdef MPEST
       write(20,952,err=9600)
       write(20,1400,err=9600)
1400   format('if ( $1:e == "") then')
       do i=1,ntpfle
         call addquote(infle(i),qfile)
         write(20,1410,err=9600) trim(qfile),trim(nulstring)
1410     format('/bin/rm ',a,a)
       end do
       write(20,1420,err=9600)
1420   format('else')
       do i=1,ntpfle
         aafile=infle(i)
         mm=npnt(aafile)
         if(mm.lt.0)then
           write(20,1430,err=9600) trim(aafile),trim(nulstring)
1430       format('/bin/rm ',a,'.$1:e',a)
         else
           write(20,1431,err=9600) aafile(1:mm),trim(aafile(mm:)),
     +     trim(nulstring)
1431       format('/bin/rm ',a,'$1:e',a,a)
         end if
       end do
       write(20,1440,err=9600)
1440   format('endif')
#else
       do i=1,ntpfle
         call addquote(infle(i),qfile)
         write(20,970,err=9600) trim(qfile),trim(nulstring)
         if(comjac.ne.' ')
     +   write(21,970,err=9620) trim(qfile),trim(nulstring)
#ifdef UNIX
970      format('/bin/rm ',a,a)
#else
970      format('del ',a,a)
#endif
       end do
#endif
       write(20,952,err=9600)
       if(comjac.ne.' ')write(21,952,err=9620)
       write(20,980,err=9600)
       if(comjac.ne.' ')write(21,980,err=9620)
#ifdef UNIX
980    format('# Run PARCALC to compute base parameters from ',
     + 'super parameters.')
#else
980    format('REM Run PARCALC to compute base parameters from ',
     + 'super parameters.')
#endif
#ifdef MPEST
       write(20,1000,err=9600) trim(nulstring)
1000   format('parcalc $1:e',a)
#else
       write(20,1000,err=9600) trim(nulstring)
       if(comjac.ne.' ')write(21,1000,err=9620) trim(nulstring)
1000   format('parcalc',a)    ! used to require redirected keybouard file
#endif
       if(aa.eq.'y')then
         write(20,952,err=9600)
         if(comjac.ne.' ')write(21,952,err=9620)
         write(20,1010,err=9600)
         if(comjac.ne.' ')write(21,1010,err=9620)
#ifdef UNIX
1010     format('# Run PICALC to compute base parameter prior ',
     +   'information.')
#else
1010     format('REM Run PICALC to compute base parameter prior ',
     +   'information.')
#endif
#ifdef MPEST
         write(20,1020,err=9600) trim(nulstring)
1020     format('picalc $1:e',a)
#else
         write(20,1020,err=9600) trim(nulstring)
         if(comjac.ne.' ')write(21,1020,err=9620) trim(nulstring)
1020     format('picalc',a)
#endif
       end if
       write(20,952,err=9600)
       call addquote(modbatch,qfile)
       write(20,1030,err=9600) trim(qfile)
       if(comjac.ne.' ')then
         write(21,952,err=9620)
         call addquote(comjac,qfile)
         write(21,1030,err=9620) trim(qfile)
       end if
#ifdef UNIX
1030   format('# The following is copied directly from file ',a)
#else
1030   format('REM The following is copied directly from file ',a)
#endif
       write(20,952,err=9600)
       if(comjac.ne.' ') write(21,952,err=9620)
       do
         read(12,'(a)',end=1099,err=9650) cline
#ifdef MPEST
         if(index(cline,'/bin/tcsh').eq.0)
     +   write(20,'(a)',err=9600) trim(cline)
#else
         write(20,'(a)',err=9600) trim(cline)
#endif
       end do
1099   continue
       if(comjac.ne.' ')then
         do
           read(13,'(a)',end=1100,err=9670) cline
           write(21,'(a)',err=9620) trim(cline)
         end do
       end if
1100   continue
       close(unit=12)
       if(comjac.ne.' ') close(unit=13)
       close(unit=20,err=9600)
       if(comjac.ne.' ') close(unit=21,err=9620)
       write(6,1040)
1040   format(' - file svdabatch.bat written ok.')
       if(comjac.ne.' ') write(6,1041)
1041   format(' - file svdabatch_d.bat written ok.')
#ifdef UNIX
#ifdef SYS_FUNCTION
       nnn=system('chmod u+x svdabatch.bat')
#else
       call system('chmod u+x svdabatch.bat')
#endif
       if(comjac.ne.' ')then
#ifdef SYS_FUNCTION
       nnn=system('chmod u+x svdabatch_d.bat')
#else
       call system('chmod u+x svdabatch_d.bat')
#endif
       end if
#endif

C -- If observation re-referencing is done, other batch files are written.

      if(irefobs.ne.0)then
        do imodbatch=1,2
          if(imodbatch.eq.1)then
            oldfile='d_'//adjustl(trim(modbatch))
            newfile='d_svdabatch.bat'
          else
            oldfile='r_'//adjustl(trim(modbatch))
            newfile='r_svdabatch.bat'
          end if

          open(unit=12,file=oldfile,status='old',iostat=ierr)
          if(ierr.ne.0)then
            call addquote(oldfile,qfile)
            write(errmsg,1940) trim(qfile)
#ifdef UNIX
1940          format('Cannot open model script file ',a,
     +        '. This file is expected when observation ',
     +        're-referencing is activated.')
#else
1940          format('Cannot open model batch file ',a,
     +        '. This file is expected when observation ',
     +        're-referencing is activated.')
#endif
              go to 9880
           end if

           open(unit=20,file=newfile,action='write',err=9700)
#ifndef UNIX
           if(am.eq.'s')then
             write(20,949,err=9700)
           end if
#endif
           write(20,951,err=9700)
           write(20,952,err=9700)
           write(20,960,err=9700)
           do i=1,ntpfle
             call addquote(infle(i),qfile)
             write(20,970,err=9700) trim(qfile),trim(nulstring)
           end do

           write(20,952,err=9700)
           write(20,980,err=9700)
           write(20,1000,err=9700) trim(nulstring)
           if(aa.eq.'y')then
             write(20,952,err=9700)
             write(20,1010,err=9700)
             write(20,1020,err=9700) trim(nulstring)
           end if
           write(20,952,err=9700)
           call addquote(oldfile,qfile)
           write(20,1030,err=9700) trim(qfile)
           write(20,952,err=9700)
           do
             read(12,'(a)',end=10991,err=9750) cline
             write(20,'(a)',err=9700) trim(cline)
           end do
10991      continue
           close(unit=12)
           close(unit=20,err=9700)
           write(6,10401) trim(newfile)
10401      format(' - file ',a,' written ok.')
#ifdef UNIX
#ifdef SYS_FUNCTION
           nnn=system('chmod u+x '//trim(newfile))
#else
           call system('chmod u+x '//trim(newfile))
#endif
#endif
         end do
       end if

C -- A warning to the user is written.

       if(extsuper.ne.1)then
       write(6,1210)
1210   format(/,' A note on the SVDA_SCALADJ variable:-')
       write(errmsg,1220)
1220   format('If all adjustable base parameters are log-',
     + 'transformed, this can be set to zero. If the bounds supplied ',
     + 'for non-log-transformed base parameters are set very wide '
     + '(presumably so that they do not interfere with parameter ',
     + 'estimates), set SVDA_SCALADJ to 2. If bounds are set ',
     + 'at the approximate +-95% confidence limit of each non-log-',
     + 'transformed parameter ',
     + 'set SVDA_SCALADJ to 3 (often the better alternative). See ',
     + 'the PEST manual for further details.')
       call write_message(errmsg)
       end if

       if(isenreuse.ne.0)then
       write(6,1230)
1230   format(/,' A note on sensitivity reuse:-')
       write(errmsg,1240)
1240   format('The base PEST control file asks for sensitivity ',
     + 'reuse. The DOSENREUSE variable has not been automatically ',
     + 'set to "senreuse" in the super parameter PEST control file. ',
     + 'Nor has a "sensitivity reuse" section between included in ',
     + 'this file. These decisions require consideration by the user.')
       call write_message(errmsg)
       end if


       go to 9900

9150   call addquote(pestfile,qfile)
       write(errmsg,9160) trim(qfile)
9160   format('Error reading file ',a,'; check this file with ',
     + 'PESTCHEK.')
       go to 9880
9200   call addquote(outfile,qfile)
       write(errmsg,9210) trim(qfile)
9210   format('Cannot write to new PEST control file ',a)
       go to 9880
9300   write(errmsg,9310)
9310   format('Cannot write to PICALC template file picalc.tpl.')
       go to 9880
9400   call addquote(pestfile,qfile)
       write(errmsg,9410) trim(qfile)
9410   format('Error in "prior information" section of file ',
     + a,'; check this file with PESTCHEK.')
       go to 9880
9500   write(errmsg,9510)
9510   format('Cannot write instruction file picalc.ins')
       go to 9880
9600   write(errmsg,9610)
#ifdef UNIX
9610   format('Cannot write to model script file svdabatch.bat')
#else
9610   format('Cannot write to model batch file svdabatch.bat')
#endif
       go to 9880
9620   write(errmsg,9630)
#ifdef UNIX
9630   format('Cannot write to model script file svdabatch_d.bat')
#else
9630   format('Cannot write to model batch file svdabatch_d.bat')
#endif
       go to 9880

9650   call addquote(modbatch,qfile)
       write(errmsg,9660) trim(qfile)
#ifdef UNIX
9660   format('Error reading model script file ',a)
#else
9660   format('Error reading model batch file ',a)
#endif
       go to 9880

9670   call addquote(comjac,qfile)
       write(errmsg,9680) trim(qfile)
#ifdef UNIX
9680   format('Error reading model derivatives script file ',a)
#else
9680   format('Error reading model derivatives batch file ',a)
#endif
       go to 9880

9700   continue
       write(errmsg,9710) trim(newfile)
#ifdef UNIX
9710   format('Cannot write to model script file ',a)
#else
9710   format('Cannot write to model batch file ',a)
#endif
       go to 9880

9750   call addquote(oldfile,qfile)
       write(errmsg,9760) trim(qfile)
#ifdef UNIX
9760   format('Error reading model script file ',a)
#else
9760   format('Error reading model batch file ',a)
#endif
       go to 9880

9780   write(errmsg,9790)
9790   format('The PEST control file contains comments and/or PEST++ ',
     + 'data lines. Remove these using PSTCLEAN before running ',
     + 'SVDAPREP and PEST. Then rename or copy the accompanying JCO ',
     + 'file so that it has the same filename base as the new PEST ',
     + 'control file. Alternatively, remove comment and PEST++ data ',
     + 'lines from the PEST control file manually.')
       go to 9880

9880   call write_message(errmsg)
9890   continue
9900   continue

       if(allocated(itrans)) deallocate(itrans)
       if(allocated(apar)) deallocate(apar)
       if(allocated(aprior)) deallocate(aprior)
       if(allocated(tplfle)) deallocate(tplfle)
       if(allocated(infle)) deallocate(infle)

       end


        SUBROUTINE LOWCASE(ASTRNG)

C -- Subroutine LOWCASE converts a string to lower case.

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,LEN_TRIM(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN
        END

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



        SUBROUTINE DREALREAD(IFAIL,CLINE,RTEMP)

C -- Subroutine REALREAD reads a real number from a string.

        INTEGER IFAIL
        double precision RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') LEN_TRIM(CLINE)
        READ(CLINE,AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END


         SUBROUTINE LINESPLIT(IFAIL,NUM,LW,RW,CLINE)

C -- Subroutine LINESPLIT splits a string into blank-delimited fragments.

        INTEGER IFAIL,NW,NBLC,J,I
        INTEGER NUM
        INTEGER LW(NUM),RW(NUM)
        CHARACTER*(*) CLINE

        IFAIL=0
        NW=0
        NBLC=LEN_TRIM(CLINE)
        IF((NBLC.NE.0).AND.(INDEX(CLINE,CHAR(9)).NE.0)) THEN
          CALL TABREM(CLINE)
          NBLC=LEN_TRIM(CLINE)
        ENDIF
        IF(NBLC.EQ.0) THEN
          IFAIL=-1
          RETURN
        END IF
        J=0
5       IF(NW.EQ.NUM) RETURN
        DO 10 I=J+1,NBLC
          IF((CLINE(I:I).NE.' ').AND.(CLINE(I:I).NE.',').AND.
     +    (ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10      CONTINUE
        IFAIL=1
        RETURN
20      NW=NW+1
        LW(NW)=I
        DO 30 I=LW(NW)+1,NBLC
          IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',').OR.
     +    (ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30      CONTINUE
        RW(NW)=NBLC
        IF(NW.LT.NUM) IFAIL=1
        RETURN
40      RW(NW)=I-1
        J=RW(NW)
        GO TO 5

        END


        SUBROUTINE TABREM(CLINE)

C -- Subroutine TABREM removes tabs from a string.

        INTEGER I
        CHARACTER*(*) CLINE

        DO 10 I=1,LEN(CLINE)
10      IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '

        RETURN
        END


        subroutine substring(astring,asub1,asub2)

! -- Note: this subroutine will not work if asub1 can be found in asub2
!    (It will get caught in an infinite loop.)

        implicit none
        integer n1,n2,i
        character*(*) astring,asub1,asub2

        n1=len_trim(asub1)
        n2=len_trim(asub2)

10      i=index(astring,asub1)
        if(i.eq.0) return
        astring=astring(1:i-1)//asub2(1:n2)//astring(i+n1:)
        go to 10

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



        SUBROUTINE PRIOR_READ(JFAIL,NPAR,NPRIOR,NESPAR,
     +  ITRANS,APAR,APRIOR,CLINE)

C -- SUBROUTINE PRIOR_READ READS THE PRIOR INFORMATION SECTION OF A
C -- PEST CONTROL FILE

        IMPLICIT NONE

        logical violine
        INTEGER IFAIL,JFAIL
        INTEGER NPAR,IPAR,NPRIOR,NESPAR,IES,N1,N2,NBLC,ILOG,J,
     +  JSTART,JOBS,JWGHT,JNAME,JX,I,NEG,JMATH,JMUL,JOGNME
        INTEGER ITRANS(NPAR)
        DOUBLE PRECISION RTEMP,OVAL,OWGHT
        CHARACTER*(*) APAR(NPAR)
        CHARACTER*10 FMT,AAPAR*12
        CHARACTER*12 ATEMP
        CHARACTER*(*) CLINE
        CHARACTER*20 APRIOR(NPRIOR)

        JFAIL=0
        IPAR=1
        JMUL=0
        JMATH=0
        JOBS=0
        JWGHT=0
        JNAME=0
        JOGNME=0
        JX=0
        NEG=0
        DO 10 I=1,NPRIOR
        JSTART=1
35      N2=0
        READ(10,36,END=9100) CLINE
        if(violine(cline)) go to 9780
36      FORMAT(A)
        NBLC=LEN_TRIM(CLINE)
38      CALL GETINT(IFAIL,CLINE,N1,N2,NBLC,' ')
        IF(N1.EQ.0) GO TO 35
        IF(CLINE(N1:N1).EQ.'&') GO TO 38
        IF(JSTART.EQ.1) THEN
          APRIOR(I)=CLINE(N1:N2)
          CALL LOWCASE(APRIOR(I))
          JSTART=0
          JX=1
          NEG=0
        ELSE IF(CLINE(N1:N2).EQ.'=') THEN
          IF(JMATH.EQ.0) GO TO 9200
          JMATH=0
          JOBS=1
        ELSE IF(CLINE(N1:N2).EQ.'+') THEN
          IF(JMATH.EQ.0) GO TO 9200
          JMATH=0
          JX=1
          NEG=0
        ELSE IF(CLINE(N1:N2).EQ.'-') THEN
          IF(JMATH.EQ.0) GO TO 9200
          JMATH=0
          JX=1
          NEG=1
        ELSE IF(CLINE(N1:N2).EQ.'*') THEN
          IF(JMUL.EQ.0) GO TO 9200
          JMUL=0
          JNAME=1
        ELSE IF(JX.EQ.1) THEN
          WRITE(FMT,20) N2-N1+1
20        FORMAT('(F',I3,'.0)')
          READ(CLINE(N1:N2),FMT,ERR=9200) RTEMP
          IF(NEG.EQ.1) RTEMP=-RTEMP
          JX=0
          JMUL=1
          NEG=0
        ELSE IF(JNAME.EQ.1) THEN
          CALL LOWCASE(CLINE(N1:N2))
          IF(CLINE(N1:N1+2).EQ.'log') THEN
            AAPAR=CLINE(N1+4:N2-1)
            ILOG=1
          ELSE
            AAPAR=CLINE(N1:N2)
            ILOG=0
          END IF
          CALL WHICH1(IFAIL,NPAR,IPAR,APAR,AAPAR)
          IF(IFAIL.NE.0) GO TO 9300
          IF(ITRANS(IPAR).LT.0) GO TO 9400
          IF((ILOG.EQ.0).AND.(ITRANS(IPAR).EQ.1)) GO TO 9500
          IF((ILOG.EQ.1).AND.(ITRANS(IPAR).EQ.0)) GO TO 9600
          IES=0
          DO 50 J=1,NPAR
          IF(ITRANS(J).LT.0) GO TO 50
          IES=IES+1
          IF(J.EQ.IPAR) GO TO 60
50        CONTINUE
60        CONTINUE
          IF(RTEMP.NE.0.0D0) THEN
            WRITE(21,61,ERR=9700) I,IES,RTEMP
61          FORMAT(1X,I6,1X,I6,1X,1PG23.16)
          END IF
          JNAME=0
          JMATH=1
        ELSE IF(JOBS.EQ.1) THEN
          WRITE(FMT,20) N2-N1+1
          READ(CLINE(N1:N2),FMT,ERR=9200) OVAL
          JOBS=0
          JWGHT=1
        ELSE IF(JWGHT.EQ.1) THEN
          WRITE(FMT,20) N2-N1+1
          READ(CLINE(N1:N2),FMT,ERR=9200) OWGHT
          JWGHT=0
          IF(CLINE(N2+1:).EQ.' ')THEN
            JFAIL=3
            RETURN
          ELSE
            JOGNME=1
          END IF
        ELSE IF(JOGNME.EQ.1)THEN
          ATEMP=CLINE(N1:N2)
          CALL LOWCASE(ATEMP)
          JOGNME=0
          WRITE(20,63,ERR=9750)
     +    TRIM(APRIOR(I)),OVAL,OWGHT,TRIM(ATEMP)
63        FORMAT(1X,A,2X,1PG14.7,2X,1PG14.7,2X,A)
          GO TO 10
        ELSE
          GO TO 9200
        END IF
        GO TO 38
10      CONTINUE
        RETURN

9100    CONTINUE
        GO TO 9891
9200    CONTINUE
        GO TO 9891
9300    CONTINUE
        GO TO 9891
9400    CONTINUE
        GO TO 9891
9500    CONTINUE
        GO TO 9891
9600    CONTINUE
        GO TO 9891
9700    CONTINUE
        JFAIL=2
        RETURN
9750    CONTINUE
        JFAIL=4
        RETURN
9780    continue
        jfail=5
        return
9891    JFAIL=1
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




        SUBROUTINE LOWCAS(ASTRNG)

C -- SUBROUTINE LOWCAS CONVERTS A STRING TO LOWER CASE

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,len_trim(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN
        END


        subroutine remchar(astring,aa)

C -- Subroutine REMCHAR removes a character from a string; it then moves
C    the rest of the string to the left to cover the space.

        implicit none
        integer l,i
        character*(*) astring,aa

        l=len_trim(aa)
10      continue
        i=index(astring,aa)
        if(i.eq.0) return
        astring(i:)=astring(i+l:)
        go to 10

        end


        subroutine remchar1(astring,aa)

C -- Subroutine REMCHAR removes a character from a string. However it leaves
C    a space in its place.

        implicit none
        integer l,i
        character*(*) astring,aa

        l=len_trim(aa)
10      continue
        i=index(astring,aa)
        if(i.eq.0) return
        astring(i:i+l-1)=' '
        go to 10

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

      SUBROUTINE SPACESUB(ASTRING)

      INTEGER I,J,K,N
      CHARACTER*1 BB
      CHARACTER*(*) ASTRING

      BB=CHAR(211)
      N=len_trim(ASTRING)
      K=1
10    CONTINUE
      IF(K.GT.N) GO TO 100
      DO I=K,N
        IF((ASTRING(I:I).EQ.'''').OR.(ASTRING(I:I).EQ.'"'))THEN
          ASTRING(I:I)=' '
          DO J=I+1,N
            IF((ASTRING(J:J).EQ.'''').OR.(ASTRING(J:J).EQ.'"'))THEN
              ASTRING(J:J)=' '
              K=J+1
              GO TO 10
            END IF
            IF(ASTRING(J:J).EQ.' ')ASTRING(J:J)=BB
          END DO
          GO TO 100
        END IF
      END DO

100   CONTINUE
      RETURN
      END



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


        integer function npnt(astring)

        implicit none
        character*(*) astring
        integer i,j

        npnt=-99999999
        j=len_trim(astring)
        do i=1,j
          if(astring(i:i).eq.'.')then
            npnt=i
            return
          end if
        end do
        return
        end



        SUBROUTINE READ_MRUN_VAR(IFAIL,DVAL,VARTEXT,UPVARTEXT,ILINE,
     +  AFILE,CLINE,ERRMSG,ITYPE,BFILE)

        IMPLICIT NONE
        INTEGER IFAIL,ILINE,ITYPE
        DOUBLE PRECISION DVAL
        CHARACTER*1 AA
        CHARACTER*(*) VARTEXT,UPVARTEXT
        CHARACTER*(*) AFILE,CLINE,ERRMSG,BFILE

        INTEGER  NN,NNLT,I
        CHARACTER*20 ALINE
        CHARACTER*25 ATEMP25
        CHARACTER*256 ATEMP256

        IFAIL=0
        AA=' '
        IF(ITYPE.EQ.2)BFILE=' '
        NNLT=LEN_TRIM(VARTEXT)
        NN = INDEX(CLINE,TRIM(VARTEXT))
        IF(NN.EQ.0) THEN
          GO TO 9900
        ELSE
          ATEMP256=ADJUSTL(CLINE(NN+NNLT:))
          IF(ATEMP256(1:1).NE.'=')THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,2105) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
2105        FORMAT('Cannot read value of ',A,' variable ',
     +      'on line ',A,' of file ',A,'.')
            GO TO 9890
          END IF
          ATEMP256=ADJUSTL(ATEMP256(2:))
          NN=INDEX(ATEMP256,' ')
          IF((NN.EQ.0).OR.(NN.EQ.1))THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,2105) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
            GO TO 9890
          END IF
          IF(ITYPE.EQ.1)THEN
            ATEMP25=ATEMP256
            ATEMP25(NN:)=' '
            CALL DREALREAD(IFAIL,ATEMP25,DVAL)
            IF(IFAIL.NE.0) THEN
              CALL WRITINT(ALINE,ILINE)
              WRITE(ERRMSG,2105) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
              GO TO 9890
            END IF
          ELSE
            AA=' '
            IF(ATEMP256(1:1).EQ.'''')THEN
              AA=''''
            ELSE IF(ATEMP256(1:1).EQ.'"')THEN
              AA='"'
            END IF
            IF(AA.NE.' ')THEN
              NN=INDEX(ATEMP256(2:),AA)
              IF(NN.EQ.0)THEN
                CALL WRITINT(ALINE,ILINE)
                WRITE(ERRMSG,2106) TRIM(UPVARTEXT),TRIM(ALINE),
     +                             TRIM(AFILE)
2106            FORMAT('Cannot read ',a,' filename on line ',A,
     +          ' of file ',A,'.')
                GO TO 9890
              END IF
              ATEMP256(NN+1:)=' '
              BFILE=ATEMP256(2:)
            ELSE
              BFILE=ADJUSTL(ATEMP256(1:NN-1))
            END IF
            IF(BFILE.EQ.' ')THEN
              CALL WRITINT(ALINE,ILINE)
              WRITE(ERRMSG,2106) TRIM(UPVARTEXT),TRIM(ALINE),
     +                           TRIM(AFILE)
            END IF
          END IF
          NN=INDEX(CLINE,TRIM(VARTEXT))
          CLINE(NN:)=CLINE(NN+NNLT:)
          DO I=NN,LEN(CLINE)
            IF(CLINE(I:I).EQ.'=')THEN
              CLINE(NN:)=CLINE(I+1:)
              GO TO 21061
            END IF
          END DO
21061     CONTINUE
          CLINE(NN:)=ADJUSTL(CLINE(NN:))
          IF(AA.NE.' ')CLINE(NN:)=ADJUSTL(CLINE(NN+1:))
          DO I=NN,LEN(CLINE)
            IF(CLINE(I:I).EQ.AA)THEN
              IF(AA.NE.' ')CLINE(I:I)=' '
              CLINE(NN:I-1)=' '
              GO TO 2108
            END IF
          END DO
2108      CONTINUE
          NN=INDEX(CLINE,TRIM(VARTEXT))
          IF(NN.NE.0)THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,2107) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
2107        FORMAT(A,' variable appears twice on line ',A,
     +      ' of file ',A,'.')
            GO TO 9890
          END IF
        END IF
        GO TO 9900

9890    CONTINUE
        IFAIL=1
9900    CONTINUE
        RETURN
        END


       logical function violine(cline)

       implicit none
       character*(*) cline
       integer nn,ll,icount,jcount,i

       violine=.FALSE.
       cline=adjustl(cline)
       if((cline.eq.' ').or.(cline(1:2).eq.'++')) then
         violine=.TRUE.
         go to 200
       end if
       nn=index(cline,'#')
       if(nn.eq.0)then
         go to 200
       else if(nn.eq.1)then
         violine=.TRUE.
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
               violine=.TRUE.
               go to 200
             end if
           end if
         end if
       end do

200    continue
       return
       end
