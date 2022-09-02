       program pnulpar

C -- Program PNULPAR projects differences between random parameter values and
C    calibrated parameter values onto the calibration null space and adds them
C    to calibrated values.

       use pestdata, only  : maxcompdim,pestmode,npar,nobsgp,nxrow,
     +                       errmsg,apar,obgnme,aobs,itrans,numcov,
     +                       covind,owght,x,pval_o,modval_o,
     +                       parchglim,pval,parlbnd,parubnd,
     +                       ipargp,scale,offset,dercom,covgp,nespar,
     +                       nobs,nobgnm,covar,nobscov


       implicit none

       logical lexist,isclose
       integer ibeg,iend,ifail,ierr,nb_pst,nb,j,nc,info,ig,ic,i,k,idim
       integer itemp1,itemp2,soldim,icount,lwork1,lwork2,lwork,
     + vtdim,icount1,icount2,ifile,iline,icurrent,nulldim,iwarn,
     + ifixedwarn
       integer irestart,ipp,itied,ies,irow,maxobs,icov,igroup,bn,
     + iobsgp
       integer p1start,pfile
       integer lw(5),rw(5)
       double precision dtemp,sum,weight,ratio,ratio1
       double precision dtempmax,utemp,ltemp,mtemp
       double precision vtdum(1,1)
       character*1 aa,ap,at,am
       character*5 aext
       character*12 aversion
       character*12 aapar
       character*15 anum,aline,anumfile
       character*20 aaobs

       character*200 afile,pestfile,jcofile,bfile,parbase1,parbase2,
     + infile,outfile,matfile,cfile
       character*300 firstline,cline


       integer, allocatable          :: apresent(:)
       double precision, allocatable :: u(:,:),vt(:,:),s(:),work(:),
     +                                  pval1(:),dval(:),scale1(:),
     +                                  offset1(:),v2tp(:)

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' PNULPAR Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

       ifixedwarn=0

C -- Input data is acquired from the user.

20     write(6,30,advance='no')
30     format(' Enter name of PEST control file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 20
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,pestfile,ibeg,iend)
#ifndef UNIX
       call lowcas(pestfile)
#endif
       if(ifail.ne.0) go to 20
       nb_pst=len_trim(pestfile)
       nb_pst=nb_pst-3
       if(nb_pst.lt.1)nb_pst=1
       aext=pestfile(nb_pst:)
       call lowcas(aext)
       if(aext.ne.'.pst')then
         write(6,40)
40       format(/,' PEST control file must have an extension of ',
     +   '".pst" - try again.',/)
         go to 20
       end if
       inquire(file=pestfile,exist=lexist)
       if(.not.lexist)then
         call addquote(pestfile,afile)
         write(6,50) trim(afile)
50       format(/,' Cannot find file ',a,' - try again.',/)
         go to 20
       end if

C -- A check is made that the corresponding Jacobian matrix file is present.

       jcofile=pestfile(1:nb_pst)//'jco'
       call open_unformatted_file(ifail,11,'read',jcofile,errmsg)
       if(ifail.ne.0)then
         call addquote(jcofile,afile)
         call addquote(pestfile,bfile)
         write(errmsg,60) trim(afile),trim(bfile)
60       format('Cannot open Jacobian matrix file ',a,
     +   ' corresponding to PEST control file ',a,'.')
         go to 9890
       end if

C -- More input is sought.

62     write(6,63,advance='no')
63     format(' Does PEST control file contain calibrated parameter ',
     + 'values? [y/n]: ')
       read(5,*) aa
       if((aa.eq.'Y').or.(aa.eq.'y'))then
         continue
       else if((aa.eq.'n').or.(aa.eq.'N'))then
         write(errmsg,64)
64       format('Then use the PARREP utility to place calibrated ',
     +   'parameter values into PEST control file as initial ',
     +   'parameter values.')
         go to 9890
       else
         go to 62
       end if

       write(6,*)
80     write(6,90,advance='no')
90     format(' Enter number of dimensions of calibration ',
     + 'solution space: ')
       read(5,*,err=80) soldim
       if(soldim.le.0) go to 80
92     write(6,91,advance='no')
91     format(' Would you like to store Q(1/2)X matrix in matrix ',
     + 'file format? [y/n]: ')
       read(5,*) am
       if((am.eq.'y').or.(am.eq.'Y'))then
         am='y'
       else if((am.eq.'n').or.(am.eq.'N'))then
         am='n'
       else
         go to 92
       end if
       if(am.eq.'y')then
93       write(6,94,advance='no')
94       format(' Enter file for storage of matrix: ')
         read(5,*) matfile
       end if

       write(6,*)
100    write(6,110,advance='no')
110    format(' Enter filename base of existing parameter value ',
     + 'files: ')
       read(5,*) parbase1
#ifndef UNIX
       call lowcas(parbase1)
#endif
       nb=len_trim(parbase1)
       if(parbase1(nb:nb).eq.'.')parbase1(nb:nb)=' '
120    write(6,130,advance='no')
130    format(' Enter filename base for new parameter value files: ')
       read(5,*) parbase2
#ifndef UNIX
       call lowcas(parbase2)
#endif
       nb=len_trim(parbase2)
       if(parbase2(nb:nb).eq.'.')parbase2(nb:nb)=' '

c140    write(6,150,advance='no')
c150    format(' Do all of these files have an extension of ',
c     + '"par" or "bpa"? [p/b]: ')
c       read(5,*) ap
c       if((ap.eq.'P').or.(ap.eq.'p'))then
c         aext='.par'
c       else if((ap.eq.'B').or.(ap.eq.'b'))then
c         aext='.bpa'
c       else
c         go to 140
c       end if
       aext='.par'

C -- The complete PEST input dataset is now read.

       write(6,*)
       call addquote(pestfile,afile)
       write(6,235) trim(afile)
235    format(' - reading PEST control file ',a,'....')
       irestart=0
       p1start=0
       pfile=0
       call read_pest_data(ifail,irestart,p1start,pfile,pestfile)
       if(ifail.ne.0) go to 9890
       if(maxcompdim.gt.1)then
         write(errmsg,240) trim(afile)
240      format('PNULPAR requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.eq.2)then
         write(errmsg,250) trim(afile)
250      format('PNULPAR requires that PEST not be run in predictive ',
     +   'analysis mode in PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.eq.3)then
260      write(errmsg,270) trim(afile)
270      format('File ',a,' tells PEST to run in ',
     +   'regularisation mode. PNULPAR does not allow this. Copy to ',
     +   'another file and alter PESTMODE to "estimation" in this ',
     +   'new file. Then get rid of regularisation observations and ',
     +   'prior information equations and use JCO2JCO to create a ',
     +   'corresponding JCO file.')
         go to 9890
       end if
       if(nespar.eq.0)then
         write(errmsg,272) trim(afile)
272      format('There are no adjustable parameters cited in PEST ',
     +   'control file ',a,'.')
         go to 9890
       end if
       if(nxrow.eq.0)then
         write(errmsg,273) trim(afile)
273      format('No observations or prior information items are ',
     +   'cited in PEST control file ',a,'.')
         go to 9890
       end if
       do ipp=1,npar
         if(itrans(ipp).eq.1)then
           if(pval(ipp).le.0.0d0)then
             write(errmsg,274) trim(apar(ipp)),trim(afile)
274          format('Parameter "',a,'" in file ',a,' is log-',
     +       'transformed. It cannot be assigned a zero or ',
     +       'negative value in this file.')
             go to 9890
           end if
         else if((itrans(ipp).lt.0).and.(itrans(ipp).gt.-1000000))then
           itied=abs(itrans(ipp))
           if(pval(itied).eq.0.0)then
             write(errmsg,275) trim(apar(itied)),trim(afile)
275          format('Parameter "',a,'" is a parent parameter to ',
     +       'a tied parameter in file ',a,'. Therefore it must not ',
     +       'possess a value of zero.')
             go to 9890
           end if
         end if
       end do
       if(soldim.ge.nespar)then
         write(errmsg,271) trim(afile)
271       format('Dimensions of solution space as provided ',
     +    'in response to a previous prompt equals or exceeds ',
     +    'number of adjustable parameters in file ',a,'.')
          go to 9890
        end if

       write(6,276) trim(afile)
276    format(' - file ',a,' read ok.')

C -- We can delete some unused PEST memory.

       deallocate(pval_o,modval_o,parchglim,
     + ipargp,scale,offset,dercom,stat=ierr)

C -- If there are any tied parameters in the PEST control file another question
C    is asked.

       do ipp=1,npar
         if((itrans(ipp).lt.0).and.(itrans(ipp).gt.-1000000))then
           write(6,*)
264        write(6,265,advance='no')
265        format(' Respect parameter bounds for tied ',
     +     'parameters?  [y/n]: ')
           read(5,*) at
           if((at.eq.'y').or.(at.eq.'Y'))then
             at='y'
           else if((at.eq.'n').or.(at.eq.'N'))then
             at='n'
           else
             go to 264
           end if
           go to 266
         end if
       end do
266    continue

C -- The Jacobian matrix file is now read.

       call addquote(jcofile,afile)
       write(6,278) trim(afile)
278    format(/,' - reading Jacobian matrix file ',a,'....')
       read(11,err=9000,end=9000)itemp1,itemp2
       if(abs(itemp1).ne.nespar) go to 9050
       if(itemp1.lt.0)then
         continue
       else
         write(errmsg,290) trim(afile)
290      format('Jacobian matrix file ',a,' was not produced by ',
     +   'version 8 or later of PEST; use program JCOTRANS to ',
     +   'convert this to new format.')
         go to 9890
       end if
       itemp2=abs(itemp2)
       bn=itemp2
       if(itemp2.ne.nxrow) go to 9050
       do i=1,nespar
         do j=1,nxrow
           x(j,i)=0.0d0
         end do
       end do
       read(11,err=9000,end=9000)icount
       do i=1,icount
         read(11,err=9000,end=9000) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         x(irow,ies)=dtemp
       end do
       do i=1,npar
         if(itrans(i).lt.0) cycle
         read(11,err=9000,end=9000) aapar
         if(aapar.ne.apar(i)) go to 9050
       end do
       do i=1,bn
         read(11,err=9000,end=9000) aaobs
         call lowcas(aaobs)
         if(aaobs.ne.aobs(i)) go to 9050
       end do
       close(unit=11)
       write(6,276) trim(afile)

C --   If any covariance matrices were in the original PEST dataset the
C      square root of the inverse of these matrices is now found.

       maxobs=0
       if(numcov.ne.0)then
         do icov=1,numcov
           if(nobscov(icov).gt.maxobs) maxobs=nobscov(icov)
         end do
         if(maxobs.eq.0) then
           numcov=0
         end if
       end if
       lwork1=8*maxobs
       lwork2=max(3*min(nespar,nxrow)+max(nespar,nxrow),
     +          5*min(nxrow,nespar))
       lwork=max(lwork1,lwork2)+2*nespar
       vtdim=max(nespar,maxobs)
       allocate(s(vtdim),work(lwork),stat=ierr)
       if(ierr.ne.0) go to 9200
       if(numcov.ne.0)then
         allocate(u(maxobs,max(maxobs,nespar)),stat=ierr)
         if(ierr.ne.0) go to 9200
         write(6,581)
581      format(/,' - finding inverse root of observation covariance ',
     +   'matrices...')
         do icov=1,numcov
           nc=nobscov(icov)
           if(nc.eq.0) cycle
           call dgesvd('A','N',nc,nc,covar(icov)%dval,nc,
     +     s,u,maxobs,vtdum,1,work,lwork,info)
           if(info.ne.0)then
             igroup=covgp(icov)
             write(errmsg,582) trim(obgnme(igroup))
582          format('Cannot decompose covariance matrix supplied ',
     +       'for observation group "',a,'".')
             go to 9890
           end if
           do i=1,nc
             if(s(i).le.0.0d0)then
               igroup=covgp(icov)
               write(errmsg,585) trim(obgnme(igroup))
585            format('Covariance matrix supplied for observation ',
     +         'group "',a,'" is singular.')
               go to 9890
             else
               s(i)=1.0d0/sqrt(s(i))
             end if
           end do
           do i=1,nc
             do j=1,i
               sum=0.0d0
               do k=1,nc
                 sum=sum+u(i,k)*s(k)*u(j,k)
               end do
               covar(icov)%dval(j,i)=sum
               if(i.ne.j)then
                 covar(icov)%dval(i,j)=sum
               end if
             end do
           end do
         end do
       end if
584    continue

C -- The matrix Q(1/2)X is now formed.

       if(numcov.eq.0)then
         do irow=1,nxrow
           weight=sqrt(owght(irow))
           do ies=1,nespar
             x(irow,ies)=x(irow,ies)*weight
           end do
         end do
       else
         do iobsgp=1,nobsgp
           if(covind(iobsgp))then
             do icov=1,numcov
               if(covgp(icov).eq.iobsgp) go to 586
             end do
586          continue
             nc=nobscov(icov)
             if(nc.ne.0)then
               do ies=1,nespar
                 do ic=1,nc
                   u(ic,ies)=0.0d0
                 end do
               end do
               icount=0
               do irow=1,nxrow
                 ig=nobgnm(irow)
                 if(ig.eq.iobsgp)then
                   icount=icount+1
                   do ies=1,nespar
                     do ic=1,nc
                       u(ic,ies)=u(ic,ies)+
     +                 covar(icov)%dval(ic,icount)*x(irow,ies)
                     end do
                   end do
                 end if
               end do
               icount=0
               do irow=1,nxrow
                 ig=nobgnm(irow)
                 if(ig.eq.iobsgp)then
                   icount=icount+1
                   do ies=1,nespar
                     x(irow,ies)=u(icount,ies)
                   end do
                 end if
               end do
             end if
           else
             do irow=1,nxrow
               ig=nobgnm(irow)
               if(ig.eq.iobsgp)then
                 weight=sqrt(owght(irow))
                 do ies=1,nespar
                   x(irow,ies)=x(irow,ies)*weight
                 end do
               end if
             end do
           end if
         end do
         deallocate(u,stat=ierr)
         write(6,587)
587      format(' - inverse roots of covariance matrices computed.')
       end if

       if(am.eq.'y')then
         call addquote(matfile,cfile)
         write(6,621) trim(cfile)
621      format(/,' - writing Q(1/2) matrix to file ',a,'...')
         open(unit=30,file=matfile,action='write',iostat=ierr)
         if(ierr.ne.0)then
           write(errmsg,95) trim(cfile)
95         format('Cannot open file ',a,' for output.')
           go to 9890
         end if
         write(30,622) nxrow,nespar,2
622      format(3i10)
         do irow=1,nxrow
           write(30,623) (x(irow,ies),ies=1,nespar)
623        format(8(1x,1pg14.7))
         end do
         write(30,624)
624      format('* row names')
         do irow=1,nxrow
           write(30,625) trim(aobs(irow))
625        format(1x,a)
         end do
         write(30,626)
626      format('* column names')
         do ipp=1,npar
           if(itrans(ipp).ge.0)then
             write(30,625) trim(apar(ipp))
           end if
         end do
         close(unit=30)
         write(6,627) trim(matfile)
627      format(' - file ',a,' written ok.')
       end if

C -- Singular value decomposition of Q(1/2)X is now undertaken.

       allocate(vt(nespar,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       write(6,609)
609    format(/,' - undertaking SVD of Q(1/2)X...')
       call dgesvd('N','A',nxrow,nespar,x,nxrow,s,u,1,vt,nespar,
     + work,lwork,info)
       if(info.ne.0)then
         write(errmsg,610)
610      format('Cannot undertake singular value decomposition of ',
     +   'Q(1/2)X matrix.')
         go to 9890
       end if
       write(6,611)
611    format(' - SVD completed ok.')

C -- Parameter value files are now read in and new ones written.

       allocate(apresent(npar),pval1(npar),scale1(npar),offset1(npar),
     + dval(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       nulldim=nespar-soldim
       allocate(v2tp(nulldim),stat=ierr)
       if(ierr.ne.0) go to 9200

       icount1=0
       icount2=0
       do ifile=1,100000
         call writint(anumfile,ifile)
         infile=trim(parbase1)//trim(anumfile)//trim(aext)
         open(unit=10,file=infile,status='old',iostat=ierr)
         if(ierr.ne.0)then
           if(icount1.eq.0)then
             if(ifile.gt.200)then
               write(errmsg,620) trim(parbase1),trim(aext)
620            format('Cannot find any parameter value files ',
     +         'named "',a,'*',a,'".')
               go to 9890
             else
               cycle
             end if
           else if(icount2.gt.100) then
             go to 1000
           else
             if(icount1.ne.0) icount2=icount2+1
             cycle
           end if
         end if
         call addquote(infile,afile)
         write(6,6251) trim(afile)
6251     format(/,' - reading parameter value file ',a,'...')
         icount2=0
         icount1=icount1+1
         iline=1
         read(10,'(a)',err=9100,end=9150) cline
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) then
           write(errmsg,630) trim(afile)
630        format('First line of parameter value file ',a,
     +     ' should read "single" or "double" followed by ',
     +     '"point" or "nopoint".')
           go to 9890
         end if
         firstline=cline
         call lowcas(firstline)
         if((firstline(lw(1):rw(1)).ne.'single').and.
     +      (firstline(lw(1):rw(1)).ne.'double'))then
            write(errmsg,630) trim(afile)
            go to 9890
          end if
         if((firstline(lw(2):rw(2)).ne.'point').and.
     +      (firstline(lw(2):rw(2)).ne.'nopoint'))then
            write(errmsg,630) trim(afile)
            go to 9890
          end if
         apresent=0          ! an array
         icurrent=1
         do
631        iline=iline+1
           read(10,'(a)',err=9100,end=680) cline
           if(cline.eq.' ') go to 631
           call linspl(ifail,4,lw,rw,cline)
           if(ifail.ne.0)then
             call writint(aline,iline)
             write(errmsg,640) trim(aline),trim(afile)
640          format('Insufficient entries on line ',a,
     +       ' of parameter value file ',a,'.')
             go to 9890
           end if
           aapar=cline(lw(1):rw(1))
           call lowcas(aapar)
           call which1(ifail,npar,icurrent,apar,aapar)
           if(ifail.ne.0) cycle
           call drealrd(ifail,cline(lw(2):rw(2)),pval1(icurrent))
           if(ifail.ne.0) go to 9100
           apresent(icurrent)=1
           call drealrd(ifail,cline(lw(3):rw(3)),scale1(icurrent))
           if(ifail.ne.0) go to 9100
           call drealrd(ifail,cline(lw(4):rw(4)),offset1(icurrent))
           if(ifail.ne.0) go to 9100
         end do
680      continue
         do ipp=1,npar
           if(apresent(ipp).eq.0)then
             call addquote(pestfile,bfile)
             write(errmsg,690) trim(apar(ipp)),trim(bfile),trim(afile)
690          format('A value for parameter "',a,'" cited in PEST ',
     +       'control file ',a,' does not appear in parameter ',
     +       'value file ',a,'.')
             go to 9890
           end if
           if(itrans(ipp).eq.-1000000)then
             if(.not.isclose(pval(ipp),pval1(ipp)))then
                ifixedwarn=1
c               call addquote(pestfile,bfile)
c               write(errmsg,589) trim(bfile),trim(apar(ipp)),
c     +         trim(afile)
c589            format('According to PEST control file ',a,
c     +         ', parameter "',a,'" is fixed. However its value ',
c     +         'is different in parameter value file ',a,'.')
c               go to 9890
             end if
           else if(itrans(ipp).lt.0)then
             itied=-itrans(ipp)
             ratio=pval(ipp)/pval(itied)
             ratio1=pval1(ipp)/pval1(itied)
             if(.not.isclose(ratio,ratio1))then
               call addquote(pestfile,bfile)
               write(errmsg,691) trim(apar(ipp)),trim(apar(itied)),
     +         trim(bfile),trim(afile)
691            format('Parameter "',a,'" is tied to parameter "',
     +         a,'" in PEST control file ',a,'. However these ',
     +         'parameters have a different ratio in parameter ',
     +         'value file ',a,'.')
               go to 9890
             end if
           end if
         end do

         close(unit=10)

         write(6,700)
700      format(' - projecting parameters onto null space...')

C -- First parameter values are subtracted from calibrated values.

         ies=0
         do ipp=1,npar
           if(itrans(ipp).eq.0)then
             ies=ies+1
             dval(ies)=pval1(ipp)-pval(ipp)
           else if(itrans(ipp).eq.1)then
             ies=ies+1
             dval(ies)=log10(pval1(ipp)/pval(ipp))
           end if
         end do

C -- V2tp is now computed.

         i=0
         do idim=soldim+1,nespar
           i=i+1
           sum=0.0d0
           do ies=1,nespar
             sum=sum+vt(idim,ies)*dval(ies)
           end do
           v2tp(i)=sum
         end do

C -- V2V2tp is now computed.

         do ies=1,nespar
           sum=0.0d0
           i=0
           do idim=soldim+1,nespar
             i=i+1
             sum =sum+vt(idim,ies)*v2tp(i)
           end do
           dval(ies)=sum
         end do

C -- The differencing vector is shortened if parameters transgress bounds.

         dtempmax=0.0d0
         ies=0
         do ipp=1,npar
           if(itrans(ipp).ge.0)then
             ies=ies+1
             if(itrans(ipp).eq.0)then
               pval1(ipp)=pval(ipp)+dval(ies)
             else
               pval1(ipp)=10**(log10(pval(ipp))+dval(ies))
             end if
             if((pval1(ipp).le.parubnd(ipp)).and.
     +          (pval1(ipp).ge.parlbnd(ipp)))cycle
             utemp=parubnd(ipp)
             ltemp=parlbnd(ipp)
             mtemp=pval(ipp)
             if(itrans(ipp).eq.1)then
               utemp=log10(utemp)
               ltemp=log10(ltemp)
               mtemp=log10(mtemp)
             end if
             if(dval(ies).gt.0.0d0)then
               dtemp=utemp-mtemp
             else
               dtemp=mtemp-ltemp
             end if
             if(dtemp.eq.0.0d0)then
               dtemp=1.0d301
             else
               dtemp=abs(dval(ies))/dtemp
             end if
             if(dtemp.gt.dtempmax)dtempmax=dtemp
           end if
         end do

C -- So do we limit the whole vector - or just offending parameters? If the factor is less
C    than 2 we limit the whole vector. Otherwise we just impose bounds on individual parameters shortly.

         if(dtempmax.gt.1.0d0)then
           if(dtempmax.le.2.0d0)then
             dtempmax=1.0d0/dtempmax
             do ies=1,nespar
               dval(ies)=dval(ies)*dtempmax
             end do
           end if
         end if

C -- New parameter values are formed.

         ies=0
         do ipp=1,npar
           if(itrans(ipp).ge.0) then
             ies=ies+1
             if(itrans(ipp).eq.0)then
               pval1(ipp)=pval(ipp)+dval(ies)
             else
               pval1(ipp)=10**(log10(pval(ipp))+dval(ies))
             end if
             if(pval1(ipp).gt.parubnd(ipp)) pval1(ipp)=parubnd(ipp)
             if(pval1(ipp).lt.parlbnd(ipp)) pval1(ipp)=parlbnd(ipp)
           else if(itrans(ipp).eq.-1000000)then
c             pval1(ipp)=pval(ipp)
           end if
         end do
         iwarn=0
         do ipp=1,npar
           if((itrans(ipp).lt.0).and.(itrans(ipp).gt.-1000000))then
             itied=-itrans(ipp)
             pval1(ipp)=pval1(itied)*pval(ipp)/pval(itied)
             if(at.eq.'y')then
               if(pval1(ipp).gt.parubnd(ipp))then
                 pval1(ipp)=parubnd(ipp)
                 iwarn=1
               else if(pval1(ipp).lt.parlbnd(ipp))then
                 pval1(ipp)=parlbnd(ipp)
                 iwarn=1
               end if
             end if
           end if
         end do

C -- The new parameter value file is written.

         outfile=trim(parbase2)//trim(anumfile)//trim(aext)
         call addquote(outfile,afile)
         write(6,710) trim(afile)
710      format(' - writing new parameter value file ',a,'...')
         open(unit=20,file=outfile,action='write',iostat=ierr)
         if(ierr.ne.0)then
           write(errmsg,730) trim(afile)
730        format('Cannot open file ',a,' for output.')
           go to 9890
         end if
         write(20,'(a)') trim(firstline)
         do ipp=1,npar
           write(20,740) trim(apar(ipp)),pval1(ipp),scale1(ipp),
     +     offset1(ipp)
740        format(1x,a,t16,3(1pg14.7,3x))
         end do
         close(unit=20)

       end do
1000   continue

       if(iwarn.ne.0)then
         write(errmsg,1010)
1010     format('Warning: The value of at least one tied parameter ',
     +   'was altered so as to respect parameter bounds. This may ',
     +   'effect the integrity of null space projections.')
         errmsg=' '//trim(errmsg)
         call writmess(6,errmsg)
       end if
       if(ifixedwarn.ne.0)then
         write(errmsg,1020)
1020     format(' Warning: the value of at least one fixed parameter ',
     +   'in a parameter value file was not the same as that in the ',
     +   'PEST control file. The value was altered to match that ',
     +   'in the parameter file.')
         call writmess(6,errmsg)
       end if


       go to 9900

9000   write(errmsg,9010) trim(afile)
9010   format('Error encountered in reading Jacobian matrix ',
     + 'file ',a,'.')
       go to 9890
9050   call addquote(pestfile,bfile)
       write(errmsg,9060) trim(afile),trim(bfile)
9060   format('Jacobian matrix file ',a,' is not compatible ',
     + 'with PEST control file ',a,'; at least one observation ',
     + 'or adjustable parameter is different between them.')
       go to 9890
9100   call writint(aline,iline)
       write(errmsg,9110) trim(aline),trim(afile)
9110   format('Error reading line ',a,' of parameter value file ',
     + a,'.')
       go to 9890
9150   write(errmsg,9160) trim(afile)
9160   format('Unexpected end encountered to parameter value file ',
     + a,'.')
       go to 9890

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890



9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)


9900   continue
       call pest_data_deallocate(ifail)
       deallocate(apresent,u,vt,s,work,pval1,dval,scale1,offset1,v2tp,
     + stat=ierr)


       end

       logical function isclose(rval1,rval2)

       implicit none
       double precision rval1,rval2

       if(rval1.eq.0.0d0)then
         if(rval2.eq.0.0d0) then
           isclose=.true.
         else
           isclose=.false.
         end if
         return
       else if(rval2.eq.0.0d0)then
         if(rval1.eq.0.0d0)then
           isclose=.true.
         else
           isclose=.false.
         end if
         return
       else if(abs(rval2-rval1).gt.1.0d-4*abs(rval1))then
         isclose=.false.
         return
       end if
       isclose=.true.
       return

       end



        subroutine getfile(ifail,cline,filename,ibeg,iend)

C-- Subroutine getfile extracts a filename from a string.

C -- Arguments are as follows:-
C       ifail: returned as zero if filename successfully read
C       cline: a character string containing the file name
C       filename: the name of the file read from the string
C       ibeg: character position at which to begin search for filename
C       iend: on input  - character position at which to end search for filename
C             on output - character postion at which filename ends


        integer, intent(out)               :: ifail
        integer, intent(in)                :: ibeg
        integer, intent(inout)             :: iend
        character (len=*), intent(in)      :: cline
        character (len=*), intent(out)     :: filename

        integer                            :: i,j,k
        character (len=1)                  :: aa

        ifail=0
        do i=ibeg,iend
          aa=cline(i:i)
          if((aa.ne.' ').and.(aa.ne.',').and.(aa.ne.char(9)))go to 50
        end do
        ifail=1
        return

50      if((aa.eq.'"').or.(aa.eq.''''))then
          do j=i+1,iend
            if(cline(j:j).eq.aa) go to 60
          end do
          ifail=1
          return
60        iend=j
          if(i+1.gt.j-1)then
            ifail=1
            return
          else
            filename=cline(i+1:j-1)
          end if
        else
          do j=i+1,iend
            if((cline(j:j).eq.' ').or.(cline(j:j).eq.',').or.
     +         (cline(j:j).eq.char(9)))then
              k=j-1
              go to 100
            end if
          end do
          k=iend
100       filename=cline(i:k)
          if(cline(k:k).eq.'"')then
            ifail=1
            return
          else if(cline(k:k).eq.'''')then
            ifail=1
            return
          end if

          iend=k
        end if
        filename=adjustl(filename)
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




        subroutine intread(ifail,cline,itemp)

C -- Subroutine INTREAD reads a real number from a string.

        integer ifail
        integer itemp
        character*6 afmt
        character*(*) cline

        ifail=0
        if(cline.eq.' ') go to 100
        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(cline)
        read(cline,afmt,err=100) itemp
        return

100     ifail=1
        return
        end




        SUBROUTINE DREALREAD(IFAIL,CLINE,RTEMP)

C -- Subroutine DREALREAD reads a real number from a string.

        INTEGER IFAIL
        double precision RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        IF(CLINE.EQ.' ') GO TO 100
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
        READ(CLINE,AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END
