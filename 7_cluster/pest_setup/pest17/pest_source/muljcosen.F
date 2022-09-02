       program muljcosen

C -- Program MULJCOSEN reads multiple JCO files for multiple iterations. It collects
C    the composite sensitivity of a parameter or observation into a single tabular
C    data file.

       use pestdata, only  : maxcompdim,pestmode,nobsgp,nxrow,
     +                       errmsg,obgnme,aobs,numcov,
     +                       covind,owght,x,pval_o,modval_o,
     +                       parchglim,pval,parlbnd,parubnd,
     +                       ipargp,scale,offset,dercom,covgp,nespar,
     +                       nobs,nobgnm,covar,nobscov,apar,itrans,
     +                       npar,svdmode,svda,numlam,rlambda1,nprior,
     +                       lsqrmode

       implicit none

       logical          :: lexist
       integer          :: n,n1,irestart,ifail,ierr,i,info,k,kjco,j,bn,
     +                     ir,jr
       integer          :: itemp,itemp1,itemp2,icount,iicount
       integer          :: irow,icol,krow,lwork,nrow,ncol
       integer          :: ipar,ies,maxnobscov,icov,kpar
       integer          :: p1start,pfile
       integer          :: lw(10),rw(10)
       double precision :: dtemp,compsens
       double precision :: u(1,1),vt(1,1)
       character*5      :: aext
       character*10     :: aobspar,anum
       character*10     :: aversion
       character*12     :: aapar
       character*20     :: aaobs
       character*25     :: aname
       character*200    :: casename,pestfile,apestfile,outfile,
     +                     jcofile,afile
       character*300    :: dline

       integer, allocatable :: irgp(:)
       double precision, allocatable :: tempcov(:,:),workvec(:),s(:)

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' MULJCOSEN Version ',a,'. Watermark Numerical ',
     + 'Computing.')

#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) dline
#else
       call pgetcl(dline)
#endif

C -- The casename is read from the MULJCOSEN command line.

       if(dline.eq.' ') go to 8900
       call spacesub(dline)
       call linspl(ifail,5,lw,rw,dline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,4,lw,rw,dline)
       if(ifail.ne.0) go to 8900
       casename=dline(lw(1):rw(1))
       call remchar(casename,char(211))
       n=len_trim(casename)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=casename(n1:)
       call lowcas(aext)
       if(aext.eq.'.pst ')casename(n1:n)=' '
       if(casename.eq.' ') go to 8900
#ifndef UNIX
       call lowcas(casename)
#endif
       aobspar=dline(lw(2):rw(2))
       call lowcas(aobspar)
       if((index(aobspar,'obs').eq.0).and.
     +    (index(aobspar,'par').eq.0)) go to 8900
       aname=dline(lw(3):rw(3))
       call lowcas(aname)
       call remchar(aname,char(211))
       aname=adjustl(aname)
       outfile=dline(lw(4):rw(4))
       call remchar(outfile,char(211))
#ifndef UNIX
       call lowcas(outfile)
#endif

       iicount=0

C -- The PEST control file is opened.

       pestfile=trim(casename)//'.pst'
       call addquote(pestfile,apestfile)
       inquire(file=pestfile,exist=lexist)
       if(.not.lexist)then
         write(errmsg,11) trim(apestfile)
11       format('Cannot find PEST control file ',a,'.')
         go to 9890
       end if

C -- The PEST control file is now read.

       write(6,*)
       write(6,235) trim(apestfile)
235    format(' - reading PEST control file ',a,'....')
       irestart=0
       p1start=0
       pfile=0
       call read_pest_data(ifail,irestart,p1start,pfile,pestfile)
       if(ifail.ne.0) go to 9890
       if(maxcompdim.gt.1)then
         write(errmsg,240) trim(apestfile)
240      format('MULJCOSEN requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if(nespar.eq.0)then
         write(errmsg,272) trim(apestfile)
272      format('There are no adjustable parameters cited in PEST ',
     +   'control file ',a,'.')
         go to 9890
       end if
       if(nxrow.eq.0)then
         write(errmsg,273) trim(apestfile)
273      format('No observations or prior information items are ',
     +   'cited in PEST control file ',a,'.')
         go to 9890
       end if
       write(6,275) trim(apestfile)
275    format(' - PEST control file ',a,' read ok.')

C -- We can delete some unused PEST memory.

       deallocate(pval_o,modval_o,parchglim,pval,parlbnd,parubnd,
     + ipargp,scale,offset,dercom,stat=ierr)

C -- We check that the nominated parameter or observation features in
C    PEST control file.

       if(index(aobspar,'obs').ne.0)then
         do irow=1,nxrow
           if(aname.eq.aobs(irow)) go to 283
         end do
         write(errmsg,277) trim(aname),trim(apestfile)
277      format('Observation name "',a,'" is not featured in PEST ',
     +   'control file ',a,'.')
         go to 9890
283      continue
         krow=irow
         if(pestmode.eq.3)then
           i=nobgnm(krow)
           if(obgnme(i)(1:5).eq.'regul')then
             write(errmsg,284) trim(aname),trim(apestfile)
284          format('Observation "',a,'" cited on MULJCOSEN ',
     +       'command line belongs to a regularisation group in PEST ',
     +       'control file ',a,'. This is not allowed.')
             go to 9890
           end if
         end if
         if(owght(krow).le.0.0d0)then
           write(errmsg,285) trim(aname),trim(apestfile)
285        format('Observation "',a,'" cited on MULJCOSEN '
     +     'command line has a weight of zero in PEST control file ',
     +     a,'. This is not allowed.')
           go to 9890
         end if
       else
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0) ies=ies+1
           if(aname.eq.apar(ipar)) then
             go to 287
           end if
         end do
         write(errmsg,286) trim(aname),trim(apestfile)
286      format(' Parameter name "',a,'" is not featured in PEST ',
     +   'control file ',a,'.')
         go to 9890
287      continue
         krow=ies
         kpar=ipar
         if(itrans(kpar).lt.0)then
           write(errmsg,288) trim(aname),trim(pestfile)
288        format(' Parameter "',a,'" is not adjustable in PEST ',
     +     'control file ',a,'.')
           go to 9890
         end if
       end if

C -- Fill the IRGP array.

       allocate(irgp(nobsgp),stat=ierr)
       if(ierr.ne.0) go to 9200
       irgp=0
       if(pestmode.eq.3)then
         do i=1,nobsgp
           if(obgnme(i)(1:5).eq.'regul') irgp(i)=1
         end do
       end if

C -- The MULJCOSEN output file is opened.

       open(unit=20,file=outfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(outfile,afile)
         write(errmsg,40) trim(afile)
40       format('Cannot open file ',a,' for output.')
         go to 9890
       end if
       if(index(aobspar,'obs').ne.0)then
         write(20,42) trim(aobs(krow))
42       format(1x,'Iteration_index',t30,
     +   'Composite_observation_sensitivity_',a)
       else
         write(20,43) trim(apar(kpar))
43       format(1x,'Iteration_index',t30,
     +   'Composite_parameter_sensitivity_',a)
       end if

C -- Zero weights are assigned to regularisation observations and to the
C    the prediction (in predictive analysis mode).

       if(pestmode.eq.3)then
         do irow=1,nxrow
           i=nobgnm(irow)
           if(irgp(i).ne.0)then
             owght(irow)=0.0d0
           end if
         end do
       else if (pestmode.eq.2)then
         do irow=1,nxrow
           i=nobgnm(irow)
           if(obgnme(i).eq.'predict')then
             if(irow.ne.krow)then
               owght(irow)=0.0d0
             end if
             go to 44
           end if
         end do
       end if
44     continue

C -- If any covariance matrices are supplied, C**(-1/2) is calculated.

C -- First we find the biggest covariance matrix and dimension a work matrix appropriately.

       maxnobscov=0
       lwork=1
       if(numcov.ne.0)then
         do icov=1,numcov
           i=covgp(icov)
           if(irgp(i).ne.0) cycle
           if(nobscov(icov).gt.maxnobscov)then
             maxnobscov=nobscov(icov)
           end if
         end do
         lwork=8*maxnobscov
       end if
       allocate(workvec(lwork),stat=ierr)
       if(ierr.ne.0) go to 9200

       if(maxnobscov.eq.0) go to 450
       itemp=max(maxnobscov,nespar)
       allocate(tempcov(maxnobscov,itemp),s(maxnobscov),
     + stat=ierr)
       if(ierr.ne.0) go to 9200

C -- Now SVD is undertaken and the inverse square root of each matrix is calculated.

       do icov=1,numcov
         i=covgp(icov)
         if(irgp(i).ne.0) cycle
         nrow=nobscov(icov)
         if(nrow.eq.0) cycle
         ncol=nrow
         do icol=1,ncol
           do irow=1,nrow
             tempcov(irow,icol)=covar(icov)%dval(irow,icol)
           end do
         end do
         call dgesvd('O','N',nrow,ncol,tempcov,maxnobscov,s,u,
     +   1,vt,1,workvec,lwork,info)
         if(info.ne.0)then
           call writint(anum,icov)
           write(errmsg,310) trim(anum),trim(apestfile)
310        format('Singular value decomposition process did not ',
     +     'converge for covariance matrix number ',a,' cited in ',
     +     'PEST control file ',a,'.')
           go to 9890
         end if
         do irow=1,nrow
           if(s(irow).le.1.0d-15)then
             call writint(anum,icov)
             write(errmsg,320) trim(anum),trim(apestfile)
320          format('Covariance matrix ',a,' supplied in PEST control ',
     +       'file ',a,' is singular.')
             go to 9890
           else
             s(irow)=sqrt(1.0d0/s(irow))
           end if
         end do
         do icol=1,ncol
           do irow=1,icol
             dtemp=0.0d0
             do k=1,ncol
               dtemp=dtemp+tempcov(irow,k)*s(k)*tempcov(icol,k)
             end do
             covar(icov)%dval(irow,icol)=dtemp
           end do
         end do
         do icol=1,ncol-1
           do irow=icol+1,ncol
             covar(icov)%dval(irow,icol)=covar(icov)%dval(icol,irow)
           end do
         end do
       end do
450    continue

C -- Here we begin a big loop in which Jacobian matrix files are read.

       kjco=0
276    continue
       kjco=kjco+1
       if(kjco.gt.100) go to 500
       call writint(anum,kjco)

C -- The JCO file is opened.

       jcofile=trim(casename)//'.jco'//'.'//trim(anum)
       call open_unformatted_file(ifail,11,'read',jcofile,errmsg)
       if(ifail.ne.0) go to 276

C -- The Jacobian matrix file is now read.

       call addquote(jcofile,afile)
       write(6,279) trim(afile)
279    format(/,' - reading Jacobian matrix file ',a,'....')
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
         call lowcas(aapar)
         if(aapar.ne.apar(i)) go to 9050
       end do
       do i=1,bn
         read(11,err=9000,end=9000) aaobs
         call lowcas(aaobs)
         if(aaobs.ne.aobs(i)) go to 9050
       end do
       close(unit=11)
       write(6,300) trim(afile)
300    format(' - Jacobian matrix file ',a,' read ok.')

C -- The rows of the matrix are now multiplied by respective weights.

       if(numcov.eq.0)then
         if(index(aobspar,'obs').eq.0)then
           itemp1=1
           itemp2=nxrow
         else
           itemp1=krow
           itemp2=krow
         end if
         do irow=itemp1,itemp2
           dtemp=sqrt(owght(irow))
           do ies=1,nespar
             x(irow,ies)=x(irow,ies)*dtemp
           end do
         end do
       else                                               !xxxx
         if(index(aobspar,'obs').ne.0)then
           i=nobgnm(krow)
           do j=1,numcov
             if(covgp(j).eq.i) go to 319
           end do
           dtemp=sqrt(owght(krow))
           do ies=1,nespar
             x(irow,ies)=x(irow,ies)*dtemp
           end do
           go to 361
         end if
319      continue
         do irow=1,nxrow
           i=nobgnm(irow)
           do j=1,numcov
             if(covgp(j).eq.i) go to 321
           end do
           dtemp=sqrt(owght(irow))
           do ies=1,nespar
             x(irow,ies)=x(irow,ies)*dtemp
           end do
321        continue
         end do
         do icov=1,numcov
           i=covgp(icov)
           if(irgp(i).ne.0) cycle
           ir=0
           do irow=1,nxrow
             j=nobgnm(irow)
             if(j.eq.i)then
               ir=ir+1
               do ies=1,nespar
                 tempcov(ir,ies)=x(irow,ies)
               end do
               if(ir.eq.nobscov(icov)) go to 350
             end if
           end do
350        continue
           ir=0
           do irow=1,nxrow
             j=nobgnm(irow)
             if(j.eq.i)then
               ir=ir+1
               do ies=1,nespar
                 dtemp=0.0d0
                 do jr=1,nobscov(icov)
                   dtemp=dtemp+covar(icov)%dval(ir,jr)*tempcov(jr,ies)
                 end do
                 x(irow,ies)=dtemp
               end do
               if(ir.eq.nobscov(icov)) go to 360
             end if
           end do
360        continue
         end do
       end if
361    continue

C -- The composite sensitivity is now calculated.

       if(index(aobspar,'obs').ne.0)then
         compsens=0.0d0
         do ies=1,nespar
           compsens=compsens+x(krow,ies)*x(krow,ies)
         end do
         compsens=sqrt(compsens)/nespar
       else
         if(numcov.ne.0)then         ! Check this - just added.
           do i=1,numcov
             j=covgp(i)
             if(irgp(j).eq.0)then
               do irow=1,nxrow
                 if(nobgnm(irow).eq.j) owght(irow)=1.0d0
               end do
             end if
           end do
         end if
         compsens=0.0d0
         icount=0
         do irow=1,nxrow
           if(owght(irow).ne.0.0d0)then
             icount=icount+1
             compsens=compsens+x(irow,krow)*x(irow,krow)
           endif
         end do
         if(icount.eq.0)then
           write(errmsg,370) trim(apestfile)
370        format('File ',a,' contains no non-regularisation, ',
     +     'non-prediction observations with non-zero weights.')
           go to 9890
         end if
         compsens=sqrt(compsens)/icount
       end if

       write(20,390) kjco,compsens
390    format(5x,i5,t40,1pg14.7)
       iicount=iicount+1

       go to 276


500    continue

C -- Close the output file.

       close(unit=20)
       if(iicount.ne.0)then
         write(6,*)
         write(6,510) trim(outfile)
510      format(' - file ',a,' written ok.')
       else
         write(errmsg,511) trim(casename)
511      format('No Jacobian matrices named according to "',a,'.jco.N',
     +   '" protocol were found.')
         go to 9890
       end if

       go to 9900

8900   continue

       write(6,8910)
8910   format(/,' MULJCOSEN is run using the command:',/)
       write(6,8920)
8920   format('     muljcosen casename obspar aname outfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8935)
8935   format('     casename is a PEST control file basename,')
       write(6,8950)
8950   format('     obspar   is either "obs" or "par",')
       write(6,8955)
8955   format('     aname    is the name of an observation or ',
     + 'parameter, and')
       write(6,8980)
8980   format('     outfile  is the MULJCOSEN output file.')

       go to 9999


9000   write(errmsg,9010) trim(afile)
9010   format('Error encountered in reading Jacobian matrix ',
     + 'file ',a,'.')
       go to 9890
9050   continue
       write(errmsg,9060) trim(afile),trim(apestfile)
9060   format('Jacobian matrix file ',a,' is not compatible ',
     + 'with PEST control file ',a,'; at least one observation ',
     + 'or adjustable parameter is different between them.')
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
       deallocate(irgp,tempcov,workvec,s,stat=ierr)

       close(unit=10,iostat=ierr)
       close(unit=20,iostat=ierr)

9999   continue
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




        SUBROUTINE DREALREAD(IFAIL,CLINE,RTEMP)

C -- Subroutine DREALREAD reads a real number from a string.

        INTEGER IFAIL
        double precision RTEMP
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
        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(cline)
        read(cline,afmt,err=100) itemp
        return

100     ifail=1
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

