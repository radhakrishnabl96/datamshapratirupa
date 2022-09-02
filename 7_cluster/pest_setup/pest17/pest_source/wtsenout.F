       program wtsenout

C -- Program WTSENOUT calculates and stores Q(1/2)X and Q(1/2)o where
C    Q is the weight matrix, X is the sensitivity matrix and o is the model
C    output vector.

       use pestdata, only  : maxcompdim,pestmode,npar,nobsgp,nxrow,
     +                       errmsg,apar,obgnme,aobs,itrans,numcov,
     +                       covind,owght,x,pval_o,modval_o,
     +                       parchglim,pval,parlbnd,parubnd,
     +                       ipargp,scale,offset,dercom,covgp,nespar,
     +                       nobs,nobgnm,covar,nobscov,oval,cline

       implicit none

       logical lexist
       integer lwork,ibeg,iend,ifail,nb_pst,ierr,irestart,
     + itemp,irow,itemp1,itemp2,bn,icount,i,j,ies,
     + icov,info,k,nb,igroup,ne,ipp
       integer maxobs
       integer p1start,pfile
       integer lw(5),rw(5)
       double precision dtemp,rtemp
       double precision vt(1,1)
       double precision, allocatable :: workvec(:),w1(:)
       character*4 aext
       character*10 anum
       character*12 aversion
       character*12 aapar,atemp
       character*20 aaobs
       character*200 afile,bfile,pestfile,jcofile,outfile1,outfile2,
     + resfile

       integer ldvt,iicount,icovrow
       integer, allocatable :: iindex(:)
       double precision, allocatable :: u(:,:),xx(:,:)
       character*15 acov

C -- Initialization


#ifdef NO_CMDLINE
        write(*,*)
1       write(*,'(A)') ' ENTER COMMAND LINE ARGUMENTS > '
        read(*,'(A)',err=1) cline
#else
        call pgetcl (cline)
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' WTSENOUT Version ',a,'. Watermark Numerical ',
     + 'Computing.')

       call spacesub(cline)
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 9400
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 9400
       pestfile=cline(lw(1):rw(1))
       outfile2=cline(lw(2):rw(2))
       outfile1=cline(lw(3):rw(3))
       call remchar(pestfile,char(211))
       call remchar(outfile2,char(211))
       call remchar(outfile1,char(211))

#ifndef UNIX
       call lowcas(pestfile)
       call lowcas(outfile2)
       call lowcas(outfile1)
#endif

       nb_pst=len_trim(pestfile)
       nb_pst=nb_pst-3
       if(nb_pst.lt.1)nb_pst=1
       aext=pestfile(nb_pst:)
       call lowcas(aext)
       if(aext.ne.'.pst')then
         write(errmsg,40)
40       format('PEST control file must have an extension of ',
     +   '".pst".')
         go to 9890
       end if
       inquire(file=pestfile,exist=lexist)
       if(.not.lexist)then
         call addquote(pestfile,afile)
         write(errmsg,50) trim(afile)
50       format('Cannot find PEST control file ',a,'.')
         go to 9890
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

C -- A check is made that the corresponding RES file is present.

      resfile=pestfile(1:nb_pst)//'res'
      open(unit=12,file=resfile,status='old',iostat=ierr)
      if(ierr.ne.0)then
         call addquote(resfile,afile)
         call addquote(pestfile,bfile)
         write(errmsg,61) trim(afile),trim(bfile)
61       format('Cannot open residuals file ',a,
     +   ' corresponding to PEST control file ',a,'.')
         go to 9890
       end if

C -- The names of the output files are now acquired.

       open(unit=21,file=outfile2,action='write',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(outfile2,afile)
         write(errmsg,130) trim(afile)
130      format('Cannot open file ',a,' for output.')
         go to 9890
       end if

       nb=len_trim(outfile1)
       ne=nb-3
       if(ne.lt.1)ne=1
       aext=outfile1(ne:nb)
       call lowcas(aext)
       if(aext.ne.'.jco')then
         write(errmsg,120)
120      format('Output JCO file must have an extension of ".jco".')
         go to 9890
       end if
       if(outfile1.eq.jcofile)then
         write(errmsg,121)
121      format('Output JCO file must not have same name as JCO ',
     +   'file corresponding to existing PEST control file.')
         go to 9890
       end if
       call open_unformatted_file(ifail,20,'write',outfile1,errmsg)
       if(ifail.ne.0)then
         call addquote(outfile1,afile)
         write(errmsg,131) trim(afile)
131      format('Cannot open unformatted file ',a,' for output.')
         go to 9890
       end if

C -- All input data has now been acquired. The complete PEST input
C    dataset is now read.

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
240      format('WTSENOUT requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.ne.1)then
         write(errmsg,250) trim(afile)
250      format('WTSENOUT requires that PEST be run in ',
     +   'parameter estimation mode in PEST control file ',a,
     +   '. If necessary, create such a file with optimised ',
     +   'parameters using PARREP and use JCO2JCO to ',
     +   'build a JCO matrix for this file. Then set NOPTMAX to ',
     +   '-1 or -2 and run pest with the "/i" switch to create ',
     +   'an output dataset without the necessity to re-calculate ',
     +   'the Jacobian matrix.')
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
       write(6,275) trim(afile)
275    format(' - file ',a,' read ok.')

C -- We can delete some unused PEST memory.

       deallocate(pval_o,parchglim,pval,parlbnd,parubnd,
     + ipargp,scale,offset,dercom,stat=ierr)

C -- The Jacobian matrix file is now read.

       call addquote(jcofile,afile)
       write(6,276) trim(afile)
276    format(/,' - reading Jacobian matrix file ',a,'....')
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
         do j=1,bn
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
       write(6,275) trim(afile)

C -- Model output values are now read from the residuals file.

       call addquote(resfile,afile)
       write(6,640) trim(afile)
640    format(/,' - reading residuals file ',a,'...')
       read(12,'(a)',err=9300,end=9350) cline
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0)then
         write(errmsg,650) trim(afile)
650      format('Column header expected on first line of ',
     +   'residuals file ',a,'.')
         go to 9890
       end if
       atemp=cline(lw(4):rw(4))
       call lowcas(atemp)
       if(atemp.ne.'modelled')then
         write(errmsg,660) trim(afile)
660      format('Fourth column header on first line of file ',a,
     +   ' expected to be "Modelled".')
         go to 9890
       end if
       do irow=1,nxrow
         read(12,*,err=9320,end=9350) aaobs,atemp,atemp,oval(irow)
         call lowcas(aaobs)
         if(aaobs.ne.aobs(irow)) then
           call addquote(pestfile,bfile)
           write(errmsg,661) trim(afile),trim(bfile)
661        format('Residuals file ',a,' does not appear to be ',
     +     'compatible with PEST control file ',a,'.')
           go to 9890
         end if
       end do
       close(unit=12)
       write(6,670) trim(afile)
670    format(' - file ',a,' read ok.')

C --   If any covariance matrices were in the original PEST dataset these
C      are now inverted and the square root of them found.

       maxobs=0
       if(numcov.ne.0)then
         do icov=1,numcov
           if(nobscov(icov).gt.maxobs) maxobs=nobscov(icov)
         end do
       end if
       if(maxobs.eq.0) go to 584
       itemp=max(maxobs,nespar)
       allocate(w1(itemp),stat=ierr)
       if(ierr.ne.0) go to 9200
       if(numcov.ne.0)then
         lwork=8*maxobs
         allocate(workvec(lwork),stat=ierr)
         if(ierr.ne.0) go to 9200
         allocate(u(maxobs,maxobs),stat=ierr)
         if(ierr.ne.0) go to 9200
         write(6,581)
581      format(/,' - undertaking SVD on observation covariance ',
     +   'matrixes...')
         iicount=0
         ldvt=1
         do icov=1,numcov
           itemp=nobscov(icov)
           if(itemp.eq.0) cycle
           iicount=iicount+1
           call dgesvd('A','N',itemp,itemp,covar(icov)%dval,itemp,
     +     w1,u,maxobs,vt,ldvt,workvec,lwork,info)
           if(info.ne.0)then
             call writint(acov,icov)
             write(errmsg,587) trim(acov)
587          format('Cannot undertake SVD on covariance matrix ',
     +       'number ',a,' cited in PEST control file.')
             go to 9890
           end if
           do i=1,itemp
             if(w1(i).le.1.0d-200)then
               call writint(acov,icov)
               write(errmsg,582) trim(acov)
582            format('Observation covariance matrix number ',a,
     +         ' cited in PEST control file is singular. ',
     +         'WTSENOUT cannot proceed.')
               go to 9890
             end if
             w1(i)=1.0d0/(sqrt(w1(i)))
           end do
           do j=1,itemp
             do i=1,j
               rtemp=0.0d0
               do k=1,itemp
                 rtemp=rtemp+u(i,k)*u(j,k)*w1(k)
               end do
               covar(icov)%dval(i,j)=rtemp
             end do
           end do
           do j=1,itemp-1
             do i=j+1,itemp
               covar(icov)%dval(i,j)=covar(icov)%dval(j,i)
             end do
           end do
         end do
         call writint(anum,iicount)
         write(6,583) trim(anum)
583      format(' - ',a,' covariance matrices decomposed.')
         deallocate(u,stat=ierr)
       end if
584    continue

C -- A new matrix is allocated.

       allocate(xx(nxrow,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- This new matrix is now filled - first where there is no covariance matrix.


       write(6,579)
579    format(/,' - calculating weighted sensitivity matrix...')
       do irow=1,nxrow
         itemp=nobgnm(irow)
         if(covind(itemp)) cycle
         rtemp=sqrt(owght(irow))
         do ies=1,nespar
           xx(irow,ies)=x(irow,ies)*rtemp
         end do
       end do

C -- Now where there are observation covariance matrices.

       if(numcov.ne.0)then
         allocate(iindex(maxobs),stat=ierr)
         if(ierr.ne.0) go to 9200
         do icov=1,numcov
           if(nobscov(icov).eq.0) cycle
           igroup=covgp(icov)
           icovrow=0
           do irow=1,nxrow
             if(nobgnm(irow).eq.igroup)then
               icovrow=icovrow+1
               iindex(icovrow)=irow
               if(icovrow.eq.nobscov(icov)) go to 585
             end if
           end do
585        continue
           icovrow=0
           do irow=1,nxrow
             itemp=nobgnm(irow)
             if(nobgnm(irow).eq.igroup)then
               icovrow=icovrow+1
               do ies=1,nespar
                 rtemp=0.0d0
                 do k=1,nobscov(icov)
                   rtemp=rtemp+
     +                   covar(icov)%dval(icovrow,k)*x(iindex(k),ies)
                 end do
                 xx(irow,ies)=rtemp
               end do
               if(icovrow.eq.nobscov(icov)) go to 589
             end if
           end do
589        continue
         end do
       end if
       write(6,588)
588    format(' - weighted sensitivity matrix calculated ok.')

       deallocate(x,stat=ierr)

C -- The weighted Jacobian matrix is now stored.

       call addquote(outfile1,afile)
       write(6,610)
610    format(/,' - writing Jacobian matrix file ',a,'...')
       write(20) -nespar,-nxrow
       icount=0
       do i=1,nespar
         do j=1,nxrow
           if(xx(j,i).ne.0.0d0) icount=icount+1
         end do
       end do
       write(20) icount
       do i=1,nespar
         do j=1,nxrow
           if(xx(j,i).ne.0.0d0)then
             icount=(i-1)*nxrow+j
             write(20) icount,xx(j,i)
           end if
         end do
       end do
       do ipp=1,npar
         if((itrans(ipp).eq.0).or.(itrans(ipp).eq.1).or.
     +      (itrans(ipp).le.-1000001))then
            write(20) apar(ipp)
         end if
       end do
       do irow=1,nxrow
         write(20) aobs(irow)
       end do
       close(unit=20)
       write(6,620) trim(afile)
620    format(' - file ',a,' written ok.')

C -- Weighted model outputs are now formed and stored.

        do irow =1,nxrow
          igroup=nobgnm(irow)
          if(covind(igroup)) cycle
          modval_o(irow)=oval(irow)*sqrt(owght(irow))
        end do

       if(numcov.ne.0)then
         do icov=1,numcov
           if(nobscov(icov).eq.0) cycle
           igroup=covgp(icov)
           icovrow=0
           do irow=1,nxrow
             if(nobgnm(irow).eq.igroup)then
               icovrow=icovrow+1
               iindex(icovrow)=irow
               if(icovrow.eq.nobscov(icov)) go to 785
             end if
           end do
785        continue
           icovrow=0
           do irow=1,nxrow
             if(nobgnm(irow).eq.igroup)then
               icovrow=icovrow+1
               rtemp=0.0d0
               do k=1,nobscov(icov)
                 rtemp=rtemp+
     +                 covar(icov)%dval(icovrow,k)*oval(iindex(k))
               end do
               modval_o(irow)=rtemp
               if(icovrow.eq.nobscov(icov)) go to 789
             end if
           end do
789        continue
         end do
       end if

       call addquote(outfile2,afile)
       write(6,801) trim(afile)
801    format(/,' - writing weighted model output matrix file ',a,'...')
       write(21,800) nxrow,1,2
800    format(3i6)
       do irow=1,nxrow
         write(21,810) modval_o(irow)
810      format(1x,1pg14.7)
       end do
       write(21,820)
820    format('* row names')
       do irow=1,nxrow
         write(21,'(a)') trim(aobs(irow))
       end do
       write(21,830)
830    format('* column names')
       write(21,840)
840    format('col1')
       write(6,850) trim(afile)
850    format(' - file ',a,' written ok.')

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
9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890
9300   write(errmsg,9310) trim(afile)
9310   format('Error encountered in reading residuals file ',a,'.')
       go to 9890
9320   call writint(atemp,irow)
       write(errmsg,9330) trim(atemp),trim(afile)
9330   format('Error reading model output number ',a,' from ',
     + 'residuals file ',a,'.')
       go to 9890
9350   write(errmsg,9360) trim(afile)
9360   format('Unexpected end encountered to residuals file ',a,'.')
       go to 9890

9400    WRITE(6,9410)
9410    FORMAT(/,' WTSENOUT is run using the command:',/)
        WRITE(6,9420)
9420    FORMAT('     wtsenout pestfile matfile jcofile',/)
        WRITE(6,9430)
9430    FORMAT(' where',/)
        WRITE(6,9450)
9450    FORMAT('     pestfile is the name of a PEST ',
     +  'control file,')
        WRITE(6,9451)
9451    FORMAT('     matfile  is the name of the weighted ',
     +  'model matrix output file, and')
        WRITE(6,9452)
9452    FORMAT('     jcofile  is the name of the output weighted ',
     +  'sensitivity file.')
        GO TO 9999

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)


9900   continue
       call pest_data_deallocate(ifail)
       deallocate(workvec,w1,iindex,stat=ierr)

       deallocate(u,xx,iindex,stat=ierr)

       close(unit=10,iostat=ierr)
       close(unit=11,iostat=ierr)
       close(unit=12,iostat=ierr)
       close(unit=20,iostat=ierr)
       close(unit=21,iostat=ierr)

9999   continue

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



        SUBROUTINE INTRD(IFAIL,CLINE,ITEMP)

C -- Subroutine INTRD reads an integer from a string.

        INTEGER IFAIL
        INTEGER ITEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        if(cline.eq.' ') go to 100
        AFMT='(I   )'
        WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
        READ(CLINE,AFMT,ERR=100) ITEMP
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
