       program obscomp

! -- Program OBSCOMP uses sensitivities in a JCO file and residuals in an REC, RES or REI file
!    to explore the relationships between competing observations.

       use pestdata
       implicit none

       integer        :: nn,ierr,jj
       integer        :: maxnum,maxnum1,maxnum2
       integer        :: ipar,iobs,iiobs,jobs,ies
       integer        :: itemp1,itemp2,bn,i,j,icount,itemp
       integer        :: irestart,pfile,ifail
       integer        :: iobsgp,iobsgp_old
       integer        :: iline
       integer        :: lw(10),rw(10)

       double precision :: dtemp,ww,sum,minval,wweight

       character*1    :: aa
       character*10   :: how,aline
       character*12   :: aapar
       character*12   :: aversion
       character*15   :: atemp
       character*20   :: aaobs,aaaobs,atemp20
       character*256  :: pestfile,jcofile,outfile,outfile1,resfile,afile

       integer, allocatable :: obsindex(:)
       double precision, allocatable :: obscmp(:)

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,50) trim(aversion)
50     format(/,' OBSCOMP Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

! -- The name of the PEST control file is acquired.

100    write(6,102,advance='no')
102    format(' Enter name of PEST control file: ')
       read(5,*,err=100) pestfile
       pestfile=adjustl(pestfile)
       i=len_trim(pestfile)
       j=i-4
       if(j.lt.1)j=1
       atemp=pestfile(j:i)
       if(index(atemp,'.').eq.0) then
         pestfile=trim(pestfile)//'.pst'
       end if
       nn=len_trim(pestfile)
       atemp=pestfile(nn-3:nn)
       call lowcas(atemp)
       if(atemp(1:5).ne.'.pst ')then
         write(6,105)
105      format(/,' PEST control file must have an extension of ',
     +   '".pst" - try again.',/)
         go to 100
       end if

! -- The PEST control file is read.

       call addquote(pestfile,afile)
       write(6,110) trim(afile)
110    format(' - reading PEST control file ',a,'....')
       irestart=0
       p1start=0
       pfile=0
       call read_pest_data(ifail,irestart,p1start,pfile,pestfile)
       if(ifail.ne.0) go to 9890
       if(maxcompdim.gt.1)then
         write(errmsg,120) trim(afile)
120      format('OBSCOMP requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       icount=0
       do iobs=1,nobs
         if(owght(iobs).gt.0.0d0) icount=icount+1
         if(icount.ge.3) go to 121
       end do
       write(errmsg,1201)
1201   format('The PEST control file must contain at least 3 ',
     + 'non-zero-weighted observations.')
       go to 9890
121    continue
       iobsgp_old=0
       if(numcov.gt.0)then
         do iobs=1,nobs
           iobsgp=nobgnm(iobs)
           if(iobsgp.ne.iobsgp_old)then
             do icov=1,numcov
               if(covgp(icov).eq.iobsgp)then
                 write(errmsg,124)
124              format('OBSCOMP requires that no covariance ',
     +           'matrices be assigned to non-prior-information ',
     +           'observation groups in PEST control file.')
                 go to 9890
               end if
             end do
             iobsgp_old=iobsgp
           end if
         end do
       end if
       write(6,125) trim(afile)
125    format(' - file ',a,' read ok.')

! -- The corresponding Jacobian matrix file is read.

       jcofile=pestfile(1:nn-3)//'jco'
       call addquote(jcofile,afile)
       write(6,126) trim(afile)
126    format(' - reading Jacobian matrix file ',a,'....')
       how='read'
       call open_unformatted_file(ifail,10,how,jcofile,errmsg)
       if(ifail.ne.0) go to 9890
       read(10,err=9250,end=9250)itemp1,itemp2
       if(abs(itemp1).ne.nespar) go to 9220
       if(abs(itemp2).ne.nxrow) go to 9220
       bn=abs(itemp2)
       if(itemp1.lt.0)then
         continue
       else
         write(errmsg,240) trim(afile)
240      format('Jacobian matrix file ',a,' was not produced by ',
     +   'version 8 or later of PEST; use program JCOTRANS to ',
     +   'convert this to new format.')
         go to 9890
       end if
       x(:nobs,:)=0.0d0   ! an array
       read(10,err=9250,end=9250)icount
       do i=1,icount
         read(10,err=9250,end=9250) j,dtemp
         ies=(j-1)/bn+1
         iobs=j-(ies-1)*bn
         if(iobs.le.nobs)x(iobs,ies)=dtemp
       end do
       do ipar=1,npar
         if(itrans(ipar).lt.0) cycle
         read(10,err=9250,end=9250) aapar
         call lowcas(aapar)
         if(aapar.ne.apar(ipar))go to 9220
       end do
       do iobs=1,nobs
         read(10,err=9250,end=9250) aaobs
         call lowcas(aaobs)
         if(aaobs.ne.aobs(iobs)) go to 9220
       end do
       close(unit=10)
       write(6,170) trim(afile)
170    format(' - file ',a,' read ok.')

       write(6,*)
172    write(6,173,advance='no')
173    format(' Read residuals from REC, RES or REI file? [c/s/i]: ')
       read(5,'(a)') aa
       call lowcas(aa)
       if((aa.ne.'c').and.(aa.ne.'s').and.(aa.ne.'i')) go to 172
       if(aa.eq.'c')then
         resfile=pestfile(1:nn-3)//'rec'
       else if(aa.eq.'s')then
         resfile=pestfile(1:nn-3)//'res'
       else if(aa.eq.'i')then
         resfile=pestfile(1:nn-3)//'rei'
       end if
       call addquote(resfile,afile)
       open(unit=10,file=resfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(6,175)
175      format(' *** Cannot open file - try again ***')
         go to 172
       end if
       write(6,176) trim(afile)
176    format(' - reading file ',a,'....')
       if(aa.eq.'c')then
         iline=0
         do
           iline=iline+1
           read(10,'(a)',err=9000,end=9020) cline
           if(index(cline,' Residual ').ne.0) exit
         end do
         iline=iline+1
         read(10,'(a)',err=9000,end=9040) cline
         do iobs=1,nobs
           iline=iline+1
           read(10,'(a)',err=9000,end=9040) cline
           call linspl(ifail,4,lw,rw,cline)
           if(ifail.ne.0) go to 9000
           aaobs=cline(lw(1):rw(1))
           call lowcas(aaobs)
           if(aaobs.ne.aobs(iobs))then
             write(errmsg,178) trim(afile)
178          format('Observations listed in residuals table in file ',
     +       a,' do not accord with those in PEST control file.')
             go to 9890
           end if
           call drealrd(ifail,cline(lw(4):rw(4)),oval(iobs))
           if(ifail.ne.0) go to 9000
         end do
       else if(aa.eq.'s')then
         iline=1
         read(10,'(a)',err=9000,end=9100) cline
         if(index(cline,' Residual ').eq.0) then
           write(errmsg,131) trim(afile)
131        format('Unexpected header to file ',a,'.')
           go to 9890
         end if
         do iobs=1,nobs
           iline=iline+1
           read(10,'(a)',err=9000,end=9040) cline
           call linspl(ifail,5,lw,rw,cline)
           if(ifail.ne.0) go to 9000
           aaobs=cline(lw(1):rw(1))
           call lowcas(aaobs)
           if(aaobs.ne.aobs(iobs))then
             write(errmsg,178) trim(afile)
             go to 9890
           end if
           call drealrd(ifail,cline(lw(5):rw(5)),oval(iobs))
           if(ifail.ne.0) go to 9000
         end do
       else if(aa.eq.'i')then
         iline=0
         do
           iline=iline+1
           read(10,'(a)',err=9000,end=9020) cline
           if(index(cline,' Residual ').ne.0) exit
         end do
         do iobs=1,nobs
           iline=iline+1
           read(10,'(a)',err=9000,end=9040) cline
           call linspl(ifail,5,lw,rw,cline)
           if(ifail.ne.0) go to 9000
           aaobs=cline(lw(1):rw(1))
           call lowcas(aaobs)
           if(aaobs.ne.aobs(iobs))then
             write(errmsg,178) trim(afile)
             go to 9890
           end if
           call drealrd(ifail,cline(lw(5):rw(5)),oval(iobs))
           if(ifail.ne.0) go to 9000
         end do
       end if
       close(unit=10)
       write(6,170) trim(afile)

! -- We now acquire information from the user.

       write(6,*)
250    write(6,260,advance='no')
260    format(' Enter name of observation of interest: ')
       read(5,*) aaobs
       do iobs=1,nobs
         if(aaobs.eq.aobs(iobs)) go to 280
       end do
       write(6,270)
270    format(' *** Unknown observation - try again ***')
       go to 250
280    iiobs=iobs
       aaaobs=aaobs
       wweight=owght(iiobs)
       if(wweight.le.0.0d0)then
         write(6,281)
281      format(' *** Observation has zero weight - ',
     +   'try again ***')
         go to 250
       end if

! -- The name of the output file is acquired.

       write(6,*)
290    write(6,295,advance='no')
295    format(' Enter name for observation competition index file: ')
       read(5,*) outfile
       open(unit=20,file=outfile,action='write',err=290)

286    write(6,287,advance='no')
287    format(' Enter name for sensitivities file: ')
       read(5,*,err=286) outfile1
       if(outfile1.eq.outfile)then
         write(6,288)
288      format(' *** Must not be the same as competition ',
     +   'index file ***')
         go to 286
       end if
       open(unit=21,file=outfile1,action='write',err=286)
296    write(6,297,advance='no')
297    format(' Enter no. of pos/neg competing observations ',
     + 'to list sensitivities for: ')
       read(5,*,err=296) maxnum
       if(maxnum.lt.1) then
         write(6,298)
298      format(' *** Must be 1 or greater - try again ***')
         go to 296
       end if

! -- We take the square root of all weights (these were squared in READPEST).

       do iobs=1,nobs
         owght(iobs)=sqrt(owght(iobs))
       end do
       wweight=sqrt(wweight)

! -- We calculate the strength of competing observations.

       allocate(obscmp(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200

       do iobs=1,nobs
         ww=owght(iobs)*wweight
         sum=0.0d0
         do ies=1,nespar
           sum=sum+x(iobs,ies)*x(iiobs,ies)
         end do
         obscmp(iobs)=sum*ww
       end do

!       write(6,*)                                                       !debug
!       do iobs=1,nobs                                                   !debug
!         write(6,299) trim(aobs(iobs)),obscmp(iobs),                    !debug
!     +   oval(iobs)                                                     !debug
!       end do                                                           !debug

! -- We now sort obscmp. We keep track of original ordering. We put the observation in question at the end.

       allocate(obsindex(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200
       do iobs=1,nobs
         obsindex(iobs)=iobs
       end do
       aaobs=aobs(iiobs)
       aobs(iiobs)=aobs(nobs)
       aobs(nobs)=aaobs
       itemp=obsindex(iiobs)
       obsindex(iiobs)=obsindex(nobs)
       obsindex(nobs)=itemp
       dtemp=obscmp(iiobs)
       obscmp(iiobs)=obscmp(nobs)
       obscmp(nobs)=dtemp
       dtemp=oval(iiobs)
       oval(iiobs)=oval(nobs)
       oval(nobs)=dtemp
       do iobs=1,nobs-2
         minval=1.0d300
         do jobs=iobs,nobs-1
           if(obscmp(jobs).lt.minval)then
             minval=obscmp(jobs)
             jj=jobs
           end if
         end do
         if(jj.ne.iobs)then
           dtemp=obscmp(iobs)
           obscmp(iobs)=obscmp(jj)
           obscmp(jj)=dtemp
           aaobs=aobs(iobs)
           aobs(iobs)=aobs(jj)
           aobs(jj)=aaobs
           itemp=obsindex(iobs)
           obsindex(iobs)=obsindex(jj)
           obsindex(jj)=itemp
           dtemp=oval(iobs)
           oval(iobs)=oval(jj)
           oval(jj)=dtemp
         end if
       end do
!       write(6,*)                                                       !debug
!       do iobs=1,nobs                                                   !debug
!         write(6,299) trim(aobs(iobs)),obscmp(iobs),                    !debug
!     +   oval(iobs)                                                     !debug
!299      format(1x,a,t25,1pg14.7,t42,1pg14.7)                           !debug
!       end do                                                           !debug

! -- We now write the first output file.

       write(20,300) trim(aaaobs)
300    format(' Observation of interest       = ',a)
       write(atemp20,'(1pg14.7)') oval(nobs)
       atemp20=adjustl(atemp20)
       write(20,305) trim(atemp20)
305    format(' Residual for this observation = ',a)
       write(20,*)
       write(20,310)
310    format(' Observation',t25,'Competition_index',t46,'Residual')
       do iobs=1,nobs-1
         write(20,320) trim(aobs(iobs)),obscmp(iobs),oval(iobs)
320      format(2x,a,t25,1pg14.7,t45,1pg14.7)
       end do

       close(unit=20)
       write(6,340) trim(outfile)
340    format(' - file ',a,' written ok.')


! -- Rows of the Jacobian matrix are multiplied by observation weights.

       do ies=1,nespar
         do iobs=1,nobs
           x(iobs,ies)=x(iobs,ies)*owght(iobs)
         end do
       end do

! -- We adjust maxnum so that there is no overlap in printing things out.

       itemp=(nobs-1)/2
       if(maxnum.gt.itemp)then
         if(itemp*2.eq.nobs-1)then
           maxnum1=itemp
           maxnum2=itemp
         else
           maxnum1=itemp+1
           maxnum2=itemp
         end if
       else
         maxnum1=maxnum
         maxnum2=maxnum
       end if

! -- The header to the second output file is written.

       write(21,429)
429    format(' Weighted sensitivity of observation to ',
     + 'parameter...')
       write(21,430) 'parameter   ', aaaobs,
     + (aobs(iobs),iobs=1,maxnum1),
     + (aobs(iobs),iobs=nobs-1-maxnum2+1,nobs-1)
430    format(1x,a,t15,1000(a20,2x))

! -- Now we write the output table.
! -- This is a little cumbersome because of a bug in the old Lahey compiler.

       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           write(21,440) trim(apar(ipar)),x(iiobs,ies),
     +     (x(obsindex(iobs),ies),iobs=1,maxnum1),
     +     (x(obsindex(iobs),ies),iobs=nobs-1-maxnum2+1,nobs-1)
440        format(1x,a,t15,1000(1pg14.7,8x))
         end if
       end do

       close(unit=21)
       write(6,340) trim(outfile1)

       go to 9900

9000   call writint(aline,iline)
       write(errmsg,9010) trim(aline),trim(afile)
9010   format('Error reading line ',a,' of file ',a,'.')
       go to 9890

9020   write(errmsg,9030) trim(afile)
9030   format('Could not find residuals table at end of file ',a,'.')
       go to 9890

9040   write(errmsg,9050) trim(afile)
9050   format('Premature end to file ',a,' encountered while reading ',
     + 'residuals table.')
       go to 9890

9100   write(errmsg,9110) trim(afile)
9110   format('Unexpected end encountered to file ',a,'.')
       go to 9890

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9220   continue
       write(errmsg,9230)
9230   format('The PEST control file and the Jacobian matrix ',
     + 'file are incompatible.')
       go to 9890

9250   write(errmsg,9260) trim(afile)
9260   format('Error or premature end encountered when reading ',
     + 'Jacobian matrix file ',a,'.')
       go to 9890

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)

9900   continue
       call pest_data_deallocate(ifail)
       deallocate(obscmp,stat=ierr)
       deallocate(obsindex,stat=ierr)

       close(unit=10,iostat=ierr)
       close(unit=20,iostat=ierr)
       close(unit=21,iostat=ierr)
       close(unit=30,iostat=ierr)

       end




        SUBROUTINE WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)

! -- SUBROUTINE WHICH1 FINDS A STRING IN AN ARRAY OF STRINGS

        implicit none

        integer, intent(out)                :: ifail    ! error indicator
        integer, intent(in)                 :: npar     ! number of parameters
        integer, intent(inout)              :: ipar     ! where to start the search
        character (len=*), intent(in), dimension(npar)  :: apar     ! parameter names
        character (len=*), intent(inout)    :: tpar     ! parameter name to look for

        integer                             :: i

        IFAIL=0
        IF((IPAR.LT.1).OR.(IPAR.GT.NPAR)) IPAR=1
        call lowcas(tpar)
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
        END subroutine



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
21      leadblank=i-1
        nblc=len_trim(amessage)
5       jend=j+78-itake
        if(jend.ge.nblc) go to 100
        do i=jend,j+1,-1
        if(amessage(i:i).eq.' ') then
          if(itake.eq.0) then
             write(junit,'(a)') amessage(j+1:i)
             itake=2+leadblank
          else
             write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:i)
          end if
          j=i
          go to 5
        end if
        end do
        if(itake.eq.0)then
          write(junit,'(a)') amessage(j+1:jend)
          itake=2+leadblank
        else
          write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:jend)
        end if
        j=jend
        go to 5
100     jend=nblc
        if(itake.eq.0)then
          write(junit,'(a)') amessage(j+1:jend)
        else
          write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:jend)
        end if
        return

      end


        subroutine writint(atemp,ival)

!       Subroutine WRITINT writes an integer to a character variable.

        integer*4 ival
        character*6 afmt
        character*(*) atemp

        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(atemp)
        write(atemp,afmt)ival
        atemp=adjustl(atemp)
        return
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
