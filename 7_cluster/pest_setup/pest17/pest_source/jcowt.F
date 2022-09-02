       program jcowt

! -- Program JCOWT multiplies elements of a JCO file by weights.

       use pestdata
       implicit none

       integer n,n1,ifail,i,j,itemp1,itemp2
       integer pfile,irestart
       integer ies,ipar,iobs,irow,iobsgp
       integer bn,icount
       integer lw(10),rw(10)
       double precision dtemp
       character*5 aext
       character*10 how
       character*12 aversion,aapar
       character*20 aaobs
       character*256 pestfile,jcofile1,jcofile2,afile

! -- Initialisation

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(' JCOWT Version ',a,'. Watermark Numerical Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

! -- The command line is parsed.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       pestfile=cline(lw(1):rw(1))
       jcofile1=cline(lw(2):rw(2))
       jcofile2=cline(lw(3):rw(3))
       call remchar(pestfile,char(211))
       call remchar(jcofile1,char(211))
       call remchar(jcofile2,char(211))

#ifndef UNIX
       call lowcas(pestfile)
       call lowcas(jcofile1)
       call lowcas(jcofile2)
#endif

! -- Extensions are tidied up.

       call addquote(pestfile,afile)
       n=len_trim(pestfile)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=pestfile(n1:n)
       call lowcas(aext)
       if((aext(1:1).eq.'.').and.(aext.ne.'.pst')) go to 9100
       if(aext.eq.'.pst')pestfile(n1:n)=' '
       n=len_trim(pestfile)
       pestfile(n+1:)='.pst'

       call addquote(jcofile1,afile)
       n=len_trim(jcofile1)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=jcofile1(n1:n)
       call lowcas(aext)
       if((aext(1:1).eq.'.').and.(aext.ne.'.jco')) go to 9150
       if(aext.eq.'.jco')jcofile1(n1:n)=' '
       n=len_trim(jcofile1)
       jcofile1(n+1:)='.jco'

       call addquote(jcofile2,afile)
       n=len_trim(jcofile2)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=jcofile2(n1:n)
       call lowcas(aext)
       if((aext(1:1).eq.'.').and.(aext.ne.'.jco')) go to 9150
       if(aext.eq.'.jco')jcofile2(n1:n)=' '
       n=len_trim(jcofile2)
       jcofile2(n+1:)='.jco'

! -- The PEST control file is read.

       write(6,*)
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
1240     format('JCOWT requires that MAXCOMPDIM be set to 1 ',
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
       do iobsgp=1,nobsgp
         if(covind(iobsgp)) go to 9290
       end do
       write(6,1275) trim(afile)
1275   format(' - file ',a,' read ok.')

! -- Compress the apar and pargnme arrays.

       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           apar(ies)=apar(ipar)
         end if
       end do

! -- The Jacobian matrix file is read.

       write(6,*)
       call addquote(jcofile1,afile)
       write(6,1100) trim(afile)
1100   format(' - reading Jacobian matrix file ',a,'...')
       how='read'
       call open_unformatted_file(ifail,15,how,jcofile1,errmsg)
       if(ifail.ne.0) go to 9890
       read(15,err=9250,end=9250)itemp1,itemp2
       if((abs(itemp1).ne.nespar).or.
     +    (abs(itemp2).ne.nxrow)) go to 9270
       bn=abs(itemp2)
       if(itemp1.lt.0)then
         continue
       else
         write(errmsg,1120) trim(afile)
1120      format('Jacobian matrix file ',a,' was not produced by ',
     +   'version 8 or later of PEST; use program JCOTRANS to ',
     +   'convert this to new format.')
         go to 9890
       end if
       x=0.0d0    ! an array
       read(15,err=9250,end=9250)icount
       do i=1,icount
         read(15,err=9250,end=9250) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         x(irow,ies)=dtemp
       end do
       do i=1,nespar
         read(15,err=9250,end=9250) aapar
         if(aapar.ne.apar(i)) go to 9270
       end do
       do i=1,nobs
         read(15,err=9250,end=9250) aaobs
         call lowcas(aaobs)
         if(aaobs.ne.aobs(i)) go to 9270
       end do
       close(unit=15)
       write(6,1121) trim(afile)
1121   format(' - file ',a,' read ok.')

! -- Now weight the Jacobian matrix

       do iobs=1,nxrow
         dtemp=max(0.0d0,owght(iobs))
         dtemp=sqrt(dtemp)
         do ies=1,nespar
           x(iobs,ies)=x(iobs,ies)*dtemp
         end do
       end do

! -- Now write the weighted Jacobian matrix.

       call addquote(jcofile2,afile)
       how='write'
       call open_unformatted_file(ifail,15,how,jcofile2,errmsg)
       if(ifail.ne.0) go to 9400
       write(15,err=9400) -nespar,-nxrow
       icount=0
       do i=1,nespar
          do j=1,nxrow
            if(x(j,i).ne.0.0d0) icount=icount+1
          end do
       end do
       write(15,err=9400) icount
       do i=1,nespar
         do j=1,nxrow
           if(x(j,i).ne.0.0d0)then
             icount=(i-1)*nxrow+j
             write(15,err=9400)icount,x(j,i)
           end if
         end do
       end do
       do ies=1,nespar
         write(15,err=9400) apar(ies)
       end do
       do iobs=1,nxrow
         write(15,err=9400) aobs(iobs)
       end do
       close(unit=15)
       write(6,1130) trim(afile)
1130   format(' - file ',a,' written ok.')

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' JCOWT is run using the command:',/)
       write(6,8920)
8920   format('     jcowt pestfile jcofile1 jcofile2')
       write(6,8930)
8930   format(/,' where',/)
       write(6,8935)
8935   format('     pestfile is a PEST control file,')
       write(6,8945)
8945   format('     jcofile1 is an existing Jacobian matrix file ',
     + 'and')
       write(6,8950)
8950   format('     jcofile2 is a new Jacobian matrix file.')

       go to 9999


9100   write(errmsg,9110) trim(afile)
9110   format('File ',a,' does not have an extension of ".pst".')
       go to 9890

9150   write(errmsg,9160) trim(afile)
9160   format('File ',a,' does not have an extension of ".jco".')
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
9295   format('As presently programmed, JCOWT requires that ',
     + 'no covariance matrices be associated with observations ',
     + 'or prior information equations in the PEST control file.')
       go to 9890

9400   write(errmsg,9410) trim(afile)
9410   format('Cannot write to file ',a,'.')
       go to 9890


9890   continue
       call write_message(errmsg)
9900   continue

       call pest_data_deallocate(ifail)

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
