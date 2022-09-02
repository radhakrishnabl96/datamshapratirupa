       program SIMCASE

C -- Program SIMCASE reads a PEST control file and associated JCO file. It then
C    writes a new PEST control file and associated JCO file in which all non-essential
C    things are ommitted. These are:-
C    1. Regularisation observations and prior information equations;
C    2. Tied and fixed parameters.

       use pestdata
       implicit none

       logical lexist
       integer  npar2,ipar,npargp2,ipg,nobs2,nprior2,irow,ig,
     + nxrow2,nobsgp2,readflag,nespar2,iobsgp,iprior
       integer  n,n1,ierr,ifail,jj
       integer  itemp1,itemp2,bn,i,j,icount,ies
       integer p1start_temp,pfile
       integer lw(5),rw(5)
       double precision dtemp
       character*4   aext
       character*12  aapar,aobsgp
       character*12  aversion
       character*20  aaobs
       character*200 pestfile1,pestfile2,jcofile1,jcofile2
       character*200 apestfile1,apestfile2,ajcofile1,ajcofile2

       integer, allocatable :: obs_retain(:),obsgp_retain(:),
     + pargp_retain(:)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(' SIMCASE Version ',a,'. Watermark Numerical Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

! -- Initialization

       readflag=0

! -- The command line is parsed.

       if(cline.eq.' ') go to 9000
       call spacesub(cline)
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.eq.0) go to 9000
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       pestfile1=cline(lw(1):rw(1))
       pestfile2=cline(lw(2):rw(2))
       call remchar(pestfile1,char(211))
       call remchar(pestfile2,char(211))
       n=len_trim(pestfile1)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=pestfile1(n1:n)
       call lowcas(aext)
       if(aext.eq.'.pst')pestfile1(n1:n)=' '
       jcofile1=pestfile1
       n=len_trim(pestfile1)
       pestfile1(n+1:)='.pst'
       jcofile1(n+1:)='.jco'
       n=len_trim(pestfile2)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=pestfile2(n1:n)
       call lowcas(aext)
       if(aext.eq.'.pst')pestfile2(n1:n)=' '
       jcofile2=pestfile2
       n=len_trim(pestfile2)
       pestfile2(n+1:)='.pst'
       jcofile2(n+1:)='.jco'

       call addquote(pestfile1,apestfile1)
       call addquote(pestfile2,apestfile2)
       call addquote(jcofile1,ajcofile1)
       call addquote(jcofile2,ajcofile2)


       inquire(file=pestfile1,exist=lexist)
       if(.not.lexist)then
         write(errmsg,40) trim(apestfile1)
40       format('Cannot open PEST control file ',a,'.')
         go to 9890
       end if

C -- The PEST input dataset is read.

       readflag=1
       restart=0
       p1start_temp=0
       pfile=0
       write(6,130) trim(apestfile1)
130    format(/,' - reading PEST control file ',a,'...')
       call read_pest_data(ifail,restart,p1start_temp,pfile,pestfile1)
       if(ifail.ne.0)then
         write(errmsg,140) trim(apestfile1)
140      format('Error encountered while reading PEST control file ',a,
     +   '. Check the PEST input dataset using PESTCHEK.')
         go to 9890
       end if
       if(svda.ne.0)then
         write(errmsg,141)
141      format('SIMCASE cannot be used with a PEST input dataset ',
     +   'that instructs PEST to conduct SVD-assisted parameter ',
     +   'estimation.')
         go to 9890
       end if
       write(6,150) trim(apestfile1)
150    format(' - file ',a,' read ok.')

C -- The Jacobian matrix file is read.

       write(6,180) trim(ajcofile1)
180    format(/,' - reading Jacobian matrix file ',a,'...')

       call open_unformatted_file(ifail,15,'read',jcofile1,errmsg)
       if(ifail.ne.0)then
         write(errmsg,120) trim(ajcofile1)
120      format('Cannot open unformatted Jacobian matrix file ',a,'.')
         go to 9890
       end if

       read(15,err=9250,end=9250)itemp1,itemp2
       if(abs(itemp1).ne.nespar) go to 9200
       bn=abs(itemp2)
       if(itemp1.lt.0)then
         continue
       else
         write(errmsg,240) trim(ajcofile1)
240      format('Jacobian matrix file ',a,' was not produced by ',
     +   'version 8 or later of PEST; use program JCOTRANS to ',
     +   'convert this to new format.')
         go to 9890
       end if
       if((bn.ne.nobs).and.(bn.ne.nxrow)) go to 9200
       do i=1,nespar
         do j=1,nobs
           x(j,i)=0.0d0
         end do
       end do
       read(15,err=9250,end=9250)icount
       do i=1,icount
         read(15,err=9250,end=9250) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         if(irow.le.nobs) x(irow,ies)=dtemp
       end do
       do i=1,npar
         if(itrans(i).lt.0) cycle
         read(15,err=9250,end=9250) aapar
         if(aapar.ne.apar(i)) go to 9200
       end do
       do i=1,nobs
         read(15,err=9250,end=9250) aaobs
         call lowcas(aaobs)
         if(aaobs.ne.aobs(i)) go to 9200
       end do
       if(nprior.ne.0)then
         if(bn.eq.nxrow)then
           do i=1,nprior
             read(15,err=9250,end=9250) aaobs
             call lowcas(aaobs)
             if(aaobs.ne.aobs(nobs+i)) go to 9200
           end do
         end if
       end if
       close(unit=15)
       write(6,150) trim(ajcofile1)

C -- The dimensionality of the simplified case is now established.

       allocate(obs_retain(nxrow),obsgp_retain(nobsgp),
     + pargp_retain(npargp),stat=ierr)
       if(ierr.ne.0)then
         write(errmsg,242)
242      format('Cannot allocate sufficient memory to continue ',
     +   'SIMCASE execution.')
         go to 9890
       end if
       obs_retain=0          ! an array
       obsgp_retain=0        ! an array
       pargp_retain=0        ! an array

       npar2=0
       do ipar=1,npar
         if(itrans(ipar).ge.0) npar2=npar2+1
       end do
       nespar2=npar2

       npargp2=0
       do ipg=1,npargp
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             if(ipargp(ipar).eq.ipg)then
               npargp2=npargp2+1
               pargp_retain(ipg)=1
               go to 243
             end if
           end if
         end do
243      continue
       end do

       if(pestmode.eq.3)then
         nobs2=0
         nprior2=0
         do irow=1,nxrow
           ig=nobgnm(irow)
           aobsgp=obgnme(ig)
           if(aobsgp(1:5).eq.'regul')then
             continue
           else
             obs_retain(irow)=1
             if(irow.le.nobs)then
               nobs2=nobs2+1
             else
               nprior2=nprior2+1
             end if
           end if
         end do
       else
         obs_retain=1        ! an array
         nobs2=nobs
         nprior2=nprior
       end if

C -- Zero-weighted observations and prior information equations are now removed.

       do irow=1,nxrow
         if(obs_retain(irow).eq.1)then
           if(owght(irow).le.0.0d0)then
             if(.not.covind(nobgnm(irow)))then
               obs_retain(irow)=0
               if(irow.le.nobs)then
                 nobs2=nobs2-1
               else
                 nprior2=nprior2-1
               end if
             end if
           end if
         end if
       end do

       nxrow2=nobs2+nprior2

       nobsgp2=0
       do iobsgp=1,nobsgp
         if(pestmode.eq.3)then
           if(obgnme(iobsgp)(1:5).eq.'regul') cycle
         end if
         do irow=1,nxrow
           if(nobgnm(irow).eq.iobsgp) go to 250
         end do
         cycle
250      continue
         nobsgp2=nobsgp2+1
         obsgp_retain(iobsgp)=1
       end do

C -- The PEST control file is re-opened and largely copied to the new PEST control file.

       write(6,340) trim(apestfile2)
340    format(/,' - writing PEST control file ',a,'...')
       open(unit=10,file=pestfile1,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,330) trim(apestfile1)
330      format('Cannot re-open PEST control file ',a,'.')
         go to 9890
       end if
       open(unit=20,file=pestfile2,action='write',err=9350)
       write(20,350,err=9350)
350    format('pcf')
       write(20,360,err=9350)
360    format('* control data')
       write(20,362)
362    format('restart  estimation')
       write(20,370,err=9350) npar2,nobs2,npargp2,nprior2,nobsgp2
370    format(5i10)
       write(20,380,err=9350)
380    format('   1      1    single  point')
       write(20,390,err=9350)
390    format('0.01   -3.0      0.3   0.03   10')
       write(20,400,err=9350)
400    format('10.0    10.0    0.1')
       write(20,410,err=9350)
410    format(' 0.1')
       write(20,420,err=9350)
420    format('50       .005      4      4   .005   4')
       write(20,430,err=9350)
430    format('  1   1   1   1')

       write(20,440)
440    format('* parameter groups')
       do
         read(10,'(a)',err=9300,end=9300) cline
         call lowcas(cline)
         if(index(cline,'* parameter gr').ne.0) go to 450
       end do
       go to 9300
450    continue
       do ipg=1,npargp
         read(10,'(a)',err=9300,end=9300) cline
         if(pargp_retain(ipg).eq.1)then
           write(20,'(a)',err=9350) trim(cline)
         end if
       end do

       write(20,460)
460    format('* parameter data')
       do
         read(10,'(a)',err=9300,end=9300) cline
         call lowcas(cline)
         if(index(cline,'* parameter da').ne.0) go to 470
       end do
       go to 9300
470    continue
       do ipar=1,npar
         read(10,'(a)',err=9300,end=9300) cline
         if(itrans(ipar).ge.0) then
           write(20,'(a)',err=9350) trim(cline)
         end if
       end do

       write(20,480)
480    format('* observation groups')
       do
         read(10,'(a)',err=9300,end=9300) cline
         call lowcas(cline)
         if(index(cline,'* observation gr').ne.0) go to 490
       end do
       go to 9300
490    continue
       do iobsgp=1,nobsgp
         read(10,'(a)',err=9300,end=9300) cline
         if(obsgp_retain(iobsgp).eq.1)then
           write(20,'(a)') trim(cline)
         end if
       end do

       write(20,491)
491    format('* observation data')
       do
         read(10,'(a)',err=9300,end=9300) cline
         call lowcas(cline)
         if(index(cline,'* observation da').ne.0) go to 495
       end do
       go to 9300
495    continue
       do irow=1,nobs
         read(10,'(a)',err=9300,end=9300) cline
         if(obs_retain(irow).eq.1)then
           write(20,'(a)') trim(cline)
         end if
       end do

       write(20,500)
500    format('* model command line')
       write(20,510)
510    format('dummy_command')
       write(20,520)
520    format('* model input/output')
       write(20,530)
530    format('dummy.tpl dummy.in')
       write(20,540)
540    format('dummy.ins dummy.out')
       if(nprior.ne.0)then
         icount=0
         do irow=nobs+1,nxrow
           if(obs_retain(irow).eq.1) icount=icount+1
         end do
         if(icount.ne.0)then
           write(20,550)
550        format('* prior information')
           do
             read(10,'(a)',err=9300,end=9300) cline
             call lowcas(cline)
             if(index(cline,'* prior in').ne.0) go to 570
           end do
           go to 9300
570        continue
           read(10,'(a)',err=9300,end=9300) cline
           cline=adjustl(cline)
           do iprior=1,nprior
580          continue
             if(obs_retain(nobs+iprior).eq.1)then
               write(20,'(a)',err=9350) trim(cline)
             end if
             if(iprior.eq.nprior)then
               read(10,'(a)',iostat=ierr) cline
               if(ierr.ne.0) then
                 cline=' '
               end if
             else
               read(10,'(a)',err=9300,end=9300) cline
               cline=adjustl(cline)
             end if
             if(cline(1:1).eq.'&') go to 580
           end do
         end if
       end if

       close(unit=10)
       close(unit=20)
       write(6,590) trim(apestfile2)
590    format(' - file ',a,' written ok.')

C -- The new JCO file is written.


       write(6,591) trim(ajcofile2)
591    format(/,' - writing Jacobian matrix file ',a,'...')
       call open_unformatted_file(ifail,15,'write',jcofile2,errmsg)
       if(ifail.ne.0) go to 9400
       write(15,err=9400) -nespar2,-nxrow2
       icount=0
       do i=1,nespar
          do j=1,nxrow
            if(obs_retain(j).eq.1)then
              if(x(j,i).ne.0.0d0) icount=icount+1
            end if
          end do
       end do
       write(15,err=9400) icount
       do i=1,nespar
         jj=0
         do j=1,nxrow
           if(obs_retain(j).eq.1)then
             jj=jj+1
             if(x(j,i).ne.0.0d0)then
               icount=(i-1)*nxrow2+jj
               write(15,err=9400)icount,x(j,i)
             end if
           end if
         end do
       end do
       do ipar=1,npar
         if((itrans(ipar).eq.0).or.(itrans(ipar).eq.1))then
            write(15,err=9400) apar(ipar)
         end if
       end do
       do irow=1,nxrow
         if(obs_retain(irow).eq.1)then
           write(15,err=9400) aobs(irow)
         end if
       end do
       close(unit=15)
       write(6,590) trim(ajcofile2)

       go to 9900

9000   continue

       write(6,9010)
9010   format(/,' SIMCASE is run using the command:',/)
       write(6,9020)
9020   format('     simcase case1 case2',/)
       write(6,9030)
9030   format(' where',/)
       write(6,9050)
9050   format('     case1 pertains to a case for which ',
     + 'PST and JCO files exist, and')
       write(6,9060)
9060   format('     case2 pertains to a simplified case ',
     + 'for which these files will be written.')
       go to 9999

9200   continue
       write(errmsg,9210) trim(ajcofile1),trim(apestfile1)
9210   format('Jacobian matrix file ',a,' is incompatible with PEST ',
     + 'control file ',a,'.')
       go to 9890
9250   continue
       write(errmsg,9260) trim(ajcofile1)
9260   format('Error encountered in reading Jacobian matrix file ',
     + a,'.')
       go to 9890
9300   continue
       write(errmsg,9310) trim(apestfile1)
9310   format('Error re-reading PEST control file ',a,'.')
       go to 9890
9350   continue
       write(errmsg,9360) trim(apestfile2)
9360   format('Error writing to PEST control file ',a,'.')
       go to 9890
9400   continue
       write(errmsg,9410) trim(ajcofile2)
9410   format('Error writing to JCO file ',a,'.')
       go to 9890

9890   call write_message(errmsg)
9900   continue
       if(readflag.eq.1)then
         call pest_data_deallocate(ierr)
       end if

       if(allocated(obs_retain)) deallocate(obs_retain,stat=ierr)
       if(allocated(obsgp_retain)) deallocate(obsgp_retain,stat=ierr)
       if(allocated(pargp_retain)) deallocate(pargp_retain,stat=ierr)

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
