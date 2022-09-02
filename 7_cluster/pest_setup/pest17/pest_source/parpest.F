C     Last change:  J    12 Jan 2005    9:26 pm

        subroutine slavdat1(jfail,nslave,ifltyp,cline,iprll,ldosch,
     +  repeatrun,run_slow_fac)

C -- Subroutine slavdat1 opens the run manager file and reads the number of
C    slaves.

#ifdef BEO
        use BEOPEST
#endif

#ifdef INTEL
        use COMMON_MOD, ONLY: FLENME,CASEFL,IWAIT
#endif
        use pestdata, only :irmf,irmr,errmsg,run_slow_fac_1
        implicit none

#ifdef INTEL
        integer nslave,irmline,ifltyp,iprll,ldosch,itemp,
     +  ifail,jfail,repeatrun,ierr
#else
        integer nslave,irmline,ifltyp,iwait,iprll,ldosch,itemp,
     +  ifail,jfail,repeatrun,ierr
#endif
        integer nn,ii,jj,kk,iiflag
        integer lw(5),rw(5)
        real rwait
        double precision dwait,dtemp
        double precision run_slow_fac
        character*(*) cline
#ifdef INTEL
        character*100 ccline
#else
        character*100 casefl,ccline
        character*200 flenme
#endif

#ifndef INTEL
        common /case/casefl
        common /flenme/flenme
        common /waitint/ iwait
#endif

        jfail=0
        run_slow_fac=1.5
#ifdef BEO
        run_slow_fac=2.5
#endif

#ifdef BEO
        if (BEOMASTER) then
          parlam_flag=0
          ldosch=1
          iwait=0
          nslave=2
#ifdef UNIX
        flenme=trim(casefl)//'.rmf'
#else
        flenme=trim(casefl)//'.RMF'
#endif
          call ffopen(jfail,irmf,'r',' ',117,cline)
          if(jfail.eq.0) then
            irmline=1
            read(irmf,*,err=9050,end=9100)
            irmline=2
            ifail=0
            read(irmf,'(a)',err=9050,end=9100) ccline
            call lowcas(ccline)
            nn=index(ccline,'run_slow_fac')
            if(nn.ne.0)then
              do ii=nn+1,len_trim(ccline)
                if(ccline(ii:ii).eq.'=') go to 120
              end do
              go to 9300
120           continue
              iiflag=0
              kk=ii+1
              do jj=ii+1,len(ccline)
                if(ccline(jj:jj).eq.' ')then
                  if(iiflag.eq.1) go to 140
                  kk=jj+1
                else
                  iiflag=1
                end if
              end do
              go to 9300
140           continue
              call drealrd(jfail,ccline(kk:jj-1),run_slow_fac)
              if(jfail.ne.0) go to 9300
              if(run_slow_fac.lt.1.2)then
                if(run_slow_fac_1.lt.0.0)then
                  write(errmsg,130)
130               format('Run_slow_fac variable must be greater than ',
     +            '1.2 in run management file.')
                  go to 9891
                end if
              end if
              ccline(nn:jj-1)=' '
            end if
            call linspl(ifail,4,lw,rw,ccline)
            if(ifail.eq.0)then
              call numrd(ifail,0,lw(4),rw(4),ldosch,dtemp,ccline)
              if(ifail.ne.0) go to 9050
              parlam_flag=1
              if(ldosch.gt.1) then
                ldosch=1
              end if
            end if
          end if
          jfail=0
          close(unit=irmf,iostat=ierr)
        else
#endif

#ifdef MPEST
        ldosch=-99999999
        iwait=-99999999
        flenme=trim(casefl)//'.number_processors'
        call delfile(jfail,-irmr,flenme,cline)
#else
        ldosch=0
#endif

#ifdef GENIE
        NSLAVE=1                                                                !ctm - initialization only - routine is called to get number of slaves connected to GMAN when needed
        IFLTYP=0
        IWAIT=1
        LDOSCH=1
        REPEATRUN=1
#else
#ifdef UNIX
        flenme=trim(casefl)//'.rmf'
#else
        flenme=trim(casefl)//'.RMF'
#endif
        if(iprll.eq.0) flenme='p###.rmf'
        call ffopen(jfail,irmf,'r',' ',117,cline)
        if(jfail.ne.0) go to 9891
        irmline=1
        read(irmf,*,err=9050,end=9100)
        irmline=2

        repeatrun=1
        ifail=0
        read(irmf,'(a)',err=9050,end=9100) ccline
        call lowcas(ccline)
        nn=index(ccline,'run_slow_fac')
        if(nn.ne.0)then
          do ii=nn+1,len_trim(ccline)
            if(ccline(ii:ii).eq.'=') go to 121
          end do
          go to 9300
121       continue
          iiflag=0
          kk=ii+1
          do jj=ii+1,len(ccline)
            if(ccline(jj:jj).eq.' ')then
              if(iiflag.eq.1) go to 141
              kk=jj+1
            else
              iiflag=1
            end if
          end do
          go to 9300
141       continue
          call drealrd(jfail,ccline(kk:jj-1),run_slow_fac)
          if(jfail.ne.0) go to 9300
          if(run_slow_fac.lt.1.2)then
            write(errmsg,131)
131         format('Run_slow_fac variable must be greater than ',
     +      '1.2 in run management file.')
            go to 9891
          end if
          ccline(nn:jj-1)=' '
        end if
        call linspl(ifail,5,lw,rw,ccline)
        if(ifail.eq.0)then
          call numrd(ifail,0,lw(5),rw(5),repeatrun,dtemp,ccline)
          if(ifail.ne.0) go to 9050
          if(repeatrun.ne.0)repeatrun=1
        end if
        call linspl(ifail,4,lw,rw,ccline)
        if(ifail.eq.0)then
          call numrd(ifail,0,lw(4),rw(4),ldosch,dtemp,ccline)
          if(ifail.ne.0) go to 9050
          if(ldosch.gt.1) then
            ldosch=1
C          else if(ldosch.lt.0)then
C            ldosch=0
          end if
        end if
        call linspl(ifail,3,lw,rw,ccline)
        if(ifail.ne.0) go to 9050
        call numrd(ifail,0,lw(1),rw(1),nslave,dtemp,ccline)
        if(ifail.ne.0) go to 9050
        call  numrd(ifail,0,lw(2),rw(2),ifltyp,dtemp,ccline)
        if(ifail.ne.0) go to 9050
        call numrd(ifail,1,lw(3),rw(3),itemp,dwait,ccline)
        if(ifail.ne.0) go to 9050
        rwait=dwait
c        read(irmf,*,err=9050,end=9100) nslave,ifltyp,rwait
        if(rwait.le.0) rwait=0.2
        iwait=nint(rwait*100.0)
        if(iwait.lt.1) iwait=1
#ifdef BEO
        endif
#endif
#endif !GENIE

        go to 9999

9050    call stperr(2,2,' ',irmline,' ',cline)
        go to 9891
9100    call stperr(3,1,' ',0,' ',cline)
        go to 9891

9300    write(errmsg,9310)
9310    format('Error encountered in reading "run_slow_fac=F.d" ',
     +  'string from run management file.')
        go to 9891

9999    return

9891    jfail=1
        return
        end


#ifndef MPEST

        subroutine slavdat2(jfail,nslave,iruntme,idet,aslave,asldir,
     +  cline,afile,sreadfle,oreadfle,preadfle,sfinfle,manfle,
     +  mreadfle,scom,slavegroup)

C -- Subroutine slavdat2 reads part of the run manager file.
C    It also tests part of the information in it.

#ifdef BEO
        use BEOPEST
        use BEOPRIV, only : TCP,MPI
#endif
#ifdef GENIE
        use GENIE_DATA, only : host
#endif
        use pestdata, only : irmf,irmr,ipcm,irsf,errmsg
#ifdef INTEL
        use COMMON_MOD, ONLY: FLENME,CASEFL,IWAIT
#endif


        implicit none

#ifdef INTEL
        integer irmline,nslave,islave,n,ndet,numerr,ierr,
     +  iiwait,iitemp,jfail
#else
        integer irmline,nslave,islave,n,ndet,numerr,ierr,iwait,
     +  iiwait,iitemp,jfail
#endif
        integer slavegroupflag
        integer lw(5),rw(5),ifail
        integer idet(nslave),iruntme(nslave)
        character*4 acom
        character*10 aa,afmt,atemp
        character*(*) aslave(nslave),asldir(nslave),scom(nslave)
        character*(*) cline,afile,sreadfle,oreadfle,preadfle,
     +  manfle,sfinfle,mreadfle
        character*(*) slavegroup(nslave)
#ifndef INTEL
        character*100 casefl
        character*200 flenme
#endif

#ifdef UNIX
        integer unlink
#endif

#ifndef INTEL
        common /case/casefl
        common /flenme/flenme
        common /waitint/iwait
#endif

        jfail=0
        iiwait=iwait
        slavegroupflag=0
        irmline=2
#ifndef GENIE
#ifdef BEO
        if (.not.BEOMASTER) then
#endif
        do 100 islave=1,nslave
          idet(islave)=0
          irmline=irmline+1
          read(irmf,'(a)',err=9050,end=9100) cline
          call spacesub(cline)
          call linspl(ifail,2,lw,rw,cline)
          if(ifail.ne.0) go to 9050
          aslave(islave)=cline(lw(1):rw(1))
          asldir(islave)=cline(lw(2):rw(2))
          call remchar(aslave(islave),char(211))
          call remchar(asldir(islave),char(211))
#ifndef UNIX
          call lowcas(aslave(islave))
          call lowcas(asldir(islave))
#endif
          slavegroup(islave)=' '
          call linspl(ifail,3,lw,rw,cline)
          if(ifail.eq.0)then
            if(islave.eq.1)then
              slavegroupflag=1
            else
              if(slavegroupflag.ne.1)then
                write(errmsg,90) trim(aslave(islave)),trim(flenme)
90              format('An agent group name has been provided for ',
     +          'agent "',a,'" but not for other agents in file ',a,'.')
                go to 9891
              end if
            end if
            slavegroup(islave)=cline(lw(3):rw(3))
            call remchar(slavegroup(islave),char(211))
            call lowcas(slavegroup(islave))
          else
            if(islave.eq.1)then
              slavegroupflag=0
            else
              if(slavegroupflag.ne.0)then
                write(errmsg,91) trim(aslave(islave)),trim(flenme)
91              format('An agent group name has not been provided for ',
     +          'agent "',a,'" but has been provided for at least ',
     +          'one other agent in file ',a,'.')
                go to 9891
              end if
            end if
          end if
100     continue
        irmline=irmline+1
        read(irmf,*,err=9050,end=9100) (iruntme(islave),islave=1,nslave)
        do 110 islave=1,nslave
          if(iruntme(islave).le.0) iruntme(islave)=1
110     continue

C -- We now verify that it is possible to read and write from the slave
C    subdirectories.

        numerr=0
        do 400 islave=1,nslave
          n=len_trim(asldir(islave))
#ifdef UNIX
          if(asldir(islave)(n:n).ne.char(47))then
            n=n+1
            asldir(islave)(n:n)=char(47)
#else
          if(asldir(islave)(n:n).ne.char(92))then
            n=n+1
            asldir(islave)(n:n)=char(92)
#endif
          end if
          afile=trim(asldir(islave))//'p###.##'
          open(unit=ipcm,file=trim(afile),iostat=ierr)
          if(ierr.ne.0) then
            write(6,150)
150         format(' Run management error:-')
            write(6,160) trim(asldir(islave))
160         format(' Cannot open a file in subdirectory ',a)
            numerr=numerr+1
            go to 400
          end if
          write(ipcm,'(a)',iostat=ierr) ' Testing'
          if(ierr.ne.0) then
            write(6,150)
            write(6,170) trim(asldir(islave))
170         format(' Cannot write to a file in subdirectory ',a)
            numerr=numerr+1
          end if
          close(unit=ipcm,iostat=ierr)
          if(ierr.ne.0) then
            write(6,150)
            write(6,190) trim(asldir(islave))
190         format(' Cannot close a file in subdirectory ',a)
            go to 9891
          end if
          afile=trim(asldir(islave))//'p###.##'
          open(unit=ipcm,file=trim(afile),status='old',
     +    iostat=ierr)
          if(ierr.ne.0) then
            write(6,150)
            write(6,160) trim(asldir(islave))
            numerr=numerr+1
            go to 400
          end if
          read(ipcm,'(a)',iostat=ierr) aa
          if(ierr.ne.0) then
            write(6,150)
            write(6,210) trim(asldir(islave))
210         format(' Cannot read from a file in subdirectory ',a)
            numerr=numerr+1
          end if
          close(unit=ipcm,iostat=ierr)
          if(ierr.ne.0) then
            write(6,150)
            write(6,190) trim(asldir(islave))
            go to 9891
          end if
400     continue
C       if(numerr.ne.0) go to 9891
#ifdef BEO
        endif
#endif
#else
        aslave(1)='Controlled by GMAN'
        asldir(1)=''
#endif !GENIE

C -- The run manager record file in next opened.

#ifdef UNIX
#ifdef UNICOS
        n=unlink(trim(manfle))
#endif
        open(unit=irmr,file=manfle,iostat=ierr)
#else
#ifdef LF90
        open(unit=irmr,file=manfle,blocksize=1,
     +  action='readwrite,denynone',iostat=ierr)
#else
#ifdef INTEL
        open(unit=irmr,file=manfle,blocksize=1,shared,iostat=ierr)
#else
        open(unit=irmr,file=manfle,blocksize=1,iostat=ierr)
#endif
#endif
#endif
        if(ierr.ne.0) then
          call stperr(1,1,manfle,0,' ',cline)
          go to 9891
        end if
        n=len_trim(casefl)
        n=max((80-(n+33))/2,1)
        write(afmt,430) n
430     format('(t',i2,',a)')
        write(irmr,afmt) ' PEST RUN MANAGEMENT RECORD: CASE '//
     +  trim(casefl)
#ifdef BEO
        if (BEOMASTER) then
          write(irmr,*)
          write(irmr,*)
          write(irmr,4303)
4303      format(1x,40('*'))
          write(irmr,4301)
4301      format(' RUN MANAGEMENT UNDERTAKEN USING BEOPEST.')
          if(tcp)then
            acom='TCP'
          else if(mpi)then
            acom='MPI'
          end if
          write(irmr,4302) trim(acom)
4302      format(' COMMUNICATIONS PROTOCOL = ',a,'.')
          write(irmr,4303)
          write(irmr,*)
          RETURN
        end if
#endif
#ifdef GENIE
        write(irmr,*)
        write(irmr,*)
        write(irmr,4303)
4303    format(1x,70('*'))
        write(irmr,4301)
4301    format(' RUN MANAGEMENT UNDERTAKEN USING GENIE.')
        write(irmr,4302) trim(host)
4302    format(1x,'HOST Socket: ',a)
        write(irmr,'(a)') ' GENIE_INTERFACE.log (in PPEST folder)'//
     +                   ' logs interface actions with GMAN.'
        write(irmr,4303)
        write(irmr,*)
#ifdef UNIX
#ifdef UNICOS
        n=unlink('GENIE_INTERFACE.log')
#endif
        open(unit=69,file='GENIE_INTERFACE.log',status='replace',
     +       iostat=ierr)
#else
#ifdef LF90
        open(unit=69,file='GENIE_INTERFACE.log',status='replace',
     +       blocksize=1,action='readwrite,denynone',iostat=ierr)
#else
#ifdef INTEL
        open(unit=69,file='GENIE_INTERFACE.log',status='replace',
     +       blocksize=1,iostat=ierr)
#else
        open(unit=69,file='GENIE_INTERFACE.log',status='replace',
     +       blocksize=1,iostat=ierr)
#endif
#endif
#endif
        if(ierr.ne.0) then
          call stperr(1,1,'GENIE_INTERFACE.log',0,' ',cline)
          go to 9891
        end if
        close(69)
        RETURN
#endif





        call writslv1(nslave,aslave,asldir,slavegroup)

C -- Next we see if the slaves have started.

        do 500 islave=1,nslave
#ifndef SCRIPT
          afile=trim(asldir(islave))//sreadfle
          call delfile(jfail,-irsf-1000,afile,cline)
          if(jfail.ne.0) go to 9891
#endif
          afile=trim(asldir(islave))//oreadfle
          call delfile(jfail,-irsf,afile,cline)
          if(jfail.ne.0) go to 9891
#ifndef SCRIPT
          afile=trim(asldir(islave))//preadfle
          call delfile(jfail,-irsf,afile,cline)
          if(jfail.ne.0) go to 9891
          afile=trim(asldir(islave))//sfinfle
          call delfile(jfail,-irsf,afile,cline)
          if(jfail.ne.0) go to 9891
          afile=trim(asldir(islave))//mreadfle
#ifdef UNIX
          open(unit=ipcm,file=trim(afile),iostat=ierr)
#else
#ifdef LF90
          open(unit=ipcm,file=trim(afile),
     +    action='readwrite,denynone',blocksize=1,iostat=ierr)
#else
#ifdef INTEL
          open(unit=ipcm,file=trim(afile),blocksize=1,shared,
     +    iostat=ierr)
#else
          open(unit=ipcm,file=trim(afile),blocksize=1,iostat=ierr)
#endif
#endif
#endif
          if(ierr.eq.0)then
            write(ipcm,'(i10)',err=9200) iwait
#ifdef UNIX
#ifdef UNICOS
            rewind(unit=ipcm)
            write(ipcm,'(i10)',err=9200) iwait
#endif
#endif
c           close(unit=ipcm,err=9200)
            call closefile(jfail,ipcm,2,afile,cline)  !la
            if(jfail.ne.0) go to 9891
          end if
#endif
500     continue
#ifndef SCRIPT
        call pestwait(iiwait)
        call pestwait(iiwait)

520     continue
        write(6,525)
525     format(' Testing whether agents are alive .....',/)
        n=0
        ndet=0
530     do 600 islave=1,nslave
          if(idet(islave).eq.0) then
            afile=trim(asldir(islave))//sreadfle
#ifdef UNIX
            open(unit=ipcm,file=trim(afile),status='old',
     +      iostat=ierr)
#else
#ifdef LF90
            open(unit=ipcm,file=trim(afile),status='old',
     +      action='readwrite,denynone',blocksize=1,iostat=ierr)
#else
#ifdef INTEL
            open(unit=ipcm,file=trim(afile),status='old',
     +      blocksize=1,shared,iostat=ierr)
#else
            open(unit=ipcm,file=trim(afile),status='old',
     +      blocksize=1,iostat=ierr)
#endif
#endif
#endif
            if(ierr.eq.0)then
              read(ipcm,'(a)') scom(islave)
              call closefile(jfail,ipcm,2,afile,cline)
              if(jfail.ne.0) go to 9891
              write(6,550) trim(aslave(islave))
550           format(' - agent "',a,'" has been detected.')
              write(irmr,550) trim(aslave(islave))
              idet(islave)=1
              ndet=ndet+1
              afile=trim(asldir(islave))//mreadfle
#ifdef UNIX
              open(unit=ipcm,file=trim(afile),err=9200)
#else
#ifdef LF90
              open(unit=ipcm,file=trim(afile),
     +        action='readwrite,denynone',blocksize=1,err=9200)
#else
#ifdef INTEL
              open(unit=ipcm,file=trim(afile),blocksize=1,shared,
     +        err=9200)
#else
              open(unit=ipcm,file=trim(afile),blocksize=1,err=9200)
#endif
#endif
#endif
              write(ipcm,'(i10)',err=9200) iwait
#ifdef UNIX
#ifdef UNICOS
              rewind(unit=ipcm)
              write(ipcm,'(i10)',err=9200) iwait
#endif
#endif
              call closefile(jfail,ipcm,2,afile,cline)  !la
              if(jfail.ne.0) go to 9891
            end if
          end if
600     continue
        if(ndet.lt.nslave) then
          n=n+1
          if(n.gt.10) then
            if(ndet.gt.0)then
              call writint(atemp,ndet)
              write(6,630) trim(atemp)
630           format(/,' Only ',a,' agents are alive.')
              write(6,640)
640           format(' PEST will now commence the parameter estimation ',
     +        'process using only these ',/,' agents and will use the ',
     +        'others if/when they become available.')
              go to 9999
            else
              call stperr(123,0,' ',0,' ',cline)
              go to 9891
            end if
          end if
          call stopress(0)
          iitemp=max(200,iwait*3/nslave)
          call pestwait(iitemp)
          go to 530
        end if
        write(6,620)
620     format(/,' All agents are alive.',/)

#endif

        go to 9999

9050    call stperr(2,2,' ',irmline,' ',cline)
        go to 9891
9100    call stperr(3,1,' ',0,' ',cline)
        go to 9891
9200    flenme=trim(afile)
        call stperr(118,1,' ',0,' ',cline)
        go to 9891

9999    return

9891    jfail=1
        return
        end




        subroutine writslv1(nslave,aslave,asldir,slavegroup)

        use pestdata, only : irmr
        implicit none

        integer nslave,islave
        character*(*) aslave(nslave),asldir(nslave)
        character*(*) slavegroup(nslave)

        write(irmr,20)
20      format(/,' AGENT DETAILS:-',/)
        write(irmr,50)
50      format(' Agent Name',t30,'PAGENT Working Directory')
        write(irmr,55)
55	format(' ----------',t30,'------------------------')
        do 100 islave=1,nslave
          write(irmr,60)trim(aslave(islave)),
     +    trim(asldir(islave))
60        format(' "',a,'"',t30,a)
100     continue
        if(slavegroup(1).ne.' ')then
          write(irmr,*)
          write(irmr,56)
56        format(' Agent Name',t30,'Agent Group')
          write(irmr,57)
57        format(' ----------',t30,'-----------')
          do islave=1,nslave
            write(irmr,61) trim(aslave(islave)),
     +      trim(slavegroup(islave))
61          format(' "',a,'"',t30,'"',a,'"')
          end do
        end if

#ifndef SCRIPT
        write(irmr,110)
110     format(/,/,' Attempting to communicate with agents ....',/)
#endif

        return
        end


        subroutine writslv2(jfail,nslave,ninfle,noutfl,ldsin,ldsou,
     +  ldosch,repeatrun,aslave,sinfle,soufle,scom,afile,cline,
     +  run_slow_fac)

#ifdef INTEL
        USE COMMON_MOD, ONLY: IWAIT
#endif
        use pestdata, only : inst,irmf,irmr
#ifdef BEO
        use BEOPEST
#endif
        implicit none

        logical lexist
        integer nslave,ninfle,noutfl,ldsin,ldsou,islave,i,ierr,
#ifdef INTEL
     +  iiwait,ldosch,repeatrun,jfail
#else
     +  iwait,iiwait,ldosch,repeatrun,jfail
#endif
        double precision run_slow_fac
        character*10 atemp
        character*20 aatemp
        character*(*) afile,cline
        character*(*) aslave(nslave),sinfle(ldsin,nslave),
     +  soufle(ldsou,nslave),scom(nslave)

#ifndef INTEL
        common /waitint/iwait
#endif

        jfail=0
#ifdef BEO
        if (BEOMASTER) then
          if(parlam_flag.eq.0)then
            if(ldosch.ge.0)then
              write(irmr,4305)
4305          format(/,' BEOPEST was either not able to find ',
     +        'a run management file in the current',/,
     +        ' working directory or did not find a value for PARLAM ',
     +        'in that file.')
              write(irmr,4306)
4306          format(/,' PARLAM has been assigned a value of 1. Hence ',
     +        'Marquardt lambda search runs ',/,' will be ',
     +        'parallelized.',/)
            else
              write(irmr,4311)
4311          format(/,' NUMLAM in the PEST control file was set to ',
     +        'a negative number.')
              write(irmr,4312)
4312          format(' PARLAM has therefore been set to -9999.')
              write(irmr,4309)
              write(irmr,4315)
              call writint(atemp,abs(ldosch))
              write(irmr,4319) trim(atemp)
            end if
          else
            write(irmr,4307)
4307        format(/,' BEOPEST found a run management file for current ',
     +      'case in the current directory.')
            write(irmr,4308)
4308        format(' A value for PARLAM was read from that file.')
            write(6,4307)
            write(6,4308)
            write(irmr,4309)
4309        format(/,' BEOPEST PARLAM behaviour is as follows:-')
            if((ldosch.eq.1).or.(ldosch.lt.0))then
              write(irmr,4315)
4315          format(' PARALLELISE LAMBDA SEARCH     : yes')
              if(ldosch.lt.0)then
                call writint(atemp,abs(ldosch))
                write(irmr,4319) trim(atemp)
4319            format(' MAX AGENTS IN LAMDBA SEARCH   : ',a)
              end if
            else if(ldosch.eq.2)then
              write(irmr,4316)
              write(irmr,4317)
4317          format(' (Altered from "yes" because RLAMBDA1 is zero.)')
              ldosch=0
            else
              write(irmr,4316)
4316          format(' PARALLELISE LAMBDA SEARCH     : no')
            end if
          end if
          write(aatemp,'(1pg11.5)') run_slow_fac
          aatemp=adjustl(aatemp)
          write(irmr,4321) trim(aatemp)
4321      format(' SLOW RUN DECLARATION FACTOR   : ',a)
          return
        end if
#endif
        iiwait=iwait
        write(irmr,100)
100	format(/,/,' AGENT MODEL INPUT AND OUTPUT FILES:-')
#ifdef GENIE
        islave=1
#else
        do 300 islave=1,nslave
#endif
          write(irmr,120) trim(aslave(islave))
120	  format(/,' Agent "',a,'" ----->')
#ifdef GENIE
          write(irmr,130)
130	  format(/,'     Model input files on agents:-')
#else
          write(irmr,130) trim(aslave(islave))
130	  format(/,'     Model input files on agent "',a,'":-')
#endif
          do 170 i=1,ninfle
            write(irmr,150) trim(sinfle(i,islave))
150	    format(t10,a)
170	  continue
#ifdef GENIE
          write(irmr,180)
180	  format(/,'     Model output files on agents:-')
#else
          write(irmr,180) trim(aslave(islave))
180	  format(/,'     Model output files on agent "',a,'":-')
#endif
          do 200 i=1,noutfl
            afile=trim(soufle(i,islave))
            write(irmr,150) trim(afile)
#ifdef GENIE
            jfail=0
#else
            call delfile(jfail,-inst,afile,cline)
#endif
            if(jfail.ne.0) go to 9891
c	    inquire(file=trim(afile),exist=lexist,
c     +      iostat=ierr)
c	    if(lexist)
c     +      call system('del "'//trim(afile)//'" > nul')
200	  continue
#ifdef GENIE
          write(irmr,250)
250	  format(/,'     Model command line for agents:-')
#else
          write(irmr,250) trim(aslave(islave))
250	  format(/,'     Model command line for agent "',a,'":-')
#endif
          write(irmr,150) trim(scom(islave))
          write(irmr,*)
#ifndef GENIE
300	continue
#endif

        call writint(atemp,iiwait)
        write(irmr,310) trim(atemp)
310	format(/,' AVERAGE WAIT INTERVAL         : ',a,
     +  ' hundredths of a second.')
        if((ldosch.eq.1).or.(ldosch.lt.0))then
          write(irmr,315)
315       format(' PARALLELISE LAMBDA SEARCH     : yes')
          if(ldosch.lt.0)then
#ifdef GENIE
            write(irmr,309)
309         format(' MAX AGENTS IN LAMDBA SEARCH   : '//
     +             'To be determined by GENIE')
#else
            call writint(atemp,abs(ldosch))
            write(irmr,309) trim(atemp)
309         format(' MAX AGENTS IN LAMDBA SEARCH   : ',a)
#endif
          end if
        else if(ldosch.eq.2)then
          write(irmr,316)
          write(irmr,317)
317       format(' (Altered from "yes" because RLAMBDA1 is zero.)')
          ldosch=0
        else
          write(irmr,316)
316       format(' PARALLELISE LAMBDA SEARCH     : no')
        end if
        if(repeatrun.eq.1)then
          write(irmr,318)
318       format(' REPEAT TROUBLESOME MODEL RUNS : yes')
        else
          write(irmr,319)
319       format(' REPEAT TROUBLESOME MODEL RUNS : no')
        end if
        write(aatemp,'(1pg11.5)') run_slow_fac
        aatemp=adjustl(aatemp)
        write(irmr,4322) trim(aatemp)
4322    format(' SLOW RUN DECLARATION FACTOR   : ',a)

        close(unit=irmf)

        return

9891    jfail=1
        return
        end



        subroutine slavdat3(jfail,nslave,ninfle,noutfl,ldsin,
     +  ldsou,sinfle,soufle,cline)

#ifdef BEO
        use BEOPEST
#endif
        use pestdata, only : irmf
#ifdef INTEL
        use COMMON_MOD, ONLY: FLENME
#endif
        implicit none

        integer nslave,ninfle,noutfl,ldsin,ldsou,i,j,irmline,jfail
        character*(*) sinfle(ldsin,nslave),soufle(ldsou,nslave)
#ifndef INTEL
        character*200 flenme
#endif
        character*(*) cline
#ifdef UNIX
        integer lw(1),rw(1),ifail
#endif

#ifndef INTEL
        common /flenme/flenme
#endif

        jfail=0
#ifdef BEO
        if (BEOMASTER) return
#endif
#ifdef GENIE
        return
#endif

        irmline=2+nslave+1
        do 100 i=1,nslave
          do 50 j=1,ninfle
            irmline=irmline+1
#ifdef UNIX
            read(irmf,'(a)',err=9050,end=9100) cline
            call linspl(ifail,1,lw,rw,cline)
            sinfle(j,i)=cline(lw(1):rw(1))
#else
            read(irmf,*,err=9050,end=9100) sinfle(j,i)
#endif
50	  continue
          do 80 j=1,noutfl
            irmline=irmline+1
#ifdef UNIX
            read(irmf,'(a)',err=9050,end=9100) cline
            call linspl(ifail,1,lw,rw,cline)
            soufle(j,i)=cline(lw(1):rw(1))
#else
            read(irmf,*,err=9050,end=9100) soufle(j,i)
#endif
80	  continue
100	continue
        return

9050	call stperr(2,2,' ',irmline,' ',cline)
        go to 9891
9100	call stperr(3,1,' ',0,' ',cline)
        go to 9891

9891    jfail=1
        return
        end


#ifndef GENIE
        subroutine doruns(jfail,nslave,nrun,istats,
     +  istatr,asldir,ninstr,noufle,asize,numl,nobs,nblbmx,lcins,
     +  ll,obsn1,obsn2,iiobs,aobs,a,mrkdel,cline,buf,afile,
     +  istrtme,iruntme,jrun,npar,precis,nopnt,ninfle,nw,
     +  scale,offset,pardel,pword,tmpfle,apar,oreadfle,preadfle,
     +  manfle,aslave,nnrun,outfle,insfle,ldsin,ldsou,sinfle,
     +  soufle,itrial,imsfle,incpar,itrans,idet,sreadfle,mreadfle,scom,
     +  irestart,ptunit,ptfile,pitn,repeatrun,lamid,slavegroup,
     +  workvecdim,workvec,run_slow_fac)


#ifdef INTEL
        USE COMMON_MOD, ONLY: ISTOP,FLENME,NCALL,MCALL,IWAIT,RESULT
#endif
        use pestdata, only : inst,irmr,ipcm,irsf,ijrf,lamforgive,
     +                       ippo,obsregfile,noptmax
        implicit none

#ifdef INTEL
        integer nslave,nrun,ninstr,noufle,asize,numl,nobs,nblbmx,
     +  npar,precis,nopnt,ninfle,nnrun,
     +  ierr,ldsin,ldsou,ifail,final,icount,iirun,iiwait,
     +  ii,iitrial,imsfle,jfail,repeatrun
#else
        integer nslave,nrun,ninstr,noufle,asize,numl,nobs,nblbmx,
     +  istop,npar,precis,nopnt,ninfle,nnrun,mcall,
     +  ncall,ierr,ldsin,ldsou,ifail,final,icount,iwait,iirun,iiwait,
     +  ii,iitrial,imsfle,jfail,repeatrun
#endif
        integer icflag
        integer nbb,yflag,kflag,icount1,kslave
        integer irestart,ptunit,ptcount,jj,itemps,pitn,titn
        integer lamid,forgiveflag,numrepeatrun
        integer workvecdim,jjfail
        integer psuccess,mcall_keep
        integer istats(nslave),istatr(nrun),lcins(ninstr),ll(numl),
     +  obsn1(nobs),obsn2(nobs),iiobs(nobs),istrtme(nslave),
     +  iruntme(nslave),jrun(nslave),nw(npar),itrial(nrun),
     +  incpar(nrun),itrans(npar),idet(nslave)
        double precision run_slow_fac
        double precision scale(npar),offset(npar)
        double precision workvec(workvecdim)
        character a(asize),mrkdel(noufle),pardel(ninfle)
        character*(*) aobs(nobs),pword(npar),
     +  tmpfle(ninfle),apar(npar),aslave(nslave),outfle(noufle),
     +  insfle(noufle)
        character*(*) asldir(nslave),sinfle(ldsin,nslave),
     +  soufle(ldsou,nslave),scom(nslave)
        character*(*) slavegroup(nslave)
        character*(*) oreadfle,cline,buf,afile,manfle,
     +  preadfle,sreadfle,mreadfle
#ifdef INTEL
        character*11 atemp*5
#else
        character*200 flenme
        character*11 result,atemp*5
#endif
        character*(*) ptfile
        character*(1) aa

        integer islave,irun,now,jslave,jjslave,itemp1,itemp2,
     +  itemp3,itemp4,ins,j,isum,i,nblc,maxtrial
cccc        integer jjjj               !debug

#ifdef SCRIPT
        integer iitemp,ishell
#endif
#ifndef INTEL
        common /flenme/flenme
        common /sstop/istop
        common /modcal/ncall,mcall
        common /waitint/iwait
        common /newtime/result
#endif

        jfail=0
        if(lamid.eq.1)then
          maxtrial=2
        else
          maxtrial=4
        end if
        icflag=0
        forgiveflag=0
        if(lamid.eq.1)then
          numrepeatrun=2
        else
          numrepeatrun=3
        end if
        if(slavegroup(1).ne.' ')then
          nbb=len(slavegroup(1))
        else
          nbb=1
        end if
        psuccess=0

        iiwait=iwait
#ifndef UNIX
        if(nrun.gt.1) write(6,*)
#endif
        iirun=0

        if(mcall.eq.0) then
          ins=0
          isum=0
          DO 100 I=1,NOUFLe
            CALL FFOPEN(JFAIL,INST,'r',INSFLE(I),78,CLINE)
            IF(JFAIL.NE.0) GO TO 9891
            READ(INST,*)
            INS=INS+1
            CLINE(1:1)=CHAR(2)
            CLINE(2:2)=' '
            CLINE(3:LEN(CLINE))=OUTFLE(I)
            LCINS(INS)=ISUM+1
            NBLC=LEN_TRIM(CLINE)
            DO 20 J=1,NBLC
20          A(J+ISUM)=CLINE(J:J)
            ISUM=ISUM+NBLC
30          READ(INST,22,END=180) CLINE
22          FORMAT(A)
            CALL TABREM(CLINE)
            IF(INDEX(CLINE,MRKDEL(I)).EQ.0) CALL CMPRSS(CLINE)
            NBLC=LEN_TRIM(CLINE)
            IF(NBLC.EQ.0) GO TO 30
            INS=INS+1
            LCINS(INS)=ISUM+1
            DO 70 J=1,NBLC
70          A(J+ISUM)=CLINE(J:J)
            ISUM=ISUM+NBLC
            GO TO 30
180         CLOSE(UNIT=INST)
100       CONTINUE
        end if

C -- Initialize some variables.

        do 10 irun=1,nrun
          istatr(irun)=0
          if(repeatrun.eq.1)then
            itrial(irun)=0
          else
            itrial(irun)=maxtrial
          end if
10      continue
	do 15 islave=1,nslave
	  jrun(islave)=0
15	continue

C -- The restart file situation is handled

        if(irestart.eq.1)then
          call ffopen(jfail,-ptunit,'w',ptfile,25,cline)
          if(jfail.ne.0)then
            jfail=0
            irestart=0
            go to 50
          end if
          write(ptunit,iostat=ierr)pitn
#ifdef FLUSHFILE
          call flush(ptunit)
#endif
        else if(irestart.eq.2)then
          ptcount=0
          call ffopen(jfail,-ptunit,'r',ptfile,22,cline)
          if(jfail.ne.0) then
            jfail=0
            go to 2000
          end if
          titn=0
          read(ptunit,iostat=ierr)titn
          if(pitn.ne.titn)then
            close(unit=ptunit,iostat=ierr,status='delete')
            go to 2000
          end if
          do
            read(ptunit,err=1900,end=1900) itemps
            do jj=1,nobs
              workvec(jj)=-1.1d300
            end do
            istatr(itemps)=-99
            ptcount=ptcount+1
            read(ptunit,err=1900,end=1900)
     +      (workvec(jj),jj=1,nobs)
            call store_parallel_register
     +      (jjfail,nobs,ippo,itemps,workvec,obsregfile)
            if(jjfail.ne.0) go to 9891
          end do
1900      close(unit=ptunit,status='delete',iostat=ierr)
          if(itemps.ne.0)then
            do jj=1,nobs
              if(workvec(jj).lt.-1.0d300)then
                ptcount=ptcount-1
                istatr(itemps)=0
                go to 1901
              end if
            end do
          end if
1901      continue
2000      continue
#ifdef LAHEY
          call writint(atemp,ptcount)
          write(6,49) trim(atemp)
49        format('+   Results from ',a,
     +    ' model runs read from restart file.')
          write(6,*)
#endif
#ifdef INTEL
          call writint(atemp,ptcount)
          write(6,49) trim(atemp)
49        format('    Results from ',a,
     +    ' model runs read from restart file.')
          write(6,*)
          icflag=1
#endif
          iirun=ptcount
          ncall=ncall+ptcount
          call ffopen(jfail,-ptunit,'w',ptfile,25,cline)
          if(jfail.ne.0)then
            jfail=0
            irestart=0
            go to 50
          end if
          write(ptunit,iostat=ierr)pitn
          if(ptcount.ne.0)then
            do i=1,nrun
              if(istatr(i).eq.-99)then
                call retrieve_parallel_register
     +          (jjfail,nobs,ippo,i,workvec,obsregfile)
                if(jjfail.ne.0) go to 9891
                write(ptunit,iostat=ierr)i
                write(ptunit,iostat=ierr)(workvec(jj),jj=1,nobs)
              end if
            end do
          end if
#ifdef FLUSHFILE
          call flush(ptunit)
#endif
          if(ptcount.eq.nrun) go to 9999
        end if

50	continue

! -- See if any new slaves have appeared.

	do 700 islave=1,nslave
	  if(idet(islave).eq.0) then
	    afile=trim(asldir(islave))//sreadfle
#ifdef UNIX
	    open(unit=ipcm,file=trim(afile),status='old',
     +      iostat=ierr)
#else
#ifdef LF90
	    open(unit=ipcm,file=trim(afile),status='old',
     +      action='readwrite,denynone',blocksize=1,iostat=ierr)
#else
#ifdef INTEL
	    open(unit=ipcm,file=trim(afile),status='old',
     +      blocksize=1,shared,iostat=ierr)
#else
	    open(unit=ipcm,file=trim(afile),status='old',
     +      blocksize=1,iostat=ierr)
#endif
#endif
#endif
	    if(ierr.eq.0)then
	      read(ipcm,'(a)') scom(islave)
	      call closefile(jfail,ipcm,2,afile,cline)
	      if(jfail.ne.0) go to 9891
	      write(irmr,750) result,
     +	      trim(aslave(islave))
750	      format(3x,a,':- agent "',a,
     +        '" has just been detected.')
              write(6,751) trim(aslave(islave))
751           format('    Agent ',a,' has just been ',
     +        'detected.')
              write(6,*)
	      idet(islave)=1
	      afile=trim(asldir(islave))//mreadfle
#ifdef UNIX
              open(unit=ipcm,file=trim(afile),err=9400)
#else
#ifdef LF90
	      open(unit=ipcm,file=trim(afile),
     +        action='readwrite,denynone',blocksize=1,err=9400)
#else
#ifdef INTEL
              open(unit=ipcm,file=trim(afile),blocksize=1,shared,
     +        err=9400)
#else
              open(unit=ipcm,file=trim(afile),blocksize=1,err=9400)
#endif
#endif
#endif
	      write(ipcm,'(i10)',err=9400) iwait
#ifdef UNIX
#ifdef UNICOS
	      rewind(unit=ipcm)
	      write(ipcm,'(i10)',err=9400) iwait
#endif
#endif
              call closefile(jfail,ipcm,2,afile,cline)  !la
              if(jfail.ne.0) go to 9891

              afile=trim(asldir(islave))//oreadfle
#ifdef UNIX
	      open(unit=irsf,file=trim(afile),
     +        status='old',iostat=ierr)
#else
#ifdef LF90
              open(unit=irsf,file=trim(afile),
     +        action='readwrite,denynone',status='old',
     +        blocksize=1,iostat=ierr)
#else
#ifdef INTEL
	      open(unit=irsf,file=trim(afile),
     +        blocksize=1,status='old',shared,iostat=ierr)
#else
	      open(unit=irsf,file=trim(afile),
     +        blocksize=1,status='old',iostat=ierr)
#endif
#endif
#endif
              if(ierr.eq.0)then
                call closefile(jfail,irsf,2,afile,cline)
                if(jfail.ne.0) go to 9891
                call pestwait(iiwait)
                call delfile(jfail,irsf,afile,cline)
                if(jfail.ne.0) go to 9891
              end if

	    end if
	  end if
700	continue


! -- Have any slaves from lost runs or re-started slaves re-appeared.

	do 710 islave=1,nslave
          if(istats(islave).eq.-999)then
	    afile=trim(asldir(islave))//sreadfle
#ifdef UNIX
	    open(unit=ipcm,file=trim(afile),status='old',
     +      iostat=ierr)
#else
#ifdef LF90
	    open(unit=ipcm,file=trim(afile),status='old',
     +      action='readwrite,denynone',blocksize=1,iostat=ierr)
#else
#ifdef INTEL
	    open(unit=ipcm,file=trim(afile),status='old',
     +      blocksize=1,shared,iostat=ierr)
#else
	    open(unit=ipcm,file=trim(afile),status='old',
     +      blocksize=1,iostat=ierr)
#endif
#endif
#endif
	    if(ierr.eq.0)then
	      read(ipcm,'(a)') scom(islave)
	      call closefile(jfail,ipcm,2,afile,cline)
	      if(jfail.ne.0) go to 9891
	      afile=trim(asldir(islave))//mreadfle
#ifdef UNIX
              open(unit=ipcm,file=trim(afile),err=9400)
#else
#ifdef LF90
	      open(unit=ipcm,file=trim(afile),
     +        action='readwrite,denynone',blocksize=1,err=9400)
#else
#ifdef INTEL
              open(unit=ipcm,file=trim(afile),blocksize=1,shared,
     +        err=9400)
#else
              open(unit=ipcm,file=trim(afile),blocksize=1,err=9400)
#endif
#endif
#endif
	      write(ipcm,'(i10)',err=9400) iwait
#ifdef UNIX
#ifdef UNICOS
	      rewind(unit=ipcm)
	      write(ipcm,'(i10)',err=9400) iwait
#endif
#endif
              call closefile(jfail,ipcm,2,afile,cline)  !la
              if(jfail.ne.0) go to 9891
	      afile=trim(asldir(islave))//oreadfle
	      call delfile(jfail,-irsf,afile,cline)
	      if(jfail.ne.0) go to 9891
	      istats(islave)=0
	    end if
	  end if
710	continue

C -- See whether the escape key has been pressed.

	call stopress(0)
	if((istop.eq.2).or.(istop.eq.1)) then
          if(irestart.ne.0) close(unit=ptunit,iostat=ierr)
          return
        end if

C -- See if it is time to try any "frozen" slaves again.

	do 150 islave=1,nslave
	  if(idet(islave).eq.0) go to 150
	  if(istats(islave).eq.150) then
	    call getsecs(now,0)
	    if(now-istrtme(islave).gt.30)then   ! 30 seconds
	      istats(islave)=0
	    end if
	  end if
150	continue

C -- Look for the fastest free slave.

151     continue
        yflag=0
        if(lamid.ne.0)then
          if(slavegroup(1).eq.' ') go to 152
          do islave=1,nslave
            slavegroup(islave)(nbb:nbb)=' '
          end do
          do islave=1,nslave
            if(slavegroup(islave)(nbb:nbb).ne.' ') cycle
            icount=0
            icount1=0
            do kslave=islave,nslave
              if(slavegroup(islave).eq.slavegroup(kslave))then
                if(idet(islave).ne.0) then
                  icount1=icount1+1
                  if(istats(islave).ne.0) then
                    icount=icount+1
                  end if
                end if
              end if
            end do
            if(icount1.eq.0)then
              aa='x'
            else
              if(icount.ne.0)then
                aa='x'
              else
                aa='y'
                yflag=1
              end if
            end if
            do kslave=islave,nslave
              if(slavegroup(islave)(1:nbb-1).eq.
     +           slavegroup(kslave)(1:nbb-1))then
                 slavegroup(kslave)(nbb:nbb)=aa
              end if
            end do
          end do
152       continue
        end if

	itemp2=0
        jjslave=0
	do 200 islave=1,nslave
	  if(idet(islave).eq.0) go to 200
	  if(istats(islave).eq.0) then
            if((yflag.eq.0).or.(slavegroup(islave)(nbb:nbb).eq.'y'))then
              itemp2=itemp2+1
              if(itemp2.eq.1) then
                itemp1=iruntme(islave)
                jjslave=islave
              else
                if(iruntme(islave).lt.itemp1)then
                  itemp1=iruntme(islave)
                  jjslave=islave
                end if
              end if
            end if
	  end if
200	continue
	if(itemp2.ne.0)then
	  islave=jjslave

C -- First we make sure that the observation flag file has been deleted.

	  afile=trim(asldir(islave))//oreadfle
#ifdef UNIX
	  open(unit=irsf,file=trim(afile),
     +    status='old',iostat=ierr)
#else
#ifdef LF90
	  open(unit=irsf,file=trim(afile),
     +    action='readwrite,denynone',status='old',
     +    blocksize=1,iostat=ierr)
#else
#ifdef INTEL
	  open(unit=irsf,file=trim(afile),
     +    blocksize=1,status='old',shared,iostat=ierr)
#else
	  open(unit=irsf,file=trim(afile),
     +    blocksize=1,status='old',iostat=ierr)
#endif
#endif
#endif
	  if(ierr.eq.0)then
	    call closefile(jfail,irsf,2,afile,cline)
	    if(jfail.ne.0) go to 9891
	    call pestwait(iiwait)
	    call delfile(jfail,irsf,afile,cline)
	    if(jfail.ne.0) go to 9891
	    go to 9300
	  end if
	  go to 1000
	end if

C -- See if any runs are complete.

210	do 500 islave=1,nslave
          if(idet(islave).eq.0) go to 500
	  afile=trim(asldir(islave))//oreadfle
#ifdef UNIX
	  open(unit=irsf,file=trim(afile),
     +    status='old',iostat=ierr)
#else
#ifdef LF90
	  open(unit=irsf,file=trim(afile),
     +    action='readwrite,denynone',status='old',
     +    blocksize=1,iostat=ierr)
#else
#ifdef INTEL
	  open(unit=irsf,file=trim(afile),
     +    blocksize=1,status='old',shared,iostat=ierr)
#else
	  open(unit=irsf,file=trim(afile),
     +    blocksize=1,status='old',iostat=ierr)
#endif
#endif
#endif
	  if(ierr.eq.0)then
	    call closefile(jfail,irsf,2,afile,cline)
	    if(jfail.ne.0) go to 9891
	    call pestwait(iiwait)
	    call delfile(jfail,irsf,afile,cline)
	    if(jfail.ne.0) go to 9891
	    call getsecs(now,0)
	    if(istats(islave).eq.150) go to 500
	    if(istats(islave).eq.-999) then !doing a run left over from before
	      write(irmr,220,err=9900) result,
     +        trim(aslave(islave))
c	      write(6,220,err=9900) result,
c     +        trim(aslave(islave))
220	      format(3x,a,
     +        ':- agent "',a,'" finished execution; old run so results ',
     +        'not needed.')
	      go to 250
	    end if
cccc            write(irmr,*) ' islave = ',islave        !debug
cccc            write(irmr,*) ' nslave = ',nslave        !debug
cccc            do jjjj=1,nslave                         !debug
cccc              write(irmr,*) jrun(jjjj)               !debug
cccc            end do                                   !debug
            if(jrun(islave).eq.0) then
              call getsecs(now,0)
              write(irmr,9310) result,
     +        trim(aslave(islave))
              go to 500
            end if
	    if(istatr(jrun(islave)).eq.-99) then  !already downloaded
	      write(irmr,230,err=9900) result,
     +        trim(aslave(islave))
c	      write(6,230,err=9900) result,
c     +        trim(aslave(islave))
230	      format(3x,a,
     +        ':- agent "',a,'" finished execution; same run already ',
     +        'completed by another agent.')
	      go to 250
	    end if
	    write(irmr,240,err=9900) result,
     +      trim(aslave(islave))
c	    write(6,240,err=9900) result,
c     +      trim(aslave(islave))
240	    format(3x,a,
     +      ':- agent "',a,'" finished execution; reading results.')
	    ncall=ncall+1
            if(repeatrun.eq.1)then
	      final=0
	    else
	      final=1
	    end if
	    icount=0
245	    iitrial=itrial(jrun(islave))
	    mcall=mcall+1
	    if(noptmax.eq.-3)then
	      if(psuccess.eq.0)then
	        mcall_keep=mcall
	        mcall=1
	      end if
	    end if
	    call poutrd(jfail,ifail,final,ninstr,noufle,asize,numl,
     +      nobs,nblbmx,lcins,ll,obsn1,obsn2,iiobs,
     +      jrun(islave),aobs,a,mrkdel,cline,buf,
     +      soufle(1,islave),iitrial,maxtrial,imsfle,asldir(islave),
     +      workvecdim,workvec)
            if(noptmax.eq.-3)then
              if(psuccess.eq.0)then
                mcall=mcall_keep
              end if
              if(jfail.eq.0) psuccess=1
            end if
            if(jfail.ne.0) then
              if ((lamforgive.eq.1).and.(lamid.eq.1))then
                ifail=0
                jfail=0
                forgiveflag=1
              else
                go to 9891
              end if
            end if
	    if(ifail.eq.1) then
              mcall=mcall-1
	      icount=icount+1
	      call getsecs(now,0)
	      write(irmr,247,err=9900) result,
     +        trim(aslave(islave))
c	      write(6,247,err=9900) result,
c     +        trim(aslave(islave))
247	      format(3x,a,
     +        ':- problem (either model or communications) ',
     +        'in reading results from agent "',a,'".')
	      do 248 ii=1,6
                call pestwait(200)
	        call stopress(0)
	        if(istop.eq.2) then
                  if(irestart.ne.0)close(unit=ptunit,iostat=ierr)
                  return
                end if
248	      continue
	      if(icount.ge.numrepeatrun) final=1
	      go to 245
	    end if
	    if(ifail.eq.2)then
              mcall=mcall-1
	      itrial(jrun(islave))=itrial(jrun(islave))+1
	      istats(islave)=0
	      istatr(jrun(islave))=0
	      call getsecs(now,0)
#ifdef INTEL
              write(6,*)
#endif
	      write(6,251,err=9900)
     +        trim(aslave(islave))
251	      format('  Problem encountered in reading results of ',
     +        'run on agent "',
     +        a,'".',/,'  Will carry out run again.')
249	      format(3x,a,':- problems continuing with run on agent "',
     +        a,'". Will run model again.')
	      write(irmr,249,err=9900) result,
     +        trim(aslave(islave))
              write(6,350,err=9900)
              write(irmr,350,err=9900)
350           format('  Error message follows:-')
              call write_pest_message(1)
              write(6,*)
	      go to 50
	    end if
	    istatr(jrun(islave))=-99
	    iirun=iirun+1
            itemps=jrun(islave)
            if(forgiveflag.ne.0)then
              do jj=1,nobs
                workvec(jj)=-1.11d35
              end do
              call store_parallel_register
     +        (jjfail,nobs,ippo,itemps,workvec,obsregfile)
              if(jjfail.ne.0) go to 9891
              forgiveflag=0
            end if
            if(irestart.ne.0)then
              write(ptunit,iostat=ierr) itemps
              write(ptunit,iostat=ierr) (workvec(jj),jj=1,nobs)
#ifdef FLUSHFILE
              call flush(ptunit)
#endif
            end if
	    if(nrun.gt.1) then
	      call writint(atemp,iirun)
#ifdef UNIX
	      write(ijrf,254) trim(atemp)
254	      format('    ',a,' Jacobian runs completed.')
#ifdef FLUSHFILE
              call flush(ijrf)
#endif
#else
#ifdef INTEL
              if((iirun.eq.1).or.(icflag.eq.1))then
                write(6,2561)
2561            format('    - number of runs completed...')
                write(6,'(a)',advance='no') '     '
                icflag=0
              end if
              write(6,2571) iirun
2571          format(i6,$)
              if((iirun.eq.nrun).or.(mod(iirun,12).eq.0))then
                write(6,*)
                write(6,'(a)',advance='no') '     '
              end if
#else
	      write(6,255) trim(atemp)
255	      format('+   ',a,' runs completed.')
#endif
#endif
	    end if
250	    continue
	    itemp1=iruntme(islave)
	    iruntme(islave)=now-istrtme(islave)
	    if(iruntme(islave).le.0) iruntme(islave)=1
	    istats(islave)=0

C -- If this is the first parallel call, all estimated run times are upgraded.

260	    continue
	    nnrun=nnrun+1
            if(nnrun.eq.1)then
              do 600 jslave=1,nslave
                if(islave.ne.jslave)then
                  iruntme(jslave)=iruntme(jslave)*
     +            float(iruntme(islave))/float(itemp1)
                  if(iruntme(jslave).le.0) iruntme(jslave)=1
	        end if
600	      continue
            end if
            do irun=1,nrun                       !jan 2005
              if(istatr(irun).eq.0) go to 151
            end do
	    do 300 irun=1,nrun
	      if(istatr(irun).gt.-99) go to 500   ! used to be 50
300	    continue
	    go to 9000
	  end if
500	continue

	call pestwait(iiwait)
	go to 50

C -- Maybe we will start another PEST run.

C -- If a slave is free and there is a run to do we give the slave that run.

1000	continue

	do 1100 irun=1,nrun
	  if(istatr(irun).eq.0) then
	    call getsecs(istrtme(islave),0)
	    write(irmr,1070) result,
     +      trim(aslave(islave))
c	    write(6,1070) result,
c     +      trim(aslave(islave))
1070	    format(3x,a,
     +      ':- agent "',a,'" commencing model run.')
#ifdef UNIX
#ifdef FLUSHFILE
	    call flush(irmr)
#endif
#endif
	    call pinwrit(jfail,ifail,npar,precis,nopnt,ninfle,nw,
     +      irun,scale,offset,pardel,pword,tmpfle,apar,
     +      cline,asldir(islave),afile,noufle,sinfle(1,islave),
     +      soufle(1,islave),imsfle,incpar(irun),itrans,nobs,
     +      workvecdim,workvec)
            if(jfail.ne.0) go to 9891
	    if(ifail.eq.1) go to 9200
	    call pestwait(iiwait)
#ifdef SCRIPT
#ifdef UNICOS
	    call writint(atemp,islave)
	    iitemp=ishell('qsub -z pslave.script.'//
     +      trim(atemp))
#endif
#else
	    afile=trim(asldir(islave))//preadfle
#ifdef UNIX
	    open(unit=ipcm,file=trim(afile),err=9200)
#else
#ifdef LF90
	    open(unit=ipcm,file=trim(afile),
     +      action='readwrite,denynone',blocksize=1,err=9200)
#else
#ifdef INTEL
	    open(unit=ipcm,file=trim(afile),blocksize=1,shared,
     +      err=9200)
#else
	    open(unit=ipcm,file=trim(afile),blocksize=1,err=9200)
#endif
#endif
#endif
	    write(ipcm,1050,err=9199)
1050	    format('P')
	    call closefile(jfail,ipcm,2,afile,cline)
	    if(jfail.ne.0) go to 9891
#endif
	    istatr(irun)=99
	    istats(islave)=99
	    jrun(islave)=irun
	    go to 50
	  end if
1100	continue

#ifdef SCRIPT
	go to 210  !All runs are batched so being overdue is not an issue.
#endif

C -- There is a slave free. Can we redo a run on a faster machine?

	call getsecs(now,0)
	itemp3=0
	jjslave=0
	do 1200 jslave=1,nslave
	  if(idet(jslave).eq.0) go to 1200
	  if(jslave.eq.islave) go to 1200
	  if(istats(jslave).ne.99) go to 1200
	  itemp4=istrtme(jslave)+iruntme(jslave)
	  if(itemp4.gt.itemp3)then
	    itemp3=itemp4
	    jjslave=jslave
	  end if
1200	continue
	if(jjslave.eq.0) go to 1500
	itemp3=itemp3-now
	if(itemp3.lt.2) go to 1500		! too small to worry about
	if(float(itemp3).gt.run_slow_fac*float(iruntme(islave)))then
	  irun=jrun(jjslave)
	  istrtme(islave)=now
	  write(irmr,1230) result,
     +    trim(aslave(islave)),
     +    trim(aslave(jjslave))
c	  write(6,1230) result,
c     +    trim(aslave(islave)),
c     +    trim(aslave(jjslave))
1230	  format(3x,a,
     +      ':- agent "',a,'" commencing model run because it can ',
     +    'complete it before agent "',a,'".')
	  call pinwrit(jfail,ifail,npar,precis,nopnt,ninfle,nw,
     +    irun,scale,offset,pardel,pword,tmpfle,apar,
     +    cline,asldir(islave),afile,noufle,sinfle(1,islave),
     +    soufle(1,islave),imsfle,incpar(irun),itrans,nobs,
     +    workvecdim,workvec)
          if(jfail.ne.0) go to 9891
	  if(ifail.eq.1) go to 9200
	  call pestwait(iiwait)
#ifdef SCRIPT
#ifdef UNICOS
	  call writint(atemp,islave)
	  iitemp=ishell('qsub -z pslave.script.'//
     +    trim(atemp))
#endif
#else
	  afile=trim(asldir(islave))//preadfle
#ifdef UNIX
	  open(unit=ipcm,file=trim(afile),err=9200)
#else
#ifdef LF90
	  open(unit=ipcm,file=trim(afile),
     +    action='readwrite,denynone',blocksize=1,err=9200)
#else
#ifdef INTEL
	  open(unit=ipcm,file=trim(afile),blocksize=1,shared,
     +    err=9200)
#else
	  open(unit=ipcm,file=trim(afile),blocksize=1,err=9200)
#endif
#endif
#endif
	  write(ipcm,1050,err=9199)
	  call closefile(jfail,ipcm,2,afile,cline)
	  if(jfail.ne.0) go to 9891
#endif
	  istatr(irun)=99
	  istats(islave)=99
	  jrun(islave)=irun
	  istats(jjslave)=100			!so only one slave does it
	  go to 210
	end if

C -- Are any runs overdue?

1500	continue
	itemp3=0
	jjslave=0
	do 1700 jslave=1,nslave
	  if(idet(jslave).eq.0) go to 1700
	  if(jslave.eq.islave) go to 1700
	  if(istats(jslave).ne.99) go to 1700
	  itemp4=istrtme(jslave)+iruntme(jslave)-now
	  if(itemp4.lt.itemp3) then
	    itemp3=itemp4
	    jjslave=jslave
	  end if
1700	continue
	if((jjslave.eq.0).or.
C     +     (float(itemp3).gt.-0.5*float(iruntme(islave)))) then         !arbitrary
     +     (float(itemp3).gt.-run_slow_fac*float(iruntme(islave))))then          ! altered for LCRA model
	  go to 210
	end if
	irun=jrun(jjslave)
	istrtme(islave)=now
	write(irmr,1520) result,
     +  trim(aslave(islave)),
     +  trim(aslave(jjslave))
c	write(6,1520) result,
c     +  trim(aslave(islave)),
c     +  trim(aslave(jjslave))
1520	format(3x,a,
     +  ':- agent "',a,'" commencing model run because agent "',
     +  a,'" is overdue.')
	call pinwrit(jfail,ifail,npar,precis,nopnt,ninfle,nw,
     +  irun,scale,offset,pardel,pword,tmpfle,apar,cline,
     +  asldir(islave),afile,noufle,sinfle(1,islave),
     +  soufle(1,islave),imsfle,incpar(irun),itrans,nobs,
     +  workvecdim,workvec)
        if(jfail.ne.0) go to 9891
	if(ifail.eq.1) go to 9200
	call pestwait(iiwait)
#ifdef SCRIPT
#ifdef UNICOS
	call writint(atemp,islave)
	iitemp=ishell('qsub -z pslave.script.'//
     +  trim(atemp))
#endif
#else
	afile=trim(asldir(islave))//preadfle
#ifdef UNIX
	open(unit=ipcm,file=trim(afile),err=9200)
#else
#ifdef LF90
	open(unit=ipcm,file=trim(afile),
     +  action='readwrite,denynone',blocksize=1,err=9200)
#else
#ifdef INTEL
	open(unit=ipcm,file=trim(afile),blocksize=1,shared,
     +  err=9200)
#else
	open(unit=ipcm,file=trim(afile),blocksize=1,err=9200)
#endif
#endif
#endif
	write(ipcm,1050,err=9199)
	call closefile(jfail,ipcm,2,afile,cline)
	if(jfail.ne.0) go to 9891
#endif
	istatr(irun)=99
	istats(islave)=99
	jrun(islave)=irun
	istats(jjslave)=100		!so only one slave does it
	go to 210

C -- Before returning we re-assign the status of incompleted runs.
C -- Also, the signal files are deleted. If they re-appear PEST will
C    use these slaves again.

9000	do 9100 islave=1,nslave
          if(idet(islave).eq.0) go to 9100
	  if((istats(islave).eq.99).or.(istats(islave).eq.100))then
	    afile=trim(asldir(islave))//sreadfle
	    call delfile(jfail,-irsf-1000,afile,cline)
	    if(jfail.ne.0) go to 9891
            istats(islave)=-999
          end if
9100	continue
	go to 9999

9199	call closefile(jfail,ipcm,0,afile,cline)
        if(jfail.ne.0) go to 9891
9200	call getsecs(istrtme(islave),0)
	write(irmr,9210) result,
     +  trim(aslave(islave))
c	write(6,9210) result,
c     +  trim(aslave(islave))
9210	format(3x,a,
     +  ':- cannot write to agent "',a,'" to commence model run.')
	istats(islave)=150
	go to 50

9300	call getsecs(istrtme(islave),0)
	write(irmr,9310) result,
     +  trim(aslave(islave))
c	write(6,9310) result,
c     +  trim(aslave(islave))
9310	format(3x,a,
     +  ':- previous observ.rdy file on agent "',a,'" not deleted.')
	istats(islave)=150
	go to 50

9900	flenme=manfle
	call stperr(116,1,' ',0,' ',cline)
	go to 9891

9400	flenme=trim(afile)
	call stperr(118,1,' ',0,' ',cline)
	go to 9891

9999    continue
        if(irestart.ne.0) close(unit=ptunit,iostat=ierr)
        return

9891    jfail=1
        return

	end

#endif
#endif GENIE



#ifdef MPEST

        subroutine doruns_m(jfail,nslave,nrun,
     +  asldir,ninstr,noufle,asize,numl,nobs,nblbmx,lcins,
     +  ll,obsn1,obsn2,iiobs,aobs,a,mrkdel,cline,buf,afile,
     +  npar,precis,nopnt,ninfle,nw,
     +  scale,offset,pardel,pword,tmpfle,apar,
     +  outfle,insfle,ldsin,ldsou,sinfle,
     +  soufle,itrial,imsfle,incpar,itrans,msinfle,msoufle,modfle,
     +  workvecdim,workvec)

#ifdef INTEL
        USE COMMON_MOD ONLY: ISTOP,FLNME,NCALL,MCALL,IWAIT
#endif
        use pestdata, only : inst,errmsg
        implicit none

#ifdef INTEL
        integer nslave,nrun,ninstr,noufle,asize,numl,nobs,nblbmx,
     +  npar,precis,nopnt,ninfle,
     +  ierr,ldsin,ldsou,ifail,final,
     +  ii,iitrial,imsfle,jfail,ifle,mm
#else
        integer nslave,nrun,ninstr,noufle,asize,numl,nobs,nblbmx,
     +  istop,npar,precis,nopnt,ninfle,mcall,
     +  ncall,ierr,ldsin,ldsou,ifail,final,iwait,
     +  ii,iitrial,imsfle,jfail,ifle,mm
#endif
        integer workvecdim
        integer lcins(ninstr),ll(numl),
     +  obsn1(nobs),obsn2(nobs),iiobs(nobs),
     +  nw(npar),itrial(nrun),
     +  incpar(nrun),itrans(npar)
        double precision scale(npar),offset(npar)
        double precision workvec(workvecdim)
        character a(asize),mrkdel(noufle),pardel(ninfle)
        character*(*) aobs(nobs),pword(npar),
     +  tmpfle(ninfle),apar(npar),outfle(noufle),
     +  insfle(noufle)
        character*(*) asldir(nslave),sinfle(ldsin,nslave),
     +  soufle(ldsou,nslave),msinfle(ldsin),msoufle(ldsou)
        character*(*) modfle
        character*(*) cline,buf,afile
#ifndef INTEL
        character*200 flenme
#endif
        character*5 atemp

        integer irun,ins,j,isum,i,nblc,maxtrial
        integer npnt

#ifndef INTEL
        common /flenme/flenme
        common /sstop/istop
        common /modcal/ncall,mcall
        common /waitint/iwait
#endif

        jfail=0
        maxtrial=1
        do i=1,nrun
          itrial(i)=maxtrial
        end do

        if(mcall.eq.0) then
          ins=0
          isum=0
          DO 100 I=1,NOUFLe
            CALL FFOPEN(JFAIL,INST,'r',INSFLE(I),78,CLINE)
            IF(JFAIL.NE.0) GO TO 9891
            READ(INST,*)
            INS=INS+1
            CLINE(1:1)=CHAR(2)
            CLINE(2:2)=' '
            CLINE(3:LEN(CLINE))=OUTFLE(I)
            LCINS(INS)=ISUM+1
            NBLC=LEN_TRIM(CLINE)
            DO 20 J=1,NBLC
20          A(J+ISUM)=CLINE(J:J)
            ISUM=ISUM+NBLC
30          READ(INST,22,END=180) CLINE
22          FORMAT(A)
            CALL TABREM(CLINE)
            IF(INDEX(CLINE,MRKDEL(I)).EQ.0) CALL CMPRSS(CLINE)
            NBLC=LEN_TRIM(CLINE)
            IF(NBLC.EQ.0) GO TO 30
            INS=INS+1
            LCINS(INS)=ISUM+1
            DO 70 J=1,NBLC
70          A(J+ISUM)=CLINE(J:J)
            ISUM=ISUM+NBLC
            GO TO 30
180         CLOSE(UNIT=INST)
100       CONTINUE
        end if

C -- Is a "stop file" present?

	call stopress(0)
	if((istop.eq.2).or.(istop.eq.1)) then
          return
        end if

C -- All model output files are deleted.

        do irun=1,nrun
          call writint(atemp,irun)
          do ifle=1,noufle
            mm=npnt(soufle(ifle,1))
C            if(mm.lt.0) then
C              write(errmsg,240) trim(soufle(ifle,1))
C              jfail=2
C              return
C            end if
            if(nrun.eq.1)then
              msoufle(ifle)=soufle(ifle,1)
            else
              if(mm.lt.0)then
                msoufle(ifle)=trim(soufle(ifle,1))//'.'//trim(atemp)
              else
                msoufle(ifle)=soufle(ifle,1)(1:mm)//trim(atemp)//
     +          trim(soufle(ifle,1)(mm:))
              end if
            end if
            call delfile(jfail,-inst,msoufle(ifle),cline)
          end do
        end do

C -- Model input files are named and written.

        do irun=1,nrun
          call writint(atemp,irun)
          do ifle=1,ninfle
            mm=npnt(sinfle(ifle,1))
c            if(mm.lt.0) then
c              write(errmsg,230) trim(sinfle(ifle,1))
c230           format('Model input file "',a,'" does not have an ',
c     +        'extension. MPEST requires that it does.')
c              jfail=2
c              return
c            end if
            if(nrun.eq.1)then
              msinfle(ifle)=sinfle(ifle,1)
            else
              if(mm.lt.0)then
                msinfle(ifle)=trim(sinfle(ifle,1))//'.'//trim(atemp)
              else
                msinfle(ifle)=sinfle(ifle,1)(1:mm)//trim(atemp)//
     +          trim(sinfle(ifle,1)(mm:))
              end if
            end if
          end do
	  call pinwrit(jfail,ifail,npar,precis,nopnt,ninfle,nw,
     +    irun,scale,offset,pardel,pword,tmpfle,apar,
     +    cline,asldir(1),afile,noufle,msinfle,
     +    msoufle,imsfle,incpar(irun),itrans,nobs,
     +    workvecdim,workvec)
          if(jfail.ne.0) go to 9891
	  if(ifail.eq.1) go to 9200
        end do

! -- The model command is issued.

        call writint(atemp,nrun)
        call system(trim(modfle)//' '//trim(atemp))

! -- Output files are read.

        do irun=1,nrun
          mcall=mcall+1
          ncall=ncall+1
          call writint(atemp,irun)
          do ifle=1,noufle
            mm=npnt(soufle(ifle,1))
c            if(mm.lt.0) then
c              write(errmsg,240) trim(soufle(ifle,1))
c240           format('Model output file "',a,'" does not have an ',
c     +        'extension. MPEST requires that it does.')
c              jfail=2
c              return
c            end if
            if(nrun.eq.1)then
              msoufle(ifle)=soufle(ifle,1)
            else
              if(mm.lt.0)then
                msoufle(ifle)=trim(soufle(ifle,1))//'.'//trim(atemp)
              else
                msoufle(ifle)=soufle(ifle,1)(1:mm)//trim(atemp)//
     +          trim(soufle(ifle,1)(mm:))
              end if
            end if
          end do
          final=1
          iitrial=itrial(irun)
          call poutrd(jfail,ifail,final,ninstr,noufle,asize,numl,
     +    nobs,nblbmx,lcins,ll,obsn1,obsn2,iiobs,
     +    irun,aobs,a,mrkdel,cline,buf,
     +    msoufle,iitrial,maxtrial,imsfle,asldir(1),
     +    workvecdim,workvec)
          if(jfail.ne.0) go to 9891
          if(ifail.ne.0) go to 9891

C -- Delete model input files (model output files have already been
C    deleted in subroutine poutrd).

          do ifle=1,ninfle
            mm=npnt(sinfle(ifle,1))
c            if(mm.lt.0) then
c              write(errmsg,230) trim(sinfle(ifle,1))
c              jfail=2
c              return
c            end if
            if(nrun.eq.1)then
              afile=trim(sinfle(ifle,1))
            else
              if(mm.lt.0)then
                afile=trim(sinfle(ifle,1))//'.'//trim(atemp)
              else
                afile=sinfle(ifle,1)(1:mm)//trim(atemp)//
     +          trim(sinfle(ifle,1)(mm:))
              end if
            end if
            call delfile(jfail,-inst,afile,cline)
	    if(jfail.ne.0) go to 9891
          end do
        end do

        return

9200    write(errmsg,9210)
9210    format('An error was encountered in writing a model ',
     +  'input dataset for a parallel model run.')
        go to 9891

9891    jfail=1
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

#endif


	subroutine getsecs(isecs,init)

#ifdef INTEL
        USE COMMON_MOD, ONLY: m1,d1,y1,h1,min1,ss1,result
#endif
        implicit none

#ifdef INTEL
	integer isecs,mm,dd,yy,hh,min,ndays,numdays,
     +  init
	real ss
	character*11 result1,result2
#else
	integer isecs,mm,dd,yy,hh,min,m1,d1,y1,h1,min1,ndays,numdays,
     +  init
	real ss,ss1
	character*11 result,result1,result2
#endif

#ifndef INTEL
	common /runtime/ m1,d1,y1,h1,min1,ss1
	common /newtime/ result
#endif

	call date_and_time(date=result1,time=result2)
	read(result1,'(i4,i2,i2)') yy,mm,dd
	read(result2,'(i2,i2,f6.0)') hh,min,ss
	write(result,'(i2.2,'':'',i2.2,'':'',f5.2)') hh,min,ss
	if(init.eq.0) then
	  ndays=numdays(d1,m1,y1,dd,mm,yy)
	  isecs=ndays*86400+(hh-h1)*3600+(min-min1)*60+ss-ss1
	else
	  m1=mm
	  d1=dd
	  y1=yy
	  h1=hh
	  min1=min
	  ss1=ss
	end if

	return
	end


        SUBROUTINE ZINCTEST(JFAIL,NOCHNG,NPAR,PRECIS,NOPNT,NW,IPPP,
     +  PVAL,TPVAL,SCALE,OFFSET,PWORD,APAR,CLINE,IGO,X1)

        IMPLICIT NONE

        INTEGER NPAR,PRECIS,NOPNT,IPP,ISC,IFAIL,IGO,NOCHNG,IPPP,I,
     +  JFAIL
        INTEGER NW(NPAR)
        DOUBLE PRECISION TVAL,X1
        DOUBLE PRECISION PVAL(NPAR),SCALE(NPAR),OFFSET(NPAR),
     +  TPVAL(NPAR)
        CHARACTER*(*) PWORD(NPAR)
        CHARACTER*(*) APAR(NPAR)
        CHARACTER*(*) CLINE

        JFAIL=0
	NOCHNG=0
        DO 100 IPP=1,NPAR
        IF(SCALE(IPP).LT.-1.0D35) GO TO 100
        ISC=0
        IF((SCALE(IPP).NE.1.0D0).OR.(OFFSET(IPP).NE.0.0D0))THEN
          PVAL(IPP)=PVAL(IPP)*SCALE(IPP)+OFFSET(IPP)
          ISC=1
        END IF
        CALL WRTSIG(IFAIL,PVAL(IPP),PWORD(IPP),NW(IPP),PRECIS,TVAL,
     +  NOPNT)
        IF(IFAIL.NE.0) THEN
          CALL STPERR(33+IFAIL,1,APAR(IPP),0,' ',CLINE)
          GO TO 9891
        END IF
        IF(ISC.EQ.1) THEN
          PVAL(IPP)=(TVAL-OFFSET(IPP))/SCALE(IPP)
        ELSE
          PVAL(IPP)=TVAL
        END IF
100     CONTINUE

        IF(IPPP.GT.0) THEN
          IF((TPVAL(IPPP).EQ.PVAL(IPPP)).OR.
     +      ((IGO.EQ.2).AND.(PVAL(IPPP).EQ.X1)))THEN
            NOCHNG=1
            RETURN
          END IF
        ELSE
          DO 510 I=1,NPAR
          IF(TPVAL(I).NE.PVAL(I)) GO TO 520
510       CONTINUE
          NOCHNG=1
          RETURN
        END IF

520     RETURN

9891    JFAIL=1
        RETURN
        END



        SUBROUTINE pINWRIT(JFAIL,iifail,NPAR,PRECIS,NOPNT,NINFL,NW,
     +  irun,SCALE,OFFSET,MARK,PWORD,INTEMP,APAR,CLINE,asldir,afile,
     +  noufl,sinfle,soufle,imsfle,incpar,itrans,nobs,
     +  workvecdim,pval)

C -- SUBROUTINE INWRIT WRITES MODEL INPUT FILES PRIOR TO RUNNING THE MODEL

#ifdef INTEL
        USE COMMON_MOD, ONLY: IPERR,JPERR
#endif
        USE PESTDATA, ONLY : INST,IMIF,IMMF,parregfile,ippp
        IMPLICIT NONE

        LOGICAL LEXIST
#ifdef INTEL
        INTEGER NPAR,PRECIS,NINFL,IFILE,J2,LC,J1,IPAR,ISC,NOPNT,
     +  J,noufl,nb,iifail,ierr,imsfle,incpar,itype,nobs,
     +  JFAIL
#else
        INTEGER NPAR,PRECIS,NINFL,IFILE,J2,LC,J1,IPAR,ISC,NOPNT,
     +  J,noufl,nb,iifail,iperr,jperr,ierr,imsfle,incpar,itype,nobs,
     +  JFAIL
#endif
        integer workvecdim,irun,jjfail
        INTEGER NW(NPAR),itrans(npar)
        INTEGER IFAIL,IPP,ILINE
        DOUBLE PRECISION TVAL
        DOUBLE PRECISION SCALE(NPAR),OFFSET(NPAR)
        double precision pval(workvecdim)
        CHARACTER*12 TPAR
        CHARACTER*(*) CLINE,asldir,afile
        CHARACTER*(*) PWORD(NPAR),APAR(NPAR)
        CHARACTER*(*) INTEMP(NINFL)
        CHARACTER MARK(NINFL)
	character*(*) sinfle(ninfl),soufle(noufl)

#ifndef INTEL
	common /pperr/iperr,jperr
#endif

	iifail=0
	JFAIL=0

C -- BEFORE ALL ELSE PREVIOUS MODEL OUTPUT FILES ARE DELETED

        NB=LEN_TRIM(ASLDIR)
c        DO 10 J=1,NOUFL
c          AFILE=trim(soufle(j))
c          INQUIRE(FILE=TRIM(AFILE),EXIST=LEXIST,
c     +    iostat=ierr)
c	  if(ierr.ne.0) then
c	    iifail=1
c	    return
c	  end if
c          IF(LEXIST)
c     +    CALL SYSTEM('DEL "'//TRIM(AFILE)//'" > NUL')
c10      CONTINUE


C -- Parameter values are retrieved from the parameter run register.

         call retrieve_parallel_register
     +   (jjfail,npar,ippp,irun,pval,parregfile)
         if(jfail.ne.0) go to 9891

C -- THE MESSAGE FILE IS WRITTEN (IF APPROPRIATE)

         if(imsfle.eq.1)then
           afile=trim(asldir)//'pest.mmf'
#ifdef UNIX
           open(unit=immf,file=afile,action='write',iostat=ierr)
#else
#ifdef LF90
           open(unit=immf,file=afile,action='write,denynone',
     +     iostat=ierr)
#else
#ifdef INTEL
           open(unit=immf,file=afile,action='write',shared,iostat=ierr)
#else
           open(unit=immf,file=afile,action='write',iostat=ierr)
#endif
#endif
#endif
           if(ierr.ne.0) then
             close(unit=immf,iostat=ierr)
             go to 10
           end if
           if(incpar.eq.0)then
             itype=1
           else
             itype=3
           end if
           call pestmess(jfail,immf,itype,npar,nobs,incpar,itrans,
     +     apar,pval,cline,afile)
           if(jfail.ne.0) go to 9891
           call closefile(jfail,immf,2,afile,cline)
           if(jfail.ne.0) go to 9891
         end if
10       continue


C -- EACH OF THE PARAMETER WORDS IS FILLED

        IPAR=1
        DO 100 IPP=1,NPAR
        IF(SCALE(IPP).LT.-1.0D35)CYCLE
        ISC=0
        IF((SCALE(IPP).NE.1.0D0).OR.(OFFSET(IPP).NE.0.0D0))THEN
          PVAL(IPP)=PVAL(IPP)*SCALE(IPP)+OFFSET(IPP)
          ISC=1
        END IF
        CALL WRTSIG(IFAIL,PVAL(IPP),PWORD(IPP),NW(IPP),PRECIS,TVAL,
     +  NOPNT)
        IF(IFAIL.NE.0) THEN
          CALL STPERR(33+IFAIL,1,APAR(IPP),0,' ',CLINE)
          GO TO 9891
        END IF
        IF(ISC.EQ.1) THEN
          PVAL(IPP)=(TVAL-OFFSET(IPP))/SCALE(IPP)
        ELSE
          PVAL(IPP)=TVAL
        END IF
100     CONTINUE

C -- NEXT THE SUBSTITUTIONS IN THE TEMPLATE FILES ARE MADE

        DO 500 IFILE=1,NINFL
        CALL FFOPEN(JFAIL,INST,'r',INTEMP(IFILE),37,CLINE)
        IF(JFAIL.NE.0) GO TO 9891
	afile=trim(sinfle(ifile))
	jperr=1
        CALL FFOPEN(JFAIL,IMIF,'w',trim(afile),38,CLINE)
        if(jfail.ne.0) go to 9891
	jperr=0
	if(iperr.eq.1)then
	  iifail=1
	  go to 450
	end if
        READ(INST,*)
        ILINE=1
120     ILINE=ILINE+1
        READ(INST,22,END=400) CLINE
22      FORMAT(A)
        LC=LEN_TRIM(CLINE)
        J2=0
150     IF(J2.GE.LC) GO TO 300
        J1=INDEX(CLINE(J2+1:LC),MARK(IFILE))
        IF(J1.EQ.0) GO TO 300
        J1=J1+J2
        J2=INDEX(CLINE(J1+1:LC),MARK(IFILE))
        J2=J2+J1
        CALL PARNAM(IFAIL,J1,J2,TPAR,CLINE)
        CALL WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)
c       The following works when space bigger than pword(:nblnk(pword))
c       cline(j1:j2)=pword(ipar)(:nblnk(pword(ipar)))
        DO 160 J=J1,J2
160     CLINE(J:J)=' '
        J=LEN_TRIM(PWORD(IPAR))
        CLINE(J2-J+1:J2)=PWORD(IPAR)(1:J)
        GO TO 150

300     WRITE(IMIF,22,ERR=320) CLINE(:MAX(LEN_TRIM(CLINE),1))
        GO TO 120
320     iifail=1
400     call closefile(jfail,imif,2,afile,cline)
        if(jfail.ne.0) go to 9891
450	CLOSE(UNIT=INST)
	if(iifail.eq.1) return
500     CONTINUE

C -- Parameter values are stored in the register again in case they
C    have been altered.

        call store_parallel_register
     +  (jjfail,npar,ippp,irun,pval,parregfile)
        if(jfail.ne.0) go to 9891

        RETURN

9891    JFAIL=1
        RETURN

        END




        SUBROUTINE pOUTRD(jfail,iifail,final,NINSTR,NOUFL,ASIZE,NUML,
     +  NOBS,NBLBMX,LCINS,LL,OBSN1,OBSN2,IIOBS,irun,AOBS,A,MRKDEL,
     +  CLINE,BUF,soufle,itrial,maxtrial,imsfle,asldir,
     +  workvecdim,workvec)

C -- SUBROUTINE OUTRD READS MODEL OUTPUT FILES USING INTRUCTIONS

        USE PESTDATA, ONLY : INST,IMMF,ippo,obsregfile
#ifdef INTEL
        USE COMMON_MOD, ONLY: FLENME,MCALL,NCALL,IWAIT,IPERR,JPERR
#endif
        IMPLICIT NONE

#ifdef INTEL
        INTEGER CIL,IFAIL,ASIZE,NINSTR,iifail,
     +  final,iiwait,itrial,maxtrial,imsfle,jfail
#else
        INTEGER MCALL,CIL,IFAIL,ASIZE,NINSTR,NCALL,iifail,iperr,jperr,
     +  final,iwait,iiwait,itrial,maxtrial,imsfle,jfail
#endif
        INTEGER INS,NBLB,NBLC,I,J,N1,N2,NOL,MRKTYP,J1,J2,
     +  NUML,JOBS,IOBS,NOBS,N3,NUM1,NUM2,IFILE,IL,NOUFL,NBLBMX,
     +  INSNUM,ALMARK,BEGINS,INSFLE,DUMFLG
        integer iflag
        integer workvecdim,irun
        INTEGER LCINS(NINSTR)
        INTEGER LL(NUML),OBSN1(NOBS),OBSN2(NOBS),IIOBS(NOBS)
        double precision workvec(workvecdim)
        DOUBLE PRECISION RTEMP
        CHARACTER*15 FMT,OBSNAM*20,MKRDEL*1,AA*1
        CHARACTER MRKDEL(NOUFL)
        CHARACTER A(ASIZE)
        CHARACTER*(*) AOBS(NOBS)
        CHARACTER*(*) CLINE,BUF,asldir
#ifndef INTEL
        CHARACTER*200 FLENME
#endif
	character*200 afile
	character*(*) soufle(noufl)

#ifndef INTEL
        COMMON /FLENME/FLENME
        COMMON /MODCAL/NCALL,MCALL
	common /pperr/ iperr,jperr
	common /waitint/iwait
#endif

        jfail=0
	iiwait=iwait
	iifail=0
        IFILE=0
        IL=0
        JOBS=0
        MKRDEL=MRKDEL(1)
        CIL=0
        IOBS=1
        BEGINS=0
        iflag=0

        INS=1
10      IF(INS.LT.NINSTR)THEN
          NBLB=LCINS(INS+1)-LCINS(INS)
        ELSE
          NBLB=ASIZE-LCINS(INS)+1
        END IF
        BUF(1:NBLBMX)=' '
        DO 20 I=1,NBLB
20      BUF(I:I)=A(LCINS(INS)+I-1)
25      N2=0
        INSNUM=0

50      CALL GETINT(IFAIL,BUF,N1,N2,NBLB,MKRDEL)
        IF(IFAIL.NE.0) THEN
          CALL STPERR(68,5,BUF,0,' ',CLINE)
          GO TO 9891
        END IF
51      IF(N1.EQ.0) GO TO 1000
        INSNUM=INSNUM+1
        IF(INSNUM.EQ.1)THEN
          IF(BUF(N1:N1).NE.'&') THEN
            MRKTYP=0
            ALMARK=1
            BEGINS=0
          ELSE
            IF(INS.EQ.INSFLE+1) THEN
              CALL STPERR(73,5,BUF,0,' ',CLINE)
              GO TO 9891
            END IF
            IF(BEGINS.EQ.1)THEN
              INS=INS-1
              GO TO 10
            END IF
          END IF
        END IF
        IF(ICHAR(BUF(N1:N1)).EQ.2)THEN
          IF(IFILE.NE.0) THEN
            call closefile(jfail,inst,2,afile,cline)
            if(jfail.ne.0) go to 9891
          END IF
          DO 60 I=N1+1,NBLB
          IF(BUF(I:I).NE.' ') GO TO 70
60        CONTINUE
70        IFILE=IFILE+1
          FLENME=soufle(ifile)
	  afile=flenme
	  if((final.eq.0).or.(itrial.lt.maxtrial))jperr=1
          CALL FFOPEN(JFAIL,INST,'r',' ',65,CLINE)
          if(jfail.ne.0) go to 9891
	  jperr=0
	  if(iperr.eq.1) then
	    if(final.eq.0)then
	      iifail=1
	    else
	      iifail=2
	    end if
	    call closefile(jfail,inst,0,afile,cline)
	    if(jfail.ne.0) go to 9891
	    return
	  end if
          CIL=0
          MKRDEL=MRKDEL(IFILE)
          INSFLE=INS
          GO TO 1000
        ELSE IF((BUF(N1:N1).EQ.'l').OR.(BUF(N1:N1).EQ.'L'))THEN
          ALMARK=0
          IL=IL+1
          IF(MCALL.EQ.1)THEN
            WRITE(FMT,150) N2-N1
150         FORMAT('(I',I4,')')
            READ(BUF(N1+1:N2),FMT,ERR=9050) NOL
            LL(IL)=NOL
          ELSE
            NOL=LL(IL)
          END IF
          IF(NOL.GT.1) THEN
            DO 160 I=1,NOL-1
	    if((final.eq.1).and.(itrial.ge.maxtrial))then
              iflag=0
              READ(INST,*,END=9100)
	    else
              iflag=1
              READ(INST,*,END=9100)
	    end if
160         CIL=CIL+1
          END IF
	  if((final.eq.1).and.(itrial.ge.maxtrial)) then
            iflag=0
            READ(INST,22,END=9100) CLINE
	  else
            iflag=1
            READ(INST,22,END=9100) CLINE
	  end if
22        FORMAT(A)
          IF(INDEX(CLINE,CHAR(9)).NE.0) CALL TABREP(CLINE)
          CIL=CIL+1
          NBLC=LEN_TRIM(CLINE)
          MRKTYP=1
          J1=0
        ELSE IF(BUF(N1:N1).EQ.MKRDEL)THEN
          IF(MRKTYP.EQ.0)THEN
200	    if((final.eq.1).and.(itrial.ge.maxtrial))then
              iflag=0
              READ(INST,22,END=9100) CLINE
	    else
              iflag=1
              READ(INST,22,END=9100) CLINE
	    end if
            IF(INDEX(CLINE,CHAR(9)).NE.0) CALL TABREP(CLINE)
            CIL=CIL+1
            J1=INDEX(CLINE,BUF(N1+1:N2-1))
            IF(J1.EQ.0) GO TO 200
            NBLC=LEN_TRIM(CLINE)
            J1=J1+N2-N1-2
            MRKTYP=1
          ELSE
            IF(J1.GE.NBLC) THEN
              IF(ALMARK.EQ.1) THEN
                BEGINS=1
                GO TO 25
              END IF
              GO TO 9200
            END IF
            J2=INDEX(CLINE(J1+1:NBLC),BUF(N1+1:N2-1))
            IF(J2.EQ.0) THEN
              IF(ALMARK.EQ.1) THEN
                BEGINS=1
                GO TO 25
              END IF
              GO TO 9200
            END IF
            J1=J1+J2
            J1=J1+N2-N1-2
          END IF
        ELSE IF(BUF(N1:N1).EQ.'&')THEN
          IF(INSNUM.NE.1) THEN
            CALL STPERR(72,5,BUF,0,' ',CLINE)
            GO TO 9891
          END IF
        ELSE IF((BUF(N1:N1).EQ.'w').OR.(BUF(N1:N1).EQ.'W'))THEN
          ALMARK=0
          IF(J1.GE.NBLC) GO TO 9400
          J2=INDEX(CLINE(J1+1:NBLC),' ')
          IF(J2.EQ.0) GO TO 9400
          J1=J1+J2
          DO 210 I=J1,NBLC
          IF(CLINE(I:I).NE.' ') GO TO 220
210       CONTINUE
          I=NBLC+1
220       J1=I-1
        ELSE IF((BUF(N1:N1).EQ.'t').OR.(BUF(N1:N1).EQ.'T'))THEN
          ALMARK=0
          WRITE(FMT,150) N2-N1
          READ(BUF(N1+1:N2),FMT,ERR=9000) J2
          IF(J2.LT.J1) then
            if((final.eq.1).and.(itrial.ge.maxtrial))then
              CALL STPERR(81,4,BUF,CIL,' ',CLINE)
              GO TO 9891
            else
              CALL STPERR(81,4,BUF,CIL,' ',CLINE)
              go to 9950
            end if
          end if
          J1=J2
          IF(J1.GT.NBLC) then
	    if((final.eq.1).and.(itrial.ge.maxtrial))then
	      CALL STPERR(70,4,BUF,CIL,' ',CLINE)
	      GO TO 9891
	    else
	      CALL STPERR(70,4,BUF,CIL,' ',CLINE)
	      go to 9950
	    end if
	  end if
        ELSE IF((BUF(N1:N1).EQ.'[').OR.(BUF(N1:N1).EQ.'('))THEN
          ALMARK=0
          AA=BUF(N1:N1)
          JOBS=JOBS+1
          IF(MCALL.EQ.1)THEN
            IF(AA.EQ.'[')THEN
              N3=INDEX(BUF(N1:N2),']')
            ELSE
              N3=INDEX(BUF(N1:N2),')')
            END IF
	    if(n3.eq.0) THEN
	      call stperr(64,5,buf,0,' ',cline)
	      GO TO 9891
	    END IF
            N3=N3+N1-1
            OBSNAM=BUF(N1+1:N3-1)
            CALL WHICH1(IFAIL,NOBS,IOBS,AOBS,OBSNAM)
            IF(IFAIL.NE.0) GO TO 9700
            CALL GETNUM(IFAIL,BUF,N3,N2,NUM1,NUM2,FMT)
            IF(IFAIL.NE.0) THEN
              CALL STPERR(64,5,BUF,0,' ',CLINE)
              GO TO 9891
            END IF
            OBSN1(JOBS)=NUM1
            OBSN2(JOBS)=NUM2
            IIOBS(JOBS)=IOBS
          ELSE
            NUM1=OBSN1(JOBS)
            NUM2=OBSN2(JOBS)
            IOBS=IIOBS(JOBS)
          END IF
          IF(AA.EQ.'(') THEN
           CALL GETTOT(IFAIL,CLINE,NUM1,NUM2,NBLC)
           IF(IFAIL.NE.0) then
	     if((final.eq.1).and.(itrial.ge.maxtrial)) then
	       CALL STPERR(88,3,
     +         AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),CIL,' ',CLINE)
               GO TO 9891
	     else
	       CALL STPERR(88,3,
     +         AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),CIL,' ',CLINE)
	       go to 9950
	     end if
	   end if
          ELSE
           IF(NUM1.GT.NBLC) then
	     if((final.eq.1).and.(itrial.ge.maxtrial)) then
	       CALL STPERR(88,3,
     +         AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),CIL,' ',CLINE)
               GO TO 9891
	     else
	       CALL STPERR(88,3,
     +         AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),CIL,' ',CLINE)
	       go to 9950
	     end if
	   end if
           IF(NUM2.GT.NBLC) NUM2=NBLC
           IF(CLINE(NUM1:NUM2).EQ.' ') then
	      if((final.eq.1).and.(itrial.ge.maxtrial)) then
	        CALL STPERR(88,3,
     +          AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),CIL,' ',CLINE)
                GO TO 9891
	      else
	        CALL STPERR(88,3,
     +          AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),CIL,' ',CLINE)
	        go to 9950
	      end if
	   end if
          END IF
          WRITE(FMT,250) NUM2-NUM1+1
250       FORMAT('(F',I4,'.0)')
          READ(CLINE(NUM1:NUM2),FMT,ERR=260) workvec(IOBS)
          J1=NUM2
          GO TO 50
260       if((final.eq.1).and.(itrial.ge.maxtrial)) then
	    CALL STPERR(82,3,AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),
     +      CIL,' ',CLINE)
            GO TO 9891
	  else
	    CALL STPERR(82,3,AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),
     +      CIL,' ',CLINE)
	    go to 9950
	  end if
        ELSE IF(BUF(N1:N1).EQ.'!') THEN
          ALMARK=0
          CALL LOWCAS(BUF(N1+1:N2-1))
          IF((N2-N1.NE.4).OR.(BUF(N1+1:N2-1).NE.'dum'))THEN
            JOBS=JOBS+1
            IF(MCALL.EQ.1) THEN
              OBSNAM=BUF(N1+1:N2-1)
              CALL WHICH1(IFAIL,NOBS,IOBS,AOBS,OBSNAM)
              IF(IFAIL.NE.0) GO TO 9700
              IIOBS(JOBS)=IOBS
            ELSE
              IOBS=IIOBS(JOBS)
            END IF
            DUMFLG=0
          ELSE
            DUMFLG=1
          END IF
          CALL GETNXT(IFAIL,CLINE,J1,NUM1,NUM2,NBLC)
          IF(IFAIL.NE.0) THEN
            IF(DUMFLG.EQ.0) THEN
	      if((final.eq.1).and.(itrial.ge.maxtrial)) then
                CALL STPERR(88,3,AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),
     +          CIL,' ',CLINE)
                GO TO 9891
	      else
                CALL STPERR(88,3,AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),
     +          CIL,' ',CLINE)
	        go to 9950
	      end if
            ELSE
	      if((final.eq.1).and.(itrial.ge.maxtrial))then
                CALL STPERR(88,3,'dum',CIL,' ',CLINE)
                GO TO 9891
	      else
                CALL STPERR(88,3,'dum',CIL,' ',CLINE)
	        go to 9950
	      end if
            END IF
          END IF
          WRITE(FMT,250) NUM2-NUM1+1
          READ(CLINE(NUM1:NUM2),FMT,ERR=270) RTEMP
          IF(DUMFLG.EQ.0) workvec(IOBS)=RTEMP
          J1=NUM2
          GO TO 50
270       CALL GETINT(IFAIL,BUF,N1,N2,NBLB,MKRDEL)
          IF(IFAIL.NE.0) THEN
            CALL STPERR(68,5,BUF,0,' ',CLINE)
            GO TO 9891
          END IF
          IF(N1.EQ.0)THEN
            IF(DUMFLG.EQ.1) GO TO 9900
            GO TO 9800
          END IF
          IF(BUF(N1:N1).NE.MKRDEL) THEN
            IF(DUMFLG.EQ.1) GO TO 9900
            GO TO 9800
          END IF
          J2=INDEX(CLINE(J1+1:NBLC),BUF(N1+1:N2-1))
          IF(J2.EQ.0) THEN
            IF(DUMFLG.EQ.1) GO TO 9900
            GO TO 9800
          END IF
          NUM2=J1+J2-1
          IF(NUM2.LT.NUM1)THEN
            IF(DUMFLG.EQ.1) GO TO 9900
            GO TO 9800
          END IF
          WRITE(FMT,250) NUM2-NUM1+1
          IF(DUMFLG.EQ.1)THEN
            READ(CLINE(NUM1:NUM2),FMT,ERR=9900) RTEMP
          ELSE
            READ(CLINE(NUM1:NUM2),FMT,ERR=9800) workvec(IOBS)
          END IF
          J1=NUM2
          GO TO 51
        ELSE
          CALL STPERR(64,5,BUF,0,' ',CLINE)
          GO TO 9891
        END IF
        GO TO 50
1000    INS=INS+1
        IF(INS.LE.NINSTR) GO TO 10

        IF(MCALL.EQ.1)THEN
          DO 1100 I=1,NOBS
          DO 1050 J=1,JOBS
          IF(IIOBS(J).EQ.I) GO TO 1100
1050      CONTINUE
          CALL STPERR(45,1,AOBS(I)(:LEN_TRIM(AOBS(I))),0,' ',CLINE)
          GO TO 9891
1100      CONTINUE
        END IF

        call closefile(jfail,inst,2,afile,cline)
        if(jfail.ne.0) go to 9891
#ifndef MPEST
	call pestwait(iiwait)
#endif

#ifndef MPEST

C -- Next we delete all model output files.

	do 1500 j=1,noufl
	  afile=trim(soufle(j))
	  call delfile(jfail,inst,afile,cline)
	  if(jfail.ne.0) go to 9891
1500	continue
        if(imsfle.eq.1)then
          afile=trim(asldir)//'pest.mmf'
          call delfile(jfail,immf,afile,cline)
          if(jfail.ne.0) go to 9891
        end if
	call pestwait(iiwait)
#endif

C -- The set of observations that has just been read is stored in the parallel
C    run register file.

        call store_parallel_register
     +  (ifail,nobs,ippo,irun,workvec,obsregfile)
        if(ifail.ne.0) go to 9891

        RETURN

9000    CALL STPERR(74,5,BUF,0,' ',CLINE)
        GO TO 9891
9050    CALL STPERR(75,5,BUF,0,' ',CLINE)
        GO TO 9891
9100    CALL STPERR(66,6,BUF,0,' ',CLINE)
        if(iflag.eq.0)then
          GO TO 9891
        else
          go to 9950
        end if
9200    if((final.eq.1).and.(itrial.ge.maxtrial)) then
	  CALL STPERR(67,4,BUF,CIL,' ',CLINE)
	  GO TO 9891
	else
	  CALL STPERR(67,4,BUF,CIL,' ',CLINE)
	  go to 9950
	end if
9400    if((final.eq.1).and.(itrial.ge.maxtrial)) then
	  CALL STPERR(69,4,BUF,CIL,' ',CLINE)
	  GO TO 9891
	else
	  CALL STPERR(69,4,BUF,CIL,' ',CLINE)
	  go to 9950
	end if
9700    CALL STPERR(84,6,BUF,0,OBSNAM(:LEN_TRIM(OBSNAM)),CLINE)
        GO TO 9891
9800    if((final.eq.1).and.(itrial.ge.maxtrial)) then
	  CALL STPERR(82,3,AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),
     +    CIL,' ',CLINE)
          GO TO 9891
	else
	  CALL STPERR(82,3,AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),
     +    CIL,' ',CLINE)
	  go to 9950
	end if
9900    if((final.eq.1).and.(itrial.ge.maxtrial)) then
	  CALL STPERR(82,3,'dum',CIL,' ',CLINE)
	  GO TO 9891
	else
	  CALL STPERR(82,3,'dum',CIL,' ',CLINE)
	  go to 9950
	end if

9950	if(final.eq.1) go to 9960
	call closefile(jfail,inst,0,afile,cline)
	if(jfail.ne.0) go to 9891
	iifail=1
	return

9960	continue
	call closefile(jfail,inst,0,afile,cline)
	if(jfail.ne.0) go to 9891
	iifail=2
	return

9891	jfail=1
        return

        END


	subroutine closefile(jfail,iunit,istatus,afile,cline)

#ifdef INTEL
        use COMMON_MOD, ONLY: FLENME,IWAIT
#endif
        implicit none

	logical lopened
#ifdef INTEL
	integer iunit,icount,ierr,istatus,iiwait,jfail
#else
	integer iunit,icount,ierr,istatus,iwait,iiwait,jfail
#endif
#ifndef INTEL
	character*200 flenme
#endif
	character*(*) afile,cline

#ifndef INTEL
	common /flenme/flenme
	common /waitint/iwait
#endif

        jfail=0
	iiwait=iwait
	if(istatus.eq.2) go to 90
	inquire(unit=iunit,opened=lopened)
	if(.not.lopened) return
90	icount=0
100	icount=icount+1
	close(unit=iunit,iostat=ierr)
	if(ierr.ne.0) then
	  if(icount.gt.20) then
	    flenme=afile
	    call stperr(124,1,' ',0,' ',cline)
	    go to 9891
	  end if
	  call pestwait(iiwait)
	  go to 100
	end if

	return

9891    jfail=1
        return
	end



	subroutine delfile(jfail,iunit,afile,cline)

#ifdef INTEL
        use COMMON_MOD, ONLY: FLENME,IWAIT
#endif
        implicit none

	logical lexist
#ifdef INTEL
	integer iunit,icount,junit,ierr,iiwait,itemp,jfail
#else
	integer iunit,icount,junit,ierr,iwait,iiwait,itemp,jfail
#endif
	integer unlink
#ifndef INTEL
	character*200 flenme
#endif
	character*(*) afile,cline

#ifndef INTEL
	common /flenme/flenme
	common /waitint/iwait
#endif

        jfail=0
	iiwait=iwait
	junit=iunit
	if(iunit.lt.0)then
	  inquire(file=trim(afile),exist=lexist)
	  if(.not.lexist) then
c	     write(6,1) charnb(afile)
c1	     format(' File ',a,' does not exist')
	     return
	  end if
	  junit=-junit
	end if
	icount=0
#ifdef UNIX
10	itemp=unlink(trim(afile))
#else
10	call system('del "'//trim(afile)//'" > nul')
#endif
	call pestwait(iiwait)
        if(iunit.lt.-1000) return
#ifdef UNIX
        open(unit=junit,file=trim(afile),
     +  status='old',iostat=ierr)
#else
#ifdef LF90
	open(unit=junit,file=trim(afile),
     +  action='readwrite,denynone',status='old',
     +  blocksize=1,iostat=ierr)
#else
#ifdef INTEL
        open(unit=junit,file=trim(afile),
     +  status='old',blocksize=1,shared,iostat=ierr)
#else
        open(unit=junit,file=trim(afile),
     +  status='old',blocksize=1,iostat=ierr)
#endif
#endif
#endif
	if(ierr.ne.0) return
	call pestwait(iiwait)
	call closefile(jfail,junit,2,afile,cline)
	if(jfail.ne.0) go to 9891
	icount=icount+1
	if(icount.gt.10)then
	  flenme=afile
	  call stperr(125,1,' ',0,' ',cline)
	  go to 9891
	end if
	call pestwait(iiwait)
	go to 10

9891    jfail=1
        return

	end


#ifdef MPEST

        subroutine hmparlam(ifail,mlamrun)

C -- Subroutine HMPARLAM reads a file supplied by MPRUN in order to ascertain the
C    number of nodes on which parallel lambda runs are to be made.

#ifdef INTEL
        use COMMON_MOD, ONLY: FLNME,CASEFL
#endif
        use pestdata, only : irmr
        implicit none

        integer ifail,mlamrun,ierr
#ifndef INTEL
        character*100 casefl
        character*200 flenme

        common /case/casefl
        common /flenme/flenme
#endif

        ifail=0

        flenme=trim(casefl)//'.number_processors'
        open(unit=irmr,file=flenme,status='old',iostat=ierr)
        if(ierr.ne.0)then
          ifail=1
          return
        end if
        read(irmr,*)mlamrun
        close(unit=irmr)
        if(mlamrun.le.0) ifail=1

        return
        end


#else

        subroutine hmruns(nslave,istats,iruntme,iorder,numslave,idet,
     +  manylam,irefobs)

C -- Subroutine HMRUNS evaluates how many model runs are to be undertaken
C    in the search for the optimum Marquardt lambda.

#ifdef GENIE
        USE GENIE_DATA
#endif
        implicit none

        integer nslave,numslave,islave,irtme,iss,is,maxtme,js,nactslv,
     +  ifstme,manylam
        integer irefobs
        integer istats(nslave),iruntme(nslave),iorder(nslave),
     +  idet(nslave)
        double precision dtol
#ifdef GENIE
        integer jfail
#endif

#ifdef BEO
        call FASTSLAVES(numslave,manylam)
#else
#ifdef GENIE
        call GET_NSLAVE(jfail,numslave)
        if(jfail.ne.0) then
          write(6,'(a)') ' *** ERROR DETERMINING NUMBER OF '//
     +                   'CONNECTED AGENT COMPUTERS.'
        end if
#else
C -- First we put the slaves in order of run-time (only if they are free).

        numslave=0
        do 50 islave=1,nslave
          iorder(islave)=0
50      continue

        maxtme=0
        do 60 islave=1,nslave
          if(idet(islave).eq.0) cycle
          if(istats(islave).eq.0)then
            if(iruntme(islave).gt.maxtme)maxtme=iruntme(islave)
          end if
60      continue
        if(maxtme.eq.0) return

        nactslv=0
        do 200 islave=1,nslave
          iss=0
          irtme=maxtme
          do 150 is=1,nslave
            if(idet(is).eq.0) cycle
            if(istats(is).eq.0)then
              if(iruntme(is).le.irtme)then
                if(islave.gt.1)then
                  do 140 js=1,islave-1
                    if(iorder(js).eq.is) go to 150
140               continue
                end if
                irtme=iruntme(is)
                iss=is
              end if
            end if
150       continue
          if(iss.eq.0)then
            go to 300
          else
            iorder(islave)=iss
            nactslv=nactslv+1
          end if
200     continue
300     continue
        if(nactslv.eq.0) return

C -- Now we find the greatest number or slaves for which the slowest is
C    slower than the fastest by less than a factor of 1.8 (arbitrary).

C -- However if PARLAM is set to -9999 we are a lot more tolerant of slower
C    slaves.
C -- We are also a lot more tolerant of slower slaves if we are doing
C    observation re-referencing

        dtol=1.8d0
        if(manylam.ne.0) dtol=10.0
        if(irefobs.eq.1) dtol=10.0

        ifstme=iruntme(iorder(1))
        do 400 islave=nactslv,1,-1
          if(float(iruntme(iorder(islave)))/float(ifstme)
     +       .lt.dtol) go to 500
400     continue
500     numslave=islave

#endif  !GENIE
#endif
        return

        end
#endif


        subroutine store_parallel_register
     +  (ifail,n,iunit,irun,vector,afile)

C -- Subroutine STORE_PARALLEL_REGISTER stores an array of values in one record
C    of a parallel run register file.

        use pestdata, only : errmsg,maxreclnum
        implicit none

        integer ifail,n,iunit,irun
        double precision vector(n)
        character*(*) afile

        integer i,ierr,numrecord,istart,ii,ifin,ires,jfin
        double precision dtemp

        ifail=0
        dtemp=1.0d35
        numrecord=(n-1)/maxreclnum+1
        istart=(irun-1)*numrecord
        jfin=numrecord*maxreclnum

        ii=0
        do
          istart=istart+1
          ifin=min(n,ii+maxreclnum)
          ires=jfin-ifin
          if((ires.eq.0).or.(ires.ge.maxreclnum))ires=0
          if(ires.eq.0)then
            write(iunit,rec=istart,iostat=ierr)(vector(i),i=ii+1,ifin)
          else
            write(iunit,rec=istart,iostat=ierr)(vector(i),i=ii+1,ifin),
     +      (dtemp,i=1,ires)
          end if
          if(ierr.ne.0)then
            write(errmsg,10) trim(afile)
10          format('Parallel run management error: cannot store ',
     +      'data in parallel run queue file ',a,'.')
            ifail=1
          end if
          if(ifin.eq.n) exit
          ii=ii+maxreclnum
        end do

        return
        end



        subroutine retrieve_parallel_register
     +  (ifail,n,iunit,irun,vector,afile)

C -- Subroutine RETRIEVE_PARALLEL_REGISTER retrieves an array of values from one record
C    of a parallel run register file.

        use pestdata, only : errmsg,maxreclnum
        implicit none

        integer ifail,n,iunit,irun
        double precision vector(n)
        character*(*) afile

        integer i,ierr,numrecord,istart,ii,ifin

        ifail=0
        numrecord=(n-1)/maxreclnum+1
        istart=(irun-1)*numrecord

        ii=0
        do
          istart=istart+1
          ifin=min(n,ii+maxreclnum)
          read(iunit,rec=istart,iostat=ierr)(vector(i),i=ii+1,ifin)
          if(ierr.ne.0)then
            write(errmsg,10) trim(afile)
10          format('Parallel run management error: cannot retrieve ',
     +      'data from parallel run queue file ',a,'.')
            ifail=1
            return
          end if
          if(ifin.eq.n) exit
          ii=ii+maxreclnum
        end do

        return
        end



