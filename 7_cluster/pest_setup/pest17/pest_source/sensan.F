

        program sensan

        logical lexist
        integer ifail,nblnk,mcase,ierr,npar,nobs,ntplfle,ninsfle,iline,
     +  mmline,i,j,iverbose,lenins,extlen,irun,jerr,iobs,ipres,iset,k,ii
        integer lss(4),rss(4)
        real rrtemp
        character*256 infile,errmsg*420,anum*10,aaline*256
        character*20000 cline,ccline*256
        character*256 outfle(3)
        character*8 aprecis,apoint,averbose*9
        character*256 varfle,tempfle
        character*100 comline
        character*12 aversion

        integer ls(:),rs(:)
        real parval(:),parval0(:),obsval(:),obsval0(:),rtemp(:)
        character*256 minfle(:),tplfle(:),insfle(:),moutfle(:)
        character*12 apar(:)
        character*20 aobs(:)

#ifdef SYS_FUNCTION
        integer nnn
        integer system
#endif

        allocatable::apar,aobs,minfle,tplfle,insfle,moutfle,ls,rs,
     +  parval,parval0,obsval,obsval0,rtemp

        common /errmsg/errmsg
        common /ierr/ierr,mcase


        include 'version.inc'
        write(6,20) trim(aversion)
20      format(/' SENSAN Version ',a,'. Watermark ',
     +  'Numerical Computing.',/)

C -- The name of the SENSAN control file is acquired from the SENSAN command line.

#ifdef NO_CMDLINE
        write(*,1)
1       format(/,' Sensan cannot run without ',
     +  'command line arguments.',/,
     +  ' Contact Watermark Numerical Computing ',
     +  'for a new version of Sensan.',/)
        stop
#else
        call pgetcl(cline)
#endif
        call spacesub(cline)
        if (cline.eq.' ') go to 9000
        call linesplit(ifail,2,lss,rss,cline)
        if(ifail.eq.0) go to 9000
        infile=cline(lss(1):rss(1))
        call remchar(infile,char(211))
        j=nblnk(infile)
        do 50 i=j,1,-1
          if(infile(i:i).eq.'.')then
            if(infile(i+1:i+1).eq.char(92)) go to 60
            go to 70
          end if
50      continue
60      infile=infile(1:j)//'.sns'
70      open(unit=10,file=infile,status='old',err=9050)

C -- The SENSAN control file is read and arrays allocated.

        iline=1
        call writlin(iline,mmline,infile,aaline)
        read(10,'(a)',end=9100) cline
        call lshift(cline)
        if(cline(1:3).ne.'scf') then
          write(errmsg,90) aaline(1:mmline)
90        format(a,'"scf" header expected.')
          call writerr
          go to 9999
        end if

95      iline=iline+1
        call writlin(iline,mmline,infile,aaline)
        read(10,'(a)',end=9100) cline
        if((cline.eq.' ').or.(cline(1:1).eq.'*')) go to 95
        call linesplit(ifail,1,lss,rss,cline)
        averbose=cline(lss(1):rss(1))
        call lowcase(averbose)
        if(averbose.eq.'verbose') then
          iverbose=1
        else if(averbose.eq.'noverbose') then
          iverbose=0
        else
          go to 9200
        end if

100     continue
        iline=iline+1
        call writlin(iline,mmline,infile,aaline)
        read(10,'(a)',end=9100) cline
        if((cline.eq.' ').or.(cline(1:1).eq.'*')) go to 100
        call linesplit(ifail,2,lss,rss,cline)
        if(ifail.ne.0) go to 9150
        call intread(ifail,cline(lss(1):rss(1)),npar)
        if(ifail.ne.0) go to 9200
        if(npar.le.0) go to 9200
        call intread(ifail,cline(lss(2):rss(2)),nobs)
        if(ifail.ne.0) go to 9200
        if(nobs.le.0) go to 9200

130     continue
        iline=iline+1
        call writlin(iline,mmline,infile,aaline)
        read(10,'(a)',end=9100) cline
        if((cline.eq.' ').or.(cline(1:1).eq.'*')) go to 130
        call linesplit(ifail,4,lss,rss,cline)
        if(ifail.ne.0) go to 9150
        call intread(ifail,cline(lss(1):rss(1)),ntplfle)
        if(ifail.ne.0) go to 9200
        if(ntplfle.le.0) go to 9200
        call intread(ifail,cline(lss(2):rss(2)),ninsfle)
        if(ifail.ne.0) go to 9200
        if(ninsfle.le.0) go to 9200
        aprecis=cline(lss(3):rss(3))
        call lowcase(aprecis)
        if((aprecis.ne.'single').and.(aprecis.ne.'double')) go to 9200
        apoint=cline(lss(4):rss(4))
        call lowcase(apoint)
        if((apoint.ne.'point').and.(apoint.ne.'nopoint')) go to 9200

        allocate(apar(npar),aobs(nobs),tplfle(ntplfle),
     +  minfle(ntplfle),insfle(ninsfle),moutfle(ninsfle),
     +  ls(npar),rs(npar),parval(npar),parval0(npar),obsval(nobs),
     +  obsval0(nobs),rtemp(nobs),stat=ierr)
        if(ierr.ne.0) go to 9250

        do 250 i=1,4
220       iline=iline+1
          call writlin(iline,mmline,infile,aaline)
          read(10,'(a)',end=9100) cline
          call spacesub(cline)
          if((cline.eq.' ').or.(cline(1:1).eq.'*')) go to 220
          call lshift(cline)
          if(i.eq.1) then
            varfle=cline(1:index(cline,' ')-1)
            call remchar(varfle,char(211))
          else
            outfle(i-1)=cline(1:index(cline,' ')-1)
            call remchar(outfle(i-1),char(211))
            inquire(file=outfle(i-1),exist=lexist)
#ifdef UNIX
#ifdef SYS_FUNCTION
            if(lexist)
     +      nnn=system('/bin/rm '//outfle(i-1)(1:nblnk(outfle(i-1))))
#else
            if(lexist)
     +      call system('/bin/rm '//outfle(i-1)(1:nblnk(outfle(i-1))))
#endif
#else
            if(lexist)
     +      call system('del "'//outfle(i-1)(1:nblnk(outfle(i-1)))//'"')
#endif
          end if
250     continue

135     continue
        iline=iline+1
        call writlin(iline,mmline,infile,aaline)
        read(10,'(a)',end=9100) cline
        if((cline.eq.' ').or.(cline(1:1).eq.'*')) go to 135
        comline=cline(1:nblnk(cline))

        do 150 i=1,ntplfle
140       iline=iline+1
          call writlin(iline,mmline,infile,aaline)
          read(10,'(a)',end=9100) cline
          call spacesub(cline)
          if((cline.eq.' ').or.(cline(1:1).eq.'*')) go to 140
          call linesplit(ifail,2,lss,rss,cline)
          if(ifail.ne.0) go to 9150
          tplfle(i)=cline(lss(1):rss(1))
          call remchar(tplfle(i),char(211))
          minfle(i)=cline(lss(2):rss(2))
          call remchar(minfle(i),char(211))
          inquire(file=minfle(i),exist=lexist)
#ifdef UNIX
#ifdef SYS_FUNCTION
          if(lexist)
     +    nnn=system('/bin/rm '//minfle(i))
#else
          if(lexist)
     +    call system('/bin/rm '//minfle(i))
#endif
#else
          if(lexist)
     +    call system('del "'//minfle(i)//'"')
#endif
150     continue

        do 200 i=1,ninsfle
170       iline=iline+1
          call writlin(iline,mmline,infile,aaline)
          read(10,'(a)',end=9100) cline
          call spacesub(cline)
          if((cline.eq.' ').or.(cline(1:1).eq.'*')) go to 170
          call linesplit(ifail,2,lss,rss,cline)
          if(ifail.ne.0) go to 9150
          insfle(i)=cline(lss(1):rss(1))
          call remchar(insfle(i),char(211))
          moutfle(i)=cline(lss(2):rss(2))
          call remchar(moutfle(i),char(211))
200     continue

        close(unit=10)

C -- The parameter variation file is now read line by line and model runs
C    made accordingly.

        open(unit=10,file=varfle,status='old',err=9300)
        do 260 i=1,3
#ifdef UNIX
          open(unit=10+i,file=outfle(i)(1:nblnk(outfle(i))))
#else
#ifdef LF90
          open(unit=10+i,file=outfle(i)(1:nblnk(outfle(i))),
     +    action='readwrite,denynone',blocksize=1)
#else
          open(unit=10+i,file=outfle(i)(1:nblnk(outfle(i))))
#endif
#endif
260     continue
        iline=0
        irun=0
        iset=0
        jerr=0

        write(6,*)
500     iline=iline+1
        call writlin(iline,mmline,varfle,aaline)
        read(10,'(a)',end=1000) cline
        if(cline.eq.' ') go to 500
        call linesplit(ifail,npar,ls,rs,cline)
        if(ifail.ne.0) go to 9150
        do 530 i=1,npar
          apar(i)=cline(ls(i):min(rs(i),ls(i)+11))
530     continue

550     continue
        iline=iline+1
        call writlin(iline,mmline,varfle,aaline)
        read(10,'(a)',end=1000) cline
        if(cline.eq.' ') go to 550
        iset=iset+1
        call writint(anum,iset)
#ifdef LAHEY
        if(jerr.eq.0) then
          write(6,610) anum(1:nblnk(anum))
610       format('+Parameter set number ',a,':')
        else
          write(6,611) anum(1:nblnk(anum))
611       format(' Parameter set number ',a,':')
        end if
#else
        write(6,612) anum(1:nblnk(anum))
612     format(' Running model for parameter set ',a,'....')
#endif
        jerr=0
        call linesplit(ifail,npar,ls,rs,cline)
        if(ifail.ne.0) then
          if(irun.eq.0) go to 9150
          write(errmsg,9160) aaline(1:mmline)
          call writerr
          jerr=1
          do 560 i=1,3
            write(10+i,'(a)') cline(1:nblnk(cline))
560       continue
          go to 550
        end if
        do 580 i=1,npar
          call realread(ifail,cline(ls(i):rs(i)),parval(i))
          if(ifail.ne.0) then
            if(irun.eq.0) go to 9200
            write(errmsg,9210) aaline(1:mmline)
            call writerr
            jerr=1
            do 565 j=1,3
              write(10+j,'(a)') cline(1:nblnk(cline))
565         continue
            go to 550
          end if
580     continue

C -- The current parameter value file is written.

        open(unit=99,file='t###.par')
        write(99,570) aprecis,apoint
570     format(2a)
        do 600 i=1,npar
          write(99,*) apar(i),parval(i),'  1  0'
600     continue
        close(unit=99)

C -- Previous model and INSCHEK output files are deleted.

        do 590 i=1,ninsfle
          inquire(file=moutfle(i)(1:nblnk(moutfle(i))),exist=lexist)
#ifdef UNIX
#ifdef SYS_FUNCTION
          if(lexist) then
            nnn=system('/bin/rm '//moutfle(i)(1:nblnk(moutfle(i))))
#else
          if(lexist) then
            call system('/bin/rm '//moutfle(i)(1:nblnk(moutfle(i))))
#endif
#else
          if(lexist) then
            call system('del "'//moutfle(i)(1:nblnk(moutfle(i)))//'"')
#endif
          end if
          lenins=nblnk(insfle(i))
          call extchek(extlen,lenins,insfle(i))
          tempfle=insfle(i)
          tempfle(extlen:extlen+3)='.obf'
          inquire(file=tempfle(1:nblnk(tempfle)),exist=lexist)
#ifdef UNIX
#ifdef SYS_FUNCTION
          if(lexist) then
            nnn=system('/bin/rm '//tempfle(1:nblnk(tempfle)))
#else
          if(lexist) then
            call system('/bin/rm '//tempfle(1:nblnk(tempfle)))
#endif
#else
          if(lexist) then
            call system('del "'//tempfle(1:nblnk(tempfle))//'"')
#endif
          end if
590     continue

C -- Model input files are built

        do 700 i=1,ntplfle
C         call pestwait(100)            !NTPROB
          if(iverbose.eq.1) then
#ifdef SYS_FUNCTION
            nnn=system('tempchek '//tplfle(i)(1:nblnk(tplfle(i)))
     +      //' '//minfle(i)(1:nblnk(minfle(i)))//' t###.par')
#else
#ifdef LAHEY
            call system
     +      ('tempchek "'//tplfle(i)(1:nblnk(tplfle(i)))//'" '//
     +      '"'//minfle(i)(1:nblnk(minfle(i)))//'" t###.par')
#else
            call system('tempchek '//tplfle(i)(1:nblnk(tplfle(i)))
     +      //' '//minfle(i)(1:nblnk(minfle(i)))//' t###.par')
#endif
#endif
          else

#ifdef UNIX
#ifdef SYS_FUNCTION
            nnn=system('tempchek '//tplfle(i)(1:nblnk(tplfle(i)))
     +      //' '//minfle(i)(1:nblnk(minfle(i)))//
     +      ' t###.par > /dev/null')
#else
            call system('tempchek '//tplfle(i)(1:nblnk(tplfle(i)))
     +      //' '//minfle(i)(1:nblnk(minfle(i)))//
     +      ' t###.par > /dev/null')
#endif
#else
            call system
     +      ('tempchek "'//tplfle(i)(1:nblnk(tplfle(i)))//'" '//
     +      '"'//minfle(i)(1:nblnk(minfle(i)))//'" t###.par > nul')
#endif
          end if
700     continue

C -- The model is run.

        irun=irun+1
C       call pestwait(100)                      !NTPROB

#ifdef SYS_FUNCTION
        nnn=system(comline(1:nblnk(comline)))
#else
        call system(comline(1:nblnk(comline)))
#endif
C       call pestwait(100)                      !NTPROB

C -- The presence of all model output file is checked for.

        ipres=1
        do 750 i=1,ninsfle
          inquire(file=moutfle(i)(1:nblnk(moutfle(i))),exist=lexist)
          if(.not.lexist)then
            write(errmsg,740) moutfle(i)(1:nblnk(moutfle(i)))
740         format('Model output file ',a,' not found.')
            call writerr
            ipres=0
            jerr=1
          end if
750     continue
        if(ipres.eq.0) then
          if(irun.eq.1) then
            go to 9999
          else
            do 760 j=1,3
              write(10+j,900) (parval(ii),ii=1,npar)
760         continue
            go to 960
          end if
        end if

C -- Model outputs are read.

        iobs=0

        do 800 i=1,ninsfle
          if(iverbose.eq.1) then
#ifdef SYS_FUNCTION
            nnn=system('inschek '//insfle(i)(1:nblnk(insfle(i)))//
     +      ' '//moutfle(i)(1:nblnk(moutfle(i))))
#else
#ifdef LAHEY
            call system
     +      ('inschek "'//insfle(i)(1:nblnk(insfle(i)))//'" '//
     +      '"'//moutfle(i)(1:nblnk(moutfle(i)))//'"')
#else
            call system('inschek '//insfle(i)(1:nblnk(insfle(i)))//
     +      ' '//moutfle(i)(1:nblnk(moutfle(i))))
#endif
#endif
          else
#ifdef UNIX
#ifdef SYS_FUNCTION
            nnn=system('inschek '//insfle(i)(1:nblnk(insfle(i)))//
     +      ' '//moutfle(i)(1:nblnk(moutfle(i)))//
     +      ' > /dev/null')
#else
            call system('inschek '//insfle(i)(1:nblnk(insfle(i)))//
     +      ' '//moutfle(i)(1:nblnk(moutfle(i)))//
     +      ' > /dev/null')
#endif
#else
            call system
     +      ('inschek "'//insfle(i)(1:nblnk(insfle(i)))//'" '//
     +       '"'//moutfle(i)(1:nblnk(moutfle(i)))//'" > nul')
#endif
          end if
C         call pestwait(100)            !NTPROB
          lenins=nblnk(insfle(i))
          call extchek(extlen,lenins,insfle(i))
          tempfle=insfle(i)
          tempfle(extlen:extlen+3)='.obf'
          open(unit=99,file=tempfle,status='old',iostat=ierr)
          if(ierr.ne.0) then
            write(errmsg,620) tempfle(1:nblnk(tempfle))
620         format('Unable to open temporary observation value file ',
     +      a)
            call writerr
            if(irun.eq.1) then
              go to 9999
            else
              jerr=1
              do 625 j=1,3
                write(10+j,900) (parval(ii),ii=1,npar)
625           continue
              go to 960
            end if
          end if
660       read(99,'(a)',end=720) ccline
          if(ccline.eq.' ') go to 660
          iobs=iobs+1
          if(iobs.gt.nobs) then
            write(errmsg,640)
640         format('Error: there are more observations named in ',
     +      'instruction file(s) than the number of observations ',
     +      'cited in the SENSAN control file.')
            call writerr
            go to 9999
          end if
          call linesplit(ifail,2,lss,rss,ccline)
          aobs(iobs)=ccline(lss(1):rss(1))
          call realread(ifail,ccline(lss(2):rss(2)),obsval(iobs))
          if(ifail.ne.0) then
            write(errmsg,680) tempfle(1:nblnk(tempfle))
680         format('Error reading temporary observation value ',
     +      'file ',a)
            call writerr
            go to 9999
          end if
          go to 660
720       close(unit=99)
800     continue
        if(iobs.lt.nobs) then
          write(errmsg,820)
820       format('Error: there are less observations named in the ',
     +    'instruction file(s) than the number of observations ',
     +    'cited in the SENSAN control file.')
          call writerr
          if(irun.eq.1)then
            go to 9999
          else
            jerr=1
            do 825 j=1,3
              write(10+j,900) (parval(ii),ii=1,npar)
825         continue
            go to 960
          end if
        end if

C -- Results are written to SENSAN output files.

        if(irun.eq.1) then
          do 840 j=1,3
            write(10+j,830) (apar(i),i=1,npar),(aobs(i),i=1,nobs)
830         format(1x,5000(1x,a20,1x))
840       continue
          do 850 i=1,npar
            parval0(i)=parval(i)
850       continue
          do 860 i=1,nobs
            obsval0(i)=obsval(i)
860       continue
        end if

        write(11,900) (parval(i),i=1,npar),(obsval(i),i=1,nobs)
900     format(1x,5000(1x,1pg14.7,7x))

        do 930 i=1,nobs
          if(irun.eq.1) then
            rtemp(i)=0.0
          else
            if (obsval0(i).ne.0.0) then
              rtemp(i)=(obsval(i)-obsval0(i))/obsval0(i)
            else
              rtemp(i)=1.0e35
            end if
          end if
930     continue
        write(12,900) (parval(i),i=1,npar),(rtemp(i),i=1,nobs)

        if(irun.eq.1) then
          do 935 i=1,nobs
            rtemp(i)=0.0
935       continue
        else
          rrtemp=0.0
          do 940 i=1,npar
            rrtemp=rrtemp+(parval(i)-parval0(i))*(parval(i)-parval0(i))
940       continue
          rrtemp=sqrt(rrtemp)

c -- The following lines adjust the sign of the sensitivity if only a single
c    parameter is varied.

          i=0
          do 945 j=1,npar
            if(parval(j)-parval0(j).ne.0.0) then
              i=i+1
              k=j
             end if
945       continue
          if(i.eq.1) then
            if(parval(k)-parval0(k).lt.0) rrtemp=-rrtemp
          end if

          do 950 i=1,nobs
            if(rrtemp.ne.0.0) then
              rtemp(i)=(obsval(i)-obsval0(i))/rrtemp
            else
              rtemp(i)=1.0e35
            end if
950       continue
        end if
        write(13,900) (parval(i),i=1,npar),(rtemp(i),i=1,nobs)

C -- If any system command have to be issued, these are issued now.

c960    if(cline(rs(npar):).ne.' ')then
960     if(rs(npar).ne.nblnk(cline)) then
          cline(1:rs(npar))=' '
          call lshift(cline)
          if(iverbose.eq.1)then
#ifdef SYS_FUNCTION
            nnn=system(cline(1:nblnk(cline)))
#else
            call system(cline(1:nblnk(cline)))
#endif
          else
#ifdef UNIX
#ifdef SYS_FUNCTION
            nnn=system(cline(1:nblnk(cline))//' > /dev/null')
#else
            call system(cline(1:nblnk(cline))//' > /dev/null')
#endif
#else
            call system(cline(1:nblnk(cline))//' > nul')
#endif
          end if
        end if

#ifdef FLUSHFILE
          do j=1,3
            call flush(10+j)
          end do
#endif
        go to 550

1000    if(jerr.ne.0)then
          write(6,*)
        else
#ifdef LAHEY
          write(6,1010)
1010      format('+                                      ')
#else
          write(6,*)
#endif
        end if
        call writint(anum,iset)
        write(6,1060) anum(1:nblnk(anum))
1060    format(' - SENSAN has examined ',a,' parameter sets.')
        call writint(anum,irun)
        write(6,1070) anum(1:nblnk(anum))
1070    format(' - SENSAN has carried out ',a,' model runs.')
        if(irun.ne.0)then
          do 1100 i=1,3
            write(6,1080) outfle(i)(1:nblnk(outfle(i)))
1080        format(' - file ',a,' written ok.')
1100      continue
        end if

        go to 9999

9000    write(6,9010)
9010    format(' SENSAN is run using the command:-')
        write(6,9020)
9020    format(/,'   SENSAN infile',/,/,' where ')
        write(6,9030)
9030    format(/,'   "infile" is the name of a SENSAN control file.',/)
        go to 9999

9050    write(errmsg,9060) infile(1:nblnk(infile))
9060    format('Cannot open SENSAN control file ',a)
        call writerr
        go to 9999

9100    write(errmsg,9110) aaline(1:mmline)
9110    format(a,'unexpected end to file.')
        call writerr
        go to 9999

9150    write(errmsg,9160) aaline(1:mmline)
9160    format(a,'insufficient data entries.')
        call writerr
        go to 9999

9200    write(errmsg,9210) aaline(1:mmline)
9210    format(a,'incorrect data entry.')
        call writerr
        go to 9999

9250    write(errmsg,9260)
9260    format('Insufficient memory available for array allocation.')
        call writerr
        go to 9999

9300    write(errmsg,9310) varfle(1:nblnk(varfle))
9310    format('Cannot open parameter variation file ',a)
        call writerr
        go to 9999

9999    close(unit=10,iostat=ierr)
        close(unit=11,iostat=ierr)
        close(unit=12,iostat=ierr)
        close(unit=13,iostat=ierr)

        deallocate(apar,aobs,tplfle,minfle,insfle,moutfle,
     +  ls,rs,parval,parval0,obsval,obsval0,rtemp,stat=ierr)

        write(6,*)
        end


