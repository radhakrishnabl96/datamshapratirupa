
	program pslave

	logical ispest,lexist
	integer ierr,iwait,iwait3,nostop
        character*12 aversion
	character*200 comline,string
	character*20 sreadfle,preadfle,oreadfle,sfinfle,mreadfle,
     +  creadfle
#ifdef UNIX
#ifdef UNICOS
	integer itemp,ishell
#endif
#ifdef SYS_FUNCTION
        integer nnn
        integer system
#endif
#endif

	creadfle='p###.cmd'
	sreadfle='pslave.rdy'
	mreadfle='pest.rdy'
	preadfle='param.rdy'
	oreadfle='observ.rdy'
	sfinfle='pslave.fin'

        include 'version.inc'
	write(6,100) trim(aversion)
100     format(' PAGENT version ',a,'. ',
     +  'Watermark Numerical Computing.',/)

C -- The command line argument (if any) is retrieved.

#ifdef INTEL
       comline = ' '
       do k=1,IARGC()
         call GETARG(k,string)
         comline = TRIM(comline)//' '//string
       enddo
#else
#ifdef NO_CMDLINE
3       WRITE(*,'(A)') ' Enter command line arguments > '
        READ(*,'(A)',ERR=3) COMLINE
#else
        CALL PGETCL(COMLINE)
#endif
#endif

        NOSTOP=0
        IF(COMLINE.NE.' ')THEN
          COMLINE=ADJUSTL(COMLINE)
          IF((COMLINE.EQ.'/N').OR.(COMLINE.EQ.'/n'))then
            NOSTOP=1
          ELSE
            WRITE(6,102)
102         FORMAT(/,' *** Unrecognized command line argument ***',/)
            STOP
          END IF
        END IF
        COMLINE=' '

C -- PSLAVE inquires as to the command to run the model.

#ifdef BEO
        comline = ' '
        do k=1,IARGC()
          call GETARG(k,string)
          comline = TRIM(comline)//' '//string
        enddo
        do while (comline.eq.' ')
          write(6,150)
150       format(' Enter command to run model: ',$)
          read(5,'(a)') comline
        enddo
#else
120     write(6,150)
150	format(' Enter command to run model: ',$)
        read(5,'(a)') comline
	if(comline.eq.' ') go to 120
#endif

C -- PSLAVE reads the wait period.
c
c210	write(6,220)
c220	format(' Enter average wait period (hundredths of a second)',
c     +  ': ',$)
c	read(5,*,err=210) iwait
c	if(iwait.le.0) go to 210
	iwait=100
	iwait3=3*iwait


C -- PSLAVE removes any old "ready" files.

	call delfile(-50,mreadfle,iwait)
	call delfile(-50,preadfle,iwait)
	call delfile(-50,sfinfle,iwait)


C -- PSLAVE writes to the "Slave ready file" to inform PEST that it is alive.

	write(6,*)
	call writread(sreadfle,comline,iwait)
        write(6,160)
160     format(' Waiting for signal that PEST is ',
     +  'alive ....')
        write(6,161)
161     format(' Note: this may take until end of current ',
     +  'optimisation iteration if you are',/,
     +         '       restarting a lost agent in mid-PEST run ',
     +         'too soon after it was lost.')
        write(6,162)
162     format(/,' Waiting for PEST....')

C -- PSLAVE now waits for PEST to inform it that it is alive.

250	continue
	if(ispest(mreadfle,1,iwait)) go to 400
	call pestwait(iwait)
C --    The following is done for re-starting. If the slave is started in the
C       same optimisation iteration in which it was halted, PEST will delete sreadfle.
	inquire(file=sreadfle,exist=lexist)
	if(.not.lexist)then
	  call pestwait(iwait3)
	  call writread(sreadfle,comline,iwait)
	end if
	go to 250

400	continue
	inquire(file=sreadfle,exist=lexist)
	if(.not.lexist)then
	  call pestwait(iwait)
	  call writread(sreadfle,comline,iwait)
	end if
500	continue
	if(ispest(mreadfle,2,iwait)) then
          call writread(sreadfle,comline,iwait)
	end if
#ifdef UNIX
	open(unit=50,file=trim(preadfle),
     +  status='old',iostat=ierr)
#else
#ifdef INTEL
        open(unit=50,file=preadfle,blocksize=1,shared,
     +  status='old',iostat=ierr)
#else
#ifdef LF90
	open(unit=50,file=trim(preadfle),
     +  action='readwrite,denynone',status='old',
     +  blocksize=1,iostat=ierr)
#else
	open(unit=50,file=trim(preadfle),
     +  status='old',blocksize=1,iostat=ierr)
#endif
#endif
#endif
	if(ierr.eq.0) then
c	  call pestwait(30)
c	  ii=0
c532	  ii=ii+1
c	  close(unit=50,status='delete',iostat=jerr)
c	  if(jerr.ne.0)then
c	    if(ii.gt.20) then
c	      write(6,535) trim(preadfle)
c535	      format(/,' *** Cannot close/delete file ',a,' ***')
c	      stop
c	    end if
c	    go to 532
	  call pestwait(iwait/2)
	  call closefile(50,preadfle,iwait)
	  call pestwait(iwait/2)
	  call delfile(50,preadfle,iwait)
	  write(6,540)
540	  format(' Running model ....')
	  call pestwait(iwait/2)

#ifdef UNIX
#ifdef UNICOS
	  itemp=ishell(trim(comline))
#else
#ifdef SYS_FUNCTION
          nnn=system(trim(comline))
#else
          call system(trim(comline))
#endif
#endif
#else
          call system(trim(comline))
#endif
	  call pestwait(iwait)
#ifdef UNIX
          open(unit=80,file=trim(oreadfle),
     +    err=9150)
#else
#ifdef INTEL
          open(unit=80,file=oreadfle,blocksize=1,
     +    shared,err=9150)
#else
#ifdef LF90
	  open(unit=80,file=trim(oreadfle),
     +    action='readwrite,denynone',blocksize=1,err=9150)
#else
          open(unit=80,file=trim(oreadfle),blocksize=1,
     +    err=9150)
#endif
#endif
#endif
	  write(80,550,err=9150)
550	  format(' O')
	  call closefile(80,oreadfle,iwait)
	  write(6,555)
555	  format(/,' Model completed: waiting for command to ',
     +    'run model again.',/,
     +    ' (Press <Ctl-C> if you wish to stop PAGENT execution.)')
	end if
        if(nostop.eq.0)then
#ifdef UNIX
          open(unit=81,file=sfinfle,status='old',
     +    iostat=ierr)
#else
#ifdef INTEL
          open(unit=81,file=sfinfle,status='old',
     +    blocksize=1,shared,iostat=ierr)
#else
#ifdef LF90
          open(unit=81,file=sfinfle,status='old',
     +    action='readwrite,denynone',blocksize=1,iostat=ierr)
#else
          open(unit=81,file=sfinfle,status='old',blocksize=1,
     +    iostat=ierr)
#endif
#endif
#endif
	  if(ierr.eq.0) then
	    call closefile(81,sfinfle,iwait)
	    write(6,600)
600	    format(/,' PEST has finished execution. ',
     +      'Agent terminating.')
	    go to 9998
	  end if
        end if
	call pestwait(iwait)
	go to 500

9150	write(6,9160) trim(oreadfle)
9160	format(' Cannot write file ',a,' to inform PEST that model ',
     +  'run complete.')

9998	continue
	call sstop
9999	end


	subroutine delfile(iunit,afile,iwait)

	logical lexist
	integer iunit,icount,junit,ierr,iwait
	character*(*) afile
#ifdef UNIX
	integer itemp,unlink
#endif

	junit=iunit
	if(iunit.lt.0)then
	  inquire(file=afile,exist=lexist)
	  if(.not.lexist) return
	  junit=-junit
	end if
	icount=0
#ifdef UNIX
10	itemp=unlink(trim(afile))
#else
10	call system('del '//trim(afile)//' > nul')
#endif
	call pestwait(iwait)
#ifdef UNIX
	open(unit=junit,file=afile,
     +  status='old',iostat=ierr)
#else
#ifdef INTEL
	open(unit=junit,file=afile,
     +  blocksize=1,shared,status='old',iostat=ierr)
#else
#ifdef LF90
	open(unit=junit,file=afile,
     +  action='readwrite,denynone',status='old',
     +  blocksize=1,iostat=ierr)
#else
	open(unit=junit,file=afile,blocksize=1,
     +  status='old',iostat=ierr)
#endif
#endif
#endif
	if(ierr.ne.0) return
	call pestwait(iwait)
	call closefile_del(junit,afile,iwait)
	icount=icount+1
	if(icount.gt.30)then
	  write(6,30) afile
30	  format(/,' Cannot delete file ',a,/)
	  call sstop
	end if
	call pestwait(iwait)
	go to 10

	end


	subroutine writread(afile,comline,iwait)

	integer iwait
	character*(*) afile,comline

#ifdef UNIX
	open(unit=80,file=trim(afile),err=9000)
#else
#ifdef INTEL
	open(unit=80,file=afile,blocksize=1,shared,err=9000)
#else
#ifdef LF90
	open(unit=80,file=trim(afile),
     +  action='readwrite,denynone',blocksize=1,err=9000)
#else
	open(unit=80,file=trim(afile),blocksize=1,err=9000)
#endif
#endif
#endif
	write(80,180,err=9000) trim(comline)
180	format(a)
#ifdef UNIX
#ifdef UNICOS
	rewind(unit=80)
	write(80,180,err=9000) trim(comline)
#endif
#endif
	call closefile(80,afile,iwait)
	return

9000	write(6,9010) trim(afile)
9010	format(/,' Unable to write file ',a,' to inform PEST that I am ',
     +  'alive.',/)
	call sstop

	end


	logical function ispest(afile,icode,iwait)

	integer icode,ierr,iwait
	character*(*) afile

#ifdef UNIX
	open(unit=81,file=trim(afile),status='old',
     +  iostat=ierr)
#else
#ifdef INTEL
	open(unit=81,file=afile,status='old',
     +  blocksize=1,shared,iostat=ierr)
#else
#ifdef LF90
	open(unit=81,file=trim(afile),status='old',
     +  action='readwrite,denynone',blocksize=1,iostat=ierr)
#else
	open(unit=81,file=trim(afile),status='old',blocksize=1,
     +  iostat=ierr)
#endif
#endif
#endif
	if(ierr.eq.0)then
	  read(81,'(i10)',iostat=ierr) iwait
	  if(ierr.ne.0) iwait=200
c	  write(6,*) iwait
	  call closefile(81,afile,iwait)
	  call pestwait(iwait*2)
          call delfile(81,afile,iwait)
	  ispest=.true.
	  if(icode.eq.1) then
	    write(6,20)
20 	    format(' PEST is alive: waiting for ',
     +      'command to run model ....',/,
     +      ' (Press <Ctl-C> if you wish to stop PAGENT execution.)')
	  else
            write(6,30)
30          format(' PEST is alive: waiting for ',
     +     'command to run model ....',/)
	  endif
	else
	  ispest=.false.
	end if

	return
	end


	subroutine closefile(iunit,afile,iwait)

	integer iunit,icount,ierr,iwait
	character*(*) afile

	icount=1
10	close(unit=iunit,iostat=ierr)
	if(ierr.eq.0) return
	icount=icount+1
	if(icount.gt.10) then
	  write(6,30) afile
30	  format(/,' Cannot close file ',a,/)
	  call sstop
	end if
	call pestwait(iwait)
	go to 10

	end



	subroutine closefile_del(iunit,afile,iwait)

	integer iunit,icount,ierr,iwait
	character*(*) afile

	icount=1
10	close(unit=iunit,iostat=ierr,status='delete')
	if(ierr.eq.0) return
	icount=icount+1
	if(icount.gt.10) then
	  write(6,30) afile
30	  format(/,' Cannot close file ',a,/)
	  call sstop
	end if
	call pestwait(iwait)
	go to 10

	end

