#ifdef BEO
C**********************************************************************C
C***  Slave version just runs those parameters requested by master  ***C
C**********************************************************************C
      SUBROUTINE RUNSLAVE(IFAIL)
#ifdef INTEL
      USE COMMON_MOD, ONLY: ISTOP,MCALL,NCALL
#endif
      USE PESTDATA
      USE BEOPEST, ONLY:BEOCOMMAND,MCALL_NOT_DF_FLAG
      IMPLICIT NONE
      ALLOCATABLE      PAR,OBS,NW,LL,LCINS,IIOBS,OBSN1,OBSN2,
     |                 PWORD,PARDEL,MRKDEL,A
      LOGICAL          BEOGET
      INTEGER          IFAIL
      INTEGER          IERR
      INTEGER          SVDA_PARSET
      INTEGER          SV
      INTEGER          BEOSLAVEFLAG,NPP,ONE,TWO,I
      INTEGER          DF_FLAG
#ifdef INTEL
      INTEGER          JFAIL,NP,NT,NS,ASIZE,NUML,NINSTR,IOS,ILINE,
     |                 INCPAR,NBLBMX,
     |                 NW(:),LL(:),LCINS(:),IIOBS(:),OBSN1(:),OBSN2(:)
#else
      INTEGER          JFAIL,NP,NT,NS,ASIZE,NUML,NINSTR,IOS,ILINE,
     |                 INCPAR,NBLBMX,NCALL,MCALL,
     |                 NW(:),LL(:),LCINS(:),IIOBS(:),OBSN1(:),OBSN2(:)
      INTEGER          ISTOP
#endif
      INTEGER          INT_CTL(4),ICOM,JCOM
      integer          mcall_temp
      integer          outrd_ok,noptmax3_flag
      INTEGER          SET_BS
      DOUBLE PRECISION PAR(:),OBS(:)
      CHARACTER*10     ATEMP
      CHARACTER        PARDEL(:),MRKDEL(:),A(:)
      CHARACTER*23     PWORD(:)
      CHARACTER*256    COMLIN1
      CHARACTER*2000   BUF
#ifndef INTEL
      COMMON /MODCAL/  NCALL,MCALL
      COMMON /SSTOP/   ISTOP
#endif


C -- Get rid of memory that we no longer need.

      DEALLOCATE(PARCHGLIM,PARLBND,PARUBND,IPARGP,PVAL_O,
     +OWGHT,NOBGNM,MODVAL_O,STAT=IERR)

C -- Delete any old "pest.stp" files.

      IFAIL=0
      CALL DELFILE1(IFAIL,'pest.stp',cline)
      IF(IFAIL.NE.0) GO TO 9900

      outrd_ok=0
      IFAIL=0
      ONE=1
      TWO=2
      SVDA_PARSET=0
      BEOSLAVEFLAG=0
      if (SVDA.eq.1) THEN
        BEOSLAVEFLAG=1
        SET_BS=0
        call INITIALISE_BASE(IFAIL,SVDA_SCALADJ,BEOSLAVEFLAG,SET_BS)
C        if (IFAIL.ne.0) call BEOFATAL('INITIALISE BASE failed')
        if(ifail.ne.0)go to 9900
      END IF

C  Allocate memory for files, parameters and observations
      allocate(PARDEL(NTPLFLE),MRKDEL(NINSFLE),STAT=IOS)
      if (IOS.ne.0) call BEOFATAL('ALLOCATE del failed')
      allocate(PAR(NPAR),NW(NPAR),PWORD(NPAR),STAT=IOS)
      if (IOS.ne.0) call BEOFATAL('ALLOCATE par failed')
      allocate(OBS(NOBS),IIOBS(NOBS),OBSN1(NOBS),OBSN2(NOBS),STAT=IOS)
      if (IOS.ne.0) call BEOFATAL('ALLOCATE obs failed')
      IIOBS=0                                       ! An array

C  Read instruction files
      call IOCTL(JFAIL,NPAR,NTPLFLE,NINSFLE,NUML,ASIZE,NINSTR,NBLBMX,
     |NW,TEMPFLE,INFLE,INSFLE,OUTFLE,PARDEL,MRKDEL,APAR,CLINE,ILINE,
     +ONE)
      if(jfail.ne.0)then
        call BEOFATAL_RET()
        ifail=1
        go to 9900
      end if
C  Allocate memory for templates
      if (NUML.eq.0) NUML = 1
      allocate(A(ASIZE),LL(NUML),LCINS(NINSTR),STAT=IOS)
      if (IOS.ne.0) call BEOFATAL('ALLOCATE tpl/ins failed')
C  Read instruction file
      call READINS(LCINS,NINSTR,A,ASIZE,MRKDEL,JFAIL)
      if (JFAIL.ne.0) call BEOFATAL('READINS failed')

c      npp=8192                                 ! I don't like this. But its only the slave. !was !!! commented out 21/9/2011 as per willem
c      call BEOINIT(TRIM(BEOCOMMAND),NPP)						     !was !!! commented out 21/9/2011 as per willem
                                                !!! If uncomment the above, also uncomment !was lines in pest.f

C  Get parameter count NP and target count NT from master
C  Use this as a sanity check
      call BEOCAST(NP,NT,NS,SV)
      if (NP.ne.NPAR .or. NT.ne.NOBS) then
        print *,'NPAR Agent=',NPAR,' Manager=',NP
        print *,'NOBS Agent=',NOBS,' Manager=',NT
        call BEOFATAL
     +  ('Par/Obs count mismatch between manager and agent')
      endif
      if(SV.ne.SVDA)then
        print *,'SVDA Agent=',SVDA,' Manager=',SV
        call BEOFATAL('SVDA mismatch between manager and agent.')
      end if

C  Wait for set of parameters
      MCALL = 0
      MCALL_NOT_DF_FLAG=0
      do while (BEOGET(PAR,IFAIL,INT_CTL))
        IF(IFAIL.NE.0) GO TO 9900
        MCALL = MCALL+1
        IF(MCALL.EQ.1)THEN
        CALL IOCTL(JFAIL,NPAR,NTPLFLE,NINSFLE,NUML,ASIZE,NINSTR,NBLBMX,
     |  NW,TEMPFLE,INFLE,INSFLE,OUTFLE,PARDEL,MRKDEL,APAR,CLINE,ILINE,
     +  TWO)
        if(jfail.ne.0)then
          call BEOFATAL_RET()
          ifail=1
          go to 9900
        end if
        END IF
C  Write model input files
        INCPAR = 0
        call INWRIT(JFAIL,NPAR,PRECIS,NOPNT,NTPLFLE,NW,PAR,SCALE,
     |    OFFSET,PARDEL,PWORD,INFLE,TEMPFLE,APAR,CLINE,MESSFILE,INCPAR,
     |    ITRANS,NOBS)
        if (JFAIL.ne.0) call BEOFATAL('INWRIT failed')

C -- Delete model output files.

        do i=1,ninsfle
          call delfile1(jfail,outfle(i),cline)
          if(jfail.ne.0) then
            ifail=1
            go to 9900
          end if
        end do

C  Run model
        icom=int_ctl(1)
        jcom=int_ctl(2)
        df_flag=int_ctl(3)
        noptmax3_flag=int_ctl(4)
        if(df_flag.eq.0) mcall_not_df_flag=mcall_not_df_flag+1
        if((icom.lt.1).or.(icom.gt.numcom))then
          obs=-1.01D299                 !an array
          call beoput(par,obs)
          write(6,*)
          write(6,30)
30        format(1x,78('-'))
          call writint(atemp,icom)
          write(6,31) trim(atemp)
31        format(' The BEOmanager has asked this agent to ',
     +    'use model command number ',a,'.')
          write(6,32)
32        format(' This command is out of range in current PEST ',
     +    'control file.')
          write(6,30)
          ifail=1
          go to 25
        end if
        if(jcom.ne.0)then
          if(irefobs.eq.0)then
            obs=-1.1d298                  !an array
            call beoput(par,obs)
            write(6,*)
            write(6,30)
            write(6,34)
34          format(' The BEOmanager has activated observation ',
     +      're-referencing.')
            write(6,35)
35          format(' However it is not activated for this agent.')
            write(6,30)
            ifail=1
            go to 25
          end if
        end if
        write(6,10)
10      format(/,' Running model ....')
        if(jcom.eq.0)then
          comlin1=adjustl(comlin(icom))
        else if(jcom.eq.-1)then
          comlin1='r_'//adjustl(trim(comlin(icom)))
        else if(jcom.eq.1)then
          comlin1='d_'//adjustl(trim(comlin(icom)))
        end if
        call SYSTEM(TRIM(comlin1))
C  Read model output files
        mcall_temp=mcall
        mcall=max(1,mcall_not_df_flag)
        if(mcall.gt.1)then
          if(outrd_ok.eq.0)then
            if(noptmax3_flag.eq.1)then
              mcall=1
            end if
          end if
        end if
        call OUTRD(JFAIL,NINSTR,NINSFLE,ASIZE,NUML,NOBS,NBLBMX,LCINS,
     |             LL,OBSN1,OBSN2,IIOBS,OBS,AOBS,A,MRKDEL,CLINE,BUF,
     |             DF_FLAG)
c        if (JFAIL.ne.0) print *,'OUTRD failed - continuing'
        mcall=mcall_temp
        if(jfail.ne.0)then
          write(6,21)
21        format(/,' Problem encountered:-')
          call write_pest_message(2)
        else
          write(6,20)
20        format(' Model run complete.')
          outrd_ok=1
        end if

C  Send results back
        IF(JFAIL.EQ.0)THEN
          call BEOPUT(PAR,OBS)
        ELSE IF(JFAIL.EQ.2)THEN
          OBS=-1.01D297                 !an array
          call BEOPUT(PAR,OBS)
          IFAIL=1
        ELSE
          OBS=-1.01D300                 !an array
          call BEOPUT(PAR,OBS)
          IFAIL=1
        END IF
25      CONTINUE

! -- Check to see if a pause or stop is needed.

        CALL STOPRESS(0)
        IF((ISTOP.EQ.1).OR.(ISTOP.EQ.2)) GO TO 9900

      enddo
      ifail=0
      WRITE(6,9901)
9901  FORMAT(/,' Agent execution terminated by manager.',/)

C     print '(A,I6)','Total model calls:',MCALL

9900  continue

      call svda_deallocate()

      if(allocated(nw)) deallocate(nw,stat=ierr)
      if(allocated(ll)) deallocate(ll,stat=ierr)
      if(allocated(lcins)) deallocate(lcins,stat=ierr)
      if(allocated(iiobs)) deallocate(iiobs,stat=ierr)
      if(allocated(obsn1)) deallocate(obsn1,stat=ierr)
      if(allocated(obsn2)) deallocate(obsn2,stat=ierr)
      if(allocated(par)) deallocate(par,stat=ierr)
      if(allocated(obs)) deallocate(obs,stat=ierr)
      if(allocated(pardel)) deallocate(pardel,stat=ierr)
      if(allocated(mrkdel)) deallocate(mrkdel,stat=ierr)
      if(allocated(a)) deallocate(a,stat=ierr)
      if(allocated(pword)) deallocate(pword,stat=ierr)

      if(ifail.ne.0) ifail=1

      RETURN

      END
C**********************************************************************C
C******  Routine to schedule manage slaves to do series of jobs  ******C
C**********************************************************************C
C  STAT is node status
C   -1 = offline
C    0 = idle
C  k>0 = running job k
C
C  MODE is the job status
C   -1 = not run
C    0 = run
C  k>0 = running on node k
      SUBROUTINE RUNMASTER(PARREG,OBSREG,NJOB,JFAIL,
     +irestart,ptunit,pitn,ptfile,workvecdim,workvec,
     +action_lamforgive,run_packet_index,itrial,do_first,
     +run_slow_fac)
#ifdef INTEL
      USE COMMON_MOD, ONLY: ISTOP,MCALL,NCALL
#endif
      USE PESTDATA
      USE BEOPEST
      USE BEORUN_DATA
      use beopriv, only : strt,suspect,av_operating,text,oldtext,
     +                    node0_keep,node1_keep,slave_speed,
     +                    slave_retired,tcp
      IMPLICIT NONE
      integer          irestart,ptunit,pitn,workvecdim
      integer          action_lamforgive
      integer          run_packet_index
      integer          ierr,ptcount,titn,itemps,jj,icflag
      integer          do_first,do_first1
#ifdef INTEL
      integer          i,istopcount,jjfail,waitflag
#else
      integer          i,istopcount,jjfail,istop,waitflag
#endif
      integer          dd,mm,yy,hh,min
      integer          bsn
      integer          newnode
      integer          iteration
      integer          icount1,icount2
      integer          isn
      integer          ii1
      integer          ntemp
      integer          df_flag
      integer          numslavecull,numslavealive,numslavekeep,kfail
      integer          iistat,iwrite
      integer          not_done
      real             ss
C      ALLOCATABLE      MODE,STAT,PAR
#ifdef INTEL
      INTEGER          PARREG,OBSREG,NJOB,JFAIL,BEOSTAT,
C     |                 MODE(:),STAT(:),node,kjob,node0,node1,
     |                 node,kjob,node0,node1,
     |                 idle,next,done,IOS
#else
      INTEGER          PARREG,OBSREG,NJOB,JFAIL,BEOSTAT,NCALL,MCALL,
C     |                 MODE(:),STAT(:),node,kjob,node0,node1,
     |                 node,kjob,node0,node1,
     |                 idle,next,done,IOS
#endif
      integer          itrial(njob)
      integer          int_ctl(4)
      integer          ii,icount
      integer          ipause
      double precision workvec(workvecdim)
C      DOUBLE PRECISION PAR(:),fast,BEOWALL
      DOUBLE PRECISION  fast,BEOWALL
      double precision  sum
      double precision  current_walltime,walltime,dtemp
      double precision  average_time
      double precision  fast1,dratio
      double precision  run_slow_fac


      CHARACTER*256    BEOTEXT
      character*30     atemp30
      character*10     dstring
      character*10     atemp,anode,ajob,aind,apause
      character*10     aprecs,apoint,atemp10
      character*11     bresult1,bresult2,bresult
      character*5      prefix
      character*(*)    ptfile
#ifndef INTEL
      COMMON /MODCAL/  NCALL,MCALL
      COMMON /SSTOP/ISTOP
#endif

      icflag=0
      istopcount=0
      waitflag=0
      do_first1=do_first
      itrial=0                 ! an array
      numslavecull=0
      numslavealive=0
      numslavekeep=0

C  Allocate memory for parameter and slave arrays
      if(ialloc_beorun_data.eq.0)then
        allocate(PAR(NPAR),MODE(NJOB),STAT(MAXNODE),
     +  CULLED(MAXNODE),STAT=IOS)
        if (IOS.ne.0)
     +  call BEOFATAL('Cannot allocate memory in RUNMASTER')
        ialloc_beorun_data=1
        pardim=npar
        modedim=njob
        statdim=maxnode
C  Mark all slaves as dead
        stat = -1
C  Mark all jobs as not run
	mode = -1
	node0_keep=0
	node1_keep=1
      else
        if(npar.gt.pardim)then
          deallocate(par,stat=ios)
          if(ios.ne.0) go to 5
          allocate(par(npar),stat=ios)
          if(ios.ne.0) go to 5
          pardim=npar
        end if
        if(njob.gt.modedim)then
          deallocate(mode,stat=ios)
          if(ios.ne.0) go to 5
          allocate(mode(njob),stat=ios)
          if(ios.ne.0) go to 5
          modedim=njob
        end if
        if(maxnode.gt.statdim)then
          deallocate(stat,culled,stat=ios)
          if(ios.ne.0) go to 5
          allocate(stat(maxnode),culled(maxnode),stat=ios)
          if(ios.ne.0) go to 5
          statdim=maxnode
        end if
        go to 10
5       call beofatal('Memory allocation error in RUNMASTER')
10      continue
      end if

      culled=0    ! an array

C  Mark all slaves as dead
      if(node0_keep.ne.0)then
        do node=node0_keep,node1_keep
          if(stat(node).ne.-999)then
            STAT(node) = -1
          end if
        end do
      end if
C  Mark all jobs as not run
      MODE = -1
C  kjob is the number of jobs completed
      kjob = 0

      node_new=-1

C -- The restart file situation is handled.

        if(irestart.eq.1)then
          call ffopen(jfail,-ptunit,'w',ptfile,25,cline)
          if(jfail.ne.0)then
            jfail=0
            irestart=0
            go to 1050
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
            mode(itemps)=0
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
                mode(itemps)=-1
                go to 1901
              end if
            end do
          end if
1901      continue
2000      continue
#ifdef INTEL
          call writint(atemp,ptcount)
          write(6,1049) trim(atemp)
1049      format('    Results from ',a,
     +    ' model runs read from restart file.')
          icflag=1
#endif
          kjob=ptcount
          if(do_first1.gt.0)then
            if(ptcount.ge.do_first1)do_first1=0
          end if
C          ncall=ncall+ptcount
          call ffopen(jfail,-ptunit,'w',ptfile,25,cline)
          if(jfail.ne.0)then
            jfail=0
            irestart=0
            go to 1050
          end if
          write(ptunit,iostat=ierr)pitn
          if(ptcount.ne.0)then
            not_done=0
            do i=1,njob
              if(mode(i).eq.0)then
                call retrieve_parallel_register
     +          (jjfail,nobs,ippo,i,workvec,obsregfile)
                if(jjfail.ne.0) go to 9891
                write(ptunit,iostat=ierr)i
                write(ptunit,iostat=ierr)(workvec(jj),jj=1,nobs)
              else
                not_done=not_done+1
              end if
            end do
          end if
#ifdef FLUSHFILE
          call flush(ptunit)
#endif
          if(not_done.eq.0) go to 9999
        end if

1050    continue

        call BEONODE(node0,node1,newnode)
        node_new=newnode
        if(node1.lt.node0)then
          write(6,*)
          write(6,1051)
1051      format('    Waiting for at least one agent to appear....')
          waitflag=1
        end if

        if(parsaverun.ne.0)then
          call writint(atemp10,run_packet_index)
          write(irmr,1052) trim(atemp10)
1052      format(/,'   Run package index = ',a)
        end if

C  Launch jobs until all jobs are completed
      do while (kjob.lt.NJOB)

C -- See whether the escape key has been pressed.

        if(istopcount.eq.100)then
          istopcount=0
          call stopress(0)
          if((istop.eq.2).or.(istop.eq.1)) then
            if(irestart.ne.0) close(unit=ptunit,iostat=ierr)
            go to 9999

! -- We see if we need to reduce the number of slaves.

          else if(istop.lt.0)then
            if(TCP) then
              numslavekeep=-istop
              if(numslavekeep.eq.1000000)numslavekeep=0
              call writint(atemp30,numslavekeep)
              write(6,*)
              write(6,*)
              write(irmr,*)
              write(6,1090) trim(atemp30)
1090          format(' BEOPEST has been instructed to reduce number ',
     +        'of active agents to ',a,'.')
              write(irmr,1090) trim(atemp30)
!              write(6,*)
!              write(6,1100)
1100          format(' Current agent stats:-')
              write(irmr,1100)
!              write(6,1110)
1110          format(' ---------------------')
              write(irmr,1110)
!              write(6,1120)
1120          format('   Node',t20,'Status')
              write(irmr,1120)
              numslavealive=0
              do node=node0,node1
                bsn=BEOSTAT(NODE)
                iistat=stat(node)
                if(bsn.gt.0)then
                  atemp30='idle'
                  numslavealive=numslavealive+1
                  if(stat(node).eq.0) stat(node)=1000000
                else if(bsn.eq.0)then
                  atemp30='working'
                  numslavealive=numslavealive+1
                else if(bsn.eq.-1)then
                  atemp30='cannot communicate'
                end if
!                write(6,1130) node,trim(atemp30)
1130            format(2x,i4,t20,a)
                write(irmr,1130) node,trim(atemp30)
              end do
              numslavecull=numslavealive-numslavekeep
              if(numslavecull.le.0)then
                write(6,1140)
1140            format(' No agents need to be culled.')
                write(irmr,1140)
                write(6,*)
                write(irmr,*)
              else
                call writint(atemp30,numslavecull)
                write(6,1150) trim(atemp30)
                write(irmr,1150) trim(atemp30)
1150            format(' ',a,' agents need to be culled.')
                write(6,*)
                write(irmr,*)
                if(numslavecull.gt.0)then
                  iwrite=0
                  do node=node0,node1
                    if(stat(node).eq.1000000)then
                      if(culled(node).eq.0)then
                        call writint(atemp30,node)
                        if(iwrite.eq.0)then
                          write(6,*)
                          iwrite=1
                        end if
                        write(6,1144) trim(atemp30)
1144                    format('    - culling agent on node ',a,'...')
                        call date_and_time(date=bresult1,time=bresult2)
                        read(bresult1,'(i4,i2,i2)') yy,mm,dd
                        call datestring(dd,mm,dstring)
                        read(bresult2,'(i2,i2,f6.0)') hh,min,ss
                        write(bresult,'(i2.2,'':'',i2.2,'':'',f5.2)')
     +                  hh,min,ss
                        if(bresult(7:7).eq.' ')bresult(7:7)='0'
                        write(irmr,11441) trim(dstring),bresult,
     +                  trim(atemp30)
11441                   format(3x,a,1x,a,':- agent on node ',a,
     +                  ' culled.')
                        call BEOCULL(node)
                        numslavecull=numslavecull-1
                        stat(node)=-1
                        culled(node)=1
                        if(numslavecull.eq.0) go to 1160
                      end if
                    end if
                  end do
                end if
1160            continue
                do node=node0,node1
                  if(stat(node).eq.1000000) stat(node)=0
                end do
              end if
            else
              write(6,*)
              write(6,1151)
1151          format(' File "pest.stp" has instructed BEOPEST to cull ',
     +        'agents.')
              write(6,1152)
1152          format(' This can only be done if BEOPEST uses TCP/IP to ',
     +        'communicate with agents.')
              write(6,1153)
1153          format(' This will instruction will be ignored.')
              write(6,*)
            end if
            istop=0
            do i=1,10
              call delfile1(kfail,'pest.stp',cline)
              if(kfail.eq.0) go to 1080
            end do
            write(errmsg,1085)
1085        format('File "pest.stp" has instructed BEOPEST to cull ',
     +      'agents. This file must now be deleted but, for ',
     +      'reasons unknown to BEOPEST, the operating system ',
     +      'will not allow deletion of this file.')
            go to 9891
1080        continue
          end if
        else
          istopcount=istopcount+1
        end if

C       Poll all slaves to check for a change in status
        call BEONODE(node0,node1,newnode)
        node_new=newnode

C -- If the new node is actually an old node with a job running the situation
C    is now rectified.

        if(node_new.gt.0)then
          if(stat(node_new).gt.0)then
            call date_and_time(date=bresult1,time=bresult2)
            read(bresult1,'(i4,i2,i2)') yy,mm,dd
            call datestring(dd,mm,dstring)
            read(bresult2,'(i2,i2,f6.0)') hh,min,ss
            write(bresult,'(i2.2,'':'',i2.2,'':'',f5.2)') hh,min,ss
            if(bresult(7:7).eq.' ')bresult(7:7)='0'
            call writint(anode,node_new)
            call writint(ajob,stat(node_new))
            write(irmr,10131) trim(dstring),bresult,trim(ajob),
     +      trim(anode)
10131       format(3x,a,1x,a,':- model run ',a,
     +      ' previously running on reappeared node ',a,
     +      ' assigned unstarted status.')
#ifdef FLUSHFILE
            call flush(irmr)
#endif
            mode(stat(node_new))=-1
            stat(node_new)=-1
          end if
          node_new=-1
        end if

        do node=node0,node1
C         Slave is idle
          bsn=BEOSTAT(NODE)
          if (BSN.gt.0) then
C           Mark job as completed
            if(STAT(node).eq.-999)then
              call date_and_time(date=bresult1,time=bresult2)
              read(bresult1,'(i4,i2,i2)') yy,mm,dd
              call datestring(dd,mm,dstring)
              read(bresult2,'(i2,i2,f6.0)') hh,min,ss
              write(bresult,'(i2.2,'':'',i2.2,'':'',f5.2)') hh,min,ss
              if(bresult(7:7).eq.' ')bresult(7:7)='0'
              call writint(anode,node)
              write(irmr,1018) trim(dstring),bresult,trim(anode)
1018          format(3x,a,1x,a,':- model run completed on node ',a,
     +        '; old run so results not needed.')
#ifdef FLUSHFILE
              call flush(irmr)
#endif
              dtemp=beowall(node)
              average_walltime=average_time(dtemp)
              stat(node)=-1
              if(numslavecull.gt.0)then
                if(culled(node).eq.0)then
                  call writint(atemp30,node)
                  write(6,*)
                  write(6,1144) trim(atemp30)
                  call date_and_time(date=bresult1,time=bresult2)
                  read(bresult1,'(i4,i2,i2)') yy,mm,dd
                  call datestring(dd,mm,dstring)
                  read(bresult2,'(i2,i2,f6.0)') hh,min,ss
                  write(bresult,'(i2.2,'':'',i2.2,'':'',f5.2)')
     +            hh,min,ss
                  if(bresult(7:7).eq.' ')bresult(7:7)='0'
                  write(irmr,11441) trim(dstring),bresult,trim(atemp30)
                  call BEOCULL(node)
                  numslavecull=numslavecull-1
                  stat(node)=-1
                  culled(node)=1
                end if
              end if
            else if(STAT(node).gt.0)then
              if(mode(stat(node)).gt.0)then
C  Store  parameters and observations
                call BEOSTORE(node,STAT(node),PARREG,OBSREG,
     +          irestart,ptunit)
                df_flag=0
                if((run_reason(stat(node)).gt.0).and.
     +             (derforgive.ne.0))df_flag=1
                if(last_first_obs.lt.-1.0d300)then
                  if((action_lamforgive.eq.0).and.(df_flag.eq.0))then
                    isn=stat(node)
                    if(itrial(isn).eq.1)then
                      call writint(anode,node)
                      write(errmsg,1015) trim(anode),trim(beotext(node))
1015                  format('Model run failure has occurred on node ',a,
     +                ' identified as "',a,'". See screen printout of ',
     +                'pertinent agent for error message details.')
                      go to 9891
                    else
                      call date_and_time(date=bresult1,time=bresult2)
                      read(bresult1,'(i4,i2,i2)') yy,mm,dd
                      call datestring(dd,mm,dstring)
                      read(bresult2,'(i2,i2,f6.0)') hh,min,ss
                      write(bresult,'(i2.2,'':'',i2.2,'':'',f5.2)')
     +                hh,min,ss
                      if(bresult(7:7).eq.' ')bresult(7:7)='0'
                      itrial(isn)=itrial(isn)+1
                      mode(isn)=-1
                      call writint(anode,node)
                      call writint(ajob,isn)
                      write(irmr,10151) trim(dstring),bresult,
     +                trim(anode),trim(ajob)
10151                 format(3x,a,1x,a,':- model run failure on node ',
     +                a,'; will attempt model run ',a,' one more time.')
#ifdef FLUSHFILE
                      call flush(irmr)
#endif
                      icount=0
                      do ii=node0,node1
                        if(stat(ii).ge.0) icount=icount+1
                      end do
                      if(icount.gt.1)then
                        do ii=node0,node1
                          if(slave_retired(ii).ne.0) icount=icount-1
                        end do
                      end if
                      if(icount.gt.1)then
                        ntemp=max(5,(node1-node0)/2)
                        slave_retired(node)=beo_run_start_count+ntemp  ! Arbitrary - this is when it will unretire.
                      end if
                      go to 10152
                    end if
                  end if
                else if(last_first_obs.lt.-1.0d299)then
                  call writint(anode,node)
                  write(errmsg,1019) trim(anode)
1019              format('The PEST control file employed by the ',
     +            'manager has a different NUMCOM setting from that ',
     +            'employed by the agent on node ',a,
     +            '. This agent was asked to ',
     +            'use a model command with an out-of-range index ',
     +            'according to the PEST control file which it is ',
     +            'using.')
                  go to 9891
                else if(last_first_obs.lt.-1.0d298)then
                  call writint(anode,node)
                  write(errmsg,10191) trim(anode)
10191             format('The PEST control file employed by the ',
     +            'manager activates observation re-referencing. '
     +            'However the agent employed by node ',a,' does ',
     +            'not activate observation re-referencing.')
                  go to 9891
                else if(last_first_obs.lt.-1.0d297)then
                  call writint(anode,node)
                  write(errmsg,10192) trim(anode),
     +            trim(beotext(node))
10192             format('Instruction set failure has occurred ',
     +            'on node ',a,
     +            ' identified as "',a,'". See screen printout of ',
     +            'pertinent agent for error message details. ',
     +            'Check PEST input dataset with PESTCHEK.')
                  go to 9891
                else
C -- Update average walltime.
                  dtemp=beowall(node)
                  average_walltime=average_time(dtemp)
                end if
                kjob = kjob+1
#ifdef INTEL
                if((kjob.eq.1).or.(icflag.eq.1))then
                  write(6,1010)
1010              format('    - number of runs completed...')
                  write(6,'(a)',advance='no') '     '
                  icflag=0
                end if
                write(6,1020) kjob
1020            format(i6,$)
                if(kjob.eq.NJOB)then
                  write(6,*)
                else if(mod(kjob,12).eq.0)then
                  write(6,*)
                  write(6,'(a)',advance='no') '     '
                end if
#endif
                MODE(STAT(node)) = 0
                call date_and_time(date=bresult1,time=bresult2)
                read(bresult1,'(i4,i2,i2)') yy,mm,dd
                call datestring(dd,mm,dstring)
                read(bresult2,'(i2,i2,f6.0)') hh,min,ss
                write(bresult,'(i2.2,'':'',i2.2,'':'',f5.2)') hh,min,ss
                if(bresult(7:7).eq.' ')bresult(7:7)='0'
                call writint(anode,node)
                call writint(ajob,stat(node))
                write(irmr,1012) trim(dstring),bresult,trim(ajob),
     +          trim(anode)
1012            format(3x,a,1x,a,':- model run ',a,
     +          ' completed on node ',a,'.')
                if(do_first1.gt.0)then
                  if(do_first1.eq.1)then
                    call writint(apause,irefobs_pause)
                    write(6,*)
                    write(6,10132) trim(apause)
10132               format('    Observation re-referencing run ',
     +              'completed. Waiting ',a' seconds...')
                    write(6,'(a)',advance='no') '     '
                  else
                    do ii=1,do_first1
                      if(mode(ii).ne.0) go to 10153
                    end do
                    call writint(apause,irefobs_pause)
                    write(6,*)
                    write(6,10141) trim(apause)
10141               format('    Observation re-referencing runs ',
     +              'completed. Waiting ',a,' seconds...')
                    write(6,'(a)',advance='no') '     '
                  end if
                  ipause=irefobs_pause*100
                  call pestwait(ipause)
                  do_first1=0
10153             continue
                end if
#ifdef FLUSHFILE
                call flush(irmr)
#endif
10152           continue
              else
                call date_and_time(date=bresult1,time=bresult2)
                read(bresult1,'(i4,i2,i2)') yy,mm,dd
                call datestring(dd,mm,dstring)
                read(bresult2,'(i2,i2,f6.0)') hh,min,ss
                write(bresult,'(i2.2,'':'',i2.2,'':'',f5.2)') hh,min,ss
                if(bresult(7:7).eq.' ')bresult(7:7)='0'
                call writint(anode,node)
                write(irmr,1018) trim(dstring),bresult,trim(anode)
#ifdef FLUSHFILE
                call flush(irmr)
#endif
                dtemp=beowall(node)
                average_walltime=average_time(dtemp)
              endif
            end if
C           Mark slave as idle
            STAT(node) = 0
            suspect(node)=0
            if(numslavecull.gt.0)then
              if(culled(node).eq.0)then
                call writint(atemp30,node)
                write(6,*)
                write(6,1144) trim(atemp30)
                call date_and_time(date=bresult1,time=bresult2)
                read(bresult1,'(i4,i2,i2)') yy,mm,dd
                call datestring(dd,mm,dstring)
                read(bresult2,'(i2,i2,f6.0)') hh,min,ss
                write(bresult,'(i2.2,'':'',i2.2,'':'',f5.2)') hh,min,ss
                if(bresult(7:7).eq.' ')bresult(7:7)='0'
                write(irmr,11441) trim(dstring),bresult,trim(atemp30)
                call BEOCULL(node)
                numslavecull=numslavecull-1
                stat(node)=-1
                culled(node)=1
              end if
            end if
C  Slave has died
          else if (bsn.lt.0) then
C  Reschedule job
            if (STAT(node).gt.0) then

              call date_and_time(date=bresult1,time=bresult2)
              read(bresult1,'(i4,i2,i2)') yy,mm,dd
              call datestring(dd,mm,dstring)
              read(bresult2,'(i2,i2,f6.0)') hh,min,ss
              write(bresult,'(i2.2,'':'',i2.2,'':'',f5.2)') hh,min,ss
              if(bresult(7:7).eq.' ')bresult(7:7)='0'
              call writint(anode,node)
              call writint(ajob,stat(node))
              write(irmr,1013) trim(dstring),bresult,trim(anode),
     +        trim(ajob)
1013          format(3x,a,1x,a,':- communications failure on node ',
     +        a,'; model run ',a,' will be re-assigned to another ',
     +        'node.')
C              print '(A,I4,2A)','FAILED job',STAT(node),
C     |          ' on node ',TRIM(BEOTEXT(node))
#ifdef FLUSHFILE
              call flush(irmr)
#endif
              MODE(STAT(node)) = -1
            endif
C   Mark slave as offline
            STAT(node) = -1
            text(node)=' '
            oldtext(node)=' '
          else if(bsn.eq.0)then

C   Check that no runs are overdue.

            if(stat(node).gt.0)then
              if(mode(stat(node)).gt.0)then
                if(suspect(node).eq.0)then
                  if(av_operating.ne.0)then
                    current_walltime= walltime()-strt(node)
                    if(current_walltime.gt.
     +                run_slow_fac*average_walltime)then                 !abitrary
                      if(irefobs.eq.0)then                               !note (because of big diff in run times)
                        if(node0.ne.node1)then
                          suspect(node)=1
                          mode(stat(node))=-1
                          call date_and_time
     +                    (date=bresult1,time=bresult2)
                          read(bresult1,'(i4,i2,i2)') yy,mm,dd
                          call datestring(dd,mm,dstring)
                          read(bresult2,'(i2,i2,f6.0)') hh,min,ss
                          write(bresult,'(i2.2,'':'',i2.2,'':'',f5.2)')
     +                    hh,min,ss
                          if(bresult(7:7).eq.' ')bresult(7:7)='0'
                          call writint(anode,node)
                          call writint(ajob,stat(node))
                          write(irmr,1014) trim(dstring),bresult,
     +                    trim(anode),trim(ajob)
1014                      format(3x,a,1x,a,
     +                    ':- overdue run on node ',a,'; ',
     +                    'model run ',a,' may be re-assigned to ',
     +                    'another node.')
#ifdef FLUSHFILE
                          call flush(irmr)
#endif
                        end if
                      end if
                    end if
                  end if
                end if
              end if
            end if
          endif
        enddo

C  Scan slaves to find the fastest idle node

C The following precaution is taken for nodes that have just appeared and have zero wall.
c        icount1=0
c        icount2=0
c        sum=0.0d0
c        do node=node0,node1
c          if(suspect(node).eq.0)then
c           if(STAT(node).eq.0)then
c              dtemp=BEOWALL(node)
c              if(dtemp.gt.0.0d0)then
c                icount1=icount1+1
c                sum=sum+dtemp
c              else
c                icount2=icount2+1
c              end if
c            end if
c          end if
c        end do
c        if(icount1.ne.0)then
c          sum=sum/icount1
c          if(icount2.ne.0)then
c            do node=node0,node1
c              if(suspect(node).eq.0)then
c                if (STAT(node).eq.0)then
c                  if(BEOWALL(node).le.0.0d0) call PUTBEOWALL(node,sum)
c                end if
c              end if
c            end do
c          end if
c        end if

! -- We create a pseudo wall time for any nodes that have just appeared.

        fast1=1.0d300
        do node=node0,node1
          if(BEOWALL(node).gt.0.0d0)then
            if (BEOWALL(node).le.fast1) then
              fast1 = BEOWALL(node)
              ii1 = node
            end if
          endif
        end do
        if((fast1.gt.0.0d0).and.(fast1.lt.1.0d299))then
          do node=node0,node1
            if(slave_speed(node).gt.0.0)then
              if(BEOWALL(node).gt.0.0d0)then
                slave_speed(node)=-slave_speed(node)
              else
                dratio=slave_speed(node)/abs(slave_speed(ii1))
                dratio=BEOWALL(ii1)*dratio
                call PUTBEOWALL(node,dratio)
                slave_speed(node)=-slave_speed(node)
              end if
            endif
          end do
          idle = 0
          fast = 1d300
          do node=node0,node1
            if(suspect(node).eq.0)then
              if (STAT(node).eq.0 .and. BEOWALL(node).le.fast) then
                if(slave_retired(node).eq.0)then
                  fast = BEOWALL(node)
                  idle = node
                end if
              end if
            endif
          enddo
        else
          idle = 0
          fast = 1d300
          do node=node0,node1
            if(suspect(node).eq.0)then
              if (STAT(node).eq.0)then
                if(slave_speed(node).gt.0.0)then
                  if(slave_speed(node).lt.fast) then
                    if(slave_retired(node).eq.0)then
                      fast = slave_speed(node)
                      idle = node
                    end if
                  end if
                endif
              end if
            end if
          end do
        end if

C  Find next unscheduled job
        if(do_first1.eq.0)then
          do next=1,NJOB
            if (MODE(next).lt.0) goto 100
          enddo
          next = 0
        else
          do next=1,do_first1
            if (MODE(next).lt.0) goto 100
          enddo
          next = 0
        end if
C  Push next job to idle slave
100     continue
        if (idle.gt.0 .and. next.gt.0) then
          if(waitflag.eq.1)then
C            write(6,101)
C101         format('    Node has appeared; commencing a model run.',/)
            waitflag=0
          end if
          STAT(idle) = next
          MODE(next) = idle
C  Retrieve parameters
          call RETRIEVE_PARALLEL_REGISTER(IOS,
     |      NPAR,PARREG,next,PAR,PARREGFILE)
          if (IOS.ne.0) call BEOFATAL('Cannot retrieve PAR ')
          beo_run_start_count=beo_run_start_count+1
          do ii=node0,node1
            if(beo_run_start_count.ge.slave_retired(ii))then
              slave_retired(ii)=0
            end if
          end do
          if(parsaverun.ne.0)then
            if(precis.eq.1)then
              aprecs='double'
            else if(precis.eq.0)then
              aprecs='single'
            end if
            if(nopnt.eq.0)then
              apoint='point'
            else if (nopnt.eq.1)then
              apoint='nopoint'
            end if
            iteration=0
            call prmsav(jfail,npar,apar,par,cline,aprecs,apoint,
     +      scale,offset,iteration,run_packet_index,next)
          end if
C  Push to slave
          int_ctl(1)=runcom(next)
          int_ctl(2)=irefobs_runcom(next)
          df_flag=0
          if((run_reason(next).gt.0).and.(derforgive.ne.0))df_flag=1
          int_ctl(3)=df_flag
          int_ctl(4)=0
          if(noptmax.eq.-3)int_ctl(4)=1
          call BEOPUSH(idle,PAR,int_ctl)
C  Write to the run management file.
          call date_and_time(date=bresult1,time=bresult2)
          read(bresult1,'(i4,i2,i2)') yy,mm,dd
          call datestring(dd,mm,dstring)
          read(bresult2,'(i2,i2,f6.0)') hh,min,ss
          write(bresult,'(i2.2,'':'',i2.2,'':'',f5.2)') hh,min,ss
          if(bresult(7:7).eq.' ')bresult(7:7)='0'
          call writint(anode,idle)
          call writint(ajob,next)
          if(irefobs.eq.0)then
          if(numcom.eq.1)then
          write(irmr,1011) trim(dstring),bresult,trim(ajob),trim(anode)
1011      format(3x,a,1x,a,':- model run ',a,' commencing on node ',
     +    a,'.')
          else
          call writint(aind,int_ctl(1))
          write(irmr,10111)trim(dstring),bresult,trim(ajob),
     +    trim(anode),trim(aind)
10111      format(3x,a,1x,a,':- model run ',a,' commencing on node ',
     +    a,'. Model command index = ',a,'.')
          end if
          else
          if(int_ctl(2).eq.0)then
            prefix='none'
          else if(int_ctl(2).eq.1)then
            prefix='"d_"'
          else if(int_ctl(2).eq.-1)then
            prefix='"r_"'
          end if
          if(numcom.eq.1)then
          write(irmr,10113) trim(dstring),bresult,trim(ajob),
     +    trim(anode),trim(prefix)
10113     format(3x,a,1x,a,':- model run ',a,' commencing on node ',
     +    a,'. Model command prefix = ',a,'.')
          else
          call writint(aind,int_ctl(1))
          write(irmr,10114)trim(dstring),bresult,trim(ajob),
     +    trim(anode),trim(aind),trim(prefix)
10114     format(3x,a,1x,a,':- model run ',a,' commencing on node ',
     +    a,'. Model command index = ',a,
     +    '. Model command prefix = ',a,'.')
          end if
          end if
C  Nothing to do right now so sleep a spell before trying again
        else
          call MSLEEP(5)
        endif
      enddo

C  Identify any nodes with unfinished runs.

      node0_keep=node0
      node1_keep=node1
      if(node0.gt.0)then
        do node=node0,node1
          if(stat(node).gt.0)then
            if(mode(stat(node)).gt.0)then
              stat(node)=-999
            end if
          end if
        end do
      end if

      go to 9999

9891  jfail=1


9999  continue
      NCALL = NCALL+NJOB

C  Done - free MODE and STAT arrays
C      deallocate(MODE,STAT,stat=ierr)
C      deallocate(PAR,stat=ierr)
      if(irestart.ne.0) close(unit=ptunit,iostat=ierr)
      return

      END
C**********************************************************************C
C**************  Get number of 'fast' slaves available  ***************C
C**********************************************************************C
      SUBROUTINE FASTSLAVES(NUMSLAVE,manylam)
      USE BEOPEST

      use beopriv, only : slave_speed
      USE BEORUN_DATA
      IMPLICIT NONE
      INTEGER          NUMSLAVE,node,node0,node1,manylam
      INTEGER          newnode
      integer          icount,firstflag,jcount,oldjcount
      DOUBLE PRECISION fast,BEOWALL
      double precision dtemp

      ICOUNT=0
      FIRSTFLAG=BEO_RUN_START_COUNT
C      if(manylam.eq.0)firstflag=100
      jcount=-999
      oldjcount=jcount
1     continue

C  Check for new nodes and get range of nodes
      call BEONODE(node0,node1,newnode)
      node_new=newnode

C -- If this is our first model run, then it must mean a restart situation. We'll wait for
C    a few seconds to see if other slaves arrive.

      if(firstflag.eq.0)then
        if(icount.eq.0)then
C        write(6,10)
C10      format(/,' BEOPEST has detected a restart.')

        write(6,10)
10      format(/,' The first parallel model runs are for testing ',
     +  'parameter upgrades.')
        write(6,11)
11      format(/,' A decision must be made on the number of Marquardt ',
     +  'lambdas to employ.')
C        write(6,12)
C12      format(' You have requested just one parallelized sequence ',
C     +  'of lambda tests ',/,' per iteration.')
        write(6,13)
13      format(/,' BEOPEST will wait for one minute after any agent ',
     +  'appears in order to ')
        write(6,14)
14      format(' provide time for multiple nodes to come on line so ',
     +  'that it knows how')
        write(6,141)
141     format(' many runs to parallelize.')
        icount=1
        call pestwait(1000)
        go to 1
        else
          jcount=node1-node0
          if(jcount.eq.-1)then
            call pestwait(1000)
            go to 1
          end if
          if(jcount.ne.oldjcount)then
            oldjcount=jcount
            icount=1
          else
            icount=icount+1
          end if
          if(icount.gt.6) go to 15
          call pestwait(1000)
          go to 1
        end if
      end if
15    continue

C  Find fastest node (0 means new)

      fast=1.0d300
      if(ialloc_beorun_data.eq.0)then
        do node=node0,node1
          dtemp=abs(slave_speed(node))
          if(dtemp.gt.0.0d0)then
c            call PUTBEOWALL(node,dtemp)
c            slave_speed(node)=-slave_speed(node)
            fast = MIN(dtemp,fast)
          end if
        end do
C  Count nodes faster than 3*fast
        NUMSLAVE = 0
        do node=node0,node1
          dtemp=abs(slave_speed(node))
          if(dtemp.gt.0.0d0)then
            if (dtemp.le.3*fast) NUMSLAVE = NUMSLAVE+1
          end if
        enddo
        go to 9900
      end if

      fast = 1d300
      do node=node0,node1
        if(STAT(node).eq.0)then
          dtemp=BEOWALL(node)
          if(dtemp.gt.0.0d0)then
            fast = MIN(dtemp,fast)
          end if
        end if
      enddo
      fast = max(fast,1D0)

C  Count nodes faster than 2*fast
      NUMSLAVE = 0
      do node=node0,node1
        if(STAT(node).eq.0)then
          dtemp=BEOWALL(node)
          if(dtemp.gt.0.0d0)then
            if (dtemp.le.2*fast) NUMSLAVE = NUMSLAVE+1
          end if
        end if
      enddo

      if(firstflag.eq.0)then
        NUMSLAVE=0
        do node=node0,node1
          NUMSLAVE=NUMSLAVE+1
        end do
      end if

9900  continue
      return

      END
C**********************************************************************C
C***********************  Read instruction file  **********************C
C**********************************************************************C
      SUBROUTINE READINS(LCINS,NINSTR,A,ASIZE,MRKDEL,JFAIL)
      USE PESTDATA
      IMPLICIT NONE
      INTEGER NINSTR
      INTEGER I,J,INS,ISUM,LCINS(NINSTR),ASIZE,NBLC,JFAIL
      CHARACTER MRKDEL(NINSFLE),A(ASIZE)

      INS  = 0
      ISUM = 0
      do I=1,NINSFLE
        call FFOPEN(JFAIL,INST,'r',INSFLE(I),78,CLINE)
        if (JFAIL.ne.0) RETURN
        read(INST,*)
        INS = INS+1
        CLINE(1:1) = CHAR(2)
        CLINE(2:2) = ' '
        CLINE(3:LEN(CLINE)) = OUTFLE(I)
        LCINS(INS) = ISUM+1
        do J=1,LEN_TRIM(CLINE)
          ISUM = ISUM+1
          A(ISUM) = CLINE(J:J)
        enddo
50      READ(INST,'(A)',END=180) CLINE
        call TABREM(CLINE)
        if (INDEX(CLINE,MRKDEL(I)).eq.0) call CMPRSS(CLINE)
        NBLC = LEN_TRIM(CLINE)
        if (NBLC.eq.0) goto 50
        INS = INS+1
        LCINS(INS) = ISUM+1
        do J=1,NBLC
          ISUM = ISUM+1
          A(ISUM) = CLINE(J:J)
        enddo
        goto 50
180     CLOSE(UNIT=INST)
      enddo
      END
#endif



! -- Other matters.
!    Send security code with each job.
