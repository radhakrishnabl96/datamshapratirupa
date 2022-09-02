C**********************************************************************C
C***********************  BeoPEST Public Data  ************************C
C**********************************************************************C
      MODULE BEOPEST
      IMPLICIT NONE
      LOGICAL BEOMASTER,BEOSLAVE,BEOLASTLOC
      INTEGER MAXNODE
      DOUBLE PRECISION last_first_obs
      INTEGER PARLAM_FLAG
      INTEGER NODE_NEW
      DOUBLE PRECISION AVERAGE_WALLTIME
      CHARACTER*200 BEOCOMMAND
      INTEGER BEO_RUN_START_COUNT
      INTEGER MCALL_NOT_DF_FLAG
      INTEGER, ALLOCATABLE :: RUNCOM(:)
      INTEGER, ALLOCATABLE :: IREFOBS_RUNCOM(:)
      INTEGER, ALLOCATABLE :: RUN_REASON(:)
      END
C**********************************************************************C
C***********************  BeoPEST private data  ***********************C
C**********************************************************************C
C  BeoPEST/TCP uses LIVE to track nodes
C  LIVE
C    -1 = idle
C     0 = offline
C     1 = running
C  BeoPEST/MPI uses IRDQ to store requests
      MODULE BEOPRIV
      IMPLICIT NONE
      ALLOCATABLE      LIVE,IRDQ,WALL,STRT,NUM,TEXT,PARBUF,OBSBUF
      ALLOCATABLE      SLAVE_SPEED
      allocatable      oldtext,super_needed
      allocatable      suspect
      LOGICAL          TCP,MPI
      INTEGER          NPAR,NOBS,node0_keep,node1_keep
      INTEGER          LIVE(:),IRDQ(:,:),NUM(:)
      INTEGER          super_needed(:)
      integer          suspect(:)
      DOUBLE PRECISION WALL(:),STRT(:),TOTIM,TSUM
      DOUBLE PRECISION PARBUF(:,:),OBSBUF(:,:)
      REAL             SLAVE_SPEED(:)
      CHARACTER*256    TEXT(:)
      character*256    oldtext(:)
      character*500    charbuf
      integer          ient_avbuf
      integer          nument_avbuf
      integer          av_operating
      double precision, allocatable :: avbuf(:)
      integer          slave_retired(:)
      allocatable      slave_retired

      END

C -- I added the following module myself because I get nervous about
C    allocating and de-allocating. I don't trust compilers.

      MODULE BEORUN_DATA
        IMPLICIT NONE
        INTEGER                          IALLOC_BEORUN_DATA
        DOUBLE PRECISION, ALLOCATABLE :: PAR(:)
        INTEGER, ALLOCATABLE          :: MODE(:),STAT(:),CULLED(:)
        INTEGER                          PARDIM,MODEDIM,STATDIM
      END


C**********************************************************************C
C********************  Initialize communications  *********************C
C**********************************************************************C
      SUBROUTINE BEOINIT(HOST,MXNODE)
      USE           BEOPEST
      INCLUDE       'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
      INTEGER IOS
#endif
      INTEGER          MXNODE,NODE
      DOUBLE PRECISION WALLTIME
      CHARACTER*10     CNN
      CHARACTER*(*)    HOST

      TCP = (MXNODE.gt.0)
      MPI = .not.TCP
C  Initialize TCP
      if (TCP) then
        MAXNODE = MXNODE
        call TCPOPEN(MAXNODE,TRIM(HOST)//CHAR(0),NODE)
C  Initialize MPI
      else
#ifdef BEOMPI
        call MPI_INIT(IOS)
        call MPI_COMM_RANK(MPI_COMM_WORLD,NODE   ,IOS)
        call MPI_COMM_SIZE(MPI_COMM_WORLD,MAXNODE,IOS)
        if (MAXNODE.eq.1) STOP 'Cannot run BeoPEST/MPI with one node'
#else
        STOP 'Compiled without MPI'
#endif
      endif

C  Mark master and slave
      BEOMASTER = (NODE.eq.0)
      BEOSLAVE  = (NODE.gt.0)
C  Allocate memory for tracking slave execution times
      if (BEOMASTER) then
        ALLOCATE(WALL(MAXNODE),STRT(MAXNODE),NUM(MAXNODE),
     +  TEXT(MAXNODE),oldtext(maxnode),super_needed(maxnode),
     +  suspect(maxnode))
        ALLOCATE(SLAVE_SPEED(MAXNODE))
        SLAVE_SPEED=0.0  ! an array
        suspect=0        ! an array
        oldtext=' '      ! an array
        allocate(slave_retired(maxnode))
        slave_retired=0  ! an array
        NUM  = 0
C  The WALL setting done here is the initial speed for all MPI slaves
C  This must be a positive value
        WALL = 1
        super_needed=0
        suspect=0
        nument_avbuf=0
        ient_avbuf=0
        av_operating=0
      else
        MAXNODE = 1
      endif

C  Initialize MASTER array for tracking slaves
      if (BEOMASTER) then
        TSUM = 0
        TOTIM = WALLTIME()
        if (TCP) then
          ALLOCATE(LIVE(MAXNODE))
          LIVE = 0
        else
#ifdef BEOMPI
          ALLOCATE(IRDQ(2,MAXNODE))
          IRDQ = MPI_REQUEST_NULL
C         Directory name for each slave
          do NODE=1,MAXNODE
            WRITE(CNN,'(I10)') NODE
            TEXT(NODE) = TRIM(HOST)//TRIM(ADJUSTL(CNN))
          enddo
#endif
        endif
C  On MPI slaves change directory
      else if (MPI) then
        WRITE(CNN,'(I10)') NODE
        call SETDIR(TRIM(HOST)//TRIM(ADJUSTL(CNN))//CHAR(0))
      endif
      END
C**********************************************************************C
C***********************  Get number of nodes  ************************C
C**********************************************************************C
      SUBROUTINE BEONODE(KP0,KP1,NEWNODE)
      USE     BEOPEST
      use pestdata, only : irmr
      INCLUDE 'beopest.inc'
      INTEGER KP0,KP1
      logical lopened
      integer  dd,mm,yy,hh,min,j,timeflag,jj
      integer nn
      integer newnode
      real     ss
      character*7 dstring
      character*10 anew
      character*11 bresult1,bresult2,bresult

      NEWNODE=-1
      timeflag=0
      KP0 = 1
      if (TCP) then
        call TCPNODE(LIVE,WALL,SLAVE_SPEED,TEXT,KP1)
      else
        KP1 = MAXNODE-1
      endif
      if(kp1.ge.1) then
        inquire(unit=irmr,opened=lopened)
        do j=1,kp1
          if(text(j).ne.oldtext(j))then
            if(timeflag.eq.0)then
              call date_and_time(date=bresult1,time=bresult2)
              read(bresult1,'(i4,i2,i2)') yy,mm,dd
              call datestring(dd,mm,dstring)
              read(bresult2,'(i2,i2,f6.0)') hh,min,ss
              write(bresult,'(i2.2,'':'',i2.2,'':'',f5.2)') hh,min,ss
              if(bresult(7:7).eq.' ')bresult(7:7)='0'
              timeflag=1
            end if
            call writint(anew,j)
            if(lopened)then
              write(irmr,10) trim(dstring),bresult,trim(anew),
     +        trim(text(j))
10            format(3x,a,1x,a,':- index of ',a,
     +        ' assigned to node at ',
     +        'working directory "',a,'".')
            end if
            oldtext(j)=text(j)
            nn=index(text(j),'\\')
            if((nn.eq.0).or.(nn.eq.1))then
              write(6,20) trim(text(j))
20            format(/,'    New agent has appeared:-',/,
     +        '    Local working directory "',a,'"')
            else
              if(text(j)(1:nn-1).eq.' ') then
                write(6,20) trim(text(j))
              else
                write(6,21)
21              format(/,'    New agent has appeared:-')
                write(6,22) text(j)(1:nn-1)
22              format('    Agent host: "',a,'"')
                write(6,23) trim(text(j)(nn+2:))
23              format('    Agent working directory: "',a,'"')
                write(6,231) trim(anew)
231             format('    Node number assigned to agent: ',a)
              end if
            end if
            if(slave_speed(j).eq.0.0)then
              write(6,24) 1.0e30
            else
              write(6,24) 10.0/slave_speed(j)
24            format('    Agent speed index: ',1pg12.5,/)
            end if
            newnode=j
          end if
        end do
#ifdef FLUSHFILE
        if(lopened)then
          call flush(irmr)
        end if
#endif
      end if
      END
C**********************************************************************C
C*************************  Terminate Slave  **************************C
C**********************************************************************C
      SUBROUTINE BEOCULL(NODE)
      USE     BEOPEST
      INCLUDE 'beopest.inc'
      INTEGER NODE

      if (.not.TCP) STOP 'Internal error:  BEOCULL called in MPI mode'
      if (LIVE(NODE).ne.0) call BEOPUTINT(NODE,0)
      LIVE(NODE) = 0
      END
C**********************************************************************C
C**********************  Terminate connections  ***********************C
C**********************************************************************C
      SUBROUTINE BEOFIN
      use pestdata, only : irmr
      USE      BEOPEST
      USE      BEORUN_DATA
      INCLUDE 'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
      INTEGER IOS
#endif
      INTEGER K,KP0,KP1
      INTEGER NEWNODE
      integer ierr
      DOUBLE PRECISION WALLTIME
      CHARACTER*256 BEOTEXT

C  Master sends count zero (close) to slaves before closing TCP connections
      if (BEOMASTER) then
        print *
        write(irmr,*)
        write(irmr,'(a)') ' BEOSTATS:-'
        call BEONODE(KP0,KP1,NEWNODE)
        do K=KP0,KP1
          print '(1X,A,I4,2X,F9.3,I5,1X,A)','Node',
     |      K,WALL(K),NUM(K),TRIM(BEOTEXT(K))
          write(irmr,'(3X,A,I4,2X,F9.3,I5,1X,A)') 'Node',
     |      K,WALL(K),NUM(K),TRIM(BEOTEXT(K))
        enddo
        TOTIM = WALLTIME()-TOTIM
        print '(1X,A,F12.1)','Total CPU time     ',TSUM
        write(irmr,'(3X,A,F12.1)') 'Total CPU time     ',TSUM
        print '(1X,A,F12.1)','Total elapsed time ',TOTIM
        write(irmr,'(3X,A,F12.1)') 'Total elapsed time ',TOTIM
        print '(1X,A,F12.3)','Speedup            ',TSUM/TOTIM
        write(irmr,'(3X,A,F12.3)') 'Speedup            ',TSUM/TOTIM
C       Shut down slaves and master communications
        if (TCP) then
          do K=KP0,KP1
            if (LIVE(K).ne.0) call BEOPUTINT(K,0)
          enddo
          call TCPCLOSE
          DEALLOCATE(LIVE)
        else
#ifdef BEOMPI
          do K=KP0,KP1
            call BEOPUTINT(K,0)
          enddo
          call MPI_Finalize(IOS)
          DEALLOCATE(IRDQ)
#endif
        endif
        if(allocated(wall)) deallocate(wall,stat=ierr)
        if(allocated(strt)) deallocate(strt,stat=ierr)
        if(allocated(num)) deallocate(num,stat=ierr)
        if(allocated(text)) deallocate(text,stat=ierr)
        if(allocated(parbuf)) deallocate(parbuf,stat=ierr)
        if(allocated(obsbuf)) deallocate(obsbuf,stat=ierr)
        if(allocated(oldtext)) deallocate(oldtext,stat=ierr)
        if(allocated(super_needed)) deallocate(super_needed,stat=ierr)
        if(allocated(suspect)) deallocate(suspect,stat=ierr)
        if(allocated(avbuf)) deallocate(avbuf,stat=ierr)

        if(allocated(par)) deallocate(par,stat=ierr)
        if(allocated(mode)) deallocate(mode,stat=ierr)
c        if(allocated(stat)) deallocate(stat,stat=ierr)
        deallocate(stat,stat=ierr)
        deallocate(culled,stat=ierr)
        if(allocated(slave_speed)) deallocate(slave_speed,stat=ierr)
        if(allocated(runcom)) deallocate(runcom,stat=ierr)
        if(allocated(irefobs_runcom)) deallocate(irefobs_runcom,
     +  stat=ierr)
        if(allocated(run_reason)) deallocate(run_reason,
     +  stat=ierr)
        if(allocated(slave_retired)) deallocate(slave_retired,
     +  stat=ierr)

C  Slave just closes connection
      else if (BEOSLAVE) then
        if (TCP) then
          call TCPCLOSE
        else
#ifdef BEOMPI
          call MPI_Finalize(IOS)
#endif
        endif
      endif
      END
C**********************************************************************C
C********************  Broadcast Parameter Counts  ********************C
C**********************************************************************C
      SUBROUTINE BEOCAST(NP,NT,CSUM,SV)
      USE     BEOPEST
      INCLUDE 'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
#endif
      INTEGER NP,NT,CSUM,SV,N,IOS
      CHARACTER*8 AVERSION,BVERSION

      INCLUDE 'version.inc'
      BVERSION = AVERSION

C  MPI Broadcast values
      if (MPI) then
#ifdef BEOMPI
        call MPI_BCAST(BVERSION,8,MPI_CHARACTER,0,MPI_COMM_WORLD,IOS)
        call MPI_BCAST(NP,1,MPI_INTEGER,0,MPI_COMM_WORLD,IOS)
        call MPI_BCAST(NT,1,MPI_INTEGER,0,MPI_COMM_WORLD,IOS)
        call MPI_BCAST(CSUM,1,MPI_INTEGER,0,MPI_COMM_WORLD,IOS)
        call MPI_BCAST(SV,1,MPI_INTEGER,0,MPI_COMM_WORLD,IOS)
#endif
C  Set values to send on master
      else if (BEOMASTER) then
        call TCPCAST(BVERSION,NP,NT,CSUM,SV)
C  Receive values on slave
      else if (BEOSLAVE) then
        call TCPRECV(0,BVERSION,1,8)
        call TCPRECV(0,NP,4,1)
        call TCPRECV(0,NT,4,1)
        call TCPRECV(0,CSUM,4,1)
        call TCPRECV(0,SV,4,1)
      endif
C  Set values for other routines to use
      NPAR = NP
      NOBS = NT

C  Check for version skew
      if (AVERSION.ne.BVERSION) call BEOFATAL('BeoPEST versions differ.'
     |  //'  Expected '//TRIM(AVERSION)//' but got '//TRIM(BVERSION))

C  Allocate memory for buffers
      if (BEOMASTER) then
C       TCP manages memory internally - just need one buffer
        if (TCP) then
          N = 1
C       With MPI allocate memory for all nades
        else
          N = MAXNODE
        endif
C       Allocate memory
        ALLOCATE(PARBUF(NPAR,N),OBSBUF(NOBS,N),STAT=IOS)
        if (IOS.ne.0) call BEOFATAL('Cannot allocate buffer memory')
      endif
      END
C**********************************************************************C
C*******************  Push run from master to slave  ******************C
C**********************************************************************C
      SUBROUTINE BEOPUSH(IDST,PAR,INT_CTL)

      use svd_parm_data, only : bnpar,BNSTMP,svalue,bitrans,sval,
     +                   last_nregadjpar,last_nlscaladj,
     +                   tbx,sf,svda_beoparset
      use pestdata, only : svda
      INCLUDE 'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
      INTEGER IOS
#endif
      INTEGER          IDST,N,J
      INTEGER          INT_CTL(4)
      DOUBLE PRECISION PAR(NPAR),WALLTIME

C  Keep track of which slaves knows the new superparameters
      if (svda.eq.1 .and. svda_beoparset.eq.1) then
        super_needed = 1
        svda_beoparset = 0
      endif
C  Decide what to send
      if (svda.eq.1 .and. super_needed(IDST).eq.1) then
        N = 2
        super_needed(IDST) = 0
      else
        N = 1
      endif

C  Post asynchronous receive for parameters and targets
      if (TCP) then
        call TCPPOST(IDST,0,8,NPAR)
        call TCPPOST(IDST,1,8,NOBS)
      else
#ifdef BEOMPI
C  Post asynchronous receive for parameters and targets
        call MPI_IRECV(PARBUF(1,IDST),NPAR,MPI_DOUBLE_PRECISION,
     |     IDST,1,MPI_COMM_WORLD,IRDQ(1,IDST),IOS)
        call MPI_IRECV(OBSBUF(1,IDST),NOBS,MPI_DOUBLE_PRECISION,
     |     IDST,2,MPI_COMM_WORLD,IRDQ(2,IDST),IOS)
#endif
      endif

C  Mark start on this node
      if (TCP) LIVE(IDST) = 1
      STRT(IDST) = WALLTIME()
C  Send parameters to slave
      call BEOPUTINT(IDST,N)
      call BEOPUTDOUBLES(IDST,PAR,NPAR)
C  Send control integer array to slave
      call BEOPUTINTS(IDST,INT_CTL,4)

C  Update SVD on slave (if necessary)
      if (N.eq.2) then
        call BEOPUTINT(IDST,BNSTMP)
        call BEOPUTINT(IDST,LAST_NREGADJPAR)
        call BEOPUTDOUBLES(IDST,SVALUE,NPAR)
        call BEOPUTDOUBLES(IDST,SVAL,BNPAR)
        call BEOPUTINTS(IDST,BITRANS,BNPAR)
        do j=1,BNSTMP
          call BEOPUTDOUBLES(IDST,tbx(1,j),NPAR-LAST_NREGADJPAR)
        enddo
        if (LAST_NLSCALADJ.ne.0) call BEOPUTDOUBLES(IDST,SF,BNPAR)
      endif

      return
      END
C**********************************************************************C
C******************  Wait for parameters from master  *****************C
C**********************************************************************C
      FUNCTION BEOGET(PAR,JFAIL,int_ctl)
      use pestdata, only : svda,apar,scale
      use svd_parm_data, only : bnpar,last_nregadjpar,BNSTMP,
     +                   last_nlscaladj,bntpfle,include_prior,svalue,
     +                   bapar,bitrans,sval,blbound,bubound,bscale,
     +                   boffset,SF,TBX,ndim_tbx

      INCLUDE 'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
      INTEGER STAT(MPI_STATUS_SIZE),IOS
#endif
      LOGICAL BEOGET
      INTEGER N,J,NREGADJPAR,NEEDED,IFAIL,IERR,JFAIL
      integer iallocflag
      DOUBLE PRECISION PAR(NPAR)
      integer int_ctl(4)

      JFAIL=0
C  Get count:  0 => done
      call BEOGETINT(0,N)
C  Mark whether we got parameters or done
      BEOGET = (N.gt.0)
C  Return here if done
      if (.not.BEOGET) return

C  Get parameters from master
      call BEOGETDOUBLES(0,PAR,NPAR)
C  Get control integer array to slave
      call BEOGETINTS(0,INT_CTL,4)
C  Get updated superparameters (if necessary)
      if (N.eq.2) then
C       Pause for paranoia
        if (svda.ne.1) call BEOFATAL('Agent not started with SVDA')
C       Read new Super-Base mapping from master
C       Do all reads right away to free up the master ASAP
C       Some memory was allocated in INITIALISE_BASE. However not all memory was allocated.
        call BEOGETINT(0,BNSTMP)
        call BEOGETINT(0,NREGADJPAR)
        call BEOGETDOUBLES(0,SVALUE,NPAR)
        call BEOGETDOUBLES(0,SVAL,BNPAR)
        call BEOGETINTS(0,BITRANS,BNPAR)
        iallocflag=0
        if(allocated(tbx))then
          if(ndim_tbx.gt.npar-nregadjpar)then
            deallocate(tbx,stat=ierr)
            if(ierr.ne.0) call BEOFATAL('Failed to deallocate TBX.')
            iallocflag=1
          end if
        else
          iallocflag=1
        end if
        if(iallocflag.eq.1)then
          allocate(tbx(npar-nregadjpar,bnstmp),stat=ierr)
          if(ierr.ne.0) call BEOFATAL('Failed to allocate TBX.')
          ndim_tbx=npar-nregadjpar
        end if
        do J=1,BNSTMP
          call BEOGETDOUBLES(0,TBX(1,J),NPAR-NREGADJPAR)
        enddo
        if (LAST_NLSCALADJ.ne.0) call BEOGETDOUBLES(0,SF,BNPAR)

C       Write a new parcalc.tpl
        write(6,'(/A)') ' BEOAGENT writing new parcalc.tpl....'
        call write_parcalc_tpl(IFAIL,NREGADJPAR,LAST_NLSCALADJ)
C        if (IFAIL.ne.0) call BEOFATAL('Failed to write parcalc.tpl')
        if(ifail.ne.0) then
          jfail=1
          return
        end if
        write(6,'(A/)') ' - file parcalc.tpl written ok.'
      endif

C  Return true (data read)
      return
      END
C**********************************************************************C
C**********************  Send targets to master  **********************C
C**********************************************************************C
      SUBROUTINE BEOPUT(PAR,OBS)
      INCLUDE          'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
      INTEGER IOS,I
#endif
      DOUBLE PRECISION PAR(NPAR),OBS(NOBS)

C  Send observations to master
      if (TCP) then
        call TCPSEND(0,PAR,8,NPAR)
        call TCPSEND(0,OBS,8,NOBS)
      else
#ifdef BEOMPI
        call MPI_SEND(PAR,NPAR,MPI_DOUBLE_PRECISION,
     |                0,1,MPI_COMM_WORLD,IOS)
        call MPI_SEND(OBS,NOBS,MPI_DOUBLE_PRECISION,
     |                0,2,MPI_COMM_WORLD,IOS)
#endif
      endif
      END
C**********************************************************************C
C**********************  Check for completed run  *********************C
C**********************************************************************C
      FUNCTION BEOSTAT(IDST)
      INCLUDE          'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
      LOGICAL FLAG
      INTEGER STAT(MPI_STATUS_SIZE),K,IOS
#endif
      LOGICAL          NEW
      INTEGER          BEOSTAT,IDST,IFLG
      DOUBLE PRECISION WALLTIME
C  RETURNS
C   1 done
C   0 working
C  -1 failed

      NEW = .false.
      if (TCP) then
C  Running
        if (LIVE(IDST).gt.0) then
C         Check if results were received
          call TCPTEST(IDST,IFLG)
C         Node has completed
          if (IFLG.gt.0) then
            NEW = .true.
            LIVE(IDST) = -1
C         Mark node as failed
          else if (IFLG.lt.0) then
            LIVE(IDST) = 0
          endif
C         Mark results as received
          BEOSTAT = IFLG
C  Idle => done
        else if (LIVE(IDST).lt.0) then
          BEOSTAT = 1
C  Dead
        else
          BEOSTAT = -1
        endif
      else
#ifdef BEOMPI
C  Check for outstanding receives that have completed
        do K=1,2
          if (IRDQ(K,IDST).ne.MPI_REQUEST_NULL) then
            call MPI_TEST(IRDQ(K,IDST),FLAG,STAT,IOS)
            if (FLAG) then
              NEW = .true.
              IRDQ(K,IDST) = MPI_REQUEST_NULL
            endif
          endif
        enddo
C  Return status
        if (IRDQ(1,IDST).eq.MPI_REQUEST_NULL .and.
     |      IRDQ(2,IDST).eq.MPI_REQUEST_NULL) then
          BEOSTAT = 1
        else
          BEOSTAT = 0
        endif
#else
        BEOSTAT = 0
#endif
      endif
      if (BEOSTAT.gt.0 .and. NEW) then
        NUM(IDST) = NUM(IDST)+1
        WALL(IDST) = WALLTIME() - STRT(IDST)
        TSUM = TSUM + WALL(IDST)
      endif
      END
C**********************************************************************C
C****************************  Get run time  **************************C
C**********************************************************************C
      FUNCTION BEOWALL(IDST)
      INCLUDE          'beopest.inc'
      INTEGER          IDST
      DOUBLE PRECISION BEOWALL

      BEOWALL = WALL(IDST)
      END

C**********************************************************************C
C****************************  Put run time  **************************C
C**********************************************************************C
      SUBROUTINE PUTBEOWALL(IDST,dtime)
      INCLUDE          'beopest.inc'
      INTEGER          IDST
      double precision dtime
      WALL(IDST)=dtime
      END

C**********************************************************************C
C**********************  Check for completed run  *********************C
C**********************************************************************C
      SUBROUTINE BEOFATAL(MSG)
      INCLUDE      'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
      INTEGER IOS
#endif
      CHARACTER(*) MSG

#ifdef BEOMPI
      if (MPI) call MPI_Finalize(IOS)
#endif
      call FATAL(TRIM(MSG)//CHAR(0))
      END

      SUBROUTINE BEOFATAL_RET()
      INCLUDE      'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
      INTEGER IOS
#endif

#ifdef BEOMPI
      if (MPI) call MPI_Finalize(IOS)
#endif
      END

C**********************************************************************C
C**************  Return wall time in seconds since 1970  **************C
C**********************************************************************C
      FUNCTION WALLTIME()
      IMPLICIT NONE
      DOUBLE PRECISION WALLTIME
      INTEGER  d(8),MDO(12),JY,JD
      DATA MDO /306,337,0,31,61,92,122,153,184,214,245,275/

C  Get system date and time (wall clock local time)
      call DATE_AND_TIME(values=d)
C  Calculate sidereal Julian day for dates in Gregorian Calendar
      if (d(2).lt.3) then
        JY = d(1)-1
      else
        JY = d(1)
      endif
      JD = 1721119 + d(3) + MDO(d(2)) + 365*JY + JY/4 - JY/100 + JY/400
C  1-Jan-1970 is JD 2440588
C  Time is adjusted to UTC to avoid daylight savings glitches
      WALLTIME = 86400*(JD-2440588) + 3600*d(5) + (d(6)-d(4))*60
     |         + d(7) + 1D-3*d(8)
      END
C**********************************************************************C
C***********************  Return name of node  ************************C
C**********************************************************************C
      FUNCTION BEOTEXT(K)
      USE     BEOPEST
      INCLUDE 'beopest.inc'
      INTEGER       K
      CHARACTER*256 BEOTEXT

      BEOTEXT = TEXT(K)
      END
C**********************************************************************C
C***********************  Return name of node  ************************C
C**********************************************************************C
      SUBROUTINE BEOSTORE(NODE,JOB,PARREG,OBSREG,
     +        irestart,ptunit)
      USE     PESTDATA,  ONLY: PARREGFILE,OBSREGFILE
      USE     BEOPEST
      INCLUDE 'beopest.inc'
      INTEGER NODE,JOB,PARREG,OBSREG,IOS,K,I
      integer irestart,ptunit
      integer ierr,jj

C  Copy data from TCP buffer and store in register file
      if (TCP) then
        K = 1
        call TCPLOAD(NODE,0,PARBUF(1,K),8,NPAR)
        call TCPLOAD(NODE,1,OBSBUF(1,K),8,NOBS)
#ifdef BEOMPI
C  Set index to node
      else
        K = NODE
#endif
      endif
C  Store data to register files
      call STORE_PARALLEL_REGISTER(IOS,
     |  NPAR,PARREG,JOB,PARBUF(1,K),PARREGFILE)
      if (IOS.ne.0) call BEOFATAL('Cannot store returned PAR')
      call STORE_PARALLEL_REGISTER(IOS,
     |  NOBS,OBSREG,JOB,OBSBUF(1,K),OBSREGFILE)
      if (IOS.ne.0) call BEOFATAL('Cannot store returned OBJ')
      last_first_obs=obsbuf(1,k)

      if(irestart.ne.0)then
        write(ptunit,iostat=ierr) job
        write(ptunit,iostat=ierr) (obsbuf(jj,k),jj=1,nobs)
        if(ierr.ne.0) call BEOFATAL('Cannot save restart OBJ data.')
#ifdef FLUSHFILE
        call flush(ptunit)
#endif
      end if

      END


C**********************************************************************C
C***********************  Calculate average walltime ******************C
C**********************************************************************C

      double precision function average_time(new_time)

      use beopriv
      use pestdata, only : nespar
      implicit none

      double precision, intent(in) :: new_time

      integer i,n
      double precision dtemp

      if(nument_avbuf.eq.0)then
        nument_avbuf=max(nespar,10)
        ient_avbuf=0
        allocate(avbuf(nument_avbuf))
        avbuf=0      ! an array
      end if
      ient_avbuf=ient_avbuf+1
      if(ient_avbuf.gt.nument_avbuf)ient_avbuf=1
      avbuf(ient_avbuf)=new_time
      if(av_operating.eq.0)then
        if(ient_avbuf.gt.1)then
          av_operating=1
        end if
      end if

      dtemp=0.0d0
      n=0
      do i=1,nument_avbuf
        if(avbuf(i).ne.0.0d0)then
          n=n+1
          dtemp=dtemp+avbuf(i)
        end if
      end do
      if(dtemp.eq.0.0d0)then
        average_time=0.0d0
      else
        average_time=dtemp/n
      end if

      return
      end

C**********************************************************************C
C************************  Send single integer  ***********************C
C**********************************************************************C
      SUBROUTINE BEOPUTINT(IDST,K)
      INCLUDE 'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
      INTEGER IOS
#endif
      INTEGER IDST,K

      if (TCP) then
        call TCPSEND(IDST,K,4,1)
      else
#ifdef BEOMPI
        call MPI_SEND(K,1,MPI_INTEGER,IDST,0,MPI_COMM_WORLD,IOS)
#endif
      endif
      END
C**********************************************************************C
C***********************  Receive single integer  *********************C
C**********************************************************************C
      SUBROUTINE BEOGETINT(ISRC,K)
      INCLUDE 'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
      INTEGER STAT(MPI_STATUS_SIZE),IOS
#endif
      INTEGER ISRC,K

      if (TCP) then
        call TCPRECV(ISRC,K,4,1)
      else
#ifdef BEOMPI
        call MPI_RECV(K,1,MPI_INTEGER,ISRC,0,MPI_COMM_WORLD,STAT,IOS)
#endif
      endif
      END

C**********************************************************************C
C***********************  Send array of integers  *********************C
C**********************************************************************C
      SUBROUTINE BEOPUTINTS(IDST,K,N)
      INCLUDE 'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
      INTEGER IOS
#endif
      INTEGER IDST,K(N),N

      if (N.le.0) then
C       do nothing
      else if (TCP) then
        call TCPSEND(IDST,K,4,N)
      else
#ifdef BEOMPI
        call MPI_SEND(K,N,MPI_INTEGER,IDST,0,MPI_COMM_WORLD,IOS)
#endif
      endif
      END
C**********************************************************************C
C**********************  Receive array of integers  *******************C
C**********************************************************************C
      SUBROUTINE BEOGETINTS(ISRC,K,N)
      INCLUDE 'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
      INTEGER STAT(MPI_STATUS_SIZE),IOS
#endif
      INTEGER ISRC,K(N),N

      if (N.le.0) then
C       do nothing
      else if (TCP) then
        call TCPRECV(ISRC,K,4,N)
      else
#ifdef BEOMPI
        call MPI_RECV(K,N,MPI_INTEGER,ISRC,0,MPI_COMM_WORLD,STAT,IOS)
#endif
      endif
      END

C**********************************************************************C
C***********************  Send array of doubles  **********************C
C**********************************************************************C
      SUBROUTINE BEOPUTDOUBLES(IDST,X,N)
      INCLUDE 'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
      INTEGER IOS
#endif
      INTEGER          IDST,N
      DOUBLE PRECISION X(N)

      if (N.le.0) then
C       do nothing
      else if (TCP) then
        call TCPSEND(IDST,X,8,N)
      else
#ifdef BEOMPI
        call MPI_SEND(X,N,MPI_DOUBLE_PRECISION,IDST,0,
     |                MPI_COMM_WORLD,IOS)
#endif
      endif
      END
C**********************************************************************C
C**********************  Receive array of doubles  ********************C
C**********************************************************************C
      SUBROUTINE BEOGETDOUBLES(ISRC,X,N)
      INCLUDE 'beopest.inc'
#ifdef BEOMPI
      INCLUDE 'mpif.h'
      INTEGER STAT(MPI_STATUS_SIZE),IOS
#endif
      INTEGER          ISRC,N
      DOUBLE PRECISION X(N)

      if (N.le.0) then
C       do nothing
      else if (TCP) then
        call TCPRECV(ISRC,X,8,N)
      else
#ifdef BEOMPI
        call MPI_RECV(X,N,MPI_DOUBLE_PRECISION,ISRC,0,
     |                MPI_COMM_WORLD,STAT,IOS)
#endif
      endif
      END

