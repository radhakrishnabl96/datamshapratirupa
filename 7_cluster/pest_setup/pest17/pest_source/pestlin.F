       program PESTLIN

C -- Program PESTLIN creates a calibration exericse for a linear model from that
C    pertaining to an existing, nonlinear model.

       implicit none
       integer    :: i,pestmode,npar,nobs,ntplfile,ninsfile,noptmax,
     +               ipar,iobs,ierr,nb,itemp,j,npargp,nprior,
     +               nobsgp,nxrow,nespar,nespar1,nxrow1,icount,ifail,
     +               ideriv,iipar
       integer    :: lw(10),rw(10)
       integer, allocatable :: itrans(:)
       double precision     :: dtemp1,dtemp2,dtemp
       double precision, allocatable :: initpval(:),initoval(:)
       character*4          :: aext
       character*10         :: precis,point,atrans
       character*12         :: aapar
       character*12         :: aversion
       character*15         :: atemp1,atemp2,atemp
       character*20         :: aaobs
       character*200        :: casename,basename
       character*200        :: pestinfile,apestinfile
       character*200        :: pestoutfile,apestoutfile
       character*200        :: recfile,arecfile
       character*200        :: jcofile,ajcofile
       character*200        :: geninfile,ageninfile
       character*200        :: genoutfile,agenoutfile
       character*200        :: genderivfile,agenderivfile
       character*200        :: insfile,ainsfile
       character*200        :: tplfile,atplfile
       character*300        :: cline
       character*500        :: amessage
       character*12, allocatable :: apar(:)
       character*20, allocatable :: aobs(:)


#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' PESTLIN version ',a,'. Watermark Numerical ',
     + 'Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

C -- The command line is parsed.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       pestinfile=cline(lw(1):rw(1))
       call remchar(pestinfile,char(211))
       pestoutfile=cline(lw(2):rw(2))
       call remchar(pestoutfile,char(211))
       basename=cline(lw(3):rw(3))
       call remchar(basename,char(211))
#ifndef UNIX
       call lowcas(pestinfile)
       call lowcas(pestoutfile)
       call lowcas(basename)
#endif

C -- Is the "/d" switch set?

      ideriv=0
      cline=cline(rw(3)+1:)
      cline=adjustl(cline)
      if(cline(1:3).eq.'/d ') then
        ideriv=1
        cline(1:3)=' '
      end if
      if(cline.ne.' ') go to 8900

C -- Further analysis is done on the input and output PEST filenames.

       nb=len_trim(pestinfile)
       nb=nb-3
       if(nb.lt.1)nb=1
       aext=pestinfile(nb:)
       call lowcas(aext)
       if(aext.eq.'.pst')then
         if(nb.eq.1) go to 8900
         casename=pestinfile(1:nb-1)
       else
         casename=pestinfile
         pestinfile=trim(casename)//'.pst'
       end if

       nb=len_trim(pestoutfile)
       nb=nb-3
       if(nb.lt.1)nb=1
       aext=pestoutfile(nb:)
       call lowcas(aext)
       if(aext.ne.'.pst')then
         pestoutfile=trim(pestoutfile)//'.pst'
       end if

       nb=len_trim(basename)
       itemp=itemp-3
       if(itemp.lt.1)itemp=1
       do j=nb,itemp,-1
         if(basename(j:j).eq.'.')then
           if((basename(j+1:j+1).ne.char(47)).and.
     +        (basename(j+1:j+1).ne.char(92)))then
             write(amessage,20)
20           format(' Linear model basename must not ',
     +       'have filename extension.')
             go to 9890
           end if
         end if
       end do

C -- Files are named.

       call addquote(pestinfile,apestinfile)
       call addquote(pestoutfile,apestoutfile)

       recfile=trim(casename)//'.rec'
       call addquote(recfile,arecfile)
       jcofile=trim(casename)//'.jco'
       call addquote(jcofile,ajcofile)

       geninfile=trim(basename)//'.in'
       call addquote(geninfile,ageninfile)
       genoutfile=trim(basename)//'.out'
       call addquote(genoutfile,agenoutfile)
       genderivfile=trim(basename)//'.drv'
       call addquote(genderivfile,agenderivfile)
       insfile=trim(basename)//'.ins'
       call addquote(insfile,ainsfile)
       tplfile=trim(basename)//'.tpl'
       call addquote(tplfile,atplfile)

C -- The old and new PEST control files are opened.

       open(unit=10,file=pestinfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,30) trim(apestinfile)
30       format(' Cannot open PEST control file ',a,'.')
         go to 9890
       end if

       open(unit=20,file=pestoutfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,40) trim(apestoutfile)
40       format(' Cannot open file ',a,' for output.')
         go to 9890
       end if

C -- The existing PEST control file is opened and partly read.

       read(10,*,err=9000,end=9000) atemp1
       call lowcas(atemp1)
       if(atemp1.ne.'pcf') go to 9000
       read(10,*,err=9000,end=9000)
       read(10,*,err=9000,end=9000) atemp1,atemp2
       call lowcas(atemp2)
       if(atemp2.eq.'estimation')then
         pestmode=1
       else if(atemp2.eq.'prediction')then
         pestmode=2
       else if (atemp2(1:5).eq.'regul')then
         pestmode=3
       else
         write(amessage,165) trim(apestinfile)
165      format(' Unknown value for PESTMODE variable on line 3 of ',
     +   'PEST control file ',a,'.')
         go to 9890
       end if
       read(10,*,err=9000,end=9000) npar,nobs,npargp,nprior,nobsgp
       read(10,*,err=9000,end=9000) ntplfile,ninsfile,precis,point
       do i=1,3
         read(10,*,err=9000,end=9000)
       end do
       read(10,*,err=9000,end=9000) noptmax

C -- Some memory is allocated before proceding further.

       nxrow=nobs+nprior
       allocate(apar(npar),aobs(nobs),initpval(npar),itrans(npar),
     + initoval(nobs))
       do
         read(10,'(a)',err=9000,end=9000) cline
         call lowcas(cline)
         cline=adjustl(cline)
         if((cline(1:1).eq.'*').and.
     +       (index(cline,'parameter').ne.0).and.
     +       (index(cline,'data').ne.0)) exit
       end do
       do ipar=1,npar
         read(10,*,err=9000,end=9000) apar(ipar),atrans,atemp,
     +   initpval(ipar)
         call lowcas(atrans)
         call lowcas(apar(ipar))
         if(atrans.eq.'log')then
           itrans(ipar)=1
         else if(atrans.eq.'none')then
           itrans(ipar)=0
         else if(atrans.eq.'fixed')then
           itrans(ipar)=-1000000
         else if(atrans.eq.'tied')then
           itrans(ipar)=-1
         else
           go to 9000
         end if
       end do

       do
         read(10,'(a)',err=9000,end=9000) cline
         call lowcas(cline)
         cline=adjustl(cline)
         if((cline(1:1).eq.'*').and.
     +      (index(cline,'observation').ne.0).and.
     +      (index(cline,'data').ne.0)) exit
       end do
       do iobs=1,nobs
         read(10,*,err=9000,end=9000) aobs(iobs),dtemp1,dtemp2,atemp
         call lowcas(aobs(iobs))
       end do
       call lowcas(atemp)
       if(pestmode.eq.2)then
         if(atemp.ne.'predict')then
           write(amessage,175) trim(apestinfile)
175        format(' If PEST is run in "predictive analysis" mode,',
     +     ' LINPEST requires that the prediction be the last item ',
     +     'in the "observation data" section of PEST control file ',
     +     a,'.')
           go to 9890
         end if
       end if

       write(6,*)
       write(6,250) trim(apestinfile)

C -- The run record file is opened.

       open(unit=11,file=recfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,180) trim(arecfile),trim(apestinfile)
180      format(' Cannot open run record file ',a,' corresponding ',
     +   'to PEST control file ',a,'.')
         go to 9890
       end if

C -- First the values of adjustable parameters are read from this file.

       nespar=0
       do ipar=1,npar
         if(itrans(ipar).lt.0)cycle
         nespar=nespar+1
       end do

       do
         read(11,'(a)',err=9200,end=9200) cline
         if(index(cline,' RESULTS ').ne.0) exit
       end do
       do
         read(11,'(a)',err=9220,end=9220) cline
         if(index(cline,'arameters ----->').ne.0)exit
       end do
       do i=1,2
         read(11,*,err=9220,end=9220)
       end do
       iipar=1
       icount=0
       do
184      continue
         read(11,'(a)',err=9220,end=9220) cline
         if(index(cline,'lower limit').ne.0) go to 184
         if(cline.eq.' ') exit
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9220
         aapar=cline(lw(1):rw(1))
         call lowcas(aapar)
         call whichone(ifail,npar,iipar,apar,aapar)
         if(ifail.ne.0)then
           write(amessage,185) trim(arecfile),trim(apestinfile)
185        format(' Error encountered when reading parameter values ',
     +     'from OPTMIISATION RESULTS section of run record file ',a,
     +     '; this file does not appear to be compatible with ',
     +     'PEST control file ',a,'.')
           go to 9890
         end if
         call drealread(ifail,cline(lw(2):rw(2)),initpval(iipar))
         if(ifail.ne.0) go to 9220
         if(itrans(iipar).ge.0) icount=icount+1
       end do
       if(icount.ne.nespar)then
         write(amessage,185) trim(arecfile),trim(apestinfile)
         go to 9890
       end if

C -- Now observation values are read.

       do
         read(11,'(a)',err=9270,end=9270) cline
         if(index(cline,'Observations ----->').ne.0)exit
       end do
       do i=1,3
         read(11,*,err=9270,end=9270)
       end do
       if(pestmode.ne.2)then
         do iobs=1,nobs
           read(11,*,err=9270,end=9270) aaobs,dtemp,initoval(iobs)
           call lowcas(aaobs)
           if(aaobs.ne.aobs(iobs)) go to 9320
         end do
       else
         do iobs=1,nobs-1
           read(11,*,err=9270,end=9270) aaobs,dtemp,initoval(iobs)
           call lowcas(aaobs)
           if(aaobs.ne.aobs(iobs)) go to 9320
         end do
         do
           read(11,'(a)',err=9250,end=9250) cline
           if(index(cline,'Prediction ----->').ne.0)exit
         end do
         read(11,*,err=9250,end=9250)
         read(11,*,err=9250,end=9250)
         read(11,*,err=9250,end=9250) aaobs,initoval(nobs)
         call lowcas(aaobs)
         if(aaobs.ne.aobs(nobs))then
           write(amessage,186) trim(arecfile),trim(apestinfile)
186        format(' Prediction name different in OPTIMISATION ',
     +     'RESULTS section of run record file ',a,' from that in ',
     +     'PEST control file ',a,'.')
           go to 9890
         end if
       end if

       close(unit=11)
       write(6,250) trim(arecfile)
250    format(' - file ',a,' read ok.')

C -- Is the JCO file present? If so, check compatibility.

       call open_unformatted_file(ifail,15,'read',jcofile,amessage)
       if(ifail.ne.0) go to 9890
       read(15,err=9600,end=9600) nespar1,nxrow1
       if(nespar1.gt.0)then
         write(amessage,205) trim(ajcofile)
205      format(' JCO file ',a,' uses old PEST JCO file format. Use ',
     +   'JCOTRANS utility to convert to new format.')
         go to 9890
       end if
       nespar1=-nespar1
       nxrow1=-nxrow1
       if(nespar.ne.nespar1) go to 9500
       if(nxrow1.lt.nobs) go to 9500
       read(15,err=9600,end=9600)icount
       do i=1,icount
         read(15,err=9600,end=9600) j,dtemp
       end do
       do ipar=1,npar
         if(itrans(ipar).lt.0) cycle
         read(15,err=9600,end=9600) aapar
         call lowcas(aapar)
         if(apar(ipar).ne.aapar) go to 9500
       end do
       do iobs=1,nobs
         read(15,err=9600,end=9600) aaobs
         call lowcas(aaobs)
         if(aobs(iobs).ne.aaobs) go to 9500
       end do
       close(unit=15)
       write(6,250) trim(ajcofile)

C -- The new PEST control file is written.

       rewind(unit=10,iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,310) trim(apestinfile)
310      format(' Cannot rewind PEST control file ',a,'.')
         go to 9890
       end if
       do i=1,3
         read(10,'(a)',end=9700,err=9700) cline
         write(20,'(a)',err=9750) trim(cline)
       end do
       read(10,'(a)',end=9700,err=9700) cline
       write(20,320,err=9750) nespar,nobs,npargp,nprior,nobsgp
320    format(5i6)
       read(10,'(a)',end=9700,err=9700) cline
       if(ideriv.eq.1)then
         write(20,350,err=9750) 1,1,trim(precis),trim(point)
  350    format(1x,i2,2x,i2,3x,a,3x,a,' 1 1 0')
       else
         write(20,351,err=9750) 1,1,trim(precis),trim(point)
  351    format(1x,i2,2x,i2,3x,a,3x,a)
       end if
       do i=1,3
         read(10,'(a)',end=9700,err=9700) cline
         write(20,'(a)',err=9750) trim(cline)
       end do
       read(10,'(a)',end=9700,err=9700) cline
       cline=adjustl(cline)
       call linspl(ifail,1,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       cline=' 50 '//cline(rw(1)+1:)
       write(20,'(a)',err=9750) trim(cline)
       do
         read(10,'(a)',err=9700,end=9700) cline
         write(20,'(a)',err=9750) trim(cline)
         call lowcas(cline)
         cline=adjustl(cline)
         if((cline(1:1).eq.'*').and.
     +      (index(cline,'parameter').ne.0).and.
     +      (index(cline,'data').ne.0)) exit
       end do
       do ipar=1,npar
         read(10,'(a)',err=9700,end=9700) cline
         if(itrans(ipar).ge.0)then
           call linspl(ifail,9,lw,rw,cline)
           if(ifail.ne.0) go to 9000
           if(ideriv.eq.1)then
             write(20,360,err=9750) cline(1:rw(7))
360          format(1x,a,1x,'1.0  0.0  0')
           else
             write(20,361,err=9750) cline(1:rw(7))
361          format(1x,a,1x,'1.0  0.0')
           end if
         end if
       end do
       do
         read(10,'(a)',err=9000,end=9000) cline
         call lowcas(cline)
         if((cline(1:1).eq.'*').and.
     +      (index(cline,'observation').ne.0).and.
     +      (index(cline,'group').ne.0)) exit
       end do
       write(20,'(a)',err=9750) trim(cline)
       do
         read(10,'(a)',err=9300,end=9300) cline
         cline=adjustl(cline)
         if(cline(1:1).eq.'*')then
           call lowcas(cline)
           if(index(cline,'model command').ne.0)then
             if(ideriv.eq.1)then
               write(20,362,err=9750)
362            format('* derivatives command line')
               write(20,'(a)',err=9750)
     +         'genlin '//trim(ageninfile)//' '//trim(agenoutfile)//
     +         ' '//trim(agenderivfile)//' > nul'
               write(20,'(a)',err=9750) trim(agenderivfile)
             end if
             write(20,'(a)',err=9750) trim(cline)
             write(20,'(a)',err=9750)
     +       'genlin '//trim(ageninfile)//' '//trim(agenoutfile)//
     +       ' > nul'
             write(20,'(a)',err=9750) '* model input/output'
             write(20,370,err=9750) trim(atplfile),trim(ageninfile)
370          format(1x,a,1x,a)
             write(20,370,err=9750) trim(ainsfile),trim(agenoutfile)
             do
               read(10,'(a)',err=9000,end=9000) cline
               cline=adjustl(cline)
               call lowcas(cline)
               if((cline(1:1).eq.'*').and.
     +            (index(cline,'model').ne.0).and.
     +            (index(cline,'input').ne.0).and.
     +            (index(cline,'output').ne.0)) go to 400
             end do
           else if((index(cline,'derivat').ne.0).and.
     +             (index(cline,'command').ne.0))then
             read(10,'(a)',err=9000,end=9000) cline
             read(10,'(a)',err=9000,end=9000) cline
             go to 399
           end if
         end if
         write(20,'(a)') trim (cline)
399      continue
       end do
400    continue
       do i=1,ntplfile+ninsfile
         read(10,'(a)',err=9000,end=9000) cline
       end do
       do
         read(10,'(a)',err=9000,end=440) cline
         write(20,'(a)',err=9750) trim(cline)
       end do
440    close(unit=20)
       write(6,450) trim(apestoutfile)
450    format(' - file ',a,' written ok.')

C -- The GENLIN instruction file is written.  xxxx

       open(unit=20,file=insfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,455) trim(ainsfile)
455      format(' Cannot open file ',a,' for output.')
         go to 9890
       end if
       write(20,'(a)') 'pif #'
       write(20,456) trim(aobs(1))
456    format('l2 [',a,']1:17')
       if(nobs.gt.2)then
         do iobs=2,nobs
           write(20,460) trim(aobs(iobs))
460        format('l1 [',a,']1:17')
         end do
       end if
       close(unit=20)
       write(6,450) trim(ainsfile)

C -- The GENLIN template file is written.

       open(unit=20,file=tplfile,action='write',iostat=ierr)
       if(ierr.ne.0) then
         write(amessage,455) trim(atplfile)
         go to 9890
       end if
       write(20,'(a)') 'ptf #'
       write(20,'(a)') '* dimensions'
       write(20,461) nespar,nobs
461    format(2i6)
       write(20,'(a)') '* parameters'
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           if(itrans(ipar).eq.1)then
             atrans='log'
           else
             atrans='none'
           end if
           write(20,371) trim(apar(ipar)),trim(atrans),trim(apar(ipar)),
     +     initpval(ipar)
371        format(1x,a,t15,a,t25,'#',a12,'#',2x,1pg14.7)
         end if
       end do
       write(20,'(a)') ' * observations'
       do iobs=1,nobs
         write(20,380) trim(aobs(iobs)),initoval(iobs)
380      format(1x,a,t23,1pg14.7)
       end do
       write(20,'(a)') '* sensitivities'
       write(20,'(a)') trim(ajcofile)
       close(unit=20)
       write(6,450) trim(atplfile)

       go to 9900


8900   continue
       write(6,8910)
8910   format(/,' PESTLIN is run using the command:',/)
       write(6,8920)
8920   format('     pestlin pestinfile pestoutfile linbasename [/d]',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     pestinfile  is the filename base of an input ',
     + 'PEST dataset,')
       write(6,8950)
8950   format('     pestoutfile is the filename base of an output ',
     + 'PEST dataset,')
       write(6,8960)
8960   format('     linbasename is the filename base of a GENLIN ',
     + 'linear model dataset, and')
       write(6,8970)
8970   format('     /d (optional) activates external derivatives ',
     + 'functionality.')

       go to 9900


9000   write(amessage,9010) trim(apestinfile)
9010   format(' Error reading PEST control file ',a,'. Check this ',
     + 'file with PESTCHEK.')
       go to 9890

9200   write(amessage,9210) trim(arecfile)
9210   format(' Cannot find OPTMISATION RESULTS section in PEST run ',
     + 'record file ',a,'.')
       go to 9890

9220   write(amessage,9230) trim(arecfile)
9230   format(' Cannot read parameter values from OPTMISATION ',
     + 'RESULTS section of PEST run record file ',a,'.')
       go to 9890

9250   write(amessage,9260) trim(arecfile)
9260   format(' Cannot read calculated prediction value in ',
     + 'OPTMISATION RESULTS section of PEST run record file ',a,'.')
       go to 9890


9270   write(amessage,9280) trim(arecfile)
9280   format(' Cannot read calculated observation values from ',
     + 'OPTMISATION RESULTS section of PEST run record file ',a,'.')
       go to 9890


9300   write(amessage,9310) trim(apestinfile)
9310   format(' Cannot fine "model command line" section of PEST ',
     + 'control file ',a,'.')
       go to 9890


9320   write(amessage,9330) trim(arecfile),trim(apestinfile)
9330   format(' Error encountered when reading observation values ',
     + 'from OPTMISATION RESULTS section of run record file ',a,
     + '; this file does not appear to be compatible with ',
     + 'PEST control file ',a,'.')
       go to 9890

9500   write(amessage,9510) trim(ajcofile),trim(apestinfile)
9510   format(' Jacobian matrix file ',a,' does not appear to ',
     + 'be compatible with PEST control file ',a,'.')
       go to 9890


9600   write(amessage,9610) trim(ajcofile)
9610   format(' Error reading binary Jacobian matrix file ',a,'.')
       go to 9890

9700   write(amessage,9710) trim(apestinfile)
9710   format(' Error re-reading PEST control file ',a,'.')
       go to 9890

9750   write(amessage,9760) trim(apestoutfile)
9760   format(' Error writing to new PEST control file ',a,'.')
       go to 9890



9890   continue
       call writmess(6,amessage)

9900   deallocate(itrans,initpval,initoval,apar,aobs,stat=ierr)


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


        subroutine drealread(ifail,cline,rtemp)

C -- Subroutine DREALREAD reads a double precision number from a string.

        integer ifail
        double precision rtemp
        character*8 afmt
        character*(*) cline

        ifail=0
        afmt='(f   .0)'
        write(afmt(3:5),'(i3)') len_trim(cline)
        read(cline,afmt,err=100) rtemp
        return

100     ifail=1
        return
        end


        subroutine whichone(ifail,npar,ipar,apar,tpar)

C -- Subroutine whichone locates a string in an array. Note that both the
C    search string and the existing array of strings are assumed to be
C    in the same case.

        integer npar,ipar,i
        integer ifail
        character*(*) tpar
        character*(*) apar(npar)

        ifail=0
        if((ipar.lt.1).or.(ipar.gt.npar)) ipar=1
        if(tpar.eq.apar(ipar)) return
        if(ipar.ne.npar)then
          do 20 i=ipar+1,npar
          if(tpar.eq.apar(i))then
            ipar=i
            return
          end if
20        continue
        end if
        if(ipar.ne.1)then
          do 40 i=ipar-1,1,-1
          if(tpar.eq.apar(i)) then
            ipar=i
            return
          end if
40        continue
        end if
        ifail=1
        return
        end



