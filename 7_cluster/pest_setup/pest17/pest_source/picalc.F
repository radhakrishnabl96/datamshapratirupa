       program PICALC

C --   program PICLAC calculates linear prior information.

       implicit none
       integer                 npar,nprior,irow,icol,ierr,i
       integer                 irowold
       integer, allocatable :: itrans(:)
       character*12            aversion
       double precision        dtemp,sum
       double precision, allocatable:: pval(:)
       character*200           infile,outfile,comline

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(' PICALC Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

       call pgetcl(comline)
       if(comline.eq.' ')then
         infile='picalc.in'
       else
         comline=adjustl(comline)
         infile='picalc.'//trim(comline)//'.in'
       end if
       open(unit=10,file=infile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(6,111) trim(infile)
111      format(/,' *** Cannot open PICALC input file ',a,
     +   ' ***',/)
         stop
       end if
       if(comline.eq.' ')then
         outfile='picalc.out'
       else
         outfile='picalc.'//trim(comline)//'.out'
       end if
       open(unit=20,file=outfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(6,112) trim(outfile)
112      format(/,' *** Cannot write to PICALC output file ',a,
     +   ' ***',/)
       end if
       read(10,*,err=9000,end=9100)
       read(10,*,err=9000,end=9100) npar,nprior
       allocate(pval(npar),itrans(npar),stat=ierr)
       if(ierr.ne.0)then
         write(6,140)
140      format(/,' *** Cannot allocate sufficient memory ',
     +   'to execute PICALC ***',/)
         stop
       end if
       read(10,*,err=9000,end=9100)
       do i=1,npar
         read(10,*,err=9000,end=9100) pval(i),itrans(i)
       end do
       read(10,*)
       irowold=0
       sum=0.0d0
       do
         read(10,*,err=9000,end=146)irow,icol,dtemp
         if((irow.le.0).or.(irow.gt.nprior).or.
     +      (icol.le.0).or.(icol.gt.npar))then
            write(6,145)
145         format(/,' *** Column or row number out of range in ',
     +      'PICALC input file ***',/)
            go to 9999
         end if
         if(irow.ne.irowold)then
           if(irowold.ne.0)then
             write(20,160,err=9300) sum
160          format(1x,1pg23.16)
           end if
           sum=0.0d0
           irowold=irow
         end if
         if(itrans(icol).eq.0)then
           sum=sum+dtemp*pval(icol)
         else
           sum=sum+dtemp*log10(pval(icol))
         end if
       end do
146    continue
       write(20,160,err=9300) sum
       close(unit=10)
       write(6,150) trim(infile)
150    format(' - file ',a,' read ok.')
       close(unit=20)
       write(6,170) trim(outfile)
170    format(' - file ',a,' written ok.')
       go to 9999

9000   write(6,9010) trim(infile)
9010   format(/,' *** Error encountered in reading file ',a,' ***',/)
       go to 9999
9100   write(6,9110) trim(infile)
9110   format(/,' *** Premature end encountered to file ',a,' ***',/)
       go to 9999
9300   write(6,9310) trim(outfile)
9310   format(/,' *** Cannot write to file ',a,' ***',/)       

9999   continue
       deallocate(pval,itrans,stat=ierr)

       end
