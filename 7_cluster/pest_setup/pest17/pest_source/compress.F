       subroutine store_value(ifail,ndim,vector,ivector,rval,irow,icol)

C -- Program stor_value places a number into a compressed matrix. Note that it also
C    maintains sorting order. It can stor a number into a location that is already
C    defined, or, add the location to the vector in correct order.

C    Arguments are as follows:-
C      ifail       : return as 1 if no more room left in vector
C      ndim        : number of used entries of the vector
C      vector      : real vector which stores matrix in compressed form
C      ivector     : integer vector containing element number of matrix
C      rval        : number to be stored
C      irow,icol   : row and column of matrix

       use pestdata, only: maxcompdim,ifound_c,nxrow,derzerolim,
     +                     nobs,pi_par,ixc_dim
       implicit none

       integer irow,icol,ifail

#ifdef INTEL
       integer (8) :: ndim
       integer (8) :: inum
       integer (8) :: i,ielem,itemp8
       integer (8) :: ivector(ixc_dim)
#else
       integer     :: ndim
       integer     :: inum
       integer     :: i,ielem,itemp8
       integer     :: ivector(ixc_dim)
#endif
       double precision rval

#ifdef SINGLE_XC
       real             vector(maxcompdim)
#else
       double precision vector(maxcompdim)
#endif

C -- The matrix index number is evaluated from its row and column.

       ifail=0

       if(ixc_dim.eq.2)then
         if(irow.le.nobs)then
           itemp8=icol-1
           itemp8=itemp8*nobs+irow+pi_par(icol)
           vector(itemp8)=rval
           go to 9999
         else
           write(6,*) ' ILLEGAL CALL TO STORE_VALUE'
           stop
         end if
       end if

       itemp8=(icol-1)
       inum=itemp8*nxrow+irow
       if(ndim.eq.0)then
         ndim=1
         if(ndim+1.gt.maxcompdim) go to 9000
         vector(1)=rval
         ivector(1)=inum
         ifound_c=1
         return
       end if
       call search_vector(ielem,ndim,ivector,inum)
       if(ielem.gt.0)then
         vector(ielem)=rval
         ifound_c=ielem
       else
         if(abs(rval).gt.derzerolim) then
           ielem=-ielem
           ndim=ndim+1
           if(ndim+1.gt.maxcompdim) go to 9000
           if(ielem.eq.ndim-1)then
             vector(ndim)=rval
             ivector(ndim)=inum
             ifound_c=ndim
           else
             do i=ndim,ielem+2,-1
               vector(i)=vector(i-1)
               ivector(i)=ivector(i-1)
             end do
             vector(ielem+1)=rval
             ivector(ielem+1)=inum
             ifound_c=ielem+1
           end if
         end if
       end if
       return

9000   ifail=1
9999   continue
       return
#ifdef PESTMOD
       end subroutine store_value
#else
       end
#endif


       subroutine search_vector(ielem,ndim,ivector,inum)

C -- Program search_vector searches a vector for a number, assuming that entries
C    are in increasing order.

       use pestdata, only: istart_c
       implicit none

#ifdef INTEL
       integer (8) :: ndim
       integer (8) :: inum,ielem,itemp,lbound,ubound,irove,inum1,inum2
       integer (8) :: ivector(ndim)
#else
       integer     :: ndim
       integer     :: inum,ielem,itemp,lbound,ubound,irove,inum1,inum2
       integer     :: ivector(ndim)
#endif

       if(istart_c.gt.ndim)istart_c=ndim
       if(istart_c.ne.0)then
         inum1=ivector(istart_c)
         if(inum.eq.inum1)then
           ielem=istart_c
           return
         else if(istart_c.lt.ndim)then
           itemp=istart_c+1
           inum2=ivector(itemp)
           if(inum.eq.inum2)then
             ielem=itemp
             return
           else
             if((inum.gt.inum1).and.(inum.lt.inum2))then
               ielem=-istart_c
               return
             end if
           end if
         end if
       end if
       itemp=ivector(ndim)
       if(inum.eq.itemp)then
         ielem=ndim
         return
       else if(inum.gt.itemp)then
         ielem=-ndim
         return
       end if
       itemp=ivector(1)
       if(inum.eq.itemp)then
         ielem=1
         return
       else if(inum.lt.itemp)then
         ielem=0
         return
       end if

C -- We've tried all the easy options; now lets do the search.

       irove=istart_c
       if(irove.lt.2)irove=2
       if(irove.gt.ndim) irove=ndim
       lbound=1
       ubound=ndim
100    continue
       itemp=ivector(irove)
       if(inum.eq.itemp)then
         ielem=irove
         return
       else if(inum.lt.itemp)then
         ubound=irove
       else if(inum.gt.itemp)then
         lbound=irove
       end if
       if(ubound.eq.lbound+1)then
         ielem=-lbound
         return
       end if
       irove=(lbound+ubound)/2
       go to 100

#ifdef PESTMOD
       end subroutine search_vector
#else
       end
#endif


       subroutine get_value(ndim,vector,ivector,rval,irow,icol)

C -- Program get_value retreives a number from a compressed matrix.

C    Arguments are as follows:-
C      ndim        : number of elements of vector used so far
C      vector      : real vector which stores matrix in compressed form
C      ivector     : integer vector containing element number of matrix
C      rval        : number retreived
C      irow,icol   : row and column of matrix

       use pestdata, only: ifound_c,nxrow,pi_par,
     +                     nobs,ixc_dim,pi_par,
     +                     ixpr,ixpc,xpval,npr_nonzero
       implicit none

       integer icol,irow
       integer jrow

#ifdef INTEL
       integer (8) :: ndim
       integer (8) :: inum,ielem
       integer (8) :: ivector(ixc_dim)
       integer (8) :: itemp8
#else
       integer     :: ndim
       integer     :: inum,ielem
       integer     :: ivector(ixc_dim)
       integer     :: itemp8
#endif
       double precision rval
#ifdef SINGLE_XC
       real             vector(ndim)
#else
       double precision vector(ndim)
#endif

C -- The matrix index number is evaluated from its row and column.

       if(ixc_dim.eq.2)then
         if(irow.le.nobs)then
           itemp8=icol-1
           ielem=nobs*itemp8+pi_par(icol)+irow
           ifound_c=ielem
           rval=vector(ielem)
           go to 10
         else
           rval=0.0d0
           itemp8=pi_par(icol)+1
5          continue
           if(itemp8.gt.npr_nonzero) go to 10
           if(ixpc(itemp8).gt.icol) go to 10
           jrow=ixpr(itemp8)
           if(jrow.le.irow)then
             itemp8=itemp8+1
             go to 5
           else if(jrow.eq.irow)then
             rval=xpval(itemp8)
             go to 10
           else
             go to 10
           end if
         end if
       end if

       itemp8=icol-1
       inum=itemp8*nxrow+irow
       call search_vector(ielem,ndim,ivector,inum)
       if(ielem.gt.0)then
         rval=vector(ielem)
         ifound_c=ielem
       else
         rval=0.0d0
         ifound_c=-ielem
       end if

10     continue
       return
#ifdef PESTMOD
       end subroutine get_value
#else
       end
#endif



       subroutine get_vector(ndim,nrow,vector,ivector,rvec,icol)

C -- Subroutine get_vector retrieves a column of the true matrix from the
C    compressed matrix.

       use pestdata, only: ifound_c,
     +                     nxrow,nobs,nxrow,ixc_dim,pi_par,
     +                     ixpc,ixpr,npr_nonzero,xpval

       implicit none

       integer icol,nrow,irow

#ifdef INTEL
       integer (8) :: ndim
       integer (8) :: inum,ielem,inum1,inum2,i,ielemnext,inumnext,in,j
       integer (8) :: ivector(ixc_dim)
       integer (8) :: itemp8,jtemp8
#else
       integer     :: ndim
       integer     :: inum,ielem,inum1,inum2,i,ielemnext,inumnext,in,j
       integer     :: ivector(ixc_dim)
       integer     :: itemp8,jtemp8
#endif
       double precision rvec(nrow)

#ifdef SINGLE_XC
       real             vector(ndim)
#else
       double precision vector(ndim)
#endif

       if(ixc_dim.eq.2)then
         itemp8=(icol-1)*nobs+pi_par(icol)
         irow=0
         do jtemp8=itemp8+1,itemp8+min(nrow,nobs)
           irow=irow+1
           rvec(irow)=vector(jtemp8)
         end do
         if(nrow.gt.nobs)then
           do irow=nobs+1,nrow
             rvec(irow)=0.0d0
           end do
           itemp8=pi_par(icol)+1
5          continue
           if(itemp8.gt.npr_nonzero) go to 6
           if(ixpc(itemp8).gt.icol) go to 6
           irow=ixpr(itemp8)
           if(irow.le.nrow)then
             rvec(irow)=xpval(itemp8)
             itemp8=itemp8+1
             go to 5
           else
             go to 6
           end if
         end if
6        continue
         go to 9999
       end if

       itemp8=icol-1
       inum1=itemp8*nxrow+1
       call search_vector(ielem,ndim,ivector,inum1)
       itemp8=icol-1
       inum2=itemp8*nxrow+nrow
       if(ielem.gt.0)then
         rvec(1)=vector(ielem)
       else
         rvec(1)=0.0d0
       end if
       inum=inum1+1
       ielem=abs(ielem)
       ielemnext=ielem+1
       i=1
10     continue
       if(ielemnext.gt.ndim)then
         do j=1+1,nrow
           rvec(j)=0.0d0
         end do
         ifound_c=ndim
         go to 500
       end if
       inumnext=ivector(ielemnext)
       if(inumnext.eq.inum)then
         i=i+1
         rvec(i)=vector(ielemnext)
         inum=inum+1
         if(inum.gt.inum2)then
           ifound_c=ielemnext
           go to 500
         end if
         ielem=ielemnext
         ielemnext=ielemnext+1
         go to 10
       end if
       do in=inum,min(inum2,inumnext-1)
         i=i+1
         rvec(i)=0.0d0
       end do
       if(inum2.le.inumnext-1)then
         ifound_c=ielem
         go to 500
       else
         i=i+1
         rvec(i)=vector(ielemnext)
         if(i.eq.nrow)then
           ifound_c=ielemnext
           go to 500
         else
           ielem=ielemnext
           ielemnext=ielemnext+1
           inum=inumnext+1
           go to 10
         end if
       end if

9999   continue
500    return
#ifdef PESTMOD
       end subroutine get_vector
#else
       end
#endif



       subroutine dual_vector_mul(ifail,ndim,startrow,nrow,vector,
     + ivector,rvec,icol1,icol2,rval)

C -- Subroutine dual_vector_mul multiplies one column by another and by an
C    external vector.

       use pestdata, only: maxcompdim,nxrow,nobs,pi_par,
     +                     ixc_dim,xpval,ixpr,ixpc,npr_nonzero
       implicit none

       integer ifail,icol1,icol2,nrow,startrow
       integer irow_1,irow_2

#ifdef INTEL
       integer (8) :: ndim
       integer (8) :: ielem1,ielem2,in1,in2,is1,is2,iflag1,iflag2,irow
       integer (8) :: ivector(ixc_dim)
       integer (8) :: itemp8_1,itemp8_2,jtemp8_1,jtemp8_2,itemp8
#else
       integer     :: ndim
       integer     :: ielem1,ielem2,in1,in2,is1,is2,iflag1,iflag2,irow
       integer     :: ivector(ixc_dim)
       integer     :: itemp8_1,itemp8_2,jtemp8_1,jtemp8_2,itemp8
#endif
       double precision rval
       double precision rvec(nrow)

#ifdef SINGLE_XC
       real             vector(ndim+1)
#else
       double precision vector(ndim+1)
#endif

       ifail=0
       if(ixc_dim.eq.2)then
         itemp8_1=(icol1-1)*nobs+pi_par(icol1)
         itemp8_2=(icol2-1)*nobs+pi_par(icol2)
         rval=0.0d0
         irow=startrow-1
         jtemp8_2=itemp8_2+startrow-1
         if(irow.le.nobs)then
           do jtemp8_1=itemp8_1+startrow,itemp8_1+min(nrow,nobs)
             jtemp8_2=jtemp8_2+1
             irow=irow+1
             rval=rval+vector(jtemp8_1)*vector(jtemp8_2)*rvec(irow)
           end do
         end if
         if(nrow.gt.nobs)then
           itemp8_1=pi_par(icol1)+1
           itemp8_2=pi_par(icol2)+1
5          continue
           if(itemp8_1.gt.npr_nonzero) go to 6
           if(itemp8_2.gt.npr_nonzero) go to 6
           if(ixpc(itemp8_1).gt.icol1) go to 6
           if(ixpc(itemp8_2).gt.icol2) go to 6
           irow_1=ixpr(itemp8_1)
           irow_2=ixpr(itemp8_2)
           if(irow_1.lt.irow_2)then
             itemp8_1=itemp8_1+1
             go to 5
           else if(irow_2.lt.irow_1)then
             itemp8_2=itemp8_2+1
             go to 5
           else
             irow=irow_1
             if((irow.ge.startrow).and.(irow.le.nrow))then
               rval=rval+xpval(itemp8_1)*xpval(itemp8_2)*rvec(irow)
               itemp8_1=itemp8_1+1
               itemp8_2=itemp8_2+1
               go to 5
             else
               if(irow.ge.nrow) go to 6
               go to 5
             end if
           end if
         end if
6        continue
         go to 9999
       end if

       if(ndim.eq.maxcompdim)then
         ifail=1
         return
       end if
       ivector(ndim+1)=0

       itemp8=icol1-1
       in1=itemp8*nxrow+startrow
       call search_vector(ielem1,ndim,ivector,in1)
       itemp8=icol2-1
       in2=itemp8*nxrow+startrow
       call search_vector(ielem2,ndim,ivector,in2)
       if(ielem1.le.0)ielem1=abs(ielem1)+1
       is1=ivector(ielem1)
       if(ielem2.le.0)ielem2=abs(ielem2)+1
       is2=ivector(ielem2)

       in1=in1-1
       in2=in2-1
       rval=0.0d0
       do irow=startrow,nrow
         in1=in1+1
         in2=in2+1
         if(is1.eq.in1)then
           iflag1=1
         else
           iflag1=0
         end if
         if(is2.eq.in2)then
           iflag2=1
         else
           iflag2=0
         end if
         if((iflag1.eq.1).and.(iflag2.eq.1))
     +     rval=rval+vector(ielem1)*vector(ielem2)*rvec(irow)
         if(iflag1.eq.1)then
           ielem1=ielem1+1   ! note that we assume there is one zero element above ncompdim
           is1=ivector(ielem1)
         end if
         if(iflag2.eq.1)then
           ielem2=ielem2+1
           is2=ivector(ielem2)
         end if
       end do

9999   continue
       return

#ifdef PESTMOD
       end subroutine dual_vector_mul
#else
       end
#endif


       subroutine single_vector_mul1(ifail,ndim,startrow,nrow,vector,
     + ivector,icol,wt,vec1,vec2,rval)

C -- Subroutine single_vector_mul1 multiplies one column of the jacobian
C    by wt(i)*(vec1(i)-vec2(i)) where wt(i) may take on zero values.

       use pestdata, only: maxcompdim,nxrow,
     +                     ixpr,ixpc,npr_nonzero,nobs,
     +                     pi_par,xpval,ixc_dim
       implicit none

       integer ifail,nrow,startrow,icol

#ifdef INTEL
       integer (8) :: ndim
       integer (8) :: ielem,in,is,iflag,irow
       integer (8) :: ivector(ixc_dim)
       integer (8) :: itemp8,jtemp8
#else
       integer     :: ndim
       integer     :: ielem,in,is,iflag,irow
       integer     :: ivector(ixc_dim)
       integer     :: itemp8,jtemp8
#endif
       double precision rval
       double precision wt(nrow),vec1(nrow),vec2(nrow)

#ifdef SINGLE_XC
       real             vector(ndim+1)
#else
       double precision vector(ndim+1)
#endif

       ifail=0
       if(ixc_dim.eq.2)then
         itemp8=(icol-1)*nobs+pi_par(icol)
         rval=0.0d0
         irow=startrow-1
         if(irow.le.nobs)then
           do jtemp8=itemp8+startrow,itemp8+min(nrow,nobs)
             irow=irow+1
             rval=rval+vector(jtemp8)*wt(irow)*(vec1(irow)-vec2(irow))
           end do
         end if
         if(nrow.gt.nobs)then
           itemp8=pi_par(icol)+1
5          continue
           if(itemp8.gt.npr_nonzero) go to 6
           if(ixpc(itemp8).gt.icol) go to 6
           irow=ixpr(itemp8)
           if((irow.ge.startrow).and.(irow.le.nrow))then
             rval=rval+xpval(itemp8)*wt(irow)*(vec1(irow)-vec2(irow))
             itemp8=itemp8+1
             go to 5
           else
             go to 6
           end if
         end if
6        continue
         go to 9999
       end if

       if(ndim.eq.maxcompdim)then
         ifail=1
         return
       end if
       ivector(ndim+1)=0

       itemp8=icol-1
       in=itemp8*nxrow+startrow
       call search_vector(ielem,ndim,ivector,in)
       if(ielem.le.0)ielem=abs(ielem)+1
       is=ivector(ielem)

       in=in-1
       rval=0.0d0
       do irow=startrow,nrow
         in=in+1
         if(is.eq.in)then
           iflag=1
         else
           iflag=0
         end if
         if(iflag.eq.1)
     +     rval=rval+vector(ielem)*wt(irow)*(vec1(irow)-vec2(irow))
         if(iflag.eq.1)then
           ielem=ielem+1   ! note that we assume there is one zero element above ncompdim
           is=ivector(ielem)
         end if
       end do

9999   continue
       return

#ifdef PESTMOD
       end subroutine single_vector_mul1
#else
       end
#endif

       subroutine single_vector_mul2(ifail,ndim,startrow,nrow,vector,
     + ivector,icol,wt,rval)

C -- Subroutine single_vector_mul2 multiplies one column of the jacobian
C    matrix squared by wt(i) (which can have many zero values).

       use pestdata, only: maxcompdim,nxrow,
     +                     ixpr,ixpc,npr_nonzero,nobs,
     +                     pi_par,xpval,ixc_dim


       implicit none

       integer ifail,nrow,startrow,icol
#ifdef INTEL
       integer (8) :: ndim
       integer (8) :: ielem,in,is,iflag,irow
       integer (8) :: ivector(ixc_dim)
       integer (8) :: itemp8,jtemp8
#else
       integer     :: ndim
       integer     :: ielem,in,is,iflag,irow
       integer     :: ivector(ixc_dim)
       integer     :: itemp8,jtemp8
#endif
       double precision rval
       double precision wt(nrow)

#ifdef SINGLE_XC
       real             vector(ndim+1)
#else
       double precision vector(ndim+1)
#endif

       ifail=0
       if(ixc_dim.eq.2)then
         itemp8=(icol-1)*nobs+pi_par(icol)
         rval=0.0d0
         irow=startrow-1
         if(irow.le.nobs)then
           do jtemp8=itemp8+startrow,itemp8+min(nrow,nobs)
             irow=irow+1
             rval=rval+vector(jtemp8)*vector(jtemp8)*wt(irow)
           end do
         end if
         if(nrow.gt.nobs)then
           itemp8=pi_par(icol)+1
5          continue
           if(itemp8.gt.npr_nonzero) go to 6
           if(ixpc(itemp8).gt.icol) go to 6
           irow=ixpr(itemp8)
           if((irow.ge.startrow).and.(irow.le.nrow))then
             rval=rval+xpval(itemp8)*xpval(itemp8)*wt(irow)
             itemp8=itemp8+1
             go to 5
           else
             go to 6
           end if
         end if
6        continue
         go to 9999
       end if

       if(ndim.eq.maxcompdim)then
         ifail=1
         return
       end if
       ivector(ndim+1)=0

       itemp8=icol-1
       in=itemp8*nxrow+startrow
       call search_vector(ielem,ndim,ivector,in)
       if(ielem.le.0)ielem=abs(ielem)+1
       is=ivector(ielem)

       in=in-1
       rval=0.0d0
       do irow=startrow,nrow
         in=in+1
         if(is.eq.in)then
           iflag=1
         else
           iflag=0
         end if
         if(iflag.eq.1)
     +     rval=rval+vector(ielem)*vector(ielem)*wt(irow)
         if(iflag.eq.1)then
           ielem=ielem+1   ! note that we assume there is one zero element above ncompdim
           is=ivector(ielem)
         end if
       end do

9999   continue
       return

#ifdef PESTMOD
       end subroutine single_vector_mul2
#else
       end
#endif


       subroutine single_vector_mul3(ifail,ndim,startrow,nrow,vector,
     + ivector,icol,wt,vec,rval)

C -- Subroutine single_vector_mul3 multiplies one column of the jacobian
C    by vec(i)*sqrt(wt(i))where wt(i) may take on zero values.

       use pestdata, only: maxcompdim,nxrow,
     +                     nobs,pi_par,ixc_dim
       implicit none

       integer ifail,nrow,startrow,icol
#ifdef INTEL
       integer (8) :: ndim
       integer (8) :: ielem,in,is,iflag,irow
       integer (8) :: ivector(ixc_dim)
       integer (8) :: itemp8,jtemp8
#else
       integer     :: ndim
       integer     :: ielem,in,is,iflag,irow
       integer     :: ivector(ixc_dim)
       integer     :: itemp8,jtemp8
#endif
       double precision rval
       double precision wt(nrow),vec(nrow)

#ifdef SINGLE_XC
       real             vector(ndim+1)
#else
       double precision vector(ndim+1)
#endif

       ifail=0
       if(ixc_dim.eq.2)then
         if(nrow.gt.nobs)then
           write(6,*) ' ILLEGAL CALL TO SINGLE_VECTOR_MUL3'
           stop
         else
           itemp8=(icol-1)*nobs+pi_par(icol)
           irow=0
           rval=0.0d0
           do jtemp8=itemp8+1,itemp8+nobs
             irow=irow+1
             rval=rval+vector(jtemp8)*vec(irow)*sqrt(wt(irow))
           end do
         end if
         go to 9999
       end if

       if(ndim.eq.maxcompdim)then
         ifail=1
         return
       end if
       ivector(ndim+1)=0

       itemp8=icol-1
       in=itemp8*nxrow+startrow
       call search_vector(ielem,ndim,ivector,in)
       if(ielem.le.0)ielem=abs(ielem)+1
       is=ivector(ielem)

       in=in-1
       rval=0.0d0
       do irow=startrow,nrow
         in=in+1
         if(is.eq.in)then
           iflag=1
         else
           iflag=0
         end if
         if(iflag.eq.1)
     +     rval=rval+vector(ielem)*vec(irow)*sqrt(wt(irow))
         if(iflag.eq.1)then
           ielem=ielem+1   ! note that we assume there is one zero element above ncompdim
           is=ivector(ielem)
         end if
       end do

9999   continue
       return

#ifdef PESTMOD
       end subroutine single_vector_mul3
#else
       end
#endif


       subroutine zero_matrix(ifail,ndim,vector,ivector,endrow)

C -- Subroutine zero_matrix zeroes all elements of compressed matrix below
C    a certain row.

       use pestdata, only: maxcompdim,nxrow,
     +                     pi_par,nobs,nespar,ixc_dim

       implicit none

       integer ifail,endrow

#ifdef INTEL
       integer (8) :: ndim
       integer (8) :: i,j,ies,irow,icol
       integer (8) :: ivector(ixc_dim)
       integer (8) :: itemp8,jtemp8
#else
       integer     :: ndim
       integer     :: i,j,ies,irow,icol
       integer     :: ivector(ixc_dim)
       integer     :: itemp8,jtemp8
#endif

#ifdef SINGLE_XC
       real             vector(ndim+1)
#else
       double precision vector(ndim+1)
#endif

       ifail=0
       if(ndim.eq.0) return
       if(ixc_dim.eq.2)then
         if(endrow.gt.nobs)then
           write(6,*) ' ILLEGAL CALL TO ZERO_MATRIX'
           stop
         else
           do icol=1,nespar
             itemp8=(icol-1)*nobs+pi_par(icol)
             do jtemp8=itemp8+1,itemp8+nobs
               vector(jtemp8)=0.0d0
             end do
           end do
         end if
         go to 9999
       end if

       do i=1,ndim
          j=ivector(i)
          ies=(j-1)/nxrow+1
          irow=j-(ies-1)*nxrow
          if(irow.le.endrow)vector(i)=0.0d0
       end do

9999   continue
       return

#ifdef PESTMOD
       end subroutine zero_matrix
#else
       end
#endif

