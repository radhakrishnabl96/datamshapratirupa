!     Last change:  JD   17 Oct 2006    3:52 pm


module message_utility
	character (len=500)     :: amessage= ' '
end module message_utility


module pdf

    private

! -- Defined types.


type density_function
    character*40                  :: pdf_type      ! distribution type
    integer                       :: pdf_type_num  ! distribution type number
    integer                       :: ndim          ! dimension of distribution
    integer                       :: numintpar     ! number of integer parameters
    integer                       :: numrealpar    ! number of real parameters
    integer, dimension(5)         :: intpar        ! array of integer parameters
    real, dimension(5)            :: realpar       ! array of real parameters
    real, dimension(:), pointer   :: vector1       ! storage vector
    real, dimension(:), pointer   :: vector2       ! storage vector
    real, dimension(:,:), pointer :: matrix1       ! storage matrix
end type density_function

! -- Variables

   integer                          :: maxpdf      ! maximum number of density functions
   integer                          :: numpdf      ! number of used density functions
   type (density_function), allocatable, dimension(:)  :: density     ! density functions
   real                             :: pi=3.14159265
   character (len=40)               :: errsub      ! header to error message


! -- Public subroutines

   public get_random_number,        &
          set_pdf_parameters,       &
          deallocate_pdf,           &
          initialise_density_set,   &
          get_likelihood


! -- Subroutines

contains


subroutine initialise_density_set(ifail,mpdf,iseed)

! -- Subroutine INITIALISE_DENSITY_SET initialises pdf functionality.

        use message_utility
        implicit none

        integer, intent(out)            :: ifail
        integer, intent(in)             :: mpdf
        integer, optional, intent(in)   :: iseed
        integer                         :: iseed1,iseed2,ierr,iii,jjjj1,jjjj2
        double precision                :: rtemp1,rtemp2

        ifail=0
        allocate(density(mpdf),stat=ierr)
        if(ierr.ne.0)then
          write(amessage,10)
10        format('Error in subroutine INITIALISE_DENSITY_SET: cannot allocate memory ', &
          'for use of module pdf.')
          ifail=1
          return
        end if
        maxpdf=mpdf

        numpdf=0

! -- Initialize random number generator.

        call inrgcm()
        call phrtsd('initialise',iseed1,iseed2)
!        iii=nextunit()
!        open(unit=iii,file='seed.dat',iostat=ierr)
!        if(ierr.eq.0)then
!          read(iii,*,iostat=ierr) rtemp1,rtemp2
!          jjjj1=nint(rtemp1)
!          jjjj2=nint(rtemp2)
!          if(ierr.eq.0)then
!            iseed1=jjjj1
!            iseed2=jjjj2
!          end if
!          close(unit=iii,iostat=ierr)
!        end if
        if(present(iseed))then
          iseed1=iseed
          iseed2=2*iseed
        end if
        call setall(iseed1,iseed2)

        return

end subroutine initialise_density_set



subroutine get_random_number(ifail,idist,gen_num,rval,rvals)

! --- Subroutine get_random_number provides a random number from a numbered
!     distribution.

        use message_utility

        implicit none
        integer, intent(out)                 ::   ifail        ! error criterion
        integer, intent(in)                  ::   idist        ! distribution number
        integer, intent(in)                  ::   gen_num      ! generator number
        real, intent(out)                    ::   rval         ! generated number
        real, intent(out),dimension(*), optional :: rvals ! multiple generated numbers

        type (density_function)   :: adensity
        real                      :: rtemp1,rtemp2
        integer                   :: ipdf

        errsub='Error in subroutine GET_RANDOM_NUMBER:'

        ifail=0
        if(idist.gt.numpdf)then
          write(amessage,10) trim(errsub)
10        format(a,' distribution number is larger than expected.')
          ifail=1
          return
        end if
        adensity=density(idist)
        call setcgn(gen_num)

        ipdf=adensity%pdf_type_num
        select case(ipdf)

          case(1)       ! Uniform real
            rval=genunf(adensity%realpar(1),adensity%realpar(2))

          case(2)       ! Normal
            rval=gennor(adensity%realpar(1),adensity%realpar(2))

          case(3)       ! Beta
            rval=genbet(adensity%realpar(1),adensity%realpar(2))

          case(4)       ! Chi-square
            rval=genchi(adensity%realpar(1))

          case(5)       ! Inv-chi-square
            rval=gengam(0.5,0.5*adensity%realpar(1))
            if(rval.ne.0.0) rval=1.0/rval

          case(6)       ! Scaled-inv-chi-square
            rtemp2=0.5*adensity%realpar(1)
!            rtemp1=rtemp2*adensity%realpar(2)*adensity%realpar(2)
            rtemp1=rtemp2*adensity%realpar(2)
            rval=gengam(rtemp1,rtemp2)
            if(rval.ne.0.0) rval=1.0/rval

          case(7)       ! Gamma
            rval=gengam(adensity%realpar(1),adensity%realpar(2))

          case(8)       ! Inv-gamma
            rval=gengam(adensity%realpar(1),adensity%realpar(2))
            if(rval.ne.0.0) rval=1.0/rval

          case(9)       ! Exponential
            rval=genexp(adensity%realpar(1))

          case(10)      ! F
            rval=genf(adensity%realpar(1),adensity%realpar(2))

          case(11)      ! Non-central-chi-square
            rval=gennch(adensity%realpar(1),adensity%realpar(2))

          case(12)      ! Non-central-F
            rval=gennf(adensity%realpar(1),adensity%realpar(2),adensity%realpar(3))

          case(101)     ! Multivariate normal
            call genmn(adensity%vector2,rvals,adensity%vector1)

        end select

        return

end subroutine get_random_number


subroutine set_pdf_parameters(ifail,atype,parameters,idist,ndim,lda,realvec,realmat,icovchg)

! --- Subroutine set_pdf_parameters sets up a new distribution and provides the
!     parameters for it.

        use message_utility

        implicit none

        integer, intent(out)                    :: ifail        ! error criterion
        character (len=*), intent(in)           :: atype        ! type of density distrib.
        real, intent(in),dimension(*)           :: parameters   ! distribution parameters
        integer, intent(inout)                  :: idist        ! assigned density number
        integer, intent(in), optional           :: ndim         ! number of dimensions
        integer, intent(in), optional           :: lda          ! leading dimension or realmat
        real, intent(inout), dimension(:), optional   :: realvec      ! parameter vector
        real, intent(inout), dimension(:,:), optional :: realmat      ! parameter matrix
        integer, intent(in), optional           :: icovchg      ! indicates wether cov. mat. altered

        integer                              :: ierr,jfail,idim,i,j,inew,jcovchg,jflag,iidim
        real                                 :: rdim
        character (len=40)                   :: btype

        ifail=0
        errsub='Error in subroutine SET_PDF_PARAMETERS:'

        jflag=0
        inew=0
        btype=atype
        call casetrans (btype,'lo')

        if(idist.le.0)then
          jflag=1
          numpdf=numpdf+1
          idist=numpdf
          inew=1
          if(numpdf.gt.MAXPDF)then
            write(amessage,10) trim(errsub)
10          format(a,' too many pdf''s required. Increase MAXPDF and re-compile program.')
            ifail=1
            return
          end if
        else
          if(idist.gt.numpdf)then
            write(amessage,11) trim(errsub)
11          format(a,'IDIST variable out of range.')
            ifail=1
            return
          end if
        end if
        if(.not.present(ndim)) then
          idim=1
        else
          idim=ndim
        end if
        if((idim.gt.1).and.                  &
          ((.not.present(lda)).or.           &
           (.not.present(realvec)).or.       &
           (.not.present(realmat))))then
          write(amessage,15) trim(errsub)
15        format(a,' LDA, REALVEC or REALMAT argument not supplied when NDIM ', &
          'is greater than 1.')
          ifail=1
          return
        end if
        if(.not.present(icovchg))then
          jcovchg=1
        else
          if(icovchg.le.0)then
            jcovchg=0
          else
            jcovchg=1
          end if
        end if
        if(jflag.eq.1) jcovchg=1

        select case(btype)

           case('uniform')
               density(idist)%pdf_type=btype
               density(idist)%pdf_type_num=1
               density(idist)%ndim=1
               density(idist)%numintpar=0
               density(idist)%numrealpar=2
               density(idist)%realpar(1)=parameters(1)    ! lower bound
               density(idist)%realpar(2)=parameters(2)    ! upper bound

           case('normal')
               if(parameters(2).le.0.0)then
                 write(amessage,49) trim(errsub)
49               format(a,' standard deviation must be greater than zero for normal ',  &
                 'distribution.')
                 ifail=1
                 return
               end if

               density(idist)%pdf_type=btype
               density(idist)%pdf_type_num=2
               density(idist)%ndim=1
               density(idist)%numintpar=0
               density(idist)%numrealpar=2
               density(idist)%realpar(1)=parameters(1)   ! mean
               density(idist)%realpar(2)=parameters(2)   ! standard deviation

           case('beta')
               if((parameters(1).le.0.0).or.(parameters(2).le.0.0))then
                 write(amessage,50) trim(errsub)
50               format(a,' alpha and beta must be greater than zero for beta ',  &
                 'distribution.')
                 ifail=1
                 return
               end if
               density(idist)%pdf_type=btype
               density(idist)%pdf_type_num=3
               density(idist)%ndim=1
               density(idist)%numintpar=0
               density(idist)%numrealpar=2
               density(idist)%realpar(1)=parameters(1)  ! first param (alpha)
               density(idist)%realpar(2)=parameters(2)  ! second param (beta)

           case('chi-square')
               if(parameters(1).le.0.0)then
                 write(amessage,60) trim(errsub)
60               format(a,' degrees of freedom must be greater than zero for ',  &
                 'Chi-square distribution.')
                 ifail=1
                 return
               end if
               density(idist)%pdf_type=btype
               density(idist)%pdf_type_num=4
               density(idist)%ndim=1
               density(idist)%numintpar=0
               density(idist)%numrealpar=1
               density(idist)%realpar(1)=parameters(1)    ! degrees of freedom

           case('inv-chi-square')
               if(parameters(1).le.0.0)then
                 write(amessage,61) trim(errsub)
61               format(a,' degrees of freedom must be greater than zero for ',  &
                 'inverse-chi-square distribution.')
                 ifail=1
                 return
               end if
               density(idist)%pdf_type=btype
               density(idist)%pdf_type_num=5
               density(idist)%ndim=1
               density(idist)%numintpar=0
               density(idist)%numrealpar=1
               density(idist)%realpar(1)=parameters(1)    ! degrees of freedom

           case('scaled-inv-chi-square')
               if((parameters(1).le.0.0).or.(parameters(2).le.0.0))then
                 write(amessage,62) trim(errsub)
62               format(a,' degrees of freedom and scale must be greater than ', &
                 'zero for scaled-inverse-chi-square distribution.')
                 ifail=1
                 return
               end if
               density(idist)%pdf_type=btype
               density(idist)%pdf_type_num=6
               density(idist)%ndim=1
               density(idist)%numintpar=0
               density(idist)%numrealpar=2
               density(idist)%realpar(1)=parameters(1)    ! degrees of freedom
               density(idist)%realpar(2)=parameters(2)    ! scale

           case('gamma')
               if((parameters(1).le.0.0).or.(parameters(2).le.0.0))then
                 write(amessage,70) trim(errsub)
70               format(a,' location and shape parameters must be greater ',   &
                 'than zero for gamma distribution.')
                 ifail=1
                 return
               end if
               density(idist)%pdf_type=btype
               density(idist)%pdf_type_num=7
               density(idist)%ndim=1
               density(idist)%numintpar=0
               density(idist)%numrealpar=2
               density(idist)%realpar(1)=parameters(1)    ! location parameter
               density(idist)%realpar(2)=parameters(2)    ! shape parameter

           case('inv-gamma')
               if((parameters(1).le.0.0).or.(parameters(2).le.0.0))then
                 write(amessage,80) trim(errsub)
80               format(a,' location and shape parameters must be greater ',   &
                 'than zero for inverse gamma distribution.')
                 ifail=1
                 return
               end if
               density(idist)%pdf_type=btype
               density(idist)%pdf_type_num=8
               density(idist)%ndim=1
               density(idist)%numintpar=0
               density(idist)%numrealpar=2
               density(idist)%realpar(1)=parameters(1)    ! location parameter
               density(idist)%realpar(2)=parameters(2)    ! shape parameter

           case('exponential')
               if(parameters(1).le.0.0) then
                 write(amessage,90) trim(errsub)
90               format(a,' mean value must be greater than zero for exponential ', &
                 'distribution.')
                 ifail=1
                 return
               end if
               density(idist)%pdf_type=btype
               density(idist)%pdf_type_num=9
               density(idist)%ndim=1
               density(idist)%numintpar=0
               density(idist)%numrealpar=1
               density(idist)%realpar(1)=parameters(1)    ! mean

           case('f')
               if((parameters(1).le.0.0).or.(parameters(2).le.0.0)) then
                 write(amessage,100) trim(errsub)
100               format(a,' degrees of freedom must be greater than zero for F ', &
                 'distribution.')
                 ifail=1
                 return
               end if
               density(idist)%pdf_type=btype
               density(idist)%pdf_type_num=10
               density(idist)%ndim=1
               density(idist)%numintpar=0
               density(idist)%numrealpar=2
               density(idist)%realpar(1)=parameters(1)    ! numerator degrees of freedom
               density(idist)%realpar(2)=parameters(2)    ! denominator degrees of freedom

           case('noncen-chi-square')
               if(parameters(1).le.1.0)then
                 write(amessage,110) trim(errsub)
110               format(a,' degrees of freedom must be greater than one for ', &
                 'noncentral chi-square distribution.')
                 ifail=1
                 return
               end if
               if(parameters(2).lt.0.0)then
                 write(amessage,120) trim(errsub)
120              format(a,' noncentrality parameter must not be negative for ', &
                 'noncentral chi-square distribution.')
                 ifail=1
                 return
               end if
               density(idist)%pdf_type=btype
               density(idist)%pdf_type_num=11
               density(idist)%ndim=1
               density(idist)%numintpar=0
               density(idist)%numrealpar=2
               density(idist)%realpar(1)=parameters(1)    ! degrees of freedom
               density(idist)%realpar(2)=parameters(2)    ! noncentrality parameter

           case('noncen-f')
               if(parameters(1).lt.1.0)then
                 write(amessage,130) trim(errsub)
130              format(a,' numerator degrees of freedom must be one or greater ', &
                 'for noncentral F distribution.')
                 ifail=1
                 return
               end if
               if(parameters(2).le.0.0)then
                 write(amessage,140) trim(errsub)
140              format(a,' denominator degrees of freedom must be positive for ', &
                 'noncentral F distribution.')
                 ifail=1
                 return
               end if
               if(parameters(3).lt.0.0)then
                 write(amessage,150) trim(errsub)
150              format(a,' noncentrality parameter be nonnegative for ', &
                 'noncentral F distribution.')
                 ifail=1
                 return
               end if
               density(idist)%pdf_type=btype
               density(idist)%pdf_type_num=12
               density(idist)%ndim=1
               density(idist)%numintpar=0
               density(idist)%numrealpar=3
               density(idist)%realpar(1)=parameters(1)    ! numerator degrees of freedom
               density(idist)%realpar(2)=parameters(2)    ! denominator degrees of freedom
               density(idist)%realpar(3)=parameters(3)    ! noncentrality parameter

           case('multinormal')
               density(idist)%pdf_type=btype
               density(idist)%pdf_type_num=101
               density(idist)%ndim=idim
               density(idist)%numintpar=0
               density(idist)%numrealpar=1
               if(inew.eq.1)then
                 rdim=idim
                 rdim=rdim*(rdim+3)*0.5+2.0
                 iidim=nint(rdim)
                 allocate (density(idist)%vector1(idim),                           &
                           density(idist)%vector2(iidim),                          &
                           density(idist)%matrix1(idim,idim),stat=ierr)
                 if(ierr.ne.0)then
                   write(amessage,20) trim(errsub)
20                 format(a,' error in memory allocation.')
                   ifail=1
                   return
                 end if
               end if
               if(jcovchg.eq.1)then
                 call setgmn(jfail,lda,idim,realvec,realmat,density(idist)%vector2,  &
                              density(idist)%realpar(1))
                                          ! vector2 stores dimension, then mean,
                                          !     then decomposition of covariance matrix
                                          ! realpar(1) stores the determinant of the
                                          !     covariance matrix
                 if(jfail.eq.1)then
                   write(amessage,30) trim(errsub)
30                 format(a,' supplied covariance matrix is not positive definite.')
                   ifail=1
                   return
                 end if
                 do i=1,idim                ! matrix1 stores the inverse of the cov matrix.
                   do j=1,i
                     density(idist)%matrix1(j,i)=realmat(j,i)
                   end do
                   if(i.ne.ndim)then
                     do j=i+1,ndim
                       density(idist)%matrix1(j,i)=realmat(i,j)
                     end do
                   end if
                 end do
               else
                 do i=1,idim
                   density(idist)%vector2(i+1)=realvec(i)
                 end do
               end if

          case default
              write(amessage,40) trim(errsub),trim(atype)
40            format(a,' unknown density type "',a,'".')
              ifail=1
              return

        end select

        return

end subroutine set_pdf_parameters


subroutine get_likelihood(ifail,idist,rval,pdf_val,rvals)

! --- Subroutine get_likelihood generates the likelihood of a certain number
!     based on a nominated distribution number.

        use message_utility
        implicit none

        integer, intent(out)                  :: ifail   ! error criterion
        integer, intent(in)                   :: idist   ! distribution number
        real, intent(in)                      :: rval    ! random number
        real, intent(out)                     :: pdf_val ! calculated likelihood value
        real, intent(in),dimension(:), optional :: rvals ! vector of random numbers

        integer           :: ipdf

        ifail=0
        errsub='Error in subroutine GET_LIKELIHOOD:'

        if(idist.gt.numpdf)then
          write(amessage,10)
10        format(' density distribution number (IDIFF) greater than number of ', &
          'allocated densities.')
          ifail=1
          return
        end if

        ipdf=density(idist)%pdf_type_num
        select case(ipdf)

          case(1)       ! Uniform real
            pdf_val=getpdfunf(density(idist)%realpar(1),density(idist)%realpar(2),rval)

          case(2)       ! Normal
            pdf_val=getpdfnor(density(idist)%realpar(1),density(idist)%realpar(2),rval)

          case(3)       ! Beta
            pdf_val=getpdfbet(density(idist)%realpar(1),density(idist)%realpar(2),rval)

          case(4)       ! Chi-square
            pdf_val=getpdfchi(density(idist)%realpar(1),rval)

          case(5)       ! Inv-chi-square
            pdf_val=getpdfinvchi(density(idist)%realpar(1),rval)

          case(6)       ! Scaled-inv-chi-square
            pdf_val=getpdfscinvchi(density(idist)%realpar(1),density(idist)%realpar(2),rval)

          case(7)       ! Gamma
            pdf_val=getpdfgam(density(idist)%realpar(1),density(idist)%realpar(2),rval)

          case(8)       ! Inv-gamma
            pdf_val=getpdfinvgam(density(idist)%realpar(1),density(idist)%realpar(2),rval)

          case(9)       ! Exponential
            pdf_val=getpdfexp(density(idist)%realpar(1),rval)

          case(10)      ! F
            pdf_val=getpdff(density(idist)%realpar(1),density(idist)%realpar(2),rval)

          case(11)      ! Non-central-chi-square
             if(density(idist)%realpar(2).ne.0.0)then
             pdf_val=getpdfncchi(density(idist)%realpar(1),density(idist)%realpar(2),rval)
             else
             pdf_val=getpdfchi(density(idist)%realpar(1),rval)
             end if

          case(12)      ! Non-central-F
             if(density(idist)%realpar(3).ne.0.0)then
             pdf_val=getpdfncf(density(idist)%realpar(1),density(idist)%realpar(2),    &
                               density(idist)%realpar(3),rval)
             else
             pdf_val=getpdff(density(idist)%realpar(1),density(idist)%realpar(2),rval)
             end if

          case(101)     ! Multivariate normal
             pdf_val=getpdfmnor(density(idist)%ndim,density(idist)%realpar(1),   &
             density(idist)%vector2,density(idist)%matrix1,rvals)

        end select

        return

end subroutine get_likelihood



subroutine deallocate_pdf(ndist)

! --- Subroutine deallocate_pdf de-allocates arrays and nullifies pointers.

        use message_utility

        implicit none
        integer, intent(in)                 :: ndist
        integer                             :: itype,ierr

        if(ndist.eq.-1)then
          deallocate(density,stat=ierr)
          return
        end if
        if(ndist.gt.numpdf) return
        itype=density(ndist)%pdf_type_num
        if(itype.eq.101)then
          if(associated(density(ndist)%vector1))   &
             deallocate(density(ndist)%vector1,stat=ierr)
          if(associated(density(ndist)%vector2))   &
             deallocate(density(ndist)%vector2,stat=ierr)
          if(associated(density(ndist)%matrix1))   &
             deallocate(density(ndist)%matrix1,stat=ierr)
          nullify(density(ndist)%vector1,density(ndist)%vector2,density(ndist)%matrix1)
        end if

        return
end subroutine deallocate_pdf



      SUBROUTINE setgmn(jfail,lda,p,meanv,covm,parm,determinant)
!***********************************************************************
!
!     SUBROUTINE SETGMN( MEANV, COVM, P, PARM)
!            SET Generate Multivariate Normal random deviate
!
!
!                              Function
!
!
!      Places P, MEANV, and the Cholesky factoriztion of COVM
!      in GENMN.
!
!
!                              Arguments
!
!
!     MEANV --> Mean vector of multivariate normal distribution.
!                                        REAL MEANV(P)
!
!     COVM   <--> (Input) Covariance   matrix    of  the  multivariate
!                 normal distribution
!                 (Output) Destroyed on output
!                                        REAL COVM(P,P)
!
!     P     --> Dimension of the normal, or length of MEANV.
!                                        INTEGER P
!
!     PARM <-- Array of parameters needed to generate multivariate norma
!                deviates (P, MEANV and Cholesky decomposition of
!                COVM).
!                1 : 1                - P
!                2 : P + 1            - MEANV
!                P+2 : P*(P+3)/2 + 1  - Cholesky decomposition of COVM
!                                             REAL PARM(P*(P+3)/2 + 1)
!
!**********************************************************************
!     .. Scalar Arguments ..
      INTEGER p,lda,jfail
!     ..
!     .. Array Arguments ..
      REAL covm(lda,p),meanv(p),parm(p* (p+3)/2+1)
      real det(2)
      real determinant
!     ..
!     .. Local Scalars ..
      INTEGER i,icount,info,j
!     ..
!     .. External Subroutines ..
      EXTERNAL spofa
!     ..
!     .. Executable Statements ..
!
!
!     TEST THE INPUT
!
      jfail=0
      IF (.NOT. (p.LE.0)) GO TO 10
      WRITE (*,*) 'P nonpositive in SETGMN'
      WRITE (*,*) 'Value of P: ',p
      STOP 'P nonpositive in SETGMN'

   10 parm(1) = p
!
!     PUT P AND MEANV INTO PARM
!
      DO 20,i = 2,p + 1
          parm(i) = meanv(i-1)
   20 CONTINUE
!
!      Cholesky decomposition to find A s.t. trans(A)*(A) = COVM
!
      CALL spofa(covm,lda,p,info)
      IF (.NOT. (info.NE.0)) GO TO 30
      jfail=1
      return

   30 icount = p + 1
!
!     PUT UPPER HALF OF A, WHICH IS NOW THE CHOLESKY FACTOR, INTO PARM
!          COVM(1,1) = PARM(P+2)
!          COVM(1,2) = PARM(P+3)
!                    :
!          COVM(1,P) = PARM(2P+1)
!          COVM(2,2) = PARM(2P+2)  ...
!
      DO 50,i = 1,p
          DO 40,j = i,p
              icount = icount + 1
              parm(icount) = covm(i,j)
   40     CONTINUE
   50 CONTINUE

! -- The following code has been added to calculate the determinant of the
!    covariance matrix.

      call spodi(covm,lda,p,det,11)
      if(det(1).eq.0.0)then
        determintant=0.0
      else
        determinant=det(1) * 10.0**det(2)
      end if

      RETURN
!
      END SUBROUTINE setgmn


      REAL FUNCTION genunf(low,high)
!**********************************************************************
!
!     REAL FUNCTION GENUNF( LOW, HIGH )
!
!               GeNerate Uniform Real between LOW and HIGH
!
!
!                              Function
!
!
!     Generates a real uniformly distributed between LOW and HIGH.
!
!
!                              Arguments
!
!
!     LOW --> Low bound (exclusive) on real value to be generated
!                         REAL LOW
!
!     HIGH --> High bound (exclusive) on real value to be generated
!                         REAL HIGH
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL high,low
!     ..
!     .. External Functions ..
      REAL ranf
      EXTERNAL ranf
!     ..
!     .. Executable Statements ..
      IF (.NOT. (low.GT.high)) GO TO 10
      WRITE (*,*) 'LOW > HIGH in GENUNF: LOW ',low,' HIGH: ',high
      WRITE (*,*) 'Abort'
      STOP 'LOW > High in GENUNF - Abort'

   10 genunf = low + (high-low)*ranf()

      RETURN

      END FUNCTION genunf


      REAL FUNCTION gennor(av,sd)
!**********************************************************************
!
!     REAL FUNCTION GENNOR( AV, SD )
!
!         GENerate random deviate from a NORmal distribution
!
!
!                              Function
!
!
!     Generates a single random deviate from a normal distribution
!     with mean, AV, and standard deviation, SD.
!
!
!                              Arguments
!
!
!     AV --> Mean of the normal distribution.
!                              REAL AV
!
!     SD --> Standard deviation of the normal distribution.
!                              REAL SD
!
!     GENNOR <-- Generated normal deviate.
!                              REAL GENNOR
!
!
!                              Method
!
!
!     Renames SNORM from TOMS as slightly modified by BWB to use RANF
!     instead of SUNIF.
!
!     For details see:
!               Ahrens, J.H. and Dieter, U.
!               Extensions of Forsythe's Method for Random
!               Sampling from the Normal Distribution.
!               Math. Comput., 27,124 (Oct. 1973), 927 - 937.
!
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL av,sd
!     ..
!     .. External Functions ..
!      REAL snorm
!      EXTERNAL snorm
!     ..
!     .. Executable Statements ..
      gennor = sd*snorm() + av
      RETURN

      END FUNCTION gennor

      REAL FUNCTION snorm()
!**********************************************************************C
!                                                                      C
!                                                                      C
!     (STANDARD-)  N O R M A L  DISTRIBUTION                           C
!                                                                      C
!                                                                      C
!**********************************************************************C
!**********************************************************************C
!                                                                      C
!     FOR DETAILS SEE:                                                 C
!                                                                      C
!               AHRENS, J.H. AND DIETER, U.                            C
!               EXTENSIONS OF FORSYTHE'S METHOD FOR RANDOM             C
!               SAMPLING FROM THE NORMAL DISTRIBUTION.                 C
!               MATH. COMPUT., 27,124 (OCT. 1973), 927 - 937.          C
!                                                                      C
!     ALL STATEMENT NUMBERS CORRESPOND TO THE STEPS OF ALGORITHM 'FL'  C
!     (M=5) IN THE ABOVE PAPER     (SLIGHTLY MODIFIED IMPLEMENTATION)  C
!                                                                      C
!     Modified by Barry W. Brown, Feb 3, 1988 to use RANF instead of   C
!     SUNIF.  The argument IR thus goes away.                          C
!                                                                      C
!**********************************************************************C
!
      DIMENSION a(32),d(31),t(31),h(31)
!
!     THE DEFINITIONS OF THE CONSTANTS A(K), D(K), T(K) AND
!     H(K) ARE ACCORDING TO THE ABOVEMENTIONED ARTICLE
!
      DATA a/0.0,.3917609E-1,.7841241E-1,.1177699,.1573107,.1970991,     &
           .2372021,.2776904,.3186394,.3601299,.4022501,.4450965,        &
           .4887764,.5334097,.5791322,.6260990,.6744898,.7245144,        &
           .7764218,.8305109,.8871466,.9467818,1.009990,1.077516,        &
           1.150349,1.229859,1.318011,1.417797,1.534121,1.675940,        &
           1.862732,2.153875/
      DATA d/5*0.0,.2636843,.2425085,.2255674,.2116342,.1999243,         &
           .1899108,.1812252,.1736014,.1668419,.1607967,.1553497,        &
           .1504094,.1459026,.1417700,.1379632,.1344418,.1311722,        &
           .1281260,.1252791,.1226109,.1201036,.1177417,.1155119,        &
           .1134023,.1114027,.1095039/
      DATA t/.7673828E-3,.2306870E-2,.3860618E-2,.5438454E-2,            &
           .7050699E-2,.8708396E-2,.1042357E-1,.1220953E-1,.1408125E-1,  &
           .1605579E-1,.1815290E-1,.2039573E-1,.2281177E-1,.2543407E-1,  &
           .2830296E-1,.3146822E-1,.3499233E-1,.3895483E-1,.4345878E-1,  &
           .4864035E-1,.5468334E-1,.6184222E-1,.7047983E-1,.8113195E-1,  &
           .9462444E-1,.1123001,.1364980,.1716886,.2276241,.3304980,     &
           .5847031/
      DATA h/.3920617E-1,.3932705E-1,.3950999E-1,.3975703E-1,            &
           .4007093E-1,.4045533E-1,.4091481E-1,.4145507E-1,.4208311E-1,  &
           .4280748E-1,.4363863E-1,.4458932E-1,.4567523E-1,.4691571E-1,  &
           .4833487E-1,.4996298E-1,.5183859E-1,.5401138E-1,.5654656E-1,  &
           .5953130E-1,.6308489E-1,.6737503E-1,.7264544E-1,.7926471E-1,  &
           .8781922E-1,.9930398E-1,.1155599,.1404344,.1836142,.2790016,  &
           .7010474/
!
   10 u = ranf()
      s = 0.0
      IF (u.GT.0.5) s = 1.0
      u = u + u - s
   20 u = 32.0*u
      i = int(u)
      IF (i.EQ.32) i = 31
      IF (i.EQ.0) GO TO 100
!
!                                START CENTER
!
   30 ustar = u - float(i)
      aa = a(i)
   40 IF (ustar.LE.t(i)) GO TO 60
      w = (ustar-t(i))*h(i)
!
!                                EXIT   (BOTH CASES)
!
   50 y = aa + w
      snorm = y
      IF (s.EQ.1.0) snorm = -y
      RETURN
!
!                                CENTER CONTINUED
!
   60 u = ranf()
      w = u* (a(i+1)-aa)
      tt = (0.5*w+aa)*w
      GO TO 80

   70 tt = u
      ustar = ranf()
   80 IF (ustar.GT.tt) GO TO 50
   90 u = ranf()
      IF (ustar.GE.u) GO TO 70
      ustar = ranf()
      GO TO 40
!
!                                START TAIL
!
  100 i = 6
      aa = a(32)
      GO TO 120

  110 aa = aa + d(i)
      i = i + 1
  120 u = u + u
      IF (u.LT.1.0) GO TO 110
  130 u = u - 1.0
  140 w = u*d(i)
      tt = (0.5*w+aa)*w
      GO TO 160

  150 tt = u
  160 ustar = ranf()
      IF (ustar.GT.tt) GO TO 50
  170 u = ranf()
      IF (ustar.GE.u) GO TO 150
      u = ranf()
      GO TO 140

      END FUNCTION snorm


      SUBROUTINE genmn(parm,x,work)
!**********************************************************************
!
!     SUBROUTINE GENMN(PARM,X,WORK)
!              GENerate Multivariate Normal random deviate
!
!
!                              Arguments
!
!
!     PARM --> Parameters needed to generate multivariate normal
!               deviates (MEANV and Cholesky decomposition of
!               COVM). Set by a previous call to SETGMN.
!               1 : 1                - size of deviate, P
!               2 : P + 1            - mean vector
!               P+2 : P*(P+3)/2 + 1  - upper half of cholesky
!                                       decomposition of cov matrix
!                                             REAL PARM(*)
!
!     X    <-- Vector deviate generated.
!                                             REAL X(P)
!
!     WORK <--> Scratch array
!                                             REAL WORK(P)
!
!
!                              Method
!
!
!     1) Generate P independent standard normal deviates - Ei ~ N(0,1)
!
!     2) Using Cholesky decomposition find A s.t. trans(A)*A = COVM
!
!     3) trans(A)E + MEANV ~ N(MEANV,COVM)
!
!**********************************************************************
!     .. Array Arguments ..
      REAL parm(*),work(*),x(*)
!     ..
!     .. Local Scalars ..
      REAL ae
      INTEGER i,icount,j,p
!     ..
!     .. External Functions ..
!      REAL snorm
!      EXTERNAL snorm
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC int
!     ..
!     .. Executable Statements ..
      p = int(parm(1))
!
!     Generate P independent normal deviates - WORK ~ N(0,1)
!
      DO 10,i = 1,p
          work(i) = snorm()
   10 CONTINUE
      DO 30,i = 1,p
!
!     PARM (P+2 : P*(P+3)/2 + 1) contains A, the Cholesky
!      decomposition of the desired covariance matrix.
!          trans(A)(1,1) = PARM(P+2)
!          trans(A)(2,1) = PARM(P+3)
!          trans(A)(2,2) = PARM(P+2+P)
!          trans(A)(3,1) = PARM(P+4)
!          trans(A)(3,2) = PARM(P+3+P)
!          trans(A)(3,3) = PARM(P+2-1+2P)  ...
!
!     trans(A)*WORK + MEANV ~ N(MEANV,COVM)
!
          icount = 0
          ae = 0.0
          DO 20,j = 1,i
              icount = icount + j - 1
              ae = ae + parm(i+ (j-1)*p-icount+p+1)*work(j)
   20     CONTINUE
          x(i) = ae + parm(i+1)
   30 CONTINUE
      RETURN
!
      END SUBROUTINE genmn


      REAL FUNCTION genbet(aa,bb)
!**********************************************************************
!
!     REAL FUNCTION GENBET( A, B )
!               GeNerate BETa random deviate
!
!
!                              Function
!
!
!     Returns a single random deviate from the beta distribution with
!     parameters A and B.  The density of the beta is
!               x^(a-1) * (1-x)^(b-1) / B(a,b) for 0 < x < 1
!
!
!                              Arguments
!
!
!     A --> First parameter of the beta distribution
!                         REAL A
!
!     B --> Second parameter of the beta distribution
!                         REAL B
!
!
!                              Method
!
!
!     R. C. H. Cheng
!     Generating Beta Variatew with Nonintegral Shape Parameters
!     Communications of the ACM, 21:317-322  (1978)
!     (Algorithms BB and BC)
!
!**********************************************************************
!     .. Parameters ..
!     Close to the largest number that can be exponentiated
      REAL expmax
      PARAMETER (expmax=89.0)
!     Close to the largest representable single precision number
      REAL infnty
      PARAMETER (infnty=1.0E38)
!     ..
!     .. Scalar Arguments ..
      REAL aa,bb
!     ..
!     .. Local Scalars ..
      REAL a,alpha,b,beta,delta,gamma,k1,k2,olda,oldb,r,s,t,u1,u2,v,w,y,z
      LOGICAL qsame
!     ..
!     .. External Functions ..
      REAL ranf
      EXTERNAL ranf
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC exp,log,max,min,sqrt
!     ..
!     .. Save statement ..
      SAVE olda,oldb,alpha,beta,gamma,k1,k2
!     ..
!     .. Data statements ..
      DATA olda,oldb/-1,-1/
!     ..
!     .. Executable Statements ..
      qsame = (olda.EQ.aa) .AND. (oldb.EQ.bb)
      IF (qsame) GO TO 20
      IF (.NOT. (aa.LE.0.0.OR.bb.LE.0.0)) GO TO 10
      WRITE (*,*) ' AA or BB <= 0 in GENBET - Abort!'
      WRITE (*,*) ' AA: ',aa,' BB ',bb
      STOP ' AA or BB <= 0 in GENBET - Abort!'

   10 olda = aa
      oldb = bb
   20 IF (.NOT. (min(aa,bb).GT.1.0)) GO TO 100


!     Alborithm BB

!
!     Initialize
!
      IF (qsame) GO TO 30
      a = min(aa,bb)
      b = max(aa,bb)
      alpha = a + b
      beta = sqrt((alpha-2.0)/ (2.0*a*b-alpha))
      gamma = a + 1.0/beta
   30 CONTINUE
   40 u1 = ranf()
!
!     Step 1
!
      u2 = ranf()
      v = beta*log(u1/ (1.0-u1))
      IF (.NOT. (v.GT.expmax)) GO TO 50
      w = infnty
      GO TO 60

   50 w = a*exp(v)
   60 z = u1**2*u2
      r = gamma*v - 1.3862944
      s = a + r - w
!
!     Step 2
!
      IF ((s+2.609438).GE. (5.0*z)) GO TO 70
!
!     Step 3
!
      t = log(z)
      IF (s.GT.t) GO TO 70
!
!     Step 4
!
      IF ((r+alpha*log(alpha/ (b+w))).LT.t) GO TO 40
!
!     Step 5
!
   70 IF (.NOT. (aa.EQ.a)) GO TO 80
      genbet = w/ (b+w)
      GO TO 90

   80 genbet = b/ (b+w)
   90 GO TO 230


!     Algorithm BC

!
!     Initialize
!
  100 IF (qsame) GO TO 110
      a = max(aa,bb)
      b = min(aa,bb)
      alpha = a + b
      beta = 1.0/b
      delta = 1.0 + a - b
      k1 = delta* (0.0138889+0.0416667*b)/ (a*beta-0.777778)
      k2 = 0.25 + (0.5+0.25/delta)*b
  110 CONTINUE
  120 u1 = ranf()
!
!     Step 1
!
      u2 = ranf()
      IF (u1.GE.0.5) GO TO 130
!
!     Step 2
!
      y = u1*u2
      z = u1*y
      IF ((0.25*u2+z-y).GE.k1) GO TO 120
      GO TO 170
!
!     Step 3
!
  130 z = u1**2*u2
      IF (.NOT. (z.LE.0.25)) GO TO 160
      v = beta*log(u1/ (1.0-u1))
      IF (.NOT. (v.GT.expmax)) GO TO 140
      w = infnty
      GO TO 150

  140 w = a*exp(v)
  150 GO TO 200

  160 IF (z.GE.k2) GO TO 120
!
!     Step 4
!
!
!     Step 5
!
  170 v = beta*log(u1/ (1.0-u1))
      IF (.NOT. (v.GT.expmax)) GO TO 180
      w = infnty
      GO TO 190

  180 w = a*exp(v)
  190 IF ((alpha* (log(alpha/ (b+w))+v)-1.3862944).LT.log(z)) GO TO 120
!
!     Step 6
!
  200 IF (.NOT. (a.EQ.aa)) GO TO 210
      genbet = w/ (b+w)
      GO TO 220

  210 genbet = b/ (b+w)
  220 CONTINUE
  230 RETURN

      END FUNCTION genbet


      REAL FUNCTION genchi(df)
!**********************************************************************
!
!     REAL FUNCTION GENCHI( DF )
!                Generate random value of CHIsquare variable
!
!
!                              Function
!
!
!     Generates random deviate from the distribution of a chisquare
!     with DF degrees of freedom random variable.
!
!
!                              Arguments
!
!
!     DF --> Degrees of freedom of the chisquare
!            (Must be positive)
!                         REAL DF
!
!
!                              Method
!
!
!     Uses relation between chisquare and gamma.
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL df
!     ..
!     .. External Functions ..
!      REAL gengam
!      EXTERNAL gengam
!     ..
!     .. Executable Statements ..
      IF (.NOT. (df.LE.0.0)) GO TO 10
      WRITE (*,*) 'DF <= 0 in GENCHI - ABORT'
      WRITE (*,*) 'Value of DF: ',df
      STOP 'DF <= 0 in GENCHI - ABORT'

   10 genchi = 2.0*gengam(1.0,df/2.0)
      RETURN

      END FUNCTION genchi



      REAL FUNCTION gengam(a,r)
!**********************************************************************
!
!     REAL FUNCTION GENGAM( A, R )
!           GENerates random deviates from GAMma distribution
!
!
!                              Function
!
!
!     Generates random deviates from the gamma distribution whose
!     density is
!          (A**R)/Gamma(R) * X**(R-1) * Exp(-A*X)
!
!
!                              Arguments
!
!
!     A --> Location parameter of Gamma distribution
!                              REAL A
!
!     R --> Shape parameter of Gamma distribution
!                              REAL R
!
!
!                              Method
!
!
!     Renames SGAMMA from TOMS as slightly modified by BWB to use RANF
!     instead of SUNIF.
!
!     For details see:
!               (Case R >= 1.0)
!               Ahrens, J.H. and Dieter, U.
!               Generating Gamma Variates by a
!               Modified Rejection Technique.
!               Comm. ACM, 25,1 (Jan. 1982), 47 - 54.
!     Algorithm GD
!
!               (Case 0.0 <= R <= 1.0)
!               Ahrens, J.H. and Dieter, U.
!               Computer Methods for Sampling from Gamma,
!               Beta, Poisson and Binomial Distributions.
!               Computing, 12 (1974), 223-246/
!     Adapted algorithm GS.
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL a,r
!     ..
!     .. External Functions ..
!      REAL sgamma
!      EXTERNAL sgamma
!     ..
!     .. Executable Statements ..
      gengam = sgamma(r)
      gengam = gengam/a
      RETURN

      END FUNCTION gengam


      REAL FUNCTION sgamma(a)
!**********************************************************************C
!                                                                      C
!                                                                      C
!     (STANDARD-)  G A M M A  DISTRIBUTION                             C
!                                                                      C
!                                                                      C
!**********************************************************************C
!**********************************************************************C
!                                                                      C
!               PARAMETER  A >= 1.0  !                                 C
!                                                                      C
!**********************************************************************C
!                                                                      C
!     FOR DETAILS SEE:                                                 C
!                                                                      C
!               AHRENS, J.H. AND DIETER, U.                            C
!               GENERATING GAMMA VARIATES BY A                         C
!               MODIFIED REJECTION TECHNIQUE.                          C
!               COMM. ACM, 25,1 (JAN. 1982), 47 - 54.                  C
!                                                                      C
!     STEP NUMBERS CORRESPOND TO ALGORITHM 'GD' IN THE ABOVE PAPER     C
!                                 (STRAIGHTFORWARD IMPLEMENTATION)     C
!                                                                      C
!     Modified by Barry W. Brown, Feb 3, 1988 to use RANF instead of   C
!     SUNIF.  The argument IR thus goes away.                          C
!                                                                      C
!**********************************************************************C
!                                                                      C
!               PARAMETER  0.0 < A < 1.0  !                            C
!                                                                      C
!**********************************************************************C
!                                                                      C
!     FOR DETAILS SEE:                                                 C
!                                                                      C
!               AHRENS, J.H. AND DIETER, U.                            C
!               COMPUTER METHODS FOR SAMPLING FROM GAMMA,              C
!               BETA, POISSON AND BINOMIAL DISTRIBUTIONS.              C
!               COMPUTING, 12 (1974), 223 - 246.                       C
!                                                                      C
!     (ADAPTED IMPLEMENTATION OF ALGORITHM 'GS' IN THE ABOVE PAPER)    C
!                                                                      C
!**********************************************************************C
!
!
!     INPUT: A =PARAMETER (MEAN) OF THE STANDARD GAMMA DISTRIBUTION
!     OUTPUT: SGAMMA = SAMPLE FROM THE GAMMA-(A)-DISTRIBUTION
!
!     COEFFICIENTS Q(K) - FOR Q0 = SUM(Q(K)*A**(-K))
!     COEFFICIENTS A(K) - FOR Q = Q0+(T*T/2)*SUM(A(K)*V**K)
!     COEFFICIENTS E(K) - FOR EXP(Q)-1 = SUM(E(K)*Q**K)
!
      DATA q1,q2,q3,q4,q5,q6,q7/.04166669,.02083148,.00801191,.00144121,  &
           -.00007388,.00024511,.00024240/
      DATA a1,a2,a3,a4,a5,a6,a7/.3333333,-.2500030,.2000062,-.1662921,    &
           .1423657,-.1367177,.1233795/
      DATA e1,e2,e3,e4,e5/1.,.4999897,.1668290,.0407753,.0102930/
!
!     PREVIOUS A PRE-SET TO ZERO - AA IS A', AAA IS A"
!     SQRT32 IS THE SQUAREROOT OF 32 = 5.656854249492380
!
      DATA aa/0.0/,aaa/0.0/,sqrt32/5.656854/
!
!     SAVE STATEMENTS
!
      SAVE aa,aaa,s2,s,d,q0,b,si,c
!
      IF (a.EQ.aa) GO TO 10
      IF (a.LT.1.0) GO TO 140
!
!     STEP  1:  RECALCULATIONS OF S2,S,D IF A HAS CHANGED
!
      aa = a
      s2 = a - 0.5
      s = sqrt(s2)
      d = sqrt32 - 12.0*s
!
!     STEP  2:  T=STANDARD NORMAL DEVIATE,
!               X=(S,1/2)-NORMAL DEVIATE.
!               IMMEDIATE ACCEPTANCE (I)
!
   10 t = snorm()
      x = s + 0.5*t
      sgamma = x*x
      IF (t.GE.0.0) RETURN
!
!     STEP  3:  U= 0,1 -UNIFORM SAMPLE. SQUEEZE ACCEPTANCE (S)
!
      u = ranf()
      IF (d*u.LE.t*t*t) RETURN
!
!     STEP  4:  RECALCULATIONS OF Q0,B,SI,C IF NECESSARY
!
      IF (a.EQ.aaa) GO TO 40
      aaa = a
      r = 1.0/a
      q0 = ((((((q7*r+q6)*r+q5)*r+q4)*r+q3)*r+q2)*r+q1)*r
!
!               APPROXIMATION DEPENDING ON SIZE OF PARAMETER A
!               THE CONSTANTS IN THE EXPRESSIONS FOR B, SI AND
!               C WERE ESTABLISHED BY NUMERICAL EXPERIMENTS
!
      IF (a.LE.3.686) GO TO 30
      IF (a.LE.13.022) GO TO 20
!
!               CASE 3:  A .GT. 13.022
!
      b = 1.77
      si = .75
      c = .1515/s
      GO TO 40
!
!               CASE 2:  3.686 .LT. A .LE. 13.022
!
   20 b = 1.654 + .0076*s2
      si = 1.68/s + .275
      c = .062/s + .024
      GO TO 40
!
!               CASE 1:  A .LE. 3.686
!
   30 b = .463 + s + .178*s2
      si = 1.235
      c = .195/s - .079 + .16*s
!
!     STEP  5:  NO QUOTIENT TEST IF X NOT POSITIVE
!
   40 IF (x.LE.0.0) GO TO 70
!
!     STEP  6:  CALCULATION OF V AND QUOTIENT Q
!
      v = t/ (s+s)
      IF (abs(v).LE.0.25) GO TO 50
      q = q0 - s*t + 0.25*t*t + (s2+s2)*alog(1.0+v)
      GO TO 60

   50 q = q0 + 0.5*t*t* ((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v
!
!     STEP  7:  QUOTIENT ACCEPTANCE (Q)
!
   60 IF (alog(1.0-u).LE.q) RETURN
!
!     STEP  8:  E=STANDARD EXPONENTIAL DEVIATE
!               U= 0,1 -UNIFORM DEVIATE
!               T=(B,SI)-DOUBLE EXPONENTIAL (LAPLACE) SAMPLE
!
   70 e = sexpo()
      u = ranf()
      u = u + u - 1.0
      t = b + sign(si*e,u)
      IF (.NOT. (u.GE.0.0)) GO TO 80
      t = b + si*e
      GO TO 90

   80 t = b - si*e

!
!     STEP  9:  REJECTION IF T .LT. TAU(1) = -.71874483771719
!
   90 IF (t.LT. (-.7187449)) GO TO 70
!
!     STEP 10:  CALCULATION OF V AND QUOTIENT Q
!
      v = t/ (s+s)
      IF (abs(v).LE.0.25) GO TO 100
      q = q0 - s*t + 0.25*t*t + (s2+s2)*alog(1.0+v)
      GO TO 110

  100 q = q0 + 0.5*t*t* ((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v
!
!     STEP 11:  HAT ACCEPTANCE (H) (IF Q NOT POSITIVE GO TO STEP 8)
!
  110 IF (q.LE.0.0) GO TO 70
      IF (q.LE.0.5) GO TO 120
      w = exp(q) - 1.0
      GO TO 130

  120 w = ((((e5*q+e4)*q+e3)*q+e2)*q+e1)*q
!
!               IF T IS REJECTED, SAMPLE AGAIN AT STEP 8
!
  130 IF (c*abs(u).GT.w*exp(e-0.5*t*t)) GO TO 70
      x = s + 0.5*t
      sgamma = x*x
      RETURN
!
!     ALTERNATE METHOD FOR PARAMETERS A BELOW 1  (.3678794=EXP(-1.))
!
  140 aa = 0.0
      b = 1.0 + .3678794*a
  150 p = b*ranf()
      IF (p.GE.1.0) GO TO 160
      sgamma = exp(alog(p)/a)
      IF (sexpo().LT.sgamma) GO TO 150
      RETURN

  160 sgamma = -alog((b-p)/a)
      IF (sexpo().LT. (1.0-a)*alog(sgamma)) GO TO 150
      RETURN

      END FUNCTION sgamma


      REAL FUNCTION sexpo()
!**********************************************************************C
!                                                                      C
!                                                                      C
!     (STANDARD-)  E X P O N E N T I A L   DISTRIBUTION                C
!                                                                      C
!                                                                      C
!**********************************************************************C
!**********************************************************************C
!                                                                      C
!     FOR DETAILS SEE:                                                 C
!                                                                      C
!               AHRENS, J.H. AND DIETER, U.                            C
!               COMPUTER METHODS FOR SAMPLING FROM THE                 C
!               EXPONENTIAL AND NORMAL DISTRIBUTIONS.                  C
!               COMM. ACM, 15,10 (OCT. 1972), 873 - 882.               C
!                                                                      C
!     ALL STATEMENT NUMBERS CORRESPOND TO THE STEPS OF ALGORITHM       C
!     'SA' IN THE ABOVE PAPER (SLIGHTLY MODIFIED IMPLEMENTATION)       C
!                                                                      C
!     Modified by Barry W. Brown, Feb 3, 1988 to use RANF instead of   C
!     SUNIF.  The argument IR thus goes away.                          C
!                                                                      C
!**********************************************************************C
!
      DIMENSION q(8)
      EQUIVALENCE (q(1),q1)
!
!     Q(N) = SUM(ALOG(2.0)**K/K!)    K=1,..,N ,      THE HIGHEST N
!     (HERE 8) IS DETERMINED BY Q(N)=1.0 WITHIN STANDARD PRECISION
!
      DATA q/.6931472,.9333737,.9888778,.9984959,.9998293,.9999833,   &
           .9999986,.9999999/
!
   10 a = 0.0
      u = ranf()
      GO TO 30

   20 a = a + q1
   30 u = u + u
      IF (u.LE.1.0) GO TO 20
   40 u = u - 1.0
      IF (u.GT.q1) GO TO 60
   50 sexpo = a + u
      RETURN

   60 i = 1
      ustar = ranf()
      umin = ustar
   70 ustar = ranf()
      IF (ustar.LT.umin) umin = ustar
   80 i = i + 1
      IF (u.GT.q(i)) GO TO 70
   90 sexpo = a + umin*q1
      RETURN

      END FUNCTION sexpo


      REAL FUNCTION genexp(av)

!**********************************************************************
!
!     REAL FUNCTION GENEXP( AV )
!
!                    GENerate EXPonential random deviate
!
!
!                              Function
!
!
!     Generates a single random deviate from an exponential
!     distribution with mean AV.
!
!
!                              Arguments
!
!
!     AV --> The mean of the exponential distribution from which
!            a random deviate is to be generated.
!                              REAL AV
!
!     GENEXP <-- The random deviate.
!                              REAL GENEXP
!
!
!                              Method
!
!
!     Renames SEXPO from TOMS as slightly modified by BWB to use RANF
!     instead of SUNIF.
!
!     For details see:
!
!               Ahrens, J.H. and Dieter, U.
!               Computer Methods for Sampling From the
!               Exponential and Normal Distributions.
!               Comm. ACM, 15,10 (Oct. 1972), 873 - 882.
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL av
!     ..
!     .. External Functions ..
!      REAL sexpo
!      EXTERNAL sexpo
!     ..
!     .. Executable Statements ..
      genexp = sexpo()*av
      RETURN

      END FUNCTION GENEXP


      REAL FUNCTION genf(dfn,dfd)
!**********************************************************************
!
!     REAL FUNCTION GENF( DFN, DFD )
!                GENerate random deviate from the F distribution
!
!
!                              Function
!
!
!     Generates a random deviate from the F (variance ratio)
!     distribution with DFN degrees of freedom in the numerator
!     and DFD degrees of freedom in the denominator.
!
!
!                              Arguments
!
!
!     DFN --> Numerator degrees of freedom
!             (Must be positive)
!                              REAL DFN
!      DFD --> Denominator degrees of freedom
!             (Must be positive)
!                              REAL DFD
!
!
!                              Method
!
!
!     Directly generates ratio of chisquare variates
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL dfd,dfn
!     ..
!     .. Local Scalars ..
      REAL xden,xnum
!     ..
!     .. External Functions ..
!      REAL genchi
!      EXTERNAL genchi
!     ..
!     .. Executable Statements ..
      IF (.NOT. (dfn.LE.0.0.OR.dfd.LE.0.0)) GO TO 10
      WRITE (*,*) 'Degrees of freedom nonpositive in GENF - abort!'
      WRITE (*,*) 'DFN value: ',dfn,'DFD value: ',dfd
      STOP 'Degrees of freedom nonpositive in GENF - abort!'

   10 xnum = genchi(dfn)/dfn
!      GENF = ( GENCHI( DFN ) / DFN ) / ( GENCHI( DFD ) / DFD )
      xden = genchi(dfd)/dfd
      IF (.NOT. (xden.LE. (1.0E-38*xnum))) GO TO 20
      WRITE (*,*) ' GENF - generated numbers would cause overflow'
      WRITE (*,*) ' Numerator ',xnum,' Denominator ',xden
      WRITE (*,*) ' GENF returning 1.0E38'
      genf = 1.0E38
      GO TO 30

   20 genf = xnum/xden
   30 RETURN

      END FUNCTION GENF

      REAL FUNCTION gennch(df,xnonc)
!**********************************************************************
!
!     REAL FUNCTION GENNCH( DF, XNONC )
!           Generate random value of Noncentral CHIsquare variable
!
!
!                              Function
!
!

!     Generates random deviate  from the  distribution  of a  noncentral
!     chisquare with DF degrees  of freedom and noncentrality  parameter
!     XNONC.
!
!
!                              Arguments
!
!
!     DF --> Degrees of freedom of the chisquare
!            (Must be > 1.0)
!                         REAL DF
!
!     XNONC --> Noncentrality parameter of the chisquare
!               (Must be >= 0.0)
!                         REAL XNONC
!
!
!                              Method
!
!
!     Uses fact that  noncentral chisquare  is  the  sum of a  chisquare
!     deviate with DF-1  degrees of freedom plus the  square of a normal
!     deviate with mean XNONC and standard deviation 1.
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL df,xnonc
!     ..
!     .. External Functions ..
!      REAL genchi,gennor
!      EXTERNAL genchi,gennor
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC sqrt
!     ..
!     .. Executable Statements ..
      IF (.NOT. (df.LE.1.0.OR.xnonc.LT.0.0)) GO TO 10
      WRITE (*,*) 'DF <= 1 or XNONC < 0 in GENNCH - ABORT'
      WRITE (*,*) 'Value of DF: ',df,' Value of XNONC',xnonc
      STOP 'DF <= 1 or XNONC < 0 in GENNCH - ABORT'

   10 gennch = genchi(df-1.0) + gennor(sqrt(xnonc),1.0)**2
      RETURN

      END FUNCTION GENNCH

      REAL FUNCTION gennf(dfn,dfd,xnonc)

!**********************************************************************
!
!     REAL FUNCTION GENNF( DFN, DFD, XNONC )
!           GENerate random deviate from the Noncentral F distribution
!
!
!                              Function
!
!
!     Generates a random deviate from the  noncentral F (variance ratio)
!     distribution with DFN degrees of freedom in the numerator, and DFD
!     degrees of freedom in the denominator, and noncentrality parameter
!     XNONC.
!
!
!                              Arguments
!
!
!     DFN --> Numerator degrees of freedom
!             (Must be >= 1.0)
!                              REAL DFN
!      DFD --> Denominator degrees of freedom
!             (Must be positive)
!                              REAL DFD
!
!     XNONC --> Noncentrality parameter
!               (Must be nonnegative)
!                              REAL XNONC
!
!
!                              Method
!
!
!     Directly generates ratio of noncentral numerator chisquare variate
!     to central denominator chisquare variate.
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL dfd,dfn,xnonc
!     ..
!     .. Local Scalars ..
      REAL xden,xnum
      LOGICAL qcond
!     ..
!     .. External Functions ..
!      REAL genchi,gennch
!      EXTERNAL genchi,gennch
!     ..
!     .. Executable Statements ..
      qcond = dfn .LE. 1.0 .OR. dfd .LE. 0.0 .OR. xnonc .LT. 0.0
      IF (.NOT. (qcond)) GO TO 10
      WRITE (*,*) 'In GENNF - Either (1) Numerator DF <= 1.0 or'
      WRITE (*,*) '(2) Denominator DF < 0.0 or '
      WRITE (*,*) '(3) Noncentrality parameter < 0.0'
      WRITE (*,*) 'DFN value: ',dfn,'DFD value: ',dfd,'XNONC value: ',xnonc
      STOP 'Degrees of freedom or noncent param our of range in GENNF'

   10 xnum = gennch(dfn,xnonc)/dfn
!      GENNF = ( GENNCH( DFN, XNONC ) / DFN ) / ( GENCHI( DFD ) / DFD )
      xden = genchi(dfd)/dfd
      IF (.NOT. (xden.LE. (1.0E-38*xnum))) GO TO 20
      WRITE (*,*) ' GENNF - generated numbers would cause overflow'
      WRITE (*,*) ' Numerator ',xnum,' Denominator ',xden
      WRITE (*,*) ' GENNF returning 1.0E38'
      gennf = 1.0E38
      GO TO 30

   20 gennf = xnum/xden
   30 RETURN

      END FUNCTION gennf


real function getpdfunf(lower,upper,rval)

! --- Function gtpdfunf calculates the pdf value for a uniform distribution.

      implicit none

      real, intent(in)     :: lower   ! lower bound of distribution
      real, intent(in)     :: upper   ! upper bound of distribution
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated

      if(rval.lt.lower)then
        getpdfunf=0.0
      else if(rval.gt.upper)then
        getpdfunf=0.0
      else
        if(upper.le.lower)then
          getpdfunf=0.0
        else
          getpdfunf=1.0/(upper-lower)
        end if
      end if

      return

end function getpdfunf


real function getpdfnor(av,sd,rval)

! --- Function gtpdfnor calculates the pdf value for a normal distribution.

      implicit none

      real, intent(in)     :: av      ! mean
      real, intent(in)     :: sd      ! standard deviation
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated

      real                 :: rtemp

      if(sd.eq.0.0)then
        getpdfnor=0.0
      else
        rtemp=(rval-av)*(rval-av)*0.5/(sd*sd)
        getpdfnor=exp(-rtemp)/sd/sqrt(2.0*pi)
      end if

      return

end function getpdfnor


real function getpdfbet(alpha,beta,rval)

! --- Function gtpdfbet calculates the pdf value for a beta distribution.

      implicit none

      real, intent(in)     :: alpha   ! alpha
      real, intent(in)     :: beta    ! beta
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated

      double precision     :: dnum1,dalpha,dbeta,dval,dratio,dloggamma

      if((rval.lt.0.0).or.(rval.gt.1.0))then
        getpdfbet=0.0
      else
        dval=rval
        dalpha=alpha
        dbeta=beta
        dratio=dloggamma(dalpha+dbeta)-dloggamma(dalpha)-dloggamma(dbeta)
        if(dratio.gt.80.59d0)then
          dratio=1.0d35
        else
          dratio=exp(dratio)
        end if
        getpdfbet=dval**(dalpha-1.0d0)*(1.0d0-dval)**(dbeta-1.0d0)*dratio
      end if

      return

end function getpdfbet


real function getpdfchi(mu,rval)

! --- Function gtpdfchi calculates the pdf value for a chi-square distribution.

      implicit none

      real, intent(in)     :: mu      ! degrees of freedom
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated

      double precision     :: dmu,dtemp1,dtemp2,dval,dloggamma

      if((rval.le.0.0).or.(mu.le.0.0))then
        getpdfchi=0.0
      else
        dval=rval
        dmu=mu
        dtemp2=dmu*0.5d0
        dtemp1=-dtemp2*log(2.0d0)+(dtemp2-1.0d0)*log(dval)-0.5d0*dval-dloggamma(dtemp2)
        if(dtemp1.gt.80.59d0)then
          getpdfchi=1.0e35
        else
          getpdfchi=exp(dtemp1)
        end if
      end if

      return

end function getpdfchi


real function getpdfinvchi(mu,rval)

! --- Function gtpdfinvchi calculates the pdf value for an inverse-chi-square distribution.

      implicit none

      real, intent(in)     :: mu      ! degrees of freedom
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated

      double precision     :: dmu,dtemp1,dtemp2,dval,dloggamma

      if((rval.le.0.0).or.(mu.le.0.0))then
        getpdfinvchi=0.0
      else
        dval=rval
        dmu=mu
        dtemp2=dmu*0.5d0
        dtemp1=-dtemp2*log(2.0d0)-(dtemp2+1.0d0)*log(dval)-0.5d0/dval-dloggamma(dtemp2)
        if(dtemp1.gt.80.59d0)then
          getpdfinvchi=1.0e35
        else
          getpdfinvchi=exp(dtemp1)
        end if
      end if

      return

end function getpdfinvchi


real function getpdfscinvchi(mu,s,rval)

! --- Function gtpdfscinvchi calculates the pdf value for a scaled
!     inverse-chi-square distribution.

      implicit none

      real, intent(in)     :: mu      ! degrees of freedom
      real, intent(in)     :: s       ! scale
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated

      double precision     :: dmu,dtemp1,dtemp2,dval,dloggamma,ds

      if((rval.le.0.0).or.(mu.le.0.0).or.(s.le.0.0))then
        getpdfscinvchi=0.0
      else
        dval=rval
        dmu=mu
        ds=s
        dtemp2=dmu*0.5d0
        dtemp1=dtemp2*log(dtemp2)+dmu*log(ds)*0.5-(dtemp2+1.0d0)*log(dval)   &
               -dmu*ds*0.5d0/dval-dloggamma(dtemp2)
        if(dtemp1.gt.80.59d0)then
          getpdfscinvchi=1.0e35
        else
          getpdfscinvchi=exp(dtemp1)
        end if
      end if

      return

end function getpdfscinvchi


real function getpdfgam(beta,alpha,rval)

! --- Function getpdfgam calculates the pdf value for a gamma distribution.

      implicit none

      real, intent(in)     :: beta    ! location
      real, intent(in)     :: alpha   ! shape
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated

      double precision     :: dbeta,dalpha,dtemp,dval,dloggamma

      if((rval.le.0.0).or.(alpha.le.0.0).or.(beta.le.0.0))then
        getpdfgam=0.0
      else
        dval=rval
        dalpha=alpha
        dbeta=beta
        dtemp=dalpha*log(dbeta)+(dalpha-1.0)*log(dval)-dbeta*dval-dloggamma(dalpha)
        if(dtemp.gt.80.59d0)then
          getpdfgam=1.0e35
        else
          getpdfgam=exp(dtemp)
        end if
      end if

      return

end function getpdfgam



real function getpdfinvgam(beta,alpha,rval)

! --- Function getpdfinvgam calculates the pdf value for an inverse gamma distribution.

      implicit none

      real, intent(in)     :: beta    ! location
      real, intent(in)     :: alpha   ! shape
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated

      double precision     :: dbeta,dalpha,dtemp,dval,dloggamma

      if((rval.le.0.0).or.(alpha.le.0.0).or.(beta.le.0.0))then
        getpdfinvgam=0.0
      else
        dval=rval
        dalpha=alpha
        dbeta=beta
        dtemp=dalpha*log(dbeta)-(dalpha+1.0)*log(dval)-dbeta/dval-dloggamma(dalpha)
        if(dtemp.gt.80.59d0)then
          getpdfinvgam=1.0e35
        else
          getpdfinvgam=exp(dtemp)
        end if
      end if

      return

end function getpdfinvgam



real function getpdfexp(mean,rval)

! --- Function gtpdfexp calculates the pdf value for an exponential distribution.

      implicit none

      real, intent(in)     :: mean    ! mean
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated

      if((rval.le.0.0).or.(mean.le.0.0))then
        getpdfexp=0.0
      else
        getpdfexp=exp(-rval/mean)/mean
      end if

      return

end function getpdfexp


real function getpdff(numdof,dendof,rval)

! --- Function getpdff calculates the pdf value for an F distribution.

      implicit none

      real, intent(in)     :: numdof  ! numerator degrees of freedom
      real, intent(in)     :: dendof  ! denominator degrees of freedom
      real, intent(in)     :: rval    ! variable value at which pdf is to be calculated

      double precision     :: dnumdof,ddendof,dtemp1,dtemp2,dtemp3,dval,dloggamma

      if((rval.le.0.0).or.(numdof.le.0.0).or.(dendof.le.0.0))then
        getpdff=0.0
      else
        dval=rval
        dnumdof=numdof
        ddendof=dendof
        dtemp1=dnumdof*0.5d0
        dtemp2=ddendof*0.5d0
        dtemp3=dloggamma(dtemp1+dtemp2)-dloggamma(dtemp1)-dloggamma(dtemp2)
        dtemp3=dtemp3+dtemp1*log(dnumdof/ddendof)+(dtemp1-1.0d0)*log(dval)  &
               -(dtemp1+dtemp2)*log(1.0d0+dnumdof/ddendof*dval)
        if(dtemp3.gt.80.59d0)then
          getpdff=1.0e35
        else
          getpdff=exp(dtemp3)
        end if
      end if

      return

end function getpdff


real function getpdfncchi(dof,noncent,rval)

! --- Function getpdfncchi calculates the pdf value for a non-central-chi-square distribution.

      implicit none

      real, intent(in)     :: dof      ! degrees of freedom
      real, intent(in)     :: noncent  ! noncentrality parameter
      real, intent(in)     :: rval     ! variable value at which pdf is to be calculated

      double precision     :: ddof,dnoncent,dtemp1,dtemp2,dtemp3,dtemp4,dval,dloggamma,jfac,j

      if((rval.le.0.0).or.(dof.le.1.0).or.(noncent.lt.0.0d0))then
        getpdfncchi=0.0
      else
        dval=rval
        ddof=dof
        dnoncent=noncent
        dtemp1=ddof*0.5d0
        dtemp2=dnoncent*0.5d0
        dtemp3=dval*0.5d0
        getpdfncchi=0.0d0
        j=-1
10      continue
        j=j+1
        if(nint(j).eq.0)then
          jfac=1.0d0
        else
         jfac=j*jfac
        end if
        dtemp4=j*log(dtemp2)+(dtemp1+j-1.0d0)*log(dval)-dtemp3-log(jfac)-  &
               (dtemp1+j)*log(2.0d0)-dloggamma(dtemp1+j)
        if(dtemp4.gt.80.59d0)then
          dtemp4=1.0e35
        else
          dtemp4=exp(dtemp4)
        end if
        getpdfncchi=getpdfncchi+dtemp4
        if((j.gt.2).and.(getpdfncchi.eq.0.0d0)) go to 20
        if(abs(dtemp4).lt.abs(getpdfncchi)*1.0d-4) go to 20
        if(j.gt.10000)then
          write(6,30)
30        format(/,' *** Convergence error in subroutine GETPDFNCCHI ***',/)
          stop
        end if
        go to 10
20      continue
        getpdfncchi=exp(-dtemp2)*getpdfncchi
      end if

      return

end function getpdfncchi


real function getpdfncf(numdof,dendof,noncent,rval)

! --- Function getpdfncf calculates the pdf value for a non-central-F distribution.

      implicit none

      real, intent(in)     :: numdof      ! numerator degrees of freedom
      real, intent(in)     :: dendof      ! denominator degrees of freedom
      real, intent(in)     :: noncent     ! noncentrality parameter
      real, intent(in)     :: rval        ! variable value at which pdf is to be calculated

      double precision     :: dnumdof,ddendof,dnoncent,dtemp1,dtemp2,dtemp3,dtemp4,  &
                              dtemp5,dval,dloggamma,jfac,j

      if((rval.le.0.0).or.(numdof.lt.1.0).or.(dendof.le.0.0).or.(noncent.lt.0.0))then
        getpdfncf=0.0
      else
        dval=rval
        dnumdof=numdof
        ddendof=dendof
        dnoncent=noncent
        dtemp1=dnumdof*0.5d0
        dtemp2=ddendof*0.5d0
        dtemp3=dnoncent*0.5d0
        dtemp4=dval*0.5d0
        getpdfncf=0.0d0
        j=-1
10      continue
        j=j+1
        if(nint(j).eq.0)then
          jfac=1.0d0
        else
         jfac=j*jfac
        end if
        dtemp5=dloggamma(dtemp1+dtemp2+j)-dloggamma(dtemp1+j)-dloggamma(dtemp2)
        dtemp5=dtemp5+                                                     &
               j*log(dtemp3)+(dtemp1+j)*log(dnumdof)+dtemp2*log(ddendof)+  &
               (dtemp1+j-1.0d0)*log(dval)-log(jfac)-                       &
               (dtemp1+dtemp2+j)*log(ddendof+dnumdof*dval)
        if(dtemp5.gt.80.59d0)then
          dtemp5=1.0e35
        else
          dtemp5=exp(dtemp5)
        end if
        getpdfncf=getpdfncf+dtemp5
        if((j.gt.2).and.(getpdfncf.eq.0.0d0)) go to 20
        if(abs(dtemp5).lt.abs(getpdfncf)*1.0d-4) go to 20
        if(j.gt.10000)then
          write(6,30)
30        format(/,' *** Convergence error in subroutine GETPDFNCF ***',/)
          stop
        end if
        go to 10
20      continue
        getpdfncf=exp(-dtemp3)*getpdfncf
      end if

      return

end function getpdfncf


real function getpdfmnor(ndim,determinant,parms,invcov,rvals)

! -- Function getpdfmnor calculates the pdf value for a multi-normal distribution.

      implicit none

      integer, intent(in)     :: ndim                 ! number of dimensions
      real, intent(in)        :: determinant          ! determinant of covariance matrix
      real, intent(in), dimension(ndim+1)  :: parms   ! vector of parameters.
                                                      !    first element is dimension
                                                      !    then follow mean values
      real, intent(in), dimension(:,:)     :: invcov  ! inverse of covariance matrix
      real, intent(in), dimension(ndim)    :: rvals   ! vector of random numbers

      integer          :: i,j
      double precision :: rsum

      rsum=0.0d0
      do i=1,ndim
        do j=1,ndim
          rsum=rsum+(rvals(i)-parms(i+1))*(rvals(j)-parms(j+1))*invcov(i,j)
        end do
      end do
      rsum=-rsum*0.5d0-0.5d0*log(determinant)-0.5d0*ndim*log(2.0d0*pi)
      getpdfmnor=exp(rsum)

      return

end function getpdfmnor



subroutine casetrans(string,hi_or_lo)

! -- Subroutine casetrans converts a string to upper or lower case.

! -- Arguments are as follows:-
!      string:	  contains the string whose case must be changed
!      hi_or_lo:  must be either 'lo' or 'hi' to indicate
!                 change of case direction.

! -- Revision history:-
!       June-November, 1995: version 1.

	character (len=*), intent(inout)        :: string
	character (len=*), intent(in)           :: hi_or_lo
	character                               :: alo, ahi
	integer                                 :: inc,i

	if(hi_or_lo.eq.'lo') then
	  alo='A'; ahi='Z'; inc=iachar('a')-iachar('A')
	else if(hi_or_lo.eq.'hi') then
	  alo='a'; ahi='z'; inc=iachar('A')-iachar('a')
	endif

	do i=1,len_trim(string)
	  if((string(i:i).ge.alo).and.(string(i:i).le.ahi)) &
	  string(i:i)=achar(iachar(string(i:i))+inc)
	end do

	return

end subroutine casetrans


integer function nextunit()

! -- Function nextunit determines the lowest unit number available for
! -- opening.

! -- Revision history:-
!       June-November, 1995: version 1.

	logical::lopen

	do nextunit=10,100
	  inquire(unit=nextunit,opened=lopen)
	  if(.not.lopen) return
	end do
	write(6,10)
10      format(' *** No more unit numbers to open files ***')
	stop

end function nextunit


end module pdf

double precision function dloggamma(x)

       double precision :: x

       dloggamma=0.0d0
       write(6,10)
10     format(/,' Error: DLOGGAMMA function is not presently programmed.')
       stop

end


