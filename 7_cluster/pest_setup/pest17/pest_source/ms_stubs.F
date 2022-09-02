        module modsub
          integer ms_npar
          integer ms_nobs
        end module modsub

        subroutine modsub_flag(ifail,ms_status,errmsg)
          use modsub
          implicit none
          integer ifail,ms_status
          character*(*) errmsg
          ifail=0
          ms_status=0
          return
        end


        subroutine modsub_init(ifail,npar,nobs,apar,itrans,aobs,errmsg)
          use modsub
          implicit none
          integer ifail,npar,nobs
          character*(*) apar(npar),aobs(nobs)
          integer itrans(npar)
          character*(*) errmsg

          ms_npar=npar
          ms_nobs=nobs

          ifail=0
          return
        end


        subroutine modsub_mrun(ifail,pval,scale,offset,oval,errmsg)
          use modsub
          implicit none
          integer ifail
          double precision pval(ms_npar),scale(ms_npar),offset(ms_npar)
          double precision oval(ms_nobs)
          character*(*) errmsg

          ifail=0
          return
        end

        subroutine modsub_drun(ifail,ipp,pval,scale,offset,dervec,
     +  errmsg)
          use modsub
          implicit none
          integer ifail,ipp
          double precision pval(ms_npar),scale(ms_npar),offset(ms_npar)
          double precision dervec(ms_nobs)
          character*(*) errmsg

          ifail=0
          return
        end

        subroutine modsub_fin()
          return
        end
