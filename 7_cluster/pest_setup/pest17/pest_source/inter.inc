MODULE INTER

! -- Contains interface blocks for all subprograms.


!******************************************************************************
! generic subprograms
!******************************************************************************

interface char2num

	subroutine a2i(ifail,string,num)
	  integer, intent(out)          :: ifail
	  character (len=*), intent(in) :: string
	  integer, intent(out)          :: num
	end subroutine a2i
	subroutine a2l(ifail,string,num)
	  integer, intent(out)          :: ifail
	  character (len=*), intent(in) :: string
	  logical, intent(out)          :: num
	end subroutine a2l
	subroutine a2r(ifail,string,num)
	  integer, intent(out)          :: ifail
	  character (len=*), intent(in) :: string
	  real, intent(out)             :: num
	end subroutine a2r
	subroutine a2d(ifail,string,num)
	  integer, intent(out)          :: ifail
	  character (len=*), intent(in) :: string
	  double precision, intent(out) :: num
	end subroutine a2d

end interface


interface num2char

	subroutine i2a(value,string,nchar)
	  integer, intent(in)           :: value
	  character (len=*), intent(out):: string
	  integer, intent(in), optional :: nchar
	end subroutine i2a
	subroutine r2a(value,string,nchar)
	  real, intent(in)              :: value
	  character (len=*), intent(out):: string
	  integer, intent(in), optional :: nchar
	end subroutine r2a
	subroutine d2a(value,string,nchar)
	  double precision, intent(in)  :: value
	  character (len=*), intent(out):: string
	  integer, intent(in), optional :: nchar
	end subroutine d2a

end interface


interface pos_test

	integer function pos_i_test(value,string)
	  integer, intent(in)           :: value
	  character (len=*), intent(in) :: string
	end function pos_i_test
	integer function pos_r_test(value,string)
	  real, intent(in)              :: value
	  character (len=*), intent(in) :: string
	end function pos_r_test
	integer function pos_d_test(value,string)
	  double precision, intent(in)  :: value
	  character (len=*), intent(in) :: string
	end function pos_d_test

end interface


interface nneg_test

	integer function nneg_i_test(value,string)
	  integer, intent(in)           :: value
	  character (len=*), intent(in) :: string
	end function nneg_i_test
	integer function nneg_r_test(value,string)
	  real, intent(in)              :: value
	  character (len=*), intent(in) :: string
	end function nneg_r_test
	integer function nneg_d_test(value,string)
	  double precision, intent(in)  :: value
	  character (len=*), intent(in) :: string
	end function nneg_d_test

end interface


interface key_read

	integer function int_key_read(value)
	  integer,intent(out)   :: value
	end function int_key_read
	integer function real_key_read(value)
	  real,intent(out)      :: value
	end function real_key_read
	integer function double_key_read(value)
	  double precision,intent(out)  :: value
	end function double_key_read

end interface


!******************************************************************************
! other subprograms
!******************************************************************************

! utility subprograms ------->

interface

	subroutine casetrans(string,hi_or_lo)
	  character (len=*), intent(inout)        :: string
	  character (len=*), intent(in)           :: hi_or_lo
	end subroutine casetrans

	subroutine sub_error(subname)
	  character (len=*)               ::subname
	end subroutine sub_error

	integer function nextunit()
	end function nextunit

	subroutine close_files
	end subroutine close_files

	subroutine open_input_file(ifail,aprompt,infile,inunit,file_format)
          integer, intent(out)                    :: ifail
          character (len=*), intent(in)           :: aprompt
          character (len=*), intent(out)          :: infile
          integer, intent(out)                    :: inunit
          character (len=*), intent(in), optional :: file_format
	end subroutine open_input_file


        subroutine open_named_input_file(ifail,aprompt,infile,inunit)
          integer, intent(out)                    :: ifail
          character (len=*), intent(in)           :: aprompt
          character (len=*), intent(inout)        :: infile
          integer, intent(out)                    :: inunit
        end subroutine open_named_input_file


	subroutine open_output_file(ifail,aprompt,outfile,outunit)
	  integer, intent(out)          :: ifail
	  character (len=*)             :: aprompt,outfile
	  integer, intent(out)          :: outunit
	end subroutine open_output_file

	subroutine readfig(specfile,coordfile,sampfile,pumpfile,pilotfile)
	  character (len=*), intent(out)                :: specfile
	  character (len=*), intent(out), optional      :: coordfile,sampfile,&
							   pumpfile,pilotfile
	end subroutine readfig

	subroutine read_settings(ifail,idate)
	  integer, intent(out)	:: ifail,idate
	end subroutine read_settings

	subroutine char_add(astring,achar)
          character (len=*), intent(inout)        :: astring
          character (len=*), intent(in)           :: achar
	end subroutine char_add

	subroutine int2alph(inum,alph,nsig)
	  integer, intent(in)			:: inum
	  character (len=*), intent(out)	:: alph
	  integer, optional, intent(in)		:: nsig
	end subroutine int2alph

        logical function isspace(astring)
          character (len=*), intent(in)   :: astring
        end function isspace

end interface


! reading-a-file subprograms ------->

interface

	subroutine linesplit(ifail,num)
	  integer, intent(out)            :: ifail
	  integer, intent(in)		  :: num
	end subroutine linesplit

	integer function char2int(ifail,num)
	  integer, intent(in)             :: num
	  integer, intent(out)            :: ifail
	end function char2int

	real function char2real(ifail,num)
	  integer, intent(in)             :: num
	  integer, intent(out)            :: ifail
	end function char2real

	double precision function char2double(ifail,num)
	  integer, intent(in)             :: num
	  integer, intent(out)            :: ifail
	end function char2double

        subroutine getfile(ifail,cline,filename,ibeg,iend)
          integer, intent(out)            :: ifail
          integer, intent(in)             :: ibeg
          integer, intent(inout)          :: iend
          character (len=*), intent(in)   :: cline
          character (len=*), intent(out)  :: filename
       end subroutine getfile

       subroutine addquote(afile,aqfile)
          character (len=*), intent(in)   :: afile
          character (len=*), intent(out)  :: aqfile
       end subroutine addquote

end interface


! message subprograms ------->

interface

	subroutine write_initial_message(leadspace,endspace)
	  character (len=*), intent(in), optional :: leadspace,endspace
	end subroutine write_initial_message

	subroutine write_message(increment,iunit,error,leadspace,endspace)
	  integer, intent(in), optional           ::increment,iunit
	  character (len=*), intent(in), optional ::error,leadspace,endspace
	end subroutine write_message

end interface


! site data manipulation subprograms ------->

interface

	subroutine read_rest_of_sample_line(ifail,cols,ndays,nsecs,value, &
	iline,sampfile)
          integer, intent(out)            :: ifail
          integer, intent(in)             :: cols
          integer, intent(out)            :: ndays,nsecs
          double precision, intent(out)   :: value
          integer, intent(in)             :: iline
          character (len=*), intent(in)   :: sampfile
	end subroutine read_rest_of_sample_line

	subroutine time_interp(ifail,nbore,ndays,nsecs,value,intday, &
	intsec,rnear,rconst,valinterp,extrap,direction,startindex)
          integer, intent(out)                    :: ifail
          integer, intent(in)                     :: nbore
          integer, intent(in), dimension(nbore)   :: ndays,nsecs
          double precision, intent(in), dimension(nbore)   :: value
          integer, intent(in)                     :: intday,intsec
	  real, intent(in)			  :: rnear,rconst
          double precision, intent(out)           :: valinterp
	  character (len=*), intent(in),optional  :: extrap
	  character (len=*), intent(in),optional  :: direction
          integer, intent(inout), optional        :: startindex
	end subroutine time_interp

	subroutine get_num_ids(ifail,iunit,afile,numid,maxsamp,ignore_x)
	  integer, intent(out)                    :: ifail
          integer, intent(in)                     :: iunit
          character (len=*), intent(in)           :: afile
          integer, intent(out)                    :: numid,maxsamp
	  character (len=*), intent(in), optional :: ignore_x
	end subroutine get_num_ids

	subroutine get_ids_and_interval(ifail,iunit,afile,nid,aid,ndays1, &
                                nsecs1,ndays2,nsecs2, ignore_x)
          integer, intent(out)                    :: ifail
          integer, intent(in)                     :: iunit
          character (len=*), intent(in)           :: afile
          integer, intent(in)                     :: nid
          character (len=*), intent(out)          :: aid(nid)
          integer, intent(out)                    :: ndays1(nid),nsecs1(nid), &
                                                     ndays2(nid),nsecs2(nid)
	  character (len=*), intent(in), optional :: ignore_x
	end subroutine get_ids_and_interval

        subroutine volume_interp(ifail,num,days,secs,flows,bdays,bsecs,  &
          fdays,fsecs,vol,fac)

          integer, intent(out)            :: ifail
          integer, intent(in)             :: num
          integer, intent(in)             :: days(num),secs(num)
          double precision, intent(in)    :: flows(num)
          integer, intent(in)             :: bdays,bsecs,fdays,fsecs
          double precision, intent(out)   :: vol
          double precision, intent(in)    :: fac
        end subroutine volume_interp


end interface


! date manipulation subprograms ------->

interface

	subroutine char2date(ifail,adate,dd,mm,yy)
          integer, intent(out)    	:: ifail
          character (len=*), intent(in) :: adate
          integer, intent(out) 		:: dd,mm,yy
	end subroutine char2date

	subroutine datestring(dd,mm,yy,hhh,mmm,sss,time,at,adate,atime)
	  integer, intent(in)             :: dd,mm,yy,hhh,mmm,sss
	  real, intent(in)                :: time
	  character (len=1), intent(in)   :: at
	  character (len=*), intent(out)  :: adate, atime
	end subroutine datestring

	logical function leap(year)
          integer, intent(in)     :: year
	end function leap

	integer function numdays(dr,mr,yr,d,m,y)
          integer, intent(in)     :: dr,mr,yr,d,m,y
	end function numdays

	integer function numsecs(h1,m1,s1,h2,m2,s2)
	  integer, intent(in)     :: h1,m1,s1,h2,m2,s2
	end function numsecs

	subroutine char2time(ifail,adate,hh,mm,ss,ignore_24)
          integer, intent(out)    	:: ifail
          character (len=*), intent(in) :: adate
          integer, intent(out) 		:: hh,mm,ss
          integer, optional,intent(in)  :: ignore_24
	end subroutine char2time

	subroutine time2char(ifail,hh,mm,ss,atime)
	  integer, intent(out)            :: ifail
	  integer, intent(in)             :: hh,mm,ss
	  character (len=*), intent(out)  :: atime
	end subroutine time2char

	subroutine elapsdate(eltime,dayfactor,day1,mon1,year1,hour1,min1,sec1,&
	  day2,mon2,year2,hour2,min2,sec2)
	  real, intent(in)		:: eltime,dayfactor
	  integer, intent(in)		:: day1,mon1,year1,hour1,min1,sec1
	  integer, intent(out)		:: day2,mon2,year2,hour2,min2,sec2
	end subroutine elapsdate

	subroutine newdate(ndays,day1,mon1,year1,day2,mon2,year2)
	  integer, intent(in)		:: ndays,day1,mon1,year1
	  integer, intent(out)		:: day2,mon2,year2
	end subroutine newdate

	subroutine sectime(nsecs,sec,min,hour)
	  integer, intent(in)   :: nsecs
	  integer, intent(out)  :: sec,min,hour
	end subroutine sectime

end interface


END MODULE INTER
