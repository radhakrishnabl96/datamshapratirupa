
	subroutine pestwait(nsec)

C -- Subroutine PESTWAIT causes PEST to "mark time" for nsec hundredths of a second.

	implicit none

#ifdef INTEL
        integer nsec,msec
        msec=nsec*10
        if(msec.lt.1)msec=1
        call sleepqq(msec)
#else
#ifdef SLEEP
        integer nsec,msec
        msec=nsec/100
        if(msec.lt.1)msec=1
        call sleep(msec)
#else
#ifdef BEO
	integer nsec
        call msleep(nsec)
#else
	integer ddate(8),iticks,iticks1,nsec

	call date_and_time(values=ddate)
	iticks=ddate(5)*360000+ddate(6)*6000+ddate(7)*100+ddate(8)/10
10	call date_and_time(values=ddate)
	iticks1=ddate(5)*360000+ddate(6)*6000+ddate(7)*100+ddate(8)/10
	if(iticks1.lt.iticks) iticks1=iticks1+8640000
	if(iticks1.lt.iticks+nsec) go to 10
#endif
#endif
#endif

	return
#ifdef PESTMOD
        end subroutine pestwait
#else
	end
#endif	

