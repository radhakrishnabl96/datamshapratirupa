        program stoppest

#ifdef UNIX
        open(unit=10,file='pest.stp')
#else
#ifdef LF90
        open(unit=10,file='pest.stp',action='readwrite,denynone')
#else
#ifdef INTEL
        open(unit=10,file='pest.stp',shared)
#else
        open(unit=10,file='pest.stp')
#endif
#endif
#endif
        write(10,'(i2)') 1
        write(6,20)
20      format(/,' Integer "1" written to file pest.stp',/)

        end
