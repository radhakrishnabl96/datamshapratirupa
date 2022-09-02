        program unpausepest

#ifdef UNIX
        open(unit=10,file='pest.stp')
#else
#ifdef LF90
        open(unit=10,file='pest.stp',action='readwrite,denynone')
#else
        open(unit=10,file='pest.stp')
#endif
#endif        
        write(10,'(i2)') 0
        write(6,20)
20      format(/,' Integer "0" written to file pest.stp',/)

        end
