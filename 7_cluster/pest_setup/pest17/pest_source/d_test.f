      program d_test
      character d_str*23, e_str*23

      write(e_str,'(1pe23.15e3)',err=9000) 1.0E10

C -- If your compiler produces an error on the following line, then
C -- you cannot use the USE_D_FORMAT specifier when compiling PEST

      write(d_str,'(1pd23.15d3)',err=9000) 1.0E10
      write(*,'(1x,2a)') 'Using E format : ', e_str
      write(*,'(1x,2a)') 'Using D format : ', d_str
      if (d_str(20:23) .ne. e_str(20:23)) write(*,'(1x,a)')
     +  'D format fails test; do not define USE_D_FORMAT in Makefile'
      stop

9000  write(*,9010)
9010  format(' Error writing in D format.')
      write(*,9020)
9020  format(' Do not define USE_D_FORMAT in makefile.')

      end
