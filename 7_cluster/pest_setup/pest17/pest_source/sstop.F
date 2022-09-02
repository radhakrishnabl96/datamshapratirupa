
	subroutine sstop

	implicit none

        go to 100
	write(6,*)
	write(6,10)
10	format(10x,61('*'))
	write(6,15)
15	format(10x,'*',t71,'*')
	write(6,20)
20	format(10x,'*  PAGENT has terminated execution.',t71,'*')
	write(6,30)
30	format(10x,'*  Please close this window yourself',
     +  ' by clicking ',t71,'*')
	write(6,40)
40	format(10x,'*  on the "close" button at top right of the ',
     +  'window.',t71,'*')
	write(6,15)
	write(6,10)

100     stop
	return
	end
