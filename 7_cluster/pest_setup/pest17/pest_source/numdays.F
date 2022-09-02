
	integer function numdays(DR,MR,YR,D,M,Y)

	implicit none

C -- Function numdays calculates the number of days between dates
C    D-M-Y and DR-MR-YR. If the former preceeds the latter the answer is
C    negative.

C -- Arguments are as follows:-
C       dr,mr,yr:     days, months and years of first date
C       d,m,y:        days, months and years of second date
C       numdays returns the number of elapsed days

	integer dr,mr,yr,d,m,y

	INTEGER FLAG,I,J,DA(12),YE,ME,DE,YL,ML,DL
	logical leap

	DATA DA /31,28,31,30,31,30,31,31,30,31,30,31/

C --    THE SMALLER OF THE TWO DATES IS NOW CHOSEN TO DO THE COUNTING FROM.

	IF(Y.LT.YR)GO TO 10
	IF((Y.EQ.YR).AND.(M.LT.MR)) GO TO 10
	IF((Y.EQ.YR).AND.(M.EQ.MR).AND.(D.LT.DR)) GO TO 10
	FLAG=0
	YE=YR
	ME=MR
	DE=DR
	YL=Y
	ML=M
	DL=D
	GO TO 20
10      FLAG=1
	YE=Y
	ME=M
	DE=D
	YL=YR
	ML=MR
	DL=DR

C --    IN THE ABOVE THE POSTSCRIPT "E" STANDS FOR EARLIER DATE, WHILE
C       "L" STANDS FOR THE LATER DATE.

20      numdays=0
	IF((ME.EQ.ML).AND.(YL.EQ.YE))THEN
	numdays=DL-DE
	IF(FLAG.EQ.1) numdays=-numdays
	RETURN
	END IF

	DO 30 J=ME,12
	IF((ML.EQ.J).AND.(YE.EQ.YL))GOTO 40
	numdays=numdays+DA(J)
	IF((J.EQ.2).AND.(leap(ye)))numdays=numdays+1
30      CONTINUE
	GO TO 50
40      numdays=numdays+DL-DE
	IF(FLAG.EQ.1)numdays=-numdays
	RETURN

50      DO 60 I=YE+1,YL
	DO 70 J=1,12
	IF((YL.EQ.I).AND.(ML.EQ.J))GO TO 80
	numdays=numdays+DA(J)
	IF((J.EQ.2).AND.(leap(i))) numdays=numdays+1
70      CONTINUE
60      CONTINUE
c	write(6,65)
c65	format(/,' Error in subroutine NUMDAYS')
c	call pstop
	RETURN

80      numdays=numdays+DL-DE
	IF(FLAG.EQ.1) numdays=-numdays

	RETURN
	end


        logical function leap(year)

C -- Function LEAP returns .true. if a year is a leap year.

        integer year

        leap = ( mod(year,4).eq.0 .and. mod(year,100).ne.0 ) .or.
     +               ( mod(year,400).eq.0 .and. year.ne.0 )

        return
        end
