module utility

!variables for reading a file ------->

	integer, parameter                      :: NUM_WORD_DIM=100
	integer, dimension(NUM_WORD_DIM)        :: left_word,right_word
	character (len=300)                     :: cline


!variables for writing a message ------->
	
	integer                 :: imessage=0
	character (len=500)     :: amessage= ' '
	character (len=200)     :: initial_message=' '


!escape variables ------->

	integer                 :: escset=0
	character (len=5)       :: eschar = 'E ~e '


!variables recording data settings ------>

	integer				:: datespec
 
end module utility   
