! ---------------------------------
! µFortran auto generated program.
! Created for : Diego
! Date : Tue Sep 26 17:16:42 CEST 2023
! ---------------------------------
program main
    implicit none
    integer :: random_number, i, max_
    integer,dimension(8) :: data_time
    
    size = 10 ! number of random integer
    
    do i=1, size ! how many random integer that we will have
    
        call date_and_time(VALUES=data_time) ! read the time in the computer
    
        random_number = int(data_time(8))
    
        random_number = mod(random_number*32457*i,567)
        random_number = mod(random_number*25546*i,249)
        random_number = mod(random_number*65477*i,max_)
        
        print*, random_number
    
    enddo

end program main