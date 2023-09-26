! ---------------------------------
! µFortran auto generated program.
! Created for : Diego
! Date : Tue Sep 26 17:16:42 CEST 2023
! ---------------------------------
program main
    implicit none
    integer :: random_number, i, max_, size
    integer,dimension(8) :: data_time ! read the time of the computer
    max_ = 101 ! 101 not include so [0,100]
    size = 10 ! number of random integer
    
    do i=1, size ! how many random integer that we will have
    
        call date_and_time(VALUES=data_time) ! read the time in the computer
    
        random_number = int(data_time(8)) ! select the ms
    
        random_number = mod(random_number*32457*i,567) ! try to randomise with the module
        random_number = mod(random_number*25546*i,249) ! try to randomise with the module
        random_number = mod(random_number*65477*i,max_) ! return it from 0 to the max_
        
        print*, random_number
    
    enddo

end program main