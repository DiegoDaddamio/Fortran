! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Tue Sep 26 12:06:23 CEST 2023
! ---------------------------------
program main
    implicit none
    integer :: mystery, guess
    integer,dimension(8) :: data_time
    
    call date_and_time(VALUES=data_time)
    
    mystery = int(data_time(8))
     
    mystery = mod(mystery*32457,101)
    
    guess = 101
    
    
        do while(guess<0 .or. guess>100)
            print*, "Entrez un nombre entre 0 et 100 : "
            read*, guess
        enddo
    
        if guess == mystery then
            print*, "Bravo !"
        elseif guess > mystery then
            print*, 
        elseif guess < mystery then
            print*, 
        endif
          
    
    
    print*, mystery
   
end program main