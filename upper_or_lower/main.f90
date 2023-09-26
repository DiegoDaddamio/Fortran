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
    random_number = mod(random_number*32457,567)
    random_number = mod(random_number*25546,249)
    random_number = mod(random_number*65477,101)
    guess = 101

    do while (guess .ne. mystery)

        print*, "Pick a number from 0 to 100 : "
        read*, guess
        
        if (guess<0 .and. guess>100)then
            cycle
        endif
        if (guess == mystery) then
            print*, "Congratulation !"
            call sleep(5)
            stop
        elseif (guess > mystery) then
            print*, "Lower !"
        elseif (guess < mystery) then
        print*, "Higher !"
        endif
    end do 

end program main
