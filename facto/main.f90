! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Fri Sep 22 09:23:28 CEST 2023
! ---------------------------------
program main
    implicit none
    
    integer :: i, n
    real :: fact 
    read*, n
        if (n < 0) error stop 'factorial is singular for negative integers'
    fact = 1.0
    do i = 2,n
        fact = fact * i
    enddo
    print*, fact
end program main