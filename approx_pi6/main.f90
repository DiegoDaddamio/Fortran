! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Tue Sep 26 11:13:28 CEST 2023
! ---------------------------------
program main
    implicit none
    real :: sum
    integer :: i
    sum = 0
    
    do i =1,1000000000
        sum = sum + 1.0/ (real(i)**2)
    enddo
    
    print*, sum
    
end program main
