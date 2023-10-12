! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Thu Oct 12 16:13:33 CEST 2023
! ---------------------------------
program main
! tester library 
    use mylib
    implicit none
    integer :: i
    do i = 1,10
        write(*,*) int(randintloop(10,i))
    enddo
end program main
