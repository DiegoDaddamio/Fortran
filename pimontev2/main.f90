! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Thu Oct 05 16:30:08 CEST 2023
! ---------------------------------
program main
    implicit none
    integer, parameter :: N = 1000
    real(8), dimension(N,2) :: xy
    integer, dimension(12) :: seed
    call date_and_time(VALUES =seed)
    call random_seed(put=seed)
    
    call random_number(xy)
    
    print*, 4.0 * count(sum(xy**2,dim=2) <=1) / real(N)
end program main
