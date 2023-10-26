! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Tue Oct 24 11:50:54 CEST 2023
! ---------------------------------
program main
    use mylib
    implicit none
    
    real :: a, b, Sy = 0, Sxy = 0, Sx = 0, Sxx = 0, S, D
    integer :: nbr_pts
    real, dimension(:), allocatable :: h
    real, dimension(:,:), allocatable :: pts
    integer :: i
    
    call txtdata(pts,"data.txt")
    
    nbr_pts = size(pts,1)
    allocate (h(nbr_pts))
    h = 1
    
    call reg_op(pts,h)
    
    
    deallocate (pts,h)
end program main
