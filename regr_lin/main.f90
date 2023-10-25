! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Tue Oct 24 11:50:54 CEST 2023
! ---------------------------------
program main
    use mylib
    implicit none
    
    real (kind=8) :: a, b, Sy = 0, Sxy = 0, Sx = 0, Sxx = 0, S, D
    integer, parameter :: nbr_pts = 5
    real, dimension(nbr_pts) :: h = 1
    integer, dimension(:,:), allocatable :: pts
    integer :: i
    
    call txtmatrix(pts,"data.txt")
    
    do i = 1, nbr_pts
        Sy = Sy + (pts(i,2)/(h(i)**2))
        Sx = Sx + (pts(i,1)/(h(i)**2))
        Sxy = Sxy + (pts(i,1)*pts(i,2)/(h(i)**2))
        Sxx = Sxx + (pts(i,1)**2/(h(i)**2))
        S = S + (1/(h(i)**2))
    enddo
    
    D = Sx**2 - S*Sxx
    a = (Sx*Sy - S*Sxy)/ D
    b = (Sx*Sxy - Sxx*Sy)/ D
    
    write(*,*) a, b
    deallocate (pts)
end program main
