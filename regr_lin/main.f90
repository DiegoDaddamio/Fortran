! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Tue Oct 24 11:50:54 CEST 2023
! ---------------------------------
program main
    implicit none
    
    real (kind=8) :: a, b, Sy = 0, Sxy = 0, Sy = 0, Sxx = 0, S, D
    integer :: nbr_pts = 10
    real, dimension(nbr_pts) :: µ = 1
    real, dimension(10,2) :: pts
    
    do i = 1, nbr_pts
        Sy = Sy + (pts(i,2)/(µ(i)**2))
        Sx = Sx + (pts(i,1)/(µ(i)**2))
        Sxy = Sxy + (pts(i,1)*pts(i,2)/(µ(i)**2))
        Sxx = Sxx + (pts(i,1)**2/(µ(i)**2))
        S = S + (1/(µ(i)**2))
    enddo
    
    D = Sx**2 - S*Sxx
    
    a = (Sx*Sy - S*Sxy)/ D
    b = (Sx*Sxy - Sxx*Sy)/ D
    
end program main
