! ---------------------------------
! µFortran auto generated program.
! Created for : Diego
! Date : Sun Oct 08 18:37:26 CEST 2023
! ---------------------------------
program main
    use ppmdraw
    implicit none
    complex :: z
    integer :: k
    real :: i, j
    integer, parameter :: dim_matrix = 1000
    integer,dimension(dim_matrix+1,dim_matrix+1) :: matrix = 0
    
    iloop : do i = 0, dim_matrix
        jloop : do j = 0, dim_matrix
            z = cmplx(real(i)/dim_matrix,real(j)/dim_matrix)
            kloop : do k = 1, 1000
                z = z**2 + cmplx(-0.2,0.7)
            end do kloop
            if (isnan(sqrt(aimag(z)**2 +real(z)**2)) .eqv. .TRUE.) then
                matrix(i+1,j+1) =  1
            endif
        enddo jloop
    enddo iloop
    
    call ppmd(matrix,"draw.ppm")
    
end program main