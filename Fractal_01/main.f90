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
    integer, parameter :: dim_matrix = 30 ! taille de l'image
    real, parameter :: prec = 0.2 ! Precision
    real, parameter :: inv_prec = 1/prec
    integer,dimension(dim_matrix*2*int(inv_prec)+1,dim_matrix*2*int(inv_prec)+1) :: matrix = 0

    iloop : do i = -dim_matrix, dim_matrix, prec
        jloop : do j = -dim_matrix, dim_matrix, prec
            z = cmplx(real(i)/dim_matrix,real(j)/dim_matrix)
            kloop : do k = 1, 1000
                z = z**2 + cmplx(-0.2,0.7) ! {cmplx(-0.2,0.7),-1,-1.75,...}
            end do kloop
            if (isnan(sqrt(aimag(z)**2 +real(z)**2)) .eqv. .TRUE.) then
                matrix(i*int(inv_prec)+dim_matrix*int(inv_prec)+1,j*int(inv_prec)+dim_matrix*int(inv_prec)+1) =  1
            endif
        enddo jloop
    enddo iloop
    
    call ppmd(matrix,"draw.ppm")

end program main
