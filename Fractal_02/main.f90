! ---------------------------------
! µFortran auto generated program.
! Created for : Diego
! Date : Sun Oct 08 18:37:26 CEST 2023
! ---------------------------------
program main
    use mylib
    implicit none
    complex :: z
    integer :: k
    real :: i, j
    integer, parameter :: norm = 1
    real, parameter :: prec = 0.02
    integer, parameter :: inv_prec = int(1/prec)
    integer,dimension(int(1/norm)*2*inv_prec,int(1/norm)*2*inv_prec) :: matrix = 0

    iloop : do i = -norm, norm, prec
        jloop : do j = -norm, norm, prec
            z = cmplx(real(i),real(j))
            kloop : do k = 0, 255
                z = z**2 + cmplx(0.285,0.01) ! {cmplx(-0.2,0.7),cmplx(0.285,0.013),-1,-1.75,...}
                if (isnan(aimag(z)**2 +real(z)**2) .eqv. .TRUE.) then
                    matrix(i*inv_prec+int(1/norm)*inv_prec+1,j*inv_prec+int(1/norm)*inv_prec+1) = 255 - k
                exit
                endif
            end do kloop
        enddo jloop
    enddo iloop
    
    call ppmd(matrix,"draw.ppm")

end program main