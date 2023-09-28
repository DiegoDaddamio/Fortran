! ---------------------------------
! µFortran auto generated program.
! Created for : Diego
! Date : Thu Sep 28 18:14:45 CEST 2023
! ---------------------------------
program main
    implicit none
    
    integer :: i
    real, parameter :: pi = 3.14159265359
    real, dimension(101,2) :: points
    points(:,1) = (/(6*pi*(i/100.0),i=0,100)/)
    points(:,2) = (/(sin(points(i,1)),i=1,101)/)
    
    do i=1, 101
        write(*,*) points(i,1), points(i,2)
    enddo
    
end program main