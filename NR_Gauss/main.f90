! ---------------------------------
! µFortran auto generated program.
! Created for : Diego
! Date : Thu Nov 30 16:46:38 CET 2023
! ---------------------------------
program main
    use libRNG
    implicit none
    real, allocatable, dimension(:) :: x
    real, allocatable, dimension(:,:) :: xy
    real :: moyenne, ec_type
    
    allocate(x(size_vector("gauss.txt")))
    x = import_vector("gauss.txt",size_vector("gauss.txt"))
    
    moyenne = average(x)
    ec_type = sigma(x)
    
    allocate (xy(int(maxval(x))-int(minval(x))+1,2))
    call rise_y(x,xy(:,1),xy(:,2))
    call N_R(xy, moyenne, ec_type, 1900.)
    deallocate(x,xy)
end program main
