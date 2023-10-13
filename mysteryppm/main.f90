! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Fri Oct 13 09:02:43 CEST 2023
! ---------------------------------
program main
    use random
    use mylib 
    implicit none
    integer, dimension(:,:), allocatable :: matrix
    integer :: m, n, mx, i, j
    integer(8) :: X=42, Y=13
    character(2) :: magic
    character(20) :: format_matrix
    
    open(unit = 1, file="mystery.ppm")
    read(1,"(A2)") magic
    read(1,*) m, n
    read(1,*) mx
    allocate (matrix(m,n))
    read(1,*) matrix
    close(1)
    
    ! call ppmd(matrix,"mystery.txt")
    call rand_seed(X, Y)
    
    iloop : do i = 1, m
        jloop : do j = 1, n
            matrix(i,j) = matrix(i,j) - mod(randi(),255)
        enddo jloop
    enddo iloop
    matrix = 255 - matrix
    write(*,*) max(matrix,dim=2)
    call ppmdd(matrix, 'demystery.ppm')
    deallocate (matrix)
end program main
