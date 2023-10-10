! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Tue Oct 10 10:43:29 CEST 2023
! ---------------------------------
program main
    implicit none
    real, dimension(:,:), allocatable :: r, s, c ! pointeur
    integer :: i,j,k,matrix_size
    integer, dimension(12) :: seed
    character(200) :: format_matrix
    
    print*, "Choice your matrix size :"
    read*, matrix_size
    
    allocate(r(matrix_size,matrix_size),s(matrix_size,matrix_size),c(matrix_size,matrix_size))
    
    call date_and_time(VALUES = seed)
    call random_seed(put=seed)
    call random_number(r)
    call random_number(s)
    
    ! matmul(A,B)
    iloop : do i = 1, matrix_size
        jloop : do j = 1, matrix_size
            c(i,j) = dot_product(r(i,:),s(:,j))
        enddo jloop
    enddo iloop
    
    
    write(format_matrix,"(A,I0,A,I0,A)") "(",matrix_size,"(",matrix_size,"(F6.2,1x),/))"
    write(*,format_matrix) r
    write(*,format_matrix) s
    write(*,format_matrix) c
    
    deallocate(r,s,c)
end program main