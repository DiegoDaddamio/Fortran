module checker
contains

recursive subroutine check(matrix,x,y)
    integer :: matrix(:,:)
    integer :: x, y, number, i
    number = matrix(x,y)
    
    if ((matrix(x+1,y) > 0) .and.(matrix(x+1,y) .ne. number)) then
        matrix(x+1,y) = number
        call check(matrix,x+1,y)
    endif 
    if ((matrix(x,y+1) > 0) .and.(matrix(x,y+1) .ne. number)) then
        matrix(x,y+1) = number
        call check(matrix,x,y+1)
    endif 
    if ((matrix(x-1,y) > 0) .and.(matrix(x-1,y) .ne. number)) then
        matrix(x-1,y) = number
        call check(matrix,x-1,y)
    endif 
    if ((matrix(x,y-1) > 0) .and.(matrix(x,y-1) .ne. number)) then
        matrix(x,y-1) = number
        call check(matrix,x,y-1)
    endif 
end subroutine check

end module