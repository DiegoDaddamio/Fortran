! ---------------------------------
! µFortran auto generated program.
! Created for : Diego
! Date : Sun Oct 22 21:45:32 CEST 2023
! ---------------------------------
program main
    use mylib
    use checker
    implicit none
    integer, parameter :: size = 11
    integer, dimension(size,size) :: laby = -1
    integer :: i, j, x ,y, count = 0, dir = 1
    
    laby(2:size-1,2:size-1) = 0
    do i = 2, size-1, 2 
        do j = 2, size-1, 2
            dir = dir + 1
            laby(i,j) = dir
        end do
    end do
    x = 0
    y = x
    i = 1
    
    do while (.TRUE.)
        i = i +1
        x = randintloop(size-2,i) + 1
        i = i +1
        y = randintloop(size-2,i) + 1
        i = i +1
        if (laby(x,y) > 1) then
            laby(x,y) = 1
            do while (.TRUE.)
                dir = randintloop(4,i)
                select case (dir)
                case(1)
                    if (laby(x-1,y) == 0) then
                        laby(x-1,y) = 1
                        call check(laby,x-1,y)
                        exit
                    endif
                case(2)
                if (laby(x,y-1) == 0) then
                        laby(x,y-1) = 1
                        call check(laby,x,y-1)
                        exit
                    endif
                case(3)
                if (laby(x+1,y) == 0) then
                        laby(x+1,y) = 1
                        call check(laby,x+1,y)
                        exit
                    endif
                case(4)
                if (laby(x,y+1) == 0) then
                        laby(x,y+1) = 1
                        call check(laby,x,y+1)
                        exit
                    endif
                end select
                i = i +1
            enddo
        endif
        
        count = 0
        do i = 2, size-1
            do j = 2, size-1
                if (laby(i,j) > 1) then
                    count = count + 1
                endif
            end do
        end do
        if (count ==0) then
            exit
        endif 
    call ppmd(laby,"laby.txt")
    enddo
        
    
end program main