module basic
    use random
    contains
    
    integer function facto(x) result(ans)
        integer :: x, i
        ans = 1
        if (x < 0) error stop 'erreur domaine'
            do i = 1, x
                ans = ans * i
            end do
    end function
    
    real function log_(a,x) result(ans)
        real :: x, a
    
        if (x < 0) error stop 'erreur domaine'
        if (a < 0) error stop 'erreur base'
        ans = log(x)/log(a)
    end function
    
    function linspace(a,b,split) result(ans)
        real :: a, b, step, j
        integer :: split, i
        real :: ans(split)
        
        ans = 0
        
        if (b <= a) error stop 'erreur bornes'
        if (split <= 1) error stop 'erreur séparation'
        ans(1) = a
        ans(split) = b
        
        do i = 2, split-1
            j = (real(i) - 1)/real(split-1)
            ans(i) = (1-j)*a + j*b
        enddo
    end function
    
    logical function detect_real_matrix(matrix) result(ans)
        real :: matrix(:,:)
        integer :: i, j
        
        ans = .false.
        if ( (ceiling(sum(matrix)) .eq. floor(sum(matrix))) .eqv. .false. ) then
            ans = .true.
        else
            do i = 1 , size(matrix,1)
                do j = 1, size(matrix,2)
                    if ( (ceiling(matrix(i,j)) .eq. floor(matrix(i,j)) ) .eqv. .false.) then
                        ans = .true.
                        exit 
                    endif
                enddo
            enddo
        endif
    end function
    
    logical function detect_real(x) result(ans)
        real :: x
        if ( (ceiling(x) .eq. floor(x)) .eqv. .false. ) then
            ans = .true.
        else
            ans = .false.
        endif
    end function
    
    real function higher(u,v) result(ans)
        real :: u,v
        if (u>v) then
            ans = u
        else
            ans = v
    end if
    end function
    
    real function lower(u,v) result(ans)
        real :: u,v
        if (u<v) then
            ans = u
        else
            ans = v
    end if
    
    end function
    
    subroutine choose(matrix)
        real :: matrix(:,:)
        integer :: x,y, choice
        
        x = mod(randi(),size(matrix,1)-2) + 2
        y = mod(randi(),size(matrix,1)-2) + 2
        
        if (matrix(x,y) .eq. 0)then
        choice = mod(randi(),2)
            select case (choice)
                case (1)
                    if((matrix(x+1,y) .ne. matrix(x-1,y)) .and. (matrix(x+1,y) .ne. 0) .and. (0 .ne. matrix(x-1,y)) )then
                        matrix(x,y) = higher(matrix(x-1,y), matrix(x+1,y))
                        call class(matrix,lower(matrix(x-1,y), matrix(x+1,y)),higher(matrix(x-1,y), matrix(x+1,y)))
                    endif
                case(0)
                     if((matrix(x,y+1) .ne. matrix(x,y-1)) .and. (matrix(x,y+1) .ne. 0) .and. (0 .ne. matrix(x,y-1)) )then
                        matrix(x,y) = higher(matrix(x,y-1), matrix(x,y+1))
                        call class(matrix,lower(matrix(x,y-1), matrix(x,y+1)),higher(matrix(x,y-1), matrix(x,y+1)))
                    endif
                end select
        endif
    
    end subroutine
    
    subroutine class(matrix,ko,kn)
        real :: matrix(:,:)
        real :: ko,kn
        integer :: i,j
        
        do i = 1, size(matrix,1)
            do j = 1, size(matrix,1)
                if (matrix(i,j) == ko) then
                    matrix (i,j) = kn
                endif
            enddo
        enddo
        
    end subroutine
    
end module