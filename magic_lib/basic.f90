module basic
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
    
end module