module function_op
    use basic
    implicit none
    contains
    
    real function f(x) result(ans)
        real :: x
        
        ans = x
    end function
    
    real function df(x) result(ans)
        real :: x
        real, parameter :: h = 0.0001
        
        ans = (f(x+h)-f(x))/h
    end function
    
    real function integral_trap(a,b,step) result(ans)
        real :: a, b, step, i
        
        ans = 0
        do i = a+step, b-step, step
            ans = ans + f(i)
        end do
        ans = ans + (f(a) + f(b))/2
        ans = ans*step
    end function integral_trap
    
    real function integral_rect(a,b,step) result(ans)
        real :: a, b, step, i
        
        ans = 0
        do i = a, b-step, step
            ans = ans + (step*f(i))
        end do
    end function integral_rect
    
    real function integral_simp(a,b,step) result(ans)
        real :: a, b, step, i

        ans = 0
        do i = a+2*step, b-step, step*2
            ans = ans + 2*f(i)
        end do
        do i = a+step, b, step*2
            ans = ans + 4*f(i)
        end do
        ans = ans + f(a) + f(b) 
        ans = ans*step/3
    end function integral_simp
    
end module