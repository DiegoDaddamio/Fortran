module points_op
    use cst
    implicit none
    contains
    
    real function reg_lin(pts,h) result(ans)
        real :: pts(:,:), h(:)
        real :: a, b, Sy = 0, Sxy = 0, Sx = 0, Sxx = 0, S, D
        integer :: i
        
        do i = 1, size(pts,1)
            Sy = Sy + (pts(i,2)/(h(i)**2))
            Sx = Sx + (pts(i,1)/(h(i)**2))
            Sxy = Sxy + (pts(i,1)*pts(i,2)/(h(i)**2))
            Sxx = Sxx + (pts(i,1)**2/(h(i)**2))
            S = S + (1/(h(i)**2))
        enddo
        
        D = Sx**2 - S*Sxx
        a = (Sx*Sy - S*Sxy)/ D
        b = (Sx*Sxy - Sxx*Sy)/ D
        
        if (b == -0)then
            b = 0.0
        endif
        write(*,"(A,f6.4,A,f6.4)") "Régression linéaire : f(x) = ",a," x +",b
    end function
    
    real function average(vector) result(ans)
        integer :: i
        real, dimension(:) :: vector
        ans = 0
        !Boucle d'addition
        do i = 1, size(vector)
            ans = ans + vector(i)
        enddo
        !Division finale par le nombre total d'élément
        ans = ans/size(vector)
    end function average
    
    real function sigma(vector) result(ans)
        integer :: i, n
        real, dimension(:) :: vector
        real :: average_vector
        ! traduction de la formule
        n = size(vector)
        average_vector = average(vector)
        ! somme de 1 à n du carré de la différence entre le coup i et la moyenne
        do i =1, n
            ans = ans + (vector(i) - average_vector)**2
        enddo
        ans = ans/(n-1)
        ans = sqrt(ans)
    end function sigma
    
    real function gauss_normal(vector,i) result(ans)
        integer :: i, n
        real, dimension(:) :: vector
        real :: average_vector, dev, expo
        ! utilisation des fonctions précédentes
        average_vector = average(vector)
        dev = sigma(vector)
        
        expo =  -((i - average_vector)**2)
        expo = expo /(2*( (dev)**2 ))
        
        ans = size(vector)
        ans = ans * exp(expo)
        ans = ans/(dev*sqrt(2*pi))
    end function gauss_normal
    
    real function f_source_NRG(x,average,sigma) result(ans)
        real :: sigma, average, x
        ans = 1/(sqrt(2*pi)*sigma)
        ans = ans * exp((-((x-average)**2))/(2*(sigma**2)))
    end function

    real function f_NRG(xy, average, sigma, A) result(ans)
    real, dimension(:,:) :: xy
    real :: average,sigma, A
    integer :: i

        ans = 0
    do i = 1, size(xy,1)
        ans = ans + (xy(i,2)-(f_source_NRG(xy(i,1),average,sigma)*A))*f_source_NRG(xy(i,1),average,sigma)
    enddo
    end function f_NRG

    real function deriv(xy, average, sigma, A) result(ans)
        real, dimension(:,:) :: xy
        real :: average, sigma, A
        real, parameter :: h = 0.0001

        ans = (f_NRG(xy, average, sigma, A+h)-f_NRG(xy, average, sigma, A))/h
    end function deriv

    recursive subroutine NR_G(xy, average, sigma, A)
        real, dimension(:,:) :: xy
        real :: average, sigma
        real :: A, A_new
        
        A_new = A - (f_NRG(xy, average, sigma, A)/deriv(xy, average, sigma, A))
        if ((abs(f_NRG(xy, average, sigma, A))<=epsilon_2) .and. (abs(A-A_new)<=epsilon_1) ) then
            write(*,*) "Paramètres finaux du fit d'une distribution "
            write(*,*) "Air (A) : ", A
            write(*,*) "Centre (x_0) : ", average
            write(*,*) "L'écart type (sigma) : ", sigma
        else
            call NR_G(xy, average, sigma, A_new)
        end if
    end subroutine NR_G
    
end module