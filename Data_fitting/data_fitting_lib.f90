! ---------------------------------
! µFortran auto generated program.
! Created for : Diego
! Date : Tue Nov 14 10:46:48 CET 2023
! ---------------------------------
module data_fitting_lib
!Fixe la valeur de pi
    real, parameter :: pi = 3.141592
    contains
    !Calcul une valeur moyenne
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
    
    !Calcul de l'écart type
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
    
    ! Calcul de la distribution de gauss associée
    real function gauss(vector,i) result(ans)
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
    end function gauss
    
end module data_fitting_lib