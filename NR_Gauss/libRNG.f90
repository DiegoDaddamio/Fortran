module libRNG

real, parameter :: epsilon_1 = 0.000001, epsilon_2 = 0.00001
real, parameter :: pi = 3.141592
contains
    
    ! Cas où la taille du vecteur n'est pas indiquée dans la liste
    integer function size_vector(text) result(ans)
        character(*) :: text
        open(10,file = text)
        read(10,*) ans
        close(10)
    
    end function size_vector
    
    function import_vector(text,size) result(ans)
        integer :: size
        real, dimension(size) :: ans
        character(*) :: text
        open(10,file = text)
        read(10,*) 
        read(10,*) ans
        close(10)
    end function import_vector
    
    subroutine rise_y(x,x_new,y)
        real,dimension(:) :: x, x_new, y
        integer :: i, j = 1
        do i = int(minval(x)), int(maxval(x)) 
            y(j) = sum(x/i, mask = x == i)
            x_new(j) = i
            j = j + 1
        enddo

    end subroutine rise_y
    
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
    
    function f_source(x,average,sigma) result(ans)
        real :: sigma, average, x
        
        ans = 1/(sqrt(2*pi)*sigma)
        ans = ans * exp((-((x-average)**2))/(2*(sigma**2)))
        
    end function
    
    function f(xy, average, sigma, A) result(ans)
    real, dimension(:,:) :: xy
    real :: average,sigma, A
        ans = 0
    do i = 1, size(xy,1)
        ans = ans + (xy(i,2)-(f_source(xy(i,1),average,sigma)*A))*f_source(xy(i,1),average,sigma)
    enddo
    end function f
    
    ! Fonction qui détermine numériquement la dérivée de f(x) numériquement par définition
    real function deriv(xy, average, sigma, A) result(ans)
        real, dimension(:,:) :: xy
        real :: average, sigma, A

        real, parameter :: h = 0.001
        ans = (f(xy, average, sigma, A+h)-f(xy, average, sigma, A))/h
    end function deriv

! Algorithme récursif, calculant la suite convergente vers une racine de f(x)
    recursive subroutine N_R(xy, average, sigma, A)
    real, dimension(:,:) :: xy
        real :: average, sigma
        real :: A, A_new
        ! Calcul de la première itération de la suite pour la condition suivante

        A_new = A - (f(xy, average, sigma, A)/deriv(xy, average, sigma, A))
        ! Condition pour arrêter la suite, comparable à la précision demandée et fixée par le module "parameters"

        if ((abs(f(xy, average, sigma, A))<=epsilon_2) .and. (abs(A-A_new)<=epsilon_1) ) then
            ! Affichage des données et réinitialisation du compteur

            write(*,*) "L'approximation de la racine est : ", A
        else
            ! Calcul de la prochaine itération et ceci en chaîne via la sous-routine
            A = A_new
            ! Calcul de la prochaine itération
            call N_R(xy, average, sigma, A)
        end if
    end subroutine N_R
  
    subroutine ppmd(matrix,txt)
        integer :: matrix(:,:)
        integer :: lig, col, mx
        character(*) :: txt
        character(150) :: format_m
        lig = size(matrix,1)
        col = size(matrix,2)
        mx = maxval(matrix)
        open(unit=1, file=txt, action="write") 
        write (1,"(A2)") "P2" 
        write (1,"(I0.4,x,I0.4)") lig, col
        write (1,"(I0)") mx
        call format_matrix_int(matrix,format_m)
        write (1,format_m) matrix
        close (unit =1)
    end subroutine ppmd
    
subroutine format_matrix_int(matrix,txt)
        integer :: matrix(:,:)
        integer :: m, n, mx, lg
        character(*) :: txt
        m = size(matrix, 1)
        n = size(matrix, 2)
        write(txt,"(A,I0,A,I0,A)") "(", n, "(", m, "(I0, x),/))"
    end subroutine format_matrix_int
    
end module libRNG