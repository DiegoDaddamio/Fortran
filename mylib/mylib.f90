module mylib
    contains

    ! Donne un nombre aléatoire entre 0 et notre _max à UTILISER HORS BOUCLE
    function randint(maximum)
        integer :: maximum
        integer, dimension(12) :: seed
        call date_and_time(VALUES = seed)
        call random_seed(put = seed)
        call random_number(randint)
        randint = randint *1423
        randint = mod(int(randint),maximum) + 1 
    end function randint

    subroutine rand_mint(matrix, maximum)
        integer :: maximum, i, j
        real:: matrix(:,:)
        integer, dimension(12) :: seed
        call date_and_time(VALUES = seed)
        call random_seed(put = seed)
        call random_number(matrix)
        matrix = matrix * 1000
        matrix = mod(int(matrix), maximum)
    end subroutine rand_mint
    
    ! Donne un nombre aléatoire entre 0 et notre _max_à UTILISER EN BOUCLE
    function randintloop(maximum,iloop)
        integer :: maximum
        integer, dimension(12) :: seed
        call date_and_time(VALUES =seed)
        call random_seed(put=seed)
        call random_number(randintloop)
        randintloop = randintloop*iloop
        randintloop = mod(int(randintloop),123) + 1
        randintloop = randintloop*iloop
        randintloop = mod(int(randintloop),maximum) + 1
    end function randintloop
    
    ! Construit un fichier ppm ou txt à partir d'une matrice
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

    subroutine txtdata(matrix,txt)
        real, dimension(:,:), allocatable :: matrix
        integer :: lig, col
        character(*) :: txt
        open(unit=1, file=txt, action="read")
        read (1,*) lig, col
        allocate (matrix(col,lig))
        read (1,*) matrix
        close (unit =1)
        matrix = transpose(matrix)
    end subroutine txtdata

    subroutine reg_op(pts,h)
        real :: pts(:,:)
        real :: h(:)
        real :: a, b, Sy = 0, Sxy = 0, Sx = 0, Sxx = 0, S, D
        
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
        write(*,"(A,f6.4,A,f6.4)") "f(x) = ",a," x +",b

    end subroutine reg_op

    subroutine format_matrix_int(matrix,txt)
        integer :: matrix(:,:)
        integer :: m, n, mx, lg
        character(*) :: txt
        m = size(matrix, 1)
        n = size(matrix, 2)
        write(txt,"(A,I0,A,I0,A)") "(", n, "(", m, "(I0, x),/))"
    end subroutine format_matrix_int

end module mylib
