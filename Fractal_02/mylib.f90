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
    ! Donne un nombre aléatoire entre 0 et notre _max_à UTILISER EN BOUCLE
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
        integer :: lig, col, mx
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
end module mylib
