module mylib
    contains
    
    ! Donne un nombre aléatoire entre 0 et notre _max_ à UTILISER HORS BOUCLE
    function randint(maximum)
        integer :: maximum
        integer, dimension(12) :: seed
        call date_and_time(VALUES = seed)
        call random_seed(put = seed)
        call random_number(randint)
        randint = randint *1423
        randint = mod(int(randint),maximum) + 1 
    end function randint
    
    ! Donne un nombre aléatoire entre 0 et notre _max_ à UTILISER EN BOUCLE
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
    integer :: lig, col
    character(*) :: txt
    character(150) :: format_matrix 
    lig = size(matrix,1)
    col = size(matrix,2)
    open(unit=1, file=txt, status="replace") 
    write (1,"(A)") "P2" 
    write (1,"(I0.4,x,I0.4)") lig, col
    write (1, "(I3)") 1
    write (format_matrix, "(A,I0,A,I0,A)") "(", lig, "(", col, "(I1, x),/))"
    write (1, format_matrix) matrix
    close (unit =1)
    end subroutine ppmd
    
    subroutine ppmdd(matrix,txt)
    integer :: matrix(:,:)
    integer :: lig, col
    character(*) :: txt
    character(150) :: format_matrix 
    lig = size(matrix,1)
    col = size(matrix,2)
    open(unit=1, file=txt, status="replace") 
    write (1,"(A)") "P2" 
    write (1,"(I0.4,x,I0.4)") lig, col
    write (1, "(I3)") 255
    write (format_matrix, "(A,I0,A,I0,A)") "(", lig, "(", col, "(I3, x),/))"
    write (1, format_matrix) matrix
    close (unit =1)
    end subroutine ppmdd

end module mylib