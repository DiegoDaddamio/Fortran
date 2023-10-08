module ppmdraw
    contains
    subroutine ppmd(matrix,txt)
    integer :: matrix(:,:)
    integer :: lig, col
    character(*) :: txt
    character(150) :: format_matrix 
    lig = size(matrix,1)
    col = size(matrix,2)
    open(unit=1, file=txt, status="replace") 
    write (1,"(A)") "P2" !(A) sert à écrire uniquement le nombre d'espace nécéssaire
                                    !(An) où n est un nombre qui défini le nombre d'espace 
    write (1,"(I0.4,x,I0.4)") lig, col
    write (1, "(I1)") 1
    write (format_matrix, "(A,I0,A,I0,A)") "(", lig, "(", col, "(I1, x),/))"
    write (1, format_matrix) matrix
    close (unit =1)

    
    end subroutine ppmd

end module ppmdraw