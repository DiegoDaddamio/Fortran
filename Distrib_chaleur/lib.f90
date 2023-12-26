! ---------------------------------
! µFortran auto generated program.
! Created for : Diego
! Date : Thu Nov 16 16:17:53 CET 2023
! ---------------------------------
module lib
contains

    ! Fait une moyenne simple des 4 valeurs des cases en contacte direct
    real function temp(plate,i,j) result(ans)
        integer :: i, j
        real, dimension(:,:) :: plate
        ans = 0
        ans = ans + plate(i-1,j)
        ans = ans + plate(i+1,j)
        ans = ans + plate(i,j-1)
        ans = ans + plate(i,j+1)
        ans = ans /4
    end function temp
    
    ! Place au bord d'une matrice, une valeur constante.
    subroutine const(plate,cst)
    real, dimension(:,:) :: plate
    real :: cst
        plate(1,:) = cst
        plate(size(plate,1),:) = cst
        plate(:,size(plate,1)) = cst
        plate(:,1) = cst
    end subroutine const
    
    ! créer un format pour la matrice demandée
    subroutine format_matrix_int(matrix,txt)
        real :: matrix(:,:)
        integer :: m, n, mx, lg
        character(*) :: txt
        m = size(matrix, 1)
        n = size(matrix, 2)
        write(txt,"(A,I0,A,I0,A)") "(", n, "(", m, "(f10.6, x),/))"
    end subroutine format_matrix_int

    ! Créé un fichier pour le lire en ppm ou txt
    subroutine ppmd(matrix,txt)
        real:: matrix(:,:)
        integer :: lig, col, mx
        character(*) :: txt
        character(150) :: format_m
        lig = size(matrix,1)
        col = size(matrix,2)
        mx = maxval(matrix)
        open(unit=1, file=txt) 
        write (1,"(A2)") "P2" 
        write (1,"(I0.4,x,I0.4)") lig, col
        write (1,"(I0)") mx
        call format_matrix_int(matrix,format_m)
        write (1,format_m) matrix
        close (unit =1)
    end subroutine ppmd

end module