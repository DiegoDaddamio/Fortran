! ---------------------------------
! µFortran auto generated program.
! Created for : Diego
! Date : Thu Nov 16 16:17:53 CET 2023
! ---------------------------------
program main
use lib
    implicit none
    ! Création d'une grille ainsi que son homologue pour les calculs
    real, dimension(10,10) :: grille, ghost
    integer :: i,j
    real :: cst 
        ! On vient fixer nos valeur de référence
        cst = 20.0
        grille = 50.0
        call const(grille,cst)
        ghost = grille
        grille(3,3) = 1000.0
        
        ! Boucle qui permet d'arriver à une températeur prtiquement constante.
        do while (.true.)
        ! On balaye toute la grille pour évalué chacun
            do i = 2, size(grille,1) - 1
                do j = 2, size(grille,1) - 1
                ! Calcul la moyenne des 4 cases aux alentours
                    ghost(i,j) = temp(grille,i,j)
                enddo
            enddo
            ! Mise à jour pour une itération
            grille = ghost
        ! Test pour la convergence
        if (maxval(grille)<(cst+0.01))then
            ! Créer un fichier .txt pour pouvoir le voir.
            call ppmd(grille,"test.txt")!ou ppm mais pas interresant)
            ! Arrêt du programme
            stop
        endif
        end do
        
end program main
