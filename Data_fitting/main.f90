! ---------------------------------
! µFortran auto generated program.
! Created for : Diego
! Date : Tue Nov 14 10:46:48 CET 2023
! ---------------------------------
program main
    use data_fitting_lib
    implicit none
    integer :: nbr_coups, i
    integer, allocatable, dimension(:) :: coups
    real :: coups_moy = 0, ec_type
    
    !Extrait les données du fichier .txt pour les remettre dans un vecteur 
    Open(10,file = "gauss.txt")
    read(10,*) nbr_coups
    allocate(coups(nbr_coups))
    read(10,*) coups
    close(10)
    !Calcul le nombre de coups moyens
    coups_moy = average(real(coups))
    !Calcul l'écart type
    ec_type = sigma(real(coups))
    
    !Montre l'odre de grandeur similaire 
    write(*,*) "Racine des coups moyen : ",sqrt(coups_moy) 
    write(*,*) "L'écart type : ",ec_type
    
    !Met les données dans un fichier .txt
    !Il ne manque qu'un automatiseur de borne pour la boucle do car nous l'avons fixé à 100
    Open(11,file = "result.txt")
    do i = int(coups_moy)-100, int(coups_moy)+100
        write(11,"(I0,x,f9.6)") i,gauss(real(coups),i)
    enddo
    close(11)
    
    deallocate(coups)
end program main
