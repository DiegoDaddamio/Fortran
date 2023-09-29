! ---------------------------------



! µFortran auto generated program.



! Created for : ddaddami



! Date : Thu Sep 28 16:37:33 CEST 2023



! ---------------------------------



program main


  implicit none
  ! Déclaration des variables 
      integer :: i      ! Entier pour une boucle
      real :: table(100,2)
     ! Tableau de stockage des coordonnées de points
      real :: count, 
norm      ! Variable utilisée, qui, ont besoins d'être réelles
      integer, dimension(12) :: seed    ! Déclare un vecteur de dimension 12



 
  ! Seed pour le random générée sur l'horloge du système
      call date_and_time(VALUES =seed)


 ! Donne simplement 
      call random_seed(put=seed)



      call random_number(table)


  
  ! Initialisation du compteur et de la boucle principale
      count = 0


 ! Initialisation du compteur
      do i = 0, size(table, 1)


 ! Pour chaque coordonnée dans le tableau
        norm = sqrt((table(i, 1)**2) + (table(i, 2)**2))


 ! On y regarde simplement la norme
        if (norm <= 1.0) then


 ! Condition pour savoir si le points est dans le cercle ou non
          count = count + 1


        endif
        ! write(*,*) table(i, 1), table(i, 1) si besoin de posséder les coordonées de points
      end do


      



  ! Simplement écrire dans la console le calcul final du rapport de position
    print*, 4.0 * (count / real(size(table, 1)))


 ! Le real est nécéssaire pour ne pas avoir une division revenue à un entier



end program main


