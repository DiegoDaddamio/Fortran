! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Tue Oct 03 11:05:36 CEST 2023
! ---------------------------------
program main
    !----------------------------------------------------
    ! Utilisation d'un module contenant la sous routine mysub
    !----------------------------------------------------
    use mysub
    implicit none
    
    !----------------------------------------------------
    ! Introduction des variables utilisée pour 
    !----------------------------------------------------
    real :: x, a, b
    
    !----------------------------------------------------
    ! Demande à l'utilisateur deux constantes
    !----------------------------------------------------
    print*, "This fonction need 2 constant, a and b"
    print*, "Enter a : "
    read*, a
    print*, "Enter b : "
    read*, b
    
    !----------------------------------------------------
    ! Demande à l'utilisateur un nombre entre 0 et 30
    ! Mise en boucle par une valeur de x trop grande
    !----------------------------------------------------
    x=31
    do while (x<0 .or. x>30)
    print*, "Pick x from 0 to 30 include: "
        read*, x
    end do
    
    !----------------------------------------------------
    ! Utilisation de la sous routine voir mysub.f90
    !----------------------------------------------------
    call fonction(a,b,x)
end program main