! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Tue Oct 17 10:46:37 CEST 2023
! ---------------------------------
program main
    implicit none
    !----------------------------------------------------
    ! Variable x et y pour les coordonnées du points à déterminer par f 
    ! Format de charactère pour fluidifier la sortie console
    !----------------------------------------------------
    real :: x, y
    character(*), parameter :: format_value = "(A,F6.2,A,F6.2,A,F6.2)"
    
    !----------------------------------------------------
    ! Demande à l'utilisateur pour avoir les variables à tester
    !----------------------------------------------------
    print*,"Quels sont les valeurs à introduire dans la fonction ? "
    print*,"x : "
    read*, x
    print*,"y : "
    read*, y
    
    !----------------------------------------------------
    ! Suite de condition imbriquée pour déterminer la fonction adaptée
    !----------------------------------------------------
    if(x>=0)then
        if(y>=0)then
            write(*,format_value) "f(",x,",",y,") =", x + y
        else
            write(*,format_value) "f(",x,",",y,") =", x + y**2 
        endif
    else
            if(y>=0) then
                write(*,format_value) "f(",x,",",y,") =", x**2 + y
            else
                write(*,format_value) "f(",x,",",y,") =", x**2 + y**2
            endif
    endif
    
end program main