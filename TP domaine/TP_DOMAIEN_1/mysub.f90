! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Tue Oct 03 11:05:36 CEST 2023
! ---------------------------------
module mysub

contains
    subroutine fonction(a,b,x)
    
    !----------------------------------------------------
    ! Introduction des variables utilisées
    ! format pour fluidifier la sortie dans la console
    !----------------------------------------------------
    real :: x,a,b
    character(*), parameter :: format_value = "(A,F6.2,A,F6.2)"
    
    !----------------------------------------------------
    ! Condition pour identifié la fonction à utiliser
    !----------------------------------------------------
    if (x>=0 .and. x<5) then
        write(*,format_value) "F(",x,") = ", x**2 + b*x - 4*exp(x+a)
    elseif (x>=5 .and. x<=15) then
        write(*,format_value) "F(",x,") = ", (((sqrt(x+a))**3)/(1+4*a*exp(a)))
    else
        write(*,format_value) "F(",x,") = ", ((x+(3/2))*sqrt((a*(x**2) + b*sqrt(x))+5*a*x)/(3*(x**2) +4*b))
    endif

    end subroutine fonction
end module mysub