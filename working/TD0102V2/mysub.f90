! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Tue Oct 03 11:05:36 CEST 2023
! ---------------------------------
module mysub
contains
    subroutine fonction(a,b,x)
    real :: x,a,b
    cond : if (x>=0 .and. x<5) then
        write(*,*) "F(",x,") = ", x**2 + b*x - 4*exp(x+a)
        else if (x>=5 .and. x<=15) then
        write(*,*) "F(",x,") = ", (((sqrt(x+a))**3)/(1+4*a*exp(a)))
        elseif  (x>15 .and. x<=30) then
        write(*,*) "F(",x,") = ", ((x+(3/2))*sqrt((a*(x**2) + b*sqrt(x))+5*a*x)/(3*(x**2) +4*b))
    endif cond

    end subroutine fonction
end Module mysub