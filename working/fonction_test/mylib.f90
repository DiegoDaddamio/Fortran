! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Tue Oct 03 11:05:36 CEST 2023
! ---------------------------------
module mylib
real :: a,b,x
contains
    function f(a,b,x)
    cond : if (x>=0 .and. x<5) then
        f = (x**2 + b*x - 4*exp(x+a))
        else if (x>=5 .and. x<=15) then
        f = ((((sqrt(x+a))**3)/(1+4*a*exp(a))))
        elseif  (x>15 .and. x<=30) then
        f = (((x+(3/2))*sqrt((a*(x**2) + b*sqrt(x))+5*a*x)/(3*(x**2) +4*b)))
    endif cond

    end function f
end Module mylib