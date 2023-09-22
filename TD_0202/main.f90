! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Fri Sep 22 10:15:14 CEST 2023
! ---------------------------------
program main
    implicit none
    real :: x,a,b,ans
    a = 8.813
    b = 721.5
    read*, x
    
    cond : if (x=>0 .and. x<5) then
        ans = x**2 + b*x - 4*exp(x+a)
    elseif  (x>5 .and. x=<15) then
        ans = (((sqrt(x+a))**3)/(1+4*a*exp(a))
    elseif  (x>15 .and. x=<30) then
        ans = ((x+(3/2))*sqrt((a*(x**2) + b sqrt(x))+5*a*x)/(3*(x**2) +4*b)
    endif cond
    
    print*, ans

end program main
