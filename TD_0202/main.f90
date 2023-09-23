program main
    implicit none
    real :: x,a,b,ans
    print*, "This fonction need 2 constant, a and b"
    print*, "Enter a : "
    read*, a
    print*, "Enter b : "
    read*, b
    x=31
    do while (x<0 .or. x>30)
    print*, "Pick x from 0 to 30 include: "
        read*, x
    end do
    cond : if (x>=0 .and. x<5) then
        write(*,*) "F(",x,") = ", x**2 + b*x - 4*exp(x+a)
        else if (x>=5 .and. x<=15) then
        write(*,*) "F(",x,") = ", (((sqrt(x+a))**3)/(1+4*a*exp(a)))
        elseif  (x>15 .and. x<=30) then
        write(*,*) "F",x,") = ", ((x+(3/2))*sqrt((a*(x**2) + b*sqrt(x))+5*a*x)/(3*(x**2) +4*b))
    endif cond
end program main
