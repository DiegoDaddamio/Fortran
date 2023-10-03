program main
    use mylib
    implicit none
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
    write(*,*) f(a,b,x)
end program main