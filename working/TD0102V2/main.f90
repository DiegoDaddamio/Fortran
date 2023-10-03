! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Tue Oct 03 11:05:36 CEST 2023
! ---------------------------------
program main
    use mysub
    implicit none
    real :: x,a,b
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
    call fonction(a,b,x)
end program main