! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Tue Oct 24 11:12:58 CEST 2023
! ---------------------------------
program main
    implicit none
    real(kind=8) :: x,y
    
    write(*,*) "Entrez deux nombres réels : "
    write(*,*) "X : "
    read(*,*) x
    write(*,*) "Y : "
    read(*,*) y 
    
    write(*,"(A, f25.20)") "x = ", x**2
    write(*,"(A, f25.20)") "x = ",  y
    
    if (x**2 .eq. y) then
        write(*,*) "VRAI !"
    else
        write(*,*) "FAUX !"
    end if
    
end program main
