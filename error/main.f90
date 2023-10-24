! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Tue Oct 24 11:01:26 CEST 2023
! ---------------------------------
program main
    implicit none
    real (kind=8) :: nbr
    
    print*, "Entrez un nombre : "
    read*, nbr
    
    write(*,"(A,f25.20)") "Vous avez entrez le nombre : ", nbr
    
end program main