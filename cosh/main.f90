! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Fri Sep 22 09:51:05 CEST 2023
! ---------------------------------
program main
    implicit none
    real :: x,ans
    print*, "Entrez l'argument d'un cosinus hyperbolique : "
    read*, x
    ans = (exp(x)+exp(-x))/2
    print*, "Par la fonction développée : ", ans
    print*, "Par la fonction implémentée: ", cosh(x)
end program main
