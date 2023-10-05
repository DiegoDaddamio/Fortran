! ---------------------------------
! µFortran auto generated program.
! Created for : ddaddami
! Date : Thu Oct 05 17:03:31 CEST 2023
! ---------------------------------
program main
    implicit none
    integer, dimension(4,4) :: IMG = 0
    open(unit=1, file="img.txt") !ppm à la place de txt 
    write (1,"(A)") "P2" !(A) sert à écrire uniquement le nombre d'espace nécéssaire
                                    !(An) où n est un nombre qui défini le nombre d'espace 
    write (1,"(I0.4,x,I0.4)") 4, 4
    write (1, "(I3)") 255
    write (1, "(4(4(I4,x),/))") IMG
    
    close(unit=1)
end program main