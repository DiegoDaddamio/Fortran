! ---------------------------------
! µFortran auto generated program.
! Created for : Diego
! Date : Sun Dec 24 09:37:44 CET 2023
! ---------------------------------
program main
    use matrix_op
    implicit none
    real, dimension(30,30) :: test = 1
    test(:,15) = 0
    test(15,:) = 0
    call export_matrix(test,"oui.ppm")

end program main
