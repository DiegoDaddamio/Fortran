! ---------------------------------
! µFortran auto generated program.
! Created for : Diego
! Date : Thu Jan 18 21:27:01 CET 2024
! ---------------------------------
program main
    use random
    use matrix_op
    implicit none
    real :: lab(201,201) !impaire
    integer :: i,j,c, loo
    real :: mval
    
    loo = size(lab,1)
    call rand_timeseed()
    c = 10
    lab = 0
    lab(1,2) = 10
    do i = 2, size(lab,1)-1, 2
        do j = 2, size(lab,1)-1, 2
            lab(i,j) = c
            c = c +1
        enddo
    enddo
    lab(loo,loo-1) = lab(loo-1,loo-1)
    mval = maxval(lab)
    
    do while(SUM(lab, MASK=MOD(lab, mval).ne.0) .ne. 0)
        call choose(lab)
    enddo
    
    
    call export_matrix(lab,"tester_lyb.ppm")
    
end program main