module matrix_op
    contains

    subroutine format_matrix_int(matrix,txt)
            real :: matrix(:,:)
            integer :: m, n
            character(*) :: txt
            matrix = int(matrix)
            m = size(matrix, 1)
            n = size(matrix, 2)
            write(txt,"(A,I0,A,I0,A)") "(", n, "(", m, "(I0, x),/))"
        end subroutine format_matrix_int
        
    subroutine format_matrix_real(matrix,txt)
            real :: matrix(:,:)
            integer :: m, n
            character(*) :: txt
            m = size(matrix, 1)
            n = size(matrix, 2)
            write(txt,"(A,I0,A,I0,A)") "(", n, "(", m, "(f6.4, x),/))"
    end subroutine format_matrix_real

    subroutine export(matrix,txt)
        real :: matrix(:,:)
        integer :: lig, col, mx
        character(*) :: txt
        character(150), parameter :: dtxt = "default.txt"
        character(150) :: format_m
        
        if (index(txt,".ppm")+index(txt,".txt") .eq. 0) then
            txt = dtxt
        endif
        
        lig = size(matrix,1)
        col = size(matrix,2)
        mx = maxval(matrix)
        open(unit=1, file=txt, action="write")
        if (index(txt,".ppm") > 0) then
            write (1,"(A2)") "P2"
        endif
        write (1,"(I0.4,x,I0.4)") lig, col
        if (index(txt,".ppm") > 0) then
            write (1,"(I0)") mx
        endif
        
        if ( ceiling(sum(matrix)) .eq. floor(sum(matrix)) ) then
            call format_matrix_int(matrix,format_m)
        else
            call format_matrix_real(matrix,format_m)
        endif
        
        write (1,format_m) matrix
        close (unit =1)
    end subroutine export


end module