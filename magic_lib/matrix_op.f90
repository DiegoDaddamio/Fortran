module matrix_op
    contains
    subroutine format_matrix_int(matrix,txt)
            real :: matrix(:,:)
            integer :: m, n
            character(*) :: txt
            matrix = int(matrix)
            m = size(matrix, 1)
            n = size(matrix, 2)
            write(txt,"(A,I0,A,I0,A)") "(",m,"(", n,"(I0, x),/))"
        end subroutine format_matrix_int
        
    subroutine format_matrix_real(matrix,txt)
            real :: matrix(:,:)
            integer :: m, n
            character(*) :: txt
            m = size(matrix, 1)
            n = size(matrix, 2)
            write(txt,"(A,I0,A,I0,A)") "(", m, "(", n, "(f8.4, x),/))"
    end subroutine format_matrix_real

    subroutine export_matrix(matrix,txt)
        real :: matrix(:,:)
        integer :: lig, col, mx
        character(*) :: txt
        character(150) :: format_m
        
        lig = size(matrix,1)
        col = size(matrix,2)
        mx = maxval(matrix)
        open(unit=1, file=txt, action="write")
        if (index(txt,".ppm") > 0) then
            write (1,"(A2)") "P2"
            write (1,"(I0.4,x,I0.4)") lig, col
            write (1,"(I0)") mx
        else
             write (1,"(I0,x,I0)") lig, col
        endif
        
        if ( detect_real(matrix) ) then
            call format_matrix_real(matrix,format_m)
            write(1,format_m) real(matrix)
        else
            call format_matrix_int(matrix,format_m)
            write(1,format_m) int(matrix)
        endif
        
        close (unit =1)
    end subroutine export_matrix

    logical function detect_real(matrix) result(ans)
        real :: matrix(:,:)
        integer :: i, j
        ans = .false.
        
        if ( (ceiling(sum(matrix)) .eq. floor(sum(matrix))) .eqv. .false. ) then
            ans = .true.
        else
            do i = 1 , size(matrix,1)
                do j = 1, size(matrix,2)
                    if ( (ceiling(matrix(i,j)) .eq. floor(matrix(i,j)) ) .eqv. .false.) then
                        ans = .true.
                        exit 
                    endif
                enddo
            enddo
        endif
    end function
    
    function size_matrix(text) result(ans)
        integer :: ans(2)
        character(*) :: text
        open(10,file = text)
        if (index(text,".ppm") > 0) then
            read (10,*)
        endif
        read(10,*) ans
        close(10)
    end function size_matrix
    
    
    function import_matrix(text) result(ans)
        integer :: size(2)
        real, allocatable :: ans(:,:)
        character(*) :: text
        size = size_matrix(text)
        allocate(ans(size(1),size(2)))
        open(10,file = text)
        if (index(text,".ppm") > 0) then
            read (10,*)
        endif
        read(10,*) size
        if (index(text,".ppm") > 0) then
            read(10,*)
        endif
        read(10,*) ans
        close(10)
    end function import_matrix

end module