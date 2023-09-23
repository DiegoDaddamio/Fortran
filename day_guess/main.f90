! ---------------------------------
! µFortran auto generated program.
! Created for : Diego
! Date : Sat Sep 23 12:35:59 CEST 2023
! ---------------------------------
program main
    implicit none
    
    integer :: i
   character(50) :: day_name,day_tester
   read(*,'(A)') day_name  !'(A)' pour alphanumérique
   day_tester = trim(day_name)
   
   do i = 1, len_trim(day_name)
     if (ichar('A') <= ichar(day_name(i:i)) .and. ichar(day_name(i:i)) <= ichar('Z')) then
        day_tester(i:i) = achar(ichar(day_name(i:i)) + ichar('a') - ichar('A'))
     end if
  end do

    select case (day_tester)
    case("monday","tuesday","wednesday","thursday","friday")
        write(*,*) trim(day_name)//" is in the week"
    case("sunday","saturday")
        write(*,*) trim(day_name)//"is in the week-end"
    case default
        write(*,*) trim(day_name)//" is not a day !"
    end select
    
end program main