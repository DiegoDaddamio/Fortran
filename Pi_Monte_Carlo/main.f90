! ---------------------------------

! µFortran auto generated program.

! Created for : Diego

! Date : Sat Sep 23 15:21:51 CEST 2023

! ---------------------------------

program main
  implicit none
  real :: r(10000000, 2)
  real :: M, d
  integer :: i, seed_size
  integer, allocatable :: seed(:)

  ! seed générée sur l'horloge du système
  call random_seed(size=seed_size)
  allocate(seed(seed_size))
  call random_seed(put=seed)
  call system_clock(count=seed(1))
  call random_number(r)

  M = 0
  do i = 1, size(r, 1)
    d = sqrt((r(i, 1)**2) + (r(i, 2)**2))
    if (d <= 1.0) then
      M = M + 1
    endif
  end do

  print *, 4.0 * (M / real(size(r, 1)))

  deallocate(seed)
end program main


