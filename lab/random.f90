module random
  implicit none
    integer(8), private :: state
    integer(8), private :: old_state
    integer(8), private :: inc
    integer,    private :: last
  contains
  integer function randi() result(num)
    integer :: xorshifted, rot
    old_state = state
    state = old_state * 6364136223846793005_8 + IOR(inc, 1_8)
    xorshifted = ISHFT(XOR(ISHFT(old_state, -18_8), old_state), -27_8)
    rot = ISHFT(old_state, -59_8)
    num =  IAND(IOR(ISHFT(xorshifted, -rot), ISHFT(xorshifted, IAND(-rot, 31))), huge(num))
    last = num
  end function

  real function rand() result(num)
    num = real(randi()) / real(huge(last))
  end function

  subroutine rand_seed(X, Y)
    integer(8) :: X, Y, Z
    state = 0_8;
    inc = IOR(ISHFT(Y, 1_8), 1_8);
    Z = randi()
    state = state + X
    Z = randi()
  end subroutine

  subroutine rand_timeseed()
    integer(8) :: s1, s2
    call SYSTEM_CLOCK(count=s1)
    call SYSTEM_CLOCK(count=s2)
    call rand_seed(s1,s2)
  end subroutine
end module 