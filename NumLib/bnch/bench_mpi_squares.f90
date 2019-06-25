real function f(x) result(res)
    implicit none
    real, intent(in) :: x
    res = 2*x**2 !1->2 = 4 2/3
end function f

program mpi_test_squares
    use squares_lib
    use mpi_manager
    implicit none
    integer :: n, points, repeats
    real :: a, b, eps
    real, external :: f
    double precision, dimension(:), allocatable :: time
    character(len=8)::temporary
    call mpi_launch
    call mpi_setup
    if(command_argument_count()>=1) then
        call get_command_argument(1, temporary)
        read(temporary, *) points
        repeats = 10
    endif
    if(command_argument_count()>=2) then
        call get_command_argument(2, temporary)
        read(temporary, *) repeats
    endif
    allocate(time(repeats))
    a = 1
    b = 2
    eps = 0.001
    n = points
    time = timing_ls_mpi(a, b, n, eps, f, repeats)
    if(is_master) write(*,"(A9,F20.10)") "LSMPIAVG:",sum(time)/dble(repeats)
    n = points
    time = timing_cs_mpi(a, b, n, eps, f, repeats)
    if(is_master) write(*,"(A9,F20.10)") "CSMPIAVG:",sum(time)/dble(repeats)
    n = points
    time = timing_rs_mpi(a, b, n, eps, f, repeats)
    if(is_master) write(*,"(A9,F20.10)") "RSMPIAVG:",sum(time)/dble(repeats)
    deallocate(time)
    call mpi_finish
end program mpi_test_squares