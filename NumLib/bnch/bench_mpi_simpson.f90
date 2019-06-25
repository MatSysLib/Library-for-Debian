real function f(x) result(res)
    implicit none
    real, intent(in) :: x
    res = 2*x**2 !1->2 = 4 2/3
end function f
program test_simpson
    use simpson_lib, only : timing_mpi
    use mpi_manager
    implicit none
    integer :: n, repeats
    real :: a, b, eps
    real, external :: f
    double precision, dimension(:), allocatable :: time
    character(len=8)::temporary
    call mpi_launch
    call mpi_setup
    if(command_argument_count()>=1) then
        call get_command_argument(1, temporary)
        read(temporary, *) n
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
    time = timing_mpi(a, b, n, eps, f, repeats)
    if(is_master) write(*,"(A7,F20.10)") "MPIAVG:", sum(time)/dble(repeats)
    deallocate(time)
    call mpi_finish
end program test_simpson