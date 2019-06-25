real function f(x) result(res)
    implicit none
    real, intent(in) :: x
    res = 2*x**2 !1->2 = 4 2/3
end function f
real function f2(x,y) result(res)
    implicit none
    real, intent(in) :: x, y
    res = 1 - x + y**2
end function f2
program mpi_test_montekarlo
    use montekarlo_lib, only : timing_one_mpi, timing_two_mpi
    use mpi_manager
    implicit none
    integer :: counter, points, repeats
    real :: ax, ay, bx, by, eps, a, b
    real, external :: f, f2
    double precision, dimension(:), allocatable :: time
    character(len=8) :: temporary
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
    ax = 0.5
    bx = 1
    ay = -3
    by = 2
    eps = 0.001
    counter = points
    time = timing_one_mpi(counter, a, b, eps, f, repeats)
    if(is_master) write(*,"(A10,F20.10)") "ONEMPIAVG:", sum(time)/dble(repeats)
    counter = points
    time = timing_two_mpi(counter, ax, bx, ay, by, eps, f2, repeats)
    if(is_master) write(*,"(A10,F20.10)") "TWOMPIAVG:", sum(time)/dble(repeats)
    deallocate(time)
    call mpi_finish
end program mpi_test_montekarlo