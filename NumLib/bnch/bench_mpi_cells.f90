real function f(x,y) result(res)
    implicit none
    real, intent(in) :: x, y
    res = 1 - x + y**2
end function f
program mpi_bench_cells
    use cells_lib, only : timing_mpi
    use mpi_manager
    implicit none
    integer :: nx, ny, points, repeats
    real :: ax, bx, ay, by, eps
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
    nx = points
    ny = points
    ax = 0.5
    bx = 1
    ay = -3
    by = 2
    eps = 0.001
    time = timing_mpi(nx, ny, ax, bx, ay, by, eps, f, repeats)
    if(is_master) write(*,"(A7,F20.10)") "MPIAVG:", sum(time)/dble(repeats)
    deallocate(time)
    call mpi_finish
end program mpi_bench_cells