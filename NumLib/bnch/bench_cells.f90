real function f(x,y) result(res)
    implicit none
    real, intent(in) :: x, y
    res = 1 - x + y**2
end function f
program bench_cells
    use cells_lib, only : timing, timing_omp
    implicit none
    integer :: nx, ny, points, repeats
    real :: ax, bx, ay, by, eps
    real, external :: f
    double precision, dimension(:), allocatable :: time
    character(len=8) :: temporary
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
    time = timing(nx, ny, ax, bx, ay, by, eps, f, repeats)
    write(*,"(A7,F20.10)") "SEQAVG:", sum(time)/dble(repeats)
    nx = points
    ny = points
    ax = 0.5
    bx = 1
    ay = -3
    by = 2
    eps = 0.001
    time = timing_omp(nx, ny, ax, bx, ay, by, eps, f, repeats)
    write(*,"(A7,F20.10)")"OMPAVG:", sum(time)/dble(repeats)
    deallocate(time)
end program bench_cells