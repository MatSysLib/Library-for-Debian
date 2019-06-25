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
program bench_montekarlo
    use montekarlo_lib
    implicit none
    integer :: counter, points, repeats
    real :: ax, ay, bx, by, eps, a, b
    real, external :: f, f2
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
    counter = points
    a = 1
    b = 2
    ax = 0.5
    bx = 1
    ay = -3
    by = 2
    eps = 0.001
    time = timing_one(counter, a, b, eps, f, repeats)
    write(*,"(A10,F20.10)") "ONESEQAVG:",sum(time)/dble(repeats)
    counter = points
    time = timing_one_omp(counter, a, b, eps, f, repeats)
    write(*,"(A10,F20.10)") "ONEOMPAVG:",sum(time)/dble(repeats)
    counter = points
    time = timing_two(counter, ax, bx, ay, by, eps, f2, repeats)
    write(*,"(A10,F20.10)") "TWOSEQAVG:",sum(time)/dble(repeats)
    counter = points
    time = timing_two_omp(counter, ax, bx, ay, by, eps, f2, repeats)
    write(*,"(A10,F20.10)") "TWOOMPAVG:",sum(time)/dble(repeats)
    counter = points
    deallocate(time)
end program bench_montekarlo