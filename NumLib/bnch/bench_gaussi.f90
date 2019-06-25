real function f(x) result(res)
    implicit none
    real, intent(in) :: x
    res = 2*x**2 !1->2 = 4 2/3
end function f

program bench_gaussi
    use gaussi_lib, only : timing, timing_omp
    implicit none
    integer :: n, p, points, repeats
    real :: a, b, eps
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
    a = 1
    b = 2
    p = 7
    eps = 0.001
    n = points
    time = timing(a, b, n, p, eps, f, repeats)
    write(*,"(A7,F12.8)") "SEQAVG:",sum(time)/dble(repeats)
    n = points
    time = timing_omp(a, b, n, p, eps, f, repeats)
    write(*,"(A7,F12.8)")"OMPAVG:",sum(time)/dble(repeats)
    deallocate(time)
end program bench_gaussi