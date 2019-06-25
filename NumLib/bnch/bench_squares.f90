real function f(x) result(res)
    implicit none
    real, intent(in) :: x
    res = 2*x**2 !1->2 = 4 2/3
end function f
program bench_squares
    use squares_lib
    implicit none
    integer :: n, points, repeats
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
    eps = 0.001
    n = points
    time = timing_ls(a, b, n, eps, f, repeats)
    write(*,"(A9,F20.10)") "LSSEQAVG:",sum(time)/dble(repeats)
    n = points
    time = timing_cs(a, b, n, eps, f, repeats)
    write(*,"(A9,F20.10)") "CSSEQAVG:",sum(time)/dble(repeats)
    n = points
    time = timing_rs(a, b, n, eps, f, repeats)
    write(*,"(A9,F20.10)") "RSSEQAVG:",sum(time)/dble(repeats)
    n = points
    time = timing_ls_omp(a, b, n, eps, f, repeats)
    write(*,"(A9,F20.10)") "LSOMPAVG:",sum(time)/dble(repeats)
    n = points
    time = timing_cs_omp(a, b, n, eps, f, repeats)
    write(*,"(A9,F20.10)") "CSOMPAVG:",sum(time)/dble(repeats)
    n = points
    time = timing_rs_omp(a, b, n, eps, f, repeats)
    write(*,"(A9,F20.10)") "RSOMPAVG:",sum(time)/dble(repeats)
    deallocate(time)
end program bench_squares