program bench_odu
    use eiler_lib, only : eiler_timing, eiler_timing_omp
    use rk_lib, only : rk_timing, rk_timing_omp
    use adams5_lib, only : adams5_timing, adams5_timing_omp
    implicit none
    integer :: n, repeats
    real :: a, b, h
    double precision, dimension(:), allocatable :: time
    character(len=8) :: temporary
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
    a = 0
    b = 1
    h = 0.001
    time = eiler_timing(a, b, h, n, repeats)
    write(*,"(A8,F20.10)") "ESEQAVG:",sum(time)/dble(repeats)
    time = eiler_timing_omp(a, b, h, n, repeats)
    write(*,"(A8,F20.10)") "EOMPAVG:",sum(time)/dble(repeats)
    time = rk_timing(a, b, h, n, repeats)
    write(*,"(A9,F20.10)") "RKSEQAVG:",sum(time)/dble(repeats)
    time = rk_timing_omp(a, b, h, n, repeats)
    write(*,"(A9,F20.10)") "RKOMPAVG:",sum(time)/dble(repeats)
    time = adams5_timing(a, b, h, n, repeats)
    write(*,"(A8,F20.10)") "ASEQAVG:",sum(time)/dble(repeats)
    time = adams5_timing_omp(a, b, h, n, repeats)
    write(*,"(A8,F20.10)") "AOMPAVG:",sum(time)/dble(repeats)
    deallocate(time)
end program bench_odu