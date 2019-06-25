real function func(x) result(res)
    implicit none
    real, intent(in) :: x
    res = (sin(x**2)*cos(x**2)+exp(sin(x**2)*cos(x**2)+x**2)) / (x**2)
end function func

program f_bench_diff
    use num_diff_lib
    use omp_lib
    implicit none
    integer :: n, i
    real :: start, end, h, diffh
    real, dimension(:), allocatable :: array
    real, external :: func
    double precision :: T1
    character(len=8) :: temporary
    if(command_argument_count()>=1) then
        call get_command_argument(1, temporary)
        read(temporary, *) n
    endif
    !1 000 000 per 10000 difference
    start = 0
    end = 10 * n
    h = 0.01
    diffh = 0.00000000001
    n = ceiling((end - start) / h)
    allocate(array(n))
    array = 0
    T1 = omp_get_wtime()
    do i = 1,n
        array(i) = d1x_5p((i-1)*h, diffh, func)
    enddo
    write(*,"(F16.10)") omp_get_wtime() - T1
    array = 0
    T1 = omp_get_wtime()
    !$omp parallel do
    do i = 1,n
        array(i) = d1x_5p((i-1)*h, diffh, func)
    enddo
    !$omp end parallel do
    write(*,"(F16.10)") omp_get_wtime() - T1
    deallocate(array)
end program f_bench_diff