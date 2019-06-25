program bench_scalar_mult
    use classic_lib, only : scalar_mult, scalar_mult_omp, get_scalar_time
    use utilities, only : random
    implicit none
    integer :: n, points, i
    real, dimension(:), allocatable :: a, b
    real :: res
    character(len=8) :: temporary
    if(command_argument_count()>=1) then
        call get_command_argument(1, temporary)
        read(temporary, *) points
    endif
    n = points
    allocate(a(n), b(n))
    do i = 1,n
        a(i) = random(-1., 1.)
        b(i) = random(-1., 1.)
    enddo
    res = scalar_mult(a, b)
    WRITE(*,"(A4,F20.10)") "SEQ:", get_scalar_time()
    res = scalar_mult_omp(a, b)
    WRITE(*,"(A4,F20.10)") "OMP:", get_scalar_time()
    deallocate(a, b)
end program bench_scalar_mult